;;;; Constant folding & propagation.

(in-package #:system.compiler)

(defvar *known-variables* nil
  "An alist mapping lexical-variables to their values, if known.")

(defun cp-form (form)
  (etypecase form
    (cons (case (first form)
	    ((block) (cp-block form))
	    ((go) (cp-go form))
	    ((if) (cp-if form))
	    ((let) (cp-let form))
	    ((load-time-value) (cp-load-time-value form))
	    ((multiple-value-call) (cp-multiple-value-call form))
	    ((multiple-value-prog1) (cp-multiple-value-prog1 form))
	    ((progn) (cp-progn form))
	    ((progv) (cp-progv form))
	    ((quote) (cp-quote form))
	    ((return-from) (cp-return-from form))
	    ((setq) (cp-setq form))
	    ((tagbody) (cp-tagbody form))
	    ((the) (cp-the form))
	    ((unwind-protect) (cp-unwind-protect form))
	    (t (cp-function-form form))))
    (lexical-variable (cp-variable form))
    (lambda-information (cp-lambda form))))

(defun form-value (form &key (reduce-use-count t))
  "Return the value of form wrapped in quote if its known, otherwise return nil."
  (cond ((or (and (consp form)
		  (eql (first form) 'quote))
	     (lambda-information-p form))
	 form)
	((lexical-variable-p form)
	 (let ((val (assoc form *known-variables*)))
	   (when val
	     (when reduce-use-count
	       (decf (lexical-variable-use-count form)))
	     (second val))))))

(defun cp-implicit-progn (x)
  (do ((i x (cdr i)))
      ((endp i))
    (setf (car i) (cp-form (car i)))))

(defun cp-block (form)
  (cp-implicit-progn (cddr form))
  form)

(defun cp-go (form)
  form)

(defun cp-if (form)
  (flet ((pick-branch (use-this-one kill-this-one)
	   ;; Disabled for now. SBCL seems to be turning print-circle off while printing?
	   #+nil(unless (and (consp kill-this-one)
			(eq (first kill-this-one) 'quote))
	     (warn 'simple-style-warning
		   :format-control "Deleting unreacable code: ~S."
		   :format-arguments (list kill-this-one)))
	   (flush-form kill-this-one)
	   (cp-form use-this-one)))
    (setf (second form) (cp-form (second form)))
    (let ((value (form-value (second form))))
      (if value
	  (progn
	    (incf *change-count*)
	    (if (eql (second value) 'nil)
		;; Use the else branch.
		(pick-branch (fourth form) (third form))
		;; Use the true branch.
		(pick-branch (third form) (fourth form))))
	  (progn
	    (setf (third form) (cp-form (third form)))
	    (setf (fourth form) (cp-form (fourth form)))
	    form)))))

(defun cp-let (form)
  (let ((new-constants *known-variables*))
    (dolist (b (second form))
      (let ((var (first b))
	    (val (second b)))
	;; Run on the init-form.
	(setf (second b) (cp-form (second b)))
	;; Add constants to the new constants list.
	(when (and (lexical-variable-p var)
		   (eql (lexical-variable-write-count var) 0)
		   (or (lambda-information-p val)
		       (and (consp val) (eq (first val) 'quote))
		       (and (lexical-variable-p val)
			    (localp val)
			    (eql (lexical-variable-write-count val) 0))))
	  (push b new-constants))))
    ;; Run on the body, with the new constants.
    (let ((*known-variables* new-constants))
      (cp-implicit-progn (cddr form)))
    form))

;;;(defun cp-load-time-value (form))

(defun cp-multiple-value-call (form)
  (cp-implicit-progn (cdr form))
  form)

(defun cp-multiple-value-prog1 (form)
  (cp-implicit-progn (cdr form))
  form)

(defun cp-progn (form)
  (cp-implicit-progn (cdr form))
  form)

(defun cp-progv (form)
  (cp-implicit-progn (cdr form))
  form)

(defun cp-quote (form)
  form)

(defun cp-return-from (form)
  (setf (third form) (cp-form (third form)))
  form)

(defun cp-setq (form)
  (setf (third form) (cp-form (third form)))
  form)

(defun cp-tagbody (form)
  (do ((i (cddr form) (cdr i)))
      ((endp i))
    (unless (go-tag-p (car i))
      (setf (car i) (cp-form (car i)))))
  form)

(defun cp-the (form)
  (setf (third form) (cp-form (third form)))
  form)

(defun cp-unwind-protect (form)
  (cp-implicit-progn (cdr form))
  form)

(defun constant-fold (function arg-list)
  ;; Bail out in case of errors.
  (ignore-errors
    (let ((mode (get function 'constant-fold-mode)))
      (if (consp mode)
	  (list 'quote (apply function (mapcar (lambda (thing type)
						 ;; Bail out if thing is non-constant or does not match the type.
						 (unless (and (consp thing)
							      (eql (first thing) 'quote)
							      (= (length thing) 2)
							      (typep (second thing) type))
						   (return-from constant-fold nil))
						 (second thing))
					       arg-list mode)))
	  (ecase mode
	    (:commutative-arithmetic
	     ;; Arguments can be freely re-ordered, assumed to be associative.
	     ;; Addition, multiplication and the logical operators use this.
             ;; FIXME: Float arithemetic is non-commutative.
	     (let ((const-args '())
		   (nonconst-args '())
		   (value nil))
	       (dolist (i arg-list)
		 (if (and (consp i) (eq (first i) 'quote))
		     (push (second i) const-args)
		     (push i nonconst-args)))
	       (setf const-args (nreverse const-args)
		     nonconst-args (nreverse nonconst-args))
	       (when (or const-args (not nonconst-args))
		 (setf value (apply function const-args))
		 (if nonconst-args
		     (list* function (list 'quote value) nonconst-args)
		     (list 'quote value)))))
	    (:arithmetic
	     ;; Arguments cannot be re-ordered, assumed to be non-associative.
	     (if arg-list
		 (let ((constant-accu '())
		       (arg-accu '()))
		   (dolist (i arg-list)
		     (if (and (consp i) (eq (first i) 'quote))
			 (push (second i) constant-accu)
			 (progn
			   (when constant-accu
			     (push (list 'quote (apply function (nreverse constant-accu))) arg-accu)
			     (setf constant-accu nil))
			   (push i arg-accu))))
		   (if arg-accu
		       (nconc (list function)
			      (nreverse arg-accu)
			      (when constant-accu
				(list (list 'quote (apply function (nreverse constant-accu))))))
		       (list 'quote (apply function (nreverse constant-accu)))))
		 (list 'quote (funcall function))))
	    ((nil) nil))))))

;;; FIXME: should be careful to avoid propagating lambdas to functions other than funcall.
(defun cp-function-form (form)
  (cp-implicit-progn (cdr form))
  (or (constant-fold (car form) (cdr form))
      form))

(defun cp-variable (form)
  (let ((val (assoc form *known-variables*)))
    (if val
	(progn
	  (incf *change-count*)
	  (decf (lexical-variable-use-count form))
	  (copy-form (second val)))
	form)))

(defun cp-lambda (form)
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (cp-form (second arg))))
    (cp-implicit-progn (lambda-information-body form)))
  form)

;;; Initialize constant folders.
(dolist (x '((sys.int::%simple-array-length ((satisfies sys.int::%simple-array-p)))
	     (char-code (character))
	     (eq (t t))
	     (eql (t t))
	     (not (t))
	     (null (t))
	     (schar (simple-string fixnum))
	     (1+ (number))
	     (1- (number))
	     (+ :commutative-arithmetic)
	     (* :commutative-arithmetic)
	     (logand :commutative-arithmetic)
	     (logeqv :commutative-arithmetic)
	     (logior :commutative-arithmetic)
	     (logxor :commutative-arithmetic)
             (sys.int::binary-+ :commutative-arithmetic)
	     (sys.int::binary-* :commutative-arithmetic)
	     (sys.int::binary-logand :commutative-arithmetic)
	     (sys.int::binary-logeqv :commutative-arithmetic)
	     (sys.int::binary-logior :commutative-arithmetic)
	     (sys.int::binary-logxor :commutative-arithmetic)))
  (setf (get (first x) 'constant-fold-mode) (second x)))
