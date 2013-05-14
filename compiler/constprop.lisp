;;;; Constant folding & propagation.

(in-package :sys.c)

(defvar *known-variables* nil
  "An alist mapping lexical-variables to their values, if known.")

(defun constprop (form)
  (let ((*known-variables* '()))
    (cp-form form)))

(defun cp-form (form)
  (etypecase form
    (cons (case (first form)
	    ((block) (cp-block form))
	    ((go) (cp-go form))
	    ((if) (cp-if form))
	    ((let) (cp-let form))
	    ((load-time-value) (cp-load-time-value form))
	    ((multiple-value-bind) (cp-multiple-value-bind form))
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

(defun flush-mutable-variable (var)
  "Remove a single mutable variables from the *known-variables* list."
  (do ((i *known-variables* (cdr i)))
      ((null i))
    (when (and (first i)
               (eql var (first (first i)))
               (not (zerop (lexical-variable-write-count (first (first i))))))
      (setf (first i) nil))))

(defun flush-mutable-variables ()
  "Remove all mutable variables from the *known-variables* list."
  (do ((i *known-variables* (cdr i)))
      ((null i))
    (when (and (first i)
               (not (zerop (lexical-variable-write-count (first (first i))))))
      (setf (first i) nil))))

(defun cp-implicit-progn (x)
  (do ((i x (cdr i)))
      ((endp i))
    (setf (car i) (cp-form (car i)))))

(defun cp-block (form)
  (flush-mutable-variables)
  (cp-implicit-progn (cddr form))
  form)

(defun cp-go (form)
  form)

(defun cp-if (form)
  (flet ((pick-branch (use-this-one kill-this-one)
	   ;; Disabled for now. SBCL seems to be turning print-circle off while printing?
	   #+nil(unless (and (consp kill-this-one)
			(eq (first kill-this-one) 'quote))
	     (warn 'sys.int::simple-style-warning
		   :format-control "Deleting unreacable code: ~S."
		   :format-arguments (list kill-this-one)))
	   (flush-form kill-this-one)
	   (cp-form use-this-one)))
    (setf (second form) (cp-form (second form)))
    (let ((value (form-value (second form))))
      (if (and value
               (not (lexical-variable-p value)))
	  (progn
	    (incf *change-count*)
	    (if (eql (or (not (consp value)) (second value)) 'nil)
		;; Use the else branch.
		(pick-branch (fourth form) (third form))
		;; Use the true branch.
		(pick-branch (third form) (fourth form))))
	  (progn
            (flush-mutable-variables)
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
	;; Add variables to the new constants list.
        ;; Non-constant variables will be flushed when a BLOCK, TAGBODY
        ;; or lambda is seen.
	(when (and (lexical-variable-p var)
		   (or (lambda-information-p val)
		       (and (consp val) (eq (first val) 'quote))
		       (and (lexical-variable-p val)
			    (localp val)
			    (eql (lexical-variable-write-count val) 0))))
	  (push (list var val 0 b) new-constants))))
    ;; Run on the body, with the new constants.
    (let ((*known-variables* new-constants))
      (cp-implicit-progn (cddr form)))
    form))

;;;(defun cp-load-time-value (form))

(defun cp-multiple-value-bind (form)
  (cp-implicit-progn (cddr form))
  form)

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
  (setf (fourth form) (cp-form (fourth form)))
  form)

(defun cp-setq (form)
  ;; Walk the value form.
  (setf (third form) (cp-form (third form)))
  (let ((info (assoc (second form) *known-variables*)))
    (if info
        (cond ((or (lambda-information-p (third form))
                   (and (consp (third form)) (eq (first (third form)) 'quote)))
               ;; The value is constant. Attempt to push it back to the
               ;; original binding.
               (cond ((zerop (third info))
                      ;; Send it back, and remove this form.
                      (incf *change-count*)
                      (setf (second (fourth info)) (third form))
                      ''nil)
                     (t ;; Just propagate forward.
                      (setf (second info) (third form))
                      form)))
              (t ;; Non-constant, flush.
               (flush-mutable-variable (second form))
               form))
        form)))

(defun cp-tagbody (form)
  (flush-mutable-variables)
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
          (incf (third val))
	  (copy-form (second val)))
	form)))

(defun cp-lambda (form)
  (flush-mutable-variables)
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (cp-form (second arg))))
    (dolist (arg (lambda-information-key-args form))
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
	     (ash (integer integer))
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
