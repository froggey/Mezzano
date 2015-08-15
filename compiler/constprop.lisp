;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Constant folding & propagation.

(in-package :sys.c)

(defvar *known-variables* nil
  "An alist mapping lexical-variables to their values, if known.")

(defparameter *constprop-lambda-copy-limit* 3)

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
	    ((multiple-value-bind) (cp-multiple-value-bind form))
	    ((multiple-value-call) (cp-multiple-value-call form))
	    ((multiple-value-prog1) (cp-multiple-value-prog1 form))
	    ((progn) (cp-progn form))
	    ((function) (cp-quote form))
            ((quote) (error "old style ast"))
	    ((return-from) (cp-return-from form))
	    ((setq) (cp-setq form))
	    ((tagbody) (cp-tagbody form))
	    ((the) (cp-the form))
	    ((unwind-protect) (cp-unwind-protect form))
	    (t (cp-function-form form))))
    (ast-quote (cp-quote form))
    (lexical-variable (cp-variable form))
    (lambda-information (cp-lambda form))))

(defun form-value (form &key (reduce-use-count t))
  "Return the value of form wrapped in quote if its known, otherwise return nil."
  (cond ((or (typep form 'ast-quote)
             (and (consp form)
		  (member (first form) '(function)))
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
	   #+nil(unless (typep kill-this-one 'ast-quote)
	     (warn 'sys.int::simple-style-warning
		   :format-control "Deleting unreacable code: ~S."
		   :format-arguments (list kill-this-one)))
	   (flush-form kill-this-one)
	   (cp-form use-this-one)))
    (setf (second form) (cp-form (second form)))
    (let ((value (form-value (second form))))
      (cond ((and value
                  (not (lexical-variable-p value)))
             (change-made)
             (if (and (typep value 'ast-quote)
                      (eql (value value) 'nil))
                 ;; Use the else branch.
                 (pick-branch (fourth form) (third form))
                 ;; Use the true branch.
                 (pick-branch (third form) (fourth form))))
            (t
             (flush-mutable-variables)
             (setf (third form) (cp-form (third form)))
             (setf (fourth form) (cp-form (fourth form)))
             form)))))

(defun cp-let (form)
  (let ((*known-variables* *known-variables*))
    (dolist (b (second form))
      (let ((var (first b))
	    (val (second b)))
	;; Run on the init-form.
	(setf val (setf (second b) (cp-form val)))
	;; Add variables to the new constants list.
        ;; Non-constant variables will be flushed when a BLOCK, TAGBODY
        ;; or lambda is seen.
	(when (and (lexical-variable-p var)
		   (or (and (lambda-information-p val)
                            (<= (getf (lambda-information-plist val) 'copy-count 0)
                                *constprop-lambda-copy-limit*))
                       (typep val 'ast-quote)
		       (and (consp val)
                            (member (first val) '(function)))
		       (and (lexical-variable-p val)
			    (localp val)
			    (eql (lexical-variable-write-count val) 0))))
	  (push (list var val 0 b) *known-variables*))))
    ;; Run on the body, with the new constants.
    (cp-implicit-progn (cddr form))
    form))

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
        (cond ((or (and (lambda-information-p (third form))
                        (<= (getf (lambda-information-plist (third form)) 'copy-count 0)
                            *constprop-lambda-copy-limit*))
                   (typep (third form) 'ast-quote)
                   (and (consp (third form))
                        (member (first (third form)) '(function))))
               ;; Always propagate the new value forward.
               (setf (second info) (third form))
               ;; The value is constant. Attempt to push it back to the
               ;; original binding.
               (cond ((zerop (third info))
                      ;; Send it back, and replace this form with the variable.
                      (change-made)
                      (setf (second info) (third form))
                      (setf (second (fourth info)) (third form))
                      ;; Prevent future SETQ forms from back-propgating values.
                      (incf (third info))
                      (second form))
                     (t ;; Leave this form alone.
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
	  (make-instance 'ast-quote
                         :value (apply function
                                       (mapcar (lambda (thing type)
                                                 ;; Bail out if thing is non-constant or does not match the type.
                                                 (unless (and (typep thing 'ast-quote)
                                                              (typep (value thing) type))
                                                   (return-from constant-fold nil))
                                                 (value thing))
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
		 (if (typep i 'ast-quote)
		     (push (value i) const-args)
		     (push i nonconst-args)))
	       (setf const-args (nreverse const-args)
		     nonconst-args (nreverse nonconst-args))
	       (when (or const-args (not nonconst-args))
		 (setf value (apply function const-args))
		 (if nonconst-args
		     (list* function (make-instance 'ast-quote :value value) nonconst-args)
		     (make-instance 'ast-quote :value value)))))
	    (:arithmetic
	     ;; Arguments cannot be re-ordered, assumed to be non-associative.
	     (if arg-list
		 (let ((constant-accu '())
		       (arg-accu '()))
		   (dolist (i arg-list)
		     (if (typep i 'ast-quote)
			 (push (value i) constant-accu)
			 (progn
			   (when constant-accu
			     (push (make-instance 'ast-quote
                                                  :value (apply function (nreverse constant-accu)))
                                   arg-accu)
			     (setf constant-accu nil))
			   (push i arg-accu))))
		   (if arg-accu
		       (nconc (list function)
			      (nreverse arg-accu)
			      (when constant-accu
				(list (make-instance 'ast-quote
                                                     :value (apply function (nreverse constant-accu))))))
		       (make-instance 'ast-quote
                                      :value (apply function (nreverse constant-accu)))))
		 (make-instance 'ast-quote :value (funcall function))))
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
	  (change-made)
          (when (lambda-information-p (second val))
            (incf (getf (lambda-information-plist (second val)) 'copy-count 0)))
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
    (setf (lambda-information-body form) (cp-form (lambda-information-body form))))
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
             (lognot (integer))
             (sys.int::binary-= :commutative-arithmetic)
             (sys.int::binary-+ :commutative-arithmetic)
             (sys.int::binary-- (number number))
	     (sys.int::binary-* :commutative-arithmetic)
	     (sys.int::binary-logand (integer integer))
	     (sys.int::binary-logeqv (integer integer))
	     (sys.int::binary-logior (integer integer))
	     (sys.int::binary-logxor (integer integer))))
  (setf (get (first x) 'constant-fold-mode) (second x)))
