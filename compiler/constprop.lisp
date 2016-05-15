;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Constant folding & propagation.

(in-package :sys.c)

(defvar *known-variables* nil
  "An alist mapping lexical-variables to their values, if known.")

(defparameter *constprop-lambda-copy-limit* 3)

(defun constprop (form)
  (let ((*known-variables* '()))
    (cp-form form)))

(defgeneric cp-form (form))

(defun form-value (form &key (reduce-use-count t))
  "Return the value of form wrapped in quote if its known, otherwise return nil."
  (cond ((or (typep form 'ast-quote)
             (typep form 'ast-function)
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

(defmethod cp-form ((form ast-block))
  (flush-mutable-variables)
  (setf (body form) (cp-form (body form)))
  form)

(defmethod cp-form ((form ast-function))
  form)

(defmethod cp-form ((form ast-go))
  (setf (info form) (cp-form (info form)))
  form)

(defmethod cp-form ((form ast-if))
  (flet ((pick-branch (use-this-one kill-this-one)
           (declare (ignore kill-this-one))
           ;; Disabled for now. SBCL seems to be turning print-circle off while printing?
           #+nil(unless (typep kill-this-one 'ast-quote)
             (warn 'sys.int::simple-style-warning
                   :format-control "Deleting unreacable code: ~S."
                   :format-arguments (list kill-this-one)))
           (cp-form use-this-one)))
    (setf (test form) (cp-form (test form)))
    (let ((value (form-value (test form))))
      (cond ((and value
                  (not (lexical-variable-p value)))
             (change-made)
             (if (and (typep value 'ast-quote)
                      (eql (value value) 'nil))
                 ;; Use the else branch.
                 (pick-branch (if-else form) (if-then form))
                 ;; Use the true branch.
                 (pick-branch (if-then form) (if-else form))))
            (t
             (flush-mutable-variables)
             (setf (if-then form) (cp-form (if-then form)))
             (setf (if-else form) (cp-form (if-else form)))
             form)))))

(defmethod cp-form ((form ast-let))
  (let ((*known-variables* *known-variables*))
    (dolist (b (bindings form))
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
                       (typep val 'ast-function)
                       (and (lexical-variable-p val)
                            (localp val)
                            (eql (lexical-variable-write-count val) 0))))
          (push (list var val 0 b) *known-variables*))))
    ;; Run on the body, with the new constants.
    (setf (body form) (cp-form (body form)))
    form))

(defmethod cp-form ((form ast-multiple-value-bind))
  (setf (value-form form) (cp-form (value-form form))
        (body form) (cp-form (body form)))
  form)

(defmethod cp-form ((form ast-multiple-value-call))
  (setf (function-form form) (cp-form (function-form form))
        (value-form form) (cp-form (value-form form)))
  form)

(defmethod cp-form ((form ast-multiple-value-prog1))
  (setf (value-form form) (cp-form (value-form form))
        (body form) (cp-form (body form)))
  form)

(defmethod cp-form ((form ast-progn))
  (cp-implicit-progn (forms form))
  form)

(defmethod cp-form ((form ast-quote))
  form)

(defmethod cp-form ((form ast-return-from))
  (setf (value form) (cp-form (value form))
        (info form) (cp-form (info form)))
  form)

(defmethod cp-form ((form ast-setq))
  ;; Walk the value form.
  (setf (value form) (cp-form (value form)))
  (let* ((info (assoc (setq-variable form) *known-variables*))
         (value (value form)))
    (if info
        (cond ((or (and (lambda-information-p value)
                        (<= (getf (lambda-information-plist value) 'copy-count 0)
                            *constprop-lambda-copy-limit*))
                   (typep value 'ast-quote)
                   (typep value 'ast-function))
               ;; Always propagate the new value forward.
               (setf (second info) value)
               ;; The value is constant. Attempt to push it back to the
               ;; original binding.
               (cond ((zerop (third info))
                      ;; Send it back, and replace this form with the variable.
                      (change-made)
                      (setf (second info) value)
                      (setf (second (fourth info)) value)
                      ;; Prevent future SETQ forms from back-propgating values.
                      (incf (third info))
                      (setq-variable form))
                     (t ;; Leave this form alone.
                      form)))
              (t ;; Non-constant, flush.
               (flush-mutable-variable (setq-variable form))
               form))
        form)))

(defmethod cp-form ((form ast-tagbody))
  (flush-mutable-variables)
  (setf (statements form)
        (loop
           for (go-tag statement) in (statements form)
           collect (list go-tag (cp-form statement))))
  form)

(defmethod cp-form ((form ast-the))
  (setf (value form) (cp-form (value form)))
  form)

(defmethod cp-form ((form ast-unwind-protect))
  (setf (protected-form form) (cp-form (protected-form form))
        (cleanup-function form) (cp-form (cleanup-function form)))
  form)

(defmethod cp-form ((form ast-jump-table))
  (setf (value form) (cp-form (value form)))
  (cp-implicit-progn (targets form))
  form)

(defun constant-fold (function arg-list)
  ;; Bail out in case of errors.
  (ignore-errors
    (let ((mode (get function 'constant-fold-mode)))
      (if (consp mode)
          (ast `(quote ,(apply function
                               (mapcar (lambda (thing type)
                                         ;; Bail out if thing is non-constant or does not match the type.
                                         (unless (and (typep thing 'ast-quote)
                                                      (typep (value thing) type))
                                           (return-from constant-fold nil))
                                         (value thing))
                                       arg-list mode))))
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
                     (ast `(call ,function
                                 (quote ,value)
                                 ,@nonconst-args))
                     (ast `(quote ,value))))))
            (:arithmetic
             ;; Arguments cannot be re-ordered, assumed to be non-associative.
             (if arg-list
                 (let ((constant-accu '())
                       (arg-accu '()))
                   (dolist (i arg-list)
                     (cond ((typep i 'ast-quote)
                            (push (value i) constant-accu))
                           (t
                            (when constant-accu
                              (push (ast `(quote ,(apply function (nreverse constant-accu))))
                                    arg-accu)
                              (setf constant-accu nil))
                            (push i arg-accu))))
                   (if arg-accu
                       (ast `(call ,function
                                   ,@(nreverse arg-accu)
                                   ,@(when constant-accu
                                       (list `(quote ,(apply function (nreverse constant-accu)))))))
                       (ast `(quote ,(apply function (nreverse constant-accu))))))
                 (ast `(quote ,(funcall function)))))
            ((nil) nil))))))

;;; FIXME: should be careful to avoid propagating lambdas to functions other than funcall.
(defmethod cp-form ((form ast-call))
  (cp-implicit-progn (arguments form))
  (or (constant-fold (name form) (arguments form))
      form))

(defmethod cp-form ((form lexical-variable))
  (let ((val (assoc form *known-variables*)))
    (cond (val
           (change-made)
           (when (lambda-information-p (second val))
             (incf (getf (lambda-information-plist (second val)) 'copy-count 0)))
           (decf (lexical-variable-use-count form))
           (incf (third val))
           (copy-form (second val)))
          (t form))))

(defmethod cp-form ((form lambda-information))
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
             (sys.int::binary-logxor (integer integer))
             (mezzano.runtime::left-shift (integer integer))
             (mezzano.runtime::right-shift (integer integer))
             (sys.int::binary-< (number number))
             (sys.int::binary-<= (number number))
             (sys.int::binary-> (number number))
             (sys.int::binary->= (number number))
             (sys.int::binary-= (number number))
             (mezzano.runtime::%fixnum-< (integer integer))
             (sys.int::fixnump (t))
             (byte-size (byte))
             (byte-position (byte))))
  (setf (get (first x) 'constant-fold-mode) (second x)))
