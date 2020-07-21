;;;; Constant folding & propagation.

(in-package :mezzano.compiler)

(defvar *known-variables* nil
  "An alist mapping lexical-variables to their values, if known.")

(defparameter *constprop-lambda-copy-limit* 3)
(defparameter *constant-fold-modes* (make-hash-table :test 'equal :enforce-gc-invariant-keys t))
(defparameter *constant-fold-modes-lock* (mezzano.supervisor:make-rw-lock '*constant-fold-modes*))

(defun constprop (lambda architecture)
  (declare (ignore architecture))
  (detect-uses lambda)
  (let ((*known-variables* '()))
    (cp-form lambda)))

(defgeneric cp-form (form))

(defun form-value (form)
  "Return the value of form wrapped in quote if its known, otherwise return nil."
  (cond ((or (typep form 'ast-quote)
             (constprop-function-ast-p form)
             (lambda-information-p form))
         form)
        ((lexical-variable-p form)
         (let ((val (assoc form *known-variables*)))
           (when val
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
  (setf (body form) (cp-form (body form)))
  form)

(defmethod cp-form ((form ast-function))
  form)

(defmethod cp-form ((form ast-go))
  (setf (info form) (cp-form (info form)))
  (flush-mutable-variables)
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
                  (not (lexical-variable-p (unwrap-the value))))
             (change-made)
             (if (and (typep value 'ast-quote)
                      (eql (value value) 'nil))
                 ;; Use the else branch.
                 (pick-branch (if-else form) (if-then form))
                 ;; Use the true branch.
                 (pick-branch (if-then form) (if-else form))))
            ((and (typep (test form) 'ast-the)
                  ;; Type must equal NULL.
                  (compiler-valid-type-equal-p (the-type (test form)) 'null))
             (change-made)
             ;; Use the else branch.
             (pick-branch (if-else form) (if-then form)))
            ((and (typep (test form) 'ast-the)
                  (compiler-valid-subtypep (the-type (test form)) '(not null)))
             (change-made)
             ;; Use the true branch.
             (pick-branch (if-then form) (if-else form)))
            (t
             (flush-mutable-variables)
             (setf (if-then form) (cp-form (if-then form)))
             (setf (if-else form) (cp-form (if-else form)))
             form)))))

(defun unwrap-the (form)
  (loop
     (when (not (typep form 'ast-the))
       (return form))
     (setf form (ast-value form))))

(defun unwrapped-the-type (form)
  (cond ((typep form 'ast-the)
         (cond ((typep (ast-value form) 'ast-the)
                (merge-the-types
                 (ast-the-type form)
                 (unwrapped-the-type (ast-value form))))
               (t
                (ast-the-type form))))
        (t 't)))

(defun constprop-function-ast-p (ast)
  ;; #'FOO is only pure if FOO is inlinable.
  (and (typep ast 'ast-function)
       (or (let* ((name (ast-name ast))
                  (decls (assoc name (ast-inline-declarations ast))))
             (and (or (function-inline-info name)
                      (eql (second decls) 'inline))
                  (not (eql (second decls) 'notinline))))
           ;; Everything in the CL package is a candidate for constprop.
           (let ((true-name (if (symbolp (ast-name ast))
                                (ast-name ast)
                                (second (ast-name ast)))))
             (eql (symbol-package true-name)
                  (find-package 'cl))))))

(defun constprop-value-p (form)
  (let ((unwrapped (unwrap-the form)))
    (or (lambda-information-p unwrapped)
        (typep unwrapped 'ast-quote)
        (constprop-function-ast-p unwrapped)
        (and (typep unwrapped 'ast-call)
             (eql (ast-name unwrapped) 'byte)
             (eql (length (ast-arguments unwrapped)) 2)
             (constprop-value-p (first (ast-arguments unwrapped)))
             (constprop-value-p (second (ast-arguments unwrapped))))
        (and (lexical-variable-p unwrapped)
             (localp unwrapped)
             (eql (lexical-variable-write-count unwrapped) 0)))))

(defun copyable-value-p (form variable)
  (let ((unwrapped (unwrap-the form)))
    (and (constprop-value-p unwrapped)
         (or (not (lambda-information-p unwrapped))
             (eql (lexical-variable-use-count variable) 1)
             (<= (getf (lambda-information-plist unwrapped) 'copy-count 0)
                 *constprop-lambda-copy-limit*)))))

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
        (when (lexical-variable-p var)
          (cond ((copyable-value-p val var)
                 (push (list var val 0 b) *known-variables*))
                ((and (typep val 'ast-the)
                      (constprop-value-p var))
                 (push (list var (ast `(the ,(the-type val) ,var) val) 0 b) *known-variables*))))))
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
  (flush-mutable-variables)
  form)

(defmethod cp-form ((form ast-setq))
  ;; Walk the value form.
  (setf (value form) (cp-form (value form)))
  (let* ((info (assoc (setq-variable form) *known-variables*))
         (value (value form)))
    (cond ((null info)
           form)
          ((and (localp (setq-variable form))
                (or (and (lambda-information-p value)
                         (<= (getf (lambda-information-plist value) 'copy-count 0)
                             *constprop-lambda-copy-limit*))
                    (typep value 'ast-quote)
                    (constprop-function-ast-p value)))
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
           form))))

(defmethod cp-form ((form ast-tagbody))
  ;; If the entry statement has one use, then we can descend into it.
  (cond ((eql (go-tag-use-count (first (first (statements form)))) 1)
         (setf (second (first (statements form)))
               (cp-form (second (first (statements form)))))
         (setf (rest (statements form))
               (loop
                  for (go-tag statement) in (rest (statements form))
                  do (flush-mutable-variables)
                  collect (list go-tag (cp-form statement)))))
        (t
         (setf (statements form)
               (loop
                  for (go-tag statement) in (statements form)
                  do (flush-mutable-variables)
                  collect (list go-tag (cp-form statement))))))
  (flush-mutable-variables)
  form)

(defmethod cp-form ((form ast-the))
  (let ((val (assoc (ast-value form) *known-variables*)))
    (cond ((and val
                (typep (second val) 'ast-the)
                (or (compiler-valid-type-equal-p (the-type (second val)) (the-type form))
                    (compiler-valid-subtypep (the-type form) (the-type (second val))))
                (eql (ast-value (second val)) (ast-value form))
                (typep (ast-value form) 'lexical-variable))
           ;; Don't do anything. This would replace this form with an identical nested THE.
           form)
          (t
           (setf (value form) (cp-form (value form)))
           form))))

(defmethod cp-form ((form ast-unwind-protect))
  (setf (protected-form form) (cp-form (protected-form form))
        (cleanup-function form) (cp-form (cleanup-function form)))
  form)

(defmethod cp-form ((form ast-jump-table))
  (setf (value form) (cp-form (value form)))
  (cp-implicit-progn (targets form))
  form)

(defun constant-fold (form function arg-list)
  ;; Bail out in case of errors.
  (ignore-errors
    (let* ((info (mezzano.supervisor:with-rw-lock-read (*constant-fold-modes-lock*)
                   (gethash function *constant-fold-modes*)))
           (mode (car info))
           (folder (or (cdr info) function)))
      (etypecase mode
        (cons
         ;; List of arguments & argument types.
         (ast `(quote ,(apply folder
                              (mapcar (lambda (thing type)
                                        ;; Bail out if thing is non-constant or does not match the type.
                                        (setf thing (unwrap-the thing))
                                        (unless (and (typep thing 'ast-quote)
                                                     (typep (value thing) type))
                                          (return-from constant-fold nil))
                                        (value thing))
                                      arg-list mode)))
              form))
        ((eql t)
         ;; Any arguments!
         (ast `(quote ,(apply folder
                              (mapcar (lambda (thing)
                                        ;; Bail out if thing is non-constant or does not match the type.
                                        (setf thing (unwrap-the thing))
                                        (unless (typep thing 'ast-quote)
                                          (return-from constant-fold nil))
                                        (value thing))
                                      arg-list)))
              form))
        ((eql :commutative-arithmetic)
         ;; Arguments can be freely re-ordered, assumed to be associative.
         ;; Addition, multiplication and the logical operators use this.
         ;; FIXME: Float arithemetic is non-commutative.
         (let ((const-args '())
               (nonconst-args '())
               (value nil))
           (dolist (arg arg-list)
             (let ((unwrapped (unwrap-the arg)))
               (if (typep unwrapped 'ast-quote)
                   (push (value unwrapped) const-args)
                   (push arg nonconst-args))))
           (setf const-args (nreverse const-args)
                 nonconst-args (nreverse nonconst-args))
           (when (or const-args (not nonconst-args))
             (setf value (apply folder const-args))
             (if nonconst-args
                 ;; Leave the constant value as the rightmost operand.
                 ;; The rest of the compiler expects this.
                 (ast `(call ,function
                             ,@nonconst-args
                             (quote ,value))
                      form)
                 (ast `(quote ,value)
                      form)))))
        ((eql :arithmetic)
         ;; Arguments cannot be re-ordered, assumed to be non-associative.
         (if arg-list
             (let ((constant-accu '())
                   (arg-accu '()))
               (dolist (arg arg-list)
                 (let ((unwrapped (unwrap-the arg)))
                   (cond ((typep unwrapped 'ast-quote)
                          (push (value unwrapped) constant-accu))
                         (t
                          (when constant-accu
                            (push (ast `(quote ,(apply folder (nreverse constant-accu)))
                                       form)
                                  arg-accu)
                            (setf constant-accu nil))
                          (push arg arg-accu)))))
               (if arg-accu
                   (ast `(call ,function
                               ,@(nreverse arg-accu)
                               ,@(when constant-accu
                                       (list `(quote ,(apply folder (nreverse constant-accu))))))
                        form)
                   (ast `(quote ,(apply folder (nreverse constant-accu)))
                        form)))
             (ast `(quote ,(funcall function))
                  form)))
        ((eql nil) nil)))))

;;; FIXME: should be careful to avoid propagating lambdas to functions other than funcall.
(defmethod cp-form ((form ast-call))
  (cp-implicit-progn (arguments form))
  (or (and (not (eql (second (assoc (name form) (ast-inline-declarations form))) 'notinline))
           (constant-fold form (name form) (arguments form)))
      form))

(defmethod cp-form ((form lexical-variable))
  (let ((val (assoc form *known-variables*)))
    (cond (val
           (change-made)
           (when (lambda-information-p (unwrap-the (second val)))
             (incf (getf (lambda-information-plist (unwrap-the (second val))) 'copy-count 0)))
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

(defun mark-as-constant-foldable (name &key (mode t) folder)
  (mezzano.supervisor:with-rw-lock-write (*constant-fold-modes-lock*)
    (setf (gethash name *constant-fold-modes*) (cons mode folder))))

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
             (sys.int::binary-= :commutative-arithmetic =)
             (sys.int::binary-+ :commutative-arithmetic +)
             (sys.int::binary-- (number number) -)
             (sys.int::binary-* :commutative-arithmetic *)
             (sys.int::binary-/ (number number) /)
             (sys.int::binary-logand (integer integer) logand)
             (sys.int::generic-logand (integer integer) logand)
             (mezzano.runtime::%fixnum-logand (integer integer))
             (mezzano.compiler::%fast-fixnum-logand (integer integer))
             (sys.int::binary-logeqv (integer integer) logeqv)
             (sys.int::generic-logeqv (integer integer) logeqv)
             (sys.int::binary-logior (integer integer) logior)
             (sys.int::generic-logior (integer integer) logior)
             (mezzano.runtime::%fixnum-logior (integer integer))
             (sys.int::binary-logxor (integer integer) logxor)
             (sys.int::generic-logxor (integer integer) logxor)
             (mezzano.runtime::%fixnum-logxor (integer integer))
             (mezzano.runtime::left-shift (integer integer))
             (mezzano.runtime::%fixnum-left-shift (integer integer))
             (mezzano.runtime::right-shift (integer integer))
             (sys.int::binary-< (number number) <)
             (sys.int::binary-<= (number number) <=)
             (sys.int::binary-> (number number) >)
             (sys.int::binary->= (number number) >=)
             (sys.int::binary-= (number number) =)
             (mezzano.runtime::%fixnum-+ (integer integer) +)
             (mezzano.runtime::%fixnum-- (integer integer) -)
             (mezzano.runtime::%fixnum-* (integer integer) *)
             (mezzano.runtime::%fixnum-< (integer integer) <)
             (%fast-fixnum-+ (integer integer) +)
             (%fast-fixnum-- (integer integer) -)
             (%fast-fixnum-* (integer integer) *)
             (sys.int::generic-+ (number number) +)
             (sys.int::generic-- (number number) -)
             (sys.int::generic-* (number number) *)
             (sys.int::fixnump (t))
             (numberp (t))
             (complexp (t))
             (realp (t))
             (rationalp (t))
             (integerp (t))
             (symbolp (t))
             (byte (integer integer))
             (byte-size (byte))
             (byte-position (byte))
             (keywordp (symbol))
             (sys.int::%type-check (t fixnum t))
             (float (t t))
             (expt (t t))))
  (mark-as-constant-foldable (first x)
                             :mode (second x)
                             :folder (third x)))
