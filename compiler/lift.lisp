;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This is the compiler pass that performs lambda-lifting.

(in-package :sys.c)

(defun lambda-lift (lambda)
  (ll-form lambda))

(defgeneric ll-form (form))

(defun ll-implicit-progn (x)
  (do ((i x (cdr i)))
      ((endp i))
    (setf (car i) (ll-form (car i)))))

(defmethod ll-form ((form ast-block))
  (unless (eql (lexical-variable-definition-point (info form)) *current-lambda*)
    (change-made)
    ;; Update the definition point.
    (setf (lexical-variable-definition-point (info form)) *current-lambda*))
  (setf (body form) (ll-form (body form)))
  form)

(defmethod ll-form ((form ast-function))
  form)

(defmethod ll-form ((form ast-go))
  (setf (info form) (ll-form (info form)))
  form)

(defmethod ll-form ((form ast-if))
  (setf (test form) (ll-form (test form))
        (if-then form) (ll-form (if-then form))
        (if-else form) (ll-form (if-else form)))
  form)

(defmethod ll-form ((form ast-let))
  ;; Patch up definition points after a lambda has been lifted.
  (dolist (binding (bindings form))
    (when (and (lexical-variable-p (first binding))
               (not (eql (lexical-variable-definition-point (first binding)) *current-lambda*)))
      (change-made)
      (setf (lexical-variable-definition-point (first binding)) *current-lambda*))
    (setf (second binding) (ll-form (second binding))))
  (setf (body form) (ll-form (body form)))
  form)

(defmethod ll-form ((form ast-multiple-value-bind))
  ;; Patch up definition points after a lambda has been lifted.
  (dolist (var (bindings form))
    (when (and (lexical-variable-p var)
               (not (eql (lexical-variable-definition-point var) *current-lambda*)))
      (change-made)
      (setf (lexical-variable-definition-point var) *current-lambda*)))
  (setf (value-form form) (ll-form (value-form form))
        (body form) (ll-form (body form)))
  form)

;; Reduce (multiple-value-call #'(lambda (&optional ... &rest unused) ...) value-form)
;; back down to (multiple-value-bind (vars...) value-form body...).
(defmethod ll-form ((form ast-multiple-value-call))
  (let ((fn (function-form form)))
    (cond ((and (lambda-information-p fn)
                (null (lambda-information-required-args fn))
                (lambda-information-rest-arg fn)
                (not (lambda-information-enable-keys fn))
                (not (lambda-information-environment-arg fn))
                (not (lambda-information-fref-arg fn))
                (not (lambda-information-closure-arg fn))
                (not (lambda-information-count-arg fn))
                (every (lambda (x)
                         (and (typep (second x) 'ast-quote)
                              (eql (value (second x)) 'nil) ; An init-form of NIL.
                              (eql (third x) nil)))    ; No suppliedp arg.
                       (lambda-information-optional-args (function-form form)))
                (lexical-variable-p (lambda-information-rest-arg fn))
                (zerop (lexical-variable-use-count (lambda-information-rest-arg fn))))
           (change-made)
           ;; Variable definition points will be fixed up by LL-MULTIPLE-VALUE-BIND.
           (ll-form (make-instance 'ast-multiple-value-bind
                                   :inherit form
                                   :bindings (mapcar 'first (lambda-information-optional-args fn))
                                   :value-form (value-form form)
                                   :body (lambda-information-body fn))))
          (t (setf (function-form form) (ll-form (function-form form))
                   (value-form form) (ll-form (value-form form)))
             form))))

(defmethod ll-form ((form ast-multiple-value-prog1))
  (setf (value-form form) (ll-form (value-form form))
        (body form) (ll-form (body form)))
  form)

(defmethod ll-form ((form ast-progn))
  (ll-implicit-progn (forms form))
  form)

(defmethod ll-form ((form ast-quote))
  form)

(defmethod ll-form ((form ast-return-from))
  (setf (value form) (ll-form (value form))
        (info form) (ll-form (info form)))
  form)

(defmethod ll-form ((form ast-setq))
  (setf (value form) (ll-form (value form)))
  form)

(defmethod ll-form ((form ast-tagbody))
  (unless (eq (lexical-variable-definition-point (info form)) *current-lambda*)
    (change-made)
    (setf (lexical-variable-definition-point (info form)) *current-lambda*))
  (setf (statements form)
        (loop
           for (go-tag statement) in (statements form)
           collect (list go-tag (ll-form statement))))
  form)

(defmethod ll-form ((form ast-the))
  (setf (value form) (ll-form (value form)))
  form)

(defmethod ll-form ((form ast-unwind-protect))
  (setf (protected-form form) (ll-form (protected-form form))
        (cleanup-function form) (ll-form (cleanup-function form)))
  form)

(defmethod ll-form ((form ast-jump-table))
  (setf (value form) (ll-form (value form)))
  (ll-implicit-progn (targets form))
  form)

;; Doesn't support fuzzy allow-other-keys matching.
(defun arguments-match-lambda-list (lambda arg-list)
  (let ((arg-count (length arg-list))
        (req-count (length (lambda-information-required-args lambda)))
        (opt-count (length (lambda-information-optional-args lambda))))
    (cond ((lambda-information-enable-keys lambda)
           (let ((keywords (mapcar 'caar (lambda-information-key-args lambda))))
             (when (and (>= arg-count req-count)
                        (evenp (- arg-count (+ req-count opt-count))))
               (do ((i (nthcdr (+ req-count opt-count) arg-list) (cddr i)))
                   ((null i)
                    t)
                 (unless (and (typep (car i) 'ast-quote)
                              (member (value (car i)) keywords))
                   (return nil))))))
          ((lambda-information-rest-arg lambda)
           (<= req-count arg-count))
          (t (<= req-count arg-count (+ req-count opt-count))))))

(defun lift-lambda (lambda arg-list)
  (let ((name (or (lambda-information-name lambda) 'lambda))
        (required-args (lambda-information-required-args lambda))
        (optional-args (lambda-information-optional-args lambda))
        (rest-arg (lambda-information-rest-arg lambda))
        (key-args (lambda-information-key-args lambda)))
    (when (getf (lambda-information-plist lambda) 'notinline)
      (return-from lift-lambda))
    (when (or (lambda-information-fref-arg lambda)
              (lambda-information-closure-arg lambda)
              (lambda-information-count-arg lambda))
      (return-from lift-lambda))
    (when (lambda-information-environment-arg lambda)
      (warn 'sys.int::simple-style-warning
            :format-control "Not inlining ~S, has environment arg."
            :format-arguments (list name))
      (return-from lift-lambda))
    (when (and rest-arg
               (typep rest-arg 'lexical-variable)
               (lexical-variable-dynamic-extent rest-arg))
      ;; Not implemented yet.
      (warn 'sys.int::simple-style-warning
            :format-control "Not inlining ~S, has dynamic-extent &REST arg."
            :format-arguments (list name))
      (return-from lift-lambda))
    ;; Attempt to match the argument list with the function's lambda list.
    (unless (arguments-match-lambda-list lambda arg-list)
      ;; Bail out.
      (warn 'simple-warning
            :format-control "Not inlining ~S, arguments do not match."
            :format-arguments (list name))
      (return-from lift-lambda))
    (change-made)
    ;; Fix argument definition points.
    (dolist (arg required-args)
      (when (lexical-variable-p arg)
        (setf (lexical-variable-definition-point arg) *current-lambda*)))
    (dolist (arg optional-args)
      (when (lexical-variable-p (first arg))
        (setf (lexical-variable-definition-point (first arg)) *current-lambda*))
      (when (lexical-variable-p (third arg))
        (setf (lexical-variable-definition-point (third arg)) *current-lambda*)))
    (when (lexical-variable-p rest-arg)
      (setf (lexical-variable-definition-point rest-arg) *current-lambda*))
    (dolist (arg key-args)
      (when (lexical-variable-p (second (first arg)))
        (setf (lexical-variable-definition-point (second (first arg))) *current-lambda*))
      (when (lexical-variable-p (third arg))
        (setf (lexical-variable-definition-point (third arg)) *current-lambda*)))
    (let* ((argument-vars (mapcar (lambda (x)
                                    (declare (ignore x))
                                    (make-instance 'lexical-variable
                                                   :inherit lambda
                                                   :name (gensym)
                                                   :definition-point *current-lambda*
                                                   :ignore :maybe))
                                  arg-list))
           (key-pairs (nthcdr (length required-args) argument-vars)))
      (labels ((var-and-suppliedp-bindings (var value suppliedp suppliedp-value)
                 (if suppliedp
                     `((,var ,value)
                       (,suppliedp ,suppliedp-value))
                     `((,var ,value))))
               (build-required-bindings (req-args arg-vars)
                 (cond (req-args
                        (ast `(let ((,(first req-args) ,(first arg-vars)))
                                ,(build-required-bindings (rest req-args) (rest arg-vars)))
                             lambda))
                       (t (build-optional-bindings optional-args arg-vars))))
               (build-optional-bindings (opt-args arg-vars)
                 (cond ((and opt-args arg-vars)
                        (destructuring-bind (var init-form suppliedp)
                            (first opt-args)
                          (declare (ignore init-form))
                          (ast `(let ,(var-and-suppliedp-bindings var (first arg-vars)
                                                                  suppliedp '(quote t))
                                  ,(build-optional-bindings (rest opt-args) (rest arg-vars)))
                               lambda)))
                       (opt-args
                        (destructuring-bind (var init-form suppliedp)
                            (first opt-args)
                          (ast `(let ,(var-and-suppliedp-bindings var init-form
                                                                  suppliedp '(quote nil))
                                  ,(build-optional-bindings (rest opt-args) '()))
                               lambda)))
                       (t (build-rest-binding arg-vars))))
               (build-rest-binding (arg-vars)
                 (if rest-arg
                     (ast `(let ((,rest-arg (call list ,@arg-vars)))
                             ,(build-key-bindings key-args))
                          lambda)
                     (build-key-bindings key-args)))
               (build-key-bindings (keys)
                 (cond (keys
                        (destructuring-bind ((keyword var) init-form suppliedp)
                            (first keys)
                          (do ((p 0 (+ p 2))
                               (i (nthcdr (+ (length required-args)
                                             (length optional-args))
                                          arg-list)
                                  (cddr i)))
                              ((null i)
                               ;; Not provided, use the initform.
                               (ast `(let ,(var-and-suppliedp-bindings var init-form
                                                                       suppliedp '(quote nil))
                                       ,(build-key-bindings (rest keys)))
                                    lambda))
                            (when (eql (value (car i)) keyword)
                              ;; Keywords match, use this argument.
                              (return (ast `(let ,(var-and-suppliedp-bindings var (nth (1+ p) key-pairs)
                                                                              suppliedp '(quote t))
                                              ,(build-key-bindings (rest keys)))
                                           lambda))))))
                       (t (ll-form (lambda-information-body lambda))))))
        ;; Evaluate arguments.
        (ast `(let ,(mapcar #'list argument-vars arg-list)
                ,(build-required-bindings required-args argument-vars))
             lambda)))))

(defmethod ll-form ((form ast-call))
  (ll-implicit-progn (arguments form))
  (if *should-inline-functions*
      (or (and (eql (name form) 'mezzano.runtime::%funcall)
               (lambda-information-p (first (arguments form)))
               (lift-lambda (first (arguments form)) (rest (arguments form))))
          ;; Couldn't lift.
          form)
      form))

(defmethod ll-form ((form lexical-variable))
  form)

(defmethod ll-form ((form lambda-information))
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (ll-form (second arg))))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (ll-form (second arg))))
    (setf (lambda-information-body form) (ll-form (lambda-information-body form))))
  form)
