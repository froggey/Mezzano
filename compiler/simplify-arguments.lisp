;;;; Simplify complicated arguments.

(in-package :mezzano.compiler)

(defun lower-arguments (form)
  "Simplify lambda lists so that no lambda argument is special and
so that no &OPTIONAL argument has a non-constant init-form.
Must be run after keywords have been lowered."
  (lower-arguments-1 form)
  form)

(defgeneric lower-arguments-1 (form))

(defmethod lower-arguments-1 ((form ast-block))
  (lower-arguments-1 (body form)))

(defmethod lower-arguments-1 ((form ast-function)))

(defmethod lower-arguments-1 ((form ast-go))
  (lower-arguments-1 (info form)))

(defmethod lower-arguments-1 ((form ast-if))
  (lower-arguments-1 (test form))
  (lower-arguments-1 (if-then form))
  (lower-arguments-1 (if-else form)))

(defmethod lower-arguments-1 ((form ast-let))
  (loop
     for (variable init-form) in (bindings form)
     do (lower-arguments-1 init-form))
  (lower-arguments-1 (body form)))

(defmethod lower-arguments-1 ((form ast-multiple-value-bind))
  (lower-arguments-1 (value-form form))
  (lower-arguments-1 (body form)))

(defmethod lower-arguments-1 ((form ast-multiple-value-call))
  (lower-arguments-1 (function-form form))
  (lower-arguments-1 (value-form form)))

(defmethod lower-arguments-1 ((form ast-multiple-value-prog1))
  (lower-arguments-1 (value-form form))
  (lower-arguments-1 (body form)))

(defmethod lower-arguments-1 ((form ast-progn))
  (mapc #'lower-arguments-1 (forms form)))

(defmethod lower-arguments-1 ((form ast-quote)))

(defmethod lower-arguments-1 ((form ast-return-from))
  (lower-arguments-1 (value form))
  (lower-arguments-1 (info form)))

(defmethod lower-arguments-1 ((form ast-setq))
  (lower-arguments-1 (value form)))

(defmethod lower-arguments-1 ((form ast-tagbody))
  (loop
     for (go-tag statement) in (statements form)
     do (lower-arguments-1 statement)))

(defmethod lower-arguments-1 ((form ast-the))
  (lower-arguments-1 (value form)))

(defmethod lower-arguments-1 ((form ast-unwind-protect))
  (lower-arguments-1 (protected-form form))
  (lower-arguments-1 (cleanup-function form)))

(defmethod lower-arguments-1 ((form ast-call))
  (mapc #'lower-arguments-1 (arguments form)))

(defmethod lower-arguments-1 ((form ast-jump-table))
  (lower-arguments-1 (value form))
  (mapc #'lower-arguments-1 (targets form)))

(defmethod lower-arguments-1 ((form lexical-variable)))

(defun wrap-type-check (variable value)
  "Wrap a type check around VALUE, based on VARIABLE's type."
  (cond ((or (eql (optimize-quality value 'safety) 0)
             (lexical-variable-p variable)
             (eql (mezzano.runtime::symbol-type (name variable)) 't))
         value)
        (t
         (ast `(let ((val ,value))
                 (if (source-fragment (typep val ',(simplify-complicated-function-type
                                                    (mezzano.runtime::symbol-type (name variable)))))
                     val
                     (progn
                       (call raise-binding-type-error
                             ',(name variable)
                             ',(mezzano.runtime::symbol-type (name variable))
                             val)
                       (call sys.int::%%unreachable))))
              value))))

(defmethod lower-arguments-1 ((form lambda-information))
  (let* ((*current-lambda* form)
         (extra-bindings '()))
    (labels ((new-var (name)
               (make-instance 'lexical-variable
                              :inherit form
                              :name (gensym name)
                              :definition-point *current-lambda*))
             (update-one (arg)
               (etypecase arg
                 (special-variable
                  (let ((temp (new-var (string (name arg)))))
                    (push (list arg (wrap-type-check arg temp)) extra-bindings)
                    temp))
                 (lexical-variable
                  arg))))
      (when (lambda-information-enable-keys form)
        (error "Keyword arguments not lowered!"))
      ;; Eliminate special required arguments.
      (setf (lambda-information-required-args form)
            (loop
               for arg in (lambda-information-required-args form)
               collect (update-one arg)))
      ;; Eliminate special optional arguments & non-constant init-forms.
      (setf (lambda-information-optional-args form)
            (loop
               for (arg init-form suppliedp) in (lambda-information-optional-args form)
               collect (let* ((new-suppliedp (etypecase suppliedp
                                               (null
                                                (new-var (format nil "~S-suppliedp" arg)))
                                               (special-variable
                                                (new-var (string (name suppliedp))))
                                               (lexical-variable
                                                suppliedp)))
                              (trivial-init-form (and (typep init-form 'ast-quote)
                                                      (or (lexical-variable-p arg)
                                                          (eql (mezzano.runtime::symbol-type (name arg)) 't))
                                                      (eql (ast-value init-form) nil)))
                              (new-arg (etypecase arg
                                         (special-variable
                                          (new-var (string (name arg))))
                                         (lexical-variable
                                          (if (not trivial-init-form)
                                              (new-var (string (lexical-variable-name arg)))
                                              arg))))
                              (new-init-form (if trivial-init-form
                                                 init-form
                                                 (ast '(quote nil) init-form))))
                         (when (or (not trivial-init-form)
                                   (typep arg 'special-variable))
                           (push (list arg (wrap-type-check arg
                                                            (ast `(if ,new-suppliedp
                                                                      ,new-arg
                                                                      ,init-form)
                                                                 init-form)))
                                 extra-bindings))
                         (when (and (not (null suppliedp))
                                    (typep suppliedp 'special-variable))
                           (push (list suppliedp (wrap-type-check suppliedp new-suppliedp))
                                 extra-bindings))
                         (list new-arg new-init-form new-suppliedp))))
      ;; And eliminate special rest args.
      (when (lambda-information-rest-arg form)
        (setf (lambda-information-rest-arg form) (update-one (lambda-information-rest-arg form))))
      (when (lambda-information-closure-arg form)
        (setf (lambda-information-closure-arg form) (update-one (lambda-information-closure-arg form))))
      (when (lambda-information-count-arg form)
        (setf (lambda-information-count-arg form) (update-one (lambda-information-count-arg form))))
      (when extra-bindings
        ;; Bindings were added.
        (setf (lambda-information-body form)
              (ast `(let ,(reverse extra-bindings)
                      ,(lambda-information-body form))
                   form)))
      (lower-arguments-1 (lambda-information-body form)))))
