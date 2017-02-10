;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

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

(defmethod lower-arguments-1 ((form lambda-information))
  (flet ((new-var (name)
           (make-instance 'lexical-variable
                          :inherit form
                          :name (gensym name)
                          :definition-point *current-lambda*)))
    (let* ((*current-lambda* form)
           (extra-bindings '()))
      (when (lambda-information-enable-keys form)
        (error "Keyword arguments not lowered!"))
      ;; Eliminate special required arguments.
      (setf (lambda-information-required-args form)
            (loop
               for arg in (lambda-information-required-args form)
               collect (etypecase arg
                         (special-variable
                          (let ((temp (new-var (string (name arg)))))
                            (push (list arg temp) extra-bindings)
                            temp))
                         (lexical-variable
                          arg))))
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
                              (trivial-init-form (typep init-form 'ast-quote))
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
                           (push (list arg (ast `(if ,new-suppliedp
                                                     ,new-arg
                                                     ,init-form)
                                                init-form))
                                 extra-bindings))
                         (when (and (not (null suppliedp))
                                    (typep suppliedp 'special-variable))
                           (push (list suppliedp new-suppliedp)
                                 extra-bindings))
                         (list new-arg new-init-form new-suppliedp))))
      ;; And eliminate special rest args.
      (when (and (lambda-information-rest-arg form)
                 (typep (lambda-information-rest-arg form) 'special-variable))
        (let ((new-rest (new-var (string (name (lambda-information-rest-arg form))))))
          (push (list (lambda-information-rest-arg form) new-rest)
                extra-bindings)
          (setf (lambda-information-rest-arg form) new-rest)))
      (when (and (lambda-information-fref-arg form)
                 (typep (lambda-information-fref-arg form) 'special-variable))
        (let ((new-fref (new-var (string (name (lambda-information-fref-arg form))))))
          (push (list (lambda-information-fref-arg form) new-fref)
                extra-bindings)
          (setf (lambda-information-fref-arg form) new-fref)))
      (when (and (lambda-information-closure-arg form)
                 (typep (lambda-information-closure-arg form) 'special-variable))
        (let ((new-closure (new-var (string (name (lambda-information-closure-arg form))))))
          (push (list (lambda-information-closure-arg form) new-closure)
                extra-bindings)
          (setf (lambda-information-closure-arg form) new-closure)))
      (when (and (lambda-information-count-arg form)
                 (typep (lambda-information-count-arg form) 'special-variable))
        (let ((new-count (new-var (string (name (lambda-information-count-arg form))))))
          (push (list (lambda-information-count-arg form) new-count)
                extra-bindings)
          (setf (lambda-information-count-arg form) new-count)))
      (when extra-bindings
        ;; Bindings were added.
        (setf (lambda-information-body form)
              (ast `(let ,(reverse extra-bindings)
                      ,(lambda-information-body form))
                   form)))
      (lower-arguments-1 (lambda-information-body form)))))
