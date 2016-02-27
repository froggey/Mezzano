;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Function inlining.

(in-package :sys.c)

(defun inline-functions (lambda)
  (il-form lambda))

(defgeneric il-form (form))

(defun il-implicit-progn (x)
  (do ((i x (cdr i)))
      ((endp i))
    (setf (car i) (il-form (car i)))))

(defmethod il-form ((form ast-block))
  (setf (body form) (il-form (body form)))
  form)

(defmethod il-form ((form ast-function))
  form)

(defmethod il-form ((form ast-go))
  (setf (info form) (il-form (info form)))
  form)

(defmethod il-form ((form ast-if))
  (setf (test form) (il-form (test form))
        (if-then form) (il-form (if-then form))
        (if-else form) (il-form (if-else form)))
  form)

(defmethod il-form ((form ast-let))
  (dolist (b (bindings form))
    ;; Run on the init-form.
    (setf (second b) (il-form (second b))))
  (setf (body form) (il-form (body form)))
  form)

(defmethod il-form ((form ast-multiple-value-bind))
  (setf (value-form form) (il-form (value-form form))
        (body form) (il-form (body form)))
  form)

(defmethod il-form ((form ast-multiple-value-call))
  (setf (function-form form) (il-form (function-form form))
        (value-form form) (il-form (value-form form)))
  form)

(defmethod il-form ((form ast-multiple-value-prog1))
  (setf (value-form form) (il-form (value-form form))
        (body form) (il-form (body form)))
  form)

(defmethod il-form ((form ast-progn))
  (il-implicit-progn (forms form))
  form)

(defmethod il-form ((form ast-quote))
  form)

(defmethod il-form ((form ast-return-from))
  (setf (value form) (il-form (value form))
        (info form) (il-form (info form)))
  form)

(defmethod il-form ((form ast-setq))
  ;; Walk the value form.
  (setf (value form) (il-form (value form)))
  form)

(defmethod il-form ((form ast-tagbody))
  (setf (statements form)
        (loop
           for (go-tag statement) in (statements form)
           collect (list go-tag (il-form statement))))
  form)

(defmethod il-form ((form ast-the))
  (setf (value form) (il-form (value form)))
  form)

(defmethod il-form ((form ast-unwind-protect))
  (setf (protected-form form) (il-form (protected-form form))
        (cleanup-function form) (il-form (cleanup-function form)))
  form)

(defmethod il-form ((form ast-jump-table))
  (setf (value form) (il-form (value form)))
  (il-implicit-progn (targets form))
  form)

(defun expand-inline-function (name arg-list)
  (multiple-value-bind (inlinep expansion)
      (function-inline-info name)
    (when inlinep
      (cond (expansion
             (ast `(call funcall ,(pass1-lambda expansion nil) ,@arg-list)))
            ((fboundp name)
             (multiple-value-bind (expansion closurep)
                 (function-lambda-expression (fdefinition name))
               (when (and expansion (not closurep))
                 (ast `(call funcall ,(pass1-lambda expansion nil) ,@arg-list)))))))))

(defmethod il-form ((form ast-call))
  (il-implicit-progn (arguments form))
  (let ((inlined-form (expand-inline-function (name form) (arguments form))))
    (cond (inlined-form
           (change-made)
           inlined-form)
          (t form))))

(defmethod il-form ((form lexical-variable))
  form)

(defmethod il-form ((form lambda-information))
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (il-form (second arg))))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (il-form (second arg))))
    (setf (lambda-information-body form) (il-form (lambda-information-body form))))
  form)
