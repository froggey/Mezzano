;;;; Copyright (c) 2018 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

(defun convert-dynamic-extent (lambda architecture)
  (declare (ignore architecture))
  (convert-dx lambda))

(defgeneric convert-dx (form))

(defmethod convert-dx ((form ast-block))
  (setf (body form) (convert-dx (body form)))
  form)

(defmethod convert-dx ((form ast-function))
  form)

(defmethod convert-dx ((form ast-go))
  form)

(defmethod convert-dx ((form ast-if))
  (setf (test form) (convert-dx (test form))
        (if-then form) (convert-dx (if-then form))
        (if-else form) (convert-dx (if-else form)))
  form)

(defun convert-dx-initializer (initform)
  (when (not (typep initform 'ast-call))
    (return-from convert-dx-initializer initform))
  (multiple-value-bind (list-body list-tail)
      (extract-list-like-forms initform)
    (cond (list-body
           (change-made)
           (ast (construct-dx-list list-body (convert-dx-initializer list-tail))
                initform))
          ((eql (ast-name initform) 'vector)
           (change-made)
           (ast `(let ((vec (call make-dx-simple-vector ',(length (ast-arguments initform)))))
                   (progn
                     ,@(loop
                          for i from 0
                          for arg in (ast-arguments initform)
                          collect `(call (setf sys.int::%object-ref-t) ,arg vec ',i))
                     vec))
                initform))
          (t
           initform))))

(defmethod convert-dx ((form ast-let))
  (setf (bindings form) (loop
                           for (var initform) in (bindings form)
                           collect (list var
                                         (if (and (lexical-variable-p var)
                                                  (lexical-variable-dynamic-extent var)
                                                  (typep initform 'ast-call))
                                             (convert-dx-initializer initform)
                                             initform))))
  (setf (body form) (convert-dx (body form)))
  form)

(defmethod convert-dx ((form ast-multiple-value-bind))
  (setf (value-form form) (convert-dx (value-form form))
        (body form) (convert-dx (body form)))
  form)

(defmethod convert-dx ((form ast-multiple-value-call))
  (setf (function-form form) (convert-dx (function-form form))
        (value-form form) (convert-dx (value-form form)))
  form)

(defmethod convert-dx ((form ast-multiple-value-prog1))
  (setf (value-form form) (convert-dx (value-form form))
        (body form) (convert-dx (body form)))
  form)

(defmethod convert-dx ((form ast-progn))
  (setf (forms form) (loop
                        for form in (forms form)
                        collect (convert-dx form)))
  form)

(defmethod convert-dx ((form ast-quote))
  form)

(defmethod convert-dx ((form ast-return-from))
  (setf (value form) (convert-dx (value form))
        (info form) (convert-dx (info form)))
  form)

(defmethod convert-dx ((form ast-setq))
  (setf (value form) (convert-dx (value form)))
  form)

(defmethod convert-dx ((form ast-tagbody))
  (setf (statements form)
        (loop
           for (go-tag statement) in (statements form)
           collect (list go-tag (convert-dx statement))))
  form)

(defmethod convert-dx ((form ast-the))
  (setf (value form) (convert-dx (value form)))
  form)

(defmethod convert-dx ((form ast-unwind-protect))
  (setf (protected-form form) (convert-dx (protected-form form))
        (cleanup-function form) (convert-dx (cleanup-function form)))
  form)

(defmethod convert-dx ((form ast-call))
  (setf (arguments form) (loop
                            for arg in (arguments form)
                            collect (convert-dx arg)))
  form)

(defmethod convert-dx ((form ast-jump-table))
  (setf (value form) (convert-dx (value form)))
  (setf (targets form) (loop
                          for target in (targets form)
                          collect (convert-dx target)))
  form)

(defmethod convert-dx ((form lexical-variable))
  form)

(defmethod convert-dx ((form lambda-information))
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (convert-dx (second arg))))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (convert-dx (second arg))))
    (setf (lambda-information-body form) (convert-dx (lambda-information-body form))))
  form)
