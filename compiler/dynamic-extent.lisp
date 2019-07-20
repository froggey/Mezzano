;;;; Copyright (c) 2018 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

;;; Convert variables bindings that have been declared dynamic-extent and have
;;; supported initializers to calls to their appropriate internal functions.

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
  (multiple-value-bind (list-body list-tail dx-p)
      (extract-list-like-forms initform)
    (cond ((and list-body
                (not dx-p))
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
                                         (convert-dx
                                          (if (and (lexical-variable-p var)
                                                   (lexical-variable-dynamic-extent var)
                                                   (typep initform 'ast-call))
                                              (convert-dx-initializer initform)
                                              initform)))))
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

;;; Lower calls to the more complicated dx-list and dx-list* functions
;;; to calls to make-dx-cons with appropriate initialization.

(defun lower-dynamic-extent-list (lambda)
  (lower-dx-list lambda))

(defgeneric lower-dx-list (form))

(defmethod lower-dx-list ((form ast-block))
  (setf (body form) (lower-dx-list (body form)))
  form)

(defmethod lower-dx-list ((form ast-function))
  form)

(defmethod lower-dx-list ((form ast-go))
  form)

(defmethod lower-dx-list ((form ast-if))
  (setf (test form) (lower-dx-list (test form))
        (if-then form) (lower-dx-list (if-then form))
        (if-else form) (lower-dx-list (if-else form)))
  form)

(defmethod lower-dx-list ((form ast-let))
  (setf (bindings form) (loop
                           for (var initform) in (bindings form)
                           collect (list var (lower-dx-list initform))))
  (setf (body form) (lower-dx-list (body form)))
  form)

(defmethod lower-dx-list ((form ast-multiple-value-bind))
  (setf (value-form form) (lower-dx-list (value-form form))
        (body form) (lower-dx-list (body form)))
  form)

(defmethod lower-dx-list ((form ast-multiple-value-call))
  (setf (function-form form) (lower-dx-list (function-form form))
        (value-form form) (lower-dx-list (value-form form)))
  form)

(defmethod lower-dx-list ((form ast-multiple-value-prog1))
  (setf (value-form form) (lower-dx-list (value-form form))
        (body form) (lower-dx-list (body form)))
  form)

(defmethod lower-dx-list ((form ast-progn))
  (setf (forms form) (loop
                        for form in (forms form)
                        collect (lower-dx-list form)))
  form)

(defmethod lower-dx-list ((form ast-quote))
  form)

(defmethod lower-dx-list ((form ast-return-from))
  (setf (value form) (lower-dx-list (value form))
        (info form) (lower-dx-list (info form)))
  form)

(defmethod lower-dx-list ((form ast-setq))
  (setf (value form) (lower-dx-list (value form)))
  form)

(defmethod lower-dx-list ((form ast-tagbody))
  (setf (statements form)
        (loop
           for (go-tag statement) in (statements form)
           collect (list go-tag (lower-dx-list statement))))
  form)

(defmethod lower-dx-list ((form ast-the))
  (setf (value form) (lower-dx-list (value form)))
  form)

(defmethod lower-dx-list ((form ast-unwind-protect))
  (setf (protected-form form) (lower-dx-list (protected-form form))
        (cleanup-function form) (lower-dx-list (cleanup-function form)))
  form)

(defmethod lower-dx-list ((form ast-call))
  (setf (arguments form) (loop
                            for arg in (arguments form)
                            collect (lower-dx-list arg)))
  (labels ((build (elements tail)
             (cond ((null elements)
                    (or tail '(quote nil)))
                   (t
                    `(let ((c (call make-dx-cons)))
                       (progn
                         (call (setf mezzano.runtime::%car) ,(first elements) c)
                         (call (setf mezzano.runtime::%cdr) ,(build (rest elements) tail) c)
                         c))))))
    (case (ast-name form)
      (dx-list
       (ast (build (ast-arguments form) nil) form))
      (dx-list*
       (ast (build (butlast (ast-arguments form))
                   (first (last (ast-arguments form))))
            form))
      (t
       form))))

(defmethod lower-dx-list ((form ast-jump-table))
  (setf (value form) (lower-dx-list (value form)))
  (setf (targets form) (loop
                          for target in (targets form)
                          collect (lower-dx-list target)))
  form)

(defmethod lower-dx-list ((form lexical-variable))
  form)

(defmethod lower-dx-list ((form lambda-information))
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (lower-dx-list (second arg))))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (lower-dx-list (second arg))))
    (setf (lambda-information-body form) (lower-dx-list (lambda-information-body form))))
  form)
