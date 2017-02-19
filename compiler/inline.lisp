;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Function inlining.

(in-package :sys.c)

(defun inline-functions (lambda architecture)
  (il-form lambda architecture))

(defgeneric il-form (form architecture))

(defun il-implicit-progn (x architecture)
  (do ((i x (cdr i)))
      ((endp i))
    (setf (car i) (il-form (car i) architecture))))

(defmethod il-form ((form ast-block) architecture)
  (setf (body form) (il-form (body form) architecture))
  form)

(defmethod il-form ((form ast-function) architecture)
  form)

(defmethod il-form ((form ast-go) architecture)
  (setf (info form) (il-form (info form) architecture))
  form)

(defmethod il-form ((form ast-if) architecture)
  (setf (test form) (il-form (test form) architecture)
        (if-then form) (il-form (if-then form) architecture)
        (if-else form) (il-form (if-else form) architecture))
  form)

(defmethod il-form ((form ast-let) architecture)
  (dolist (b (bindings form))
    ;; Run on the init-form.
    (setf (second b) (il-form (second b) architecture)))
  (setf (body form) (il-form (body form) architecture))
  form)

(defmethod il-form ((form ast-multiple-value-bind) architecture)
  (setf (value-form form) (il-form (value-form form) architecture)
        (body form) (il-form (body form) architecture))
  form)

(defmethod il-form ((form ast-multiple-value-call) architecture)
  (setf (function-form form) (il-form (function-form form) architecture)
        (value-form form) (il-form (value-form form) architecture))
  form)

(defmethod il-form ((form ast-multiple-value-prog1) architecture)
  (setf (value-form form) (il-form (value-form form) architecture)
        (body form) (il-form (body form) architecture))
  form)

(defmethod il-form ((form ast-progn) architecture)
  (il-implicit-progn (forms form) architecture)
  form)

(defmethod il-form ((form ast-quote) architecture)
  form)

(defmethod il-form ((form ast-return-from) architecture)
  (setf (value form) (il-form (value form) architecture)
        (info form) (il-form (info form) architecture))
  form)

(defmethod il-form ((form ast-setq) architecture)
  ;; Walk the value form.
  (setf (value form) (il-form (value form) architecture))
  form)

(defmethod il-form ((form ast-tagbody) architecture)
  (setf (statements form)
        (loop
           for (go-tag statement) in (statements form)
           collect (list go-tag (il-form statement architecture))))
  form)

(defmethod il-form ((form ast-the) architecture)
  (setf (value form) (il-form (value form) architecture))
  form)

(defmethod il-form ((form ast-unwind-protect) architecture)
  (setf (protected-form form) (il-form (protected-form form) architecture)
        (cleanup-function form) (il-form (cleanup-function form) architecture))
  form)

(defmethod il-form ((form ast-jump-table) architecture)
  (setf (value form) (il-form (value form) architecture))
  (il-implicit-progn (targets form) architecture)
  form)

(defun expand-inline-function (form name arg-list architecture)
  (multiple-value-bind (inlinep expansion)
      (function-inline-info name)
    (when (and inlinep
               ;; Don't inline builtin functions.
               ;; There may be inlinable definitions available, but they're for the new compiler.
               (not (gethash name (ecase architecture
                                    (:x86-64 mezzano.compiler.codegen.x86-64::*builtins*)
                                    (:arm64 mezzano.compiler.codegen.arm64::*builtins*)))))
      (cond (expansion
             (ast `(call mezzano.runtime::%funcall
                         ,(pass1-lambda expansion
                                        (extend-environment
                                         nil
                                         :declarations `((optimize ,@(loop for (quality value) on (ast-optimize form) by #'cddr
                                                                        collect (list quality value))))))
                         ,@arg-list)
                  form))
            ((fboundp name)
             (multiple-value-bind (expansion closurep)
                 (function-lambda-expression (fdefinition name))
               (when (and expansion (not closurep))
                 (ast `(call mezzano.runtime::%funcall
                             ,(pass1-lambda expansion
                                            (extend-environment
                                             nil
                                             :declarations `((optimize ,@(loop for (quality value) on (ast-optimize form) by #'cddr
                                                                            collect (list quality value))))))
                             ,@arg-list)
                      form))))))))

(defmethod il-form ((form ast-call) architecture)
  (il-implicit-progn (arguments form) architecture)
  (let ((inlined-form (expand-inline-function form (name form) (arguments form) architecture)))
    (cond (inlined-form
           (change-made)
           inlined-form)
          (t form))))

(defmethod il-form ((form lexical-variable) architecture)
  form)

(defmethod il-form ((form lambda-information) architecture)
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (il-form (second arg) architecture)))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (il-form (second arg) architecture)))
    (setf (lambda-information-body form) (il-form (lambda-information-body form) architecture)))
  form)
