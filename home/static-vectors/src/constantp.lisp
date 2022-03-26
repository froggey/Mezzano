;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Checking for compile-time constance and evaluating such forms
;;;

(in-package :static-vectors)

(defun constantp (form &optional env)
  (cl:constantp (if (symbolp form)
                    (macroexpand form env)
                    form)
                env))

(defun eval-constant (form &optional env)
  (declare (ignorable env))
  #+clozure
  (ccl::eval-constant form)
  #+sbcl
  (sb-int:constant-form-value form env)
  #-(or clozure sbcl)
  (eval form))

(defmacro cmfuncall (op &rest args &environment env)
  (let ((cmfun (compiler-macro-function op))
        (form (cons op args)))
    (if cmfun
        (funcall cmfun form env)
        form)))
