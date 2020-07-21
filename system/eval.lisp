(in-package :mezzano.internals)

(defvar *eval-hook* 'mezzano.full-eval:eval-in-lexenv
  "A function that takes the form to evaluate and the top-level
lexical environment in which to evaluate it and returns the result
of evaluating it.

Suggested values:
'mezzano.fast-eval:eval-in-lexenv
  An evaluator that uses the compiler to evaluate complicated forms,
  supposedly faster.
'mezzano.full-eval:eval-in-lexenv
  A metacircular evaluator that completely implements the Common Lisp
  special forms. It does not rely on the compiler.")

(defun eval (form)
  (eval-in-lexenv form))

(defun eval-in-lexenv (form &optional env)
  (funcall *eval-hook* form env))
