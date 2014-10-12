(in-package :sys.int)

(defvar *eval-hook* 'mezzanine.full-eval:eval-in-lexenv
  "A function that takes the form to evaluate and the top-level
lexical environment in which to evaluate it and returns the result
of evaluating it.

Suggested values:
'mezzanine.fast-eval:eval-in-lexenv
  An evaluator that uses the compiler to evaluate complicated forms,
  supposedly faster.
'mezzanine.full-eval:eval-in-lexenv
  A metacircular evaluator that completely implements the Common Lisp
  special forms. It does not rely on the compiler.")

(defun eval (form)
  (eval-in-lexenv form))

(defun eval-in-lexenv (form &optional env)
  (funcall *eval-hook* form env))
