(in-package :sys.newc)

;;; Some rewrite rules that are already provided by the optimizer function.

;; Eliminate FUNCALL forms.
(define-rewrite-rule simplify-funcall ()
  ('funcall cont fn . args)
  (fn cont . args))

;; Turn an invokation of a known continuation into a binding.
(define-rewrite-rule simplify-invoke-continuation ()
  ('%invoke-continuation (clambda params body) . args)
  ((clambda params body) . args))

;; Remove redundant lambdas & applications.
(define-rewrite-rule empty-lambda-1 ()
  ((clambda () body))
  body)
(define-rewrite-rule empty-lambda-2 ()
  (clambda () (body))
  body)

;; Eliminate empty TAGBODY forms.
(define-rewrite-rule empty-tagbody ()
  ('%tagbody cont (lambda (exit) body))
  ((clambda (exit) body) cont))
