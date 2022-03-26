(defpackage package-inferred-system-test/local-nicknames-1
  (:use :cl)
  (:local-nicknames
   (:dep :package-inferred-system-test/local-nicknames-1-dep))
  (:export #:f))

(in-package :package-inferred-system-test/local-nicknames-1)

(defun f () (dep:g))
