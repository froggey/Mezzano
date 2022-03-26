(defpackage package-inferred-system-test/local-nicknames-1-dep
  (:use :cl)
  (:export #:g))

(in-package :package-inferred-system-test/local-nicknames-1-dep)

(defun g () "g")
