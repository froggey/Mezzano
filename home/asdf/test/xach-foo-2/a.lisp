;;;; a.lisp

(defpackage :second-version
  (:use #:cl)
  (:export #:loaded #:wtf))

(in-package :second-version)

(defparameter loaded t)
