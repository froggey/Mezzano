;;;; a.lisp

(defpackage :first-version
  (:use #:cl)
  (:export #:loaded))

(in-package :first-version)
(defparameter loaded t)

