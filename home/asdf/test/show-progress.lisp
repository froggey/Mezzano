(defpackage :show-progress (:use :cl))
(in-package :show-progress)

(eval-when (:load-toplevel) (defparameter *time* :load-toplevel))
(eval-when (:compile-toplevel) (defparameter *time* :compile-toplevel))
(eval-when (:execute) (defparameter *time* :execute))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (format t "It's ~A time!~%" *time*)
  (defvar *times* '())
  (push *time* *times*))

(defun foo (x)
  (format t "~S~%" x))

(defmacro m1 () '(foo 1))
(defmacro m2 () '(foo 2))
(defmacro me () (error "Failing at compile-time"))

(defmacro mb () '(me))
(defmacro mc () '(m1))
