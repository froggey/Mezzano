;;;; -*- indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; This file provides a portable implementation of condition
;;; variables (given a working WITH-LOCK-HELD and THREAD-YIELD), and
;;; should be used if there is no condition variable implementation in
;;; the host Lisp.

(defstruct condition-var
  name
  lock
  active)

(defun condition-wait (condition-variable lock &key timeout)
  (signal-error-if-condition-wait-timeout timeout)
  (check-type condition-variable condition-var)
  (setf (condition-var-active condition-variable) nil)
  (release-lock lock)
  (do ()
      ((when (condition-var-active condition-variable)
         (acquire-lock lock)
         t))
    (thread-yield))
  t)

(define-condition-wait-compiler-macro)

(defun condition-notify (condition-variable)
  (check-type condition-variable condition-var)
  (with-lock-held ((condition-var-lock condition-variable))
    (setf (condition-var-active condition-variable) t)))
