;;;; -*- indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil
Copyright 2010 Jean-Claude Beaudoin.

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

(deftype thread ()
  'mt:thread)

;;; Thread Creation

(defun %make-thread (function name)
  (mt:thread-run-function name function))

(defun current-thread ()
  mt::*thread*)

(defun threadp (object)
  (typep object 'mt:thread))

(defun thread-name (thread)
  (mt:thread-name thread))

;;; Resource contention: locks and recursive locks

(deftype lock () 'mt:lock)

(deftype recursive-lock ()
  '(and mt:lock (satisfies mt:recursive-lock-p)))

(defun lock-p (object)
  (typep object 'mt:lock))

(defun recursive-lock-p (object)
  (and (typep object 'mt:lock)
       (mt:recursive-lock-p object)))

(defun make-lock (&optional name)
  (mt:make-lock :name (or name "Anonymous lock")))

(defun acquire-lock (lock &optional (wait-p t))
  (mt:get-lock lock wait-p))

(defun release-lock (lock)
  (mt:giveup-lock lock))

(defmacro with-lock-held ((place) &body body)
  `(mt:with-lock (,place) ,@body))

(defun make-recursive-lock (&optional name)
  (mt:make-lock :name (or name "Anonymous recursive lock") :recursive t))

(defun acquire-recursive-lock (lock &optional (wait-p t))
  (mt:get-lock lock wait-p))

(defun release-recursive-lock (lock)
  (mt:giveup-lock lock))

(defmacro with-recursive-lock-held ((place) &body body)
  `(mt:with-lock (,place) ,@body))

;;; Resource contention: condition variables

(defun make-condition-variable (&key name)
  (declare (ignore name))
  (mt:make-condition-variable))

(defun condition-wait (condition-variable lock &key timeout)
  (signal-error-if-condition-wait-timeout timeout)
  (mt:condition-wait condition-variable lock)
  t)

(define-condition-wait-compiler-macro)

(defun condition-notify (condition-variable)
  (mt:condition-signal condition-variable))

(defun thread-yield ()
  (mt:thread-yield))

;;; Introspection/debugging

(defun all-threads ()
  (mt:all-threads))

(defun interrupt-thread (thread function &rest args)
  (flet ((apply-function ()
           (if args
               (lambda () (apply function args))
               function)))
    (declare (dynamic-extent #'apply-function))
    (mt:interrupt-thread thread (apply-function))))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (mt:thread-kill thread))

(defun thread-alive-p (thread)
  (mt:thread-active-p thread))

(defun join-thread (thread)
  (mt:thread-join thread))

(mark-supported)
