;;;; -*- indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

(deftype thread ()
  'mt:thread)

;;; Thread Creation
(defun %make-thread (function name)
  (mt:make-thread function
                  :name name
                  :initial-bindings mt:*default-special-bindings*))

(defun current-thread ()
  (mt:current-thread))

(defun threadp (object)
  (mt:threadp object))

(defun thread-name (thread)
  (mt:thread-name thread))

;;; Resource contention: locks and recursive locks

(deftype lock () 'mt:mt-mutex)

(deftype recursive-lock ()
  '(and mt:mt-mutex (satisfies mt:mutex-recursive-p)))

(defun lock-p (object)
  (typep object 'mt:mt-mutex))

(defun recursive-lock-p (object)
  (and (typep object 'mt:mt-mutex)
       (mt:mutex-recursive-p object)))

(defun make-lock (&optional name)
  (mt:make-mutex :name (or name "Anonymous lock")))

(defun acquire-lock (lock &optional (wait-p t))
  (mt:mutex-lock lock :timeout (if wait-p nil 0)))

(defun release-lock (lock)
  (mt:mutex-unlock lock))

(defmacro with-lock-held ((place) &body body)
  `(mt:with-mutex-lock (,place) ,@body))

(defun make-recursive-lock (&optional name)
  (mt:make-mutex :name (or name "Anonymous recursive lock")
                 :recursive-p t))

(defmacro with-recursive-lock-held ((place) &body body)
  `(mt:with-mutex-lock (,place) ,@body))

;;; Resource contention: condition variables

(defun make-condition-variable (&key name)
  (mt:make-exemption :name (or name "Anonymous condition variable")))

(defun condition-wait (condition-variable lock &key timeout)
  (mt:exemption-wait condition-variable lock :timeout timeout)
  t)

(defun condition-notify (condition-variable)
  (mt:exemption-signal condition-variable))

(defun thread-yield ()
  (mt:thread-yield))

;;; Timeouts

(defmacro with-timeout ((timeout) &body body)
  (once-only (timeout)
    `(mt:with-timeout (,timeout (error 'timeout :length ,timeout))
       ,@body)))

;;; Introspection/debugging

;;; VTZ: mt:list-threads returns all threads that are not garbage collected.
(defun all-threads ()
  (delete-if-not #'mt:thread-active-p (mt:list-threads)))

(defun interrupt-thread (thread function &rest args)
  (mt:thread-interrupt thread :function function :arguments args))

(defun destroy-thread (thread)
  ;;; VTZ: actually we can kill ourselelf.
  ;;; suicide is part of our contemporary life :)
  (signal-error-if-current-thread thread)
  (mt:thread-interrupt thread :function t))

(defun thread-alive-p (thread)
  (mt:thread-active-p thread))

(defun join-thread (thread)
  (mt:thread-join thread))

(mark-supported)
