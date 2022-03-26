;;;; -*- indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

(deftype thread ()
  'ccl::process)

;;; Thread Creation

(defun %make-thread (function name)
  (ccl:process-run-function name function))

(defun current-thread ()
  ccl:*current-process*)

(defun threadp (object)
  (ccl::processp object))

(defun thread-name (thread)
  (ccl:process-name thread))

;;; Resource contention: locks and recursive locks

(deftype lock () 'ccl:lock)

(defun lock-p (object)
  (typep object 'ccl:lock))

(defun make-lock (&optional name)
  (ccl:make-lock (or name "Anonymous lock")))

(defun acquire-lock (lock &optional (wait-p t))
  (if wait-p
    (ccl:process-lock lock ccl:*current-process*)
    ;; this is broken, but it's better than a no-op
    (ccl:without-interrupts
     (when (null (ccl::lock.value lock))
       (ccl:process-lock lock ccl:*current-process*)))))

(defun release-lock (lock)
  (ccl:process-unlock lock))

(defmacro with-lock-held ((place) &body body)
  `(ccl:with-lock-grabbed (,place) ,@body))

(defun thread-yield ()
  (ccl:process-allow-schedule))

;;; Introspection/debugging

(defun all-threads ()
  ccl:*all-processes*)

(defun interrupt-thread (thread function &rest args)
  (declare (dynamic-extent args))
  (apply #'ccl:process-interrupt thread function args))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (ccl:process-kill thread))

(mark-supported)
