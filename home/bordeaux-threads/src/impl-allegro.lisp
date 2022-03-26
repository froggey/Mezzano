;;;; -*- indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; documentation on the Allegro Multiprocessing interface can be found at
;;; http://www.franz.com/support/documentation/8.1/doc/multiprocessing.htm

;;; Resource contention: locks and recursive locks

(deftype lock () 'mp:process-lock)

(deftype recursive-lock () 'mp:process-lock)

(defun lock-p (object)
  (typep object 'mp:process-lock))

(defun recursive-lock-p (object)
  (typep object 'mp:process-lock))

(defun make-lock (&optional name)
  (mp:make-process-lock :name (or name "Anonymous lock")))

(defun make-recursive-lock (&optional name)
  (mp:make-process-lock :name (or name "Anonymous recursive lock")))

(defun acquire-lock (lock &optional (wait-p t))
  (mp:process-lock lock mp:*current-process* "Lock" (if wait-p nil 0)))

(defun release-lock (lock)
  (mp:process-unlock lock))

(defmacro with-lock-held ((place) &body body)
  `(mp:with-process-lock (,place :norecursive t)
     ,@body))

(defmacro with-recursive-lock-held ((place &key timeout) &body body)
  `(mp:with-process-lock (,place :timeout ,timeout)
     ,@body))

;;; Resource contention: condition variables

(defun make-condition-variable (&key name)
  (declare (ignorable name))
  #-(version>= 9)
  (mp:make-gate nil)
  #+(version>= 9)
  (mp:make-condition-variable :name name))

(defun condition-wait (condition-variable lock &key timeout)
  #-(version>= 9)
  (progn
    (release-lock lock)
    (if timeout
        (mp:process-wait-with-timeout "wait for message" timeout
                                      #'mp:gate-open-p condition-variable)
        (mp:process-wait "wait for message" #'mp:gate-open-p condition-variable))
    (acquire-lock lock)
    (mp:close-gate condition-variable))
  #+(version>= 9)
  (mp:condition-variable-wait condition-variable lock :timeout timeout)
  t)

(defun condition-notify (condition-variable)
  #-(version>= 9)
  (mp:open-gate condition-variable)
  #+(version>= 9)
  (mp:condition-variable-signal condition-variable))

(defun thread-yield ()
  (mp:process-allow-schedule))

(deftype thread ()
  'mp:process)

;;; Thread Creation

(defun start-multiprocessing ()
  (mp:start-scheduler))

(defun %make-thread (function name)
  #+smp
  (mp:process-run-function name function)
  #-smp
  (mp:process-run-function
   name
   (lambda ()
     (let ((return-values
             (multiple-value-list (funcall function))))
       (setf (getf (mp:process-property-list mp:*current-process*)
                   'return-values)
             return-values)
       (values-list return-values)))))

(defun current-thread ()
  mp:*current-process*)

(defun threadp (object)
  (typep object 'mp:process))

(defun thread-name (thread)
  (mp:process-name thread))

;;; Timeouts

(defmacro with-timeout ((timeout) &body body)
  (once-only (timeout)
    `(mp:with-timeout (,timeout (error 'timeout :length ,timeout))
       ,@body)))

;;; Introspection/debugging

(defun all-threads ()
  mp:*all-processes*)

(defun interrupt-thread (thread function &rest args)
  (apply #'mp:process-interrupt thread function args))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (mp:process-kill thread))

(defun thread-alive-p (thread)
  (mp:process-alive-p thread))

(defun join-thread (thread)
  #+smp
  (values-list (mp:process-join thread))
  #-smp
  (progn
    (mp:process-wait (format nil "Waiting for thread ~A to complete" thread)
                     (complement #'mp:process-alive-p)
                     thread)
    (let ((return-values
            (getf (mp:process-property-list thread) 'return-values)))
      (values-list return-values))))

(mark-supported)
