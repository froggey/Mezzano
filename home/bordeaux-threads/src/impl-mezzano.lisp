;;;; -*- indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil
Copyright 2016 Henry Harrington

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

(deftype thread ()
  'mezzano.supervisor:thread)

;;; Thread Creation

(defun %make-thread (function name)
  (mezzano.supervisor:make-thread
   (lambda ()
     (with-simple-restart (abort "Terminate thread ~S" (mezzano.supervisor:current-thread))
       (funcall function)))
   :name name))

(defun current-thread ()
  (mezzano.supervisor:current-thread))

(defun threadp (object)
  (mezzano.supervisor:threadp object))

(defun thread-name (thread)
  (mezzano.supervisor:thread-name thread))

;;; Resource contention: locks and recursive locks

(deftype lock () 'mezzano.supervisor:mutex)

(defun lock-p (object)
  (mezzano.supervisor:mutex-p object))

(defun make-lock (&optional name)
  (mezzano.supervisor:make-mutex name))

(defun acquire-lock (lock &optional (wait-p t))
  (mezzano.supervisor:acquire-mutex lock wait-p))

(defun release-lock (lock)
  (mezzano.supervisor:release-mutex lock))

(defmacro with-lock-held ((place) &body body)
  `(mezzano.supervisor:with-mutex (,place) ,@body))

(defstruct (recursive-lock
             (:constructor make-recursive-lock
                           (&optional name &aux
                                      (mutex (mezzano.supervisor:make-mutex name)))))
  mutex
  (depth 0))

(defun call-with-recursive-lock-held (lock function)
  (cond ((mezzano.supervisor:mutex-held-p
          (recursive-lock-mutex lock))
         (unwind-protect
              (progn (incf (recursive-lock-depth lock))
                     (funcall function))
           (decf (recursive-lock-depth lock))))
        (t
         (mezzano.supervisor:with-mutex ((recursive-lock-mutex lock))
           (multiple-value-prog1
               (funcall function)
             (assert (eql (recursive-lock-depth lock) 0)))))))

(defmacro with-recursive-lock-held ((place) &body body)
  `(call-with-recursive-lock-held ,place (lambda () ,@body)))

;;; Resource contention: condition variables

(defun make-condition-variable (&key name)
  (mezzano.supervisor:make-condition-variable name))

(defun condition-wait (condition-variable lock &key timeout)
  (mezzano.supervisor:condition-wait
   condition-variable
   (if (recursive-lock-p lock)
       (recursive-lock-mutex lock)
       lock)
   timeout)
  t)

(defun condition-notify (condition-variable)
  (mezzano.supervisor:condition-notify condition-variable))

(defun thread-yield ()
  (mezzano.supervisor:thread-yield))

;;; Timeouts

;;; Introspection/debugging

(defun all-threads ()
  (mezzano.supervisor:all-threads))

(defun interrupt-thread (thread function &rest args)
  (mezzano.supervisor:establish-thread-foothold
   thread
   (lambda ()
     (apply function args))))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (mezzano.supervisor:terminate-thread thread))

(defun thread-alive-p (thread)
  (not (eql (mezzano.supervisor:thread-state thread) :dead)))

(defun join-thread (thread)
  (signal-error-if-current-thread thread)
  ;; THREAD-JOIN can return non-lists if the thread was destroyed.
  (let ((values (mezzano.supervisor:thread-join thread)))
    (if (listp values)
        (values-list values)
        nil)))

(mark-supported)
