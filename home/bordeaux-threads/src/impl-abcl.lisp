;;;; -*- indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Reimplemented with java.util.concurrent.locks.ReentrantLock by Mark Evenson 2011.

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; the implementation of the Armed Bear thread interface can be found in
;;; src/org/armedbear/lisp/LispThread.java

(deftype thread ()
  'threads:thread)

;;; Thread Creation

(defun %make-thread (function name)
  (threads:make-thread function :name name))

(defun current-thread ()
  (threads:current-thread))

(defun thread-name (thread)
  (threads:thread-name thread))

(defun threadp (object)
  (typep object 'thread))

;;; Resource contention: locks and recursive locks

(defstruct mutex name lock)
(defstruct (mutex-recursive (:include mutex)))

;; Making methods constants in this manner avoids the runtime expense of
;; introspection involved in JCALL with string arguments.
(defconstant +lock+ 
  (jmethod "java.util.concurrent.locks.ReentrantLock" "lock"))
(defconstant +try-lock+ 
  (jmethod "java.util.concurrent.locks.ReentrantLock" "tryLock"))
(defconstant +is-held-by-current-thread+ 
  (jmethod "java.util.concurrent.locks.ReentrantLock" "isHeldByCurrentThread"))
(defconstant +unlock+ 
  (jmethod "java.util.concurrent.locks.ReentrantLock" "unlock"))
(defconstant +get-hold-count+ 
  (jmethod "java.util.concurrent.locks.ReentrantLock" "getHoldCount"))

(deftype lock () 'mutex)

(deftype recursive-lock () 'mutex-recursive)

(defun lock-p (object)
  (typep object 'mutex))

(defun recursive-lock-p (object)
  (typep object 'mutex-recursive))

(defun make-lock (&optional name)
  (make-mutex 
   :name (or name "Anonymous lock")
   :lock (jnew "java.util.concurrent.locks.ReentrantLock")))

(defun acquire-lock (lock &optional (wait-p t))
  (check-type lock mutex)
  (when (jcall +is-held-by-current-thread+ (mutex-lock lock))
    (error "Non-recursive lock being reacquired by owner."))
  (cond
    (wait-p
     (jcall +lock+ (mutex-lock lock))
     t)
    (t (jcall +try-lock+ (mutex-lock lock)))))

(defun release-lock (lock)
  (check-type lock mutex)
  (unless (jcall +is-held-by-current-thread+ (mutex-lock lock))
    (error "Attempt to release lock not held by calling thread."))
  (jcall +unlock+ (mutex-lock lock))
  (values))

(defun make-recursive-lock (&optional name)
  (make-mutex-recursive
   :name (or name "Anonymous lock")
   :lock (jnew "java.util.concurrent.locks.ReentrantLock")))

(defun acquire-recursive-lock (lock &optional (wait-p t))
  (check-type lock mutex-recursive)
  (cond
    (wait-p
     (jcall +lock+ (mutex-recursive-lock lock))
     t)
    (t (jcall +try-lock+ (mutex-recursive-lock lock)))))

(defun release-recursive-lock (lock)
  (check-type lock mutex-recursive)
  (unless (jcall +is-held-by-current-thread+ (mutex-lock lock))
    (error "Attempt to release lock not held by calling thread."))
  (jcall +unlock+ (mutex-lock lock))
  (values))

;;; Resource contention: condition variables

(defun thread-yield ()
  (java:jstatic "yield" "java.lang.Thread"))

(defstruct condition-variable
  (name "Anonymous condition variable"))

(defun condition-wait (condition lock &key timeout)
  (threads:synchronized-on condition
    (release-lock lock)
    (if timeout
        ;; Since giving a zero time value to threads:object-wait means
        ;; an indefinite wait, use some arbitrary small number.
        (threads:object-wait condition
                             (if (zerop timeout)
                                 least-positive-single-float
                                 timeout))
        (threads:object-wait condition)))
  (acquire-lock lock)
  t)

(defun condition-notify (condition)
  (threads:synchronized-on condition
     (threads:object-notify condition)))

;;; Introspection/debugging

(defun all-threads ()
  (let ((threads ()))
    (threads:mapcar-threads (lambda (thread)
			      (push thread threads)))
    (reverse threads)))

(defun interrupt-thread (thread function &rest args)
  (apply #'threads:interrupt-thread thread function args))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (threads:destroy-thread thread))

(defun thread-alive-p (thread)
  (threads:thread-alive-p thread))

(defun join-thread (thread)
  (threads:thread-join thread))

(mark-supported)
