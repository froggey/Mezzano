;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;;   Title: CLIM-2, Chapter 32.2 Multi-processing (BT)
;;; Created: 2016-01-27
;;;  Author: Daniel Kochmański <daniel@turtleware.eu>
;;; License: LGPL-2.1
;;; ---------------------------------------------------------------------------
;;; (c) copyright 2016 by Daniel Kochmański


;;;; Multiprocessing non-portable stuff (implemented using
;;;; bordeaux-threads where feasible). BT has also limited support for
;;;; single-threaded implementations. Supersedes MP-NIL backend.

(in-package :clim-internals)


;;; B.2 Multi-processing
;;;
;;; Most Lisp implementations provide some form of
;;; multi-processing. CLIM provides a pset of functions that implement
;;; a uniform interface to the multi-processing functionality.

(defconstant *multiprocessing-p* bt:*supports-threads-p*
  "The value of *multiprocessing-p* is t if the current Lisp environment
supports multi-processing, otherwise it is nil.")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (when bt:*supports-threads-p*
    (pushnew :clim-mp *features*)))

(defun make-process (function &key name)
    "Creates a process named name. The new process will evaluate the
function function. On systems that do not support multi-processing,
make-process will signal an error."
  (bt:make-thread function :name name))

(defun destroy-process (process)
  "Terminates the process process. process is an object returned by
make-process."
  (bt:destroy-thread process))

(defun current-process ()
  "Returns the currently running process, which will be the same kind
of object as would be returned by make-process. "
  (bt:current-thread))

(defun all-processes ()
  "Returns a sequence of all of the processes."
  (bt:all-threads))

(defun processp (object)
  "Returns t if object is a process, otherwise returns nil."
  (bt:threadp object))

(defun process-name (process)
  "Returns the name of the process. Platform dependent."
  (bt:thread-name process))

(defun process-state (process)
  "Returns the state of the process. Platform dependent."
  (bt:thread-alive-p process))

;;; This would require some wapper around the process primitive.
(defun process-whostate (process)
  "Returns the whostate of the process. Platform dependent."
  (declare (ignore process))
  nil)

(defun process-wait (reason predicate)
  "Causes the current process to wait until PREDICATE returns
true. REASON is a reason for waiting, usually a string. On systems
that do not support multi-processing, process-wait will loop until
predicate returns true."
  (declare (ignore reason))
  (loop (when (funcall predicate)
          (return))
     ;; (sleep 0.1)
     (process-yield)))

(defun process-wait-with-timeout (reason timeout predicate)
  "Causes the current process to wait until either predicate returns
true, or the number of seconds specified by timeout has
elapsed. reason is a \"reason\" for waiting, usually a string. On
systems that do not support multi-processing,
process-wait-with-timeout will loop until predicate returns true or
the timeout has elapsed."
  (declare (ignore reason))
  (let ((end-time (+ (get-universal-time) timeout)))
    (loop (when (or (funcall predicate)
                    (> (get-universal-time) end-time))
            (return))
       (process-yield))))

(defun process-yield ()
  "Allows other processes to run. On systems that do not support
multi-processing, this does nothing."
  (bt:thread-yield))

(defun process-interrupt (process function)
    "Interrupts the process process and causes it to evaluate the
function function. On systems that do not support multi-processing,
this is equivalent to funcall'ing function."
  (bt:interrupt-thread process function))

;;; These functions aren't supported by bordeaux-threads and arguably
;;; shouldn't be supported by McCLIM to keep backend as portable as
;;; possible.
;;;
;;; DISABLE-PROCESS, ENABLE-PROCESS, RESTART-PROCESS,
;;; WITHOUT-SCHEDULING, ATOMIC-INCF, ATOMIC-DECF.

(defun disable-process (process)
  "Disables the process process from becoming runnable until it is
enabled again."
  (declare (ignore process))
  (error "Bordeaux-threads doesn't support DISABLE-PROCESS"))

(defun enable-process (process)
  "Allows the process process to become runnable again after it has
been disabled."
  (declare (ignore process))
  (error "Bordeaux-threads doesn't support ENABLE-PROCESS"))

(defun restart-process (process)
  "Restarts the process process by \"unwinding\" it to its initial
state, and reinvoking its top-level function."
  (declare (ignore process))
  (error "Bordeaux-threads doesn't support RESTART-PROCESS"))

;;; Usless on SMP systems, replacing with PROGN.
(defmacro without-scheduling (&body body)
  "Evaluates body in a context that is guaranteed to be free from
interruption by other processes. On systems that do not support
multi-processing, without-scheduling is equivalent to progn."
  (declare (ignorable body))
  #+clim-mp (error "WITHOUT-SCHEDULING not supported.")
  #-clim-mp `(progn ,@body))

(defun atomic-incf (reference)
  "Increments the fixnum value referred to by reference as a single,
atomic operation."
  (declare (ignorable reference))
  #+clim-mp (error "Atomic operations aren't supported.")
  #-clim-mp (incf reference))

(defun atomic-decf (reference)
  "Decrements the fixnum value referred to by reference as a single,
atomic operation."
  (declare (ignorable reference))
  #+clim-mp (error "Atomic operations aren't supported.")
  #-clim-mp (decf reference))


;;; B.3 Locks

(defun make-lock (&optional name)
  "Creates a lock whose name is name.

On systems that do not support locking, this will return a new list of
one element, nil."
  (bt:make-lock name))

(defmacro with-lock-held ((place &optional state) &body body)
  "Evaluates body with the lock named by place. place is a reference
to a lock created by make-lock.

On systems that do not support locking, with-lock-held is equivalent
to progn."
  (declare (ignore state))
  `(bt:with-lock-held (,place) ,@body))

(defun make-recursive-lock (&optional name)
  "Creates a recursive lock whose name is name.

On systems that do not support locking, this will return a new list of
one element, nil. A recursive lock differs from an ordinary lock in
that a process that already holds the recursive lock can call
with-recursive-lock-held on the same lock without blocking."
  (bt:make-recursive-lock name))

(defmacro with-recursive-lock-held ((place &optional state) &body body)
  "Evaluates body with the recursive lock named by place. place is a
reference to a recursive lock created by make-recursive-lock.

On systems that do not support locking, with-recursive-lock-held is
equivalent to progn."
  (declare (ignore state))
  `(bt:with-recursive-lock-held (,place) ,@body))


;;; O.0 Conditionals
;;; 
;;; These functions aren't in the CLIM specification but event queue uses them.

(defun make-condition-variable ()
  (bt:make-condition-variable))

(defun condition-wait (cv lock &optional timeout)
  ;; Some implementations have interface for condition wait with timeout, but
  ;; raise an error when this function is called. When not necessary, we provide
  ;; working variant.
  (if timeout
      (bt:condition-wait cv lock :timeout timeout)
      (bt:condition-wait cv lock)))

(defun condition-notify (cv)
  (bt:condition-notify cv))
