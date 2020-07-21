;;;; A thread pool

(defpackage :mezzano.sync.thread-pool
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor))
  (:export #:*default-keepalive-time*
           #:thread-pool
           #:work-item
           #:make-thread-pool
           #:thread-pool-add
           #:thread-pool-add-many
           #:thread-pool-cancel-item
           #:thread-pool-flush
           #:thread-pool-shutdown

           ;; Catch tag that can be used as a throw target to
           ;; leave the current task.
           #:terminate-work))

(in-package :mezzano.sync.thread-pool)

(defparameter *default-keepalive-time* 60
  "Default value for the idle worker thread keepalive time.")

(defclass thread-pool ()
  ((%name :initarg :name :reader thread-pool-name)
   (%initial-bindings :initarg :initial-bindings :reader thread-pool-initial-bindings)
   (%lock :reader thread-pool-lock)
   (%cvar :reader thread-pool-cvar)
   (%pending :initform '() :accessor thread-pool-pending)
   (%working-threads :initform '() :accessor thread-pool-working-threads)
   (%idle-threads :initform '() :accessor thread-pool-idle-threads)
   (%n-total-threads :initform 0 :accessor thread-pool-n-total-threads)
   (%n-blocked-threads :initform 0 :accessor thread-pool-n-blocked-threads)
   (%shutdown :initform nil :accessor thread-pool-shutdown-p)
   (%keepalive-time :initarg :keepalive-time :accessor thread-pool-keepalive-time))
  (:default-initargs :name nil :initial-bindings '()))

(defmethod initialize-instance :after ((instance thread-pool) &key)
  (setf (slot-value instance '%lock) (sup:make-mutex instance)
        (slot-value instance '%cvar) (sup:make-condition-variable instance)))

(defclass work-item ()
  ((%name :initarg :name :reader work-item-name)
   (%function :initarg :function :reader work-item-function)
   (%thread-pool :initarg :thread-pool :reader work-item-thread-pool)))

(defun make-thread-pool (&key name initial-bindings (keepalive-time *default-keepalive-time*))
  "Create a new thread-pool."
  (check-type *default-keepalive-time* (rational 0))
  (make-instance 'thread-pool
                 :name name
                 :initial-bindings initial-bindings
                 :keepalive-time keepalive-time))

(defmethod sup:thread-pool-block ((thread-pool thread-pool) blocking-function &rest arguments)
  (declare (dynamic-extent arguments))
  (when (and (eql blocking-function 'mezzano.supervisor:acquire-mutex)
             (eql (first arguments) (thread-pool-lock thread-pool)))
    ;; Don't suspend when acquiring the thread-pool lock, this causes
    ;; recursive locking on it.
    (return-from sup:thread-pool-block
      (apply blocking-function arguments)))
  (unwind-protect
       (progn
         (sup:with-mutex ((thread-pool-lock thread-pool))
           (incf (thread-pool-n-blocked-threads thread-pool)))
         (apply blocking-function arguments))
    (sup:with-mutex ((thread-pool-lock thread-pool))
      (decf (thread-pool-n-blocked-threads thread-pool)))))

(defun thread-pool-n-concurrent-threads (thread-pool)
  "Return the number of threads in the pool are not blocked."
  (- (thread-pool-n-total-threads thread-pool)
     (thread-pool-n-blocked-threads thread-pool)))

(defun thread-pool-main (thread-pool)
  (let* ((self (sup:current-thread))
         (thread-name (sup:thread-name self)))
    (loop
       (let ((work nil))
         (sup:with-mutex ((thread-pool-lock thread-pool))
           ;; Move from active to idle.
           (setf (thread-pool-working-threads thread-pool)
                 (remove self (thread-pool-working-threads thread-pool)))
           (push self (thread-pool-idle-threads thread-pool))
           (setf (third thread-name) nil)
           (let ((start-idle-time (get-internal-run-time)))
             (flet ((exit-while-idle ()
                      (setf (thread-pool-idle-threads thread-pool)
                            (remove self (thread-pool-idle-threads thread-pool)))
                      (decf (thread-pool-n-total-threads thread-pool))
                      (return-from thread-pool-main)))
               (loop
                  (when (thread-pool-shutdown-p thread-pool)
                    (exit-while-idle))
                  (when (not (endp (thread-pool-pending thread-pool)))
                    (setf work (pop (thread-pool-pending thread-pool)))
                    (setf (third thread-name) work)
                    ;; Back to active from idle.
                    (setf (thread-pool-idle-threads thread-pool)
                          (remove self (thread-pool-idle-threads thread-pool)))
                    (push self (thread-pool-working-threads thread-pool))
                    (return))
                  ;; If there is no work available and there are more
                  ;; unblocked threads than cores, then terminate this thread.
                  (when (> (thread-pool-n-concurrent-threads thread-pool)
                           (sup:logical-core-count))
                    (exit-while-idle))
                  (let* ((end-idle-time (+ start-idle-time (* (thread-pool-keepalive-time thread-pool) internal-time-units-per-second)))
                         (idle-time-remaining (- end-idle-time (get-internal-run-time))))
                    (when (minusp idle-time-remaining)
                      (exit-while-idle))
                    (sup:condition-wait (thread-pool-cvar thread-pool)
                                        (thread-pool-lock thread-pool)
                                        (/ idle-time-remaining internal-time-units-per-second)))))))
         (setf (sup:thread-thread-pool self) thread-pool)
         (mezzano.internals::unwind-protect-unwind-only
              (catch 'terminate-work
                (funcall (work-item-function work)))
           ;; Getting here means an unwind occured in the work item and
           ;; this thread is terminating in the active state. Clean up.
           (setf (thread-pool-working-threads thread-pool)
                 (remove self (thread-pool-working-threads thread-pool)))
           (decf (thread-pool-n-total-threads thread-pool)))
         (setf (sup:thread-thread-pool self) nil)))))

(defun thread-pool-add (function thread-pool &key name priority bindings)
  "Add a work item to the thread-pool.
Functions are called concurrently and in FIFO order.
A work item is returned, which can be passed to THREAD-POOL-CANCEL-ITEM
to attempt cancel the work.
BINDINGS is a list of (SYMBOL VALUE) pairs which specify special bindings
that should be active when FUNCTION is called. These override the
thread pool's initial-bindings."
  (declare (ignore priority)) ; TODO
  (check-type function function)
  (let ((work (make-instance 'work-item
                             :function (if bindings
                                           (let ((vars (mapcar #'first bindings))
                                                 (vals (mapcar #'second bindings)))
                                             (lambda ()
                                               (progv vars vals
                                                 (funcall function))))
                                           function)
                             :name name
                             :thread-pool thread-pool)))
    (sup:with-mutex ((thread-pool-lock thread-pool) :resignal-errors t)
      (when (thread-pool-shutdown-p thread-pool)
        (error "Attempted to add work item to shut down thread pool ~S" thread-pool))
      (setf (thread-pool-pending thread-pool) (append (thread-pool-pending thread-pool) (list work)))
      (when (and (endp (thread-pool-idle-threads thread-pool))
                 (< (thread-pool-n-concurrent-threads thread-pool)
                    (sup:logical-core-count)))
        ;; There are no idle threads and there are more logical cores than
        ;; currently running threads. Create a new thread for this work item.
        ;; Push it on the active list to make the logic in T-P-MAIN work out.
        (push (sup:make-thread (lambda () (thread-pool-main thread-pool))
                               :name `(thread-pool-worker ,thread-pool nil)
                               :initial-bindings (thread-pool-initial-bindings thread-pool))
              (thread-pool-working-threads thread-pool))
        (incf (thread-pool-n-total-threads thread-pool)))
      (sup:condition-notify (thread-pool-cvar thread-pool)))
    work))

(defun thread-pool-add-many (function values thread-pool &key name priority bindings)
  "Add many work items to the pool.
A work item is created for each element of VALUES and FUNCTION is called
in the pool with that element.
Returns a list of the work items added."
  (loop
     for value in values
     collect (thread-pool-add
              (let ((value value))
                (lambda () (funcall function value)))
              thread-pool
              :name name
              :priority priority
              :bindings bindings)))

(defun thread-pool-cancel-item (item)
  "Cancel a work item, removing it from its thread-pool.
Returns true if the item was successfully cancelled,
false if the item had finished or is currently running on a worker thread."
  (let ((thread-pool (work-item-thread-pool item)))
    (sup:with-mutex ((thread-pool-lock thread-pool))
      (cond ((find item (thread-pool-pending thread-pool))
             (setf (thread-pool-pending thread-pool) (remove item (thread-pool-pending thread-pool)))
             t)
            (t
             nil)))))

(defun thread-pool-flush (thread-pool)
  "Cancel all outstanding work on THREAD-POOL.
Returns a list of all cancelled items.
Does not cancel work in progress."
  (sup:with-mutex ((thread-pool-lock thread-pool))
    (prog1
        (thread-pool-pending thread-pool)
      (setf (thread-pool-pending thread-pool) '()))))

(defun thread-pool-shutdown (thread-pool &key abort)
  "Shutdown THREAD-POOL.
This cancels all outstanding work on THREAD-POOL
and notifies the worker threads that they should
exit once their active work is complete.
Once a thread pool has been shut down, no further work
can be added.
If ABORT is true then worker threads will be terminated
via TERMINATE-THREAD."
  (sup:with-mutex ((thread-pool-lock thread-pool))
    (setf (thread-pool-shutdown-p thread-pool) t)
    (setf (thread-pool-pending thread-pool) '())
    (when abort
      (dolist (thread (thread-pool-working-threads thread-pool))
        (sup:terminate-thread thread))
      (dolist (thread (thread-pool-idle-threads thread-pool))
        (sup:terminate-thread thread)))
    (sup:condition-notify (thread-pool-cvar thread-pool) t))
  (values))
