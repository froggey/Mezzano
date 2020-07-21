;;;; libdispatch inspired concurrency API.

(defpackage :mezzano.sync.dispatch
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:pool :mezzano.sync.thread-pool)
                    (:sync :mezzano.sync)
                    (:clos :mezzano.clos))
  (:export #:context
           #:local-context
           #:make-dispatch-context
           #:dispatch-shutdown

           ;; Generic dispatch objects
           #:dispatch-object

           #:resume
           #:suspend
           #:cancel
           #:canceled-p
           #:wait
           #:notify

           ;; Queues
           #:queue
           #:serial-queue
           #:concurrent-queue
           #:manager-queue
           #:global-queue
           #:standard-queue
           #:standard-serial-queue
           #:standard-concurrent-queue

           #:make-queue
           #:dispatch-async
           #:dispatch-sync
           #:dispatch-multiple
           #:dispatch-after
           #:dispatch-delayed

           ;; Sources
           #:source
           #:make-source
           #:source-target
           #:source-event
           #:source-handler
           #:source-cancellation-handler

           ;; Groups
           #:group
           #:make-group
           #:group-enter
           #:group-leave
))

(in-package :mezzano.sync.dispatch)

;;; A context owns a set of queues, sources, threads, etc.

(defclass context ()
  ((%name :reader context-name :initarg :name)
   (%thread-pool :reader context-thread-pool)
   (%manager-thread :reader context-manager-thread)
   (%manager-mailbox :reader context-manager-mailbox)
   (%manager-queue :reader context-manager-queue)
   (%manager-events :initform '() :accessor context-manager-events)
   (%global-queue :reader context-global-queue))
  (:default-initargs :name nil))

(defmethod print-object ((instance context) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~A" (context-name instance))))

(defvar *local-context* nil)

(defun local-context ()
  *local-context*)

(defun manager ()
  (let* ((context *local-context*)
         (mailbox (context-manager-mailbox context)))
    (with-simple-restart (abort "Terminate manager thread for context ~S" context)
      (loop
         (dolist (evt (apply #'sync:wait-for-objects mailbox (context-manager-events context)))
           (cond ((eql evt mailbox)
                  ;; Process pending functions.
                  (loop
                     (multiple-value-bind (value validp)
                         (sync:mailbox-receive mailbox :wait-p nil)
                       (when (not validp)
                         (return))
                       (with-simple-restart (continue "Ignore manager work ~S" value)
                         (funcall value)))))
                 (t
                  (with-simple-restart (continue "Ignore source event ~S" evt)
                    (process-source evt)))))))))

(defmethod initialize-instance :after ((instance context) &key initial-bindings)
  (push (list '*local-context* instance) initial-bindings)
  (setf (slot-value instance '%thread-pool)
        (pool:make-thread-pool :name instance
                               :initial-bindings initial-bindings))
  (setf (slot-value instance '%manager-mailbox)
        (sync:make-mailbox :name `(manager-mailbox ,instance)))
  (setf (slot-value instance '%manager-thread)
        (sup:make-thread #'manager
                         :name `(manager ,instance)
                         :initial-bindings initial-bindings))
  (setf (slot-value instance '%manager-queue)
        (make-instance 'manager-queue :context instance))
  (setf (slot-value instance '%global-queue)
        (make-instance 'global-queue :context instance)))

(defun make-dispatch-context (&key initial-bindings initial-work name)
  "Create a new dispatch context.
INITIAL-BINDINGS is a list of (symbol value) pairs which specifies the initial
values of the given symbols. Modifications to these bindings may or may not
persist between task invocations.
INITIAL-WORK will be submitted to the manager queue immediately on context
creation and can be used to perform initial setup."
  (let ((ctx (make-instance 'context
                            :initial-bindings initial-bindings
                            :name name)))
    (when initial-work
      (dispatch-async initial-work (manager-queue :context ctx)))
    ctx))

(defun dispatch-shutdown (&key (context *local-context*) abort)
  "Shut down CONTEXT.
Shuts down the thread pool and terminates the manager thread.
If ABORT is true then threads will be terminated immediately, else it
will wait for the currently running tasks to complete."
  (pool:thread-pool-shutdown (context-thread-pool context) :abort abort)
  (cond (abort
         (sup:terminate-thread (context-manager-thread context)))
        (t
         (dispatch-async (lambda () (throw 'sup:terminate-thread nil))
                         (manager-queue :context context))))
  (values))

;;; Generic dispatch-related objects.

(defgeneric context (object)
  (:documentation "Return the dispatch context in which OBJECT exists."))

(defgeneric resume (object)
  (:documentation "Resumes the invocation of blocks on a dispatch object."))
(defgeneric suspend (object)
  (:documentation "Suspends the invocation of blocks on a dispatch object."))

(defgeneric cancel (object)
  (:documentation "Asynchronously cancel the specifed object."))
(defgeneric canceled-p (object)
  (:documentation "Test whether the specified object has been canceled."))

(defgeneric wait (object &key timeout)
  (:documentation "Wait synchronously for an object or until the specified timeout has elapsed."))
(defgeneric notify (object function &key target)
  (:documentation "Schedule a notification function to be submitted to a queue when the execution of a specified object has completed."))

(defclass dispatch-object ()
  ((%context :initarg :context :reader context))
  (:documentation "Base class of all dispatch objects associated with a context."))

;;; Queues

(defclass queue (dispatch-object)
  ()
  (:documentation "Base class of all queues.
Queues must implement DISPATCH-ASYNC."))

(defclass serial-queue (queue) ()
  (:documentation "An abstract queue type that dispatches tasks one by one."))
(defclass concurrent-queue (queue) ()
  (:documentation "An abstract queue type that dispatches tasks in parallel."))

(defclass manager-queue (serial-queue) ()
  (:documentation "A queue type that dispatches events serially on the context's manager thread."))
(defclass global-queue (concurrent-queue) ()
  (:documentation "A queue type that dispatches events concurrently using the context's thread pool."))

(defclass standard-queue (queue)
  ((%name :initarg :name :reader standard-queue-name)
   (%target :initarg :target :reader standard-queue-target)
   (%active :initarg :active :accessor standard-queue-active-p)
   (%lock :reader queue-lock)))

(defmethod initialize-instance :after ((instance standard-queue) &key)
  (setf (slot-value instance '%lock) (sup:make-mutex instance)))

(defclass standard-serial-queue (standard-queue serial-queue)
  ((%queue-running :initform nil :accessor queue-running-p)
   (%pending :initform '() :accessor queue-pending)))

(defclass standard-concurrent-queue (standard-queue concurrent-queue)
  ((%queue-work-count :initform 0 :accessor queue-work-count)
   (%pending :initform '() :accessor queue-pending)
   (%barriered :initform nil :accessor queue-barriered)))

(defun global-queue (&key context priority)
  "Return the global queue for CONTEXT for the specified PRIORITY.
CONTEXT defaults to the local context."
  (declare (ignore priority))
  (context-global-queue (or context *local-context*)))

(defun manager-queue (&key context)
  "Return the manager queue for CONTEXT
CONTEXT defaults to the local context."
  (context-manager-queue (or context *local-context*)))

(defgeneric dispatch-async (function queue &key barrier group)
  (:documentation "Submits a function for asynchronous execution on a dispatch queue.
If BARRIER is supplied, then all prior functions will be executed before the function
is executed and functions added after will be deferred until the barrier function completes.
GROUP specifies a group to associate this function with."))

(defmethod dispatch-async (function (queue manager-queue) &key barrier group)
  (declare (ignore barrier))
  (sync:mailbox-send (group-wrap function group)
                     (context-manager-mailbox (context queue)))
  (values))

(defmethod dispatch-async (function (queue global-queue) &key barrier group)
  (when barrier
    (error "Barrier not supported on global queues."))
  (pool:thread-pool-add (group-wrap function group)
                        (context-thread-pool (context queue)))
  (values))

(defun make-queue (&key name target context concurrent priority suspended)
  "Create a new standard queue.
The queue will dispatch tasks using the TARGET queue or CONTEXT's global
queue for supplied priority.
If SUSPENDED is true then the queue will initially be suspended and
RESUME must be called before tasks are executed."
  (when (and target (not context))
    (setf context (context target)))
  (when target
    (assert (not priority) (priority) "Can't specify priority and target queue"))
  (when (not target)
    (setf target (global-queue :context (or context *local-context*)
                               :priority priority)))
  (when (and (typep target 'serial-queue)
             concurrent)
    (error "Cannot target a concurrent queue on to serial queue ~S" target))
  (when context
    (assert (eql context (context target))))
  (make-instance (if concurrent
                     'standard-concurrent-queue
                     'standard-serial-queue)
                 :name name
                 :target target
                 :context (context target)
                 :active (not suspended)))

(defun serial-queue-runner (queue)
  (loop
     (let ((current (sup:with-mutex ((queue-lock queue))
                      (when (or (not (standard-queue-active-p queue))
                                (endp (queue-pending queue)))
                        (setf (queue-running-p queue) nil)
                        (return-from serial-queue-runner))
                      (pop (queue-pending queue)))))
       (funcall current))))

(defmethod dispatch-async (function (queue standard-serial-queue) &key barrier group)
  (declare (ignore barrier))
  (sup:with-mutex ((queue-lock queue))
    (when (and (standard-queue-active-p queue)
               (not (queue-running-p queue)))
      (setf (queue-running-p queue) t)
      (dispatch-async (lambda () (serial-queue-runner queue))
                      (standard-queue-target queue)))
    (setf (queue-pending queue) (append (queue-pending queue)
                                        (list (group-wrap function group)))))
  (values))

(defmethod resume ((queue standard-serial-queue))
  (sup:with-mutex ((queue-lock queue))
    (when (not (standard-queue-active-p queue))
      (setf (standard-queue-active-p queue) t)
      (when (and (not (queue-running-p queue))
                 (queue-pending queue))
        ;; There is work pending but no active runner. (Re)start it.
        (setf (queue-running-p queue) t)
        (dispatch-async (lambda () (serial-queue-runner queue))
                        (standard-queue-target queue)))))
  (values))

(defmethod suspend ((queue standard-serial-queue))
  (sup:with-mutex ((queue-lock queue))
    (setf (standard-queue-active-p queue) nil))
  (values))

(defclass barrier-work ()
  ((%function :initarg :function :reader barrier-work-function)))

(defun run-concurrent-fn (queue function)
  (unwind-protect
       (funcall function)
    (sup:with-mutex ((queue-lock queue))
      (decf (queue-work-count queue))
      (when (and (standard-queue-active-p queue)
                 (zerop (queue-work-count queue))
                 (queue-pending queue))
        ;; There's pending work, this should be a barrier function.
        (let ((fn (pop (queue-pending queue))))
          (assert (typep fn 'barrier-work))
          (setf fn (barrier-work-function fn))
          (incf (queue-work-count queue))
          (setf (queue-barriered queue) t)
          (dispatch-async (lambda ()
                            (run-concurrent-barrier-fn queue fn))
                          (standard-queue-target queue)))))))

(defun run-concurrent-barrier-fn (queue function)
  (unwind-protect
       (funcall function)
    (sup:with-mutex ((queue-lock queue))
      (decf (queue-work-count queue))
      (assert (queue-barriered queue))
      (assert (zerop (queue-work-count queue)))
      (setf (queue-barriered queue) nil)
      (when (and (standard-queue-active-p queue)
                 (queue-pending queue))
        (cond ((typep (first (queue-pending queue)) 'barrier-work)
               ;; If the first function is a barrier function, then just run that.
               (let ((fn (barrier-work-function (pop (queue-pending queue)))))
                 (setf (queue-barriered queue) t)
                 (incf (queue-work-count queue))
                 (dispatch-async (lambda ()
                                   (run-concurrent-barrier-fn queue fn))
                                 (standard-queue-target queue))))
              (t
               ;; Otherwise, dispatch all pending work units until a
               ;; barrier or end-of-list is seen.
               (loop
                  (when (or (endp (queue-pending queue))
                            (typep (first (queue-pending queue)) 'barrier-work))
                    (return))
                  (let ((fn (pop (queue-pending queue))))
                    (incf (queue-work-count queue))
                    (dispatch-async (lambda ()
                                      (run-concurrent-fn queue fn))
                                    (standard-queue-target queue))))))))))

(defmethod dispatch-async (function (queue standard-concurrent-queue) &key barrier group)
  (setf function (group-wrap function group))
  (sup:with-mutex ((queue-lock queue))
    (cond ((identity barrier)
           (cond ((and (standard-queue-active-p queue)
                       (zerop (queue-work-count queue)))
                  (setf (queue-barriered queue) t)
                  (incf (queue-work-count queue))
                  (dispatch-async (lambda ()
                                    (run-concurrent-barrier-fn queue function))
                                  (standard-queue-target queue)))
                 (t
                  ;; Outstanding work or inactive queue, just add to the
                  ;; pending list. It'll be picked up when work finishes.
                  (let ((barrier-fn (make-instance 'barrier-work
                                                   :function function)))
                    (setf (queue-pending queue) (append (queue-pending queue)
                                                        (list barrier-fn)))))))
          ((or (not (standard-queue-active-p queue))
               (queue-pending queue)
               (queue-barriered queue))
           (setf (queue-pending queue) (append (queue-pending queue)
                                               (list function))))
          (t
           (incf (queue-work-count queue))
           (dispatch-async (lambda () (run-concurrent-fn queue function))
                           (standard-queue-target queue)))))
  (values))

(defmethod resume ((queue standard-concurrent-queue))
  (sup:with-mutex ((queue-lock queue))
    (when (not (standard-queue-active-p queue))
      (setf (standard-queue-active-p queue) t)
      (when (and (zerop (queue-work-count queue))
                 (queue-pending queue))
        ;; There is work pending but no active runners.
        (cond ((typep (first (queue-pending queue)) 'barrier-work)
               ;; If the first function is a barrier function, then just run that.
               (let ((fn (barrier-work-function (pop (queue-pending queue)))))
                 (setf (queue-barriered queue) t)
                 (incf (queue-work-count queue))
                 (dispatch-async (lambda ()
                                   (run-concurrent-barrier-fn queue fn))
                                 (standard-queue-target queue))))
              (t
               ;; Otherwise, dispatch all pending work units until a
               ;; barrier or end-of-list is seen.
               (loop
                  (when (or (endp (queue-pending queue))
                            (typep (first (queue-pending queue)) 'barrier-work))
                    (return))
                  (let ((fn (pop (queue-pending queue))))
                    (incf (queue-work-count queue))
                    (dispatch-async (lambda ()
                                      (run-concurrent-fn queue fn))
                                    (standard-queue-target queue)))))))))
  (values))

(defmethod suspend ((queue standard-concurrent-queue))
  (sup:with-mutex ((queue-lock queue))
    (setf (standard-queue-active-p queue) nil))
  (values))

(defun dispatch-sync (function queue &key barrier)
  "Dispatch FUNCTION on QUEUE and wait for completion."
  (let ((group (make-group :context (context queue))))
    (dispatch-async function queue :group group :barrier barrier)
    (wait group))
  (values))

(defun dispatch-multiple (function iterations queue &key group)
  "Dispatch FUNCTION on QUEUE multiple times. The function is called with the iteration number."
  (dotimes (i iterations)
    (let ((n i))
      (dispatch-async (lambda () (funcall function n))
                      queue
                      :group group)))
  (values))

(defun dispatch-after (function run-time queue)
  "Submits a function for asynchronous execution on a dispatch queue after the specified time."
  (let* ((timer (mezzano.supervisor:make-timer :deadline run-time))
         (source (make-source timer nil
                              :cancellation-handler (lambda ()
                                                      ;; Return the timer to the pool after the source
                                                      ;; is fully canceled.
                                                      (mezzano.supervisor::push-timer-pool timer))
                              :suspended t)))
    (setf (source-handler source) (lambda ()
                                    (dispatch-async function queue)
                                    ;; Disarm the timer to prevent the source
                                    ;; from immediately refiring (suspend is async).
                                    (mezzano.supervisor:timer-disarm timer)
                                    ;; Cancel the source to stop it hanging around.
                                    (cancel source)))
    (resume source))
  (values))

(defun delay-to-run-time (delay)
  "Convert a delay (a non-negative real representing seconds) to an absolute run time."
  (+ (get-internal-run-time)
     (truncate (* delay internal-time-units-per-second))))

(defun dispatch-delayed (function delay queue)
  "Submits a function for asynchronous execution on a dispatch queue after the specified delay."
  (dispatch-at function (delay-to-run-time delay) queue)
  (values))

;;; Sources

;; Implementation detail:
;; Sources are operated on entirely from the manager thread,
;; this avoids the need for any locking or other kinds of
;; synchronization.

(defclass source (dispatch-object)
  ((%target :initarg :target :reader source-target)
   (%event :initarg :event :reader source-event)
   (%handler :initarg :handler :reader source-handler)
   (%cancellation-handler :initarg :cancellation-handler
                          :reader source-cancellation-handler)
   (%state :initform :suspended :accessor source-state))
  (:documentation "A source connects an event to a dispatch context."))

(defmethod print-object ((instance source) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~A targeting ~S" (source-event instance) (source-target instance))))

(defmethod sync:get-object-event ((object source))
  (sync:get-object-event (source-event object)))

(defun make-source (event handler &key target cancellation-handler suspended)
  "Create a new source object connected to EVENT.
HANDLER is the function to be called when EVENT becomes active. It
can initially be NIL but must be set via (SETF SOURCE-HANDLER) before
the source is initially activated.
TARGET is the queue that the handlers are to be dispatched on. It defaults
to the global queue for the current context.
CANCELLATION-HANDLER is a function that will be dispatched when CANCEL completes
and the source has been fully unregistered.
SUSPENDED specifies if the source should start in a suspeneded state or
if it should immediately begin dispatching events."
  (assert (or handler suspended) (handler suspended) "The queue must start in a suspended state if no handler is supplied.")
  (let* ((target (or target (global-queue)))
         (context (context target))
         (source (make-instance 'source
                                :event event
                                :context context
                                :target target
                                :handler handler
                                :cancellation-handler cancellation-handler)))
    (when (not suspended)
      (resume source))
    source))

(defun %set-source-handler (value source)
  (assert (eql (source-state source) :suspended))
  (setf (slot-value source '%handler) value))

(defun (setf source-handler) (value source)
  (check-type value (or function null))
  (dispatch-async (lambda () (%set-source-handler value source))
                  (manager-queue :context (context source)))
  value)

(defun %resume-source (source)
  (ecase (source-state source)
    (:suspended
     (assert (source-handler source))
     (setf (source-state source) :active)
     (push source (context-manager-events *local-context*)))
    (:event-running-suspend-requested
     (setf (source-state source) :event-running))
    (:event-running)
    (:active)
    ((:canceled :event-running-cancel-requested)
     (error "Cannot resume a canceled source ~S" source))))

(defmethod resume ((source source))
  (dispatch-async (lambda () (%resume-source source))
                  (manager-queue :context (context source)))
  (values))

(defun %suspend-source (source)
  (ecase (source-state source)
    ((:suspended :canceled))
    ((:event-running-suspend-requested :event-running-cancel-requested))
    (:event-running
     (setf (source-state source) :event-running-suspend-requested))
    (:active
     (setf (source-state source) :suspended)
     (setf (context-manager-events *local-context*)
           (remove source (context-manager-events *local-context*))))))

(defmethod suspend ((source source))
  (dispatch-async (lambda () (%suspend-source source))
                  (manager-queue :context (context source)))
  (values))

(defun process-source (source)
  ;; can't be in either of the event-running states.
  (ecase (source-state source)
    (:suspended :canceled) ; possible if the source was suspended at the same time that it was triggered
    (:active
     (setf (source-state source) :event-running)
     (setf (context-manager-events *local-context*)
           (remove source (context-manager-events *local-context*)))
     (dispatch-async (lambda ()
                       (unwind-protect
                            (funcall (source-handler source))
                         (dispatch-async (lambda () (complete-source source))
                                         (manager-queue :context (context source)))))
                     (source-target source)))))

(defun complete-source (source)
  (ecase (source-state source)
    (:event-running-cancel-requested
     (setf (source-state source) :canceled)
     (when (source-cancellation-handler source)
       (dispatch-async (source-cancellation-handler source)
                       (source-target source))))
    (:event-running-suspend-requested
     (setf (source-state source) :suspended))
    (:event-running
     (setf (source-state source) :active)
     (push source (context-manager-events *local-context*)))))

(defun %cancel-source (source)
  (ecase (source-state source)
    ((:canceled :event-running-cancel-requested))
    (:suspended
     (setf (source-state source) :canceled)
     (when (source-cancellation-handler source)
       (dispatch-async (source-cancellation-handler source)
                       (source-target source))))
    ((:event-running-suspend-requested :event-running)
     (setf (source-state source) :event-running-cancel-requested))
    (:active
     (setf (source-state source) :canceled)
     (setf (context-manager-events *local-context*)
           (remove source (context-manager-events *local-context*)))
     (when (source-cancellation-handler source)
       (dispatch-async (source-cancellation-handler source)
                       (source-target source))))))

(defmethod cancel ((source source))
  (dispatch-async (lambda () (%cancel-source source))
                  (manager-queue :context (context source)))
  (values))

(defmethod canceled-p ((source source))
  (eql (source-state source) :canceled))

;;; Groups

(defclass group (dispatch-object)
  ((%count :initform 0 :accessor group-count)
   (%pending :initform '() :accessor group-notifiers)
   (%lock :initform (sup:make-mutex) :reader group-lock)
   (%cvar :initform (sup:make-condition-variable) :reader group-cvar)))

(defun make-group (&key (context *local-context*))
  "Create a new group.
Groups can be used to determine when a collections of work items
have all been completed."
  (make-instance 'group :context context))

(defmethod wait ((group group) &key timeout)
  (sup:with-mutex ((group-lock group))
    (sup:condition-wait-for ((group-cvar group) (group-lock group) timeout)
      (zerop (group-count group)))))

(defmethod notify (function (group group) &key target)
  (setf target (or target (global-queue :context (context group))))
  (sup:with-mutex ((group-lock group))
    (if (zerop (group-count group))
        (dispatch-async function target)
        (push (list function target) (group-notifiers group))))
  (values))

(defun group-enter (group)
  "Manually enter a group, incrementing the internal counter."
  (sup:with-mutex ((group-lock group))
    (incf (group-count group)))
  (values))

(defun group-leave (group)
  "Manually leave a group, decrementing the internal counter.
When the group's internal counter reaches zero all pending
notifications will be dispatched."
  (sup:with-mutex ((group-lock group))
    (decf (group-count group))
    (when (zerop (group-count group))
      (sup:condition-notify (group-cvar group) t)
      (loop
         for (function target) in (group-notifiers group)
         do (dispatch-async function target))
      (setf (group-notifiers group) '())))
  (values))

(defun group-wrap (function group)
  (cond (group
         (group-enter group)
         (lambda ()
           (unwind-protect
                (funcall function)
             (group-leave group))))
        (t
         function)))
