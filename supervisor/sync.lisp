;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Syncronization primitives.

(in-package :mezzano.supervisor)

;;; Thread pool support for hijacking blocking functions.
;;; When the current thread's thread-pool slot is non-nil, the blocking
;;; functions will call THREAD-POOL-BLOCK with the thread pool, the name
;;; of the function and supplied arguments instead of actually blocking.
;;; The thread's thread-pool slot will be set to NIL for the duration
;;; of the call to THREAD-POOL-BLOCK.

(defmacro thread-pool-blocking-hijack (function-name &rest arguments)
  (let ((self (gensym "SELF"))
        (pool (gensym "POOL")))
    `(let* ((,self (current-thread))
            (,pool (thread-thread-pool ,self)))
       (when ,pool
         (unwind-protect
              (progn
                (setf (thread-thread-pool ,self) nil)
                (return-from ,function-name
                  (thread-pool-block ,pool ',function-name ,@arguments)))
           (setf (thread-thread-pool ,self) ,pool))))))

(defmacro thread-pool-blocking-hijack-apply (function-name &rest arguments)
  (let ((self (gensym "SELF"))
        (pool (gensym "POOL")))
    `(let* ((,self (current-thread))
            (,pool (thread-thread-pool ,self)))
       (when ,pool
         (unwind-protect
              (progn
                (setf (thread-thread-pool ,self) nil)
                (return-from ,function-name
                  (apply #'thread-pool-block ,pool ',function-name ,@arguments)))
           (setf (thread-thread-pool ,self) ,pool))))))

;;; Common structure for sleepable things.
(defstruct (wait-queue
             (:area :wired))
  (name nil :read-only t)
  (%lock (place-spinlock-initializer))
  (head nil)
  (tail nil))

(define-doubly-linked-list-helpers wait-queue
    thread-queue-next thread-queue-prev
    wait-queue-head wait-queue-tail)

(defun push-wait-queue (thread wait-queue)
  (wait-queue-push-back thread wait-queue))

(defun pop-wait-queue (wait-queue)
  (wait-queue-pop-front wait-queue))

(defun remove-from-wait-queue (thread wait-queue)
  (when (wait-queue-linked-p thread)
    (wait-queue-remove thread wait-queue)))

(defun lock-wait-queue (wait-queue)
  (acquire-place-spinlock (wait-queue-%lock wait-queue)))

(defun unlock-wait-queue (wait-queue)
  (release-place-spinlock (wait-queue-%lock wait-queue)))

(defmacro with-wait-queue-lock ((wait-queue) &body body)
  (let ((sym (gensym "WAIT-QUEUE")))
    `(let ((,sym ,wait-queue))
       (unwind-protect
            (progn
              (lock-wait-queue ,sym)
              ,@body)
         (unlock-wait-queue ,sym)))))

(sys.int::defglobal *lock-violations-are-fatal* t)

(defstruct (mutex
             (:include wait-queue)
             (:constructor make-mutex (&optional name))
             (:area :wired)
             :slot-offsets)
  ;; Thread holding the lock, or NIL if it is free.
  ;; May not be correct when the lock is being acquired/released.
  (owner nil)
  ;; Lock state.
  ;; :unlocked - No thread is holding the lock.
  ;; :locked - A thread is holding the lock and no other threads have
  ;;           attempted to acquire it.
  ;; :contested - The lock is held, and there are threads attempting to
  ;;              acquire it. This causes release to wake sleeping threads.
  (state :unlocked)
  (stack-next nil)
  ;; Number of times ACQUIRE-MUTEX failed to immediately acquire the lock.
  (contested-count 0))

(defun acquire-mutex (mutex &optional (wait-p t))
  (check-type mutex mutex)
  (let ((self (current-thread)))
    ;; Fast path - try to lock.
    (when (eql (sys.int::cas (mutex-state mutex) :unlocked :locked) :unlocked)
      ;; We got it.
      (setf (mutex-owner mutex) self)
      (return-from acquire-mutex t))
    ;; Idiot check.
    (unless (not (mutex-held-p mutex))
      (if *lock-violations-are-fatal*
          (panic "Recursive locking detected on " mutex " " (mutex-name mutex))
          (error 'sys.int::mutex-error
                 :mutex mutex
                 :format-control "Recursive locking detected on ~S ~S"
                 :format-arguments (list mutex (mutex-name mutex)))))
    ;; Increment MUTEX-CONTESTED-COUNT
    (sys.int::%atomic-fixnum-add-object mutex +mutex-contested-count+ 1)
    (when wait-p
      (ensure-interrupts-enabled)
      (unless (not *pseudo-atomic*)
        (panic "Trying to acquire mutex " mutex " while pseudo-atomic."))
      (thread-pool-blocking-hijack acquire-mutex mutex wait-p)
      (%call-on-wired-stack-without-interrupts
       #'acquire-mutex-slow-path nil mutex self)
      t)))

(defun acquire-mutex-slow-path (sp fp mutex self)
  ;; Slow path.
  ;; Now try to sleep on the lock.
  (lock-wait-queue mutex)
  ;; Put the lock into the contested state.
  ;; Try to acquire again, release may have been running.
  (when (eql (sys.int::%xchg-object mutex +mutex-state+ :contested) :unlocked)
    ;; We got it.
    (setf (mutex-owner mutex) self)
    (unlock-wait-queue mutex)
    (return-from acquire-mutex-slow-path))
  ;; Add to wait queue. Release will directly transfer ownership
  ;; to this thread.
  (push-wait-queue self mutex)
  ;; Now sleep.
  ;; Must take the thread lock before dropping the mutex lock or release
  ;; may be able to remove the thread from the sleep queue before it goes
  ;; to sleep.
  (acquire-global-thread-lock)
  (unlock-wait-queue mutex)
  (setf (thread-wait-item self) mutex
        (thread-state self) :sleeping
        (thread-unsleep-helper self) #'acquire-mutex
        (thread-unsleep-helper-argument self) mutex)
  (%reschedule-via-wired-stack sp fp))

(defun mutex-held-p (mutex)
  "Return true if this thread holds MUTEX."
  (eql (mutex-owner mutex) (current-thread)))

(defun check-mutex-release-consistence (mutex)
  (let ((current-owner (mutex-owner mutex)))
    (cond ((not current-owner)
           (if *lock-violations-are-fatal*
               (panic "Trying to release unheld mutex " mutex)
               (error 'sys.int::mutex-error
                      :mutex mutex
                      :format-control "Trying to release unheld mutex ~S ~S"
                      :format-arguments (list mutex (mutex-name mutex)))))
          ((not (eql current-owner (current-thread)))
           (if *lock-violations-are-fatal*
               (panic "Trying to release mutex " mutex " held by other thread " current-owner)
               (error 'sys.int::mutex-error
                      :mutex mutex
                      :format-control "Trying to release mutex ~S ~S held by other thread ~S"
                      :format-arguments (list mutex (mutex-name mutex) current-owner)))))))

(defun release-mutex (mutex)
  (check-type mutex mutex)
  (check-mutex-release-consistence mutex)
  (setf (mutex-owner mutex) nil)
  (when (not (eql (sys.int::cas (mutex-state mutex) :locked :unlocked) :locked))
    ;; Mutex must be in the contested state.
    (release-mutex-slow-path mutex))
  (values))

(defun release-mutex-slow-path (mutex)
  ;; Contested lock. Need to wake a thread and pass the lock to it.
  (safe-without-interrupts (mutex)
    (with-wait-queue-lock (mutex)
      ;; Look for a thread to wake.
      (let ((thread (pop-wait-queue mutex)))
        (cond (thread
               ;; Found one, wake it & transfer the lock.
               (setf (mutex-owner mutex) thread)
               (wake-thread thread))
              (t
               ;; No threads sleeping, just drop the lock.
               ;; Any threads trying to lock will be spinning on the wait queue lock.
               (setf (mutex-state mutex) :unlocked)))))))

(defun release-mutex-for-condition-variable (mutex)
  (setf (mutex-owner mutex) nil)
  (when (not (eql (sys.int::cas (mutex-state mutex) :locked :unlocked) :locked))
    ;; Mutex must be in the contested state.
    ;; Look for a thread to wake.
    (let ((thread (pop-wait-queue mutex)))
      (cond (thread
             ;; Found one, wake it & transfer the lock.
             (setf (mutex-owner mutex) thread)
             (wake-thread-1 thread))
            (t
             ;; No threads sleeping, just drop the lock.
             ;; Any threads trying to lock will be spinning on the wait queue lock.
             (setf (mutex-state mutex) :unlocked))))))

(defun call-with-mutex (thunk mutex wait-p)
  (unwind-protect
       (when (acquire-mutex mutex wait-p)
         (funcall thunk))
    (when (mutex-held-p mutex)
      (release-mutex mutex))))

(defmacro with-mutex ((mutex &key (wait-p t) resignal-errors) &body body)
  "Run body with MUTEX locked.
May be used from an interrupt handler when TIMEOUT is 0.
If RESIGNAL-ERRORS is non-NIL, any conditions signalled inside BODY
that match the type specified by RESIGNAL-ERRORS (as by HANDLER-CASE/-BIND)
will be resignalled (via ERROR) with the lock released.
If RESIGNAL-ERRORS is T, then it will be treated as though it were ERROR."
  (let ((call-with-mutex-thunk (gensym "CALL-WITH-MUTEX-THUNK")))
    (flet ((emit-body ()
             `(flet ((,call-with-mutex-thunk () ,@body))
                (declare (dynamic-extent #',call-with-mutex-thunk))
                (call-with-mutex #',call-with-mutex-thunk
                                 ,mutex
                                 ,wait-p))))
      (cond (resignal-errors
             `(handler-case ,(emit-body)
                (,(if (eql resignal-errors 't)
                      'error
                      resignal-errors)
                    (condition)
                  (error condition))))
            (t
             (emit-body))))))

(defstruct (condition-variable
             (:include wait-queue)
             (:constructor make-condition-variable (&optional name))
             (:area :wired)))

(defun condition-wait (condition-variable mutex &optional timeout)
  (check-type condition-variable condition-variable)
  (check-type mutex mutex)
  (assert (mutex-held-p mutex))
  (check-mutex-release-consistence mutex)
  (ensure-interrupts-enabled)
  (thread-pool-blocking-hijack condition-wait condition-variable mutex timeout)
  (unwind-protect
       (cond (timeout
              (with-timer (timer :relative timeout)
                (unwind-protect
                     (progn
                       (setf (timer-cvar timer) condition-variable)
                       (%call-on-wired-stack-without-interrupts
                        #'condition-wait-inner nil condition-variable mutex timer)
                       ;; This gets a little fuzzy with timeouts vs a legit wake...
                       (not (timer-expired-p timer)))
                  ;; Make sure to clear the timer's cvar slot before returning it.
                  (setf (timer-cvar timer) nil))))
             (t
              (%call-on-wired-stack-without-interrupts
               #'condition-wait-inner nil condition-variable mutex nil)
              t))
    ;; Got woken up. Reacquire the mutex.
    ;; Slightly tricky, if the thread was interrupted and unwound before
    ;; interrupts were disabled, then the mutex won't have been released.
    (when (not (mutex-held-p mutex))
      (acquire-mutex mutex t))))

(defun condition-wait-inner (sp fp condition-variable mutex timer)
  (let ((self (current-thread)))
    (lock-wait-queue condition-variable)
    (lock-wait-queue mutex)
    (acquire-global-thread-lock)
    ;; The timer may have expired and signalled the cvar before the locks were
    ;; taken, check before going to sleep.
    (when (and timer (timer-expired-p timer))
      (unlock-wait-queue condition-variable)
      (unlock-wait-queue mutex)
      (release-global-thread-lock)
      (return-from condition-wait-inner))
    ;; Attach to the list.
    (push-wait-queue self condition-variable)
    ;; Drop the mutex.
    (release-mutex-for-condition-variable mutex)
    ;; Sleep.
    ;; need to be careful with that, returning or unwinding from condition-wait
    ;; with the lock unlocked would be quite bad.
    (setf (thread-wait-item self) condition-variable
          (thread-state self) :sleeping
          ;; Relock the mutex and return if unsleeped.
          ;; Condition-wait can return spuriously.
          (thread-unsleep-helper self) #'acquire-mutex
          (thread-unsleep-helper-argument self) mutex)
    (unlock-wait-queue mutex)
    (unlock-wait-queue condition-variable)
    (%reschedule-via-wired-stack sp fp)))

(defun condition-notify (condition-variable &optional broadcast)
  "Wake one or many threads waiting on CONDITION-VARIABLE.
May be used from an interrupt handler, assuming the associated mutex is interrupt-safe."
  (check-type condition-variable condition-variable)
  (safe-without-interrupts (condition-variable broadcast)
    (flet ((pop-one ()
             (wake-thread (pop-wait-queue condition-variable))))
      (declare (dynamic-extent #'pop-one))
      (with-wait-queue-lock (condition-variable)
        (cond (broadcast
               ;; Loop until all the threads have been woken.
               (do ()
                   ((null (condition-variable-head condition-variable)))
                 (pop-one)))
              (t
               ;; Wake exactly one.
               (when (condition-variable-head condition-variable)
                 (pop-one)))))))
  (values))

(defstruct (irq-fifo
             (:area :wired)
             (:constructor %make-irq-fifo))
  (name nil)
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (size)
  (element-type)
  (buffer (error "no buffer supplied") :read-only t)
  (count)
  data-available
  (lock (place-spinlock-initializer)))

(defun make-irq-fifo (size &key (element-type 't) name)
  ;; TODO: non-t element types.
  (let ((fifo (%make-irq-fifo :size size
                              :buffer (sys.int::make-simple-vector size :wired)
                              :element-type 't
                              :count 0
                              :name name)))
    (setf (irq-fifo-data-available fifo)
          (make-event :name (sys.int::cons-in-area
                             'irq-fifo-data-available-event
                             (sys.int::cons-in-area
                              fifo
                              nil
                              :wired)
                             :wired)))
    fifo))

(defun irq-fifo-push (value fifo)
  "Push a byte onto FIFO. Returns true if there was space and value was pushed successfully.
If the fifo is full, then FIFO-PUSH will return false.
Safe to use from an interrupt handler."
  (check-type fifo irq-fifo)
  (safe-without-interrupts (value fifo)
    (with-place-spinlock ((irq-fifo-lock fifo))
      (let ((next (1+ (irq-fifo-tail fifo))))
        (when (>= next (irq-fifo-size fifo))
          (setf next 0))
        ;; When next reaches head, the buffer is full.
        (unless (= next (irq-fifo-head fifo))
          (setf (svref (irq-fifo-buffer fifo) (irq-fifo-tail fifo)) value
                (irq-fifo-tail fifo) next)
          (incf (irq-fifo-count fifo))
          (setf (event-state (irq-fifo-data-available fifo)) t)
          t)))))

(defun irq-fifo-pop (fifo &optional (wait-p t))
  "Pop a byte from FIFO.
Returns two values. The first value is the value popped from the FIFO.
The second value is true if a value was popped, false otherwise.
It is only possible for the second value to be false when wait-p is false."
  (check-type fifo irq-fifo)
  (loop
       (multiple-value-bind (value validp)
           (safe-without-interrupts (fifo)
             (with-place-spinlock ((irq-fifo-lock fifo))
               (cond ((zerop (irq-fifo-count fifo))
                      (values nil nil))
                     (t
                      ;; Pop byte.
                      (let ((value (svref (irq-fifo-buffer fifo) (irq-fifo-head fifo)))
                            (next (1+ (irq-fifo-head fifo))))
                        (when (>= next (irq-fifo-size fifo))
                          (setf next 0))
                        (setf (irq-fifo-head fifo) next)
                        (decf (irq-fifo-count fifo))
                        (when (zerop (irq-fifo-count fifo))
                          (setf (event-state (irq-fifo-data-available fifo)) nil))
                        (values value t))))))
         (when validp
           (return (values value t))))
     (when (not wait-p)
       (return (values nil nil)))
     (event-wait (irq-fifo-data-available fifo))))

(defun irq-fifo-reset (fifo)
  "Flush any waiting data."
  (check-type fifo irq-fifo)
  (safe-without-interrupts (fifo)
    (with-place-spinlock ((irq-fifo-lock fifo))
      (setf (irq-fifo-head fifo) 0
            (irq-fifo-tail fifo) 0
            (irq-fifo-count fifo) 0)
      (setf (event-state (irq-fifo-data-available fifo)) nil))))

;;;; WAIT-FOR-OBJECTS and the EVENT primitive.

(defstruct (wfo
             (:constructor %make-wfo)
             (:include wait-queue)
             (:area :wired))
  objects
  events
  links)

(sys.int::defglobal *big-wait-for-objects-lock*)

;; Object pools are used for WFO structures and the wired cons cells
;; used to form the events & links lists. This is to reduce allocation
;; in the wired area which is very slow, and GCing the wired area requires
;; a full GC cycle.
(sys.int::defglobal *wfo-pool-lock* (make-mutex "WFO pool"))
(sys.int::defglobal *wfo-pool-hit-count* 0)
(sys.int::defglobal *wfo-pool-miss-count* 0)

(sys.int::defglobal *wfo-cons-pool* '())
(sys.int::defglobal *wfo-cons-pool-size* 0)
(sys.int::defglobal *wfo-cons-pool-limit* 1000)

(defun wfo-cons (car cdr)
  ;; Structured so that CONS-IN-AREA isn't called inside the pool lock.
  (flet ((pop-pool ()
           (with-mutex (*wfo-pool-lock*)
             (let ((cons *wfo-cons-pool*))
               (cond (cons
                      (setf *wfo-cons-pool* (cdr cons))
                      (decf *wfo-cons-pool-size*)
                      (setf (car cons) car
                            (cdr cons) cdr)
                      (incf *wfo-pool-hit-count*)
                      cons)
                     (t
                      (incf *wfo-pool-miss-count*)
                      nil))))))
    (or (pop-pool)
        (sys.int::cons-in-area car cdr :wired))))

(defun wfo-uncons (cons)
  (with-mutex (*wfo-pool-lock*)
    (when (< *wfo-cons-pool-size* *wfo-cons-pool-limit*)
      (incf *wfo-cons-pool-size*)
      (setf (car cons) nil ; don't leak objects!
            (cdr cons) *wfo-cons-pool*)
      (setf *wfo-cons-pool* cons))))

(defun wfo-uncons-list (list)
  "Like WFO-UNCONS, but unconses the entire spine of LIST."
  (with-mutex (*wfo-pool-lock*)
    (loop
       (when (or (null list)
                 (>= *wfo-cons-pool-size* *wfo-cons-pool-limit*))
         (return))
       (let ((cons list))
         (setf list (cdr cons))
         (incf *wfo-cons-pool-size*)
         (setf (car cons) nil ; don't leak objects!
               (cdr cons) *wfo-cons-pool*)
         (setf *wfo-cons-pool* cons)))))

(sys.int::defglobal *wfo-pool* '())
(sys.int::defglobal *wfo-pool-size* 0)
(sys.int::defglobal *wfo-pool-limit* 1000)

(defun make-wfo (&key objects events)
  ;; Structured so that %MAKE-WFO isn't called inside the pool lock.
  (flet ((pop-pool ()
           (with-mutex (*wfo-pool-lock*)
             (let ((wfo *wfo-pool*))
               (cond (wfo
                      (setf *wfo-pool* (wfo-links wfo))
                      (decf *wfo-pool-size*)
                      (setf (wfo-objects wfo) objects
                            (wfo-events wfo) events
                            (wfo-links wfo) nil)
                      (incf *wfo-pool-hit-count*)
                      wfo)
                     (t
                      (incf *wfo-pool-miss-count*)
                      nil))))))
    (or (pop-pool)
        (%make-wfo :objects objects :events events))))

(defun unmake-wfo (wfo)
  (with-mutex (*wfo-pool-lock*)
    (when (< *wfo-pool-size* *wfo-pool-limit*)
      (incf *wfo-pool-size*)
      (setf (wfo-objects wfo) nil ; don't leak objects!
            (wfo-events wfo) nil
            (wfo-links wfo) *wfo-pool*)
      (setf *wfo-pool* wfo))))

(defun wfo-wait-1 (sp fp wfo)
  (acquire-place-spinlock *big-wait-for-objects-lock*)
  ;; Scan for completed events before registration.
  (let ((completed nil))
    (dolist (event (wfo-events wfo))
      (let ((state (event-%state event)))
        (when state
          ;; This one is completed.
          (let ((link (wfo-links wfo)))
            (setf (wfo-links wfo) (cdr link))
            (setf (car link) event
                  (cdr link) completed
                  completed link)))))
    (when completed
      ;; There were some completed events.
      (release-place-spinlock *big-wait-for-objects-lock*)
      (return-from wfo-wait-1 completed)))
  ;; Register our wait queue with each event.
  (dolist (event (wfo-events wfo))
    (let ((link (wfo-links wfo)))
      (setf (wfo-links wfo) (cdr link))
      (setf (cdr link) (event-waiters event)
            (event-waiters event) link)))
  ;; Set LINKS to :SLEEP-IN-PROGRESS to indicate that events were registered
  ;; with and WFO-UNREGISTER must be run.
  ;; See the big comment in WFO-WAIT.
  (setf (wfo-links wfo) :sleep-in-progress)
  ;; Now go to sleep, zzz.
  (let ((self (current-thread)))
    (lock-wait-queue wfo)
    (release-place-spinlock *big-wait-for-objects-lock*)
    (acquire-global-thread-lock)
    ;; Attach to the list.
    (push-wait-queue self wfo)
    ;; Sleep.
    (setf (thread-wait-item self) wfo
          (thread-state self) :sleeping
          (thread-unsleep-helper self) #'wfo-unsleep-helper
          (thread-unsleep-helper-argument self) wfo)
    (unlock-wait-queue wfo)
    (%reschedule-via-wired-stack sp fp)))

(defun wfo-unsleep-helper (wfo)
  (declare (ignore wfo))
  nil)

(defun wfo-unregister (wfo)
  (safe-without-interrupts (wfo)
    (with-place-spinlock (*big-wait-for-objects-lock*)
      ;; Unregister from events and reclaim the links.
      ;; This resets the WFO object and prepares it for re-waiting.
      (flet ((unregister-from-event (event marker)
               (do ((itr (event-waiters event) (cdr itr))
                    (prev nil itr))
                   ((null itr)
                    (panic "WFO link not registered?"))
                 (when (eql (car itr) marker)
                   (cond (prev
                          (setf (cdr prev) (cdr itr)))
                         (t
                          (setf (event-waiters event) (cdr itr))))
                   (return itr)))))
        (setf (wfo-links wfo) '())
        (dolist (event (wfo-events wfo))
          (let ((link (unregister-from-event event wfo)))
            (setf (cdr link) (wfo-links wfo)
                  (wfo-links wfo) link)))))))

(defun wfo-wait (wfo)
  ;; Loop required because the thread may be woken up and
  ;; not have any ready events.
  (loop
     (unwind-protect
          (let ((completed (%call-on-wired-stack-without-interrupts
                            #'wfo-wait-1 nil
                            wfo)))
            (when completed
              (return completed)))
       ;; There are 5 ways to end up here:
       ;; 1) Normal return from WFO-WAIT-1 with completed events.
       ;; 2) Normal return without any completed events.
       ;; 3) Interrupted sleep, via normal return from the wfo unsleep helper.
       ;; 4) Interrupted sleep, via unwind.
       ;; 5) Unwinding after establishing the cleanup handler but before
       ;;    turning interrupts off.
       ;; Cases 2,3,4 are all effectively identical: WFO-WAIT-1 registered
       ;; the WFO with events and went to sleep. The WFO must be unregistered
       ;; from the events.
       ;; Cases 1 and 5 are similar: WFO-WAIT-1 did not register with any
       ;; events and WFO-UNREGISTER must *not* be called.
       ;; UNWIND-PROTECT catches case 4, cases 2 and 3 could be done in
       ;; the loop without an unwind protect. UNWIND-PROTECT screws over
       ;; cases 1 & 5, so extra tests are required to avoid this.
       ;; To resolve this, WFO-LINKS is set to :SLEEP-IN-PROGRESS after
       ;; WFO-WAIT-1 registers with events and actually goes to sleep.
       (when (eql (wfo-links wfo) :sleep-in-progress)
         (wfo-unregister wfo)))))

(defun convert-objects-to-events (objects)
  ;; The event list must be wired, so do this instead of a simple mapcar.
  (let* ((head (cons nil nil))
         (tail head))
    (declare (dynamic-extent head))
    (dolist (object objects)
      ;; Special case events to avoid the call
      ;; through G-O-E as it is defined much later.
      (let ((event (typecase object
                     (event object)
                     (timer (timer-event object))
                     (t (get-object-event object)))))
        (assert (event-p event))
        (setf (cdr tail) (wfo-cons event nil)
              tail (cdr tail))))
    (cdr head)))

(defun wait-for-objects (&rest objects)
  (thread-pool-blocking-hijack-apply wait-for-objects objects)
  ;; Objects converted to events.
  (let* ((events (convert-objects-to-events objects))
         (wfo (make-wfo :objects objects
                        :events events)))
    ;; Allocate links for each event.
    (dolist (evt events)
      (declare (ignore evt))
      (setf (wfo-links wfo) (wfo-cons wfo (wfo-links wfo))))
    ;; Wait & convert completed events back to objects
    (let* ((completed (wfo-wait wfo))
           (completed-objects (loop
                                 for evt in completed
                                 collect (loop
                                            for input-object in objects
                                            for input-event in events
                                            when (eql evt input-event)
                                            do (return input-object)
                                            finally (error "Impossible? Can't convert event back to object")))))
      ;; Make sure to release links back to the pool
      (wfo-uncons-list completed)
      (wfo-uncons-list events)
      (wfo-uncons-list (wfo-links wfo))
      (unmake-wfo wfo)
      completed-objects)))

(defstruct (event
             (:constructor %make-event (name %state))
             (:include wait-queue)
             (:area :wired))
  %state
  waiters)

(defun make-event (&key name state)
  "Create a new event with the specified initial state."
  (%make-event name state))

(defun event-state (event)
  "Return the current state of EVENT."
  (event-%state event))

(defun (setf event-state) (value event)
  "Set the state of EVENT.
STATE may be any object and will be treated as a generalized boolean by EVENT-WAIT and WAIT-FOR-OBJECTS."
  (check-type event event)
  (safe-without-interrupts (value event)
    (with-place-spinlock (*big-wait-for-objects-lock*)
      (when (and value
                 (not (event-%state event)))
        ;; Moving from the false state to the true state. Wake waiters.
        (dolist (waiter (event-waiters event))
          (with-wait-queue-lock (waiter)
            (do ()
                ((null (wait-queue-head waiter)))
              (wake-thread (pop-wait-queue waiter)))))
        (with-wait-queue-lock (event)
          (do ()
              ((null (wait-queue-head event)))
            (wake-thread (pop-wait-queue event)))))
      (setf (event-%state event) value))))

(defun event-wait (event)
  "Wait until EVENT's state is not NIL."
  (check-type event event)
  (thread-pool-blocking-hijack event-wait event)
  (%run-on-wired-stack-without-interrupts (sp fp event)
    (acquire-place-spinlock *big-wait-for-objects-lock*)
    (let ((self (current-thread)))
      (lock-wait-queue event)
      (cond ((event-%state event)
             ;; Event state is non-NIL, don't sleep.
             (unlock-wait-queue event)
             (release-place-spinlock *big-wait-for-objects-lock*))
            (t
             ;; Event state is NIL.
             (acquire-global-thread-lock)
             ;; Attach to the list.
             (push-wait-queue self event)
             ;; Sleep.
             (setf (thread-wait-item self) event
                   (thread-state self) :sleeping
                   (thread-unsleep-helper self) #'event-wait
                   (thread-unsleep-helper-argument self) event)
             (unlock-wait-queue event)
             (release-place-spinlock *big-wait-for-objects-lock*)
             (%reschedule-via-wired-stack sp fp))))))
