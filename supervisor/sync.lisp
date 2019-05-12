;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Syncronization primitives.

(in-package :mezzano.supervisor)

;;; Common structure for sleepable things.
(defstruct (wait-queue
             (:area :wired))
  name
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

(defmacro with-mutex ((mutex &optional (wait-p t)) &body body)
  "Run body with MUTEX locked.
May be used from an interrupt handler when WAIT-P is false or if MUTEX is a spin mutex."
  ;; Cold generator has some odd problems with uninterned symbols...
  `(flet ((call-with-mutex-thunk () ,@body))
     (declare (dynamic-extent #'call-with-mutex-thunk))
     (call-with-mutex #'call-with-mutex-thunk
                      ,mutex
                      ,wait-p)))

(defstruct (condition-variable
             (:include wait-queue)
             (:constructor make-condition-variable (&optional name))
             (:area :wired)))

(defun condition-wait (condition-variable mutex)
  (assert (mutex-held-p mutex))
  (check-mutex-release-consistence mutex)
  (ensure-interrupts-enabled)
  (unwind-protect
       (%run-on-wired-stack-without-interrupts (sp fp condition-variable mutex)
        (let ((self (current-thread)))
          (lock-wait-queue condition-variable)
          (lock-wait-queue mutex)
          (acquire-global-thread-lock)
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
    ;; Got woken up. Reacquire the mutex.
    ;; Slightly tricky, if the thread was interrupted and unwound before
    ;; interrupts were disabled, then the mutex won't have been released.
    (when (not (mutex-held-p mutex))
      (acquire-mutex mutex t)))
  (values))

(defun condition-notify (condition-variable &optional broadcast)
  "Wake one or many threads waiting on CONDITION-VARIABLE.
May be used from an interrupt handler, assuming the associated mutex is interrupt-safe."
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

(defstruct (semaphore
             (:include wait-queue)
             (:constructor make-semaphore (value &optional name))
             (:area :wired))
  (value 0 :type (integer 0)))

(defun semaphore-up (semaphore)
  "Increment the semaphore count, or wake a waiting thread.
May be used from an interrupt handler."
  (with-wait-queue-lock (semaphore)
    ;; If there is a thread, wake it instead of incrementing.
    (let ((thread (pop-wait-queue semaphore)))
      (cond (thread
             ;; Found one, wake it.
             (wake-thread thread))
            (t
             ;; No threads sleeping, increment.
             (incf (semaphore-value semaphore)))))))

(defun semaphore-down (semaphore &optional (wait-p t))
  (ensure-interrupts-enabled)
  ;; Invert the result here because %RESCHEDULE-VIA-WIRED-STACK will always
  ;; cause %R-O-W-S-W-I to return NIL, which is actually a success result.
  (not (%run-on-wired-stack-without-interrupts (sp fp semaphore wait-p)
        (let ((self (current-thread)))
          (lock-wait-queue semaphore)
          (cond ((not (zerop (semaphore-value semaphore)))
                 (decf (semaphore-value semaphore))
                 (unlock-wait-queue semaphore)
                 ;; Success (inverted).
                 nil)
                (wait-p
                 ;; Go to sleep.
                 (push-wait-queue self semaphore)
                 ;; Now sleep.
                 ;; Must take the thread lock before dropping the semaphore lock or up
                 ;; may be able to remove the thread from the sleep queue before it goes
                 ;; to sleep.
                 (acquire-global-thread-lock)
                 (unlock-wait-queue semaphore)
                 (setf (thread-wait-item self) semaphore
                       (thread-state self) :sleeping
                       (thread-unsleep-helper self) #'semaphore-down
                       (thread-unsleep-helper-argument self) semaphore)
                 (%reschedule-via-wired-stack sp fp))
                (t (unlock-wait-queue semaphore)
                   ;; Failure (inverted).
                   t))))))

(defstruct (latch
             (:include wait-queue)
             (:constructor make-latch (&optional name))
             (:area :wired))
  (state nil))

(defun latch-reset (latch)
  (safe-without-interrupts (latch)
    (with-wait-queue-lock (latch)
      (setf (latch-state latch) nil))))

(defun latch-wait (latch)
  (when (latch-state latch)
    (return-from latch-wait))
  (ensure-interrupts-enabled)
  (%run-on-wired-stack-without-interrupts (sp fp latch)
   (let ((self (current-thread)))
     (lock-wait-queue latch)
     (cond ((latch-state latch)
            ;; Latch was opened after the wait-queue was locked.
            ;; Don't sleep.
            (unlock-wait-queue latch))
           (t ;; Latch is closed, sleep.
            (acquire-global-thread-lock)
            ;; Attach to the list.
            (push-wait-queue self latch)
            ;; Sleep.
            (setf (thread-wait-item self) latch
                  (thread-state self) :sleeping
                  (thread-unsleep-helper self) #'latch-wait
                  (thread-unsleep-helper-argument self) latch)
            (unlock-wait-queue latch)
            (%reschedule-via-wired-stack sp fp)))))
  (values))

(defun latch-trigger (latch)
  (safe-without-interrupts (latch)
    (with-wait-queue-lock (latch)
      (setf (latch-state latch) t)
      ;; Loop until all the threads have been woken.
      (do ()
          ((null (wait-queue-head latch)))
        (wake-thread (pop-wait-queue latch))))))

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
  (lock (place-spinlock-initializer)))

(defun make-irq-fifo (size &key (element-type 't) name)
  ;; TODO: non-t element types.
  (%make-irq-fifo :size size
                  :buffer (sys.int::make-simple-vector size :wired)
                  :element-type 't
                  :count (make-semaphore 0 name)
                  :name name))

(defun irq-fifo-push (value fifo)
  "Push a byte onto FIFO. Returns true if there was space adn value was pushed successfully.
If the fifo is full, then FIFO-PUSH will return false.
Safe to use from an interrupt handler."
  (safe-without-interrupts (value fifo)
    (with-place-spinlock ((irq-fifo-lock fifo))
      (let ((next (1+ (irq-fifo-tail fifo))))
        (when (>= next (irq-fifo-size fifo))
          (setf next 0))
        ;; When next reaches head, the buffer is full.
        (unless (= next (irq-fifo-head fifo))
          (setf (svref (irq-fifo-buffer fifo) (irq-fifo-tail fifo)) value
                (irq-fifo-tail fifo) next)
          (semaphore-up (irq-fifo-count fifo))
          t)))))

(defun irq-fifo-pop (fifo &optional (wait-p t))
  "Pop a byte from FIFO.
Returns two values. The first value is the value popped from the FIFO.
The second value is true if a value was popped, false otherwise.
It is only possible for the second value to be false when wait-p is false."
  (when (not (semaphore-down (irq-fifo-count fifo) wait-p))
    (return-from irq-fifo-pop
      (values nil nil)))
  (safe-without-interrupts (fifo)
    (with-place-spinlock ((irq-fifo-lock fifo))
      ;; FIFO must not be empty.
      (ensure (not (eql (irq-fifo-head fifo) (irq-fifo-tail fifo))))
      ;; Pop byte.
      (let ((value (svref (irq-fifo-buffer fifo) (irq-fifo-head fifo)))
            (next (1+ (irq-fifo-head fifo))))
        (when (>= next (irq-fifo-size fifo))
          (setf next 0))
        (setf (irq-fifo-head fifo) next)
        (values value t)))))

(defun irq-fifo-reset (fifo)
  "Flush any waiting data."
  (loop
     (multiple-value-bind (value validp)
         (irq-fifo-pop fifo nil)
       (declare (ignore value))
       (when (not validp)
         (return)))))

(defstruct (fifo
             (:area :wired)
             (:constructor (make-fifo (size &key (element-type 't) &aux (buffer (make-array (list size) :element-type element-type))))))
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (size nil :read-only t)
  (element-type nil :read-only t)
  (buffer nil :read-only t)
  (cv (make-condition-variable))
  (lock (make-mutex "fifo-lock")))

(defun fifo-push (value fifo &optional (wait-p t))
  "Push a byte onto FIFO. Returns true if successful.
If the fifo is full, then FIFO-PUSH will wait for space to become available
when WAIT-P is true, otherwise it will immediately return false."
  (with-mutex ((fifo-lock fifo))
    (loop
       (let ((next (1+ (fifo-tail fifo))))
         (when (>= next (fifo-size fifo))
           (setf next 0))
         ;; When next reaches head, the buffer is full.
         (unless (= next (fifo-head fifo))
           (setf (aref (fifo-buffer fifo) (fifo-tail fifo)) value
                 (fifo-tail fifo) next)
           (condition-notify (fifo-cv fifo))
           (return t)))
       (unless wait-p
         (return nil))
       (condition-wait (fifo-cv fifo)
                       (fifo-lock fifo)))))

(defun fifo-pop (fifo &optional (wait-p t))
  "Pop a byte from FIFO.
Returns two values. The first value is the value popped from the FIFO.
The second value is true if a value was popped, false otherwise.
It is only possible for the second value to be false when wait-p is false."
  (with-mutex ((fifo-lock fifo))
    (loop
       (when (not (eql (fifo-head fifo) (fifo-tail fifo)))
         ;; Fifo not empty, pop byte.
         (let ((value (aref (fifo-buffer fifo) (fifo-head fifo)))
               (next (1+ (fifo-head fifo))))
           (when (>= next (fifo-size fifo))
             (setf next 0))
           (setf (fifo-head fifo) next)
           (condition-notify (fifo-cv fifo))
           (return (values value t))))
       ;; Fifo empty, maybe wait?
       (unless wait-p
         (return (values nil nil)))
       (condition-wait (fifo-cv fifo)
                       (fifo-lock fifo)))))

(defun fifo-reset (fifo)
  "Flush any waiting data."
  (with-mutex ((fifo-lock fifo))
    (setf (fifo-head fifo) 0
          (fifo-tail fifo) 0)
    ;; Signal the cvar to wake any waiting FIFO-PUSH calls.
    (condition-notify (fifo-cv fifo) t)))

;;;; WAIT-FOR-OBJECTS and the EVENT primitive.

(defstruct (wfo
             (:include wait-queue)
             (:area :wired))
  objects
  events
  links)

(sys.int::defglobal *big-wait-for-objects-lock*
    (place-spinlock-initializer))

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

(defun wait-for-objects (&rest objects)
  ;; Objects converted to events.
  (let* ((events (mapcar #'(lambda (object)
                           ;; Special case events to avoid the call
                           ;; through G-O-E as it is defined much later.
                           (typecase object
                             (event object)
                             (timer (timer-event object))
                             (t (get-object-event object))))
                         objects))
         (wfo (make-wfo :objects objects
                        :events events)))
    ;; Allocate links for each event.
    (dolist (evt events)
      (declare (ignore evt))
      (setf (wfo-links wfo) (sys.int::cons-in-area wfo (wfo-links wfo) :wired)))
    ;; Wait & convert completed events back to objects
    (loop
       for evt in (wfo-wait wfo)
       collect (loop
                  for input-object in objects
                  for input-event in events
                  when (eql evt input-event)
                  do (return input-object)
                  finally (error "Impossible? Can't convert event back to object")))))

(defstruct (event
             (:constructor %make-event (name %state))
             (:area :wired))
  (name nil :read-only t)
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
  (safe-without-interrupts (value event)
    (with-place-spinlock (*big-wait-for-objects-lock*)
      (when (and value
                 (not (event-%state event)))
        ;; Moving from the false state to the true state. Wake waiters.
        (dolist (waiter (event-waiters event))
          (with-wait-queue-lock (waiter)
            (do ()
                ((null (wait-queue-head waiter)))
              (wake-thread (pop-wait-queue waiter))))))
      (setf (event-%state event) value))))

(defun event-wait (event)
  "Wait until EVENT's state is not NIL and return the state."
  (loop
     (let ((current (event-state event)))
       (when current
         (return current)))
     (wait-for-objects event)))
