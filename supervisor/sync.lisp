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

(defun push-wait-queue (thread wait-queue)
  (cond ((null (wait-queue-head wait-queue))
         (setf (wait-queue-head wait-queue) thread
               (wait-queue-tail wait-queue) thread)
         (setf (thread-%next thread) nil
               (thread-%prev thread) nil))
        (t
         (setf (thread-%next (wait-queue-tail wait-queue)) thread
               (thread-%prev thread) (wait-queue-tail wait-queue)
               (thread-%next thread) nil
               (wait-queue-tail wait-queue) thread))))

(defun pop-wait-queue (wait-queue)
  (let ((thread (wait-queue-head wait-queue)))
    (when thread
      (cond ((thread-%next thread)
             (setf (thread-%prev (thread-%next thread)) nil)
             (setf (wait-queue-head wait-queue) (thread-%next thread)))
            (t (setf (wait-queue-head wait-queue) nil
                     (wait-queue-tail wait-queue) nil)))
      thread)))

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

(sys.int::defglobal *lock-violations-are-fatal* nil)

(defstruct (mutex
             (:include wait-queue)
             (:constructor make-mutex (&optional name))
             (:area :wired))
  ;; Thread holding the lock, or NIL if it is free.
  ;; May not be correct when the lock is being acquired/released.
  (owner nil)
  ;; Lock state.
  ;; :unlocked - No thread is holding the lock.
  ;; :locked - A thread is holding the lock and no other threads have
  ;;           attempted to acquire it.
  ;; :contested - The lock is held, and there are threads attempting to
  ;;              acquire it. This causes release to wake sleeping threads.
  ;; Must be index 6. CONS grovels directly in the lock.
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
    (sys.int::%atomic-fixnum-add-object mutex 8 1)
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
  (when (eql (sys.int::%xchg-object mutex 6 :contested) :unlocked)
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
  (%lock-thread self)
  (unlock-wait-queue mutex)
  (setf (thread-wait-item self) mutex
        (thread-state self) :sleeping)
  (%reschedule-via-wired-stack sp fp))

(defun mutex-held-p (mutex)
  "Return true if this thread holds MUTEX."
  (eql (mutex-owner mutex) (current-thread)))

(defun release-mutex (mutex)
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
                      :format-arguments (list mutex (mutex-name mutex) current-owner))))))
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
  (ensure-interrupts-enabled)
  (unwind-protect
       (%run-on-wired-stack-without-interrupts (sp fp condition-variable mutex)
        (let ((self (current-thread)))
          (lock-wait-queue condition-variable)
          (%lock-thread self)
          ;; Attach to the list.
          (push-wait-queue self condition-variable)
          ;; Drop the mutex.
          (release-mutex mutex)
          ;; Sleep.
          ;; need to be careful with that, returning or unwinding from condition-wait
          ;; with the lock unlocked would be quite bad.
          (setf (thread-wait-item self) condition-variable
                (thread-state self) :sleeping)
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
                 (%lock-thread self)
                 (unlock-wait-queue semaphore)
                 (setf (thread-wait-item self) semaphore
                       (thread-state self) :sleeping)
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
            (%lock-thread self)
            ;; Attach to the list.
            (push-wait-queue self latch)
            ;; Sleep.
            (setf (thread-wait-item self) latch
                  (thread-state self) :sleeping)
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
