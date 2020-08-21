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

(defmacro inhibit-thread-pool-blocking-hijack (&body body)
  "Run body with the thread's thread-pool unset."
  (let ((self (gensym "SELF"))
        (pool (gensym "POOL")))
    `(let* ((,self (current-thread))
            (,pool (thread-thread-pool ,self)))
       (unwind-protect
            (progn
              (setf (thread-thread-pool ,self) nil)
              ,@body)
         (setf (thread-thread-pool ,self) ,pool)))))

;;; Common structure for sleepable things.
(defstruct (wait-queue
             (:area :wired))
  (name nil)
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
  (state :unlocked)
  (stack-next nil)
  ;; Number of times ACQUIRE-MUTEX failed to immediately acquire the lock.
  (contested-count 0 :type fixnum))

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
    (sys.int::atomic-incf (mutex-contested-count mutex))
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
  (when (eql (sys.int::atomic-swapf (mutex-state mutex) :contested) :unlocked)
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

(defmacro condition-wait-for ((condition-variable mutex &optional timeout) &body predicate)
  "Evaluate PREDICATE in a loop, waiting on CONDITION-VARIABLE until PREDICATE returns true.
Returns the first non-NIL result of PREDICATE, or NIL if a timeout occurs.
The second return value is the remaining timeout, or NIL if no timeout was specified.
A block named NIL is defined allowing RETURN to be used within the predicate.
Handles timeouts properly."
  (let ((timeout-sym (gensym "TIMEOUT"))
        (timer-sym (gensym "TIMER"))
        (cvar-sym (gensym "CVAR"))
        (mutex-sym (gensym "MUTEX"))
        (predicate-fn (gensym "PREDICATE"))
        (prediate-result-sym (gensym)))
    `(let ((,cvar-sym ,condition-variable)
           (,mutex-sym ,mutex)
           (,timeout-sym ,timeout))
       (block nil
         (flet ((,predicate-fn () (progn ,@predicate)))
           (cond ((eql ,timeout-sym 0)
                  (values (,predicate-fn) 0))
                 (,timeout-sym
                  (mezzano.supervisor:with-timer (,timer-sym :relative ,timeout-sym
                                                             :name ,cvar-sym)
                    (loop
                       (let ((,prediate-result-sym (,predicate-fn)))
                         (when ,prediate-result-sym
                           (return (values ,prediate-result-sym
                                           (timer-remaining ,timer-sym)))))
                       (when (mezzano.supervisor:timer-expired-p ,timer-sym)
                         (return (values nil 0)))
                       (condition-wait ,cvar-sym ,mutex-sym ,timer-sym))))
                 (t
                  (loop
                     (let ((,prediate-result-sym (,predicate-fn)))
                       (when ,prediate-result-sym
                         (return (values ,prediate-result-sym nil))))
                     (condition-wait ,cvar-sym ,mutex-sym)))))))))

(defun condition-wait (condition-variable mutex &optional timeout)
  "Wait for a notification on CONDITION-VARIBLE.
It is not defined if MUTEX is dropped & reacquired if TIMEOUT expires immediately.
False wakeups can occur and calling code must account for them.
Returns true if a normal or false wakeup occurs, false if a timeout occurs."
  (check-type condition-variable condition-variable)
  (check-type mutex mutex)
  (check-type timeout (or null timer real))
  (assert (mutex-held-p mutex))
  (check-mutex-release-consistence mutex)
  (ensure-interrupts-enabled)
  (thread-pool-blocking-hijack condition-wait condition-variable mutex timeout)
  (unwind-protect
       (cond ((timer-p timeout)
              (assert (not (timer-cvar timeout)))
              (unwind-protect
                   (progn
                     (setf (timer-cvar timeout) condition-variable)
                     (%call-on-wired-stack-without-interrupts
                      #'condition-wait-inner nil condition-variable mutex timeout)
                     ;; This gets a little fuzzy with timeouts vs a legit wake...
                     (not (timer-expired-p timeout)))
                ;; Make sure to clear the timer's cvar slot before returning it.
                (setf (timer-cvar timeout) nil)))
             (timeout
              (with-timer (timer :relative timeout :name condition-variable)
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

;;;; Reader/writer locks.

(defconstant +rw-lock-state-mode+ (byte 2 0))
(defconstant +rw-lock-mode-readers+ #b00) ; or unlocked if count=0
(defconstant +rw-lock-mode-read-locked-contested+ #b01)
(defconstant +rw-lock-mode-write-locked+ #b10)
(defconstant +rw-lock-mode-write-locked-contested+ #b11)
;; Once the state field enters the contested state it
;; can only be modified with the internal lock held
;; or by all readers except the last unlocking it.
(defconstant +rw-lock-mode-contested-bit+ 1)
(defconstant +rw-lock-mode-write-bit+ 2)

(defconstant +rw-lock-state-unlocked+ 0)
(defconstant +rw-lock-state-reader-increment+ 4)

(defstruct (rw-lock
             (:constructor %make-rw-lock (name))
             (:area :wired))
  name
  (state +rw-lock-state-unlocked+ :type fixnum)
  (lock (place-spinlock-initializer))
  writer-wait-queue
  reader-wait-queue
  (n-pending-readers 0 :type fixnum)
  (read-contested-count 0 :type fixnum)
  (write-contested-count 0 :type fixnum)
  write-owner)

(defun make-rw-lock (&optional name)
  (let ((rw-lock (%make-rw-lock name)))
    (setf (rw-lock-writer-wait-queue rw-lock) (make-wait-queue
                                               :name (sys.int::cons-in-area
                                                      :writer
                                                      (sys.int::cons-in-area
                                                       rw-lock nil :wired)
                                                      :wired))
          (rw-lock-reader-wait-queue rw-lock) (make-wait-queue
                                               :name (sys.int::cons-in-area
                                                      :reader
                                                      (sys.int::cons-in-area
                                                       rw-lock
                                                       nil
                                                       :wired)
                                                      :wired)))
    rw-lock))

(defun rw-lock-read-acquire-slow (sp fp rw-lock)
  ;; Slow path for acquiring a RW lock.
  ;; Returns true if the lock was *not* acquired, false if it was.
  (acquire-place-spinlock (rw-lock-lock rw-lock))
  (lock-wait-queue (rw-lock-reader-wait-queue rw-lock))
  ;; Move to the appropriate contested state.
  (let* ((self (current-thread))
         (current (rw-lock-state rw-lock)))
    (when (and (not (logtest current +rw-lock-mode-contested-bit+))
               (or (not (logtest +rw-lock-mode-write-bit+ current))
                   (not (eql (sys.int::cas (rw-lock-state rw-lock)
                                           current
                                           (logior +rw-lock-mode-contested-bit+ current))
                             current))))
        ;; Failed to contest the lock or the lock is uncontested-read. Start over.
        (unlock-wait-queue (rw-lock-reader-wait-queue rw-lock))
        (release-place-spinlock (rw-lock-lock rw-lock))
        (return-from rw-lock-read-acquire-slow t))
    ;; Successfully contested the lock (or it was already contested).
    (sys.int::atomic-incf (rw-lock-read-contested-count rw-lock))
    ;; Now we can safely sleep on the reader wait queue.
    ;; The unlock paths will directly transfer ownership to this thread.
    (push-wait-queue self (rw-lock-reader-wait-queue rw-lock))
    ;; Sadly this isn't an accurate count due to interrupts.
    ;; Nothing will ever decrement the count if this thread has to
    ;; restart the acquire call.
    ;; RELEASE-SLOW accounts for this.
    (incf (rw-lock-n-pending-readers rw-lock))
    ;; Now sleep.
    ;; Must take the thread lock before dropping the mutex lock or release
    ;; may be able to remove the thread from the sleep queue before it goes
    ;; to sleep.
    (acquire-global-thread-lock)
    (unlock-wait-queue (rw-lock-reader-wait-queue rw-lock))
    (release-place-spinlock (rw-lock-lock rw-lock))
    (setf (thread-wait-item self) (rw-lock-reader-wait-queue rw-lock)
          (thread-state self) :sleeping
          (thread-unsleep-helper self) #'rw-lock-read-acquire
          (thread-unsleep-helper-argument self) rw-lock)
    ;; Returns NIL (don't retry, lock successful)
    (%reschedule-via-wired-stack sp fp)))

(defun rw-lock-read-acquire (rw-lock &optional (wait-p t))
  (cond (wait-p
         (loop
            ;; Examine the state variable.
            (let ((state (rw-lock-state rw-lock)))
              ;; Try to acquire as an uncontested reader.
              (when (and (not (logtest (logior +rw-lock-mode-write-bit+ +rw-lock-mode-contested-bit+) state))
                         (eql (sys.int::cas (rw-lock-state rw-lock) state (+ state +rw-lock-state-reader-increment+)) state))
                ;; Easy!
                (return)))
            (thread-pool-blocking-hijack rw-lock-read-acquire rw-lock wait-p)
            ;; Going to block.
            (when (not (%call-on-wired-stack-without-interrupts
                        #'rw-lock-read-acquire-slow nil rw-lock))
              ;; Retry not required, lock is now read-locked.
              (return)))
         t)
        (t
         (loop
            (let ((state (rw-lock-state rw-lock)))
              (when (logtest (logior +rw-lock-mode-write-bit+ +rw-lock-mode-contested-bit+) state)
                ;; Lock is contested or write-locked. Fail.
                (sys.int::atomic-incf (rw-lock-read-contested-count rw-lock))
                (return nil))
              ;; Try to acquire as an uncontested reader.
              ;; Retry if the lock state changed under us.
              (when (eql (sys.int::cas (rw-lock-state rw-lock) state (+ state +rw-lock-state-reader-increment+)) state)
                ;; Easy!
                (return t)))))))

(defun rw-lock-release-slow (rw-lock)
  (safe-without-interrupts (rw-lock)
    (block nil
      (with-place-spinlock ((rw-lock-lock rw-lock))
        ;; Try to wake writers first.
        (with-wait-queue-lock ((rw-lock-writer-wait-queue rw-lock))
          (when (not (wait-queue-empty-p (rw-lock-writer-wait-queue rw-lock)))
            ;; Writer present, move to write-contested mode and hand
            ;; the lock over.
            (setf (rw-lock-state rw-lock) +rw-lock-mode-write-locked-contested+)
            ;; TODO: Move to write-locked uncontested if this is the only writer/reader.
            (wake-thread (pop-wait-queue (rw-lock-writer-wait-queue rw-lock)))
            ;; Woken one writer, stop here.
            (return nil)))
        ;; Look for readers now.
        (with-wait-queue-lock ((rw-lock-reader-wait-queue rw-lock))
          (cond ((wait-queue-empty-p (rw-lock-reader-wait-queue rw-lock))
                 ;; No readers? Everybody went home.
                 (setf (rw-lock-state rw-lock) +rw-lock-state-unlocked+))
                (t
                 ;; Move to the uncontested read state with the appropriate number
                 ;; of readers.
                 ;; Setting the reader count before waking them prevents woken
                 ;; readers from unlocking the lock too soon.
                 (setf (rw-lock-state rw-lock) (* (rw-lock-n-pending-readers rw-lock)
                                                  +rw-lock-state-reader-increment+))
                 ;; Now wake all pending readers.
                 (loop
                    until (wait-queue-empty-p (rw-lock-reader-wait-queue rw-lock))
                    do
                      (wake-thread (pop-wait-queue (rw-lock-reader-wait-queue rw-lock)))
                      (decf (rw-lock-n-pending-readers rw-lock)))
                 ;; N-PENDING-READERS may be out of sync due to interrupts.
                 ;; Patch things up here.
                 (when (not (zerop (rw-lock-n-pending-readers rw-lock)))
                   (loop
                      for current = (rw-lock-state rw-lock)
                      for new = (- current (* (rw-lock-n-pending-readers rw-lock)
                                              +rw-lock-state-reader-increment+))
                      until (eql (sys.int::cas (rw-lock-state rw-lock) current new) current))
                   (setf (rw-lock-n-pending-readers rw-lock) 0)))))))))

(defun rw-lock-read-release (rw-lock)
  (loop
     (let ((state (rw-lock-state rw-lock)))
       (ensure (not (logtest state +rw-lock-mode-write-bit+)))
       (ensure (plusp (logand state (lognot +rw-lock-mode-contested-bit+))))
       (cond ((eql (logior +rw-lock-state-reader-increment+
                           +rw-lock-mode-contested-bit+)
                   state)
              ;; Lock is contested and we are the last reader.
              (rw-lock-release-slow rw-lock)
              (return))
             (t
              ;; Locked for reading, nothing pending or we're not
              ;; the last reader. Just decrement the reader count.
              (let ((new (- state +rw-lock-state-reader-increment+)))
                (when (eql (sys.int::cas (rw-lock-state rw-lock) state new) state)
                  ;; Successfully released as a reader.
                  (return)))))))
  (values))

(defmacro with-rw-lock-read ((rw-lock &key (wait-p t)) &body body)
  (let ((lock (gensym "RW-LOCK")))
    `(let ((,lock ,rw-lock))
       (when (rw-lock-read-acquire ,lock ,wait-p)
         (unwind-protect
              (progn ,@body)
           (rw-lock-read-release ,lock))))))

(defun rw-lock-write-acquire-slow (sp fp rw-lock)
  ;; Slow path for acquiring a RW lock.
  ;; Returns true if the lock was *not* acquired, false if it was.
  (acquire-place-spinlock (rw-lock-lock rw-lock))
  (lock-wait-queue (rw-lock-writer-wait-queue rw-lock))
  ;; Move to the appropriate contested state.
  (let* ((self (current-thread))
         (current (rw-lock-state rw-lock)))
    ;; Don't contest if the lock actually unlocked.
    (when (or (eql current +rw-lock-state-unlocked+)
              (not (eql (sys.int::cas (rw-lock-state rw-lock)
                                      current
                                      (logior current +rw-lock-mode-contested-bit+))
                        current)))
      ;; Failed to contest the lock. Start over.
      (unlock-wait-queue (rw-lock-writer-wait-queue rw-lock))
      (release-place-spinlock (rw-lock-lock rw-lock))
      (return-from rw-lock-write-acquire-slow t))
    ;; Successfully contested the lock (or it was already contested).
    (sys.int::atomic-incf (rw-lock-write-contested-count rw-lock))
    ;; Now we can safely sleep on the writer wait queue.
    ;; The unlock paths will directly transfer ownership to this thread.
    (push-wait-queue self (rw-lock-writer-wait-queue rw-lock))
    ;; Now sleep.
    ;; Must take the thread lock before dropping the mutex lock or release
    ;; may be able to remove the thread from the sleep queue before it goes
    ;; to sleep.
    (acquire-global-thread-lock)
    (unlock-wait-queue (rw-lock-writer-wait-queue rw-lock))
    (release-place-spinlock (rw-lock-lock rw-lock))
    (setf (thread-wait-item self) (rw-lock-writer-wait-queue rw-lock)
          (thread-state self) :sleeping
          (thread-unsleep-helper self) #'rw-lock-write-acquire
          (thread-unsleep-helper-argument self) rw-lock)
    ;; Returns NIL (don't retry, lock successful)
    (%reschedule-via-wired-stack sp fp)))

(defun rw-lock-write-acquire (rw-lock &optional (wait-p t))
  (loop
     (when (eql (sys.int::cas (rw-lock-state rw-lock)
                              +rw-lock-state-unlocked+
                              +rw-lock-mode-write-locked+)
                +rw-lock-state-unlocked+)
       ;; CAS passed, lock is now write-locked.
       (setf (rw-lock-write-owner rw-lock) (current-thread))
       (return t))
     (when (eql (rw-lock-write-owner rw-lock) (current-thread))
       (if *lock-violations-are-fatal*
           (panic "Recursive write locking detected on " rw-lock " " (rw-lock-name rw-lock))
           (error 'sys.int::mutex-error
                  :mutex rw-lock
                  :format-control "Recursive locking detected on ~S ~S"
                  :format-arguments (list rw-lock (rw-lock-name rw-lock)))))
     (thread-pool-blocking-hijack rw-lock-write-acquire rw-lock wait-p)
     (when (not wait-p)
       (sys.int::atomic-incf (rw-lock-write-contested-count rw-lock))
       (return nil))
     (when (not (%call-on-wired-stack-without-interrupts
                 #'rw-lock-write-acquire-slow nil rw-lock))
       ;; Retry not required, lock is now write-locked.
       (setf (rw-lock-write-owner rw-lock) (current-thread))
       (return t))))

(defun rw-lock-write-release (rw-lock)
  (let ((current-owner (rw-lock-write-owner rw-lock)))
    (cond ((not current-owner)
           (if *lock-violations-are-fatal*
               (panic "Trying to release unheld rw-lock " rw-lock)
               (error 'sys.int::mutex-error
                      :mutex rw-lock
                      :format-control "Trying to release unheld rw-lock ~S ~S"
                      :format-arguments (list rw-lock (rw-lock-name rw-lock)))))
          ((not (eql current-owner (current-thread)))
           (if *lock-violations-are-fatal*
               (panic "Trying to release rw-lock " rw-lock " held by other thread " current-owner)
               (error 'sys.int::mutex-error
                      :mutex rw-lock
                      :format-control "Trying to release rw-lock ~S ~S held by other thread ~S"
                      :format-arguments (list rw-lock (rw-lock-name rw-lock) current-owner))))))
  (setf (rw-lock-write-owner rw-lock) nil)
  ;; Try to move out of the uncontested state.
  (let ((state (sys.int::cas (rw-lock-state rw-lock)
                             +rw-lock-mode-write-locked+
                             +rw-lock-state-unlocked+)))
    (when (not (eql state +rw-lock-mode-write-locked+))
      ;; Lock must be contested.
      (ensure (eql state +rw-lock-mode-write-locked-contested+))
      (rw-lock-release-slow rw-lock)))
  (values))

(defmacro with-rw-lock-write ((rw-lock &key (wait-p t)) &body body)
  (let ((lock (gensym "RW-LOCK")))
    `(let ((,lock ,rw-lock))
       (when (rw-lock-write-acquire ,lock ,wait-p)
         (unwind-protect
              (progn ,@body)
           (rw-lock-write-release ,lock))))))

(defun rw-lock-write-held-p (rw-lock)
  (eql (rw-lock-write-owner rw-lock) (current-thread)))

;;;; The EVENT primitive, used by WAIT-FOR-OBJECTS
;;;; to support waiting for multiple objects.

(sys.int::defglobal *big-wait-for-objects-lock*)

(defstruct (event
             (:constructor %make-event (name %state))
             (:include wait-queue)
             (:area :wired))
  %state
  monitors)

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
        (loop
           for monitor = (event-monitors event)
           then (watcher-event-monitor-event-next monitor)
           until (null monitor)
           do
             (let ((watcher (watcher-event-monitor-watcher monitor)))
               (with-wait-queue-lock (watcher)
                 (do ()
                     ((null (wait-queue-head watcher)))
                   (wake-thread (pop-wait-queue watcher))))))
        (with-wait-queue-lock (event)
          (do ()
              ((null (wait-queue-head event)))
            (wake-thread (pop-wait-queue event)))))
      (setf (event-%state event) value))))

(defmacro event-wait-for ((event &key timeout) &body predicate)
  "As with CONDITION-WAIT-FOR, this waits until PREDICATE is true using EVENT as a way of blocking.
EVENT can be any object that supports GET-OBJECT-EVENT."
  (let ((timeout-sym (gensym "TIMEOUT"))
        (timer-sym (gensym "TIMER"))
        (event-sym (gensym "EVENT"))
        (predicate-fn (gensym "PREDICATE"))
        (prediate-result-sym (gensym)))
    `(let ((,event-sym (convert-object-to-event ,event))
           (,timeout-sym ,timeout))
       (block nil
         (flet ((,predicate-fn () (progn ,@predicate)))
           (cond ((eql ,timeout-sym 0)
                  (values (,predicate-fn) 0))
                 (,timeout-sym
                  (mezzano.supervisor:with-timer (,timer-sym :relative ,timeout-sym :name ,event-sym)
                    (loop
                       (let ((,prediate-result-sym (,predicate-fn)))
                         (when ,prediate-result-sym
                           (return (values ,prediate-result-sym
                                           (timer-remaining ,timer-sym)))))
                       (when (mezzano.supervisor:timer-expired-p ,timer-sym)
                         (return (values nil 0)))
                       (mezzano.supervisor:wait-for-objects ,timer-sym ,event-sym))))
                 (t
                  (loop
                     (let ((,prediate-result-sym (,predicate-fn)))
                       (when ,prediate-result-sym
                         (return (values ,prediate-result-sym nil))))
                     (mezzano.supervisor:event-wait ,event-sym)))))))))

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

;;;; A concurrent object pool.

(defstruct (object-pool
             (:constructor make-object-pool (name object-limit field-getter field-setter))
             (:area :wired))
  name
  ;; Using a generation count avoids the ABA problem.
  (generation 0 :dcas-sibling head :type fixnum)
  (head nil :dcas-sibling generation)
  ;; An approximate count of the number of objects in the pool.
  (n-objects 0 :type fixnum)
  (object-limit (error "not specified") :type fixnum)
  (field-setter (error "not specified") :type function)
  (field-getter (error "not specified") :type function)
  (hit-count 0 :type fixnum)
  (miss-count 0 :type fixnum))

(defun object-pool-push (pool object)
  (loop
     (when (>= (object-pool-n-objects pool)
               (object-pool-object-limit pool))
       ;; Make sure to clear the link field if we set it to something
       ;; in another loop.
       (funcall (object-pool-field-setter pool) nil object)
       (return nil))
     (let ((current-object (object-pool-head pool))
           (current-generation (object-pool-generation pool)))
       ;; Point the new object's link field at the head object.
       (funcall (object-pool-field-setter pool)
                current-object object)
       (when (sys.int::double-compare-and-swap
              (object-pool-generation pool) (object-pool-head pool)
              current-generation current-object
              ;; Avoid fixnum overflow when writing the generation.
              (sys.int::wrapping-fixnum-+ current-generation 1)
              object)
         (sys.int::atomic-incf (object-pool-n-objects pool))
         (return t)))))

(defun object-pool-pop (pool)
  (loop
     (let ((current-object (object-pool-head pool))
           (current-generation (object-pool-generation pool)))
       (when (not current-object)
         (sys.int::atomic-incf (object-pool-miss-count pool))
         (return nil))
       (let ((next-object (funcall (object-pool-field-getter pool) current-object)))
       (when (sys.int::double-compare-and-swap
              (object-pool-generation pool) (object-pool-head pool)
              current-generation current-object
              ;; Avoid fixnum overflow when writing the generation.
              (sys.int::wrapping-fixnum-+ current-generation 1) next-object)
         (sys.int::atomic-decf (object-pool-n-objects pool))
         (sys.int::atomic-incf (object-pool-hit-count pool))
         (return current-object))))))

;;; Event/object watcher.
;;;
;;; This is a generalization of WAIT-FOR-OBJECTS that allows users
;;; to front-load allocation.
;;; W-F-O performs allocation each call, but watchers only perform
;;; allocation at creation & object addition time.
;;; Drivers can create & initialize a watcher during device
;;; initialization, and then use it during normal operation
;;; without performing any additional allocations.

;; Object pools are used for watcher & associated structures.
;; This is to reduce allocation in the wired area which is
;; very slow, and GCing the wired area requires a full GC cycle.
(sys.int::defglobal *watcher-watcher-pool*)
(sys.int::defglobal *watcher-monitor-pool*)

(defstruct (watcher
             (:constructor %make-watcher (name))
             (:include wait-queue)
             (:area :wired))
  watched-objects)

(defstruct (watcher-event-monitor
             (:constructor %make-watcher-event-monitor (watcher object event))
             (:area :wired))
  (result-cons (sys.int::cons-in-area nil nil :wired))
  object
  event
  watcher
  ;; Links in the event's watcher list.
  event-next
  ;; Links in the watcher's watched-object list.
  watcher-next)

(defun convert-object-to-event (object)
  ;; Special case some events to avoid the call
  ;; through G-O-E as it is defined much later.
  (let ((event (typecase object
                 (event object)
                 (timer (timer-event object))
                 (simple-irq (simple-irq-event object))
                 (t (get-object-event object)))))
    (assert (event-p event))
    event))

(defmacro with-watcher ((watcher &key name) &body body)
  "Execute BODY with a watcher, destroys the watcher on unwind."
  (let ((watcher-sym (gensym "WATCHER")))
    `(let ((,watcher-sym (make-watcher :name ,name)))
       (unwind-protect
            (let ((,watcher ,watcher-sym))
              ,@body)
         (watcher-destroy ,watcher-sym)))))

(defun make-watcher (&key name)
  "Create a new event watcher."
  ;; Structured so that CONS-IN-AREA isn't called inside the pool lock.
  (flet ((pop-pool ()
           (let ((watcher (object-pool-pop *watcher-watcher-pool*)))
             (when watcher
               (setf (watcher-name watcher) name
                     (watcher-watched-objects watcher) nil)
               watcher))))
    (declare (dynamic-extent #'pop-pool))
    (or (pop-pool)
        (%make-watcher name))))

(defun watcher-destroy (watcher)
  "Unregister WATCHER from all watched events.
This must be called when the WATCHER is no longer needed.
The watcher and the list returned by WATCHER-WAIT must not be used
after this function returns."
  (check-type watcher watcher)
  (loop
     for monitor = (watcher-watched-objects watcher)
     until (null monitor)
     do (watcher-remove monitor watcher))
  (unmake-watcher watcher)
  (values))

(defun unmake-watcher (watcher)
  (assert (null (watcher-watched-objects watcher)))
  (setf (watcher-name watcher) 'pooled-watcher) ; don't leak the old name
  (object-pool-push *watcher-watcher-pool* watcher))

(defun make-watcher-event-monitor (watcher object event)
  "Create a new event watcher monitor."
  ;; Structured so that CONS-IN-AREA isn't called inside the pool lock.
  (flet ((pop-pool ()
           (let ((monitor (object-pool-pop *watcher-monitor-pool*)))
             (when monitor
               (setf (watcher-event-monitor-watcher monitor) watcher
                     (watcher-event-monitor-event monitor) event
                     (watcher-event-monitor-object monitor) object)
               monitor))))
    (declare (dynamic-extent #'pop-pool))
    (or (pop-pool)
        (%make-watcher-event-monitor watcher object event))))

(defun unmake-watcher-event-monitor (monitor)
  ;; Don't leak objects.
  (setf (car (watcher-event-monitor-result-cons monitor)) nil
        (cdr (watcher-event-monitor-result-cons monitor)) nil
        (watcher-event-monitor-object monitor) nil
        (watcher-event-monitor-event monitor) nil
        (watcher-event-monitor-watcher monitor) nil
        (watcher-event-monitor-event-next monitor) nil
        (watcher-event-monitor-watcher-next monitor) nil)
  (object-pool-push *watcher-monitor-pool* monitor))

(defun watcher-add-object (object watcher)
  "Begin watching a new object.
OBJECT's underlying event is fetched via GET-OBJECT-EVENT as normal.
Returns an opaque tag that can be passed to WATCHER-REMOVE.
If an object is added multiple times then it will be watched multiple times
and can appear multiple times in the watch list.
Invalidates the list returned by WATCHER-WAIT.
This function allocates."
  (check-type watcher watcher)
  (let* ((event (convert-object-to-event object))
         (monitor (make-watcher-event-monitor watcher object event)))
    (safe-without-interrupts (watcher event monitor)
      (with-place-spinlock (*big-wait-for-objects-lock*)
        ;; Link onto the watched objects list.
        (shiftf (watcher-event-monitor-watcher-next monitor)
                (watcher-watched-objects watcher)
                monitor)
        ;; Link onto the event's watcher list.
        (shiftf (watcher-event-monitor-event-next monitor)
                (event-monitors event)
                monitor)))
    monitor))

(defun watcher-remove (tag watcher)
  "Stop watching an object.
TAG must be the tag returned by WATCHER-ADD-OBJECT.
Invalidates the list returned by WATCHER-WAIT."
  ;; Opaque tags are used here to allow multiple identical
  ;; objects to be disambiguated.
  (check-type watcher watcher)
  (check-type tag watcher-event-monitor)
  (safe-without-interrupts (tag watcher)
    (with-place-spinlock (*big-wait-for-objects-lock*)
      ;; Remove from the watcher's monitor list.
      (loop
         for prev = nil then monitor
         for monitor = (watcher-watched-objects watcher)
         then (watcher-event-monitor-watcher-next monitor)
         until (null monitor)
         when (eql monitor tag)
         do
           (if prev
               (setf (watcher-event-monitor-watcher-next prev)
                     (watcher-event-monitor-watcher-next monitor))
               (setf (watcher-watched-objects watcher)
                     (watcher-event-monitor-watcher-next monitor))))
      ;; Remove from the event's monitor list.
      (loop
         with event = (watcher-event-monitor-event tag)
         for prev = nil then monitor
         for monitor = (event-monitors event)
         then (watcher-event-monitor-event-next monitor)
         until (null monitor)
         when (eql monitor tag)
         do
           (if prev
               (setf (watcher-event-monitor-event-next prev)
                     (watcher-event-monitor-event-next monitor))
               (setf (event-monitors event)
                     (watcher-event-monitor-event-next monitor))))))
  (unmake-watcher-event-monitor tag)
  (values))

(defun watcher-objects (watcher)
  "Return a fresh list containing the objects being watched by WATCHER."
  (check-type watcher watcher)
  (loop
     for monitor = (watcher-watched-objects watcher)
     then (watcher-event-monitor-watcher-next monitor)
     until (null monitor)
     collect (watcher-event-monitor-object monitor)))

(defun watcher-wait-1 (sp fp watcher)
  (acquire-place-spinlock *big-wait-for-objects-lock*)
  ;; Scan each object now looking for events that're active.
  (let ((active-events nil))
    (loop
       for monitor = (watcher-watched-objects watcher)
       then (watcher-event-monitor-watcher-next monitor)
       until (null monitor)
       when (event-%state (watcher-event-monitor-event monitor))
       do
         (let ((link (watcher-event-monitor-result-cons monitor)))
           (setf (car link) (watcher-event-monitor-object monitor))
           (shiftf (cdr link)
                   active-events
                   link)))
    (when active-events
      ;; There were some completed events.
      (release-place-spinlock *big-wait-for-objects-lock*)
      (return-from watcher-wait-1 active-events)))
  ;; Now go to sleep, zzz.
  (let ((self (current-thread)))
    (lock-wait-queue watcher)
    (release-place-spinlock *big-wait-for-objects-lock*)
    (acquire-global-thread-lock)
    ;; Attach to the list.
    (push-wait-queue self watcher)
    ;; Sleep.
    (setf (thread-wait-item self) watcher
          (thread-state self) :sleeping
          ;; Reenter the wait after being interrupted.
          (thread-unsleep-helper self) #'watcher-wait
          (thread-unsleep-helper-argument self) watcher)
    (unlock-wait-queue watcher)
    (%reschedule-via-wired-stack sp fp)))

(defun watcher-wait (watcher)
  "Wait for at least one object to activate.
Returns a list of objects were active or became active during the call to WATCHER-WAIT.
The returned list is not fresh and operations on the watcher will destroy it.
This function does not allocate.
This function is not thread-safe. The watcher must not be modified during a
to WATCHER-WAIT and WATCHER-WAIT must not be called simultaneously from
multiple threads."
  (check-type watcher watcher)
  (thread-pool-blocking-hijack watcher-wait watcher)
  (loop
     (let ((completed (%call-on-wired-stack-without-interrupts
                       #'watcher-wait-1 nil
                       watcher)))
       (when completed
         (return completed)))))

;;;; WAIT-FOR-OBJECTS.

(defun wait-for-objects (&rest objects)
  (declare (dynamic-extent objects))
  (with-watcher (watcher)
    (dolist (object objects)
      (watcher-add-object object watcher))
    ;; The returned list must be copied as the watcher is about to
    ;; be destroyed.
    (copy-list (watcher-wait watcher))))

;;;; IRQ-FIFO. An interrupt-safe fixed-size FIFO queue.

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

(defun initialize-sync (first-run-p)
  (when first-run-p
    (setf *watcher-watcher-pool*
          (make-object-pool '*watcher-watcher-pool* 1000
                            #'watcher-watched-objects #'(setf watcher-watched-objects)))
    (setf *watcher-monitor-pool*
          (make-object-pool '*watcher-monitor-pool* 1000
                            #'watcher-event-monitor-watcher #'(setf watcher-event-monitor-watcher)))))
