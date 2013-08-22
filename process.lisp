(in-package :sys.int)

(defvar *current-process* nil)
(defvar *idle-process* nil)
(defvar *run-queue* nil)

(defstruct (process
             (:constructor %make-process))
  name
  (run-reasons '())
  (arrest-reasons '())
  (wait-function nil)
  (wait-argument-list '())
  (whostate '())
  (initial-form nil)
  stack-group
  next prev)

(defmethod print-object ((object process) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (process-name object))))

(defun process-preset (process function &rest arguments)
  (setf (process-initial-form process) (cons function arguments))
  (process-reset process))

(defun process-reset (process)
  (setf (process-wait-function process) nil
	(process-wait-argument-list process) nil)
  (stack-group-preset (process-stack-group process)
                      (lambda ()
                        (with-simple-restart (abort "Terminate process ~S." (process-name process))
                          (apply (first (process-initial-form process))
                                 (rest (process-initial-form process))))
                        (process-disable process)
                        (process-yield))))

(defun process-run-reason (process object)
  (pushnew object (process-run-reasons process))
  (process-consider-runnability process))

(defun process-revoke-run-reason (process object)
  (setf (process-run-reasons process) (remove object (process-run-reasons process)))
  (process-consider-runnability process))

(defun process-arrest-reason (process object)
  (pushnew object (process-arrest-reasons process))
  (process-consider-runnability process))

(defun process-revoke-arrest-reason (process object)
  (setf (process-run-reasons process) (remove object (process-arrest-reasons process)))
  (process-consider-runnability process))

(defun process-wait (reason function &rest arguments)
  (declare (dynamic-extent arguments))
  (setf (process-whostate *current-process*) reason
        (process-wait-function *current-process*) function
        (process-wait-argument-list *current-process*) arguments)
  (process-yield))

;; todo, Fix this...
(defun process-wait-with-timeout (reason timeout function &rest arguments)
  (declare (dynamic-extent arguments))
  (cond (timeout
         (process-wait reason (lambda ()
                                (if (<= (decf timeout) 0)
                                    t
                                    (apply function arguments))))
         (<= timeout 0))
        (t (apply #'process-wait reason function arguments)
           t)))

(defun process-consider-runnability (process)
  (cond ((or (process-arrest-reasons process)
	     (null (process-run-reasons process)))
         (when (process-next process)
           (when (eql *run-queue* process)
             (setf *run-queue* (if (eql (process-next process) process)
                                   nil
                                   (process-next process))))
           (psetf (process-next (process-prev process)) (process-next process)
                  (process-prev (process-next process)) (process-prev process))
           (setf (process-next process) nil
                 (process-prev process) nil)))
        ;; Already on run queue.
        ((process-next process))
	(*run-queue*
         ;; Run queue not empty, add to list.
         (setf (process-next process) *run-queue*
               (process-prev process) (process-prev *run-queue*))
         (setf (process-next (process-prev *run-queue*)) process
               (process-prev *run-queue*) process))
        (t ;; Run queue empty, add as the only entry.
         (setf (process-next process) process
               (process-prev process) process
               *run-queue* process))))

(defun process-enable (process)
  (process-disable process)
  (process-run-reason process :enable))

(defun process-reset-and-enable (process)
  (process-reset process)
  (process-enable process))

(defun process-disable (process)
  (setf (process-run-reasons process) nil
	(process-arrest-reasons process) nil)
  (process-consider-runnability process))

(defun get-next-process ()
  (when *run-queue*
    (loop with start = *run-queue*
       with proc = start
       do (let ((wait-fn (process-wait-function proc))
                (wait-args (process-wait-argument-list proc)))
            (when (or (null wait-fn) (apply wait-fn wait-args))
              (setf (process-wait-function proc) nil
                    (process-wait-argument-list proc) nil)
              (setf *run-queue* (process-next proc))
              (return proc)))
         (setf proc (process-next proc))
         (when (eql proc start) (return)))))

(defun process-yield ()
  (with-interrupts-disabled ()
    (let ((next-process (or (get-next-process)
                            *idle-process*)))
      (unless (eql next-process *current-process*)
        (setf *current-process* next-process)
        (switch-to-stack-group (process-stack-group next-process))))))

(defvar *preemption-enabled* t)

(defun %maybe-preempt-from-interrupt-frame ()
  "Return a process to switch to, or NIL if the current process should keep running."
  (let ((next-process (or (get-next-process)
                          *idle-process*)))
    ;; Must not preempt the GC, or try to switch to current process.
    (when (and *preemption-enabled*
               (not *gc-in-progress*)
               (not (eql next-process *current-process*)))
      (setf *current-process* next-process)
      (process-stack-group next-process))))

(defun idle-process ()
  (%cli)
  (loop
     (let ((next-process (get-next-process)))
       (cond (next-process
              (setf *current-process* next-process)
              (switch-to-stack-group (process-stack-group next-process)))
             (t (%stihlt))))))

(defun make-process (name &key
                            control-stack-size
                            binding-stack-size
                            run-reasons
                            arrest-reasons
                            whostate)
  (let* ((stack-group (make-stack-group name
                                        :control-stack-size control-stack-size
                                        :binding-stack-size binding-stack-size))
         (process (%make-process :name name
                                 :stack-group stack-group
                                 :run-reasons run-reasons
                                 :arrest-reasons arrest-reasons
                                 :whostate whostate)))
    (process-consider-runnability process)
    process))

(defmacro with-process ((name function &rest arguments) &body body)
  (let ((x (gensym)))
    `(let ((,x (make-process ,name)))
       (unwind-protect (progn
                         (sys.int::process-preset ,x ,function ,@arguments)
                         (sys.int::process-enable ,x)
                         ,@body)
         (sys.int::process-disable ,x)))))

(defun current-process ()
  *current-process*)

(setf *current-process* (%make-process :name "Initial Process"
                                       :stack-group (current-stack-group)
                                       :run-reasons '(:initial)
                                       :whostate "RUN"))
(process-consider-runnability *current-process*)

(setf *idle-process* (make-process "Idle"))
(process-preset *idle-process* #'idle-process)

(defconstant +spinlocked-locked-value+ 0)
(defconstant +spinlocked-unlocked-value+ 1)

;(defun (cas foo) (old new args)

(defmacro compare-and-swap (place old new)
  (unless (and (consp place)
               (eql (length place) 2)
               (symbolp (first place)))
    (error "Bad CAS place ~S." place))
  (let ((info (get (first place) 'structure-cas))
        (sym (gensym)))
    (when (not info)
      (error "Bad CAS place ~S." place))
    `(let ((,sym ,(second place)))
       (check-type ,sym ,(first info))
       (%cas-struct-slot ,sym ',(second info) ,old ,new))))

(defstruct (spinlock (:area t))
  (name (error "Name is required!") :read-only t)
  (lock-bits +spinlocked-unlocked-value+ :atomic t :type fixnum))

;;; WARNING! Spinlocks don't use unwind-protect!
;;; They're meant for use in super low-level code
;;; that can't allocated (such as interrupt handlers).
;;; Use a mutex or something instead!
(defmacro with-spinlock-held (spinlock &body body)
  (let ((sym (gensym "lock")))
    `(let ((,sym ,spinlock))
       (spinlock-lock ,sym)
       (multiple-value-prog1 (progn ,@body)
         (spinlock-unlock ,sym)))))

(defun spinlock-lock (lock)
  (assert (not (%interrupt-state)))
  (do ()
      ((compare-and-swap (spinlock-lock-bits lock)
                         +spinlocked-unlocked-value+
                         +spinlocked-locked-value+))
    (cpu-relax)))

(defun spinlock-unlock (lock)
  (assert (eql (spinlock-lock-bits lock) +spinlocked-locked-value+) (lock))
  (setf (spinlock-lock-bits lock) +spinlocked-unlocked-value+))


(defstruct (semaphore (:area t))
  (name (error "Name is required!") :read-only t)
  (count 0 :atomic t :type fixnum))

(defun signal-semaphore (semaphore &optional (n 1))
  "Increment SEMAPHORE, waking processes as required.
Increment the semaphore N times, N must be a non-negative integer.
Safe to call with interrupts off and from inside an interrupt context."
  (check-type n (integer 0))
  (with-interrupts-disabled ()
    (incf (semaphore-count semaphore) n)))

(defun wait-on-semaphore (semaphore &key timeout)
  "Decrement SEMAPHORE, blocking if it is 0.
TIMEOUT is a real, specifying the number of seconds to sleep form.
If TIMEOUT is NIL or some float infinity, then down will sleep forever.
Returns true on success, false on timeout."
  (cond ((try-semaphore semaphore))
        ;; wait functions are called with interrupts off, so it's ok
        ;; to grovel the semaphore in them.
        (t (process-wait-with-timeout (semaphore-name semaphore)
                                      timeout
                                      (lambda ()
                                        (when (not (zerop (semaphore-count semaphore)))
                                          (decf (semaphore-count semaphore))
                                          t))))))

(defun try-semaphore (semaphore)
  "Attempt to decrement SEMAPHORE, returning false the count would become negative."
  (with-interrupts-disabled ()
    (when (not (zerop (semaphore-count semaphore)))
      (decf (semaphore-count semaphore))
      t)))

(defstruct mutex
  (name (error "Name is required!") :read-only t)
  (lock (make-spinlock :name name))
  (owner nil))

(defun call-with-mutex (fn mutex wait-p timeout)
  (assert (%interrupt-state))
  (let ((got-it nil))
    (unwind-protect
         (progn
           ;; Updating GOT-IT must be done without interrupts, or
           ;; the update may be lost if an interrupt occurs after
           ;; the lock but before the setf.
           (with-interrupts-disabled ()
             (setf got-it (grab-mutex mutex :wait-p wait-p :timeout timeout)))
           (when got-it
             (funcall fn)))
      (with-interrupts-disabled ()
        (when got-it
          (release-mutex mutex))))))

(defmacro with-mutex ((mutex &key (wait-p t) timeout) &body body)
  `(call-with-mutex (lambda () ,@body)
                    ,mutex
                    ,wait-p
                    ,timeout))

(defun grab-mutex (mutex &key (wait-p t) timeout)
  (assert (not (%interrupt-state)))
  (when (not (mutex-owner mutex))
    (setf (mutex-owner mutex) (current-process))
    (return-from grab-mutex t))
  (when wait-p
    (let ((process (current-process)))
      (process-wait-with-timeout (mutex-name mutex)
                                 timeout
                                 (lambda ()
                                   (when (not (mutex-owner mutex))
                                     (setf (mutex-owner mutex) process)
                                     t))))))

(defun release-mutex (mutex &key (if-not-owner :error))
  (assert (not (%interrupt-state)))
  (when (or (eql (mutex-owner mutex) (current-process))
            (eql if-not-owner :force))
    (setf (mutex-owner mutex) nil)
    (return-from release-mutex))
  (ecase if-not-owner
    (:error (cerror "Attempting to release mutex ~S not owned by the current process."
                    mutex))
    (:warn (warn "Attempting to release mutex ~S not owned by the current process."
                 mutex))
    ((:punt :force))))

;;; Like boost:thread::barrier, but named so as not to conflict with memory barriers.
(defstruct (rendezvous
             (:constructor make-rendezvous (name count)))
  (name (error "Name not specified."))
  (count (error "Count not specified")
         :type '(integer 1)
         :read-only t)
  (waiting-threads 0))

(defun wait-on-rendezvous (rendezvous)
  (incf (rendezvous-waiting-threads rendezvous))
  (process-wait (rendezvous-name rendezvous)
                (lambda ()
                  (>= (rendezvous-waiting-threads rendezvous)
                      (rendezvous-count rendezvous)))))

(defstruct (fifo (:constructor %make-fifo (buffer name)))
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (buffer (error "No buffer specified.")
          :type array)
  (count-sem (sys.int::make-semaphore :name name)))

(defun make-fifo (size name &optional (type 't))
  (%make-fifo (make-array size :element-type type) name))

(defun fifo-emptyp (fifo)
  (eql (fifo-head fifo) (fifo-tail fifo)))

(defun fifo-fullp (fifo)
  (let ((next (1+ (fifo-tail fifo))))
    (when (>= next (length (fifo-buffer fifo)))
      (setf next 0))
    (eql next (fifo-head fifo))))

(defun fifo-push (value fifo)
  (let ((x (1+ (fifo-tail fifo))))
    (when (>= x (length (fifo-buffer fifo)))
      (setf x 0))
    ;; When next reaches head, the buffer is full.
    (cond ((= x (fifo-head fifo))
           nil)
          (t
           (setf (aref (fifo-buffer fifo) (fifo-tail fifo)) value
                 (fifo-tail fifo) x)
           (sys.int::signal-semaphore (fifo-count-sem fifo))))))

(defun fifo-pop (fifo &key (wait-p t) timeout)
  "Pop a byte from FIFO. Returns NIL if FIFO is empty and wait-p is false, otherwise waits."
  (when (if wait-p
            (sys.int::wait-on-semaphore (fifo-count-sem fifo) :timeout timeout)
            (sys.int::try-semaphore (fifo-count-sem fifo)))
    (assert (not (fifo-emptyp fifo)) (fifo))
    (prog1 (aref (fifo-buffer fifo) (fifo-head fifo))
      (incf (fifo-head fifo))
      (when (>= (fifo-head fifo) (length (fifo-buffer fifo)))
        (setf (fifo-head fifo) 0)))))
