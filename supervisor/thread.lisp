;;;; Multithreading and scheduling

(in-package :mezzano.supervisor)

(sys.int::defglobal *global-thread-lock* nil
  "This lock protects the special variables that make up the thread list/run queues and the thread objects.")
(sys.int::defglobal *supervisor-priority-run-queue*)
(sys.int::defglobal *high-priority-run-queue*)
(sys.int::defglobal *normal-priority-run-queue*)
(sys.int::defglobal *low-priority-run-queue*)
(sys.int::defglobal *all-threads*)
(sys.int::defglobal *n-running-cpus*)

(sys.int::defglobal *world-stopper*)
(sys.int::defglobal *pseudo-atomic-thread-count*)
(sys.int::defglobal *pending-world-stoppers*)
(sys.int::defglobal *pending-pseudo-atomics*)

(sys.int::defglobal *default-stack-size*)

;; Timeslice length, in internal time units.
(sys.int::defglobal *timeslice-length*)

(defvar *pseudo-atomic* nil)

(defstruct (run-queue
             (:area :wired)
             (:constructor make-run-queue (name)))
  name
  head
  tail)

;; Threads initially created by the cold-generator.
(sys.int::defglobal sys.int::*pager-thread*)
(sys.int::defglobal sys.int::*snapshot-thread*)
(sys.int::defglobal sys.int::*disk-io-thread*)
;; The cold-generator creates an idle thread for the BSP.
(sys.int::defglobal sys.int::*bsp-idle-thread*)

(defconstant +thread-mv-slots-size+ 96)
;; Must be a power of two.
(defconstant +thread-symbol-cache-size+ 128)

(defconstant +thread-stack-soft-guard-size+ #x10000
  "Size of the soft guard area.
This area is normally access-protected and is only made accessible
when a stack overflow occurs. It is automatically re-protected after the
thread stops using it.")

(defconstant +thread-stack-guard-return-size+ #x1000
  "Size of the guard return area.
This area is made read-only when the soft guard is triggered and
is used to catch when the thread has left the guard region so that it
can be reprotected.")

(defstruct (thread
             (:area :wired)
             (:constructor %make-thread (%name))
             (:predicate threadp)
             :slot-offsets
             :sealed)
  ;; The name of the thread, a string.
  (%name nil)
  ;; Current state.
  ;;   :active    - the thread is currently running on a core.
  ;;   :runnable  - the thread can be run, but is not currently running.
  ;;   :sleeping  - the thread is waiting for an event and cannot run.
  ;;   :dead      - the thread has exited or been killed and cannot run.
  ;;   :waiting-for-page - the thread is waiting for memory to be paged in.
  ;;   :pager-request - the thread is waiting for a pager RPC call to complete.
  ;;   :stopped   - the thread has been stopped for inspection by a debugger.
  ;;   0 - Thread has not been initialized completely
  (state 0 :type (member :active :runnable :sleeping :dead
                         :waiting-for-page-read :waiting-for-page-write
                         :pager-request
                         :stopped 0))
  ;; Stack object for the stack.
  stack
  ;; State of the stack guard page.
  stack-guard-page-state
  ;; If a thread is sleeping, waiting for page or performing a pager-request, this will describe what it's waiting for.
  ;; When waiting for paging to complete, this will be the faulting address.
  ;; When waiting for a pager-request, this will be the called function.
  (wait-item nil)
  ;; The thread's current special stack pointer.
  (special-stack-pointer nil)
  ;; When true, all registers are saved in the the thread's state save area.
  ;; When false, only the stack pointer and frame pointer are valid.
  full-save-p
  ;; The thread object, used to make CURRENT-THREAD fast.
  self
  ;; Next/previous links for run queues and wait queues.
  (queue-next :unlinked)
  (queue-prev :unlinked)
  ;; A list of foothold functions that need to be run.
  (pending-footholds '())
  ;; A non-negative fixnum, when 0 footholds are permitted to run.
  ;; When positive, they are deferred.
  (inhibit-footholds 1)
  ;; Permit WITH-FOOTHOLDS to temporarily reenable footholds when true.
  (allow-with-footholds t)
  ;; Next/previous links for the *all-threads* list.
  ;; This only contains live (not state = :dead) threads.
  global-next
  global-prev
  ;; Thread's priority, can be :supervisor, :high, :normal, or :low.
  ;; Threads at :supervisor have priority over all other threads.
  (priority :normal :type (member :low :normal :high :supervisor :idle))
  ;; Arguments passed to the pager when performing an RPC.
  pager-argument-1
  pager-argument-2
  pager-argument-3
  ;; Symbol binding cache hit and miss counts.
  (symbol-cache-hit-count 0)
  (symbol-cache-miss-count 0)
  ;; Helper function for UNSLEEP-THREAD.
  unsleep-helper
  ;; Argument for the unsleep helper.
  unsleep-helper-argument
  ;; Event indicating when the thread has died.
  join-event
  ;; The thread pool that this thread belongs to, if any.
  thread-pool
  ;; Run time meter.
  (%run-time 0)
  (switch-time-start 0) ; set when the thread is switched to, used to update run-time
  ;; Time spent in the slow allocation path (including GC time).
  (allocation-time 0)
  ;; Per-thread allocation meter.
  (bytes-consed 0)
  ;; Slots used as part of the multiple-value return convention.
  ;; These contain lisp values, but need to be scanned specially by the GC,
  ;; which is why they have type UB64 instead of T.
  (mv-slots 0 :fixed-vector #.+thread-mv-slots-size+ :type (unsigned-byte 64))
  ;; Symbol binding cell cache.
  (symbol-cache (sys.int::%symbol-binding-cache-sentinel)
                :fixed-vector #.+thread-symbol-cache-size+)
  ;; Other saved machine state.
  (arm64-fpsr 0 :type (unsigned-byte 32))
  (arm64-fpcr 0 :type (unsigned-byte 32))
  (fxsave-area 0 :type (unsigned-byte 8) :fixed-vector 512 :align 16)
  ;; Interrupt save area.
  ;; Used to save an interrupt frame when the thread has stopped to wait for a page.
  ;; The registers are saved here, not on the stack, because the stack may not be paged in.
  ;; This has the same layout as an interrupt frame.
  (state-r15 0 :type (signed-byte 64))
  (state-r14 0 :type (signed-byte 64))
  (state-r13 0 :type (signed-byte 64))
  (state-r12 0 :type (signed-byte 64))
  (state-r11 0 :type (signed-byte 64))
  (state-r10 0 :type (signed-byte 64))
  (state-r9  0 :type (signed-byte 64))
  (state-r8  0 :type (signed-byte 64))
  (state-rdi 0 :type (signed-byte 64))
  (state-rsi 0 :type (signed-byte 64))
  (state-rbx 0 :type (signed-byte 64))
  (state-rdx 0 :type (signed-byte 64))
  (state-rcx 0 :type (signed-byte 64))
  (state-rax 0 :type (signed-byte 64))
  (state-rbp 0 :type (signed-byte 64))
  (state-rip 0 :type (signed-byte 64))
  (state-cs  0 :type (signed-byte 64))
  (state-rflags 0 :type (signed-byte 64))
  (state-rsp 0 :type (signed-byte 64))
  (state-ss  0 :type (signed-byte 64)))

(define-doubly-linked-list-helpers run-queue
    thread-queue-next thread-queue-prev
    run-queue-head run-queue-tail)

(defconstant +thread-interrupt-save-area+ +thread-state-r15+)

;;; Aliases for a few registers.

(defun thread-frame-pointer (thread)
  (thread-state-rbp thread))

(defun (setf thread-frame-pointer) (value thread)
  (setf (thread-state-rbp thread) value))

(defun thread-stack-pointer (thread)
  (thread-state-rsp thread))

(defun (setf thread-stack-pointer) (value thread)
  (setf (thread-state-rsp thread) value))

(macrolet ((reg-value (name field)
             `(progn
                (defun ,name (thread)
                  (check-type thread thread)
                  (sys.int::%object-ref-t thread ,field))
                (defun (setf ,name) (value thread)
                  (check-type thread thread)
                  (setf (sys.int::%object-ref-t thread ,field) value)))))
  (reg-value thread-state-r15-value +thread-state-r15+)
  (reg-value thread-state-r14-value +thread-state-r14+)
  (reg-value thread-state-r13-value +thread-state-r13+)
  (reg-value thread-state-r12-value +thread-state-r12+)
  (reg-value thread-state-r11-value +thread-state-r11+)
  (reg-value thread-state-r10-value +thread-state-r10+)
  (reg-value thread-state-r9-value  +thread-state-r9+)
  (reg-value thread-state-r8-value  +thread-state-r8+)
  (reg-value thread-state-rdi-value +thread-state-rdi+)
  (reg-value thread-state-rsi-value +thread-state-rsi+)
  (reg-value thread-state-rbx-value +thread-state-rbx+)
  (reg-value thread-state-rdx-value +thread-state-rdx+)
  (reg-value thread-state-rcx-value +thread-state-rcx+)
  (reg-value thread-state-rax-value +thread-state-rax+)
  (reg-value thread-state-rbp-value +thread-state-rbp+)
  (reg-value thread-state-rip-value +thread-state-rip+)
  (reg-value thread-state-cs-value  +thread-state-cs+)
  (reg-value thread-state-rflags-value +thread-state-rflags+)
  (reg-value thread-state-rsp-value +thread-state-rsp+)
  (reg-value thread-state-ss-value  +thread-state-ss+))

;;; Locking.

(defun acquire-global-thread-lock ()
  (acquire-symbol-spinlock *global-thread-lock*))

(defun release-global-thread-lock ()
  (release-symbol-spinlock *global-thread-lock*))

(defmacro with-global-thread-lock ((&optional) &body body)
  `(with-symbol-spinlock (*global-thread-lock*)
     ,@body))

(defun ensure-global-thread-lock-held ()
  (ensure-symbol-spinlock-held *global-thread-lock*))

;;; Run queue management.

(defun run-queue-for-priority (priority)
  (ecase priority
    (:supervisor *supervisor-priority-run-queue*)
    (:high *high-priority-run-queue*)
    (:normal *normal-priority-run-queue*)
    (:low *low-priority-run-queue*)))

(defun push-run-queue-1 (thread rq)
  (run-queue-push-back thread rq))

(defun push-run-queue (thread)
  (ensure-global-thread-lock-held)
  (when (eql thread *world-stopper*)
    (return-from push-run-queue))
  (push-run-queue-1 thread (run-queue-for-priority (thread-priority thread))))

(defun pop-run-queue-1 (rq)
  (run-queue-pop-front rq))

(defun pop-run-queue ()
  (or (pop-run-queue-1 *supervisor-priority-run-queue*)
      (pop-run-queue-1 *high-priority-run-queue*)
      (pop-run-queue-1 *normal-priority-run-queue*)
      (pop-run-queue-1 *low-priority-run-queue*)))

(defun dump-run-queue (rq)
  (debug-print-line "Run queue " rq "/" (run-queue-name rq) ":")
  (do-run-queue (thread rq)
    (debug-print-line "  " thread "/" (thread-name thread))))

(defun dump-run-queues ()
  (debug-print-line "Run queues:")
  (when *world-stopper*
    (debug-print-line "Thread " *world-stopper* " holds the world"))
  (when (boundp '*normal-priority-run-queue*)
    (dump-run-queue *supervisor-priority-run-queue*)
    (dump-run-queue *high-priority-run-queue*)
    (dump-run-queue *normal-priority-run-queue*)
    (dump-run-queue *low-priority-run-queue*)))

(defun other-threads-ready-to-run-p ()
  (not (and (run-queue-empty-p *supervisor-priority-run-queue*)
            (run-queue-empty-p *high-priority-run-queue*)
            (run-queue-empty-p *normal-priority-run-queue*)
            (run-queue-empty-p *low-priority-run-queue*))))

(defun %update-run-queue ()
  "Possibly return the current thread to the run queue, and
return the next thread to run.
Interrupts must be off and the global thread lock must be held."
  (ensure-global-thread-lock-held)
  ;; Cancel the preemption timer, only used for scheduling normal threads.
  (preemption-timer-reset nil)
  (let ((current (current-thread)))
    ;; Return the current thread to the run queue and fetch the next thread.
    (when (and (not (eql current (local-cpu-idle-thread)))
               (not (eql current *world-stopper*))
               (eql (thread-state current) :runnable))
      (push-run-queue current))
    (cond (*world-stopper*
           ;; World is stopped, the only runnable threads are the world stopper
           ;; or any thread at :supervisor priority.
           ;; Supervisor priority threads first.
           (cond ((pop-run-queue-1 *supervisor-priority-run-queue*))
                 ((eql (thread-state *world-stopper*) :runnable)
                  ;; The world stopper is ready.
                  *world-stopper*)
                 (t ;; Switch to idle.
                  (local-cpu-idle-thread))))
          (t
           ;; Try taking from the run queue.
           (let ((next (pop-run-queue)))
             (cond (next
                    ;; This is a normal thread, re-arm the preemption timer.
                    ;; Note: Some of this logic is replicated in the idle thread's body...
                    ;; Only needed when there are other threads waiting to run,
                    ;; if this is the only runnable thread then it can run forever.
                    (when (other-threads-ready-to-run-p)
                      (preemption-timer-reset *timeslice-length*))
                    next)
                   (t
                    ;; Fall back on idle.
                    (local-cpu-idle-thread))))))))

(defun update-run-queue ()
  "Return the current thread to the run queue, if required and pick a new thread to run."
  (ensure-global-thread-lock-held)
  (let ((current (current-thread)))
    (ensure (not (eql (thread-state current) :active))
            "Current thread " current " has wrong state " (thread-state current))
    (let ((next (%update-run-queue)))
      (ensure (eql (thread-state next) :runnable) "Switching to thread " next " with bad state " (thread-state next))
      (setf (thread-state next) :active)
      (when (<= (car sys.int::*exception-stack*)
                (thread-stack-pointer next)
                (1- (+ (car sys.int::*exception-stack*)
                       (cdr sys.int::*exception-stack*))))
        (panic "Other thread " next " stopped on exception stack!!!"))
      next)))

;;; Thread switching.

(defun thread-yield ()
  "Call this to give up the remainder of the current thread's timeslice and possibly switch to another runnable thread."
  (%run-on-wired-stack-without-interrupts (sp fp)
   (let ((current (current-thread)))
     (acquire-global-thread-lock)
     (setf (thread-state current) :runnable)
     (%reschedule-via-wired-stack sp fp))))

(defun %reschedule-via-wired-stack (sp fp)
  ;; Switch to the next thread saving minimal state.
  ;; Interrupts must be off and the global thread lock must be held.
  ;; Releases the thread lock and reenables interrupts.
  (ensure-global-thread-lock-held)
  (let ((next (update-run-queue))
        (current (current-thread)))
    (cond ((eql next current)
           ;; Staying on the same thread, unlock and return.
           (release-global-thread-lock)
           (%%return-to-same-thread sp fp))
          (t
           (%%switch-to-thread-via-wired-stack current sp fp next)))
    (panic "unreachable")))

(defun %reschedule-via-interrupt (interrupt-frame)
  ;; Switch to the next thread saving the full state.
  ;; Interrupts must be off and the global thread lock must be held.
  ;; Releases the thread lock and reenables interrupts.
  (ensure-global-thread-lock-held)
  (let ((next (update-run-queue))
        (current (current-thread)))
    (%%switch-to-thread-via-interrupt current interrupt-frame next)))

(defun maybe-preempt-via-interrupt (interrupt-frame)
  (let ((current (current-thread)))
    (acquire-global-thread-lock)
    (cond ((or (and *world-stopper*
                    (eql current *world-stopper*))
               (eql (thread-priority current) :supervisor)
               (eql current (local-cpu-idle-thread)))
           (release-global-thread-lock))
          (t
           (setf (thread-state current) :runnable)
           (%reschedule-via-interrupt interrupt-frame)))))

(defun %%switch-to-thread-via-wired-stack (current-thread sp fp next-thread)
  ;; Save frame pointer.
  (setf (thread-state-rbp-value current-thread) fp)
  ;; Save FPU state.
  ;; FIXME: FPU state doesn't need to be completely saved for voluntary task switches.
  ;; Only MXCSR & FCW or FPCR need to be preserved.
  (save-fpu-state current-thread)
  ;; Save stack pointer.
  (setf (thread-state-rsp-value current-thread) sp)
  ;; Only partial state was saved.
  (setf (thread-full-save-p current-thread) nil)
  ;; Jump to common function.
  (%%switch-to-thread-common current-thread next-thread))

(defun %%switch-to-thread-via-interrupt (current-thread interrupt-frame next-thread)
  (save-fpu-state current-thread)
  (save-interrupted-state current-thread interrupt-frame)
  ;; Jump to common function.
  (%%switch-to-thread-common current-thread next-thread))

(defun update-run-time (thread now)
  (let ((start (thread-switch-time-start thread)))
    (incf (thread-%run-time thread)
          (high-precision-time-units-to-internal-time-units
           (if (< now start)
               ;; Wrapped. Assume that it has only wrapped once.
               (- (- now start))
               (- now start))))))

(defun thread-run-time (thread)
  (when (eql thread (current-thread))
    ;; Update the run-time variable so it is as accurate as possible.
    (safe-without-interrupts ()
      (let ((self (current-thread))
            (now (get-high-precision-timer)))
        (update-run-time self now)
        (setf (thread-switch-time-start self) now))))
  (thread-%run-time thread))

(defun %%switch-to-thread-common (current-thread new-thread)
  ;; Current thread's state has been saved, restore the new-thread's state.
  ;; Update run-time meters.
  (let ((now (get-high-precision-timer)))
    (update-run-time current-thread now)
    (setf (thread-switch-time-start new-thread) now))
  ;; Switch threads.
  (set-current-thread new-thread)
  ;; Restore FPU state.
  (restore-fpu-state new-thread)
  ;; The global thread lock is dropped by the restore functions, not here.
  ;; We are still running on the current (old) thread's stack, so cannot
  ;; allow another CPU to switch on to it just yet.
  ;; This can only occur when performing a voluntary switch away from
  ;; a thread with a wired stack - one of the ephemeral supervisor threads.
  ;; Check if the thread is full-save.
  (if (thread-full-save-p new-thread)
      (%%restore-full-save-thread new-thread)
      (%%restore-partial-save-thread new-thread)))

;;; Stuff.

(defun copy-name-to-wired-area (name)
  (typecase name
    (string
     (mezzano.runtime::copy-string-in-area name :wired))
    (cons
     (sys.int::copy-list-in-area
      (mapcar #'copy-name-to-wired-area name)
      :wired))
    (t
     name)))

(defun thread-name (thread)
  (thread-%name thread))

(defun (setf thread-name) (value thread)
  (setf (thread-%name thread) (copy-name-to-wired-area value))
  value)

(defun make-thread (function &key name initial-bindings (stack-size *default-stack-size*) (priority :normal))
  (declare (mezzano.compiler::closure-allocation :wired))
  (check-type function (or function symbol))
  (check-type priority (member :supervisor :high :normal :low))
  (setf name (copy-name-to-wired-area name))
  (setf stack-size (align-up stack-size #x1000))
  (let* ((thread (%make-thread name))
         (stack (%allocate-stack (+ stack-size +thread-stack-soft-guard-size+))))
    (setf (thread-stack thread) stack
          (thread-self thread) thread
          (thread-priority thread) priority
          (thread-join-event thread) (make-event :name thread))
    ;; Protect the guard area, making it fully inaccessible.
    (protect-memory-range (stack-base stack) +thread-stack-soft-guard-size+ 0)
    ;; Perform initial bindings.
    (when initial-bindings
      (let ((symbols (mapcar #'first initial-bindings))
            (values (mapcar #'second initial-bindings))
            (original-function function))
        (setf function (lambda ()
                         (progv symbols values
                           (funcall original-function))))))
    ;; Set up the initial register state.
    (let ((stack-pointer (+ (stack-base stack) (stack-size stack)))
          (trampoline #'thread-entry-trampoline))
      (arch-initialize-thread-state thread stack-pointer)
      ;; Initialize state save area.
      (setf (thread-state-rbp thread) 0
            ;; Trampoline entry point.
            (thread-state-rip thread) (sys.int::%object-ref-signed-byte-64 trampoline 0)
            (thread-state-rax thread) 0
            ;; 1 argument passed.
            (thread-state-rcx-value thread) 1
            (thread-state-rdx thread) 0
            ;; FUNCALL calling convention.
            (thread-state-rbx-value thread) trampoline
            (thread-state-rsi thread) 0
            (thread-state-rdi thread) 0
            ;; First arg, function to call.
            (thread-state-r8-value thread) function
            (thread-state-r9 thread) 0
            (thread-state-r10 thread) 0
            (thread-state-r11 thread) 0
            (thread-state-r12 thread) 0
            (thread-state-r13 thread) 0
            (thread-state-r14 thread) 0
            (thread-state-r15 thread) 0))
    (setf (thread-full-save-p thread) t
          (thread-state thread) :runnable)
    (safe-without-interrupts (thread)
      (with-symbol-spinlock (*global-thread-lock*)
        (push-run-queue thread)
        ;; Add thread to global thread list.
        (setf (thread-global-prev *all-threads*) thread
              (thread-global-next thread) *all-threads*
              (thread-global-prev thread) nil
              *all-threads* thread)))
    thread))

;; MAKE-THREAD arranges for new threads to call this function with the thread's
;; initial function as an argument.
;; It sets up the top-level catch for 'terminate-thread, and deals with cleaning
;; up when the thread exits (either by normal return or by a throw to terminate-thread).
(defun thread-entry-trampoline (function)
  (let ((return-values 'terminate-thread))
    (unwind-protect
         (catch 'terminate-thread
           (unwind-protect
                ;; Footholds in a new thread are inhibited until the terminate-thread
                ;; catch block is established, to guarantee that it's always available.
                (progn
                  (when (eql (sys.int::%atomic-fixnum-add-object (current-thread) +thread-inhibit-footholds+ -1) 1)
                    (run-pending-footholds))
                  (setf return-values (multiple-value-list (funcall function))))
             ;; Re-inhibit footholds when leaving. This way it is never possible
             ;; to foothold a thread after the TERMINATE-THREAD catch has exited.
             ;; There's still a race here: If TERMINATE-THREAD is thrown to while
             ;; the unwind protect handler is executing (before inhibit footholds
             ;; has been incremented), then footholds will still be enabled.
             ;; The perils of asynchronous interrupts...
             (sys.int::%atomic-fixnum-add-object (current-thread) +thread-inhibit-footholds+ 1)))
      ;; Cleanup, terminate the thread.
      (thread-final-cleanup return-values))))

;; This is seperate from thread-entry-trampoline so steppers can detect it.
(defun thread-final-cleanup (return-values)
  (%run-on-wired-stack-without-interrupts (sp fp return-values)
    (let ((self (current-thread)))
      ;; FIXME: This should be done with the global lock held, but that makes
      ;; the lock ordering incorrect in (setf event-state).
      ;; (setf event-state) expects to be called with the thread lock released.
      ;; This leaves a small race window between the thread's join event
      ;; being set and the thread state being set to dead, but this is only
      ;; visible on SMP as interrupts are disabled here.
      (setf (event-state (thread-join-event self)) (or return-values :no-values))
      (acquire-global-thread-lock)
      (setf (thread-state self) :dead)
      ;; Remove thread from the global list.
      (when (thread-global-next self)
        (setf (thread-global-prev (thread-global-next self)) (thread-global-prev self)))
      (when (thread-global-prev self)
        (setf (thread-global-next (thread-global-prev self)) (thread-global-next self)))
      (when (eql self *all-threads*)
        (setf *all-threads* (thread-global-next self)))
      (%reschedule-via-wired-stack sp fp))))

(defun thread-join (thread &optional (wait-p t))
  "Wait for THREAD to exit.
If the thread has exited, then the first value returned will
be either a list of values returned by the thread's initial function
or 'TERMINATE-THREAD if the thread exited due to a throw to 'TERMINATE-THREAD.
The second value will be true if the thread has exited or false if it has
not and WAIT-P is false."
  (when wait-p
    (event-wait (thread-join-event thread)))
  (let ((values (event-state (thread-join-event thread))))
    (cond ((null values)
           ;; Not yet exited.
           (values nil nil))
          ((eql values :no-values)
           ;; Returned no values but event-state can't represent that as an empty list.
           (values '() t))
          (t
           (values values t)))))

;; The idle thread is not a true thread. It does not appear in all-threads, nor in any run-queue.
;; When the machine boots, one idle thread is created for each core. When a core is idle, the
;; idle thread will be run.
(defun idle-thread ()
  (%disable-interrupts)
  (decrement-n-running-cpus)
  (let ((self (current-thread)))
    (loop
       ;; Must be running on the right CPU.
       (ensure (eql self (local-cpu-idle-thread)))
       ;; Make sure the preemption timer has been properly switched off when
       ;; the system idles. Not needed to be correct, but reduces activity
       ;; when idle.
       (ensure (not (preemption-timer-remaining)))
       ;; Look for a thread to switch to.
       (acquire-global-thread-lock)
       (setf (thread-state self) :runnable)
       (let ((next (update-run-queue)))
         (cond ((not (eql next self))
                (increment-n-running-cpus)
                ;; Switch to thread.
                (%run-on-wired-stack-without-interrupts (sp fp next self)
                  (%%switch-to-thread-via-wired-stack self sp fp next))
                (%disable-interrupts)
                (decrement-n-running-cpus))
               (t ;; Wait for an interrupt.
                (release-global-thread-lock)
                (%wait-for-interrupt)
                (%disable-interrupts)))))))

(defun increment-n-running-cpus ()
  (let ((prev (sys.int::%atomic-fixnum-add-symbol '*n-running-cpus* 1)))
    (when (zerop prev)
      (set-run-light t))))

(defun decrement-n-running-cpus ()
  (let ((prev (sys.int::%atomic-fixnum-add-symbol '*n-running-cpus* -1)))
    (when (and (eql prev 1)
               (boundp '*light-run*))
      ;; Clear the run light immediately so it doesn't stay on between
      ;; GUI screen updates.
      (clear-light *light-run*))))

(defun make-ephemeral-thread (entry-point initial-state &key name (stack-size (* 256 1024)) (priority :normal))
  (let* ((thread (%make-thread name))
         (stack (%allocate-stack stack-size t)))
    (setf (thread-stack thread) stack
          (thread-self thread) thread
          (thread-priority thread) priority)
    (reset-ephemeral-thread thread entry-point initial-state priority)
    thread))

(defun reset-ephemeral-thread (thread entry-point state priority)
  ;; Threads created by the cold-generator have conses instead of real stack
  ;; objects. Work around this.
  (when (consp (thread-stack thread))
    (setf (thread-stack thread) (%make-stack (car (thread-stack thread))
                                             (cdr (thread-stack thread)))))
  ;; Set up the initial register state.
  (let ((stack-pointer (+ (stack-base (thread-stack thread))
                          (stack-size (thread-stack thread))))
        (function (sys.int::%coerce-to-callable entry-point)))
    ;; Initialize state save area.
    (arch-initialize-thread-state thread stack-pointer)
    (setf (thread-state-rbp thread) 0
          ;; Entry point.
          (thread-state-rip thread) (sys.int::%object-ref-signed-byte-64 function 0)
          (thread-state-rax thread) 0
          ;; 0 arguments passed.
          (thread-state-rcx-value thread) 0
          (thread-state-rdx thread) 0
          ;; FUNCALL calling convention.
          (thread-state-rbx-value thread) function
          (thread-state-rsi thread) 0
          (thread-state-rdi thread) 0
          (thread-state-r8 thread) 0
          (thread-state-r9 thread) 0
          (thread-state-r10 thread) 0
          (thread-state-r11 thread) 0
          (thread-state-r12 thread) 0
          (thread-state-r13 thread) 0
          (thread-state-r14 thread) 0
          (thread-state-r15 thread) 0))
  ;; Remove the thread from any potential run queue it may be on.
  (when (and (not (eql priority :idle))
             (run-queue-linked-p thread))
    (debug-print-line "Removing thread " thread " from rq "
                      (run-queue-for-priority (thread-priority thread))
                      " next is " (thread-queue-next thread)
                      " prev is " (thread-queue-prev thread)
                      " head is " (run-queue-head (run-queue-for-priority (thread-priority thread)))
                      " tail is " (run-queue-tail (run-queue-for-priority (thread-priority thread))))
    ;; FIXME: Something funny is going on here...
    ;; The disk io thread has nil (not :unlinked) in the next/prev fields but
    ;; the supervisor run queue is empty.
    ;; Hack around this bug ^
    (when (run-queue-head (run-queue-for-priority (thread-priority thread)))
      (run-queue-remove thread
                        (run-queue-for-priority (thread-priority thread)))))
  (setf (thread-state thread) state
        (thread-priority thread) (or priority :supervisor)
        (thread-special-stack-pointer thread) nil
        (thread-wait-item thread) nil
        (thread-pending-footholds thread) '()
        (thread-inhibit-footholds thread) 1
        (thread-full-save-p thread) t)
  (when (and (not (eql priority :idle))
             (eql state :runnable))
    (safe-without-interrupts (thread)
      (acquire-global-thread-lock)
      (push-run-queue thread)
      (release-global-thread-lock)))
  ;; Flush the symbol cache.
  (dotimes (i +thread-symbol-cache-size+)
    (setf (thread-symbol-cache thread i) (sys.int::%symbol-binding-cache-sentinel))))

(defun initialize-threads ()
  (when (not (boundp '*global-thread-lock*))
    ;; First-run stuff.
    (setf *global-thread-lock* :unlocked)
    (setf *supervisor-priority-run-queue* (make-run-queue :supervisor)
          *high-priority-run-queue* (make-run-queue :high)
          *normal-priority-run-queue* (make-run-queue :normal)
          *low-priority-run-queue* (make-run-queue :low))
    ;; Default timeslice is 10ms/0.01s.
    ;; Work at read-time to avoid floats at runtime.
    (setf *timeslice-length* #.(truncate (* 0.01 internal-time-units-per-second)))
    (setf *pseudo-atomic-thread-count* 0
          *pending-world-stoppers* (make-wait-queue :name '*pending-world-stoppers*)
          *pending-pseudo-atomics* (make-wait-queue :name '*pending-pseudo-atomics*))
    (setf *all-threads* sys.int::*snapshot-thread*
          (thread-global-next sys.int::*snapshot-thread*) sys.int::*pager-thread*
          (thread-global-prev sys.int::*snapshot-thread*) nil
          (thread-global-next sys.int::*pager-thread*) sys.int::*disk-io-thread*
          (thread-global-prev sys.int::*pager-thread*) sys.int::*snapshot-thread*
          (thread-global-next sys.int::*disk-io-thread*) nil
          (thread-global-prev sys.int::*disk-io-thread*) sys.int::*pager-thread*)
    (setf *default-stack-size* (* 1024 1024)))
  (setf *n-running-cpus* 1)
  (reset-ephemeral-thread sys.int::*bsp-idle-thread* #'idle-thread :runnable :idle)
  (reset-ephemeral-thread sys.int::*snapshot-thread* #'snapshot-thread :sleeping :supervisor)
  ;; Don't let the pager run until the paging disk has been found.
  (reset-ephemeral-thread sys.int::*pager-thread* #'pager-thread :sleeping :supervisor)
  (setf (thread-wait-item sys.int::*pager-thread*) "Waiting for paging disk")
  (reset-ephemeral-thread sys.int::*disk-io-thread* #'disk-thread :runnable :supervisor))

(defun wake-thread (thread)
  "Wake a sleeping thread."
  (without-interrupts
    (with-symbol-spinlock (*global-thread-lock*)
      (wake-thread-1 thread))))

(defun wake-thread-1 (thread)
  "Wake a sleeping thread, with locks held."
  (ensure-interrupts-disabled)
  (ensure-global-thread-lock-held)
  (ensure (not (or (eql (thread-state thread) :runnable)
                   (eql (thread-state thread) :active)))
          "Thread " thread " not sleeping.")
  (setf (thread-state thread) :runnable)
  (push-run-queue thread)
  (broadcast-wakeup-ipi))

(defun initialize-initial-thread ()
  "Called very early after boot to reset the initial thread."
  (let* ((thread (current-thread)))
    (setf *world-stopper* thread)
    (setf (thread-state thread) :active)
    (setf (thread-switch-time-start thread) (get-high-precision-timer))))

(defun finish-initial-thread ()
  "Called when the boot code is done with the initial thread."
  ;; The initial thread never dies, it just sleeps until the next boot.
  ;; The bootloader will partially wake it up, then initialize-initial-thread
  ;; will finish initialization.
  ;; The initial thread must finish with no values on the special stack.
  ;; This is required by INITIALIZE-INITIAL-THREAD.
  (let ((thread (current-thread)))
    (%call-on-wired-stack-without-interrupts
     #'%resume-the-world nil)
    (%disable-interrupts)
    (acquire-global-thread-lock)
    (setf (thread-wait-item thread) "The start of a new world"
          (thread-state thread) :sleeping)
    (%run-on-wired-stack-without-interrupts (sp fp)
     (%reschedule-via-wired-stack sp fp))
    (panic "Initial thread woken??")))

(defun all-threads ()
  (do ((list '())
       (current *all-threads* (thread-global-next current)))
      ((null current)
       list)
    (push current list)))

(defun terminate-thread (thread)
  (establish-thread-foothold
   thread
   (lambda ()
     (throw 'terminate-thread nil))))

(defmacro dx-lambda (lambda-list &body body)
  `(flet ((dx-lambda ,lambda-list ,@body))
     (declare (dynamic-extent #'dx-lambda))
     #'dx-lambda))

;;; Foothold management.

(defmacro without-footholds (&body body)
  (let ((thread (gensym))
        (old-allow-with-footholds (gensym)))
    ;; THREAD-INHIBIT-FOOTHOLDS is incremented before the UNWIND-PROTECT
    ;; is established. Doing it the other way around would allow a race
    ;; condition. The threads could be interrupted after the U-P is established
    ;; but before footholds are inhibited, and the foothold could unwind.
    ;; This would cause T-I-F to be decremented by the cleanup handler, even
    ;; though it had never been incremented.
    `(let ((,thread (current-thread)))
       (sys.int::%atomic-fixnum-add-object ,thread +thread-inhibit-footholds+ 1)
       (let ((,old-allow-with-footholds (thread-allow-with-footholds ,thread)))
         (when ,old-allow-with-footholds
           ;; The WHEN is a performance kludge, avoid touching thread slots as much as possible
           ;; TODO: Restructure this to avoid fiddling with T-A-W-F at all
           ;; if A-W-F isn't used.
           (setf (thread-allow-with-footholds ,thread) nil))
         (unwind-protect
              (macrolet ((allow-with-footholds (&body allow-forms)
                           `(unwind-protect
                                 (progn
                                   (setf (thread-allow-with-footholds ,',thread) ,',old-allow-with-footholds)
                                   (locally ,@allow-forms))
                              (setf (thread-allow-with-footholds ,',thread) nil)))
                         (with-local-footholds (&body with-forms)
                           `(allow-with-footholds
                             (with-footholds ,@with-forms))))
                ,@body)
           (when ,old-allow-with-footholds
             (setf (thread-allow-with-footholds ,thread) t))
           (when (eql (sys.int::%atomic-fixnum-add-object ,thread +thread-inhibit-footholds+ -1) 1)
             (run-pending-footholds)))))))

(defmacro with-footholds (&body body)
  "Enable footholds for the duration of BODY, if permitted by ALLOW-WITH-FOOTHOLDS.
If thereis a matching ALLOW-WITH-FOOTHOLDS for every WITHOUT-FOOTHOLDS, then
footholds will be reenabled, otherwise footholds will stay inhibited."
  (let ((thread (gensym))
        (old-inhibit-footholds (gensym)))
    `(let* ((,thread (current-thread))
            (,old-inhibit-footholds (thread-inhibit-footholds ,thread)))
       (unwind-protect
            (progn
              (when (thread-allow-with-footholds ,thread)
                (setf (thread-inhibit-footholds ,thread) 0)
                (run-pending-footholds))
              (locally ,@body))
         (setf (thread-inhibit-footholds ,thread) ,old-inhibit-footholds)))))

(defmacro allow-with-footholds (&body body)
  (declare (ignore body))
  (error "ALLOW-WITH-FOOTHOLDS not permitted outside WITHOUT-FOOTHOLDS"))

(defmacro with-local-footholds (&body body)
  (declare (ignore body))
  (error "WITH-LOCAL-FOOTHOLDS not permitted outside WITHOUT-FOOTHOLDS"))

(declaim (inline run-pending-footholds))
(defun run-pending-footholds ()
  (let ((footholds (sys.int::%xchg-object (mezzano.supervisor:current-thread)
                                          mezzano.supervisor::+thread-pending-footholds+
                                          nil)))
    (dolist (fh footholds)
      (funcall fh))))

(defun unsleep-thread (thread)
  (let ((did-wake (safe-without-interrupts (thread)
                    (let ((wi (thread-wait-item thread)))
                      (when (wait-queue-p wi)
                        (lock-wait-queue wi))
                      (with-symbol-spinlock (*global-thread-lock*)
                        (cond ((eql (thread-state thread) :sleeping)
                               ;; Remove the thread from its associated wait-queue.
                               (ensure (wait-queue-p wi)
                                       "Thread " thread " sleeping on non-wait-queue " wi)
                               (remove-from-wait-queue thread wi)
                               (unlock-wait-queue wi)
                               (wake-thread-1 thread)
                               t)
                              (t
                               (when (wait-queue-p wi)
                                 (unlock-wait-queue wi))
                               nil)))))))
    (when did-wake
      ;; Arrange for the unsleep helper to be called.
      ;; Must be done outside the s-w-i form as this touches the thread's stack.
      (force-call-on-thread thread
                            (thread-unsleep-helper thread)
                            (thread-unsleep-helper-argument thread)))))

(defun establish-thread-foothold (thread function &key force)
  (check-type thread thread)
  (check-type function function)
  (assert (not (member (thread-priority thread) '(:idle :supervisor))))
  ;; Can't be allocated in push-foothold as that's called with the world stopped.
  (let ((push-cons (sys.int::cons-in-area function nil :wired)))
    (flet ((push-foothold ()
             (safe-without-interrupts (thread push-cons)
               (with-symbol-spinlock (*global-thread-lock*)
                 (setf (cdr push-cons) (thread-pending-footholds thread)
                       (thread-pending-footholds thread) push-cons)))))
      (cond ((eql thread (current-thread))
             (cond ((or force
                        (eql (thread-inhibit-footholds thread) 0))
                    (funcall function))
                   (t
                    (push-foothold))))
            (t
             (with-world-stopped ()
               ;; Stopping the world will prevent the thread from running on another CPU.
               (ensure (not (eql (thread-state thread) :active))
                       "Impossible! Tried to foothold running thread " thread)
               (cond ((eql (thread-state thread) :dead)
                      ;; Thread is dead, do nothing.
                      nil)
                     ((or force
                          (eql (thread-inhibit-footholds thread) 0))
                      (unsleep-thread thread)
                      (force-call-on-thread thread function))
                     (t
                      (push-foothold)))))))))

(defun stop-current-thread ()
  (%run-on-wired-stack-without-interrupts (sp fp)
   (let ((current (current-thread)))
     (acquire-global-thread-lock)
     (setf (thread-state current) :stopped)
     (%reschedule-via-wired-stack sp fp))))

;; Reads of THREAD-STATE must be protected by the global thread lock to
;; allow the thread to settle.
(defun sample-thread-state (thread)
  (safe-without-interrupts (thread)
    (with-symbol-spinlock (*global-thread-lock*)
      (thread-state thread))))

(defun stop-thread (thread)
  "Stop a thread, waiting for it to enter the stopped state."
  (check-type thread thread)
  (assert (not (eql thread (current-thread))))
  (establish-thread-foothold thread #'stop-current-thread)
  (loop
     (when (member (sample-thread-state thread) '(:stopped :dead))
       (return))
     (thread-yield))
  (values))

(defun resume-thread (thread &optional why)
  "Resume a stopped thread."
  (check-type thread thread)
  (assert (eql (thread-state thread) :stopped))
  (safe-without-interrupts (thread why)
    (setf (thread-wait-item thread) why)
    (wake-thread thread))
  (values))

;;; Stopping the world.
;;; WITH-WORLD-STOPPED and WITH-PSEUDO-ATOMIC work together as a sort-of global
;;; reader/writer lock over the whole system.

(defun acquire-stw-locks ()
  (lock-wait-queue *pending-world-stoppers*)
  (lock-wait-queue *pending-pseudo-atomics*)
  (acquire-global-thread-lock))

(defun release-stw-locks (&optional exclude-global-thread-lock)
  (when (not exclude-global-thread-lock)
    (release-global-thread-lock))
  (unlock-wait-queue *pending-pseudo-atomics*)
  (unlock-wait-queue *pending-world-stoppers*))

(defun %stop-the-world-unsleep (arg)
  (declare (ignore arg))
  (%call-on-wired-stack-without-interrupts #'%stop-the-world nil))

(defun %stop-the-world (sp fp)
  (acquire-stw-locks)
  (let ((self (current-thread)))
    (when (and (eql *pseudo-atomic-thread-count* 0)
               (not *world-stopper*))
      (setf *world-stopper* self)
      (release-stw-locks)
      (return-from %stop-the-world))
    (push-wait-queue self *pending-world-stoppers*)
    ;; Leave the thread-lock locked, going to sleep.
    (release-stw-locks t)
    (setf (thread-wait-item self) *pending-world-stoppers*
          (thread-state self) :sleeping
          (thread-unsleep-helper self) #'%stop-the-world-unsleep
          (thread-unsleep-helper-argument self) nil)
    (%reschedule-via-wired-stack sp fp)))

(defun %resume-the-world (sp fp)
  (declare (ignore sp fp))
  (acquire-stw-locks)
  (cond ((not (wait-queue-empty-p *pending-world-stoppers*))
         ;; If there is another thread waiting to stop, hand over directly to them.
         (let ((thread (pop-wait-queue *pending-world-stoppers*)))
           (setf *world-stopper* thread)
           (wake-thread-1 thread)))
        (t
         ;; Wake any PA threads & resume the world.
         (setf *world-stopper* nil)
         (loop
            until (wait-queue-empty-p *pending-pseudo-atomics*)
            do
              (wake-thread-1 (pop-wait-queue *pending-pseudo-atomics*))
              (incf *pseudo-atomic-thread-count*))))
  (release-stw-locks))

(defun call-with-world-stopped (thunk)
  (let ((self (current-thread)))
    (when (eql *world-stopper* self)
      (panic "Nested world stop!"))
    (when *pseudo-atomic*
      (panic "Stopping world while pseudo-atomic!"))
    (ensure-interrupts-enabled)
    (inhibit-thread-pool-blocking-hijack
     (%call-on-wired-stack-without-interrupts #'%stop-the-world nil)
     (unwind-protect
          (progn
            (quiesce-cpus-for-world-stop)
            (funcall thunk))
       (%call-on-wired-stack-without-interrupts #'%resume-the-world nil)
       (broadcast-wakeup-ipi)))))

(defmacro with-world-stopped (&body body)
  `(call-with-world-stopped (dx-lambda () ,@body)))

(defun world-stopped-p ()
  "Returns true if the world is stopped."
  *world-stopper*)

(defun %enter-pseudo-atomic-unsleep (arg)
  (declare (ignore arg))
  (%call-on-wired-stack-without-interrupts #'%enter-pseudo-atomic nil))

(defun %enter-pseudo-atomic (sp fp)
  (acquire-stw-locks)
  (let ((self (current-thread)))
    (when (and (wait-queue-empty-p *pending-world-stoppers*)
               ;; This might be possible? If the world is stopped on
               ;; another CPU this might slip in between the other CPU
               ;; releasing locks & quiescing CPUs?
               (not *world-stopper*))
      (incf *pseudo-atomic-thread-count*)
      (release-stw-locks)
      (return-from %enter-pseudo-atomic))
    (push-wait-queue self *pending-pseudo-atomics*)
    ;; Leave the thread-lock locked, going to sleep.
    (release-stw-locks t)
    (setf (thread-wait-item self) *pending-pseudo-atomics*
          (thread-state self) :sleeping
          (thread-unsleep-helper self) #'%enter-pseudo-atomic-unsleep
          (thread-unsleep-helper-argument self) nil)
    (%reschedule-via-wired-stack sp fp)))

(defun %leave-pseudo-atomic (sp fp)
  (declare (ignore sp fp))
  (acquire-stw-locks)
  (decf *pseudo-atomic-thread-count*)
  (when (and (eql *pseudo-atomic-thread-count* 0)
             (not (wait-queue-empty-p *pending-world-stoppers*)))
    (let ((thread (pop-wait-queue *pending-world-stoppers*)))
      (setf *world-stopper* thread)
      (wake-thread-1 thread)))
  (release-stw-locks))

(defun call-with-pseudo-atomic (thunk)
  (when (eql *world-stopper* (current-thread))
    (panic "Going PA with world stopped!"))
  (ensure-interrupts-enabled)
  (inhibit-thread-pool-blocking-hijack
   (%call-on-wired-stack-without-interrupts #'%enter-pseudo-atomic nil)
   (unwind-protect
        (let ((*pseudo-atomic* t))
          (funcall thunk))
     (%call-on-wired-stack-without-interrupts #'%leave-pseudo-atomic nil))))

(defmacro with-pseudo-atomic (&body body)
  `(call-with-pseudo-atomic (dx-lambda () ,@body)))
