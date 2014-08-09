(in-package :mezzanine.supervisor)

(defvar *global-thread-lock* nil
  "This lock protects the special variables that make up the thread list and run queues.")
(defvar *thread-run-queue-head*)
(defvar *thread-run-queue-tail*)

;; FIXME: This must be per cpu.
(defvar *idle-thread*)

;; Thread object layout:
;;  0 name.
;;    The name of the thread, a string.
;;  1 state (:active :runnable :sleeping :dead)
;;    Current state.
;;      :active   - the thread is currently running on a core.
;;      :runnable - the thread can be run, but is not currently running.
;;      :sleeping - the thread is waiting for an event and cannot run.
;;      :dead     - the thread has exited or been killed and cannot run.
;;  2 lock
;;    Spinlock protecting access to the thread.
;;  3 control stack
;;    Stack object for the control stack.
;;  4 control stack pointer
;;    The thread's current RSP value. Not valid when :active or :dead. An SB64.
;;  5 binding stack
;;    Stack object for the binding stack.
;;    Base address of the binding stack, a fixnum.
;;  6 binding stack pointer
;;    The thread's current binding stack pointer RSP value. An SB64.
;;    Note! The compiler must be updated if this changes and all code rebuilt.
;;  7 preemption-disable-depth
;;    Zero when the thread can be preempted, incremented each time the thread
;;    disables preemption. A fixnum.
;;  8 preemption-pending
;;    Set when the thread should be preempted, but has a non-zero preemption-disable-depth. When p-d-d returns to 0, the thread will be preempted.
;;  9 next
;;    Forward link to the next thread in whatever list the thread is in.
;; 10 prev
;;    Backward link to the previous thread in whatever list the thread is in.
;; 32-127 MV slots
;;    Slots used as part of the multiple-value return convention.
;;    Note! The compiler must be updated if this changes and all code rebuilt.
;; 128-446 TLS slots
;;    Slots used for bound symbol values.
;;    Note! The start of this area is known by the cold-generator.
;; 447-510 FXSAVE area
;;    Unboxed area where the FPU/SSE state is saved.
;; COLD-GENERATOR::CREATE-INITIAL-THREAD must match.

(defconstant +thread-name+ 0)
(defconstant +thread-state+ 1)
(defconstant +thread-lock+ 2)
(defconstant +thread-control-stack+ 3)
(defconstant +thread-control-stack-pointer+ 4)
(defconstant +thread-binding-stack+ 5)
(defconstant +thread-binding-stack-pointer+ 6)
(defconstant +thread-preemption-disable-depth+ 7)
(defconstant +thread-preemption-pending+ 8)
(defconstant +thread-next+ 9)
(defconstant +thread-prev+ 10)
(defconstant +thread-mv-slots-start+ 32)
(defconstant +thread-mv-slots-end+ 128)
(defconstant +thread-tls-slots-start+ 128)
(defconstant +thread-tls-slots-end+ 447)
(defconstant +thread-fx-save-area+ 447)

(deftype thread ()
  `(satisfies threadp))

(defun threadp (object)
  (and (eql (sys.int::%tag-field object) sys.int::+tag-object+)
       (eql (sys.int::%object-tag object) sys.int::+object-tag-thread+)))

(defun thread-name (thread)
  "Return THREAD's name."
  (check-type thread thread)
  (sys.int::%array-like-ref-t thread +thread-name+))

(defun thread-state (thread)
  "Return THREAD's state."
  (check-type thread thread)
  (sys.int::%array-like-ref-t thread +thread-state+))

(defun (setf %thread-state) (value thread)
  "Internal function to set the state of THREAD."
  (check-type thread thread)
  (setf (sys.int::%array-like-ref-t thread +thread-state+) value))

(defun thread-control-stack (thread)
  "Return THREAD's control stack."
  (check-type thread thread)
  (sys.int::%array-like-ref-t thread +thread-control-stack+))

(defun thread-control-stack-pointer (thread)
  "Return THREAD's control stack pointer."
  (check-type thread thread)
  (sys.int::%array-like-ref-signed-byte-64 thread +thread-control-stack-pointer+))

(defun thread-binding-stack (thread)
  "Return THREAD's binding stack."
  (check-type thread thread)
  (sys.int::%array-like-ref-t thread +thread-binding-stack+))

(defun thread-binding-stack-pointer (thread)
  "Return THREAD's binding stack pointer."
  (check-type thread thread)
  (sys.int::%array-like-ref-signed-byte-64 thread +thread-binding-stack-pointer+))

(defun thread-preemption-disable-depth (thread)
  (check-type thread thread)
  (sys.int::%array-like-ref-t thread +thread-preemption-disable-depth+))

(defun (setf thread-preemption-disable-depth) (value thread)
  (check-type thread thread)
  (check-type value (and fixnum (integer 0)))
  (setf (sys.int::%array-like-ref-t thread +thread-preemption-disable-depth+) value))

(defun thread-preemption-pending (thread)
  (check-type thread thread)
  (sys.int::%array-like-ref-t thread +thread-preemption-pending+))

(defun (setf thread-preemption-pending) (value thread)
  (check-type thread thread)
  (setf (sys.int::%array-like-ref-t thread +thread-preemption-pending+) value))

(defun %thread-next (thread)
  (check-type thread thread)
  (sys.int::%array-like-ref-t thread +thread-next+))

(defun (setf %thread-next) (value thread)
  (check-type thread thread)
  (setf (sys.int::%array-like-ref-t thread +thread-next+) value))

(defun %thread-prev (thread)
  (check-type thread thread)
  (sys.int::%array-like-ref-t thread +thread-prev+))

(defun (setf %thread-prev) (value thread)
  (check-type thread thread)
  (setf (sys.int::%array-like-ref-t thread +thread-prev+) value))

(defun %lock-thread (thread)
  (check-type thread thread)
  (let ((current-thread (current-thread)))
    (do ()
        ((sys.int::%cas-array-like thread
                                   +thread-lock+
                                   :unlocked
                                   current-thread))
      (sys.int::cpu-relax))))

(defun %unlock-thread (thread)
  (assert (eql (sys.int::%array-like-ref-t thread +thread-lock+)
               (current-thread)))
  (setf (sys.int::%array-like-ref-t thread +thread-lock+) :unlocked))

(defun current-thread ()
  "Returns the thread object for the calling thread."
  (sys.int::%%assemble-value (sys.int::msr sys.int::+msr-ia32-gs-base+) 0))

(defun %make-thread (function &key name initial-bindings control-stack-size binding-stack-size)
  (check-type function (or function symbol))
  ;; Defer the GC until the thread object is created. Scanning a partially initialized thread
  ;; is bad news.
  (with-gc-deferred
    (let* ((thread (%allocate-object sys.int::+object-tag-thread+ 0 511 :wired))
           (control-stack (%allocate-stack :control control-stack-size))
           (binding-stack (%allocate-stack :binding binding-stack-size)))
      (setf (sys.int::%array-like-ref-t thread +thread-name+) name
            (sys.int::%array-like-ref-t thread +thread-state+) :runnable
            (sys.int::%array-like-ref-t thread +thread-lock+) :unlocked
            (sys.int::%array-like-ref-t thread +thread-control-stack+) control-stack
            (sys.int::%array-like-ref-t thread +thread-binding-stack+) binding-stack
            (sys.int::%array-like-ref-t thread +thread-preemption-disable-depth+) 0
            (sys.int::%array-like-ref-t thread +thread-preemption-pending+) nil)
      ;; Clear the binding stack.
      (dotimes (i (truncate (stack-size binding-stack) 8))
        (setf (sys.int::memref-unsigned-byte-64 (stack-base binding-stack) i) 0))
      (setf (sys.int::%array-like-ref-signed-byte-64 thread +thread-binding-stack-pointer+)
            (+ (stack-base binding-stack) (stack-size binding-stack)))
      ;; Reset TLS slots.
      (dotimes (i (- +thread-tls-slots-end+ +thread-tls-slots-start+))
        (setf (sys.int::%array-like-ref-t thread (+ +thread-tls-slots-start+ i))
              (sys.int::%unbound-tls-slot)))
      ;; Perform initial bindings.
      (loop for (symbol . value) in initial-bindings do
           (let ((slot (or (sys.int::symbol-tls-slot symbol)
                           (sys.int::%allocate-tls-slot symbol))))
             (setf (sys.int::%array-like-ref-t thread slot) value)))
      ;; Initialize the FXSAVE area.
      ;; All FPU/SSE interrupts masked, round to nearest,
      ;; x87 using 80 bit precision (long-float).
      (dotimes (i 64)
        (setf (sys.int::%array-like-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ i)) 0))
      (setf (ldb (byte 16 0) (sys.int::%array-like-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ 0)))
            #x037F) ; FCW
      (setf (ldb (byte 32 0) (sys.int::%array-like-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ 3)))
            #x00001F80) ; MXCSR
      ;; Push initial state on the control stack.
      ;; This must match the frame %%switch-to-thread expects.
      (let ((pointer (+ (stack-base control-stack) (stack-size control-stack))))
        (flet ((push-t (value)
                 (setf (sys.int::memref-t (decf pointer 8) 0) value))
               (push-ub64 (value)
                 (setf (sys.int::memref-unsigned-byte-64 (decf pointer 8) 0) value)))
          ;; Function to run.
          (push-t function)
          ;; Saved RIP for %%switch-to-thread.
          (push-ub64 (sys.int::%array-like-ref-unsigned-byte-64 #'%%thread-entry-trampoline 0))
          ;; Saved frame pointer.
          (push-ub64 0))
        ;; Update the control stack pointer.
        (setf (sys.int::%array-like-ref-unsigned-byte-64 thread +thread-control-stack-pointer+)
              pointer))
      thread)))

(defun make-thread (function &key name initial-bindings (control-stack-size (* 256 1024)) (binding-stack-size (* 8 1024)))
  (let ((thread (%make-thread function
                              :name name
                              :initial-bindings initial-bindings
                              :control-stack-size control-stack-size
                              :binding-stack-size binding-stack-size)))
    (with-spinlock (*global-thread-lock*)
      ;; Attach to the run-queue.
      (cond ((null *thread-run-queue-head*)
             (setf *thread-run-queue-head* thread
                   *thread-run-queue-tail* thread)
             (setf (%thread-next thread) nil
                   (%thread-prev thread) nil))
            (t
             (setf (%thread-next *thread-run-queue-tail*) thread
                   (%thread-prev thread) *thread-run-queue-tail*
                   *thread-run-queue-tail* thread))))
    thread))

(defun thread-entry-trampoline (function)
  (sys.int::%sti)
  (unwind-protect
       (funcall function)
    (without-interrupts
      (let ((self (current-thread)))
        (%lock-thread self)
        (setf (%thread-state self) :dead)
        (%reschedule)))))

(sys.int::define-lap-function %%thread-entry-trampoline ()
  (:gc :no-frame :layout #*1)
  ;; The binding stack is empty.
  ;; The regular stack contains the function to call.
  (sys.lap-x86:pop :r8)
  (:gc :no-frame)
  ;; Call the high-level trampoline function..
  (sys.lap-x86:mov64 :r13 (:function thread-entry-trampoline))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2))

;; The idle thread is not a true thread. It does not appear in all-threads, nor in any run-queue.
;; When the machine boots, one idle thread is created for each core. When a core is idle, the
;; idle thread will be run.
(defun idle-thread ()
  (without-interrupts
    (loop
       (sys.int::%stihlt)
       (sys.int::%cli))))

(defun initialize-threads ()
  (when (not (boundp '*global-thread-lock*))
    ;; First-run stuff.
    ;; FIXME: Patch up initial thread's stack objects.
    (setf *global-thread-lock* :unlocked)
    (setf *thread-run-queue-head* nil)
    (setf *thread-run-queue-tail* nil))
  (setf *idle-thread* (%make-thread #'idle-thread
                                    :name "Initial Idle Thread"
                                    :control-stack-size (* 4 1024)
                                    :binding-stack-size 256)))

(defun initialize-initial-thread ()
  "Called very early after boot to reset the initial thread."
  (let* ((thread (current-thread)))
    (setf (%thread-state thread) :active)
    (setf (thread-preemption-disable-depth thread) 1)))

(defun pick-next-thread ()
  (when *thread-run-queue-head*
    ;; Pop thread from run-queue.
    (prog1 *thread-run-queue-head*
      (when (%thread-next *thread-run-queue-head*)
        (setf (%thread-prev (%thread-next *thread-run-queue-head*)) nil))
      (setf *thread-run-queue-head* (%thread-next *thread-run-queue-head*)))))

(defun thread-yield ()
  (without-interrupts
    (%lock-thread (current-thread))
    (setf (thread-state current) :runnable)
    (%reschedule)))

(defun %reschedule ()
  ;; Interrupts must be off and the current thread's lock must be held.
  ;; Releases the thread lock.
  (let ((current (current-thread))
        next)
    ;; Return the current thread to the run queue and fetch the next thread.
    (with-spinlock (*global-thread-lock*)
      (when (and (eql (thread-state current) :runnable)
                 (not (eql current *idle-thread*)))
        (cond ((null *thread-run-queue-head*)
               (setf *thread-run-queue-head* thread
                     *thread-run-queue-tail* thread)
               (setf (%thread-next thread) nil
                     (%thread-prev thread) nil))
              (t
               (setf (%thread-next *thread-run-queue-tail*) thread
                     (%thread-prev thread) *thread-run-queue-tail*
                     *thread-run-queue-tail* thread))))
      (setf next (or (pick-next-thread)
                     *idle-thread*)))
    ;; todo: reset preemption timer here.
    (when (eql next current)
      ;; Staying on the same thread, unlock and return.
      (%unlock-thread current)
      (return-from %reschedule))
    (%lock-thread next)
    (setf (%thread-state next) :active)
    (%%switch-to-thread current next)))

(sys.int::define-lap-function %%switch-to-thread ()
  (:gc :no-frame)
  ;; Save frame pointer.
  (sys.lap-x86:push :rbp)
  (:gc :frame)
  ;; Save fpu state.
  (sys.lap-x86:gs)
  (sys.lap-x86:fxsave (:object nil #.+thread-fx-save-area+))
  ;; Save control stack pointer.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:object nil #.+thread-control-stack-pointer+) :rsp)
  ;; Switch threads.
  (sys.lap-x86:mov32 :ecx #.sys.int::+msr-ia32-gs-base+)
  (sys.lap-x86:mov64 :rax :r9)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  (sys.lap-x86:wrmsr)
  ;; Restore control stack pointer.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 :rsp (:object nil #.+thread-control-stack-pointer+))
  ;; Restore fpu state.
  (sys.lap-x86:gs)
  (sys.lap-x86:fxrstor (:object nil #.+thread-fx-save-area+))
  ;; Drop the locks on both threads.
  (sys.lap-x86:mov64 :r10 (:constant :unlocked))
  (sys.lap-x86:mov64 (:object :r9 #.+thread-lock+) :r10)
  (sys.lap-x86:mov64 (:object :r8 #.+thread-lock+) :r10)
  ;; Restore frame pointer & rip.
  (sys.lap-x86:pop :rbp)
  (sys.lap-x86:ret))

(defun finish-initial-thread ()
  "Called when the boot code is done with the initial thread."
  ;; The initial thread never dies, it just sleeps until the next boot.
  ;; The bootloader will partially wake it up, then initialize-initial-thread
  ;; will finish initialization.
  ;; It's not a true thread, and doesn't appear in the all-threads list, and
  ;; it is never preempted, so will never appear on any run-queue.
  ;; It is kept live by the sys.int::*initial-thread* symbol.
  ;; The initial thread must finish with no values on the binding stack, and
  ;; all TLS slots initialized. This is required by INITIALIZE-INITIAL-THREAD.
  (let ((thread (current-thread)))
    (without-interrupts
      (%lock-thread thread)
      (setf (%thread-state thread) :sleeping)
      (%reschedule)))
  (error "Initial thread woken??"))
