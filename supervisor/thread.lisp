(in-package :mezzanine.supervisor)

(defvar *global-thread-lock* nil
  "This lock protects the special variables that make up the thread list and run queues.")
(defvar *thread-run-queue-head*)
(defvar *thread-run-queue-tail*)
(defvar *all-threads*)

(defvar *world-stop-lock*)
(defvar *world-stop-resume-cvar*)
(defvar *world-stop-pa-exit-cvar*)
(defvar *world-stop-pending*)
(defvar *world-stopper*)
(defvar *pseudo-atomic-thread-count*)

(defvar *pseudo-atomic* nil)

;; FIXME: There must be one idle thread per cpu.
;; The cold-generator creates an idle thread for the BSP.
(defvar sys.int::*bsp-idle-thread*)

;; Thread object layout:
;;  0 name.
;;    The name of the thread, a string.
;;  1 state (:active :runnable :sleeping :dead)
;;    Current state.
;;      :active   - the thread is currently running on a core.
;;      :runnable - the thread can be run, but is not currently running.
;;      :sleeping - the thread is waiting for an event and cannot run.
;;      :dead     - the thread has exited or been killed and cannot run.
;;      :waiting-for-page - the thread is waiting for memory to be paged in.
;;  2 lock
;;    Spinlock protecting access to the thread.
;;  3 stack
;;    Stack object for the stack.
;;  4 stack pointer
;;    The thread's current RSP value. Not valid when :active or :dead. An SB64.
;;  5 Wait item.
;;    If a thread is sleeping or waiting for page, this will describe what it's waiting for.
;;    When waiting for paging to complete, this will be the faulting address.
;;  6 special stack pointer
;;    The thread's current special stack pointer.
;;    Note! The compiler must be updated if this changes and all code rebuilt.
;;  7 preemption-disable-depth
;;    Zero when the thread can be preempted, incremented each time the thread
;;    disables preemption. A fixnum.
;;    This must only be modified by the thread.
;;  8 preemption-pending
;;    Set when the thread should be preempted, but has a non-zero preemption-disable-depth. When p-d-d returns to 0, the thread will be preempted.
;;  9 %next
;;    Forward link to the next thread in whatever list the thread is in.
;; 10 %prev
;;    Backward link to the previous thread in whatever list the thread is in.
;; 11 foothold-disable-depth
;;    Zero when ESTABLISH-THREAD-FOOTHOLD may break into the thread.
;;    DESTROY-THREAD will also be unable to destroy the thread unless the abort option is set.
;; 12 frame pointer
;;    The thread's current RBP value. Not valid when :active or :dead. An SB64.
;;    Here rather than on the stack to avoid GC problems during thread switch.
;; 13 mutex stack
;; 14 global-next
;; 15 global-prev
;;    Linked list of all living threads.
;; 32-127 MV slots
;;    Slots used as part of the multiple-value return convention.
;;    Note! The compiler must be updated if this changes and all code rebuilt.
;; 128-426 TLS slots
;;    Slots used for bound symbol values.
;;    Note! The start of this area is known by the cold-generator.
;; 427-446 Interrupt save area
;;    Used to save an interrupt frame when the thread has stopped to wait for a page.
;;    The registers are saved here, not on the stack, because the stack may not be paged in.
;; 447-510 FXSAVE area
;;    Unboxed area where the FPU/SSE state is saved.
;; COLD-GENERATOR::CREATE-INITIAL-THREAD must match.

(deftype thread ()
  `(satisfies threadp))

(defun threadp (object)
  (and (eql (sys.int::%tag-field object) sys.int::+tag-object+)
       (eql (sys.int::%object-tag object) sys.int::+object-tag-thread+)))

(macrolet ((field (name offset &key (type 't) (accessor 'sys.int::%array-like-ref-t))
             (let ((field-name (intern (format nil "+THREAD-~A+" (symbol-name name))
                                       (symbol-package name)))
                   (accessor-name (intern (format nil "THREAD-~A" (symbol-name name))
                                          (symbol-package name))))
               `(progn
                  (defconstant ,field-name ,offset)
                  (defun ,accessor-name (thread)
                    (check-type thread thread)
                    (,accessor thread ,field-name))
                  (defun (setf ,accessor-name) (value thread)
                    (check-type thread thread)
                    ,@(when (not (eql type 't))
                        `((check-type value ,type)))
                    (setf (,accessor thread ,field-name) value))))))
  (field name                     0)
  (field state                    1 :type (member :active :runnable :sleeping :dead :waiting-for-page))
  (field lock                     2)
  (field stack                    3)
  (field stack-pointer            4 :accessor sys.int::%array-like-ref-signed-byte-64)
  (field wait-item                5)
  (field special-stack-pointer    6)
  (field preemption-disable-depth 7)
  (field preemption-pending       8)
  (field %next                    9)
  (field %prev                   10)
  (field foothold-disable-depth  11)
  (field frame-pointer           12 :accessor sys.int::%array-like-ref-signed-byte-64)
  (field mutex-stack             13)
  (field global-next             14)
  (field global-prev             15))

(defconstant +thread-mv-slots-start+ 32)
(defconstant +thread-mv-slots-end+ 128)
(defconstant +thread-tls-slots-start+ 128)
(defconstant +thread-tls-slots-end+ 427)
(defconstant +thread-interrupt-save-area+ 427)
(defconstant +thread-fx-save-area+ 447)

(defun thread-footholds-enabled-p (thread)
  (zerop (thread-foothold-disable-depth thread)))

(defmacro with-footholds-inhibited (&body body)
  "Inhibit thread footholds within body."
  `(call-with-footholds-inhibited
    (flet ((%%with-footholds-inhibited-thunk%% ()
             ,@body))
      (declare (dynamic-extent #'%%with-footholds-inhibited-thunk%%))
      #'%%with-footholds-inhibited-thunk%%)))

(defmacro with-footholds-permitted (&body body)
  "Permit thread footholds within body.
Must only appear within the dynamic extent of a WITH-FOOTHOLDS-INHIBITED form."
  `(call-with-footholds-permitted
    (flet ((%%with-footholds-permitted-thunk%% ()
             ,@body))
      (declare (dynamic-extent #'%%with-footholds-permitted-thunk%%))
      #'%%with-footholds-permitted-thunk%%)))

(defun call-with-footholds-inhibited (thunk)
  ;; Footholds are active at this point, so another thread may
  ;; be able to establish a foothold which unwinds after the unwind-protect
  ;; begins, but before with-thread-lock takes the thread lock.
  ;; Use a variable to prevent decrementing the disable depth if this occurs.
  (let ((self (current-thread))
        (have-incremented nil))
    (unwind-protect
         (progn
           (with-thread-lock (self)
             (incf (thread-foothold-disable-depth self))
             (setf have-incremented t))
           (funcall thunk))
      (when have-incremented
        (with-thread-lock (self)
          (decf (thread-foothold-disable-depth self)))
        (when (zerop (thread-foothold-disable-depth self))
          (establish-deferred-footholds self))))))

(defun call-with-footholds-permitted (thunk)
  (let ((self (current-thread)))
    ;; Footholds are initially inhibited, no need for a protection variable.
    (unwind-protect
         (progn
           (with-thread-lock (self)
             (assert (not (zerop (thread-foothold-disable-depth self))))
             (decf (thread-foothold-disable-depth self)))
           ;; Establish any deferred footholds.
           (when (zerop (thread-foothold-disable-depth self))
             (establish-deferred-footholds self))
           (funcall thunk))
      (with-thread-lock (self)
        (incf (thread-foothold-disable-depth self))))))

(defmacro with-thread-lock ((thread) &body body)
  (let ((sym (gensym "thread")))
    `(let ((,sym ,thread))
       (without-interrupts
         (unwind-protect
              (progn
                (%lock-thread ,sym)
                ,@body)
           (%unlock-thread ,sym))))))

(defun %lock-thread (thread)
  (check-type thread thread)
  (let ((current-thread (current-thread)))
    (do ()
        ((sys.int::%cas-array-like thread
                                   +thread-lock+
                                   :unlocked
                                   current-thread))
      (panic "thread lock " thread " held by " (sys.int::%array-like-ref-t thread +thread-lock+))
      (sys.int::cpu-relax))))

(defun %unlock-thread (thread)
  (assert (eql (sys.int::%array-like-ref-t thread +thread-lock+)
               (current-thread)))
  (setf (sys.int::%array-like-ref-t thread +thread-lock+) :unlocked))

(defun push-run-queue (thread)
  (when (or (eql thread *world-stopper*)
            (eql thread sys.int::*pager-thread*)
            (eql thread sys.int::*disk-io-thread*))
    (return-from push-run-queue))
  (cond ((null *thread-run-queue-head*)
         (setf *thread-run-queue-head* thread
               *thread-run-queue-tail* thread)
         (setf (thread-%next thread) nil
               (thread-%prev thread) nil))
        (t
         (setf (thread-%next *thread-run-queue-tail*) thread
               (thread-%prev thread) *thread-run-queue-tail*
               (thread-%next thread) nil
               *thread-run-queue-tail* thread))))

(defun pop-run-queue ()
  (when *thread-run-queue-head*
    (prog1 *thread-run-queue-head*
      (cond ((thread-%next *thread-run-queue-head*)
             (setf (thread-%prev (thread-%next *thread-run-queue-head*)) nil)
             (setf *thread-run-queue-head* (thread-%next *thread-run-queue-head*)))
            (t (setf *thread-run-queue-head* nil
                     *thread-run-queue-tail* nil))))))

(defun current-thread ()
  "Returns the thread object for the calling thread."
  (sys.int::%%assemble-value (sys.int::msr sys.int::+msr-ia32-gs-base+) 0))

(defun make-thread (function &key name initial-bindings (stack-size (* 256 1024)))
  (check-type function (or function symbol))
  ;; FIXME: need to make te GC aware of partially initialized threads.
  (let* ((thread (mezzanine.runtime::%allocate-object sys.int::+object-tag-thread+ 0 511 :wired))
         (stack (%allocate-stack stack-size)))
    (setf (sys.int::%array-like-ref-t thread +thread-name+) name
          (sys.int::%array-like-ref-t thread +thread-state+) :runnable
          (sys.int::%array-like-ref-t thread +thread-lock+) :unlocked
          (sys.int::%array-like-ref-t thread +thread-stack+) stack
          (sys.int::%array-like-ref-t thread +thread-special-stack-pointer+) nil
          (sys.int::%array-like-ref-t thread +thread-wait-item+) nil
          (sys.int::%array-like-ref-t thread +thread-preemption-disable-depth+) 0
          (sys.int::%array-like-ref-t thread +thread-preemption-pending+) nil
          ;; Decremented by the trampoline when it calls the thread function.
          (sys.int::%array-like-ref-t thread +thread-foothold-disable-depth+) 1
          (sys.int::%array-like-ref-t thread +thread-mutex-stack+) nil)
    ;; Reset TLS slots.
    (dotimes (i (- +thread-tls-slots-end+ +thread-tls-slots-start+))
      (setf (sys.int::%array-like-ref-t thread (+ +thread-tls-slots-start+ i))
            (sys.int::%unbound-tls-slot)))
    ;; Perform initial bindings.
    (loop for (symbol value) in initial-bindings do
         (let ((slot (or (sys.int::symbol-tls-slot symbol)
                         (sys.int::%allocate-tls-slot symbol))))
           (setf (sys.int::%array-like-ref-t thread (1- slot)) value)))
    ;; Initialize the FXSAVE area.
    ;; All FPU/SSE interrupts masked, round to nearest,
    ;; x87 using 80 bit precision (long-float).
    (dotimes (i 64)
      (setf (sys.int::%array-like-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ i)) 0))
    (setf (ldb (byte 16 0) (sys.int::%array-like-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ 0)))
          #x037F) ; FCW
    (setf (ldb (byte 32 0) (sys.int::%array-like-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ 3)))
          #x00001F80) ; MXCSR
    ;; Push initial state on the stack.
    ;; This must match the frame %%switch-to-thread expects.
    (let ((pointer (+ (stack-base stack) (stack-size stack))))
      (flet ((push-t (value)
               (setf (sys.int::memref-t (decf pointer 8) 0) value))
             (push-ub64 (value)
               (setf (sys.int::memref-unsigned-byte-64 (decf pointer 8) 0) value)))
        ;; Function to run.
        (push-t function)
        ;; Saved RIP for %%switch-to-thread.
        (push-ub64 (sys.int::%array-like-ref-unsigned-byte-64 #'%%thread-entry-trampoline 0)))
      ;; Update the control stack pointer.
      (setf (sys.int::%array-like-ref-unsigned-byte-64 thread +thread-stack-pointer+)
            pointer)
      (setf (sys.int::%array-like-ref-unsigned-byte-64 thread +thread-frame-pointer+) 0))
    (with-symbol-spinlock (*global-thread-lock*)
      (push-run-queue thread)
      ;; Add thread to global thread list.
      (setf (thread-global-prev *all-threads*) thread
            (thread-global-next thread) *all-threads*
            (thread-global-prev thread) nil
            *all-threads* thread))
    thread))

(defun thread-entry-trampoline (function)
  (let ((self (current-thread)))
    (unwind-protect
         (catch 'terminate-thread
           (with-footholds-permitted
             (funcall function)))
      ;; Cleanup, terminate the thread.
      (sys.int::%cli)
      (%lock-thread self)
      (setf (thread-state self) :dead)
      ;; Remove thread from the global list.
      (with-symbol-spinlock (*global-thread-lock*)
        (when (thread-global-next self)
          (setf (thread-global-prev (thread-global-next self)) (thread-global-prev self)))
        (when (thread-global-prev self)
          (setf (thread-global-next (thread-global-prev self)) (thread-global-next self)))
        (when (eql self *all-threads*)
          (setf *all-threads* (thread-global-next self))))
      (%reschedule))))

(sys.int::define-lap-function %%thread-entry-trampoline ()
  (:gc :no-frame :layout #*1)
  ;; The regular stack contains the function to call.
  (sys.lap-x86:pop :r8)
  (:gc :no-frame)
  ;; Call the high-level trampoline function.
  (sys.lap-x86:mov64 :r13 (:function thread-entry-trampoline))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2))

;; The idle thread is not a true thread. It does not appear in all-threads, nor in any run-queue.
;; When the machine boots, one idle thread is created for each core. When a core is idle, the
;; idle thread will be run.
;; FIXME: SMP-safety.
(defun idle-thread ()
  (loop
     (sys.int::%cli)
     ;; Look for a thread to switch to.
     (let ((next (with-symbol-spinlock (*global-thread-lock*)
                   (cond ((eql (thread-state sys.int::*pager-thread*) :runnable)
                          sys.int::*pager-thread*)
                         ((eql (thread-state sys.int::*disk-io-thread*) :runnable)
                          sys.int::*disk-io-thread*)
                         (*world-stopper*
                          (when (eql (thread-state *world-stopper*) :runnable)
                            *world-stopper*))
                         (t (pop-run-queue))))))
       (cond (next
              (set-run-light t)
              ;; Switch to thread.
              (%lock-thread sys.int::*bsp-idle-thread*)
              (%lock-thread next)
              (setf (thread-state next) :active)
              (%%switch-to-thread sys.int::*bsp-idle-thread* next)
              (set-run-light nil))
             (t ;; Wait for an interrupt.
              (sys.int::%stihlt))))))

(sys.int::define-lap-function %%simple-thread-entry-trampoline ()
  (:gc :no-frame :layout #*1)
  ;; The regular stack contains the function to call.
  (sys.lap-x86:pop :rbx)
  (:gc :no-frame)
  (sys.lap-x86:xor32 :ecx :ecx) ; fixnum 0
  (sys.lap-x86:call (:object :rbx 0))
  (sys.lap-x86:ud2))

(defun reset-ephemeral-thread (thread entry-point state)
  (let ((rsp (+ (stack-base (thread-stack thread))
                (stack-size (thread-stack thread)))))
    ;; Function to call.
    (setf (sys.int::memref-t (decf rsp 8) 0) entry-point)
    ;; Trampoline for switch threads.
    (setf (sys.int::memref-signed-byte-64 (decf rsp 8) 0) (sys.int::%array-like-ref-signed-byte-64 #'%%simple-thread-entry-trampoline 0))
    (setf (thread-stack-pointer thread) rsp
          (thread-frame-pointer thread) 0))
  (setf (thread-state thread) state
        (sys.int::%array-like-ref-t thread +thread-special-stack-pointer+) nil
        (sys.int::%array-like-ref-t thread +thread-wait-item+) nil
        (sys.int::%array-like-ref-t thread +thread-preemption-disable-depth+) 1
        (sys.int::%array-like-ref-t thread +thread-foothold-disable-depth+) 1
        (sys.int::%array-like-ref-t thread +thread-mutex-stack+) nil)
  ;; Initialize the FXSAVE area.
  ;; All FPU/SSE interrupts masked, round to nearest,
  ;; x87 using 80 bit precision (long-float).
  (dotimes (i 64)
    (setf (sys.int::%array-like-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ i)) 0))
  (setf (ldb (byte 16 0) (sys.int::%array-like-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ 0)))
        #x037F) ; FCW
  (setf (ldb (byte 32 0) (sys.int::%array-like-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ 3)))
        #x00001F80) ; MXCSR
  ;; Reset TLS slots.
  (dotimes (i (- +thread-tls-slots-end+ +thread-tls-slots-start+))
    (setf (sys.int::%array-like-ref-t thread (+ +thread-tls-slots-start+ i))
          (sys.int::%unbound-tls-slot))))

(defun initialize-threads ()
  (when (not (boundp '*global-thread-lock*))
    ;; First-run stuff.
    (setf *global-thread-lock* :unlocked)
    (setf *thread-run-queue-head* nil
          *thread-run-queue-tail* nil)
    (setf *world-stop-lock* (make-mutex "World stop lock" :spin)
          *world-stop-resume-cvar* (make-condition-variable "World resume cvar")
          *world-stop-pa-exit-cvar* (make-condition-variable "World stop PA exit cvar")
          *world-stop-pending* nil
          *pseudo-atomic-thread-count* 0)
    (setf *all-threads* sys.int::*snapshot-thread*
          (thread-global-next sys.int::*snapshot-thread*) sys.int::*pager-thread*
          (thread-global-prev sys.int::*snapshot-thread*) nil
          (thread-global-next sys.int::*pager-thread*) sys.int::*disk-io-thread*
          (thread-global-prev sys.int::*pager-thread*) sys.int::*snapshot-thread*
          (thread-global-next sys.int::*disk-io-thread*) nil
          (thread-global-prev sys.int::*disk-io-thread*) sys.int::*pager-thread*))
  (reset-ephemeral-thread sys.int::*bsp-idle-thread* #'idle-thread :sleeping)
  (reset-ephemeral-thread sys.int::*snapshot-thread* #'snapshot-thread :sleeping)
  (reset-ephemeral-thread sys.int::*pager-thread* #'pager-thread :runnable)
  (reset-ephemeral-thread sys.int::*disk-io-thread* #'disk-thread :runnable)
  (condition-notify *world-stop-resume-cvar*))

(defun thread-yield ()
  (let ((current (current-thread)))
    (sys.int::%cli)
    (%lock-thread current)
    (setf (thread-state current) :runnable)
    (%reschedule)))

(defun %update-run-queue ()
  "Possibly return the current thread to the run queue, and
return the next thread to run.
Interrupts must be off, the current thread must be locked."
  (let ((current (current-thread)))
    (with-symbol-spinlock (*global-thread-lock*)
      (cond (*world-stopper*
             ;; World is stopped, the only runnable threads are
             ;; the pager, the disk io thread, the idle thread and the world stopper.
             (unless (or (eql current *world-stopper*)
                         (eql current sys.int::*pager-thread*)
                         (eql current sys.int::*disk-io-thread*))
               (panic "Aiee. %UPDATE-RUN-QUEUE called with bad thread " current))
             (cond ((eql (thread-state sys.int::*pager-thread*) :runnable)
                    ;; Pager is ready to run.
                    sys.int::*pager-thread*)
                   ((eql (thread-state sys.int::*disk-io-thread*) :runnable)
                    ;; Disk IO is ready to run.
                    sys.int::*disk-io-thread*)
                   ((eql (thread-state *world-stopper*) :runnable)
                    ;; The world stopper is ready.
                    *world-stopper*)
                   (t ;; Switch to idle.
                    sys.int::*bsp-idle-thread*)))
            (t ;; Return the current thread to the run queue and fetch the next thread.
             (when (eql current sys.int::*bsp-idle-thread*)
               (panic "Aiee. Idle thread called %UPDATE-RUN-QUEUE."))
             (when (eql (thread-state current) :runnable)
               (push-run-queue current))
             (or (when (eql (thread-state sys.int::*pager-thread*) :runnable)
                   ;; Pager is ready to run.
                   sys.int::*pager-thread*)
                 (when (eql (thread-state sys.int::*disk-io-thread*) :runnable)
                   ;; Disk IO is ready to run.
                   sys.int::*disk-io-thread*)
                 ;; Try taking from the run queue.
                 (pop-run-queue)
                 ;; Fall back on idle.
                 sys.int::*bsp-idle-thread*))))))

(defun %reschedule ()
  ;; Interrupts must be off and the current thread's lock must be held.
  ;; Releases the thread lock and reenables interrupts.
  (let ((current (current-thread))
        (next (%update-run-queue)))
    ;; todo: reset preemption timer here.
    (when (eql next current)
      ;; Staying on the same thread, unlock and return.
      (%unlock-thread current)
      (sys.int::%sti)
      (return-from %reschedule))
    (when (<= sys.int::*exception-stack-base*
              (thread-stack-pointer next)
              (1- sys.int::*exception-stack-size*))
      (panic "Other thread " next " stopped on exception stack!!!"))
    (%lock-thread next)
    (setf (thread-state next) :active)
    (%%switch-to-thread current next)))

;;; Switch to a new thread. Takes the current thread and the new thread as arguments.
;;; Watch out. The GC grovels around in the stack of not-running threads, if the
;;; layout changes, it must be updated.
(sys.int::define-lap-function %%switch-to-thread ()
  (:gc :no-frame)
  ;; Save frame pointer.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:object nil #.+thread-frame-pointer+) :rbp)
  ;; Save fpu state.
  (sys.lap-x86:gs)
  (sys.lap-x86:fxsave (:object nil #.+thread-fx-save-area+))
  ;; Save stack pointer.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:object nil #.+thread-stack-pointer+) :rsp)
  ;; Switch threads.
  (sys.lap-x86:mov32 :ecx #.sys.int::+msr-ia32-gs-base+)
  (sys.lap-x86:mov64 :rax :r9)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  (sys.lap-x86:wrmsr)
  ;; Restore stack pointer.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 :rsp (:object nil #.+thread-stack-pointer+))
  ;; Restore fpu state.
  (sys.lap-x86:gs)
  (sys.lap-x86:fxrstor (:object nil #.+thread-fx-save-area+))
  ;; Drop the locks on both threads. Must be done before touching the thread stack.
  (sys.lap-x86:mov64 :r10 (:constant :unlocked))
  (sys.lap-x86:mov64 (:object :r9 #.+thread-lock+) :r10)
  (sys.lap-x86:mov64 (:object :r8 #.+thread-lock+) :r10)
  ;; Check if the thread is in the interrupt save area.
  (sys.lap-x86:lea64 :rax (:object :r9 #.+thread-interrupt-save-area+))
  (sys.lap-x86:cmp64 :rax :rsp)
  (sys.lap-x86:je INTERRUPT-RESUME)
  ;; Restore frame pointer.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 :rbp (:object nil #.+thread-frame-pointer+))
  ;; Reenable interrupts. Must be done before touching the thread stack.
  (sys.lap-x86:sti)
  ;; No value return.
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:mov64 :r8 nil)
  ;; Return, restoring RIP.
  (sys.lap-x86:ret)
  ;; Returning to an interrupted thread. Restore saved registers and stuff.
  INTERRUPT-RESUME
  (:gc :frame :interrupt t)
  (sys.lap-x86:pop :r15)
  (sys.lap-x86:pop :r14)
  (sys.lap-x86:pop :r13)
  (sys.lap-x86:pop :r12)
  (sys.lap-x86:pop :r11)
  (sys.lap-x86:pop :r10)
  (sys.lap-x86:pop :r9)
  (sys.lap-x86:pop :r8)
  (sys.lap-x86:pop :rdi)
  (sys.lap-x86:pop :rsi)
  (sys.lap-x86:pop :rbx)
  (sys.lap-x86:pop :rdx)
  (sys.lap-x86:pop :rcx)
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:pop :rbp)
  (sys.lap-x86:iret))

(defun %reschedule-via-interrupt (interrupt-frame)
  ;; Interrupts must be off and the current thread's lock must be held.
  ;; Releases the thread lock and reenables interrupts.
  (let ((current (current-thread))
        (next (%update-run-queue)))
    ;; todo: reset preemption timer here.
    (when (eql next current)
      ;; Can't do this.
      (panic "Bad! Returning to interrupted thread through %reschedule-via-interrupt"))
    (%lock-thread next)
    (setf (thread-state next) :active)
    (%%switch-to-thread-via-interrupt current interrupt-frame next)))

(defun wake-thread (thread)
  "Wake a sleeping thread."
  (with-thread-lock (thread)
    (with-symbol-spinlock (*global-thread-lock*)
      (setf (thread-state thread) :runnable)
      (push-run-queue thread))))

;;; current-thread interrupt-frame next-thread
;;; Interrupts must be off, current & next must be locked.
(sys.int::define-lap-function %%switch-to-thread-via-interrupt ()
  (:gc :no-frame)
  ;; Save fpu state.
  (sys.lap-x86:gs)
  (sys.lap-x86:fxsave (:object nil #.+thread-fx-save-area+))
  ;; Copy the interrupt frame over to the save area.
  (sys.lap-x86:mov64 :rsi (:object :r9 0))
  (sys.lap-x86:sar64 :rsi #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:sub64 :rsi #.(* 14 8)) ; 14 registers below the pointer, 6 above.
  (sys.lap-x86:lea64 :rdi (:object :r8 #.+thread-interrupt-save-area+))
  (sys.lap-x86:mov32 :ecx 20) ; 20 values to copy.
  (sys.lap-x86:rep)
  (sys.lap-x86:movs64)
  ;; Save stack pointer.
  (sys.lap-x86:lea64 :rax (:object :r8 #.+thread-interrupt-save-area+))
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:object nil #.+thread-stack-pointer+) :rax)
  ;; Switch threads.
  (sys.lap-x86:mov32 :ecx #.sys.int::+msr-ia32-gs-base+)
  (sys.lap-x86:mov64 :rax :r10)
  (sys.lap-x86:mov64 :rdx :r10)
  (sys.lap-x86:shr64 :rdx 32)
  (sys.lap-x86:wrmsr)
  ;; Restore stack pointer.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 :rsp (:object nil #.+thread-stack-pointer+))
  ;; Restore fpu state.
  (sys.lap-x86:gs)
  (sys.lap-x86:fxrstor (:object nil #.+thread-fx-save-area+))
  ;; Drop the locks on both threads. Must be done before touching the thread stack.
  (sys.lap-x86:mov64 :r11 (:constant :unlocked))
  (sys.lap-x86:mov64 (:object :r10 #.+thread-lock+) :r11)
  (sys.lap-x86:mov64 (:object :r8 #.+thread-lock+) :r11)
  ;; Check if the thread is in the interrupt save area.
  (sys.lap-x86:lea64 :rax (:object :r10 #.+thread-interrupt-save-area+))
  (sys.lap-x86:cmp64 :rax :rsp)
  (sys.lap-x86:je INTERRUPT-RESUME)
  ;; Restore frame pointer.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 :rbp (:object nil #.+thread-frame-pointer+))
  ;; Reenable interrupts. Must be done before touching the thread stack.
  (sys.lap-x86:sti)
  ;; No value return.
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:mov64 :r8 nil)
  ;; Return, restoring RIP.
  (sys.lap-x86:ret)
  ;; Returning to an interrupted thread. Restore saved registers and stuff.
  INTERRUPT-RESUME
  (:gc :frame :interrupt t)
  (sys.lap-x86:pop :r15)
  (sys.lap-x86:pop :r14)
  (sys.lap-x86:pop :r13)
  (sys.lap-x86:pop :r12)
  (sys.lap-x86:pop :r11)
  (sys.lap-x86:pop :r10)
  (sys.lap-x86:pop :r9)
  (sys.lap-x86:pop :r8)
  (sys.lap-x86:pop :rdi)
  (sys.lap-x86:pop :rsi)
  (sys.lap-x86:pop :rbx)
  (sys.lap-x86:pop :rdx)
  (sys.lap-x86:pop :rcx)
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:pop :rbp)
  (sys.lap-x86:iret))

(defun establish-deferred-footholds (thread)
  (declare (ignore self)))

(defun establish-thread-foothold (thread function)
  (cond ((eql thread (current-thread))
         (assert (thread-footholds-enabled-p thread))
         (funcall function))
        (t (with-thread-lock (thread)
             (tagbody
              AGAIN
                (ecase (thread-state thread)
                  ;; todo: SMP.
                  (:active (error "what?"))
                  (:runnable
                   ;; Wait for the thread to allow footholds.
                   (do ()
                       ((thread-footholds-enabled-p thread))
                     ;; Drop the lock & reschedule.
                     (%unlock-thread thread)
                     (sys.int::%sti)
                     (thread-yield)
                     (sys.int::%cli)
                     (%lock-thread thread)
                     (go AGAIN))
                   ;; Top stack element is the rbp, 2nd is return rip.
                   (let* ((rsp (thread-stack-pointer thread))
                          (rbp (sys.int::memref-signed-byte-64 rsp 0)))
                     ;; %%SWITCH-TO-THREAD is kind enough to clear rcx for us,
                     ;; so no thunk required.
                     ;; +1 RIP
                     ;; +0 RBP
                     ;; becomes
                     ;; +2 RIP
                     ;; +1 function-entry
                     ;; +0 RBP
                     (setf (sys.int::memref-signed-byte-64 rsp 0)
                           (sys.int::%array-like-ref-signed-byte-64 (sys.int::%coerce-to-callable function) 0))
                     (setf (sys.int::memref-signed-byte-64 rsp -1) rbp)
                     (setf (sys.int::%array-like-ref-signed-byte-64 thread +thread-stack-pointer+) (- rsp 8))))
                  ;; todo.
                  (:sleeping (error "Thread is sleeping."))
                  (:dead (error "Trying to interrupt dead thread."))))))))

(defun destroy-thread (thread &optional abort)
  "Terminate THREAD.
If abort is false, then cleanup forms will be run before the thread exits;
otherwise the thread will exit immediately, and not execute cleanup forms."
  (establish-thread-foothold thread
                             (lambda ()
                               (throw 'terminate-thread nil))))

(defun initialize-initial-thread ()
  "Called very early after boot to reset the initial thread."
  (let* ((thread (current-thread)))
    (setf *world-stopper* thread)
    (dotimes (i (- +thread-tls-slots-end+ +thread-tls-slots-start+))
      (setf (sys.int::%array-like-ref-t thread (+ +thread-tls-slots-start+ i))
            (sys.int::%unbound-tls-slot)))
    (setf (thread-state thread) :active)))

(defun finish-initial-thread ()
  "Called when the boot code is done with the initial thread."
  ;; The initial thread never dies, it just sleeps until the next boot.
  ;; The bootloader will partially wake it up, then initialize-initial-thread
  ;; will finish initialization.
  ;; The initial thread must finish with no values on the special stack, and
  ;; all TLS slots initialized. This is required by INITIALIZE-INITIAL-THREAD.
  (let ((thread (current-thread)))
    (setf *world-stopper* nil)
    (sys.int::%cli)
    (%lock-thread thread)
    (setf (thread-wait-item thread) "The start of a new world"
          (thread-state thread) :sleeping)
    (%reschedule)
    (panic "Initial thread woken??")))

(defun all-threads ()
  (do ((list '())
       (current *all-threads* (thread-global-next current)))
      ((null current)
       list)
    (push current list)))

(defmacro dx-lambda (lambda-list &body body)
  `(flet ((dx-lambda ,lambda-list ,@body))
     (declare (dynamic-extent #'dx-lambda))
     #'dx-lambda))

;;; WITH-WORLD-STOPPED and WITH-PSEUDO-ATOMIC work together as a sort-of global
;;; reader/writer lock over the whole system.

(defun call-with-world-stopped (thunk)
  (let ((self (current-thread)))
    (when (eql *world-stopper* self)
      (panic "Nested world stop!"))
    (when *pseudo-atomic*
      (panic "Stopping world while pseudo-atomic!"))
    (with-mutex (*world-stop-lock*)
      ;; First, try to position ourselves as the next thread to stop the world.
      ;; This prevents any more threads from becoming PA.
      (loop
         (when (null *world-stop-pending*)
           (setf *world-stop-pending* self)
           (return))
         ;; Wait for the world to unstop.
         (condition-wait *world-stop-resume-cvar* *world-stop-lock*))
      ;; Now wait for any PA threads to finish.
      (loop
         (when (zerop *pseudo-atomic-thread-count*)
           (setf *world-stopper* self
                 *world-stop-pending* nil)
           (return))
         (condition-wait *world-stop-pa-exit-cvar* *world-stop-lock*)))
    ;; Don't hold the mutex over the thunk, it's a spinlock and disables interrupts.
    (multiple-value-prog1
        (funcall thunk)
      (with-mutex (*world-stop-lock*)
        ;; Release the dogs!
        (setf *world-stopper* nil)
        (condition-notify *world-stop-resume-cvar*)))))

(defmacro with-world-stopped (&body body)
  `(call-with-world-stopped (dx-lambda () ,@body)))

(defun call-with-pseudo-atomic (thunk)
  (when (eql *world-stopper* (current-thread))
    (panic "Going PA with world stopped!"))
  (with-mutex (*world-stop-lock*)
    (when *world-stop-pending*
      ;; Wait for the world to stop & resume.
      (condition-wait *world-stop-resume-cvar* *world-stop-lock*))
    ;; TODO: Have a list of pseudo atomic threads, and prevent PA threads
    ;; from being inspected.
    (incf *pseudo-atomic-thread-count*))
  (unwind-protect
       (let ((*pseudo-atomic* t))
         (funcall thunk))
    (with-mutex (*world-stop-lock*)
      (decf *pseudo-atomic-thread-count*)
      (condition-notify *world-stop-pa-exit-cvar*))))

(defmacro with-pseudo-atomic (&body body)
  `(call-with-pseudo-atomic (dx-lambda () ,@body)))

;;; Common structure for sleepable things.
(defstruct wait-queue
  name
  ;; Spin mutexes also abuse this field as a place to store the old interrupt state.
  (%lock :unlocked) ; must be 2nd slot.
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
  (do ((current-thread (current-thread)))
      ((sys.int::%cas-array-like wait-queue
                                 2
                                 :unlocked
                                 current-thread))
    (sys.int::cpu-relax)))

(defun unlock-wait-queue (wait-queue)
  (setf (wait-queue-%lock wait-queue) :unlocked))

(defmacro with-wait-queue-lock ((wait-queue) &body body)
  (let ((sym (gensym "WAIT-QUEUE")))
    `(let ((,sym ,wait-queue))
       (without-interrupts
         (unwind-protect
              (progn
                (lock-wait-queue ,sym)
                ,@body)
           (unlock-wait-queue ,sym))))))

(defstruct (mutex
             (:include wait-queue)
             (:constructor make-mutex (&optional name (kind :block)))
             (:area :wired))
  ;; When NIL, the lock is free, otherwise is set to
  ;; the thread that holds the lock.
  (owner nil) ; must be slot 5, after wait-queue is included.
  (kind nil :type (member :block :spin) :read-only t)
  (stack-next nil))

(defun acquire-mutex (mutex &optional (wait-p t))
  (ecase (mutex-kind mutex)
    (:block (acquire-block-mutex mutex wait-p))
    (:spin (acquire-spin-mutex mutex wait-p))))

(defun acquire-block-mutex (mutex wait-p)
  (let ((self (current-thread)))
    (unless (sys.int::%interrupt-state)
      (panic "Trying to acquire block mutex " mutex " with interrupts disabled."))
    (unless (not (thread-footholds-enabled-p self))
      (panic "Trying to acquire block mutex " mutex " with footholds enabled."))
    (unless (not *pseudo-atomic*)
      (panic "Trying to acquire block mutex " mutex " while pseudo-atomic."))
    ;; Fast path - try to lock.
    (when (sys.int::%cas-struct-slot mutex 5 nil self)
      ;; We got it.
      (setf (mutex-stack-next mutex) (thread-mutex-stack self)
            (thread-mutex-stack self) mutex)
      (return-from acquire-block-mutex t))
    ;; Idiot check.
    (unless (not (mutex-held-p mutex))
      (error "Recursive locking detected on ~S." mutex))
    (when wait-p
      ;; Slow path.
      (sys.int::%cli)
      (lock-wait-queue mutex)
      ;; Try to acquire again, release may have been running.
      (when (sys.int::%cas-struct-slot mutex 5 nil self)
        ;; We got it.
        (setf (mutex-stack-next mutex) (thread-mutex-stack self)
              (thread-mutex-stack self) mutex)
        (sys.int::%sti)
        (unlock-wait-queue mutex)
        (return-from acquire-block-mutex t))
      ;; No good, have to sleep.
      ;; Add to wait queue. Release will directly transfer ownership
      ;; to this thread.
      (push-wait-queue self mutex)
      ;; Now sleep.
      ;; Must take the thread lock before dropping the mutex lock or release
      ;; may be able to remove the thread from the sleep queue before it goes
      ;; to sleep.
      ;; todo: reenable footholds when the thread is sleeping, but only one level.
      (%lock-thread self)
      ;; Do some deadlock detection before going to sleep. If the current owner
      ;; is blocked on a mutex held by self, then a deadlock has occurred.
      (let ((owner (mutex-owner mutex)))
        (with-thread-lock (owner)
          (when (eql (thread-state owner) :sleeping)
            (do ((lock (thread-mutex-stack self) (mutex-stack-next lock)))
                ((null lock))
              (when (eql lock (thread-wait-item owner))
                (%unlock-thread owner)
                (%unlock-thread self)
                (pop-wait-queue mutex)
                (unlock-wait-queue mutex)
                (sys.int::%sti)
                (panic "Deadlock detected!~%~
Current thread ~S locking ~S, held by ~S, waiting on lock ~S!"
                       current
                       mutex
                       owner
                       lock))))))
      (unlock-wait-queue mutex)
      (setf (thread-wait-item self) mutex
            (thread-state self) :sleeping)
      (%reschedule)
      t)))

(defun acquire-spin-mutex (mutex wait-p)
  (let ((self (current-thread))
        (istate (sys.int::%interrupt-state)))
    (sys.int::%cli)
    ;; Fast path - try to lock.
    (when (sys.int::%cas-struct-slot mutex 5 nil self)
      ;; We got it.
      (setf (mutex-%lock mutex) istate)
      (return-from acquire-spin-mutex t))
    ;; Idiot check.
    (unless (not (mutex-held-p mutex))
      (panic "Recursive locking detected."))
    (panic "Spin-mutex " mutex " held by " (mutex-owner mutex))
    (cond (wait-p
           ;; Spin path.
           (do ()
               ((sys.int::%cas-struct-slot mutex 5 nil self))
             (sys.int::cpu-relax))
           (setf (mutex-%lock mutex) istate)
           t)
          (t ;; Trylock path.
           (when istate
             (sys.int::%sti))))))

(defun mutex-held-p (mutex)
  "Return true if this thread holds MUTEX."
  (eql (mutex-owner mutex) (current-thread)))

(defun release-mutex (mutex)
  (unless (mutex-held-p mutex)
    (panic "Tryin to release mutex " mutex " not held by thread."))
  (ecase (mutex-kind mutex)
    (:block (release-block-mutex mutex))
    (:spin (release-spin-mutex mutex)))
  (values))

(defun release-block-mutex (mutex)
  (with-wait-queue-lock (mutex)
    (let ((self (current-thread)))
      (when (not (eql mutex (thread-mutex-stack self)))
        (panic "Thread " self " releasing mutex " mutex " out of order."))
      (setf (thread-mutex-stack self) (mutex-stack-next mutex)))
    ;; Look for a thread to wake.
    (let ((thread (pop-wait-queue mutex)))
      (cond (thread
             ;; Found one, wake it & transfer the lock.
             (setf (mutex-owner mutex) thread)
             (setf (mutex-stack-next mutex) (thread-mutex-stack thread)
                   (thread-mutex-stack thread) mutex)
             (wake-thread thread))
            (t
             ;; No threads sleeping, just drop the lock.
             (setf (mutex-owner mutex) nil))))))

(defun release-spin-mutex (mutex)
  (let ((istate (mutex-%lock mutex)))
    (setf (mutex-owner mutex) nil)
    (when istate
      (sys.int::%sti))))

(defun call-with-mutex (thunk mutex wait-p)
  (ecase (mutex-kind mutex)
    (:block (call-with-block-mutex thunk mutex wait-p))
    (:spin (call-with-spin-mutex thunk mutex wait-p))))

(defun call-with-block-mutex (thunk mutex wait-p)
  (let ((got-it nil))
    ;; Disable footholds while taking the lock, this prevents a
    ;; foothold from running after the mutex has been locked, but
    ;; before GOT-IT has been set to true. If it were to unwind
    ;; at that point, then the mutex would never be released.
    (with-footholds-inhibited
      (unwind-protect
           (when (setf got-it (acquire-mutex mutex wait-p))
             (with-footholds-permitted
               (funcall thunk)))
        (when got-it
          (release-mutex mutex))))))

(defun call-with-spin-mutex (thunk mutex wait-p)
  ;; No foothold messing around required here. These locks disable interrupts.
  (let ((got-it nil))
    (unwind-protect
         (when (setf got-it (acquire-mutex mutex wait-p))
           (funcall thunk))
      (when got-it
        (release-mutex mutex)))))

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
  (ecase (mutex-kind mutex)
    ;; Interrupts must be enabled.
    (:block (assert (sys.int::%interrupt-state)))
    ;; Interrupts must have been enabled when the lock was taken.
    ;; TODO: Track how many spin mutexes are currently taken and assert when
    ;; count != 1.
    (:spin (assert (mutex-%lock mutex))))
  (let ((self (current-thread))
        (prior-istate (mutex-%lock mutex)))
    (with-footholds-inhibited
      (sys.int::%cli)
      (lock-wait-queue condition-variable)
      (%lock-thread self)
      ;; Attach to the list.
      (push-wait-queue self condition-variable)
      ;; Drop the mutex.
      ;; Make sure spin mutexes don't reenable interrupts.
      (when (eql (mutex-kind mutex) :spin)
        (setf (mutex-%lock mutex) nil))
      (release-mutex mutex)
      ;; Sleep.
      ;; todo: reenable footholds when the thread is sleeping, but only one level.
      ;; need to be careful with that, returning or unwinding from condition-wait
      ;; with the lock unlocked would be quite bad.
      (setf (thread-wait-item self) condition-variable
            (thread-state self) :sleeping)
      (unlock-wait-queue condition-variable)
      (%reschedule)
      ;; Got woken up. Reacquire the mutex.
      (acquire-mutex mutex t)
      ;; And restore :spin istate.
      (when (eql (mutex-kind mutex) :spin)
        (setf (mutex-%lock mutex) prior-istate))))
  (values))

(defun condition-notify (condition-variable &optional broadcast)
  "Wake one or many threads waiting on CONDITION-VARIABLE.
May be used from an interrupt handler, assuming the associated mutex is interrupt-safe."
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
               (pop-one))))))
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
  (let ((self (current-thread)))
    (assert (sys.int::%interrupt-state))
    (assert (not (thread-footholds-enabled-p self)))
    (sys.int::%cli)
    (lock-wait-queue semaphore)
    (when (not (zerop (semaphore-value semaphore)))
      (decf (semaphore-value semaphore))
      (unlock-wait-queue semaphore)
      (sys.int::%sti)
      (return-from semaphore-down t))
    (cond (wait-p
           ;; Go to sleep.
           (push-wait-queue self mutex)
           ;; Now sleep.
           ;; Must take the thread lock before dropping the semaphore lock or up
           ;; may be able to remove the thread from the sleep queue before it goes
           ;; to sleep.
           ;; todo: reenable footholds when the thread is sleeping, but only one level.
           (%lock-thread self)
           (unlock-wait-queue semaphore)
           (setf (thread-wait-item self) semaphore
                 (thread-state self) :sleeping)
           (%reschedule)
           t)
          (t (unlock-wait-queue semaphore)
             (sys.int::%sti)
             nil))))

(defstruct (fifo
             (:area :wired)
             (:constructor %make-fifo))
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (size)
  (element-type)
  (buffer (error "no buffer supplied") :read-only t)
  (cv (make-condition-variable))
  (lock (make-mutex "fifo-lock" :spin)))

(defun make-fifo (size &key (element-type 't))
  ;; TODO: non-t element types.
  (%make-fifo :size size
              :buffer (sys.int::make-simple-vector size :wired)
              :element-type 't))

(defun fifo-push (value fifo &optional (wait-p t))
  "Push a byte onto FIFO. Returns true if successful.
If the fifo is full, then FIFO-PUSH will wait for space to become available
when WAIT-P is true, otherwise it will immediately return false.
May be used from an interrupt handler if WAIT-P is false."
  (with-mutex ((fifo-lock fifo))
    (loop
       (let ((next (1+ (fifo-tail fifo))))
         (when (>= next (fifo-size fifo))
           (setf next 0))
         ;; When next reaches head, the buffer is full.
         (unless (= next (fifo-head fifo))
           (setf (svref (fifo-buffer fifo) (fifo-tail fifo)) value
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
It is only possible for the second value to be false when wait-p is false.
May be used from an interrupt handler if WAIT-P is false."
  (with-mutex ((fifo-lock fifo))
    (loop
       (when (not (eql (fifo-head fifo) (fifo-tail fifo)))
         ;; Fifo not empty, pop byte.
         (let ((value (svref (fifo-buffer fifo) (fifo-head fifo)))
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
  "Flush any waiting data.
May be used from an interrupt handler."
  (with-mutex ((fifo-lock fifo))
    (setf (fifo-head fifo) 0
          (fifo-tail fifo) 0)
    ;; Signal the cvar to wake any waiting FIFO-PUSH calls.
    (condition-notify (fifo-cv fifo))))
