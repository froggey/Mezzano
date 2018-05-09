;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(sys.int::defglobal *global-thread-lock* nil
  "This lock protects the special variables that make up the thread list/run queues and the thread objects.")
(sys.int::defglobal *supervisor-priority-run-queue*)
(sys.int::defglobal *high-priority-run-queue*)
(sys.int::defglobal *normal-priority-run-queue*)
(sys.int::defglobal *low-priority-run-queue*)
(sys.int::defglobal *all-threads*)
(sys.int::defglobal *n-running-cpus*)

(sys.int::defglobal *world-stop-lock*)
(sys.int::defglobal *world-stop-cvar*)
(sys.int::defglobal *world-stop-pending*)
(sys.int::defglobal *world-stopper*)
(sys.int::defglobal *pseudo-atomic-thread-count*)

(sys.int::defglobal *default-stack-size*)

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

(deftype thread ()
  `(satisfies threadp))

(defun threadp (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-thread+))

(macrolet ((field (name offset &key (type 't) (accessor 'sys.int::%object-ref-t))
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
                    (setf (,accessor thread ,field-name) value)))))
          (reg-field (name offset)
            (let ((state-name (intern (format nil "STATE-~A" name) (symbol-package name)))
                  (state-name-value (intern (format nil "STATE-~A-VALUE" name) (symbol-package name))))
              `(progn
                 (field ,state-name ,offset :accessor sys.int::%object-ref-signed-byte-64)
                 (field ,state-name-value ,offset :accessor sys.int::%object-ref-t)))))
  ;; The name of the thread, a string.
  (field name                     0 :type string)
  ;; Current state.
  ;;   :active    - the thread is currently running on a core.
  ;;   :runnable  - the thread can be run, but is not currently running.
  ;;   :sleeping  - the thread is waiting for an event and cannot run.
  ;;   :dead      - the thread has exited or been killed and cannot run.
  ;;   :waiting-for-page - the thread is waiting for memory to be paged in.
  ;;   :pager-request - the thread is waiting for a pager RPC call to complete.
  ;;   :stopped   - the thread has been stopped for inspection by a debugger.
  (field state                    1 :type (member :active :runnable :sleeping :dead :waiting-for-page :pager-request :stopped))
  ;; 2 - free
  ;; Stack object for the stack.
  (field stack                    3)
  ;; 4 - magic field used by bootloader.
  ;; If a thread is sleeping, waiting for page or performing a pager-request, this will describe what it's waiting for.
  ;; When waiting for paging to complete, this will be the faulting address.
  ;; When waiting for a pager-request, this will be the called function.
  (field wait-item                5)
  ;; The thread's current special stack pointer.
  ;; Note! The compiler must be updated if this changes and all code rebuilt.
  (field special-stack-pointer    6)
  ;; When true, all registers are saved in the the thread's state save area.
  ;; When false, only the stack pointer and frame pointer are valid.
  (field full-save-p              7)
  ;; The thread object, used to make CURRENT-THREAD fast.
  (field self                     8)
  ;; Next/previous links for run queues and wait queues.
  (field %next                    9)
  (field %prev                   10)
  ;; A list of foothold functions that need to be run.
  (field pending-footholds       11)
  ;; A non-negative fixnum, when 0 footholds are permitted to run.
  ;; When positive, they are deferred.
  (field inhibit-footholds       12)
  (field mutex-stack             13)
  ;; Next/previous links for the *all-threads* list.
  ;; This only contains live (not state = :dead) threads.
  (field global-next             14)
  (field global-prev             15)
  ;; Thread's priority, can be :supervisor, :high, :normal, or :low.
  ;; Threads at :supervisor have priority over all other threads.
  (field priority                16 :type (member :low :normal :high :supervisor :idle))
  ;; Arguments passed to the pager when performing an RPC.
  (field pager-argument-1        17)
  (field pager-argument-2        18)
  (field pager-argument-3        19)
  ;; Table of active breakpoints.
  (field breakpoint-table        20)
  ;; Sorted simple-vector of breakpoint addresses, used when the thread is running in software-breakpoint mode.
  (field software-breakpoints    21)
  ;; Symbol binding cache hit count.
  (field symbol-cache-hit-count  22)
  ;; Symbol binding cache miss count.
  (field symbol-cache-miss-count 23)
  ;; Helper function UNSLEEP-THREAD.
  (field unsleep-helper 24)
  ;; Argument for the unsleep helper.
  (field unsleep-helper-argument 25)
  ;; 26-32 - free
  ;; 32-127 MV slots
  ;;    Slots used as part of the multiple-value return convention.
  ;;    Note! The compiler must be updated if this changes and all code rebuilt.
  (defconstant +thread-mv-slots-start+ 32)
  (defconstant +thread-mv-slots-end+ 128)
  ;; 128-256 Symbol binding cell cache.
  (defconstant +thread-symbol-cache-start+ 128)
  (defconstant +thread-symbol-cache-end+ 256)
  ;; 256-424 free
  (field arm64-fpsr 425 :type (unsigned-byte 32) :accessor sys.int::%object-ref-unsigned-byte-32)
  (field arm64-fpcr 426 :type (unsigned-byte 32) :accessor sys.int::%object-ref-unsigned-byte-32)
  ;; 427-446 State save area.
  ;;    Used to save an interrupt frame when the thread has stopped to wait for a page.
  ;;    The registers are saved here, not on the stack, because the stack may not be paged in.
  ;;    This has the same layout as an interrupt frame.
  ;; 447-510 FXSAVE area
  ;;    Unboxed area where the FPU/SSE state is saved.
  (defconstant +thread-interrupt-save-area+ 427)
  (defconstant +thread-fx-save-area+ 447)
  (reg-field r15                427)
  (reg-field r14                428)
  (reg-field r13                429)
  (reg-field r12                430)
  (reg-field r11                431)
  (reg-field r10                432)
  (reg-field r9                 433)
  (reg-field r8                 434)
  (reg-field rdi                435)
  (reg-field rsi                436)
  (reg-field rbx                437)
  (reg-field rdx                438)
  (reg-field rcx                439)
  (reg-field rax                440)
  (reg-field rbp                441)
  (reg-field rip                442)
  (reg-field cs                 443)
  (reg-field rflags             444)
  (reg-field rsp                445)
  (reg-field ss                 446))

;;; Aliases for a few registers.

(defun thread-frame-pointer (thread)
  (thread-state-rbp thread))

(defun (setf thread-frame-pointer) (value thread)
  (setf (thread-state-rbp thread) value))

(defun thread-stack-pointer (thread)
  (thread-state-rsp thread))

(defun (setf thread-stack-pointer) (value thread)
  (setf (thread-state-rsp thread) value))

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
  (cond ((null (run-queue-head rq))
         (setf (run-queue-head rq) thread
               (run-queue-tail rq) thread)
         (setf (thread-%next thread) nil
               (thread-%prev thread) nil))
        (t
         (setf (thread-%next (run-queue-tail rq)) thread
               (thread-%prev thread) (run-queue-tail rq)
               (thread-%next thread) nil
               (run-queue-tail rq) thread))))

(defun push-run-queue (thread)
  (ensure-global-thread-lock-held)
  (when (eql thread *world-stopper*)
    (return-from push-run-queue))
  (push-run-queue-1 thread (run-queue-for-priority (thread-priority thread))))

(defun pop-run-queue-1 (rq)
  (let ((thread (run-queue-head rq)))
    (when thread
      (cond ((thread-%next thread)
             (setf (thread-%prev (thread-%next thread)) nil)
             (setf (run-queue-head rq) (thread-%next thread)))
            (t
             (setf (run-queue-head rq) nil
                   (run-queue-tail rq) nil)))
      thread)))

(defun pop-run-queue ()
  (or (pop-run-queue-1 *supervisor-priority-run-queue*)
      (pop-run-queue-1 *high-priority-run-queue*)
      (pop-run-queue-1 *normal-priority-run-queue*)
      (pop-run-queue-1 *low-priority-run-queue*)))

(defun dump-run-queue (rq)
  (debug-print-line "Run queue " rq "/" (run-queue-name rq) ":")
  (do ((thread (run-queue-head rq) (thread-%next thread)))
      ((null thread))
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

(defun %update-run-queue ()
  "Possibly return the current thread to the run queue, and
return the next thread to run.
Interrupts must be off and the global thread lock must be held."
  (ensure-global-thread-lock-held)
  (let ((current (current-thread)))
    (when (eql current (local-cpu-idle-thread))
      (panic "Aiee. Idle thread called %UPDATE-RUN-QUEUE."))
    ;; Return the current thread to the run queue and fetch the next thread.
    (when (and (not (eql current *world-stopper*))
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
           (or
            ;; Try taking from the run queue.
            (pop-run-queue)
            ;; Fall back on idle.
            (local-cpu-idle-thread))))))

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
  (let ((current (current-thread))
        (next (%update-run-queue)))
    ;; todo: reset preemption timer here.
    (when (eql next current)
      ;; Staying on the same thread, unlock and return.
      (release-global-thread-lock)
      (%%return-to-same-thread sp fp)
      (panic "unreachable"))
    (when (<= sys.int::*exception-stack-base*
              (thread-stack-pointer next)
              (1- sys.int::*exception-stack-size*))
      (panic "Other thread " next " stopped on exception stack!!!"))
    (ensure (eql (thread-state next) :runnable) "Switching to thread " next " with bad state " (thread-state next))
    (setf (thread-state next) :active)
    (%%switch-to-thread-via-wired-stack current sp fp next)))

(defun %reschedule-via-interrupt (interrupt-frame)
  ;; Switch to the next thread saving the full state.
  ;; Interrupts must be off and the global thread lock must be held.
  ;; Releases the thread lock and reenables interrupts.
  (ensure-global-thread-lock-held)
  (let ((current (current-thread))
        (next (%update-run-queue)))
    ;; todo: reset preemption timer here.
    (when (not (eql next current))
      (ensure (eql (thread-state next) :runnable) "Switching to thread " next " with bad state " (thread-state next)))
    (setf (thread-state next) :active)
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

(defun %%switch-to-thread-common (current-thread new-thread)
  ;; Current thread's state has been saved, restore the new-thread's state.
  ;; Switch threads.
  (set-current-thread new-thread)
  ;; Restore FPU state.
  (restore-fpu-state new-thread)
  ;; Drop the global thread lock.
  (release-global-thread-lock)
  ;; Check if the thread is full-save.
  (if (thread-full-save-p new-thread)
      (%%restore-full-save-thread new-thread)
      (%%restore-partial-save-thread new-thread)))

;;; Stuff.

(defun make-thread (function &key name initial-bindings (stack-size *default-stack-size*) (priority :normal))
  (declare (sys.c::closure-allocation :wired))
  (check-type name (or null string))
  (check-type function (or function symbol))
  (check-type priority (member :supervisor :high :normal :low))
  (when name
    (setf name (mezzano.runtime::copy-string-in-area name :wired)))
  ;; Allocate-object will leave the thread's state variable initialized to 0.
  ;; The GC detects this to know when it's scanning a partially-initialized thread.
  (let* ((thread (mezzano.runtime::%allocate-object sys.int::+object-tag-thread+ 0 511 :wired))
         (stack (%allocate-stack stack-size)))
    (setf (sys.int::%object-ref-t thread +thread-name+) name
          (sys.int::%object-ref-t thread +thread-stack+) stack
          (sys.int::%object-ref-t thread +thread-special-stack-pointer+) nil
          (sys.int::%object-ref-t thread +thread-self+) thread
          (sys.int::%object-ref-t thread +thread-wait-item+) nil
          (sys.int::%object-ref-t thread +thread-mutex-stack+) nil
          (sys.int::%object-ref-t thread +thread-pending-footholds+) '()
          (sys.int::%object-ref-t thread +thread-inhibit-footholds+) 1
          (sys.int::%object-ref-t thread +thread-priority+) priority
          (sys.int::%object-ref-t thread +thread-pager-argument-1+) nil
          (sys.int::%object-ref-t thread +thread-pager-argument-2+) nil
          (sys.int::%object-ref-t thread +thread-pager-argument-3+) nil)
    ;; Perform initial bindings.
    (when initial-bindings
      (let ((symbols (mapcar #'first initial-bindings))
            (values (mapcar #'second initial-bindings))
            (original-function function))
        (setf function (lambda ()
                         (progv symbols values
                           (funcall original-function))))))
    ;; Initialize the FXSAVE area.
    ;; All FPU/SSE interrupts masked, round to nearest,
    ;; x87 using 80 bit precision (long-float).
    (dotimes (i 64)
      (setf (sys.int::%object-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ i)) 0))
    (setf (ldb (byte 16 0) (sys.int::%object-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ 0)))
          #x037F) ; FCW
    (setf (ldb (byte 32 0) (sys.int::%object-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ 3)))
          #x00001F80) ; MXCSR
    (setf (thread-arm64-fpsr thread) 0
          (thread-arm64-fpcr thread) 0)
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

(defun make-ephemeral-thread (entry-point initial-state &key name (stack-size (* 256 1024)) (priority :normal))
  (let* ((thread (mezzano.runtime::%allocate-object sys.int::+object-tag-thread+ 0 511 :wired))
         (stack (%allocate-stack stack-size t)))
    (setf (sys.int::%object-ref-t thread +thread-name+) name
          (sys.int::%object-ref-t thread +thread-stack+) stack
          (sys.int::%object-ref-t thread +thread-special-stack-pointer+) nil
          (sys.int::%object-ref-t thread +thread-self+) thread
          (sys.int::%object-ref-t thread +thread-wait-item+) nil
          (sys.int::%object-ref-t thread +thread-mutex-stack+) nil
          (sys.int::%object-ref-t thread +thread-pending-footholds+) '()
          (sys.int::%object-ref-t thread +thread-inhibit-footholds+) 1
          (sys.int::%object-ref-t thread +thread-priority+) priority
          (sys.int::%object-ref-t thread +thread-pager-argument-1+) nil
          (sys.int::%object-ref-t thread +thread-pager-argument-2+) nil
          (sys.int::%object-ref-t thread +thread-pager-argument-3+) nil)
    (reset-ephemeral-thread thread entry-point initial-state priority)
    thread))

;; MAKE-THREAD arranges for new threads to call this function with the thread's
;; initial function as an argument.
;; It sets up the top-level catch for 'terminate-thread, and deals with cleaning
;; up when the thread exits (either by normal return or by a throw to terminate-thread).
(defun thread-entry-trampoline (function)
  (unwind-protect
       (catch 'terminate-thread
         ;; Footholds in a new thread are inhibited until the terminate-thread
         ;; catch block is established, to guarantee that it's always available.
         (let ((thread (current-thread)))
           (sys.int::%atomic-fixnum-add-object thread +thread-inhibit-footholds+ -1)
           (when (zerop (sys.int::%object-ref-t thread +thread-inhibit-footholds+))
             (dolist (fh (sys.int::%xchg-object thread +thread-pending-footholds+ nil))
               (funcall fh))))
         (funcall function))
    ;; Cleanup, terminate the thread.
    (thread-final-cleanup)))

;; This is seperate from thread-entry-trampoline so steppers can detect it.
(defun thread-final-cleanup ()
  (%run-on-wired-stack-without-interrupts (sp fp)
    (let ((self (current-thread)))
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

;; The idle thread is not a true thread. It does not appear in all-threads, nor in any run-queue.
;; When the machine boots, one idle thread is created for each core. When a core is idle, the
;; idle thread will be run.
(defun idle-thread ()
  (%disable-interrupts)
  (decrement-n-running-cpus)
  (loop
     ;; Look for a thread to switch to.
     (acquire-global-thread-lock)
     (let ((next (cond (*world-stopper*
                        (cond ((pop-run-queue-1 *supervisor-priority-run-queue*))
                              ((eql (thread-state *world-stopper*) :runnable)
                               *world-stopper*)
                              (t nil)))
                       (t
                        (pop-run-queue)))))
       (cond (next
              (increment-n-running-cpus)
              ;; Switch to thread.
              (setf (thread-state next) :active)
              (setf (thread-state (local-cpu-idle-thread)) :runnable)
              (%run-on-wired-stack-without-interrupts (sp fp next)
                (%%switch-to-thread-via-wired-stack (local-cpu-idle-thread) sp fp next))
              (%disable-interrupts)
              (decrement-n-running-cpus))
             (t ;; Wait for an interrupt.
              (release-global-thread-lock)
              (%wait-for-interrupt)
              (%disable-interrupts))))))

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
  (when (not (eql priority :idle))
    (let ((rq (run-queue-for-priority (thread-priority thread))))
      (cond ((and (eql (run-queue-head rq) thread)
                  (eql (run-queue-tail rq) thread))
             ;; Only thread on run queue.
             (setf (run-queue-head rq) nil
                   (run-queue-tail rq) nil))
            ((eql (run-queue-head rq) thread)
             ;; More than one thread, at head.
             (setf (thread-%prev (thread-%next thread)) nil)
             (setf (run-queue-head rq) (thread-%next thread)))
            ((eql (run-queue-tail rq) thread)
             ;; More than one thread, at tail.
             (setf (thread-%next (thread-%prev thread)) nil)
             (setf (run-queue-tail rq) (thread-%prev thread)))
            ((thread-%next thread)
             ;; Somewhere in the middle of the run queue.
             (setf (thread-%next (thread-%prev thread)) (thread-%next thread)
                   (thread-%prev (thread-%next thread)) (thread-%prev thread))
             (setf (thread-%next thread) nil
                   (thread-%prev thread) nil)))))
  (setf (thread-state thread) state
        (thread-priority thread) (or priority :supervisor)
        (sys.int::%object-ref-t thread +thread-special-stack-pointer+) nil
        (sys.int::%object-ref-t thread +thread-wait-item+) nil
        (sys.int::%object-ref-t thread +thread-mutex-stack+) nil
        (sys.int::%object-ref-t thread +thread-pending-footholds+) '()
        (sys.int::%object-ref-t thread +thread-inhibit-footholds+) 1
        (thread-full-save-p thread) t)
  (when (and (not (eql priority :idle))
             (eql state :runnable))
    (safe-without-interrupts (thread)
      (acquire-global-thread-lock)
      (push-run-queue thread)
      (release-global-thread-lock)))
  ;; Initialize the FXSAVE area.
  ;; All FPU/SSE interrupts masked, round to nearest,
  ;; x87 using 80 bit precision (long-float).
  (dotimes (i 64)
    (setf (sys.int::%object-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ i)) 0))
  (setf (ldb (byte 16 0) (sys.int::%object-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ 0)))
        #x037F) ; FCW
  (setf (ldb (byte 32 0) (sys.int::%object-ref-unsigned-byte-64 thread (+ +thread-fx-save-area+ 3)))
        #x00001F80) ; MXCSR
  (setf (thread-arm64-fpsr thread) 0
        (thread-arm64-fpcr thread) 0)
  ;; Flush the symbol cache.
  (dotimes (i (- +thread-symbol-cache-end+ +thread-symbol-cache-start+))
    (setf (sys.int::%object-ref-t thread (+ +thread-symbol-cache-start+ i)) 0)))

(defun initialize-threads ()
  (when (not (boundp '*global-thread-lock*))
    ;; First-run stuff.
    (setf *global-thread-lock* :unlocked)
    (setf *supervisor-priority-run-queue* (make-run-queue :supervisor)
          *high-priority-run-queue* (make-run-queue :high)
          *normal-priority-run-queue* (make-run-queue :normal)
          *low-priority-run-queue* (make-run-queue :low))
    (setf *world-stop-lock* (make-mutex "World stop lock")
          *world-stop-cvar* (make-condition-variable "World stop cvar")
          *world-stop-pending* nil
          *pseudo-atomic-thread-count* 0)
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
  (reset-ephemeral-thread sys.int::*disk-io-thread* #'disk-thread :runnable :supervisor)
  (condition-notify *world-stop-cvar* t))

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
    (setf (thread-state thread) :active)))

(defun finish-initial-thread ()
  "Called when the boot code is done with the initial thread."
  ;; The initial thread never dies, it just sleeps until the next boot.
  ;; The bootloader will partially wake it up, then initialize-initial-thread
  ;; will finish initialization.
  ;; The initial thread must finish with no values on the special stack.
  ;; This is required by INITIALIZE-INITIAL-THREAD.
  (let ((thread (current-thread)))
    (setf *world-stopper* nil)
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
        (footholds (gensym "FOOTHOLDS"))
        (fh (gensym "FH")))
    `(unwind-protect
          (progn
            (sys.int::%atomic-fixnum-add-object (current-thread) +thread-inhibit-footholds+ 1)
            ,@body)
       (let ((,thread (current-thread)))
         (sys.int::%atomic-fixnum-add-object ,thread +thread-inhibit-footholds+ -1)
         (when (zerop (sys.int::%object-ref-t ,thread +thread-inhibit-footholds+))
           (run-pending-footholds))))))

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

(defun establish-thread-foothold (thread function &optional force)
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

(defmacro with-world-stop-lock (&body body)
  ;; Run with boosted priority to prevent livelocking with other threads spinning against the lock.
  `(let ((%prev-priority (thread-priority (current-thread))))
     (unwind-protect
          (progn
            (setf (thread-priority (current-thread)) :high)
            (loop
               ;; Spin on the lock to prevent threads stacking up on wait list.
               ;; Threads sleeping on the mutex will never wake up if the world
               ;; is stopped.
               ;; Mutexes and cvars probably aren't the right thing to use here.
               (when (acquire-mutex *world-stop-lock* nil)
                 (return))
               (thread-yield))
            ,@body)
       (release-mutex *world-stop-lock*)
       (setf (thread-priority (current-thread)) %prev-priority))))

(defun call-with-world-stopped (thunk)
  (let ((self (current-thread)))
    (when (eql *world-stopper* self)
      (panic "Nested world stop!"))
    (when *pseudo-atomic*
      (panic "Stopping world while pseudo-atomic!"))
    (ensure-interrupts-enabled)
    (with-world-stop-lock ()
      ;; First, try to position ourselves as the next thread to stop the world.
      ;; This prevents any more threads from becoming PA.
      (loop
         (when (null *world-stop-pending*)
           (setf *world-stop-pending* self)
           (return))
         ;; Wait for the world to unstop.
         (condition-wait *world-stop-cvar* *world-stop-lock*))
      ;; Now wait for any PA threads to finish.
      (loop
         (when (zerop *pseudo-atomic-thread-count*)
           (return))
         (condition-wait *world-stop-cvar* *world-stop-lock*))
      (safe-without-interrupts (self)
        (acquire-global-thread-lock)
        (setf *world-stopper* self
              *world-stop-pending* nil)
        (release-global-thread-lock))
      (quiesce-cpus-for-world-stop))
    ;; Don't hold the mutex over the thunk, it's a spinlock and disables interrupts.
    (multiple-value-prog1
        (funcall thunk)
      (with-world-stop-lock ()
        ;; Release the dogs!
        (safe-without-interrupts (self)
          (acquire-global-thread-lock)
          (setf *world-stopper* nil)
          (release-global-thread-lock))
        (condition-notify *world-stop-cvar* t)
        (broadcast-wakeup-ipi)))))

(defmacro with-world-stopped (&body body)
  `(call-with-world-stopped (dx-lambda () ,@body)))

(defun world-stopped-p ()
  "Returns true if the world is stopped."
  *world-stopper*)

(defun call-with-pseudo-atomic (thunk)
  (when (eql *world-stopper* (current-thread))
    (panic "Going PA with world stopped!"))
  (ensure-interrupts-enabled)
  (with-world-stop-lock ()
    (loop
       (when (null *world-stop-pending*)
         (return))
       ;; Don't go PA if there is a thread waiting to stop the world.
       (condition-wait *world-stop-cvar* *world-stop-lock*))
    ;; TODO: Have a list of pseudo atomic threads, and prevent PA threads
    ;; from being inspected.
    (incf *pseudo-atomic-thread-count*))
  (unwind-protect
       (let ((*pseudo-atomic* t))
         (funcall thunk))
    (with-world-stop-lock ()
      (decf *pseudo-atomic-thread-count*)
      (condition-notify *world-stop-cvar* t))))

(defmacro with-pseudo-atomic (&body body)
  `(call-with-pseudo-atomic (dx-lambda () ,@body)))
