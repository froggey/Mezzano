;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(sys.int::defglobal *global-thread-lock* nil
  "This lock protects the special variables that make up the thread list and run queues.")
(sys.int::defglobal *supervisor-priority-run-queue*)
(sys.int::defglobal *high-priority-run-queue*)
(sys.int::defglobal *normal-priority-run-queue*)
(sys.int::defglobal *low-priority-run-queue*)
(sys.int::defglobal *all-threads*)

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
;; FIXME: There must be one idle thread per cpu.
;; The cold-generator creates an idle thread for the BSP.
(sys.int::defglobal sys.int::*bsp-idle-thread*)

(deftype thread ()
  `(satisfies threadp))

(defun threadp (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-thread+))

;;; Thread locking.
;;;
;;; Each thread has a per-thread spinlock (the thread-lock field), and there is the *GLOBAL-THREAD-LOCK*.

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
  (field state                    1 :type (member :active :runnable :sleeping :dead :waiting-for-page :pager-request :stopped))
  ;; Spinlock protecting access to the thread.
  (field lock                     2)
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
  (field priority                16 :type (member :low :normal :high :supervisor))
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
  ;; 24-32 - free
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

;;; Locking macros.

(defmacro with-global-thread-lock ((&optional) &body body)
  `(with-symbol-spinlock (*global-thread-lock*)
     ,@body))

(defmacro with-thread-lock ((thread) &body body)
  (let ((sym (gensym "thread")))
    `(let ((,sym ,thread))
       (unwind-protect
            (progn
              (%lock-thread ,sym)
              ,@body)
         (%unlock-thread ,sym)))))

(defun %lock-thread (thread)
  (check-type thread thread)
  (ensure-interrupts-disabled)
  (let ((current-thread (current-thread)))
    (do ()
        ((sys.int::%cas-object thread
                               +thread-lock+
                               :unlocked
                               current-thread))
      (panic "thread lock " thread " held by " (sys.int::%object-ref-t thread +thread-lock+))
      (sys.int::cpu-relax))))

(defun %unlock-thread (thread)
  (assert (eql (sys.int::%object-ref-t thread +thread-lock+)
               (current-thread)))
  (setf (sys.int::%object-ref-t thread +thread-lock+) :unlocked))

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
Interrupts must be off, the current thread must be locked."
  (let ((current (current-thread)))
    (with-symbol-spinlock (*global-thread-lock*)
      (cond (*world-stopper*
             ;; World is stopped, the only runnable threads are the world stopper
             ;; or any thread at :supervisor priority.
             (unless (or (eql current *world-stopper*)
                         (eql (thread-priority current) :supervisor))
               (panic "Aiee. %UPDATE-RUN-QUEUE called with bad thread " current))
             ;; Supervisor priority threads first.
             (cond ((pop-run-queue-1 *supervisor-priority-run-queue*))
                   ((eql (thread-state *world-stopper*) :runnable)
                    ;; The world stopper is ready.
                    *world-stopper*)
                   (t ;; Switch to idle.
                    sys.int::*bsp-idle-thread*)))
            (t
             ;; Return the current thread to the run queue and fetch the next thread.
             (when (eql current sys.int::*bsp-idle-thread*)
               (panic "Aiee. Idle thread called %UPDATE-RUN-QUEUE."))
             (when (eql (thread-state current) :runnable)
               (push-run-queue current))
             (or
              ;; Try taking from the run queue.
              (pop-run-queue)
              ;; Fall back on idle.
              sys.int::*bsp-idle-thread*))))))

;;; Thread switching.

(defun thread-yield ()
  "Call this to give up the remainder of the current thread's timeslice and possibly switch to another runnable thread."
  (%run-on-wired-stack-without-interrupts (sp fp)
   (let ((current (current-thread)))
     (%lock-thread current)
     (setf (thread-state current) :runnable)
     (%reschedule-via-wired-stack sp fp))))

(defun %reschedule-via-wired-stack (sp fp)
  ;; Switch to the next thread saving minimal state.
  ;; Interrupts must be off and the current thread's lock must be held.
  ;; Releases the thread lock and reenables interrupts.
  (let ((current (current-thread))
        (next (%update-run-queue)))
    ;; todo: reset preemption timer here.
    (when (eql next current)
      ;; Staying on the same thread, unlock and return.
      (%unlock-thread current)
      (%%return-to-same-thread sp fp)
      (panic "unreachable"))
    (when (<= sys.int::*exception-stack-base*
              (thread-stack-pointer next)
              (1- sys.int::*exception-stack-size*))
      (panic "Other thread " next " stopped on exception stack!!!"))
    (%lock-thread next)
    (setf (thread-state next) :active)
    (%%switch-to-thread-via-wired-stack current sp fp next)))

(defun %reschedule-via-interrupt (interrupt-frame)
  ;; Switch to the next thread saving the full state.
  ;; Interrupts must be off and the current thread's lock must be held.
  ;; Releases the thread lock and reenables interrupts.
  (let ((current (current-thread))
        (next (%update-run-queue)))
    ;; todo: reset preemption timer here.
    ;; Avoid double-locking the thread when returning to the current thread.
    (when (not (eql next current))
      (%lock-thread next))
    (setf (thread-state next) :active)
    (%%switch-to-thread-via-interrupt current interrupt-frame next)))

(defun maybe-preempt-via-interrupt (interrupt-frame)
  (let ((current (current-thread)))
    (when (not (or (eql current *world-stopper*)
                   (eql (thread-priority current) :supervisor)
                   (eql current sys.int::*bsp-idle-thread*)))
      (%lock-thread current)
      (setf (thread-state current) :runnable)
      (%reschedule-via-interrupt interrupt-frame))))

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
          (sys.int::%object-ref-t thread +thread-lock+) :unlocked
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
          (sys.int::%object-ref-t thread +thread-lock+) :unlocked
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
    (reset-ephemeral-thread thread entry-point initial-state)
    thread))

;; MAKE-THREAD arranges for new threads to call this function with the thread's
;; initial function as an argument.
;; It sets up the top-level catch for 'terminate-thread, and deals with cleaning
;; up when the thread exits (either by normal return or by a throw to terminate-thread).
(defun thread-entry-trampoline (function)
  (unwind-protect
       (catch 'terminate-thread
         (decf (thread-inhibit-footholds (current-thread)))
         (funcall function))
    ;; Cleanup, terminate the thread.
    (thread-final-cleanup)))

;; This is seperate from thread-entry-trampoline so steppers can detect it.
(defun thread-final-cleanup ()
  (%run-on-wired-stack-without-interrupts (sp fp)
    (let ((self (current-thread)))
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
      (%reschedule-via-wired-stack sp fp))))

;; The idle thread is not a true thread. It does not appear in all-threads, nor in any run-queue.
;; When the machine boots, one idle thread is created for each core. When a core is idle, the
;; idle thread will be run.
;; FIXME: SMP-safety.
(defun idle-thread ()
  (%disable-interrupts)
  (loop
     ;; Look for a thread to switch to.
     (let ((next (with-symbol-spinlock (*global-thread-lock*)
                   (cond (*world-stopper*
                          (cond ((pop-run-queue-1 *supervisor-priority-run-queue*))
                                ((eql (thread-state *world-stopper*) :runnable)
                                 *world-stopper*)
                                (t nil)))
                         (t
                          (pop-run-queue))))))
       (cond (next
              (set-run-light t)
              ;; Switch to thread.
              (%lock-thread sys.int::*bsp-idle-thread*)
              (%lock-thread next)
              (setf (thread-state next) :active)
              (%run-on-wired-stack-without-interrupts (sp fp next)
                (%%switch-to-thread-via-wired-stack sys.int::*bsp-idle-thread* sp fp next))
              (%disable-interrupts)
              (when (boundp '*light-run*)
                ;; Clear the run light immediately so it doesn't stay on between
                ;; GUI screen updates.
                (clear-light *light-run*)))
             (t ;; Wait for an interrupt.
              (%wait-for-interrupt)
              (%disable-interrupts))))))

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
  (when priority
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
  (when priority
    (push-run-queue thread))
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
    (setf *default-stack-size* (* 256 1024)))
  (reset-ephemeral-thread sys.int::*bsp-idle-thread* #'idle-thread :sleeping nil)
  (reset-ephemeral-thread sys.int::*snapshot-thread* #'snapshot-thread :sleeping :supervisor)
  (reset-ephemeral-thread sys.int::*pager-thread* #'pager-thread :runnable :supervisor)
  (reset-ephemeral-thread sys.int::*disk-io-thread* #'disk-thread :runnable :supervisor)
  (condition-notify *world-stop-cvar* t))

(defun wake-thread (thread)
  "Wake a sleeping thread."
  (without-interrupts
    (with-thread-lock (thread)
      (with-symbol-spinlock (*global-thread-lock*)
        (setf (thread-state thread) :runnable)
        (push-run-queue thread)))))

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
    (%lock-thread thread)
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

(defun %pop-foothold ()
  (safe-without-interrupts ()
    (pop (thread-pending-footholds (current-thread)))))

(defun %run-thread-footholds (footholds)
  (loop
     for fn in footholds
     do (funcall fn))
  (values))

(defmacro without-footholds (&body body)
  (let ((thread (gensym)))
    `(unwind-protect
          (progn
            (sys.int::%atomic-fixnum-add-object (current-thread) +thread-inhibit-footholds+ 1)
            ,@body)
       (let ((,thread (current-thread)))
         (sys.int::%atomic-fixnum-add-object ,thread +thread-inhibit-footholds+ -1)
         (when (and (zerop (sys.int::%object-ref-t ,thread +thread-inhibit-footholds+))
                    (sys.int::%object-ref-t ,thread +thread-pending-footholds+))
           (%run-thread-footholds (sys.int::%xchg-object ,thread +thread-pending-footholds+ nil)))))))

(defun establish-thread-foothold (thread function)
  (loop
     (let ((old (thread-pending-footholds thread)))
       ;; Use CAS to avoid having to disable interrupts/lock the thread/etc.
       ;; Tricky to mix with allocation.
       (when (sys.int::%cas-object thread +thread-pending-footholds+
                                   old (cons function old))
         (return)))))

(defun stop-current-thread ()
  (%run-on-wired-stack-without-interrupts (sp fp)
   (let ((current (current-thread)))
     (%lock-thread current)
     (setf (thread-state current) :stopped)
     (%reschedule-via-wired-stack sp fp))))

(defun stop-thread (thread)
  "Stop a thread, waiting for it to enter the stopped state."
  (check-type thread thread)
  (assert (not (eql thread (current-thread))))
  (establish-thread-foothold thread
                             (lambda ()
                               (%run-on-wired-stack-without-interrupts (sp fp)
                                (let ((self (current-thread)))
                                  (%lock-thread self)
                                  (setf (thread-wait-item self) :stopped
                                        (thread-state self) :stopped)
                                  (%reschedule-via-wired-stack sp fp)))))
  (loop
     (when (eql (thread-state thread) :stopped)
       (return))
     (thread-yield))
  (values))

(defun resume-thread (thread &optional why)
  "Resume a stopped thread."
  (check-type thread thread)
  (assert (eql (thread-state thread) :stopped))
  (safe-without-interrupts (thread why)
    (thread-wait-item thread) why
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
           (setf *world-stopper* self
                 *world-stop-pending* nil)
           (return))
         (condition-wait *world-stop-cvar* *world-stop-lock*)))
    ;; Don't hold the mutex over the thunk, it's a spinlock and disables interrupts.
    (multiple-value-prog1
        (funcall thunk)
      (with-mutex (*world-stop-lock*)
        ;; Release the dogs!
        (setf *world-stopper* nil)
        (condition-notify *world-stop-cvar* t)))))

(defmacro with-world-stopped (&body body)
  `(call-with-world-stopped (dx-lambda () ,@body)))

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
