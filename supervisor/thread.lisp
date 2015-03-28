;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

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
;;  4 ** free (ish)
;;  5 Wait item.
;;    If a thread is sleeping or waiting for page, this will describe what it's waiting for.
;;    When waiting for paging to complete, this will be the faulting address.
;;  6 special stack pointer
;;    The thread's current special stack pointer.
;;    Note! The compiler must be updated if this changes and all code rebuilt.
;;  7 full-save-p
;;    When true, all registers are saved in the the thread's state save area.
;;    When false, only the stack pointer and frame pointer are valid.
;;  8 ** free
;;  9 %next
;;    Forward link to the next thread in whatever list the thread is in.
;; 10 %prev
;;    Backward link to the previous thread in whatever list the thread is in.
;; 11 ** free
;; 12 ** free
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
;; 427-446 State save area.
;;    Used to save an interrupt frame when the thread has stopped to wait for a page.
;;    The registers are saved here, not on the stack, because the stack may not be paged in.
;;    This has the same layout as an interrupt frame.
;;   +0 R15
;;   +1 R14
;;   +2 R13
;;   +3 R12
;;   +4 R11
;;   +5 R10
;;   +6 R9
;;   +7 R8
;;   +8 RDI
;;   +9 RSI
;;  +10 RBX
;;  +11 RDX
;;  +12 RCX
;;  +13 RAX
;;  +14 RBP (frame pointer)
;;  +15 RIP
;;  +16 CS
;;  +17 RFLAGS
;;  +18 RSP (stack pointer)
;;  +19 SS
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
                    (setf (,accessor thread ,field-name) value)))))
          (reg-field (name offset)
            (let ((state-name (intern (format nil "STATE-~A" name) (symbol-package name)))
                  (state-name-value (intern (format nil "STATE-~A-VALUE" name) (symbol-package name))))
              `(progn
                 (field ,state-name ,offset :accessor sys.int::%array-like-ref-signed-byte-64)
                 (field ,state-name-value ,offset :accessor sys.int::%array-like-ref-t)))))
  (field name                     0)
  (field state                    1 :type (member :active :runnable :sleeping :dead :waiting-for-page))
  (field lock                     2)
  (field stack                    3)
  ;; 4 - magic field used by bootloader.
  (field wait-item                5)
  (field special-stack-pointer    6)
  (field full-save-p              7)
  ;; 8
  (field %next                    9)
  (field %prev                   10)
  (field pending-footholds       11)
  (field inhibit-footholds       12)
  (field mutex-stack             13)
  (field global-next             14)
  (field global-prev             15)
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

(defun thread-frame-pointer (thread)
  (thread-state-rbp thread))

(defun (setf thread-frame-pointer) (value thread)
  (setf (thread-state-rbp thread) value))

(defun thread-stack-pointer (thread)
  (thread-state-rsp thread))

(defun (setf thread-stack-pointer) (value thread)
  (setf (thread-state-rsp thread) value))

(defconstant +thread-mv-slots-start+ 32)
(defconstant +thread-mv-slots-end+ 128)
(defconstant +thread-tls-slots-start+ 128)
(defconstant +thread-tls-slots-end+ 427)
(defconstant +thread-interrupt-save-area+ 427)
(defconstant +thread-fx-save-area+ 447)

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
  ;; FIXME: need to make the GC aware of partially initialized threads.
  (let* ((thread (mezzano.runtime::%allocate-object sys.int::+object-tag-thread+ 0 511 :wired))
         (stack (%allocate-stack stack-size)))
    (setf (sys.int::%array-like-ref-t thread +thread-name+) name
          (sys.int::%array-like-ref-t thread +thread-state+) :runnable
          (sys.int::%array-like-ref-t thread +thread-lock+) :unlocked
          (sys.int::%array-like-ref-t thread +thread-stack+) stack
          (sys.int::%array-like-ref-t thread +thread-special-stack-pointer+) nil
          (sys.int::%array-like-ref-t thread +thread-wait-item+) nil
          (sys.int::%array-like-ref-t thread +thread-mutex-stack+) nil
          (sys.int::%array-like-ref-t thread +thread-pending-footholds+) '()
          (sys.int::%array-like-ref-t thread +thread-inhibit-footholds+) 1)
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
    ;; Set up the initial register state.
    (let ((stack-pointer (+ (stack-base stack) (stack-size stack)))
          (trampoline #'thread-entry-trampoline))
      ;; Push a fake return address on the stack, this keeps the stack aligned correctly.
      (setf (sys.int::memref-unsigned-byte-64 (decf stack-pointer 8) 0) 0)
      ;; Initialize state save area.
      (setf (thread-state-ss thread) 0
            (thread-state-rsp thread) stack-pointer
            ;; Start with interrupts enabled.
            (thread-state-rflags thread) #x202
            ;; Kernel code segment (defined in cpu.lisp).
            (thread-state-cs thread) 8
            ;; Trampoline entry point.
            (thread-state-rip thread) (sys.int::%array-like-ref-signed-byte-64 trampoline 0)
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
    (setf (thread-full-save-p thread) t)
    (safe-without-interrupts (thread)
      (with-symbol-spinlock (*global-thread-lock*)
        (push-run-queue thread)
        ;; Add thread to global thread list.
        (setf (thread-global-prev *all-threads*) thread
              (thread-global-next thread) *all-threads*
              (thread-global-prev thread) nil
              *all-threads* thread)))
    thread))

(defun thread-entry-trampoline (function)
  (let ((self (current-thread)))
    (unwind-protect
         (catch 'terminate-thread
           (decf (thread-inhibit-footholds self))
           (funcall function))
      ;; Cleanup, terminate the thread.
      (safe-without-interrupts (self)
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
        (%reschedule)))))

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

(defun reset-ephemeral-thread (thread entry-point state)
  ;; Set up the initial register state.
  (let ((stack-pointer (+ (stack-base (thread-stack thread))
                          (stack-size (thread-stack thread))))
        (function (sys.int::%coerce-to-callable entry-point)))
    ;; Push a fake return address on the stack, this keeps the stack aligned correctly.
    (setf (sys.int::memref-unsigned-byte-64 (decf stack-pointer 8) 0) 0)
    ;; Initialize state save area.
    (setf (thread-state-ss thread) 0
          (thread-state-rsp thread) stack-pointer
          ;; Start with interrupts enabled.
          (thread-state-rflags thread) #x202
          ;; Kernel code segment (defined in cpu.lisp).
          (thread-state-cs thread) 8
          ;; Entry point.
          (thread-state-rip thread) (sys.int::%array-like-ref-signed-byte-64 function 0)
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
  (setf (thread-state thread) state
        (sys.int::%array-like-ref-t thread +thread-special-stack-pointer+) nil
        (sys.int::%array-like-ref-t thread +thread-wait-item+) nil
        (sys.int::%array-like-ref-t thread +thread-mutex-stack+) nil
        (sys.int::%array-like-ref-t thread +thread-pending-footholds+) '()
        (sys.int::%array-like-ref-t thread +thread-inhibit-footholds+) 1
        (thread-full-save-p thread) t)
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
    (setf *world-stop-lock* (make-mutex "World stop lock")
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

(defun %reschedule-via-wired-stack (sp fp)
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

(sys.int::define-lap-function %%return-to-same-thread ()
  (sys.lap-x86:mov64 :rsp :r8)
  (sys.lap-x86:mov64 :rbp :r9)
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:sti)
  (:gc :no-frame)
  (sys.lap-x86:ret))

(sys.int::define-lap-function %%switch-to-thread-via-wired-stack ()
  ;; Save frame pointer.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:object nil #.+thread-state-rbp+) :r10)
  ;; Save fpu state.
  (sys.lap-x86:gs)
  (sys.lap-x86:fxsave (:object nil #.+thread-fx-save-area+))
  ;; Save stack pointer.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:object nil #.+thread-state-rsp+) :r9)
  ;; Only partial state was saved.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:object nil #.+thread-full-save-p+) nil)
  ;; Jump to common function.
  (sys.lap-x86:mov64 :r9 :r11)
  (sys.lap-x86:mov64 :r13 (:function %%switch-to-thread-common))
  (sys.lap-x86:jmp (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8)))))

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
  (sys.lap-x86:mov64 (:object nil #.+thread-state-rbp+) :rbp)
  ;; Save fpu state.
  (sys.lap-x86:gs)
  (sys.lap-x86:fxsave (:object nil #.+thread-fx-save-area+))
  ;; Save stack pointer.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:object nil #.+thread-state-rsp+) :rsp)
  ;; Only partial state was saved.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:object nil #.+thread-full-save-p+) nil)
  ;; Jump to common function.
  (sys.lap-x86:mov64 :r13 (:function %%switch-to-thread-common))
  (sys.lap-x86:jmp (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8)))))

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
  (without-interrupts
    (with-thread-lock (thread)
      (with-symbol-spinlock (*global-thread-lock*)
        (setf (thread-state thread) :runnable)
        (push-run-queue thread)))))

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
  ;; Full state was saved.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:object nil #.+thread-full-save-p+) t)
  ;; Jump to common function.
  (sys.lap-x86:mov64 :r9 :r10) ; next-thread
  (sys.lap-x86:mov64 :r13 (:function %%switch-to-thread-common))
  (sys.lap-x86:jmp (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8)))))

;; (current-thread new-thread)
(sys.int::define-lap-function %%switch-to-thread-common ()
  ;; Old thread's state has been saved, restore the new-thread's state.
  ;; Switch threads.
  (sys.lap-x86:mov32 :ecx #.sys.int::+msr-ia32-gs-base+)
  (sys.lap-x86:mov64 :rax :r9)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  (sys.lap-x86:wrmsr)
  ;; Restore fpu state.
  (sys.lap-x86:gs)
  (sys.lap-x86:fxrstor (:object nil #.+thread-fx-save-area+))
  ;; Drop the locks on both threads. Must be done before touching the thread stack.
  (sys.lap-x86:mov64 :r10 (:constant :unlocked))
  (sys.lap-x86:mov64 (:object :r9 #.+thread-lock+) :r10)
  (sys.lap-x86:mov64 (:object :r8 #.+thread-lock+) :r10)
  ;; Check if the thread is in the interrupt save area.
  (sys.lap-x86:gs)
  (sys.lap-x86:cmp64 (:object nil #.+thread-full-save-p+) nil)
  (sys.lap-x86:jne FULL-RESTORE)
  ;; Restore stack pointer.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 :rsp (:object nil #.+thread-state-rsp+))
  ;; Restore frame pointer.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 :rbp (:object nil #.+thread-state-rbp+))
  ;; Reenable interrupts. Must be done before touching the thread stack.
  (sys.lap-x86:sti)
  (:gc :no-frame)
  ;; Check for pending footholds.
  (sys.lap-x86:gs)
  (sys.lap-x86:cmp64 (:object nil #.+thread-pending-footholds+) nil)
  (sys.lap-x86:jne RUN-FOOTHOLDS)
  ;; No value return.
  NORMAL-RETURN
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:mov64 :r8 nil)
  ;; Return, restoring RIP.
  (sys.lap-x86:ret)
  RUN-FOOTHOLDS
  (sys.lap-x86:gs)
  (sys.lap-x86:cmp64 (:object nil #.+thread-inhibit-footholds+) 0)
  (sys.lap-x86:jne NORMAL-RETURN)
  ;; Jump to the support function to run the footholds.
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:gs)
  (sys.lap-x86:xchg64 (:object nil #.+thread-pending-footholds+) :r8)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:mov64 :r13 (:function %run-thread-footholds))
  (sys.lap-x86:jmp (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  ;; Returning to an interrupted thread. Restore saved registers and stuff.
  ;; TODO: How to deal with footholds here? The stack might be paged out here.
  FULL-RESTORE
  (sys.lap-x86:lea64 :rsp (:object :r9 #.+thread-interrupt-save-area+))
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
            (sys.int::%atomic-fixnum-add-array-like (current-thread) +thread-inhibit-footholds+ 1)
            ,@body)
       (let ((,thread (current-thread)))
         (sys.int::%atomic-fixnum-add-array-like ,thread +thread-inhibit-footholds+ -1)
         (when (and (zerop (sys.int::%array-like-ref-t ,thread +thread-inhibit-footholds+))
                    (sys.int::%array-like-ref-t ,thread +thread-pending-footholds+))
           (%run-thread-footholds (sys.int::%xchg-array-like ,thread +thread-pending-footholds+ nil)))))))

(defun establish-thread-foothold (thread function)
  (loop
     (let ((old (thread-pending-footholds thread)))
       ;; Use CAS to avoid having to disable interrupts/lock the thread/etc.
       ;; Tricky to mix with allocation.
       (when (sys.int::%cas-array-like thread +thread-pending-footholds+
                                       old (cons function old))
         (return)))))

(defun terminate-thread (thread)
  (establish-thread-foothold
   thread
   (lambda ()
     (throw 'terminate-thread nil))))

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
    (ensure-interrupts-enabled)
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
  (ensure-interrupts-enabled)
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

(defstruct (mutex
             (:include wait-queue)
             (:constructor make-mutex (&optional name))
             (:area :wired))
  ;; When NIL, the lock is free, otherwise is set to
  ;; the thread that holds the lock.
  (owner nil) ; must be slot 5, after wait-queue is included.
  (stack-next nil))

(defun acquire-mutex (mutex &optional (wait-p t))
  (let ((self (current-thread)))
    (ensure-interrupts-enabled)
    (unless (not *pseudo-atomic*)
      (panic "Trying to acquire mutex " mutex " while pseudo-atomic."))
    ;; Fast path - try to lock.
    (when (sys.int::%cas-struct-slot mutex 5 nil self)
      ;; We got it.
      (setf (mutex-stack-next mutex) (thread-mutex-stack self)
            (thread-mutex-stack self) mutex)
      (return-from acquire-mutex t))
    ;; Idiot check.
    (unless (not (mutex-held-p mutex))
      (error "Recursive locking detected on ~S." mutex))
    (when wait-p
      (%call-on-wired-stack-without-interrupts
       #'acquire-mutex-slow-path nil mutex self)
      t)))

(defun acquire-mutex-slow-path (sp fp mutex self)
  ;; Slow path.
  (lock-wait-queue mutex)
  ;; Try to acquire again, release may have been running.
  (when (sys.int::%cas-struct-slot mutex 5 nil self)
    ;; We got it.
    (setf (mutex-stack-next mutex) (thread-mutex-stack self)
          (thread-mutex-stack self) mutex)
    (unlock-wait-queue mutex)
    (return-from acquire-mutex-slow-path))
  ;; No good, have to sleep.
  ;; Add to wait queue. Release will directly transfer ownership
  ;; to this thread.
  (push-wait-queue self mutex)
  ;; Now sleep.
  ;; Must take the thread lock before dropping the mutex lock or release
  ;; may be able to remove the thread from the sleep queue before it goes
  ;; to sleep.
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
            (panic "Deadlock detected!~%~
Current thread ~S locking ~S, held by ~S, waiting on lock ~S!"
                   current
                   mutex
                   owner
                   lock))))))
  (unlock-wait-queue mutex)
  (setf (thread-wait-item self) mutex
        (thread-state self) :sleeping)
  (%reschedule-via-wired-stack sp fp))

(defun mutex-held-p (mutex)
  "Return true if this thread holds MUTEX."
  (eql (mutex-owner mutex) (current-thread)))

(defun release-mutex (mutex)
  (unless (mutex-held-p mutex)
    (panic "Trying to release mutex " mutex " not held by thread."))
  (safe-without-interrupts (mutex)
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
  (values))

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
       (%call-on-wired-stack-without-interrupts
        (lambda (sp fp condition-variable mutex)
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
        nil condition-variable mutex)
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
  ;; cause %C-O-W-S-W-I to return NIL, which is actually a success result.
  (not (%call-on-wired-stack-without-interrupts
        (lambda (sp fp semaphore wait-p)
          (let ((self (current-thread)))
            (lock-wait-queue semaphore)
            (cond ((not (zerop (semaphore-value semaphore)))
                   (decf (semaphore-value semaphore))
                   (unlock-wait-queue semaphore)
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
                     t))))
          nil semaphore wait-p)))

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
  (%call-on-wired-stack-without-interrupts
   (lambda (sp fp latch)
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
   nil latch)
  (values))

(defun latch-trigger (latch)
  (safe-without-interrupts (latch)
    (with-wait-queue-lock (latch)
      (setf (latch-state latch) t)
      ;; Loop until all the threads have been woken.
      (do ()
          ((null (condition-variable-head latch)))
        (wake-thread (pop-wait-queue latch))))))

(defstruct (irq-fifo
             (:area :wired)
             (:constructor %make-irq-fifo))
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (size)
  (element-type)
  (buffer (error "no buffer supplied") :read-only t)
  (count (make-semaphore 0))
  (lock (place-spinlock-initializer)))

(defun make-irq-fifo (size &key (element-type 't))
  ;; TODO: non-t element types.
  (%make-irq-fifo :size size
                  :buffer (sys.int::make-simple-vector size :wired)
                  :element-type 't))

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
             (:constructor (make-fifo (size &key (element-type 't) &aux (buffer (make-array size :element-type element-type))))))
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
    (condition-notify (fifo-cv fifo))))
