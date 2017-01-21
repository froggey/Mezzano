;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(sys.int::define-lap-function %%return-to-same-thread ()
  (sys.lap-x86:mov64 :rsp :r8)
  (sys.lap-x86:mov64 :rbp :r9)
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:sti)
  (:gc :no-frame :layout #*0)
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

;;; current-thread interrupt-frame next-thread
;;; Interrupts must be off, current & next must be locked.
(sys.int::define-lap-function %%switch-to-thread-via-interrupt ()
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
  (sys.lap-x86:mov32 :ecx #.+msr-ia32-gs-base+)
  (sys.lap-x86:mov64 :rax :r9)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  (sys.lap-x86:wrmsr)
  ;; Restore fpu state.
  (sys.lap-x86:gs)
  (sys.lap-x86:fxrstor (:object nil #.+thread-fx-save-area+))
  ;; Drop the locks on both threads. Must be done before touching the thread stack.
  (sys.lap-x86:mov64 :r10 (:constant :unlocked))
  (sys.lap-x86:cmp64 :r9 :r8)
  (sys.lap-x86:je SWITCH-TO-SAME-THREAD)
  (sys.lap-x86:mov64 (:object :r9 #.+thread-lock+) :r10)
  SWITCH-TO-SAME-THREAD
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
  (:gc :no-frame :layout #*0)
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

(sys.int::define-lap-function current-thread (())
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:test64 :rcx :rcx)
  (sys.lap-x86:jnz BAD-ARGS)
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 :r8 (:object nil #.+thread-self+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  BAD-ARGS
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-invalid-argument-error))
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2))

(defun arch-initialize-thread-state (thread stack-pointer)
  ;; Push a fake return address on the stack, this keeps the stack aligned correctly.
  (setf (sys.int::memref-unsigned-byte-64 (decf stack-pointer 8) 0) 0)
  (setf (thread-state-rsp thread) stack-pointer
        (thread-state-ss thread) 0
        ;; Start with interrupts enabled.
        (thread-state-rflags thread) #x202
        ;; Kernel code segment (defined in cpu.lisp).
        (thread-state-cs thread) 8))

(sys.int::define-lap-function %%full-save-return-thunk (())
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret))

(defun convert-thread-to-full-save (thread)
  (when (not (thread-full-save-p thread))
    (setf (thread-state-rcx-value thread) 0
          (thread-state-rbx-value thread) nil
          (thread-state-r8-value thread) nil
          (thread-state-r9-value thread) nil
          (thread-state-r10-value thread) nil
          (thread-state-r11-value thread) nil
          (thread-state-r12-value thread) nil
          (thread-state-r13-value thread) nil
          (thread-state-rip thread) (sys.int::%object-ref-unsigned-byte-64
                                     #'%%full-save-return-thunk
                                     sys.int::+function-entry-point+)
          (thread-state-cs thread) #x08
          (thread-state-ss thread) #x00
          (thread-state-rflags thread) #x202
          (thread-full-save-p thread) t)))

(defun stop-thread-for-single-step (interrupt-frame)
  (let ((self (current-thread)))
    (%lock-thread self)
    (setf (thread-state self) :stopped
          (thread-wait-item self) :single-step-trap))
    (%reschedule-via-interrupt interrupt-frame))

(defun single-step-thread (thread)
  (check-type thread thread)
  (assert (eql (thread-state thread) :stopped))
  ;; If the thread is not in the full-save state, then convert it.
  (convert-thread-to-full-save thread)
  ;; Set the single-step bit in rflags.
  (setf (thread-state-rflags thread) (logior (thread-state-rflags thread)
                                             (ash 1 8)))
  ;; Resume the thread & wait for it to stop.
  (resume-thread thread :single-step)
  ;; FIXME: this will need to lock the thread before reading the state
  ;; or on SMP it will wake before the saved state is written.
  (loop
     (when (eql (thread-state thread) :stopped)
       (return))
     (thread-yield))
  ;; Clear the single-step bit.
  (setf (thread-state-rflags thread) (logand (thread-state-rflags thread)
                                             (lognot (ash 1 8))))
  (values))
