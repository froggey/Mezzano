;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

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

(sys.int::define-lap-function current-thread (())
  (sys.lap-x86:test64 :rcx :rcx)
  (sys.lap-x86:jnz BAD-ARGS)
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 :r8 (:object nil #.+thread-self+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  BAD-ARGS
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-invalid-argument-error))
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2))
