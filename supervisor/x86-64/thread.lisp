(in-package :mezzano.supervisor)

(sys.int::define-lap-function fxsave ((address))
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:fxsave (:rax))
  (sys.lap-x86:ret))

(sys.int::define-lap-function fxrstor ((address))
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:fxrstor (:rax))
  (sys.lap-x86:ret))

(sys.int::define-lap-function %%return-to-same-thread ()
  (sys.lap-x86:mov64 :rsp :r8)
  (sys.lap-x86:mov64 :rbp :r9)
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:sti)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret))

(defun save-fpu-state (thread)
  (fxsave (mezzano.runtime::%object-slot-address thread +thread-fxsave-area+)))

(defun restore-fpu-state (thread)
  (fxrstor (mezzano.runtime::%object-slot-address thread +thread-fxsave-area+)))

(defun save-interrupted-state (thread interrupt-frame)
  ;; Copy the interrupt frame over to the save area.
  (sys.int::%copy-words (mezzano.runtime::%object-slot-address thread +thread-interrupt-save-area+)
                        (- (interrupt-frame-pointer interrupt-frame)
                           ;; 14 registers below the pointer, 6 above.
                           (* 14 8))
                        ;; For a total of 20 values to copy.
                        20)
  ;; Full state was saved.
  (setf (thread-full-save-p thread) t))

(defun set-current-thread (thread)
  (setf (sys.int::msr +msr-ia32-gs-base+) (sys.int::lisp-object-address thread)))

(sys.int::define-lap-function %%restore-full-save-thread ((thread))
  ;; Drop the global thread lock.
  ;; This must be done here, not in %%switch-to-thread-common to prevent
  ;; another CPU from switching on to the old thread's stack while it is
  ;; still in use.
  (sys.lap-x86:mov64 :r9 (:symbol-global-cell *global-thread-lock*))
  (sys.lap-x86:mov64 :r10 (:constant :unlocked))
  (sys.lap-x86:mov64 (:object :r9 #.sys.int::+symbol-value-cell-value+) :r10)
  ;; Returning to an interrupted thread. Restore saved registers and stuff.
  (sys.lap-x86:lea64 :rsp (:object :r8 #.+thread-interrupt-save-area+))
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

(sys.int::define-lap-function %%restore-partial-save-thread ((thread))
  ;; Drop the global thread lock.
  ;; This must be done here, not in %%switch-to-thread-common to prevent
  ;; another CPU from switching on to the old thread's stack while it is
  ;; still in use.
  (sys.lap-x86:mov64 :r9 (:symbol-global-cell *global-thread-lock*))
  (sys.lap-x86:mov64 :r10 (:constant :unlocked))
  (sys.lap-x86:mov64 (:object :r9 #.sys.int::+symbol-value-cell-value+) :r10)
  ;; Restore stack pointer.
  (sys.lap-x86:mov64 :rsp (:object :r8 #.+thread-state-rsp+))
  ;; Restore frame pointer.
  (sys.lap-x86:mov64 :rbp (:object :r8 #.+thread-state-rbp+))
  ;; Reenable interrupts. Must be done before touching the thread stack.
  (sys.lap-x86:sti)
  (:gc :no-frame :layout #*0)
  ;; No value return.
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:mov64 :r8 nil)
  ;; Return, restoring RIP.
  (sys.lap-x86:ret))

(sys.int::define-lap-function current-thread (())
  ENTRY-POINT
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:test64 :rcx :rcx)
  (sys.lap-x86:jnz BAD-ARGS)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 :r8 (:object nil #.+thread-self+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  BAD-ARGS
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:lea64 :rbx (:rip (+ (- ENTRY-POINT 16) #.sys.int::+tag-object+)))
  (sys.lap-x86:jmp (:named-call sys.int::raise-invalid-argument-error)))

(defun arch-initialize-thread-state (thread stack-pointer)
  ;; Initialize the FXSAVE area.
  ;; All FPU/SSE interrupts masked, round to nearest,
  ;; x87 using 80 bit precision (long-float).
  ;; FCW
  (setf (thread-fxsave-area thread 0) #x7F
        (thread-fxsave-area thread 1) #x03)
  ;; MXCSR
  (setf (thread-fxsave-area thread 24) #x80
        (thread-fxsave-area thread 25) #x1F
        (thread-fxsave-area thread 26) #x00
        (thread-fxsave-area thread 27) #x00)
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

(sys.int::define-lap-function %%partial-save-return-thunk (())
  (:gc :frame :interrupt t)
  ;; Restore MV area.
  (sys.lap-x86:mov64 :rcx #.+thread-mv-slots-size+)
  (sys.lap-x86:lea64 :rsi (:rsp #.(+ (* 20 8) 512)))
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 :rdi (:object nil #.+thread-self+))
  (sys.lap-x86:lea64 :rdi (:object :rdi #.+thread-mv-slots+))
  (sys.lap-x86:rep)
  (sys.lap-x86:movs64)
  ;; Restore FPU state.
  (sys.lap-x86:fxrstor (:rsp #.(* 20 8)))
  ;; Restore GPRs.
  (sys.lap-x86:mov64 :r15 (:rsp #.(* 0 8)))
  (sys.lap-x86:mov64 :r14 (:rsp #.(* 1 8)))
  (sys.lap-x86:mov64 :r13 (:rsp #.(* 2 8)))
  (sys.lap-x86:mov64 :r12 (:rsp #.(* 3 8)))
  (sys.lap-x86:mov64 :r11 (:rsp #.(* 4 8)))
  (sys.lap-x86:mov64 :r10 (:rsp #.(* 5 8)))
  (sys.lap-x86:mov64 :r9 (:rsp #.(* 6 8)))
  (sys.lap-x86:mov64 :r8 (:rsp #.(* 7 8)))
  (sys.lap-x86:mov64 :rdi (:rsp #.(* 8 8)))
  (sys.lap-x86:mov64 :rsi (:rsp #.(* 9 8)))
  (sys.lap-x86:mov64 :rbx (:rsp #.(* 10 8)))
  (sys.lap-x86:mov64 :rdx (:rsp #.(* 11 8)))
  (sys.lap-x86:mov64 :rcx (:rsp #.(* 12 8)))
  (sys.lap-x86:mov64 :rax (:rsp #.(* 13 8)))
  (sys.lap-x86:mov64 :rbp (:rsp #.(* 14 8)))
  ;; Now return.
  (sys.lap-x86:add64 :rsp #.(* 15 8))
  (:gc :no-frame :interrupt t)
  (sys.lap-x86:iret))

(defun stack-space-required-for-force-call-on-thread (thread)
  (declare (ignore thread))
  (+ 16 ; Return address and more alignment
     (* +thread-mv-slots-size+ 8) ; Saved MV area.
     512 ; FPU state
     (* 20 8))) ; GPRs

(defun convert-thread-to-partial-save (thread)
  (when (thread-full-save-p thread)
    ;; Push the current full save state on the stack and create an interrupt frame.
    (let ((sp (thread-state-rsp thread)))
      (decf sp (+ (* +thread-mv-slots-size+ 8)
                  512
                  (* 20 8)))
      ;; Align the stack.
      (setf sp (logand sp (lognot 15)))
      ;; Save the MV area.
      (sys.int::%copy-words (+ sp 512 (* 20 8))
                            (mezzano.runtime::%object-slot-address thread +thread-mv-slots+)
                            +thread-mv-slots-size+)
      ;; Save the FPU state.
      (sys.int::%copy-words (+ sp (* 20 8))
                            (mezzano.runtime::%object-slot-address thread +thread-fxsave-area+)
                            (truncate 512 8))
      ;; Save the interrupt state, 20 elements.
      (sys.int::%copy-words sp
                            (mezzano.runtime::%object-slot-address thread +thread-interrupt-save-area+)
                            20)
      ;; Push the address of the return thunk
      (decf sp 8)
      (setf (sys.int::memref-unsigned-byte-64 sp) (sys.int::%object-ref-unsigned-byte-64
                                                   #'%%partial-save-return-thunk
                                                   sys.int::+function-entry-point+))
      (setf (thread-state-rsp thread) sp
            (thread-state-rbp thread) (+ sp (* 15 8))
            (thread-full-save-p thread) nil))))

(defun force-call-on-thread (thread function &optional (argument nil argumentp))
  (convert-thread-to-partial-save thread)
  (setf (thread-state-rip thread) (sys.int::%object-ref-unsigned-byte-64
                                   function
                                   sys.int::+function-entry-point+)
        (thread-state-rcx-value thread) (if argumentp 1 0)
        (thread-state-rbx-value thread) function
        (thread-state-r8-value thread) argument
        (thread-state-r9-value thread) nil
        (thread-state-r10-value thread) nil
        (thread-state-r11-value thread) nil
        (thread-state-r12-value thread) nil
        (thread-state-r13-value thread) nil
        (thread-state-cs thread) #x08
        (thread-state-ss thread) #x00
        (thread-state-rflags thread) #x202
        (thread-full-save-p thread) t))

(defun stop-thread-for-single-step (interrupt-frame)
  (let ((self (current-thread)))
    (acquire-global-thread-lock)
    (setf (thread-state self) :stopped
          (thread-wait-item self) :single-step-trap))
    (%reschedule-via-interrupt interrupt-frame))

(defun wait-for-thread-stop (thread)
  (loop
     ;; Take the global thread lock when inspecting the state to
     ;; synchronize properly with STOP-THREAD-FOR-SINGLE-STEP.
     (let ((sync-state (safe-without-interrupts (thread)
                         (with-global-thread-lock ()
                           (thread-state thread)))))
       (when (member sync-state '(:stopped :dead))
         (return)))
     (thread-yield)))

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
  (wait-for-thread-stop thread)
  ;; Clear the single-step bit.
  (setf (thread-state-rflags thread) (logand (thread-state-rflags thread)
                                             (lognot (ash 1 8))))
  (values))
