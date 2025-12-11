(in-package :mezzano.supervisor)

(sys.int::define-lap-function %%return-to-same-thread ()
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:msr :spsel 0)
  (mezzano.lap.arm64:add :sp :x0 0)
  (:gc :frame)
  (mezzano.lap.arm64:orr :x29 :xzr :x1)
  (mezzano.lap.arm64:orr :x5 :xzr :xzr)
  (mezzano.lap.arm64:orr :x0 :x26 :xzr)
  (mezzano.lap.arm64:msr :daifclr #b1111)
  (:gc :no-frame :layout #*00)
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function save-fpu-state ((thread))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:add :x12 :x0 (:object-literal #.+thread-fxsave-area+))
  (mezzano.lap.arm64:stp :q0 :q1 (:post :x12 32))
  (mezzano.lap.arm64:stp :q2 :q3 (:post :x12 32))
  (mezzano.lap.arm64:stp :q4 :q5 (:post :x12 32))
  (mezzano.lap.arm64:stp :q6 :q7 (:post :x12 32))
  (mezzano.lap.arm64:stp :q8 :q9 (:post :x12 32))
  (mezzano.lap.arm64:stp :q10 :q11 (:post :x12 32))
  (mezzano.lap.arm64:stp :q12 :q13 (:post :x12 32))
  (mezzano.lap.arm64:stp :q14 :q15 (:post :x12 32))
  (mezzano.lap.arm64:stp :q16 :q17 (:post :x12 32))
  (mezzano.lap.arm64:stp :q18 :q19 (:post :x12 32))
  (mezzano.lap.arm64:stp :q20 :q21 (:post :x12 32))
  (mezzano.lap.arm64:stp :q22 :q23 (:post :x12 32))
  (mezzano.lap.arm64:stp :q24 :q25 (:post :x12 32))
  (mezzano.lap.arm64:stp :q26 :q27 (:post :x12 32))
  (mezzano.lap.arm64:stp :q28 :q29 (:post :x12 32))
  (mezzano.lap.arm64:stp :q30 :q31 (:post :x12 32))
  (mezzano.lap.arm64:mrs :x9 :fpsr)
  (mezzano.lap.arm64:mrs :x10 :fpcr)
  (mezzano.lap.arm64:bfi :x9 :x10 32 32)
  (mezzano.lap.arm64:movz :x10 (:object-literal #.+thread-state-ss+))
  (mezzano.lap.arm64:str :x9 (:x0 :x10))
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function restore-fpu-state ((thread))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:add :x12 :x0 (:object-literal #.+thread-fxsave-area+))
  (mezzano.lap.arm64:ldp :q0 :q1 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q2 :q3 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q4 :q5 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q6 :q7 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q8 :q9 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q10 :q11 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q12 :q13 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q14 :q15 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q16 :q17 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q18 :q19 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q20 :q21 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q22 :q23 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q24 :q25 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q26 :q27 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q28 :q29 (:post :x12 32))
  (mezzano.lap.arm64:ldp :q30 :q31 (:post :x12 32))
  (mezzano.lap.arm64:movz :x10 (:object-literal #.+thread-state-ss+))
  (mezzano.lap.arm64:ldr :x9 (:x0 :x10))
  (mezzano.lap.arm64:ubfx :x10 :x9 32 32)
  (mezzano.lap.arm64:msr :fpcr :x10)
  (mezzano.lap.arm64:bfi :x9 :xzr 32 32)
  (mezzano.lap.arm64:msr :fpsr :x9)
  (mezzano.lap.arm64:ret))

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

(sys.int::define-lap-function set-current-thread ((thread))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:orr :x28 :xzr :x0)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %%restore-full-save-thread ((thread))
  ;; Drop the global thread lock.
  ;; This must be done here, not in %%switch-to-thread-common to prevent
  ;; another CPU from switching on to the old thread's stack while it is
  ;; still in use.
  (mezzano.lap.arm64:ldr :x1 (:symbol-global-cell *global-thread-lock*))
  (mezzano.lap.arm64:ldr :x2 (:constant :unlocked))
  (mezzano.lap.arm64:str :x2 (:object :x1 #.sys.int::+symbol-value-cell-value+))
  ;; Switch back to SP_EL0, restoring the original value of SP_EL1.
  (mezzano.lap.arm64:add :sp :x27 0)
  (mezzano.lap.arm64:msr :spsel 0)
  ;; Returning to an interrupted thread. Restore saved registers and stuff.
  (mezzano.lap.arm64:movz :x9 (:object-literal #.+thread-state-rsp+))
  (mezzano.lap.arm64:ldr :x9 (:x0 :x9))
  (mezzano.lap.arm64:add :sp :x9 :xzr)
  ;; Set up for exception return.
  (mezzano.lap.arm64:movz :x9 (:object-literal #.+thread-state-cs+))
  (mezzano.lap.arm64:ldr :x30 (:x0 :x9))
  (mezzano.lap.arm64:movz :x9 (:object-literal #.+thread-state-rbp+))
  (mezzano.lap.arm64:ldr :x29 (:x0 :x9))
  (mezzano.lap.arm64:movz :x9 (:object-literal #.+thread-state-rflags+))
  (mezzano.lap.arm64:ldr :x9 (:x0 :x9))
  (mezzano.lap.arm64:msr :spsr-el1 :x9)
  ;; Enable MDSCR.SS if we're single-stepping.
  (mezzano.lap.arm64:tbz :x9 #.+spsr-ss+ L1)
  (mezzano.lap.arm64:mrs :x9 :mdscr-el1)
  (mezzano.lap.arm64:orr :x9 :x9 #.(ash 1 +mdscr-ss+))
  (mezzano.lap.arm64:msr :mdscr-el1 :x9)
  (mezzano.lap.arm64:isb)
  L1
  (mezzano.lap.arm64:movz :x9 (:object-literal #.+thread-state-rip+))
  (mezzano.lap.arm64:ldr :x9 (:x0 :x9))
  (mezzano.lap.arm64:msr :elr-el1 :x9)
  (mezzano.lap.arm64:movz :x9 (:object-literal #.+thread-interrupt-save-area+))
  (mezzano.lap.arm64:add :x9 :x0 :x9)
  (mezzano.lap.arm64:ldp :x14 :x13 (:post :x9 16))
  (mezzano.lap.arm64:ldp :x7 :x4 (:post :x9 16))
  (mezzano.lap.arm64:ldp :x3 :x2 (:post :x9 16))
  (mezzano.lap.arm64:ldp :x1 :x0 (:post :x9 16))
  (mezzano.lap.arm64:ldp :x12 :x11 (:post :x9 16))
  (mezzano.lap.arm64:ldp :x6 :x10 (:post :x9 16))
  (mezzano.lap.arm64:ldp :x5 :x9 (:x9))
  (mezzano.lap.arm64:eret))

(sys.int::define-lap-function %%restore-partial-save-thread ((thread))
  ;; Drop the global thread lock.
  ;; This must be done here, not in %%switch-to-thread-common to prevent
  ;; another CPU from switching on to the old thread's stack while it is
  ;; still in use.
  (mezzano.lap.arm64:ldr :x1 (:symbol-global-cell *global-thread-lock*))
  (mezzano.lap.arm64:ldr :x2 (:constant :unlocked))
  (mezzano.lap.arm64:str :x2 (:object :x1 #.sys.int::+symbol-value-cell-value+))
  ;; Switch back to SP_EL0, restoring the original value of SP_EL1.
  (mezzano.lap.arm64:add :sp :x27 0)
  (mezzano.lap.arm64:msr :spsel 0)
  ;; Restore stack pointer.
  (mezzano.lap.arm64:movz :x9 (:object-literal #.+thread-state-rsp+))
  (mezzano.lap.arm64:ldr :x9 (:x28 :x9))
  (mezzano.lap.arm64:add :sp :x9 0)
  ;; Restore frame pointer.
  (mezzano.lap.arm64:movz :x9 (:object-literal #.+thread-state-rbp+))
  (mezzano.lap.arm64:ldr :x29 (:x28 :x9))
  ;; Reenable interrupts. Must be done before touching the thread stack.
  (mezzano.lap.arm64:msr :daifclr #b1111)
  (:gc :no-frame)
  ;; No value return.
  (mezzano.lap.arm64:orr :x5 :xzr :xzr)
  (mezzano.lap.arm64:orr :x0 :xzr :x26)
  ;; Return.
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function current-thread (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:orr :x0 :xzr :x28)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))

(defconstant +initial-fpsr/fpcr+ 0)
;; Start with interrupts unmasked, EL1, SP_EL0.
(defconstant +initial-spsr+ #x00000004)

(defun arch-initialize-thread-state (thread stack-pointer)
  (setf (thread-state-rsp thread) stack-pointer
        ;; Packed fpsr/fpcr.
        (thread-state-ss thread) +initial-fpsr/fpcr+
        ;; SPSR
        (thread-state-rflags thread) +initial-spsr+
        ;; x30
        (thread-state-cs thread) 0))

;; This thunk is used to patch things up when a full-save state
;; was converted to a partial save state.
(sys.int::define-lap-function %%partial-save-return-thunk ()
  (:gc :frame :interrupt t)
  ;; Call out to a helper function to do the return via the
  ;; pager. This leaves the interrupt frame intact and avoids
  ;; awkward issues around interrupts that ERET causes. x86's IRET
  ;; does not have this issue as it atomically restores much more
  ;; state directly from the stack. No special registers to be
  ;; clobbered by a badly timed interrupt.
  ;; Note that this does not use the normal pager-rpc mechanism
  ;; due to issues around footholds.
  (mezzano.lap.arm64:brk 28))

(defun partial-save-return-helper (interrupt-frame)
  (let ((self (current-thread)))
    (setf (thread-pager-argument-1 self) self
          (thread-pager-argument-2 self) (interrupt-frame-raw-register interrupt-frame :rsp)
          (thread-pager-argument-3 self) nil)
    (with-symbol-spinlock (*pager-lock*)
      (acquire-global-thread-lock)
      (setf (thread-state self) :pager-request
            (thread-wait-item self) 'partial-save-return-helper-in-pager
            (thread-queue-next self) *pager-waiting-threads*
            *pager-waiting-threads* self)
      (when (and (eql (thread-state sys.int::*pager-thread*) :sleeping)
                 (eql (thread-wait-item sys.int::*pager-thread*) '*pager-waiting-threads*))
        (setf (thread-state sys.int::*pager-thread*) :runnable)
        (push-run-queue sys.int::*pager-thread*)))
    (%reschedule-via-interrupt interrupt-frame)))

(defun partial-save-return-helper-in-pager (thread interrupt-frame ignore3)
  (declare (ignore ignore3))
  (pager-log-op "arm64-partial-save-return-helper " thread " " interrupt-frame)
  ;; FIXME: Make sure the stack is paged in (see pager-invoke-function-on-thread)
  ;; This is why we're doing this in the pager, instead of the interrupt handler.
  ;; Restore the MV area.
  (sys.int::%copy-words (mezzano.runtime::%object-slot-address thread +thread-mv-slots+)
                        (+ interrupt-frame 512 (* 20 8))
                        +thread-mv-slots-size+)
  ;; Save the FPU state.
  (sys.int::%copy-words (mezzano.runtime::%object-slot-address thread +thread-fxsave-area+)
                        (+ interrupt-frame (* 20 8))
                        (truncate 512 8))
  ;; Save the interrupt state, 20 elements.
  (sys.int::%copy-words (mezzano.runtime::%object-slot-address thread +thread-interrupt-save-area+)
                        interrupt-frame
                        20)
  (setf (thread-full-save-p thread) t))

(defun stack-space-required-for-force-call-on-thread (thread)
  (declare (ignore thread))
  (+ 16 ; Return address and more alignment
     (* +thread-mv-slots-size+ 8) ; Saved MV area.
     512 ; FPU state
     (* 20 8)))

(defun convert-thread-to-partial-save (thread)
  (when (thread-full-save-p thread)
    ;; Push the current full save state on the stack and create an interrupt frame.
    (let ((sp (thread-state-rsp thread)))
      (decf sp (+ (* +thread-mv-slots-size+ 8)
                  512
                  (* 20 8)))
      ;; Stack must always be aligned on ARM64
      (when (not (zerop (logand sp 15)))
        (panic "Misaligned thread stack! " thread))
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
      ;; Push frame pointer, keeping the stack aligned
      (decf sp 8)
      (setf (sys.int::memref-unsigned-byte-64 sp) (+ sp (* 16 8)))
      (setf (thread-state-rsp thread) sp
            (thread-state-rbp thread) (+ sp (* 16 8))
            (thread-full-save-p thread) nil))))

(defun convert-thread-to-full-save (thread)
  (when (not (thread-full-save-p thread))
    ;; Pop saved fp & lr off the stack.
    (let ((fp (sys.int::memref-unsigned-byte-64 (thread-state-rsp thread) 0))
          (lr (sys.int::memref-unsigned-byte-64 (thread-state-rsp thread) 1)))
      (incf (thread-state-rsp thread) 16)
      ;; Make sure to flush the value registers
      ;; and also set up for a 0-value return.
      (setf (thread-state-rcx-value thread) 0
            (thread-state-rbx-value thread) nil
            (thread-state-r8-value thread) nil
            (thread-state-r9-value thread) nil
            (thread-state-r10-value thread) nil
            (thread-state-r11-value thread) nil
            (thread-state-r12-value thread) nil
            (thread-state-r13-value thread) nil
            (thread-state-cs thread) lr ; lr
            (thread-state-rip thread) lr
            (thread-state-rbp thread) fp
            (thread-state-ss thread) +initial-fpsr/fpcr+
            (thread-state-rflags thread) +initial-spsr+
            (thread-full-save-p thread) t))))

(defun force-call-on-thread (thread function &optional (argument nil argumentp))
  (convert-thread-to-partial-save thread)
  ;; Pop saved fp & lr off the stack.
  (let ((fp (sys.int::memref-unsigned-byte-64 (thread-state-rsp thread) 0))
        (lr (sys.int::memref-unsigned-byte-64 (thread-state-rsp thread) 1)))
    (incf (thread-state-rsp thread) 16)
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
          (thread-state-ss thread) +initial-fpsr/fpcr+
          (thread-state-rflags thread) +initial-spsr+
          (thread-state-cs thread) lr
          (thread-state-rbp thread) fp
          (thread-full-save-p thread) t)))

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
  ;; Set the single-step bit in spsr
  (setf (thread-state-rflags thread) (logior (thread-state-rflags thread)
                                             (ash 1 +spsr-ss+)))
  ;; Resume the thread & wait for it to stop.
  (resume-thread thread :single-step)
  (wait-for-thread-stop thread)
  ;; Clear the single-step bit.
  (setf (thread-state-rflags thread) (logand (thread-state-rflags thread)
                                             (lognot (ash 1 +spsr-ss+))))
  (values))
