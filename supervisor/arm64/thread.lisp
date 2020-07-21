(in-package :mezzano.supervisor)

(sys.int::define-lap-function %%return-to-same-thread ()
  (mezzano.lap.arm64:msr :spsel 0)
  (mezzano.lap.arm64:add :sp :x0 0)
  (mezzano.lap.arm64:orr :x29 :xzr :x1)
  (mezzano.lap.arm64:orr :x5 :xzr :xzr)
  (mezzano.lap.arm64:orr :x0 :x26 :xzr)
  (mezzano.lap.arm64:msr :daifclr #b1111)
  (:gc :no-frame)
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function save-fpu-state ((thread))
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
  (mezzano.lap.arm64:movz :x10 (:object-literal #.+thread-arm64-fpsr+))
  (mezzano.lap.arm64:str :x9 (:x0 :x10))
  (mezzano.lap.arm64:mrs :x9 :fpcr)
  (mezzano.lap.arm64:movz :x10 (:object-literal #.+thread-arm64-fpcr+))
  (mezzano.lap.arm64:str :x9 (:x0 :x10))
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function restore-fpu-state ((thread))
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
  (mezzano.lap.arm64:movz :x10 (:object-literal #.+thread-arm64-fpsr+))
  (mezzano.lap.arm64:ldr :x9 (:x0 :x10))
  (mezzano.lap.arm64:msr :fpsr :x9)
  (mezzano.lap.arm64:movz :x10 (:object-literal #.+thread-arm64-fpcr+))
  (mezzano.lap.arm64:ldr :x9 (:x0 :x10))
  (mezzano.lap.arm64:msr :fpcr :x9)
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
  (mezzano.lap.arm64:orr :x0 :xzr :x28)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))

(defun arch-initialize-thread-state (thread stack-pointer)
  (setf (thread-arm64-fpsr thread) 0
        (thread-arm64-fpcr thread) 0)
  (setf (thread-state-rsp thread) stack-pointer
        ;; Unused.
        (thread-state-ss thread) 0
        ;; Start with interrupts unmasked, EL1, SP_EL0.
        (thread-state-rflags thread) #x00000004
        ;; x30
        (thread-state-cs thread) 0))
