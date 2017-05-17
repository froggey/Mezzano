;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

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
  (mezzano.lap.arm64:add :x12 :x0 (:object-literal #.+thread-fx-save-area+))
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
  (mezzano.lap.arm64:add :x12 :x0 (:object-literal #.+thread-fx-save-area+))
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
  (sys.int::%copy-words (%object-slot-address thread +thread-interrupt-save-area+)
                        (- (interrupt-frame-pointer interrupt-frame)
                           ;; 14 registers below the pointer, 6 above.
                           (* 14 8))
                        ;; For a total of 20 values to copy.
                        20)
  ;; Full state was saved.
  (setf (thread-full-save-p thread) t))

(defun %%switch-to-thread-via-wired-stack (current-thread sp fp next-thread)
  ;; Save frame pointer.
  (setf (thread-state-rbp-value current-thread) fp)
  ;; Save FPU state.
  ;; FIXME: FPU state doesn't need to be completely saved for voluntary task switches.
  ;; Only FPCR needs to be preserved.
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

(sys.int::define-lap-function set-current-thread ((thread))
  (mezzano.lap.arm64:orr :x28 :xzr :x0)
  (mezzano.lap.arm64:ret))

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

(sys.int::define-lap-function %%restore-full-save-thread ((thread))
  ;; Switch back to SP_EL0, restoring the original value of SP_EL1.
  (mezzano.lap.arm64:add :sp :x27 0)
  (mezzano.lap.arm64:msr :spsel 0)
  ;; Returning to an interrupted thread. Restore saved registers and stuff.
  ;; TODO: How to deal with footholds here? The stack might be paged out here.
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
  ;; Check for pending footholds.
  (mezzano.lap.arm64:movz :x9 (:object-literal #.+thread-pending-footholds+))
  (mezzano.lap.arm64:ldr :x2 (:x28 :x9))
  (mezzano.lap.arm64:subs :xzr :x2 :x26)
  (mezzano.lap.arm64:b.ne RUN-FOOTHOLDS)
  ;; No value return.
  NORMAL-RETURN
  (mezzano.lap.arm64:orr :x5 :xzr :xzr)
  (mezzano.lap.arm64:orr :x0 :xzr :x26)
  ;; Return.
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (mezzano.lap.arm64:ret)
  RUN-FOOTHOLDS
  (mezzano.lap.arm64:movz :x9 (:object-literal #.+thread-inhibit-footholds+))
  (mezzano.lap.arm64:ldr :x2 (:x28 :x9))
  (mezzano.lap.arm64:cbnz :x2 NORMAL-RETURN)
  ;; Jump to the support function to run the footholds.
  ;; FIXME: This should be an atomic swap.
  (mezzano.lap.arm64:movz :x9 (:object-literal #.+thread-pending-footholds+))
  (mezzano.lap.arm64:ldr :x0 (:x28 :x9))
  (mezzano.lap.arm64:str :x26 (:x28 :x9))
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ldr :x7 (:function %run-thread-footholds))
  (mezzano.lap.arm64:ldr :x9 (:object :x7 #.sys.int::+fref-entry-point+))
  (mezzano.lap.arm64:br :x9))

(sys.int::define-lap-function current-thread (())
  (mezzano.lap.arm64:orr :x0 :xzr :x28)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))

(defun arch-initialize-thread-state (thread stack-pointer)
  (setf (thread-state-rsp thread) stack-pointer
        ;; Unused.
        (thread-state-ss thread) 0
        ;; Start with interrupts unmasked, EL1, SP_EL0.
        (thread-state-rflags thread) #x00000004
        ;; x30
        (thread-state-cs thread) 0))
