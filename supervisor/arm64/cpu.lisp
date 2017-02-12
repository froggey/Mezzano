;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defun initialize-boot-cpu ()
  (let* ((addr (align-up (+ (sys.int::lisp-object-address sys.int::*bsp-info-vector*)
                            (- sys.int::+tag-object+)
                            8
                            (* 1 8))
                         1024))
         (sp-el1 (+ sys.int::*bsp-wired-stack-base* sys.int::*bsp-wired-stack-size*)))
    (flet ((gen-vector (offset common entry)
             (let ((base (+ addr offset))
                   (common-entry (sys.int::%object-ref-signed-byte-64
                                  common
                                  sys.int::+function-entry-point+))
                   (entry-fref (sys.int::%object-ref-t
                                entry
                                sys.int::+symbol-function+)))
               ;; sub sp, sp, #x30. Space for the iret frame & frame pointer
               (setf (sys.int::memref-unsigned-byte-32 base 0) #xD100C3FF)
               ;; str x29, [sp]
               (setf (sys.int::memref-unsigned-byte-32 base 1) #xF90003FD)
               ;; ldr x29, [fn]
               (setf (sys.int::memref-unsigned-byte-32 base 2) #x5800005D)
               ;; b common-entry
               (let ((entry-rel (- common-entry (+ base 12))))
                 (setf (sys.int::memref-unsigned-byte-32 base 3)
                       (logior #x14000000
                               (ldb (byte 26 2) entry-rel))))
               ;; fn: entry-fref
               (setf (sys.int::memref-t base 2) entry-fref)))
           (gen-invalid (offset)
             ;; HLT #1
             (setf (sys.int::memref-unsigned-byte-32 (+ addr offset) 0) #xD4400020)))
      (declare (dynamic-extent #'gen-vector #'gen-invalid))
      (gen-vector #x000 #'%el0-common '%synchronous-el0-handler)
      (gen-vector #x080 #'%el0-common '%irq-el0-handler)
      (gen-vector #x100 #'%el0-common '%fiq-el0-handler)
      (gen-vector #x180 #'%el0-common '%serror-el0-handler)
      (gen-vector #x200 #'%elx-common '%synchronous-elx-handler)
      (gen-vector #x280 #'%elx-common '%irq-elx-handler)
      (gen-vector #x300 #'%elx-common '%fiq-elx-handler)
      (gen-vector #x380 #'%elx-common '%serror-elx-handler)
      ;; These vectors are used when the CPU moves from a lower EL.
      ;; We're always running in EL1, so these are not used.
      (dotimes (i 8)
        (gen-invalid (+ #x400 (* i #x80)))))
    (%load-cpu-bits sp-el1 (ash sp-el1 -1) addr)))

(sys.int::define-lap-function %load-cpu-bits ((sp-el1 cpu-data vbar-el1))
  ;; Switch to SP_EL1.
  (mezzano.lap.arm64:msr :spsel 1)
  ;; Unbox sp-el1.
  (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  ;; Set SP_EL1.
  (mezzano.lap.arm64:add :sp :x9 0)
  ;; Move back to SP_EL0.
  (mezzano.lap.arm64:msr :spsel 0)
  ;; Set the current CPU register.
  (mezzano.lap.arm64:orr :x27 :xzr :x1)
  ;; Set VBAR_EL1.
  (mezzano.lap.arm64:add :x9 :xzr :x2 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:msr :vbar-el1 :x9)
  (mezzano.lap.arm64:isb)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function local-cpu-info (())
  (mezzano.lap.arm64:orr :x0 :xzr :x27)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %el0-common ()
  ;; Stack looks like:
  ;; +40 pad (ss on x86-64)
  ;; +32 sp (not set)
  ;; +24 spsr (not set)
  ;; +16 x30 (cs on x86-64)
  ;; +8 pc (not set)
  ;; +0 x29 (frame pointer)
  ;; x29 contains function to branch to.
  ;; Push registers in the same order as x86-64.
  (mezzano.lap.arm64:stp :x5 :x9 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x6 :x10 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x12 :x11 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x1 :x0 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x3 :x2 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x7 :x4 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x14 :x13 (:pre :sp -16))
  ;; Flush the pad slot.
  (mezzano.lap.arm64:str :xzr (:sp #x98))
  ;; Read & save SP_EL0
  (mezzano.lap.arm64:mrs :x9 :sp-el0)
  (mezzano.lap.arm64:str :x9 (:sp #x90))
  ;; Read & save ELR_EL1
  (mezzano.lap.arm64:mrs :x9 :elr-el1)
  (mezzano.lap.arm64:str :x9 (:sp #x78))
  ;; Read & save SPSR_EL1
  (mezzano.lap.arm64:mrs :x9 :spsr-el1)
  (mezzano.lap.arm64:str :x9 (:sp #x88))
  ;; Save x30.
  (mezzano.lap.arm64:str :x30 (:sp #x80))
  ;; Set up for call to handler.
  (mezzano.lap.arm64:orr :x7 :xzr :x29)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; 1 arg.
  ;; Build frame.
  (mezzano.lap.arm64:add :x29 :sp #x70)
  ;; Build interrupt frame object.
  (mezzano.lap.arm64:sub :sp :sp 16)
  (mezzano.lap.arm64:movz :x9 #.(ash sys.int::+object-tag-interrupt-frame+ sys.int::+object-type-shift+))
  (mezzano.lap.arm64:str :x9 (:sp))
  (mezzano.lap.arm64:add :x9 :xzr :x29 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:str :x9 (:sp 8))
  (mezzano.lap.arm64:add :x0 :sp #.sys.int::+tag-object+)
  (:gc :frame :interrupt t)
  ;; Call handler.
  (mezzano.lap.arm64:ldr :x9 (:object :x7 #.sys.int::+fref-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  ;; Drop the frame.
  (mezzano.lap.arm64:add :sp :sp 16)
  ;; Restore x30.
  (mezzano.lap.arm64:ldr :x30 (:sp #x80))
  ;; Restore SPSR_EL1
  (mezzano.lap.arm64:ldr :x9 (:sp #x88))
  (mezzano.lap.arm64:msr :spsr-el1 :x9)
  ;; Restore ELR_EL1
  (mezzano.lap.arm64:ldr :x9 (:sp #x78))
  (mezzano.lap.arm64:msr :elr-el1 :x9)
  ;; Restore SP_EL0
  (mezzano.lap.arm64:ldr :x9 (:sp #x90))
  (mezzano.lap.arm64:msr :sp-el0 :x9)
  ;; Restore registers.
  (mezzano.lap.arm64:ldp :x14 :x13 (:post :sp 16))
  (mezzano.lap.arm64:ldp :x7 :x4 (:post :sp 16))
  (mezzano.lap.arm64:ldp :x3 :x2 (:post :sp 16))
  (mezzano.lap.arm64:ldp :x1 :x0 (:post :sp 16))
  (mezzano.lap.arm64:ldp :x12 :x11 (:post :sp 16))
  (mezzano.lap.arm64:ldp :x6 :x10 (:post :sp 16))
  (mezzano.lap.arm64:ldp :x5 :x9 (:post :sp 16))
  (mezzano.lap.arm64:ldr :x29 (:sp))
  (mezzano.lap.arm64:add :sp :sp #x30)
  (mezzano.lap.arm64:eret))

(sys.int::define-lap-function %elx-common ()
  ;; Stack looks like:
  ;; +40 pad (ss on x86-64)
  ;; +32 sp (not set)
  ;; +24 spsr (not set)
  ;; +16 x30 (cs on x86-64)
  ;; +8 pc (not set)
  ;; +0 x29 (frame pointer)
  ;; x29 contains function to branch to.
  ;; Push registers in the same order as x86-64.
  (mezzano.lap.arm64:stp :x5 :x9 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x6 :x10 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x12 :x11 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x1 :x0 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x3 :x2 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x7 :x4 (:pre :sp -16))
  (mezzano.lap.arm64:stp :x14 :x13 (:pre :sp -16))
  ;; Flush the pad slot.
  (mezzano.lap.arm64:str :xzr (:sp #x98))
  ;; Read & save SP.
  (mezzano.lap.arm64:add :x9 :sp 0)
  (mezzano.lap.arm64:str :x9 (:sp #x90))
  ;; Read & save ELR_EL1
  (mezzano.lap.arm64:mrs :x9 :elr-el1)
  (mezzano.lap.arm64:str :x9 (:sp #x78))
  ;; Read & save SPSR_EL1
  (mezzano.lap.arm64:mrs :x9 :spsr-el1)
  (mezzano.lap.arm64:str :x9 (:sp #x88))
  ;; Save x30.
  (mezzano.lap.arm64:str :x30 (:sp #x80))
  ;; Set up for call to handler.
  (mezzano.lap.arm64:orr :x7 :xzr :x29)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; 1 arg.
  ;; Build frame.
  (mezzano.lap.arm64:add :x29 :sp #x70)
  ;; Build interrupt frame object.
  (mezzano.lap.arm64:sub :sp :sp 16)
  (mezzano.lap.arm64:movz :x9 #.(ash sys.int::+object-tag-interrupt-frame+ sys.int::+object-type-shift+))
  (mezzano.lap.arm64:str :x9 (:sp))
  (mezzano.lap.arm64:add :x9 :xzr :x29 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:str :x9 (:sp 8))
  (mezzano.lap.arm64:add :x0 :sp #.sys.int::+tag-object+)
  (:gc :frame :interrupt t)
  ;; Call handler.
  (mezzano.lap.arm64:ldr :x9 (:object :x7 #.sys.int::+fref-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  (mezzano.lap.arm64:hlt 4))
