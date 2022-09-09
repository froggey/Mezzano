(in-package :mezzano.supervisor)

(sys.int::defglobal sys.int::*arm64-exception-vector*)
(sys.int::defglobal sys.int::*arm64-exception-vector-base*)
(sys.int::defglobal sys.int::*bsp-wired-stack*)

(defun initialize-boot-cpu ()
  (let ((sp-el1 (+ (car sys.int::*bsp-wired-stack*) (cdr sys.int::*bsp-wired-stack*))))
    (%load-cpu-bits sp-el1 (ash sp-el1 -1)
                    sys.int::*arm64-exception-vector-base*)))

(sys.int::define-lap-function %load-cpu-bits ((sp-el1 cpu-data vbar-el1))
  (:gc :no-frame :layout #*)
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
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:orr :x0 :xzr :x27)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))

(defun local-cpu ()
  (local-cpu-info))

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
  ;; Read the function out of the fref.
  (mezzano.lap.arm64:ldr :x6 (:object :x7 #.sys.int::+fref-function+))
  ;; Read the function entry point and call it.
  (mezzano.lap.arm64:ldr :x9 (:object :x6 #.sys.int::+function-entry-point+))
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
  ;; Read the function out of the fref.
  (mezzano.lap.arm64:ldr :x6 (:object :x7 #.sys.int::+fref-function+))
  ;; Read the function entry point and call it.
  (mezzano.lap.arm64:ldr :x9 (:object :x6 #.sys.int::+function-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  (mezzano.lap.arm64:hlt 4))

(defun broadcast-panic-ipi ()
  nil)

(defun broadcast-wakeup-ipi ()
  nil)

(defun quiesce-cpus-for-world-stop ()
  nil)

(defun begin-tlb-shootdown ()
  nil)

(defun tlb-shootdown-single (address)
  (declare (ignore address))
  nil)

(defun tlb-shootdown-range (base length)
  (declare (ignore base length))
  nil)

(defun tlb-shootdown-all ()
  nil)

(defun finish-tlb-shootdown ()
  nil)

(defun check-tlb-shootdown-not-in-progress ()
  nil)

(defun local-cpu-idle-thread ()
  sys.int::*bsp-idle-thread*)

(defun boot-secondary-cpus ()
  nil)

(sys.int::defglobal *n-up-cpus* 1)

(defstruct (cpu
             (:area :wired))
  state
  info-vector
  apic-id
  idle-thread
  wired-stack
  exception-stack
  irq-stack
  lapic-timer-active)

(defun logical-core-count ()
  1)

(defun preemption-timer-reset (time-remaining)
  (declare (ignore time-remaining))
  nil)

(defun preemption-timer-remaining ()
  nil)

(defun stop-other-cpus-for-debug-magic-button ()
  nil)

(defun arch-pre-panic ()
  nil)

(sys.int::define-lap-function %dmb.oshld (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:dmb.oshst)
  (mezzano.lap.arm64:ret))

(defun sys.int::dma-write-barrier ()
  (%dmb.oshld)
  (%isb))

(defun restore-page-fault-ist (state)
  (declare (ignore state))
  nil)

(sys.int::define-lap-function %dc.cvau ((address))
  "Clean data cache for the given virtual address back to the point of unification"
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:dc.cvau :x9)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %ic.ivau ((address))
  "Invalidate instruction cache for the given virtual address back to the point of unification"
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ic.ivau :x9)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %dsb.ish (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:dsb.ish)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %isb (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:isb)
  (mezzano.lap.arm64:ret))

(defun %arm64-sync-icache (start length)
  (let ((end (+ start length)))
    ;; Clear (write dirty data, but don't invalidate) data cache back to
    ;; the point of unification (where I & D caches meet)
    (loop for addr from start below end by 64
          do (%dc.cvau addr))
    ;; Ensure visibility of the data cleaned from cache.
    (%dsb.ish)
    ;; Now that the dcache is up to date at the PoU, any lines in
    ;; the icache can be invalidated back there.
    (loop for addr from start below end by 64
          do (%ic.ivau addr))
    ;; Ensure completion of the invalidations.
    (%dsb.ish)
    ;; Make sure we don't have stale instructions in the pipeline.
    (%isb)))
