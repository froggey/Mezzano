(in-package :mezzano.supervisor)

(sys.int::defglobal *arm64-exception-vector*)
(sys.int::defglobal *arm64-exception-vector-base*)
(sys.int::defglobal *bsp-wired-stack*)
(sys.int::defglobal *bsp-cpu*)

(sys.int::defglobal *n-up-cpus*)
(sys.int::defglobal *cpus*)

(defstruct (arm64-cpu
            (:area :wired)
            (:include cpu)
            :slot-locations)
  self
  (state :offline :type (member :offline :online :timed-out))
  cpu-id
  idle-thread
  wired-stack
  (sp-el1 0)
  page-fault-hook)

(defun initialize-boot-cpu ()
  (setf (arm64-cpu-self *bsp-cpu*) *bsp-cpu*)
  (setf (arm64-cpu-state *bsp-cpu*) :online)
  (setf (arm64-cpu-idle-thread *bsp-cpu*)
        sys.int::*bsp-idle-thread*)
  (setf (arm64-cpu-wired-stack *bsp-cpu*) *bsp-wired-stack*)
  (setf (arm64-cpu-sp-el1 *bsp-cpu*)
        (+ (car *bsp-wired-stack*) (cdr *bsp-wired-stack*) -16))
  (setf (sys.int::memref-unsigned-byte-64 (arm64-cpu-sp-el1 *bsp-cpu*))
        (sys.int::lisp-object-address *bsp-cpu*))
  (%load-cpu-bits (arm64-cpu-sp-el1 *bsp-cpu*)
                  (ash (arm64-cpu-sp-el1 *bsp-cpu*) -1)
                  *arm64-exception-vector-base*)
  (setf *n-up-cpus* 1)
  (setf *cpus* '()))

(defconstant +spsr-ss+ 21)

(defconstant +mdscr-ss+ 0)
(defconstant +mdscr-kde+ 13)
(defconstant +mdscr-mde+ 15)

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
  ;; Configure MDSCR_EL1, enable KDE, & MDE. Delay enabling SS until we actually
  ;; want to single-step something.
  (mezzano.lap.arm64:mrs :x9 :mdscr-el1)
  (mezzano.lap.arm64:orr :x9 :x9 #.(ash 1 +mdscr-kde+))
  (mezzano.lap.arm64:orr :x9 :x9 #.(ash 1 +mdscr-mde+))
  (mezzano.lap.arm64:msr :mdscr-el1 :x9)
  (mezzano.lap.arm64:isb)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function local-cpu-info (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:orr :x9 :xzr :x27)
  (mezzano.lap.arm64:ldr :x0 (:x9))
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))

(defun local-cpu ()
  (local-cpu-info))

(defun initialize-cpu ()
  (setf (arm64-cpu-cpu-id *bsp-cpu*) (fdt-boot-cpuid))
  (setf (cpu-cpu-index *bsp-cpu*) 0)
  (push-wired *bsp-cpu* *cpus*))

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
  ;; Clear MDSCR_EL1.SS
  (mezzano.lap.arm64:mrs :x9 :mdscr-el1)
  (mezzano.lap.arm64:bfc :x9 #.+mdscr-ss+ 1)
  (mezzano.lap.arm64:msr :mdscr-el1 :x9)
  (mezzano.lap.arm64:isb)
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
  ;; Enable MDSCR.SS if we're single-stepping.
  (mezzano.lap.arm64:tbz :x9 #.+spsr-ss+ L1)
  (mezzano.lap.arm64:mrs :x9 :mdscr-el1)
  (mezzano.lap.arm64:orr :x9 :x9 #.(ash 1 +mdscr-ss+))
  (mezzano.lap.arm64:msr :mdscr-el1 :x9)
  (mezzano.lap.arm64:isb)
  L1
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
  ;; Clear MDSCR_EL1.SS
  (mezzano.lap.arm64:mrs :x9 :mdscr-el1)
  (mezzano.lap.arm64:bfc :x9 #.+mdscr-ss+ 1)
  (mezzano.lap.arm64:msr :mdscr-el1 :x9)
  (mezzano.lap.arm64:isb)
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
  (send-ipi-to-all +panic-sgi-id+))

(defun panic-ipi-handler (interrupt-frame)
  (declare (ignore interrupt-frame))
  (loop (%arch-panic-stop)))

(defun broadcast-wakeup-ipi ()
  (dolist (cpu *cpus*)
    (when (eql (arm64-cpu-state cpu) :online)
      (wake-cpu cpu))))

(defun wake-cpu (cpu)
  (when (cpu-idle-p cpu)
    (send-ipi-to-cpu cpu +wakeup-sgi-id+)))

(sys.int::defglobal *non-quiescent-cpus-remaining*)

;; FIXME: quiesce-cpus-for-world-stop needs to prevent migration across CPUs.
(defun quiesce-cpus-for-world-stop ()
  "Bring all CPUs to a consistent state to stop the world.
Protected by the world stop lock."
  (setf *non-quiescent-cpus-remaining* (1- *n-up-cpus*))
  (send-ipi-to-all +quiesce-sgi-id+)
  ;; FIXME: Use WFE/SEV instead of this spin-loop.
  (loop
     (when (eql *non-quiescent-cpus-remaining* 0)
       (return))
     (sys.int::cpu-relax)))

;; Save the current thread's state and switch to the CPU's idle thread.
(defun quiesce-ipi-handler (interrupt-frame)
  (cond (*debug-magic-button-hold-variable*
         (magic-button-ipi-handler interrupt-frame))
        (t
         (let* ((current (current-thread))
                (idle (local-cpu-idle-thread))
                (was-active (not (eql current idle))))
           (when was-active
             (acquire-global-thread-lock)
             (setf (thread-state current) :runnable)
             (push-run-queue current)
             (preemption-timer-reset nil)
             (save-fpu-state current)
             (save-interrupted-state current interrupt-frame)
             (setf (thread-state idle) :active))
           (sys.int::%atomic-fixnum-add-symbol '*non-quiescent-cpus-remaining* -1)
           (when was-active
             (%%switch-to-thread-common idle idle))))))

;; TODO: This needs to be fixed up to prevent multiple CPUs hitting it at
;; once. It can't currently happen because it is only used from IRQ handlers
;; and IRQs are only sent to the BSP.
(sys.int::defglobal *debug-magic-button-hold-variable*)
(sys.int::defglobal *debug-magic-button-ready-variable*)

(defun stop-other-cpus-for-debug-magic-button ()
  (setf *debug-magic-button-ready-variable* (1- *n-up-cpus*)
        *debug-magic-button-hold-variable* t)
  (send-ipi-to-all +quiesce-sgi-id+)
  ;; Wait for other CPUs to arrive, this ensures the thread state is actually
  ;; consistent.
  (loop until (eql *debug-magic-button-ready-variable* 0)))

(defun resume-other-cpus-for-debug-magic-button ()
  (setf *debug-magic-button-ready-variable* (1- *n-up-cpus*)
        *debug-magic-button-hold-variable* nil)
  ;; Wait for other CPUs to leave, this ensures they've all seen
  ;; the hold variable going to NIL.
  (loop until (eql *debug-magic-button-ready-variable* 0)))

(defun magic-button-ipi-handler (interrupt-frame)
  ;; Save the current thread state so it looks approximately correct.
  (let ((current (current-thread)))
    (save-fpu-state current)
    (save-interrupted-state current interrupt-frame))
  (sys.int::%atomic-fixnum-add-symbol
   '*debug-magic-button-ready-variable* -1)
  (loop while *debug-magic-button-hold-variable*)
  (sys.int::%atomic-fixnum-add-symbol
   '*debug-magic-button-ready-variable* -1))

;; TLB shootdown isn't required as ARM has cross-core TLB invalidation instructions

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
  (arm64-cpu-idle-thread (local-cpu-info)))

;; This will be called from %%pe-bootstrap. It needs to report that the cpu
;; is online, reenable interrupts, do any final registration, then fall
;; into the idle thread.
(defun %pe-entry-point ()
  (configure-gic-cpu)
  (let ((old (sys.int::cas (arm64-cpu-state (local-cpu)) :offline :online)))
    (when (not (eql old :offline))
      ;; The system decided that this CPU failed to come up for some reason.
      (loop
        (%arch-panic-stop))))
  (increment-n-running-cpus)
  (idle-thread))

(defun register-secondary-cpu (cpu-id)
  (let* ((idle-thread (make-ephemeral-thread #'%pe-entry-point
                                             :runnable
                                             :name (sys.int::cons-in-area
                                                    "Idle Thread" cpu-id
                                                    :wired)
                                             :priority :idle))
         (wired-stack (%allocate-stack (* 128 1024) t))
         (cpu (make-arm64-cpu :state :offline
                               :cpu-id cpu-id
                               :idle-thread idle-thread
                               :wired-stack wired-stack
                               :sp-el1 (+ (stack-base wired-stack)
                                          (stack-size wired-stack)
                                          -16))))
    (setf (cpu-cpu-index cpu) (length *cpus*))
    (setf (arm64-cpu-self cpu) cpu)
    (setf (sys.int::memref-unsigned-byte-64 (arm64-cpu-sp-el1 cpu))
          (sys.int::lisp-object-address cpu))
    (debug-print-line "Registered new CPU " cpu " " idle-thread " with ID " cpu-id)
    (push-wired cpu *cpus*)))

(defun detect-secondary-cpus ()
  (let* ((boot-cpu (fdt-boot-cpuid))
         (cpus (fdt-get-named-child-node (fdt-root) "cpus")))
    (debug-print-line "Boot cpu is " boot-cpu)
    (debug-print-line "cpus: " cpus)
    (when cpus
      (do-fdt-child-nodes (node cpus)
        (when (or (fdt-compatible-p node "arm,arm-v8")
                  (fdt-compatible-p node "arm,cortex-a57"))
          (let ((id (fdt-read-u32 (fdt-get-property node "reg"))))
            (when (not (eql id boot-cpu))
              (register-secondary-cpu id))))))))

(sys.int::defglobal *pe-bootstrap-address*)

(defun boot-cpu (cpu)
  (debug-print-line "Booting CPU " cpu "/" (arm64-cpu-cpu-id cpu))
  (psci-cpu-on (arm64-cpu-cpu-id cpu) *pe-bootstrap-address*
               (sys.int::lisp-object-address cpu))
  ;; Wait for the CPU to come up.
  (let ((start-time (get-internal-run-time)))
    (loop
      (when (eql (arm64-cpu-state cpu) :online)
        (return))
      (when (> (- (get-internal-run-time) start-time)
               (* 5 internal-time-units-per-second))
        (sys.int::cas (arm64-cpu-state cpu) :offline :timed-out)
        (return))))
  (case (arm64-cpu-state cpu)
    (:online
     (incf *n-up-cpus*)
     (debug-print-line "CPU " cpu "/" (arm64-cpu-cpu-id cpu) " booted"))
    (t
     (debug-print-line "CPU " cpu "/" (arm64-cpu-cpu-id cpu) " timed out"))))

(defun boot-secondary-cpus ()
  (setf *pe-bootstrap-address* (initialize-pe-bootstrap-data))
  (detect-secondary-cpus)
  (dolist (cpu *cpus*)
    (when (eql (arm64-cpu-state cpu) :offline)
      (boot-cpu cpu))))

(defun logical-core-count ()
  (length *cpus*))

(defun preemption-timer-reset (time-remaining)
  (declare (ignore time-remaining))
  nil)

(defun preemption-timer-remaining ()
  nil)

(defun arch-pre-panic ()
  nil)

(defun restore-page-fault-ist (state)
  (declare (ignore state))
  nil)

(defmacro define-system-register-accessors ()
  `(progn
     ,@(loop for (name) in mezzano.lap.arm64::*system-registers*
             for symbol = (intern (format nil "%~A" name))
             collect `(sys.int::define-lap-function ,symbol (())
                        (:gc :no-frame :layout #*)
                        (mezzano.lap.arm64:mrs :x9 ,name)
                        (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
                        (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
                        (mezzano.lap.arm64:ret))
             collect `(sys.int::define-lap-function (setf ,symbol) ((value))
                        (:gc :no-frame :layout #*)
                        (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
                        (mezzano.lap.arm64:msr ,name :x9)
                        (mezzano.lap.arm64:ret)))))

(define-system-register-accessors)

;; First function invoked when bringing up a PE.
;; This will initialize the core, bringing it from cache/mmu off,
;; all the way to running in the lisp address space and performing
;; the initial thread switch.
(sys.int::define-lap-function %%pe-bootstrap ()
  ;; Invalidate caches
  (mezzano.lap.arm64:ic.iallu)
  (mezzano.lap.arm64:isb)
  ;; Drop to EL1 if booted in EL2
  (mezzano.lap.arm64:mrs :x9 :current-el)
  (mezzano.lap.arm64:cmp :x9 #.(ash 1 2))
  (mezzano.lap.arm64:b.eq at-el1)
  ;; Return to 64-bit EL1
  (mezzano.lap.arm64:mov :x9 #x80000000) ; RW=64-bit, no other traps or virtualization.
  (mezzano.lap.arm64:msr :hcr-el2 :x9)
  (mezzano.lap.arm64:adr :x9 at-el1)
  (mezzano.lap.arm64:msr :elr-el2 :x9)
  (mezzano.lap.arm64:mov :x9 #x000003C5) ; DAIF set, returning to AArch64 EL1h (sp is sp_el1)
  (mezzano.lap.arm64:msr :spsr-el2 :x9)
  (mezzano.lap.arm64:isb)
  (mezzano.lap.arm64:eret)
  at-el1
  ;; Caches on
  (mezzano.lap.arm64:mrs :x9 :sctlr-el1)
  (mezzano.lap.arm64:orr :x9 :x9 #.(ash 1 12)) ; Enable icache
  (mezzano.lap.arm64:orr :x9 :x9 #.(ash 1 2))  ; Enable dcache/ucache
  ;; TODO: Enable this. Not possible at the moment because
  ;; %APPLY will briefly unalign the stack.
  ;;(mezzano.lap.arm64:orr :x9 :x9 #.(ash 1 3))  ; Force stack alignment
  (mezzano.lap.arm64:msr :sctlr-el1 :x9)
  ;; Invalidate TLB
  (mezzano.lap.arm64:tlbi.vmalle1)
  (mezzano.lap.arm64:isb)
  (mezzano.lap.arm64:dsb.sy)
  ;; Initialize Memory Attribute Indirection Register
  ;; Must match the ARM64_MAIR_foo_MEMORY defines in arm64/mmu.h
  ;; Index 0 configured as normal cachable memory.
  ;; Index 1 configured as normal write-through memory. (??)
  ;; Index 2 configured as Device-nGnRnE memory.
  (mezzano.lap.arm64:mov :x9 #x00AAFF)
  (mezzano.lap.arm64:msr :mair-el1 :x9)
  ;; Initialize TCR_EL1
  (mezzano.lap.arm64:mov :x9 #.(logior
                                (ash 1 38) ; TBI1: top byte ignored
                                (ash 1 37) ; TBI0: top byte ignored
                                (ash 5 32) ; IPS: 48-bit intermediate physical address.
                                (ash 2 30) ; TG1: 4k granule (different value to TG0)
                                (ash 3 28) ; SH1: inner shareable page tables
                                (ash 1 26) ; ORGN1: write back, write allocate
                                (ash 1 24) ; IRGN1: write back, write allocate
                                (ash 16 16); T1SZ: 48-bit
                                (ash 0 14) ; TG0: 4k graunle (different value to TG1)
                                (ash 3 12) ; SH0: inner shareable page tables
                                (ash 1 10) ; ORGN1: write back, write allocate
                                (ash 1 8)  ; IRGN1: write back, write allocate
                                (ash 16 0))); T0SZ: 48-bit
  (mezzano.lap.arm64:msr :tcr-el1 :x9)
  (mezzano.lap.arm64:isb)
  ;; Load the transition translation table.
  (mezzano.lap.arm64:ldr :x9 (:constant %%initial-ttbr0-el1%%))
  (mezzano.lap.arm64:msr :ttbr0-el1 :x9)
  (mezzano.lap.arm64:ldr :x9 (:constant %%initial-ttbr1-el1%%))
  (mezzano.lap.arm64:msr :ttbr1-el1 :x9)
  (mezzano.lap.arm64:isb)
  ;; Turn on the MMU
  (mezzano.lap.arm64:mrs :x9 :sctlr-el1)
  (mezzano.lap.arm64:orr :x9 :x9 #x1)
  (mezzano.lap.arm64:msr :sctlr-el1 :x9)
  (mezzano.lap.arm64:isb)
  ;; We're now running with paging enabled on the identity page in
  ;; the transition table. The pmap exists and is mapped in.
  ;; Next steps are to branch to the remaining code there, then
  ;; to switch to true translation table, do final register setup
  ;; and then jump to lisp.
  (mezzano.lap.arm64:adr :x9 jump-to-pmap)
  (mezzano.lap.arm64:mov :x10 #.+physical-map-base+)
  (mezzano.lap.arm64:add :x9 :x9 :x10)
  (mezzano.lap.arm64:br :x9)
  jump-to-pmap
  ;; Now running in the pmap.
  ;; Switch over to the final page tables.
  (mezzano.lap.arm64:ldr :x9 (:constant %%ttbr0-el1%%))
  (mezzano.lap.arm64:msr :ttbr0-el1 :x9)
  (mezzano.lap.arm64:ldr :x9 (:constant %%ttbr1-el1%%))
  (mezzano.lap.arm64:msr :ttbr1-el1 :x9)
  (mezzano.lap.arm64:isb)
  ;; Invalidate TLB again
  (mezzano.lap.arm64:tlbi.vmalle1)
  (mezzano.lap.arm64:isb)
  (mezzano.lap.arm64:dsb.sy)
  ;; Final register setup.
  ;; Enable the FP and SIMD registers and instructions in EL0 and EL1.
  (mezzano.lap.arm64:mrs :x9 :cpacr-el1)
  (mezzano.lap.arm64:orr :x9 :x9 #.(ash 3 20))
  (mezzano.lap.arm64:msr :cpacr-el1 :x9)
  (mezzano.lap.arm64:isb)
  ;; Set the exception base register
  (mezzano.lap.arm64:ldr :x9 (:constant %%vbar-el1%%))
  (mezzano.lap.arm64:msr :vbar-el1 :x9)
  (mezzano.lap.arm64:isb)
  ;; Configure MDSCR_EL1, enable KDE, & MDE. Delay enabling SS until we actually
  ;; want to single-step something.
  (mezzano.lap.arm64:mrs :x9 :mdscr-el1)
  (mezzano.lap.arm64:orr :x9 :x9 #.(ash 1 +mdscr-kde+))
  (mezzano.lap.arm64:orr :x9 :x9 #.(ash 1 +mdscr-mde+))
  (mezzano.lap.arm64:msr :mdscr-el1 :x9)
  (mezzano.lap.arm64:isb)
  ;; Initialize cpu register - this is the only per-cpu context we get.
  (mezzano.lap.arm64:ldr :x27 (:object-location :x0 #.+arm64-cpu-sp-el1+))
  (mezzano.lap.arm64:asr :x27 :x27 1) ; defixnum
  ;; Initialize thread register, start with the idle thread.
  (mezzano.lap.arm64:ldr :x28 (:object-location :x0 #.+arm64-cpu-idle-thread+))
  ;; Clear everything except a few core registers
  (mezzano.lap.arm64:mov :x0 :xzr)
  (mezzano.lap.arm64:mov :x1 :xzr)
  (mezzano.lap.arm64:mov :x2 :xzr)
  (mezzano.lap.arm64:mov :x3 :xzr)
  (mezzano.lap.arm64:mov :x4 :xzr)
  (mezzano.lap.arm64:mov :x5 :xzr)
  (mezzano.lap.arm64:mov :x6 :xzr)
  (mezzano.lap.arm64:mov :x7 :xzr)
  (mezzano.lap.arm64:mov :x8 :xzr)
  (mezzano.lap.arm64:mov :x9 :xzr)
  (mezzano.lap.arm64:mov :x10 :xzr)
  (mezzano.lap.arm64:mov :x11 :xzr)
  (mezzano.lap.arm64:mov :x12 :xzr)
  (mezzano.lap.arm64:mov :x13 :xzr)
  (mezzano.lap.arm64:mov :x14 :xzr)
  (mezzano.lap.arm64:mov :x15 :xzr)
  (mezzano.lap.arm64:mov :x16 :xzr)
  (mezzano.lap.arm64:mov :x17 :xzr)
  (mezzano.lap.arm64:mov :x18 :xzr)
  (mezzano.lap.arm64:mov :x19 :xzr)
  (mezzano.lap.arm64:mov :x20 :xzr)
  (mezzano.lap.arm64:mov :x21 :xzr)
  (mezzano.lap.arm64:mov :x22 :xzr)
  (mezzano.lap.arm64:mov :x23 :xzr)
  (mezzano.lap.arm64:mov :x24 :xzr)
  (mezzano.lap.arm64:mov :x25 :xzr)
  (mezzano.lap.arm64:ldr :x26 (:constant nil)) ; nil register
  ;; Not x27, contains cpu data
  ;; Not x28, contains current thread
  (mezzano.lap.arm64:mov :x29 :xzr) ; frame pointer
  (mezzano.lap.arm64:mov :x30 :xzr) ; link register
  ;; Restore SP_EL1
  (mezzano.lap.arm64:mov :sp :x27)
  ;; Perform the initial thread switch, loading saved state from the current thread.
  (mezzano.lap.arm64:msr :spsel 0) ; switch back to sp_el0
  (mezzano.lap.arm64:movz :x9 (:object-literal #.+thread-state-rsp+))
  (mezzano.lap.arm64:ldr :x9 (:x28 :x9))
  (mezzano.lap.arm64:add :sp :x9 0)
  ;; Don't enable interrupts at this point.
  ;; This is fine, even though we're running with SP_EL0, because
  ;; the initial thread we switch on to (the idle thread) will
  ;; always have a wired stack.
  (:gc :no-frame)
  ;; Thread will be in the full-save state, however to minimize code
  ;; here I'm just going to assume a zero-argument call. This is true
  ;; of fresh ephemeral threads!
  (mezzano.lap.arm64:movz :x9 (:object-literal #.+thread-state-rip+))
  (mezzano.lap.arm64:ldr :x9 (:x28 :x9))
  (mezzano.lap.arm64:orr :x5 :xzr :xzr) ; zero arguments
  (mezzano.lap.arm64:br :x9))

;; Copy the bootstrap assembly somewhere in memory and build the initial translation tables.
(defun initialize-pe-bootstrap-data ()
  ;; We could refer directly to the bootstrap code, however if it crosses
  ;; a page boundary that makes constructing translation tables difficult.
  (let* ((bootstrap-frame (allocate-physical-pages 1 :mandatory-p "PE bootstrap code"))
         (bootstrap-page (ash bootstrap-frame 12))
         ;; Allocate all the levels of the lower-half transition translation table.
         ;; This will have exactly one page mapped, the bootstrap code.
         ;; Since the pmap exists in the higher half, the final translation table
         ;; can just be used for the transition.
         (initial-ttl0-frame (allocate-physical-pages 1 :mandatory-p "Initial TTL0"))
         (initial-ttl0 (ash initial-ttl0-frame 12))
         (initial-ttl1-frame (allocate-physical-pages 1 :mandatory-p "Initial TTL1"))
         (initial-ttl1 (ash initial-ttl1-frame 12))
         (initial-ttl2-frame (allocate-physical-pages 1 :mandatory-p "Initial TTL2"))
         (initial-ttl2 (ash initial-ttl2-frame 12))
         (initial-ttl3-frame (allocate-physical-pages 1 :mandatory-p "Initial TTL3"))
         (initial-ttl3 (ash initial-ttl3-frame 12))
         (trampoline #'%%pe-bootstrap))
    (zeroize-page (convert-to-pmap-address bootstrap-page))
    ;; Copy code, skipping the header.
    (loop for i from 16 below (sys.int::function-code-size trampoline)
          do (setf (physical-memref-unsigned-byte-8 (- bootstrap-page 16) i)
                   (sys.int::function-code-byte trampoline i)))
    ;; Copy constants. There are values we need to patch here too.
    ;; These cannot be patched in the original object because then
    ;; we'd lose what they were for future boots with this image.
    (loop with pool-base = (sys.int::function-pool-base trampoline)
          for i from 0 below (sys.int::function-pool-size trampoline)
          for value = (sys.int::function-pool-object trampoline i)
          do (setf (physical-memref-unsigned-byte-64 (+ (- bootstrap-page 16) 8)
                                                     (+ pool-base i))
                   (case value
                     (%%initial-ttbr0-el1%% initial-ttl0)
                     (%%initial-ttbr1-el1%% (%ttbr1-el1))
                     (%%ttbr0-el1%% (%ttbr0-el1))
                     (%%ttbr1-el1%% (%ttbr1-el1))
                     (%%vbar-el1%% (%vbar-el1))
                     (otherwise (sys.int::lisp-object-address value)))))
    ;; Write back data cache to PoU to ensure it's visible to other cores.
    ;; TODO: Is this actually needed?
    (%arm64-sync-icache (convert-to-pmap-address bootstrap-page) #x1000)
    ;; Now we know where the bootstrap page is, we can populate the initial
    ;; translation tables.
    (zeroize-page (convert-to-pmap-address initial-ttl0))
    (zeroize-page (convert-to-pmap-address initial-ttl1))
    (zeroize-page (convert-to-pmap-address initial-ttl2))
    (zeroize-page (convert-to-pmap-address initial-ttl3))
    (setf (physical-memref-unsigned-byte-64 initial-ttl0 (address-l4-bits bootstrap-page))
          (make-pte initial-ttl1-frame))
    (setf (physical-memref-unsigned-byte-64 initial-ttl1 (address-l3-bits bootstrap-page))
          (make-pte initial-ttl2-frame))
    (setf (physical-memref-unsigned-byte-64 initial-ttl2 (address-l2-bits bootstrap-page))
          (make-pte initial-ttl3-frame))
    (setf (physical-memref-unsigned-byte-64 initial-ttl3 (address-l1-bits bootstrap-page))
          (make-pte bootstrap-frame))
    ;; All done.
    (values bootstrap-page initial-ttl0)))
