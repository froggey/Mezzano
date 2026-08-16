(in-package :mezzano.supervisor)

(sys.int::define-lap-function ensure-on-wired-stack ()
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:stp :x29 :x30 (:pre :sp -16))
  (:gc :no-frame :layout #*00)
  (mezzano.lap.arm64:add :x29 :sp :xzr)
  (:gc :frame)
  (mezzano.lap.arm64:add :x9 :sp 0)
  (mezzano.lap.arm64:orr :x5 :xzr #x200000000000)
  (mezzano.lap.arm64:sub :x9 :x9 :x5)
  (mezzano.lap.arm64:orr :x5 :xzr #x8000000000)
  (mezzano.lap.arm64:subs :xzr :x9 :x5)
  (mezzano.lap.arm64:b.hs BAD)
  (mezzano.lap.arm64:orr :x5 :xzr :xzr)
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:ret)
  BAD
  (mezzano.lap.arm64:ldr :x0 (:constant "Not on wired stack."))
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:named-call panic)
  (mezzano.lap.arm64:brk 42))

(sys.int::define-lap-function sys.int::%interrupt-state (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:mrs :x9 :daif)
  (mezzano.lap.arm64:ldr :x0 (:constant t))
  (mezzano.lap.arm64:ands :xzr :x9 :x9)
  (mezzano.lap.arm64:csel.ne :x0 :x26 :x0)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %disable-interrupts (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:msr :daifset #b1111)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %enable-interrupts (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:msr :daifclr #b1111)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %wait-for-interrupt (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:wfi)
  (mezzano.lap.arm64:msr :daifclr #b1111)
  (mezzano.lap.arm64:ret))

;; TODO: Use SEV/WFE hints
(sys.int::define-lap-function sys.int::cpu-relax (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:yield)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %arch-panic-stop (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:wfi)
  (mezzano.lap.arm64:ret))

(defun sys.int::%save-irq-state ()
  (sys.int::%interrupt-state))

(defun sys.int::%restore-irq-state (state)
  (when state
    (%enable-interrupts)))

(sys.int::define-lap-function %call-on-wired-stack-without-interrupts ((function unused &optional arg1 arg2 arg3))
  (:gc :no-frame :layout #*)
  ;; Argument setup for the frame pointer.
  (mezzano.lap.arm64:orr :x1 :xzr :x29) ; fp
  ;; Build a frame and save the old stack pointer.
  (mezzano.lap.arm64:stp :x29 :x30 (:pre :sp -16))
  (:gc :no-frame :layout #*00)
  (mezzano.lap.arm64:add :x29 :sp :xzr)
  (:gc :frame)
  ;; Save the callee-save registers too.
  (mezzano.lap.arm64:stp :x13 :x14 (:pre :sp -16))
  (:gc :frame :layout #*11)
  ;; Argument setup.
  (mezzano.lap.arm64:orr :x6 :xzr :x0) ; function
  (mezzano.lap.arm64:add :x0 :sp 0) ; sp
  ;; Test if interrupts are enabled.
  (mezzano.lap.arm64:mrs :x9 :daif)
  (mezzano.lap.arm64:cbnz :x9 INTERRUPTS-DISABLED)
  ;; Disable interrupts after setting up the frame, not before.
  ;; Modifying the normal stack may cause page-faults which can't
  ;; occur with interrupts disabled.
  (mezzano.lap.arm64:msr :daifset #b1111)
  ;; Switch over to the wired stack.
  (mezzano.lap.arm64:msr :spsel 1)
  ;; Call function, arguments were setup above.
  (mezzano.lap.arm64:ldr :x9 (:object :x6 0))
  (mezzano.lap.arm64:blr :x9)
  (:gc :frame :layout #*11 :multiple-values 0)
  ;; Switch back to the old stack.
  ;; Do not restore frame & stack pointer here, that would touch the old stack with
  ;; interrupts disabled.
  (mezzano.lap.arm64:msr :spsel 0)
  ;; Reenable interrupts, must not be done when on the wired stack.
  (mezzano.lap.arm64:msr :daifclr #b1111)
  ;; Pop callee-save registers.
  (mezzano.lap.arm64:ldp :x13 :x14 (:post :sp 16))
  (:gc :frame :layout #* :multiple-values 0)
  ;; Now safe to restore the frame pointer.
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame :layout #* :multiple-values 0)
  ;; Done, return.
  (mezzano.lap.arm64:ret)
  INTERRUPTS-DISABLED
  (:gc :frame :layout #*11)
  ;; Call function, arguments were setup above.
  (mezzano.lap.arm64:ldr :x9 (:object :x6 0))
  (mezzano.lap.arm64:blr :x9)
  ;; Restore frame and return.
  (mezzano.lap.arm64:ldp :x13 :x14 (:post :sp 16))
  (:gc :frame :layout #*)
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame :layout #* :multiple-values 0)
  (mezzano.lap.arm64:ret))

(defun unhandled-interrupt (interrupt-frame name)
  (panic "Unhandled " name " interrupt."
         " SPSR: " (interrupt-frame-raw-register interrupt-frame :rflags)
         " PC: " (interrupt-frame-raw-register interrupt-frame :rip)
         " x30: " (interrupt-frame-raw-register interrupt-frame :cs)
         " SP: " (interrupt-frame-raw-register interrupt-frame :rsp)
         " ESR: " (%esr-el1)
         " FAR: " (%far-el1)))

(defun local-cpu-page-fault-hook ()
  (arm64-cpu-page-fault-hook (local-cpu)))

(defun (setf local-cpu-page-fault-hook) (value)
  (setf (arm64-cpu-page-fault-hook (local-cpu-info)) value))

(defun %page-fault-handler (interrupt-frame fault-addr reason)
  (let ((hook (local-cpu-page-fault-hook)))
    (when hook
      ;; FIXME: This doesn't work when the hook was bound in SP_EL0,
      ;; it doesn't switch back to EL0, which will leave SP_EL1 pointing at the EL0
      ;; stack. Even if it did switch back to SP_EL0 it would also need to restore
      ;; the original SP_EL1.
      (funcall hook interrupt-frame reason fault-addr nil)))
  (cond ((not *paging-disk*)
         (unhandled-interrupt interrupt-frame "early-page-fault"))
        ((logtest #x3C0 (interrupt-frame-raw-register interrupt-frame :rflags))
         ;; IRQs must be enabled when a page fault occurs.
         (unhandled-interrupt interrupt-frame "page-fault-no-irqs"))
        ((and (eql (thread-priority (current-thread)) :supervisor)
              (address-in-non-faulting-range-p fault-addr))
         (unhandled-interrupt interrupt-frame "wired-page-fault"))
        (t ;; Defer to the pager.
         ;; Might not return.
         (wait-for-page-via-interrupt interrupt-frame
                                      fault-addr
                                      (eql reason :write-to-ro)
                                      nil))))

(defun %instruction-abort-handler (interrupt-frame fault-addr esr)
  (let ((status (ldb (byte 5 0) esr)))
    (case status
      ((#x04 #x05 #x06 #x07) ;; Translation fault (page not mapped).
       (%page-fault-handler interrupt-frame fault-addr :not-present))
      (t
       (unhandled-interrupt interrupt-frame "instruction-abort")))))

(defun %data-abort-handler (interrupt-frame fault-addr esr)
  (let ((status (ldb (byte 5 0) esr)))
    (case status
      ((#x04 #x05 #x06 #x07) ;; Translation fault (page not mapped).
       (%page-fault-handler interrupt-frame fault-addr :not-present))
      ((#x0C #x0D #x0E #x0F) ;; Permission fault.
       (let* ((pte (get-pte-for-address fault-addr nil))
              (current (and pte (page-table-entry pte))))
         (cond ((and (logtest esr #x40)
                     pte
                     (logtest current +arm64-tte-writable+)
                     (eql (ldb +arm64-tte-ap+ current)
                          +arm64-tte-ap-pro-una+))
                ;; Dirty bit emulation.
                ;; Set the dirty bit and make the page writable again.
                #+(or)
                (debug-print-line "Dirty emulation for address " fault-addr)
                (let ((new (dpb +arm64-tte-ap-prw-una+
                                +arm64-tte-ap+
                                (logior current +arm64-tte-dirty+))))
                  ;; We don't bother trying to retry the result of this.
                  ;; No matter if it passes or fails we return from the
                  ;; fault and retry the access. We'll either succeed,
                  ;; end up back here, or trigger another fault.
                  (ext:cas (page-table-entry pte) current new))
                (flush-tlb-single fault-addr))
               ((logtest esr #x40)
                (%page-fault-handler interrupt-frame fault-addr :write-to-ro))
               (t
                (unhandled-interrupt interrupt-frame "data-abort")))))
      (t
       (unhandled-interrupt interrupt-frame "data-abort")))))

(defun %synchronous-el0-handler (interrupt-frame)
  (let* ((esr (%esr-el1))
         (class (ldb (byte 6 26) esr)))
    (case class
      (#x21
       (%instruction-abort-handler interrupt-frame (%far-el1) esr))
      (#x25
       (%data-abort-handler interrupt-frame (%far-el1) esr))
      (#x3C ; BRK instruction
       (let ((comment (ldb (byte 16 0) esr)))
         (case comment
           (28 ; %%partial-save-return-thunk
            (partial-save-return-helper interrupt-frame))
           (42 ; %%unreachable
            (pager-invoke-via-interrupt
             #'mezzano.runtime::%raise-unreachable interrupt-frame nil))
           (t
            (unhandled-interrupt interrupt-frame "brk")))))
      (#x33 ; Software Step exception taken without a change in Exception level
       (stop-thread-for-single-step interrupt-frame))
      (t
       (unhandled-interrupt interrupt-frame "synchronous-el0")))))

(defun %irq-el0-handler (interrupt-frame)
  (gic-handle-interrupt interrupt-frame))

(defun %fiq-el0-handler (interrupt-frame)
  (unhandled-interrupt interrupt-frame "fiq-el0"))

(defun %serror-el0-handler (interrupt-frame)
  (unhandled-interrupt interrupt-frame "serror-el0"))

(defun %synchronous-elx-handler (interrupt-frame)
  (let* ((esr (%esr-el1))
         (class (ldb (byte 6 26) esr)))
    (case class
      (#x21
       (%instruction-abort-handler interrupt-frame (%far-el1) esr))
      (#x25
       (%data-abort-handler interrupt-frame (%far-el1) esr))
      (t
       (unhandled-interrupt interrupt-frame "synchronous-elx")))))

(defun %irq-elx-handler (interrupt-frame)
  (unhandled-interrupt interrupt-frame "irq-elx"))

(defun %fiq-elx-handler (interrupt-frame)
  (unhandled-interrupt interrupt-frame "fiq-elx"))

(defun %serror-elx-handler (interrupt-frame)
  (unhandled-interrupt interrupt-frame "serror-elx"))
