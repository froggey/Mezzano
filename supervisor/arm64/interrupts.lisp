;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(sys.int::define-lap-function sys.int::%interrupt-state (())
  (mezzano.lap.arm64:mrs :x9 :daif)
  (mezzano.lap.arm64:ldr :x0 (:constant t))
  (mezzano.lap.arm64:ands :xzr :x9 :x9)
  (mezzano.lap.arm64:csel.ne :x0 :x26 :x0)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %disable-interrupts (())
  (mezzano.lap.arm64:msr :daifset #b1111)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %enable-interrupts (())
  (mezzano.lap.arm64:msr :daifclr #b1111)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %call-on-wired-stack-without-interrupts ((function unused &optional arg1 arg2 arg3))
  ;; Argument setup.
  (mezzano.lap.arm64:orr :x6 :xzr :x0) ; function
  (mezzano.lap.arm64:add :x0 :sp 0) ; sp
  (mezzano.lap.arm64:orr :x1 :xzr :x29) ; fp
  ;; Test if interrupts are enabled.
  (mezzano.lap.arm64:mrs :x9 :daif)
  (mezzano.lap.arm64:cbz :x9 INTERRUPTS-ENABLED)
  ;; Interrupts are already disabled, tail-call to the function.
  (mezzano.lap.arm64:ldr :x9 (:object :x6 0))
  (mezzano.lap.arm64:br :x9)
  INTERRUPTS-ENABLED
  ;; Build a frame and save the old stack pointer.
  (mezzano.lap.arm64:stp :x29 :x30 (:pre :sp -16))
  (:gc :no-frame :layout #*0)
  (mezzano.lap.arm64:add :x29 :sp :xzr)
  (:gc :frame)
  ;; Disable interrupts after setting up the frame, not before.
  ;; Modifying the normal stack may cause page-faults which can't
  ;; occur with interrupts disabled.
  (mezzano.lap.arm64:msr :daifset #b1111)
  ;; Switch over to the wired stack.
  (mezzano.lap.arm64:msr :spsel 1)
  ;; Call function, arguments were setup above.
  (mezzano.lap.arm64:ldr :x9 (:object :x6 0))
  (mezzano.lap.arm64:blr :x9)
  (:gc :frame :multiple-values 0)
  ;; Switch back to the old stack.
  ;; Do not restore frame & stack pointer here, that would touch the old stack with
  ;; interrupts disabled.
  (mezzano.lap.arm64:msr :spsel 0)
  ;; Reenable interrupts, must not be done when on the wired stack.
  (mezzano.lap.arm64:msr :daifclr #b1111)
  ;; Now safe to restore the frame pointer.
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame :multiple-values 0)
  ;; Done, return.
  (mezzano.lap.arm64:ret))

(defun unhandled-interrupt (interrupt-frame info name)
  (declare (ignore interrupt-frame info))
  (panic "Unhandled " name " interrupt."))

(defun %synchronous-el0-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "synchronous-el0"))

(defun %irq-el0-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "irq-el0"))

(defun %fiq-el0-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "fiq-el0"))

(defun %serror-el0-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "serror-el0"))

(defun %synchronous-elx-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "synchronous-elx"))

(defun %irq-elx-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "irq-elx"))

(defun %fiq-elx-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "fiq-elx"))

(defun %serror-elx-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "serror-elx"))

(defun platform-mask-irq (vector)
  (gic-mask-interrupt vector))

(defun platform-unmask-irq (vector)
  (gic-unmask-interrupt vector))

(defun platform-attach-irq (vector handler)
  (gic-hook-interrupt vector handler))
