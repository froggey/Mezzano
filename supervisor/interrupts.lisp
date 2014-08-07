(in-package :mezzanine.supervisor)

(defmacro without-interrupts (&body body)
  "Execute body with local IRQs inhibited."
  ;; FIXME: should use unwind-protect, but the compiler needs to
  ;; be fixed so it generates a dx environment & closure.
  (let ((irq-state (gensym)))
    `(let ((,irq-state (sys.int::%save-irq-state)))
       (sys.int::%cli)
       (multiple-value-prog1
           (progn ,@body)
         (sys.int::%restore-irq-state ,irq-state)))))

(defmacro with-spinlock ((lock-symbol) &body body)
  (check-type lock-symbol symbol)
  (let ((current-thread (gensym)))
    `(without-interrupts
       (let ((,current-thread (sys.int::current-stack-group)))
         (do ()
             ((sys.int::%cas-symbol-global-value ',lock-symbol
                                                 :unlocked
                                                 ,current-thread))
           (sys.int::cpu-relax))
         (multiple-value-prog1 (progn ,@body)
           (setf (sys.int::symbol-global-value ',lock-symbol) :unlocked))))))

;;; Low-level interrupt support.

(defvar *user-interrupt-handlers*)

(defun initialize-interrupts ()
  "Called when the system is booted to reset all user interrupt handlers."
  ;; Avoid high-level array/seq functions.
  ;; fixme: should be wired.
  (setf *user-interrupt-handlers* #.(make-array 256)) ; ugly hack avoiding early gc
  (dotimes (i 256)
    (setf (svref *user-interrupt-handlers* i) nil)))

(defun hook-user-interrupt (interrupt handler)
  (check-type handler (or null function symbol))
  (setf (svref *user-interrupt-handlers* interrupt) handler))

(defun unhandled-interrupt (interrupt-frame info name)
  (declare (ignore interrupt-frame info))
  (error "Unhandled ~A interrupt." name))

(defun sys.int::%divide-error-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "divide error"))

(defun sys.int::%debug-exception-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "debug exception"))

(defun sys.int::%nonmaskable-interrupt-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "nonmaskable"))

(defun sys.int::%breakpoint-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "breakpoint"))

(defun sys.int::%overflow-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "overflow"))

(defun sys.int::%bound-exception-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "bound exception"))

(defun sys.int::%invalid-opcode-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "invalid opcode"))

(defun sys.int::%device-not-available-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "device not available"))

(defun sys.int::%double-fault-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "double fault"))

(defun sys.int::%invalid-tss-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "invalid tss"))

(defun sys.int::%segment-not-present-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "segment not present"))

(defun sys.int::%stack-segment-fault-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "stack segment fault"))

(defun sys.int::%general-protection-fault-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "general protection fault"))

(defun sys.int::%page-fault-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "page fault"))

(defun sys.int::%math-fault-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "math fault"))

(defun sys.int::%alignment-check-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "alignment check"))

(defun sys.int::%machine-check-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "machine check"))

(defun sys.int::%simd-exception-handler (interrupt-frame info)
  (unhandled-interrupt interrupt-frame info "simd exception"))

(defun sys.int::%user-interrupt-handler (interrupt-frame info)
  (let ((handler (svref *user-interrupt-handlers* info)))
    (if handler
        (funcall handler interrupt-frame info)
        (unhandled-interrupt interrupt-frame info "user"))))

;;; i8259 PIC support.

(defconstant +i8259-base-interrupt+ 32)

;; These are all initialized during early boot,
;; The defvars will be run during cold load, but never see the symbols as unbound.
(defvar *i8259-shadow-mask* nil
  "Caches the current IRQ mask, so it doesn't need to be read from the PIC when being modified.")
(defvar *i8259-spinlock* nil ; should be defglobal or something. defspinlock.
  "Lock serializing access to i8259 and associated variables.")
(defvar *i8259-handlers* nil)

(defun i8259-interrupt-handler (interrupt-frame info)
  (declare (ignore interrupt-frame))
  (let* ((irq (- info +i8259-base-interrupt+))
         (handler (svref *i8259-handlers* irq)))
    (when handler
      (funcall handler irq))
    ;; Send EOI.
    (with-spinlock (*i8259-spinlock*)
      (setf (sys.int::io-port/8 #x20) #x20)
      (when (>= irq 8)
        (setf (sys.int::io-port/8 #xA0) #x20)))))

(defun i8259-mask-irq (irq)
  (check-type irq (integer 0 15))
  (with-spinlock (*i8259-spinlock*)
    (when (not (logbitp irq *i8259-shadow-mask*))
      ;; Currently unmasked, mask it.
      (setf (ldb (byte 1 irq) *i8259-shadow-mask*) 1)
      (if (< irq 8)
          (setf (sys.int::io-port/8 #x21) (ldb (byte 8 0) *i8259-shadow-mask*))
          (setf (sys.int::io-port/8 #xA1) (ldb (byte 8 8) *i8259-shadow-mask*))))))

(defun i8259-unmask-irq (irq)
  (check-type irq (integer 0 15))
  (with-spinlock (*i8259-spinlock*)
    (when (logbitp irq *i8259-shadow-mask*)
      ;; Currently masked, unmask it.
      (setf (ldb (byte 1 irq) *i8259-shadow-mask*) 0)
      (if (< irq 8)
          (setf (sys.int::io-port/8 #x21) (ldb (byte 8 0) *i8259-shadow-mask*))
          (setf (sys.int::io-port/8 #xA1) (ldb (byte 8 8) *i8259-shadow-mask*))))))

(defun initialize-i8259 ()
  ;; TODO: do the APIC & IO-APIC as well.
  ;; FIXME: should be wired.
  (setf *i8259-handlers* #.(make-array 16) ; ugly hack avoiding early gc
        *i8259-spinlock* :unlocked)
  (dotimes (i 16)
    (setf (svref *i8259-handlers* i) nil))
  ;; Hook interrupts.
  (dotimes (i 16)
    (hook-user-interrupt (+ +i8259-base-interrupt+ i)
                         'i8259-interrupt-handler))
  ;; Initialize both i8259 chips.
  (setf (sys.int::io-port/8 #x20) #x11
        (sys.int::io-port/8 #xA0) #x11
        (sys.int::io-port/8 #x21) +i8259-base-interrupt+
        (sys.int::io-port/8 #xA1) (+ +i8259-base-interrupt+ 8)
        (sys.int::io-port/8 #x21) #x04
        (sys.int::io-port/8 #xA1) #x02
        (sys.int::io-port/8 #x21) #x01
        (sys.int::io-port/8 #xA1) #x01
        ;; Mask all IRQs.
        (sys.int::io-port/8 #x21) #xFF
        (sys.int::io-port/8 #xA1) #xFF)
  (setf *i8259-shadow-mask* #xFFFF)
  ;; Unmask the cascade IRQ, required for the 2nd chip to function.
  (i8259-unmask-irq 2))
