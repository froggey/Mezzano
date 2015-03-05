;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; High-level interrupt management.

(in-package :mezzano.supervisor)

(sys.int::define-lap-function %stack-probe ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:xor32 :eax :eax)
  LOOP
  (sys.lap-x86:sub64 :r8 #x1000)
  (sys.lap-x86:jb DONE)
  (sys.lap-x86:sub64 :rsp #x1000)
  (sys.lap-x86:mov64 (:rsp) :rax)
  (sys.lap-x86:jmp LOOP)
  DONE
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:ret))

(defmacro without-interrupts (&body body)
  "Execute body with local IRQs inhibited."
  (let ((irq-state (gensym)))
    `(let ((,irq-state (sys.int::%save-irq-state)))
       (when (logtest #x200 ,irq-state)
         (%stack-probe ,(* 16 1024)))
       (sys.int::%cli)
       (unwind-protect
            (progn ,@body)
         (sys.int::%restore-irq-state ,irq-state)))))

(defmacro with-symbol-spinlock ((lock) &body body)
  (check-type lock symbol)
  (let ((current-thread (gensym)))
    `(without-interrupts
       (do ((,current-thread (current-thread)))
           ((sys.int::%cas-symbol-global-value ',lock
                                               :unlocked
                                               ,current-thread))
         (panic "Symbol spinlock " ',lock " held by " ,lock)
         (sys.int::cpu-relax))
       (unwind-protect
            (progn ,@body)
         (setf (sys.int::symbol-global-value ',lock) :unlocked)))))

;;; Low-level interrupt support.

(defvar *user-interrupt-handlers*)

(defun initialize-interrupts ()
  "Called when the system is booted to reset all user interrupt handlers."
  ;; Avoid high-level array/seq functions.
  ;; fixme: allocation should be done once (by the cold-gen?)
  ;; but the reset should be done every boot.
  (when (not (boundp '*user-interrupt-handlers*))
    (setf *user-interrupt-handlers* (sys.int::make-simple-vector 256 :wired)))
  (dotimes (i 256)
    (setf (svref *user-interrupt-handlers* i) nil)))

(defun hook-user-interrupt (interrupt handler)
  (check-type handler (or null function symbol))
  (setf (svref *user-interrupt-handlers* interrupt) handler))

(defun unhandled-interrupt (interrupt-frame info name)
  (declare (ignore interrupt-frame info))
  (panic "Unhandled " name " interrupt."))

;;; Mid-level interrupt handlers, called by the low-level assembly code.

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

;;; Bits in the page-fault error code.
(defconstant +page-fault-error-present+ 0
  "If set, the fault was caused by a page-level protection violation.
If clear, the fault was caused by a non-present page.")
(defconstant +page-fault-error-write+ 1
  "If set, the fault was caused by a write.
If clear, the fault was caused by a read.")
(defconstant +page-fault-error-user+ 2
  "If set, the fault occured in user mode.
If clear, the fault occured in supervisor mode.")
(defconstant +page-fault-error-reserved-violation+ 3
  "If set, the fault was caused by a reserved bit violation in a page directory.")
(defconstant +page-fault-error-instruction+ 4
  "If set, the fault was caused by an instruction fetch.")

(defvar *pagefault-hook* nil)

(defun sys.int::%page-fault-handler (interrupt-frame info)
  (let* ((fault-addr (sys.int::%cr2)))
    (when (and (boundp '*pagefault-hook*)
               *pagefault-hook*)
      (funcall *pagefault-hook* interrupt-frame info fault-addr))
    (cond ((not *paging-disk*)
           (debug-print-line "Fault addr: " fault-addr)
           (unhandled-interrupt interrupt-frame info "early page fault"))
          ((not (logtest #x200 (interrupt-frame-raw-register interrupt-frame :rflags)))
           ;; IRQs must be enabled when a page fault occurs.
           (debug-print-line "Fault addr: " fault-addr)
           (unhandled-interrupt interrupt-frame info "IRQL_NOT_LESS_OR_EQUAL"))
          ((or (<= 0 fault-addr (1- (* 2 1024 1024 1024)))
               (<= (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+)
                   fault-addr
                   (+ (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+)
                      (* 512 1024 1024 1024))))
           ;; Pages below 2G are wired and should never be unmapped or protected.
           ;; Same for pages in the wired stack area.
           (debug-print-line "Fault addr: " fault-addr)
           (unhandled-interrupt interrupt-frame info "page fault in wired area"))
          ((and (logbitp +page-fault-error-present+ info)
                (logbitp +page-fault-error-write+ info))
           ;; Copy on write page.
           (snapshot-clone-cow-page-via-page-fault fault-addr))
          ;; All impossible.
          ((or (logbitp +page-fault-error-present+ info)
               (logbitp +page-fault-error-user+ info)
               (logbitp +page-fault-error-reserved-violation+ info))
           (debug-print-line "Fault addr: " fault-addr)
           (unhandled-interrupt interrupt-frame info "page fault"))
          (t ;; Non-present page. Try to load it from the store.
           ;; Will not return.
           (wait-for-page-via-interrupt interrupt-frame fault-addr)))))

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
      (funcall handler interrupt-frame irq))
    ;; Send EOI.
    (with-symbol-spinlock (*i8259-spinlock*)
      (setf (sys.int::io-port/8 #x20) #x20)
      (when (>= irq 8)
        (setf (sys.int::io-port/8 #xA0) #x20)))))

(defun i8259-mask-irq (irq)
  (check-type irq (integer 0 15))
  (with-symbol-spinlock (*i8259-spinlock*)
    (when (not (logbitp irq *i8259-shadow-mask*))
      ;; Currently unmasked, mask it.
      (setf (ldb (byte 1 irq) *i8259-shadow-mask*) 1)
      (if (< irq 8)
          (setf (sys.int::io-port/8 #x21) (ldb (byte 8 0) *i8259-shadow-mask*))
          (setf (sys.int::io-port/8 #xA1) (ldb (byte 8 8) *i8259-shadow-mask*))))))

(defun i8259-unmask-irq (irq)
  (check-type irq (integer 0 15))
  (with-symbol-spinlock (*i8259-spinlock*)
    (when (logbitp irq *i8259-shadow-mask*)
      ;; Currently masked, unmask it.
      (setf (ldb (byte 1 irq) *i8259-shadow-mask*) 0)
      (if (< irq 8)
          (setf (sys.int::io-port/8 #x21) (ldb (byte 8 0) *i8259-shadow-mask*))
          (setf (sys.int::io-port/8 #xA1) (ldb (byte 8 8) *i8259-shadow-mask*))))))

(defun i8259-hook-irq (irq handler)
  (check-type handler (or null function symbol))
  (setf (svref *i8259-handlers* irq) handler))

(defun initialize-i8259 ()
  ;; TODO: do the APIC & IO-APIC as well.
  (when (not (boundp '*i8259-handlers*))
    (setf *i8259-handlers* (sys.int::make-simple-vector 16 :wired)
          ;; fixme: do at cold-gen time.
          *i8259-spinlock* :unlocked))
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

;;; Introspection.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun interrupt-frame-register-offset (register)
  (ecase register
    (:ss   5)
    (:rsp  4)
    (:rflags 3)
    (:cs   2)
    (:rip  1)
    (:rbp  0)
    (:rax -1)
    (:rcx -2)
    (:rdx -3)
    (:rbx -4)
    (:rsi -5)
    (:rdi -6)
    (:r8  -7)
    (:r9  -8)
    (:r10 -9)
    (:r11 -10)
    (:r12 -11)
    (:r13 -12)
    (:r14 -13)
    (:r15 -14)))
)

(define-compiler-macro interrupt-frame-raw-register (&whole whole frame register)
  (let ((offset (ignore-errors (interrupt-frame-register-offset register))))
    (if offset
        `(sys.int::memref-signed-byte-64 (interrupt-frame-pointer ,frame)
                                         ,offset)
        whole)))

(define-compiler-macro (setf interrupt-frame-raw-register) (&whole whole value frame register)
  (let ((offset (ignore-errors (interrupt-frame-register-offset register))))
    (if offset
        `(setf (sys.int::memref-signed-byte-64 (interrupt-frame-pointer ,frame)
                                               ,offset)
               ,value)
        whole)))

(define-compiler-macro interrupt-frame-value-register (&whole whole frame register)
  (let ((offset (ignore-errors (interrupt-frame-register-offset register))))
    (if offset
        `(sys.int::memref-t (interrupt-frame-pointer ,frame) ,offset)
        whole)))

(define-compiler-macro (setf interrupt-frame-value-register) (&whole whole value frame register)
  (let ((offset (ignore-errors (interrupt-frame-register-offset register))))
    (if offset
        `(setf (sys.int::memref-t (interrupt-frame-pointer ,frame) ,offset)
               ,value)
        whole)))

(defun interrupt-frame-pointer (frame)
  (sys.int::%array-like-ref-t frame 0))

(defun interrupt-frame-raw-register (frame register)
  (sys.int::memref-unsigned-byte-64 (interrupt-frame-pointer frame)
                                    (interrupt-frame-register-offset register)))

(defun (setf interrupt-frame-raw-register) (value frame register)
  (setf (sys.int::memref-unsigned-byte-64 (interrupt-frame-pointer frame)
                                          (interrupt-frame-register-offset register))
        value))

(defun interrupt-frame-value-register (frame register)
  (sys.int::memref-t (interrupt-frame-pointer frame)
                     (interrupt-frame-register-offset register)))

(defun (setf interrupt-frame-value-register) (value frame register)
  (setf (sys.int::memref-t (interrupt-frame-pointer frame)
                           (interrupt-frame-register-offset register))
        value))
