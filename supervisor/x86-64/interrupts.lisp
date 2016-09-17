;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; High-level interrupt management.

(in-package :mezzano.supervisor)

(sys.int::define-lap-function ensure-on-wired-stack ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:mov64 :rax :rsp)
  (sys.lap-x86:mov64 :rcx #x200000000000)
  (sys.lap-x86:sub64 :rax :rcx)
  (sys.lap-x86:mov64 :rcx #x8000000000)
  (sys.lap-x86:cmp64 :rax :rcx)
  (sys.lap-x86:jae BAD)
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:ret)
  BAD
  (sys.lap-x86:mov64 :r8 (:constant "Not on wired stack."))
  (sys.lap-x86:mov64 :r13 (:function panic))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2))

(declaim (inline %disable-interrupts))
(defun %disable-interrupts ()
  (sys.int::%cli))

(declaim (inline %enable-interrupts))
(defun %enable-interrupts ()
  (sys.int::%sti))

;; Call FUNCTION on the wired stack with interrupts disabled.
;; FUNCTION must be a function, not a function designator.
;; UNUSED should be NIL.
;; FUNCTION will be called with the old stack pointer & frame pointer and
;; any additional arguments.
;; If %C-O-W-S-W-I is called with interrupts enabled, then it will switch over
;; to the CPU's wired stack for the duration of the call.
;; %C-O-W-S-W-I must not be exited using a non-local exit.
;; %RESCHEDULE and similar functions must not be called.
(sys.int::define-lap-function %call-on-wired-stack-without-interrupts ()
  ;; Argument setup.
  (sys.lap-x86:mov64 :rbx :r8) ; function
  (sys.lap-x86:mov64 :r8 :rsp) ; sp
  (sys.lap-x86:mov64 :r9 :rbp) ; fp
  ;; Test if interrupts are enabled.
  (sys.lap-x86:pushf)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:test64 (:rsp) #x200)
  (sys.lap-x86:jnz INTERRUPTS-ENABLED)
  ;; Interrupts are already disabled, tail-call to the function.
  (sys.lap-x86:add64 :rsp 8) ; drop pushed flags.
  (sys.lap-x86:jmp (:object :rbx 0))
  INTERRUPTS-ENABLED
  ;; Save the old stack pointer.
  (sys.lap-x86:mov64 (:rsp) :rbp) ; overwrite the saved interrupt state.
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  ;; Disable interrupts after setting up the frame, not before.
  ;; Modifying the normal stack may cause page-faults which can't
  ;; occur with interrupts disabled.
  (sys.lap-x86:cli)
  ;; Switch over to the wired stack.
  (sys.lap-x86:fs)
  (sys.lap-x86:mov64 :rsp (#.+cpu-info-wired-stack-offset+))
  ;; Call function, argument were setup above.
  (sys.lap-x86:call (:object :rbx 0))
  (:gc :frame :multiple-values 0)
  ;; Switch back to the old stack.
  ;; Do not restore :RBP here, that would touch the old stack with
  ;; interrupts disabled.
  (sys.lap-x86:mov64 :rsp :rbp)
  ;; Reenable interrupts, must not be done when on the wired stack.
  (sys.lap-x86:sti)
  ;; Now safe to restore :RBP.
  (sys.lap-x86:pop :rbp)
  (:gc :no-frame :multiple-values 0)
  ;; Done, return.
  (sys.lap-x86:ret))

;;; Low-level interrupt support.

(sys.int::defglobal *user-interrupt-handlers*)

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

(sys.int::defglobal *page-fault-hook* nil)

(defun fatal-page-fault (interrupt-frame info reason address)
  (declare (ignore interrupt-frame info))
  (panic reason " on address " address))

(defmacro with-page-fault-hook (((&optional frame info fault-address) &body hook-body) &body body)
  (let ((old (gensym))
        (frame (or frame (gensym "FRAME")))
        (info (or info (gensym "INFO")))
        (fault-address (or fault-address (gensym "FAULT-ADDRESS"))))
    `(flet ((page-fault-hook-fn (,frame ,info ,fault-address)
              (declare (ignorable ,frame ,info ,fault-address))
              ,hook-body))
       (declare (dynamic-extent #'page-fault-hook-fn))
       (ensure-interrupts-disabled)
       (let ((,old (sys.int::symbol-global-value '*page-fault-hook*)))
         (unwind-protect
              (progn
                (setf (sys.int::symbol-global-value '*page-fault-hook*) #'page-fault-hook-fn)
                ,@body)
           (setf (sys.int::symbol-global-value '*page-fault-hook*) ,old))))))

(defun sys.int::%page-fault-handler (interrupt-frame info)
  (let* ((fault-addr (sys.int::%cr2)))
    (when (and (sys.int::symbol-global-boundp '*page-fault-hook*)
               (sys.int::symbol-global-value '*page-fault-hook*))
      (funcall (sys.int::symbol-global-value '*page-fault-hook*)
               interrupt-frame info fault-addr))
    (cond ((not (sys.int::symbol-global-value '*paging-disk*))
           (fatal-page-fault interrupt-frame info "Early page fault" fault-addr))
          ((not (logtest #x200 (interrupt-frame-raw-register interrupt-frame :rflags)))
           ;; IRQs must be enabled when a page fault occurs.
           (fatal-page-fault interrupt-frame info "Page fault with interrupts disabled" fault-addr))
          ((or (<= 0 fault-addr (1- (* 2 1024 1024 1024)))
               (<= (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+)
                   fault-addr
                   (+ (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+)
                      (* 512 1024 1024 1024))))
           ;; Pages below 2G are wired and should never be unmapped or protected.
           ;; Same for pages in the wired stack area.
           (fatal-page-fault interrupt-frame info "Page fault in wired area" fault-addr))
          ((and (logbitp +page-fault-error-present+ info)
                (logbitp +page-fault-error-write+ info))
           ;; Copy on write page, might not return.
           (snapshot-clone-cow-page-via-page-fault interrupt-frame fault-addr))
          ;; All impossible.
          ((or (logbitp +page-fault-error-present+ info)
               (logbitp +page-fault-error-user+ info)
               (logbitp +page-fault-error-reserved-violation+ info))
           (fatal-page-fault interrupt-frame info "Page fault" fault-addr))
          (t ;; Non-present page. Try to load it from the store.
           ;; Might not return.
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
  (let ((handler (svref (sys.int::symbol-global-value '*user-interrupt-handlers*) info)))
    (if handler
        (funcall handler interrupt-frame info)
        (unhandled-interrupt interrupt-frame info "user"))))

;;; i8259 PIC support.

(defconstant +i8259-base-interrupt+ 32)

;; These are all initialized during early boot,
;; The defvars will be run during cold load, but never see the symbols as unbound.
(sys.int::defglobal *i8259-shadow-mask* nil
  "Caches the current IRQ mask, so it doesn't need to be read from the PIC when being modified.")
(sys.int::defglobal *i8259-spinlock* nil
  "Lock serializing access to i8259 and associated variables.")
(sys.int::defglobal *i8259-handlers* nil)

(defun i8259-interrupt-handler (interrupt-frame info)
  (let ((irq (- info +i8259-base-interrupt+)))
    (dolist (handler (svref *i8259-handlers* irq))
      (funcall handler interrupt-frame irq))
    ;; Send EOI.
    (with-symbol-spinlock (*i8259-spinlock*)
      (setf (sys.int::io-port/8 #x20) #x20)
      (when (>= irq 8)
        (setf (sys.int::io-port/8 #xA0) #x20)))
    (maybe-preempt-via-interrupt interrupt-frame)))

(defun i8259-mask-irq (irq)
  (check-type irq (integer 0 15))
  (without-interrupts
    (with-symbol-spinlock (*i8259-spinlock*)
      (when (not (logbitp irq *i8259-shadow-mask*))
        ;; Currently unmasked, mask it.
        (setf (ldb (byte 1 irq) *i8259-shadow-mask*) 1)
        (if (< irq 8)
            (setf (sys.int::io-port/8 #x21) (ldb (byte 8 0) *i8259-shadow-mask*))
            (setf (sys.int::io-port/8 #xA1) (ldb (byte 8 8) *i8259-shadow-mask*)))))))

(defun i8259-unmask-irq (irq)
  (check-type irq (integer 0 15))
  (without-interrupts
    (with-symbol-spinlock (*i8259-spinlock*)
      (when (logbitp irq *i8259-shadow-mask*)
        ;; Currently masked, unmask it.
        (setf (ldb (byte 1 irq) *i8259-shadow-mask*) 0)
        (if (< irq 8)
            (setf (sys.int::io-port/8 #x21) (ldb (byte 8 0) *i8259-shadow-mask*))
            (setf (sys.int::io-port/8 #xA1) (ldb (byte 8 8) *i8259-shadow-mask*)))))))

(defun i8259-hook-irq (irq handler)
  (check-type handler (or null function symbol))
  (push-wired handler (svref *i8259-handlers* irq)))

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
  (sys.int::%object-ref-t frame 0))

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
