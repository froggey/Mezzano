;;;; DISASSEMBLE for arm64

(defpackage :mezzano.disassemble.arm64
  (:use :cl)
  (:local-nicknames (:int :mezzano.internals)
                    (:a64 :mezzano.lap.arm64)
                    (:dis :mezzano.disassemble)))

(in-package :mezzano.disassemble.arm64)

(defclass arm64-disassembler-context (dis:disassembler-context)
  ())

(defmethod dis:make-disassembler-context-using-architecture ((architecture mezzano.compiler::arm64-target) &rest initargs)
  (apply #'make-instance 'arm64-disassembler-context initargs))

(defclass arm64-instruction (dis:instruction)
  ((%opcode :initarg :opcode :reader inst-opcode)
   (%operands :initarg :operands :reader inst-operands)))

(defmethod dis:inst-size ((inst arm64-instruction)) 4)

(defun load/store-exclusive (context word)
  (values nil :load/store-exclusive))

(defconstant +v-bit+ 26)
(defconstant +l-bit+ 22)

(defconstant +rt+ (byte 5 0))
(defconstant +rn+ (byte 5 5))
(defconstant +rt2+ (byte 5 10))

(defun decode-gp32 (register-number &key sp)
  (if (and sp (eql register-number 31))
      :wsp
      (aref #(:w0  :w1  :w2  :w3  :w4  :w5  :w6  :w7
              :w8  :w9  :w10 :w11 :w12 :w13 :w14 :w15
              :w16 :w17 :w18 :w19 :w20 :w21 :w22 :w23
              :w24 :w25 :w26 :w27 :w28 :w29 :w30 :wzr)
            register-number)))

(defun decode-gp64 (register-number &key sp)
  (if (and sp (eql register-number 31))
      :sp
      (aref #(:x0  :x1  :x2  :x3  :x4  :x5  :x6  :x7
              :x8  :x9  :x10 :x11 :x12 :x13 :x14 :x15
              :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
              :x24 :x25 :x26 :x27 :x28 :x29 :x30 :xzr)
            register-number)))

;; Load/store no-allocate pair (offset)
;; Load/store register pair (post-indexed)
;; Load/store register pair (offset)
;; Load/store register pair (pre-indexed)
(defun load/store-pair (context word)
  (declare (ignore context))
  (let* ((immediate (int::sign-extend (ldb (byte 7 15) word) 7))
         (address-reg (decode-gp64 (ldb +rn+ word) :sp t))
         (writeback (logbitp 23 word))
         (non-temporal (and (not writeback) (not (logbitp 24 word))))
         (address-mode (and writeback (if (logbitp 24 word) :pre :post)))
         (simd&fp (logbitp +v-bit+ word))
         (size (if simd&fp
                   (ecase (ldb (byte 2 30) word)
                     (0 4)
                     (1 8)
                     (2 16)
                     (3 (return-from load/store-pair (values :load/store-pair-simd&fp-size))))
                   (ecase (ldb (byte 2 30) word)
                     (0 4)
                     (1
                      (when non-temporal
                        (return-from load/store-pair (values :load/store-pair-size)))
                      ;; For LDPWS
                      8)
                     (2 8)
                     (3 (return-from load/store-pair (values :load/store-pair-size))))))
         (base-address (if (zerop immediate)
                           (list address-reg)
                           (list address-reg (* immediate size))))
         (address (if address-mode
                      (list* address-mode base-address)
                      base-address))
         (opcode (if non-temporal
                     (if (logbitp +l-bit+ word)
                         :ldnp ; lap todo
                         :stnp) ; lap todo
                     (if (logbitp +l-bit+ word)
                         (cond ((eql (ldb (byte 2 30) word) 1)
                                :ldpws) ; lap todo
                               (t
                                'a64:ldp))
                         'a64:stp)))
         (reg-decoder (if simd&fp
                          (ecase (ldb (byte 2 30) word)
                            (0 #'decode-fp32)
                            (1 #'decode-fp64)
                            (2 #'decode-fp128))
                          (ecase (ldb (byte 2 30) word)
                            (0 #'decode-gp32)
                            (1 #'decode-gp64)
                            (2 #'decode-gp64)))))
    (make-instance 'arm64-instruction
                   :opcode opcode
                   :operands (list (funcall reg-decoder (ldb +rt+ word))
                                   (funcall reg-decoder (ldb +rt2+ word))
                                   address))))

;; Load register (literal)
(defun load-register-literal (context word)
  (declare (ignore context))
  (let* ((immediate (int::sign-extend (ldb (byte 19 5) word) 19))
         (address (list :pc (ash immediate 2))))
    ;; FIXME: 32-bit and SIMD&FP versions
    (make-instance 'arm64-instruction
                   :opcode 'a64:ldr
                   :operands (list (decode-gp64 (ldb +rt+ word))
                                   address))))

(defparameter *load/store-opcodes*
  #(a64:strb a64:ldrb a64:ldrsb a64:ldrsb
    a64:strh a64:ldrh a64:ldrsh a64:ldrsh
    a64:str  a64:ldr  a64:ldrsw nil
    a64:str  a64:ldr  :prfm     nil))

;; Load/store register (unscaled immediate)
;; Load/store register (immediate post-indexed)
;; Load/store register (unprivileged)
;; Load/store register (immediate pre-indexed)
(defun load/store-register (context word)
  (declare (ignore context))
  (let* ((immediate (int::sign-extend (ldb (byte 9 12) word) 9))
         (address-reg (decode-gp64 (ldb +rn+ word) :sp t))
         (writeback (logbitp 10 word))
         (unpriviliged (and (not writeback) (logbitp 11 word)))
         (address-mode (and writeback (if (logbitp 11 word) :pre :post)))
         (simd&fp (logbitp +v-bit+ word))
         (size (ldb (byte 2 30) word))
         (opc (ldb (byte 2 22) word))
         (scale (if simd&fp
                    (logior (if (logbitp 1 opc) #b100 0)
                            size)
                    0))
         (base-address (if (zerop immediate)
                           (list address-reg)
                           (list address-reg (ash immediate scale))))
         (address (if address-mode
                      (list* address-mode base-address)
                      base-address))
         (opcode (cond (simd&fp
                        (return-from load/store-register
                          (values nil :load/store-register-simd&fp)))
                       (t
                        (let ((opcode-byte (logior (ash size 2) opc)))
                          (cond (unpriviliged
                                 (aref #(:sttrb :ldtrb :ldtrsb :ldtrsb :sttrh :ldtrh :ldtrsh :ldtrsh :sttr :ldtr :ldtrsw nil :sttr :ldtr nil nil) opcode-byte))
                                ((and address-mode (eql size #b11) (eql opc #b10))
                                 ;; No prefetch instruction with funny address modes.
                                 nil)
                                (t
                                 (aref *load/store-opcodes* opcode-byte))))))))
    (when (and simd&fp (> scale 4))
      (return-from load/store-register
        (values nil :load/store-register-simd&fp-scale)))
    (when (and simd&fp unpriviliged)
      (return-from load/store-register
        (values nil :load/store-register-simd&fp-unpriviliged)))
    (when (not opcode)
      (return-from load/store-register
        (values nil :load/store-register)))
    (make-instance 'arm64-instruction
                   :opcode opcode
                   ;; FIXME: Register decode here is wrong
                   :operands (list (decode-gp64 (ldb +rt+ word))
                                   address))))

;; Load/store register (register offset)
(defun load/store-register-register-offset (context word)
  (values nil :load/store-register-register-offset))

;; Load/store register (unsigned immediate)
(defun load/store-register-unsigned-immediate (context word)
  (declare (ignore context))
  (let* ((immediate (ldb (byte 12 10) word))
         (address-reg (decode-gp64 (ldb +rn+ word) :sp t))
         (simd&fp (logbitp +v-bit+ word))
         (size (ldb (byte 2 30) word))
         (opc (ldb (byte 2 22) word))
         (scale (if simd&fp
                    (logior (if (logbitp 1 opc) #b100 0)
                            size)
                    size))
         (address (if (zerop immediate)
                      (list address-reg)
                      (list address-reg (ash immediate scale))))
         (opcode-byte (logior (ash size 2) opc))
         (opcode (cond (simd&fp
                        (return-from load/store-register-unsigned-immediate
                          (values nil :load/store-register-unsigned-immediate-simd&fp)))
                       (t
                        (aref *load/store-opcodes* opcode-byte)))))
    (when (and simd&fp (> scale 4))
      (return-from load/store-register-unsigned-immediate
        (values nil :load/store-register-unsigned-immediate-simd&fp-scale)))
    (when (not opcode)
      (return-from load/store-register-unsigned-immediate
        (values nil :load/store-register-unsigned-immediate)))
    (make-instance 'arm64-instruction
                   :opcode opcode
                   :operands (list (decode-gp64 (ldb +rt+ word))
                                   address))))

(defun loads-and-stores (context word)
  (cond ((eql (logand word #x3F000000) #x08000000)
         (load/store-exclusive context word))
        ((eql (logand word #x3B000000) #x18000000)
         (load-register-literal context word))
        ((eql (logand word #x3A000000) #x28000000)
         (load/store-pair context word))
        ((eql (logand word #x3B200000) #x38000000)
         (load/store-register context word))
        ((eql (logand word #x3B200C00) #x38200800)
         (load/store-register-register-offset context word))
        ((eql (logand word #x3B000000) #x39000000)
         (load/store-register-unsigned-immediate context word))
        (t (values nil :loads-and-stores))))

(defun data-processing-register (context word)
  (values nil :data-processing-register))

(defun data-processing-simd (context word)
  (values nil :data-processing-simd))

(defun data-processing-immediate (context word)
  (values nil :data-processing-immediate))

(defun branches-exceptions-system (context word)
  (values nil :branches-exceptions-system))

(defmethod dis:disassemble-one-instruction ((context arm64-disassembler-context))
  (let ((word (dis:consume-ub32/le context)))
    (multiple-value-bind (inst decode-kind)
        (ecase (ldb (byte 4 25) word)
          ((#x0 #x1 #x2 #x3) (values nil :unallocated))
          ((#x4 #x6 #xC #xE) (loads-and-stores   context word))
          ((#x5 #xD) (data-processing-register   context word))
          ((#x7 #xF) (data-processing-simd       context word))
          ((#x8 #x9) (data-processing-immediate  context word))
          ((#xA #xB) (branches-exceptions-system context word)))
      (or inst
          (make-instance 'arm64-instruction :opcode :bad :operands (list word decode-kind))))))

(defmethod dis:print-instruction ((context arm64-disassembler-context) instruction
                                  &key (print-annotations t) (print-labels t))
  (format t "(")
  (format t "~A" (inst-opcode instruction))
  (dolist (operand (inst-operands instruction))
    (format t " ")
    (if (integerp operand)
        (format t "~8,'0X" operand)
        (format t "~S" operand)))
  (format t ")"))
