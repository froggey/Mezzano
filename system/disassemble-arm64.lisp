;;;; DISASSEMBLE for arm64

(defpackage :mezzano.disassemble.arm64
  (:use :cl)
  (:local-nicknames (:int :mezzano.internals)
                    (:a64 :mezzano.lap.arm64)
                    (:dis :mezzano.disassemble)))

(in-package :mezzano.disassemble.arm64)

(defclass arm64-disassembler-context (dis:disassembler-context)
  ((%decoding-jump-table-p :initform nil :accessor decoding-jump-table-p)
   (%in-literal-pool-p :initform nil :accessor in-literal-pool-p)))

(defmethod dis:make-disassembler-context-using-architecture ((architecture mezzano.compiler::arm64-target) &rest initargs)
  (apply #'make-instance 'arm64-disassembler-context initargs))

(defclass arm64-instruction (dis:instruction)
  ((%opcode :initarg :opcode :reader inst-opcode)
   (%operands :initarg :operands :reader inst-operands)))

(defmethod dis:inst-size ((inst arm64-instruction))
  (case (inst-opcode inst)
    ((:d64/le :jump-target) 8)
    (t 4)))

(defun load/store-exclusive (context word)
  (values nil :load/store-exclusive))

(defconstant +v-bit+ 26)
(defconstant +l-bit+ 22)

(defconstant +rd+ (byte 5 0))
(defconstant +rt+ (byte 5 0))
(defconstant +rn+ (byte 5 5))
(defconstant +rm+ (byte 5 16))
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

(defun decode-gp (register-number &key sf sp)
  (if (eql sf 0)
      (decode-gp32 register-number :sp sp)
      (decode-gp64 register-number :sp sp)))

(defun decode-fp (register-number type)
  (aref (ecase type
          (0 #(:s0  :s1  :s2  :s3  :s4  :s5  :s6  :s7
               :s8  :s9  :s10 :s11 :s12 :s13 :s14 :s15
               :s16 :s17 :s18 :s19 :s20 :s21 :s22 :s23
               :s24 :s25 :s26 :s27 :s28 :s29 :s30 :s31))
          (1 #(:d0  :d1  :d2  :d3  :d4  :d5  :d6  :d7
               :d8  :d9  :d10 :d11 :d12 :d13 :d14 :d15
               :d16 :d17 :d18 :d19 :d20 :d21 :d22 :d23
               :d24 :d25 :d26 :d27 :d28 :d29 :d30 :d31))
          (2 #(:q0  :q1  :q2  :q3  :q4  :q5  :q6  :q7
               :q8  :q9  :q10 :q11 :q12 :q13 :q14 :q15
               :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
               :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))
          (3 #(:h0  :h1  :h2  :h3  :h4  :h5  :h6  :h7
               :h8  :h9  :h10 :h11 :h12 :h13 :h14 :h15
               :h16 :h17 :h18 :h19 :h20 :h21 :h22 :h23
               :h24 :h25 :h26 :h27 :h28 :h29 :h30 :h31)))
        register-number))

(defun decode-simd (register-number)
  (aref #(:v0  :v1  :v2  :v3  :v4  :v5  :v6  :v7
          :v8  :v9  :v10 :v11 :v12 :v13 :v14 :v15
          :v16 :v17 :v18 :v19 :v20 :v21 :v22 :v23
          :v24 :v25 :v26 :v27 :v28 :v29 :v30 :v31)
        register-number))

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
                          (lambda (reg) (decode-fp reg (ldb (byte 2 30) word)))
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
         (scale (if (not (zerop (ldb (byte 2 10) word)))
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
                        (if (logbitp 0 opc)
                            'a64:ldr
                            'a64:str))
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
                   :operands (list (if simd&fp
                                       (cond ((and (eql size 2) (not (logbitp 1 opc)))
                                              (decode-fp (ldb +rt+ word) 0))
                                             ((and (eql size 3) (not (logbitp 1 opc)))
                                              (decode-fp (ldb +rt+ word) 1))
                                             ((and (eql size 0) (logbitp 1 opc))
                                              (decode-fp (ldb +rt+ word) 2)))
                                       ;; FIXME: Register decode here is wrong
                                       (decode-gp64 (ldb +rt+ word)))
                                   address))))

;; Load/store register (register offset)
(defun load/store-register-register-offset (context word)
  (declare (ignore context))
  (let* ((simd&fp (logbitp +v-bit+ word))
         (size (ldb (byte 2 30) word))
         (opc (ldb (byte 2 22) word))
         (option (ldb (byte 3 13) word))
         (opcode (if simd&fp
                     (case opc
                       (0 'a64:str)
                       (1 'a64:ldr))
                     (case size
                       (0 (case opc
                            (0 'a64:strb)
                            (1 'a64:ldrb)
                            ((2 3) 'a64:ldrsb)))
                       (1 (case opc
                            (0 'a64:strh)
                            (1 'a64:ldrh)
                            ((2 3) 'a64:ldrsh)))
                       (2 (case opc
                            (0 'a64:str)
                            (1 'a64:ldr)
                            (2 'a64:ldrsw)))
                       (3 (case opc
                            (0 'a64:str)
                            (1 'a64:ldr))))))
         (s (logbitp 12 word))
         ;; FIXME: Address decode here is wrong, particularly around the shift count
         (address (list* (decode-gp64 (ldb +rn+ word) :sp t)
                         (if (logbitp 0 option)
                             (decode-gp64 (ldb +rm+ word))
                             (decode-gp32 (ldb +rm+ word)))
                         (unless (and (eql option 3) (not s))
                           (list (case option
                                   (2 :uxtw)
                                   (3 :lsl)
                                   (6 :sxtw)
                                   (7 :sxtx))
                                 (if s
                                     size
                                     0))))))
    (when (not opcode)
      (return-from load/store-register-register-offset
        (values nil :load/store-register-register-offset)))
    (make-instance 'arm64-instruction
                   :opcode opcode
                   ;; FIXME: Register decode here is wrong
                   :operands (list (decode-gp64 (ldb +rt+ word))
                                   address))))

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

(defun logical-shifted-register (context word)
  (declare (ignore context))
  (let ((sf (ldb (byte 1 31) word))
        (shift (case (ldb (byte 2 22) word)
                 (0 :lsl)
                 (1 :lsr)
                 (2 :asr)
                 (3 nil)))
        (imm6 (ldb (byte 6 10) word))
        (opcode (aref (if (logbitp 21 word)
                          #(a64:bic a64:orn a64:eon a64:bics)
                          #(a64:and a64:orr a64:eor a64:ands))
                      (ldb (byte 2 29) word))))
    (when (and (zerop sf)
               (logbitp 5 imm6))
      (return-from logical-shifted-register
        (values nil :logical-shifted-register)))
    (make-instance 'arm64-instruction
                   :opcode opcode
                   :operands (list* (decode-gp (ldb +rd+ word) :sf sf)
                                    (decode-gp (ldb +rn+ word) :sf sf)
                                    (decode-gp (ldb +rm+ word) :sf sf)
                                    (unless (zerop imm6)
                                      (list shift imm6))))))

(defun add/sub-shifted-register (context word)
  (declare (ignore context))
  (let* ((sf (ldb (byte 1 31) word))
         (op (ldb (byte 2 29) word))
         (shift (case (ldb (byte 2 22) word)
                  (0 :lsl)
                  (1 :lsr)
                  (2 :asr)
                  (3 nil)))
         (imm6 (ldb (byte 6 10) word))
         (opcode (aref #(a64:add a64:adds a64:sub a64:subs) op)))
    (when (or (not shift)
              (and (zerop sf)
                   (logbitp 5 imm6)))
      (return-from add/sub-shifted-register
        (values nil :add/sub-shifted-register)))
    (make-instance 'arm64-instruction
                   :opcode opcode
                   :operands (list* (decode-gp (ldb +rd+ word) :sf sf)
                                    (decode-gp (ldb +rn+ word) :sf sf)
                                    (decode-gp (ldb +rm+ word) :sf sf)
                                    (unless (zerop imm6)
                                      (list shift imm6))))))

(defun add/sub-extended-register (context word)
  (declare (ignore context))
  (let* ((sf (ldb (byte 1 31) word))
         (op (ldb (byte 1 30) word))
         (s (ldb (byte 1 29) word))
         (opt (ldb (byte 2 22) word))
         (option (ldb (byte 3 13) word))
         (imm3 (ldb (byte 3 10) word))
         (opcode (cond ((and (eql op 0) (eql s 0)) 'a64:add)
                       ((and (eql op 0) (eql s 1)) 'a64:adds)
                       ((and (eql op 1) (eql s 0)) 'a64:sub)
                       ((and (eql op 1) (eql s 1)) 'a64:subs)))
         (extend (ecase option
                   (0 :uxtb)
                   (1 :uxth)
                   (2 (if (zerop sf) :lsl :uxtw))
                   (3 (if (zerop sf) :uxtx :lsl))
                   (4 :sxtb)
                   (5 :sxth)
                   (6 :sxtw)
                   (7 :sxtx))))
    (when (or (not (eql opt 0))
              (not (eql (logand imm3 4) 0)))
      (return-from add/sub-extended-register
        (values nil :add/sub-extended-register)))
    (make-instance 'arm64-instruction
                   :opcode opcode
                   :operands `(,(decode-gp (ldb +rd+ word) :sf sf :sp (member opcode '(a64:add a64:sub)))
                               ,(decode-gp (ldb +rn+ word) :sf sf :sp t)
                               ,(decode-gp (ldb +rm+ word) :sf sf)
                               ,@(if (not (and (eql extend :lsl) (zerop imm3)))
                                     (list extend imm3))))))

(defun conditional-select (context word)
  (declare (ignore context))
  (let* ((sf (logbitp 31 word))
         (op (logbitp 30 word))
         (s (logbitp 29 word))
         (cond (aref #(:eq :ne :cs :cc :mi :pl :vs :vc :hi :ls :ge :lt :gt :le :al nil)
                     (ldb (byte 4 12) word)))
         (op2 (ldb (byte 2 10) word))
         (opcode (if (logbitp 0 op2)
                     (if op
                         :csneg
                         :csinc)
                     (if op
                         :csinv
                         :csel)))
         (rm (decode-gp (ldb +rm+ word) :sf sf))
         (rn (decode-gp (ldb +rn+ word) :sf sf))
         (rd (decode-gp (ldb +rd+ word) :sf sf)))
    (when (or (not cond)
              (logbitp 1 op2)
              s)
      (return-from conditional-select
        (values nil :conditional-select)))
    (let ((opcode (intern
                   (format nil "~A.~A" opcode cond)
                   (find-package :mezzano.lap.arm64))))
      (make-instance 'arm64-instruction
                     :opcode opcode
                     :operands (list rd rn rm)))))

(defun data-processing-register (context word)
  (let ((op0 (ldb (byte 1 30) word))
        (op1 (ldb (byte 1 28) word))
        (op2 (ldb (byte 4 21) word))
        (op3 (ldb (byte 1 11) word)))
    (cond ((and (eql op0 1) (eql op1 1) (eql op2 6))
           (values nil :data-processing-2-source))
          ((and (eql op0 0) (eql op1 1) (eql op2 6))
           (values nil :data-processing-1-source))
          ((and (eql op1 0) (eql (logand op2 #x8) 0))
           (logical-shifted-register context word))
          ((and (eql op1 0) (eql (logand op2 #x9) 8))
           (add/sub-shifted-register context word))
          ((and (eql op1 0) (eql (logand op2 #x9) 9))
           (add/sub-extended-register context word))
          ((and (eql op1 1) (eql op2 0))
           (values nil :add/sub-with-carry))
          ((and (eql op1 1) (eql op2 2) (eql op3 0))
           (values nil :conditional-compare-register))
          ((and (eql op1 1) (eql op2 2) (eql op3 1))
           (values nil :conditional-compare-immediate))
          ((and (eql op1 1) (eql op2 4))
           (conditional-select context word))
          ((and (eql op1 1) (eql (logand op2 #x8) 8))
           (values nil :data-processing-3-source))
          (t
           (values nil :data-processing-register)))))

(defun fmov-register (context word)
  (declare (ignore context))
  (let ((type (ldb (byte 2 22) word)))
    (make-instance 'arm64-instruction
                   :opcode 'a64:fmov
                   :operands `(,(decode-fp (ldb +rd+ word) type)
                               ,(decode-fp (ldb +rn+ word) type)))))

(defun fmov-general (context word)
  (declare (ignore context))
  (let ()
    (if (logbitp 16 word)
        (make-instance 'arm64-instruction
                       :opcode 'a64:fmov
                       :operands `(,(decode-fp (ldb +rd+ word) type)
                                   ,(decode-gp (ldb +rn+ word) :sf sf)))
        (make-instance 'arm64-instruction
                       :opcode 'a64:fmov
                       :operands `(,(decode-gp (ldb +rd+ word) :sf sf)
                                   ,(decode-fp (ldb +rn+ word) type))))))

(defun conversions-between-floating-point-and-integer (context word)
  (declare (ignore context))
  (let* ((sf (ldb (byte 1 31) word))
         (type (ldb (byte 2 22) word))
         (opcode-and-rmode (ldb (byte 5 16) word))
         (opcode (aref
                  ;; 000    001    010   011   100    101    110  111
                  #(a64:fcvtns :fcvtnu a64:scvtf :ucvtf :fcvtas :fcvtau a64:fmov a64:fmov ; 00
                    :fcvtps :fcvtpu nil   nil   nil    nil    nil  nil  ; 01
                    :fcvtms :fcvtmu nil   nil   nil    nil    nil  nil  ; 10
                    a64:fcvtzs :fcvtzu nil   nil   nil    nil    nil  nil) ; 11
                  opcode-and-rmode))
         (direction (or (eql opcode-and-rmode 2) ; scvtf
                        (eql opcode-and-rmode 3) ; ucvtf
                        (eql opcode-and-rmode 7)))) ; fmov
    (unless opcode
      (return-from conversions-between-floating-point-and-integer
        (values nil :conversions-between-floating-point-and-integer)))
    (if direction
        ;; general to float
        (make-instance 'arm64-instruction
                       :opcode opcode
                       :operands `(,(decode-fp (ldb +rd+ word) type)
                                   ,(decode-gp (ldb +rn+ word) :sf sf)))
        (make-instance 'arm64-instruction
                       :opcode opcode
                       :operands `(,(decode-gp (ldb +rd+ word) :sf sf)
                                   ,(decode-fp (ldb +rn+ word) type))))))

(defun floating-point-data-processing-2-source (context word)
  (declare (ignore context))
  (let ((opcode (aref #(a64:fmul a64:fdiv a64:fadd a64:fsub
                        :fmax :fmin :fmaxnm :fminnm
                        :fnmul nil nil nil
                        nil nil nil nil)
                      (ldb (byte 4 12) word)))
        (type (ldb (byte 2 22) word)))
    (unless opcode
      (return-from floating-point-data-processing-2-source
        (values nil :floating-point-data-processing-2-source)))
    (make-instance 'arm64-instruction
                   :opcode opcode
                   :operands `(,(decode-fp (ldb +rd+ word) type)
                               ,(decode-fp (ldb +rn+ word) type)
                               ,(decode-fp (ldb +rm+ word) type)))))

(defun data-processing-simd (context word)
  (cond
    ((eql (logand word #xFF3FFC00) #x1E204000)
     (fmov-register context word))
    ;((eql (logand word #x7F36FC00) #x1E260000)
    ; (fmov-general context word))
    ((eql (logand word #xFF20FC17) #x1E202000)
     (let ((type (ldb (byte 2 22) word)))
       (make-instance 'arm64-instruction
                      :opcode 'a64:fcmp
                      :operands `(,(decode-fp (ldb +rn+ word) type)
                                  ,(decode-fp (ldb +rm+ word) type)))))
    ((eql (logand word #x7F20FC00) #x1E200000)
     (conversions-between-floating-point-and-integer context word))
    ((eql (logand word #xFFA00C00) #x1E200800)
     (floating-point-data-processing-2-source context word))
    (t
     (values nil :data-processing-simd))))

(defun pcrel-addressing (context word)
  (declare (ignore context))
  (let ((opcode (if (eql (ldb (byte 1 31) word) 0)
                    'a64:adr
                    ':adrp))
        (imm (int::sign-extend
              (logior (ash (ldb (byte 19 5) word) 2)
                      (ldb (byte 2 29) word))
              21)))
    (make-instance 'arm64-instruction
                   :opcode opcode
                   :operands `(,(decode-gp64 (ldb +rd+ word))
                               (:pc ,(if (eql opcode 'a64:adr)
                                         imm
                                         (* imm 4096)))))))

(defun add/sub-immediate (context word)
  (declare (ignore context))
  (let* ((sf (ldb (byte 1 31) word))
         (op-s (ldb (byte 2 29) word))
         (shift (ldb (byte 2 22) word))
         (opcode (ecase op-s
                   (0 'a64:add)
                   (1 'a64:adds)
                   (2 'a64:sub)
                   (3 'a64:subs))))
    (when (eql (logand shift 2) 2)
      (return-from add/sub-immediate
        (values nil :add/sub-immediate)))
    (make-instance 'arm64-instruction
                   :opcode opcode
                   :operands `(,(decode-gp (ldb +rd+ word) :sf sf :sp (member opcode '(a64:add a64:sub)))
                               ,(decode-gp (ldb +rn+ word) :sf sf :sp t)
                               ,(ldb (byte 12 10) word)
                               ,@(if (not (zerop shift))
                                     (list :lsl 12))))))

(defun logical-immediate (context word)
  (declare (ignore context))
  (let ((sf (ldb (byte 1 31) word))
        (n (ldb (byte 1 22) word))
        (immr (ldb (byte 6 16) word))
        (imms (ldb (byte 6 10) word))
        (opcode (aref #(a64:and a64:orr a64:eor a64:ands)
                      (ldb (byte 2 29) word))))
    (when (and (eql sf 0) (eql n 1))
      (return-from logical-immediate
        (values nil :logical-immediate)))
    (make-instance 'arm64-instruction
                   :opcode opcode
                   :operands `(,(decode-gp (ldb +rd+ word) :sf sf :sp t)
                               ,(decode-gp (ldb +rn+ word) :sf sf :sp t)
                               ;; FIXME: Decode this properly
                               ,immr ,imms))))

(defun move-wide-immediate (context word)
  (declare (ignore context))
  (let ((sf (ldb (byte 1 31) word))
        (hw (ldb (byte 2 21) word))
        (opcode (aref #(a64:movn nil a64:movz :movk)
                      (ldb (byte 2 29) word))))
    (when (or (not opcode)
              (and (zerop sf) (not (zerop (logand hw 2)))))
      (return-from move-wide-immediate
        (values nil :move-wide-immediate)))
    (make-instance 'arm64-instruction
                   :opcode opcode
                   :operands `(,(decode-gp (ldb +rd+ word) :sf sf :sp t)
                               ,(ldb (byte 16 5) word)
                               ,@(unless (zerop hw)
                                   `(:lsl ,(* hw 16)))))))

(defun bitfield (context word)
  (declare (ignore context))
  (let* ((sf (ldb (byte 1 31) word))
         (opc (ldb (byte 2 29) word))
         (n (ldb (byte 1 22) word))
         (rn (ldb +rn+ word))
         (immr (ldb (byte 6 16) word))
         (imms (ldb (byte 6 10) word))
         (opcode (aref #(a64:sbfm a64:bfm a64:ubfm nil) opc)))
    (when (or (not (eql sf n))
              (not opcode))
      (return-from bitfield
        (values nil :bitfield)))
    (cond
      ((and (eql opcode 'a64:bfm)
            (not (eql rn 31))
            (< imms immr))
       (make-instance 'arm64-instruction
                      :opcode 'a64:bfi
                      :operands (list (decode-gp (ldb +rd+ word) :sf sf)
                                      (decode-gp rn :sf sf)
                                      (- (if (zerop sf) 32 64) immr)
                                      (1+ imms))))
      ((and (eql opcode 'a64:bfm)
            (eql rn 31)
            (< imms immr))
       (make-instance 'arm64-instruction
                      :opcode 'a64:bfc
                      :operands (list (decode-gp (ldb +rd+ word) :sf sf)
                                      (- (if (zerop sf) 32 64) immr)
                                      (1+ imms))))
      (t
       (make-instance 'arm64-instruction
                      :opcode opcode
                      :operands (list (decode-gp (ldb +rd+ word) :sf sf)
                                      (decode-gp rn :sf sf)
                                      immr
                                      imms))))))

(defun data-processing-immediate (context word)
  (case (ldb (byte 3 23) word)
    ((#x0 #x1) (pcrel-addressing context word))
    ((#x2 #x3) (add/sub-immediate context word))
    (#x4 (logical-immediate context word))
    (#x5 (move-wide-immediate context word))
    (#x6 (bitfield context word))
    (#x7 (values nil :extract))))

(defun conditional-branch-immediate (context word)
  (declare (ignore context))
  (when (not (and (not (logbitp 24 word)) (not (logbitp 4 word))))
    (return-from conditional-branch-immediate
      (values nil :conditional-branch-immediate)))
  (make-instance 'arm64-instruction
                 :opcode
                 (aref #(a64:b.eq a64:b.ne
                         a64:b.cs a64:b.cc
                         a64:b.mi a64:b.pl
                         a64:b.vs a64:b.vc
                         a64:b.hi a64:b.ls
                         a64:b.ge a64:b.lt
                         a64:b.gt a64:b.le
                         a64:b.al :b.nv)
                       (ldb (byte 4 0) word))
                 :operands (list `(:pc ,(* (int::sign-extend (ldb (byte 19 5) word) 19) 4)))))

(defun exception-generation (context word)
  (declare (ignore context))
  (let* ((opc (ldb (byte 3 21) word))
         (op2 (ldb (byte 3 2) word))
         (ll (ldb (byte 2 0) word))
         (opcode (cond
                   ((not (zerop op2)) nil)
                   ((and (eql opc 0) (eql ll 1)) :svc)
                   ((and (eql opc 0) (eql ll 2)) :hvc)
                   ((and (eql opc 0) (eql ll 3)) :smc)
                   ((and (eql opc 1) (eql ll 0)) 'a64:brk)
                   ((and (eql opc 2) (eql ll 0)) 'a64:hlt))))
    (when (not opcode)
      (return-from exception-generation
        (values nil :exception-generation)))
    (make-instance 'arm64-instruction
                 :opcode opcode
                 :operands (list (ldb (byte 16 5) word)))))

(defun msr-immediate (context word)
  (declare (ignore context))
  (let* ((imm (ldb (byte 4 8) word))
         (pstatefield (logior (ash (ldb (byte 3 16) word) 3)
                              (ldb (byte 3 5) word)))
         (name (case pstatefield
                 (5 :spsel)
                 (30 :daifset)
                 (31 :daifclr)
                 (t pstatefield))))
    (make-instance 'arm64-instruction
                   :opcode 'a64:msr
                   :operands (list name imm))))

(defun hint (context word)
  (declare (ignore context))
  (let* ((imm (ldb (byte 7 5) word))
         (special (case imm
                    (0 'a64:nop)
                    (1 'a64:yield)
                    (2 'a64:wfe)
                    (3 'a64:wfi)
                    (4 'a64:sev)
                    (5 'a64:sevl))))
    (if special
        (make-instance 'arm64-instruction
                       :opcode special
                       :operands '())
        (make-instance 'arm64-instruction
                       :opcode :hint
                       :operands (list imm)))))

(defun msr/mrs (context word)
  (declare (ignore context))
  (let* ((op0 (ldb (byte 2 19) word))
         (op1 (ldb (byte 3 16) word))
         (crn (ldb (byte 4 12) word))
         (crm (ldb (byte 4 8) word))
         (op2 (ldb (byte 3 5) word))
         (name (or (car (rassoc (list (list op0 op1 crn crm op2))
                                a64::*system-registers*
                                :test 'equal))
                   (intern
                    (format nil "S~D_~D_~D_~D_~D"
                            op0 op1 crn crm op2)
                    (find-package :keyword))))
         (rt (decode-gp64 (ldb +rt+ word))))
    (if (logbitp 21 word)
        (make-instance 'arm64-instruction
                       :opcode 'a64:mrs
                       :operands (list rt name))
        (make-instance 'arm64-instruction
                       :opcode 'a64:msr
                       :operands (list name rt)))))

(defun system (context word)
  (cond ((eql (logand word #xFFF8F01F) #xD500401F)
         (msr-immediate context word))
        ((eql (logand word #xFFFFF01F) #xD503201F)
         (hint context word))
        ((eql (logand word #xFFD00000) #xD5100000)
         (msr/mrs context word))
        (t
         (values nil :system))))

(defun unconditional-branch-register (context word)
  (declare (ignore context))
  (let ((opc (ldb (byte 4 21) word))
        (op2 (ldb (byte 5 16) word))
        (op3 (ldb (byte 6 10) word))
        (op4 (ldb (byte 5 0) word)))
    (cond ((and (eql opc 0) (eql op2 31) (eql op3 0) (eql op4 0))
           (make-instance 'arm64-instruction
                   :opcode 'a64:br
                   :operands (list (decode-gp64 (ldb +rn+ word)))))
          ((and (eql opc 1) (eql op2 31) (eql op3 0) (eql op4 0))
           (make-instance 'arm64-instruction
                   :opcode 'a64:blr
                   :operands (list (decode-gp64 (ldb +rn+ word)))))
          ((and (eql opc 2) (eql op2 31) (eql op3 0) (eql op4 0))
           (make-instance 'arm64-instruction
                   :opcode 'a64:ret
                   :operands '()))
          ((and (eql opc 4) (eql op2 31) (eql op3 0) (eql op4 0))
           (make-instance 'arm64-instruction
                   :opcode 'a64:eret
                   :operands '()))
          (t
           (values nil :unconditional-branch-register)))))

(defun unconditional-branch-immediate (context word)
  (declare (ignore context))
  (make-instance 'arm64-instruction
                 :opcode (if (logbitp 31 word) ':bl 'a64:b)
                 :operands (list `(:pc ,(* (int::sign-extend (ldb (byte 26 0) word) 26) 4)))))

(defun compare-and-branch-immediate (context word)
  (declare (ignore context))
  (let ((sf (ldb (byte 1 31) word))
        (opcode (ecase (ldb (byte 1 24) word)
                  (0 'a64:cbz)
                  (1 'a64:cbnz))))
    (make-instance 'arm64-instruction
                   :opcode opcode
                   :operands `(,(decode-gp (ldb +rt+ word) :sf sf)
                               (:pc ,(ash (int::sign-extend (ldb (byte 19 5) word) 19) 4))))))

(defun branches-exceptions-system (context word)
  (let ((op0 (ldb (byte 3 29) word))
        (op1 (ldb (byte 4 22) word)))
    (cond ((and (eql op0 2) (eql (logand op1 #x8) 0))
           (conditional-branch-immediate context word))
          ((and (eql op0 6) (eql (logand op1 #xC) 0))
           (exception-generation context word))
          ((and (eql op0 6) (eql op1 4))
           (system context word))
          ((and (eql op0 6) (eql (logand op1 #x8) 8))
           (unconditional-branch-register context word))
          ((or (eql op0 0) (eql op0 4))
           (unconditional-branch-immediate context word))
          ((and (or (eql op0 1) (eql op0 5)) (eql (logand op1 #x8) 0))
           (compare-and-branch-immediate context word))
          ((and (or (eql op0 1) (eql op0 5)) (eql (logand op1 #x8) 8))
           (values nil :test-and-branch-immediate))
          (t
           (values nil :branches-exceptions-system)))))

(defun disassemble-one-instruction-1 (context)
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

(defmethod dis:disassemble-one-instruction ((context arm64-disassembler-context))
  (let* ((start (dis:context-code-offset context))
         (code-start 16)
         (code-end (dis:code-end (dis:context-function context))))
    (cond ((and (decoding-jump-table-p context)
                (>= start (decoding-jump-table-p context)))
           (let* ((dest (dis:consume-ub64/le context))
                  (absolute-dest (+ (decoding-jump-table-p context) dest)))
             ;; Best guess...
             ;; If the destination is outside the function's code,
             ;; then this is probably past the end of the table.
             (cond ((<= code-start absolute-dest code-end)
                    (let ((inst (make-instance
                                 'arm64-instruction
                                 :opcode :jump-target
                                 :operands (list (decoding-jump-table-p context) dest))))
                      (setf (slot-value inst 'dis::%offset) start)
                      (dis:label context absolute-dest :createp t)
                      inst))
                   (t
                    ;; Rewind & try again.
                    (setf (decoding-jump-table-p context) nil
                          (slot-value context 'dis::%offset) start)
                    (dis:disassemble-one-instruction context)))))
          (t
           (let ((inst (disassemble-one-instruction-1 context)))
             ;; If this is a bad instruction and the previous was some kind
             ;; of terminator instruction (ret or br) then assume we're in the literal pool.
             (when (or (in-literal-pool-p context)
                       (and (eql (inst-opcode inst) :bad)
                            (plusp (length (dis:context-instructions context)))
                            (member (inst-opcode
                                     (aref (dis:context-instructions context)
                                           (1- (length (dis:context-instructions context)))))
                                    '(a64:ret a64:br a64:b))))
               (setf (in-literal-pool-p context) t)
               (cond ((logtest start 7)
                      ;; Literal pool always start 16b-aligned
                      (setf inst (make-instance 'arm64-instruction
                                               :opcode :align
                                               :operands (list 16))))
                     (t
                      (decf (slot-value context 'dis::%offset) 4) ; because we consumed 4 bytes.
                      (setf inst (make-instance 'arm64-instruction
                                                :opcode :d64/le
                                                :operands (list (dis:consume-ub64/le context)))))))
             ;; Identify if we're starting a jump table.
             ;; The ADR instruction is only used for jump tables or as part of the invalid
             ;; arguments handling. The latter always has a negative address to materialize
             ;; the address of the function.
             (when (and (eql (inst-opcode inst) 'a64:adr)
                        (plusp (second (second (inst-operands inst)))))
               (let ((start (+ (dis:context-code-offset context)
                               -4 ; since we advanced pc
                               (second (second (inst-operands inst))))))
                 (dis:label context start :createp t)
                 (setf (decoding-jump-table-p context) start)))
             (setf (slot-value inst 'dis::%offset) start)
             ;; Update labels.
             (dolist (operand (inst-operands inst))
               (when (and (listp operand)
                          (eql (first operand) :pc)
                          (<= code-start
                              (+ (dis:context-code-offset context) -4 (second operand))
                              (1- code-end)))
                 (dis:label context
                            (+ (dis:context-code-offset context)
                               -4
                               (second operand))
                            :createp t)))
             inst)))))

(defmethod dis:print-instruction ((context arm64-disassembler-context) instruction
                                  &key (print-annotations t) (print-labels t))
  (let ((annotations '()))
    (format t "(")
    (cond ((eql (inst-opcode instruction) :jump-target)
           (format t ":D64/LE (- L~D L~D)"
                   (dis:label context (+ (first (inst-operands instruction))
                                         (second (inst-operands instruction))))
                   (dis:label context (first (inst-operands instruction)))))
          (t
           (format t "~A" (inst-opcode instruction))
           (dolist (operand (inst-operands instruction))
             (format t " ")
             (cond ((integerp operand)
                    (let ((operand (if (eql (inst-opcode instruction) 'a64:movn)
                                       (lognot operand)
                                       operand)))
                      (push (format nil "~D" operand) annotations)
                      (multiple-value-bind (object validp)
                          (dis:decode-object-from-integer operand)
                        (when validp
                          (push (format nil "'~S" object) annotations))))
                    (format t "#x~8,'0X" operand))
                   ((and (listp operand)
                         (eql (first operand) :pc))
                    (let* ((address (logand (int::lisp-object-address
                                             (dis:context-function context))
                                            -16))
                           (target (+ (dis:inst-offset instruction)
                                      (second operand)))
                           (pool-index (truncate (- target (dis:code-end (dis:context-function context))) 8))
                           (label (dis:label context target)))
                      (when (and (member (inst-opcode instruction) '(a64:ldr))
                                 (not (logtest target #b111))
                                 (<= 0 target (int::function-code-size (dis:context-function context))))
                        ;; This is probably in the literal pool. TODO: Use the instruction & register
                        ;; to determine exactly what type to read
                        (let ((value (int::%object-ref-unsigned-byte-64-unscaled
                                      (dis:context-function context)
                                      (- target 8))))
                          (push (format nil "#x~X" value) annotations)
                          (push (format nil "~D" (int::sign-extend value 64)) annotations)
                          (multiple-value-bind (object validp)
                              (dis:decode-object-from-integer value)
                            (when validp
                              (push (format nil "'~S" object) annotations)))))
                      (cond
                        ((and print-labels label)
                         (push (format nil "#x~8,'0X" (+ address target)) annotations)
                         (format t "L~D" label))
                        (t
                         (when (and (not (logtest target #b111))
                                    (<= 0 pool-index)
                                    (not (int::function-reference-p (dis:context-function context)))
                                    (< pool-index (int::function-pool-size (dis:context-function context))))
                           (let ((pool-object (int::function-pool-object (dis:context-function context) pool-index)))
                             (push
                              (let ((*print-lines* 1)
                                    (*print-length* 2)
                                    (*print-level* 2))
                                (format nil "'~S" pool-object))
                              annotations)))
                         (format t "(:PC #x~X)" (+ address target))))))
                   (t
                    (when (and (listp operand)
                               (integerp (second operand))
                               (eql (logand (second operand) 7) 7))
                      (let ((slot (truncate (+ (second operand) 1) 8)))
                        (push (format nil "slot ~D" slot) annotations)))
                    (when (and (listp operand)
                               (endp (cddr operand)))
                      (when (eql (second operand) (- int::+tag-cons+))
                        (push "car" annotations))
                      (when (eql (second operand) (+ (- int::+tag-cons+) 8))
                        (push "cdr" annotations)))
                    (format t "~S" operand))))))
    (format t ")")
    (when (and print-annotations annotations)
      (format t "~85T; ~A" (pop annotations))
      (dolist (an annotations)
        (format t ", ~A" an)))))
