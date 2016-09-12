;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.lap.arm64)

(defparameter *instruction-assemblers* (make-hash-table))
(defvar *function-reference-resolver*)

(defun current-address ()
  sys.lap:*current-address*)

(defun assemble (code-list &rest args &key &allow-other-keys)
  (apply 'sys.lap:perform-assembly *instruction-assemblers* code-list args))

(defun add-instruction (name function)
  (unless (keywordp name)
    (export name :mezzano.lap.arm64))
  (setf (gethash name *instruction-assemblers*) function)
  name)

(defmacro define-instruction (name lambda-list &body body)
  (let ((fname (intern (format nil "~S-ASSEMBLER" name)))
        (insn (gensym "INSTRUCTION")))
    `(progn
       (defun ,fname (,insn)
         (destructuring-bind ,lambda-list (rest ,insn)
           (block instruction
             ,@body
             (error "Could not encode instruction ~S." ,insn))))
       (add-instruction ',name ',fname))))

(defun emit-byte (value)
  (check-type value (unsigned-byte 8))
  (sys.lap:emit value))

(defun emit-instruction (value)
  (check-type value (unsigned-byte 32))
  (assert (not (logtest (current-address) #b11)) ()
          "Instruction stream is misaligned.")
  (sys.lap:emit (ldb (byte 8 0) value)
                (ldb (byte 8 8) value)
                (ldb (byte 8 16) value)
                (ldb (byte 8 24) value)))

(defun resolve-immediate (value)
  "Convert an immediate value to an integer."
  (sys.lap:resolve-immediate value))

(defun register-class (register)
  (case register
    ((:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7 :x8 :x9
      :x10 :x11 :x12 :x13 :x14 :x15 :x16 :x17 :x18 :x19
      :x20 :x21 :x22 :x23 :x24 :x25 :x26 :x27 :x28 :x29
      :x30)
     :gpr-64)
    ((:w0 :w1 :w2 :w3 :w4 :w5 :w6 :w7 :w8 :w9
      :w10 :w11 :w12 :w13 :w14 :w15 :w16 :w17 :w18 :w19
      :w20 :w21 :w22 :w23 :w24 :w25 :w26 :w27 :w28 :w29
      :w30)
     :gpr-32)
    (:wsp :wsp)
    (:sp :sp)
    (:wzr :wzr)
    (:xzr :xzr)
    (:pc :pc)))

(defun check-register-class (register &rest classes)
  (assert (member (register-class register) classes)))

(defun register-number (register &optional restrict-to)
  (when restrict-to
    (assert (member (register-class register) restrict-to)))
  (ecase register
    ((:x0 :w0) 0)
    ((:x1 :w1) 1)
    ((:x2 :w2) 2)
    ((:x3 :w3) 3)
    ((:x4 :w4) 4)
    ((:x5 :w5) 5)
    ((:x6 :w6) 6)
    ((:x7 :w7) 7)
    ((:x8 :w8) 8)
    ((:x9 :w9) 9)
    ((:x10 :w10) 10)
    ((:x11 :w11) 11)
    ((:x12 :w12) 12)
    ((:x13 :w13) 13)
    ((:x14 :w14) 14)
    ((:x15 :w15) 15)
    ((:x16 :w16) 16)
    ((:x17 :w17) 17)
    ((:x18 :w18) 18)
    ((:x19 :w19) 19)
    ((:x20 :w20) 20)
    ((:x21 :w21) 21)
    ((:x22 :w22) 22)
    ((:x23 :w23) 23)
    ((:x24 :w24) 24)
    ((:x25 :w25) 25)
    ((:x26 :w26) 26)
    ((:x27 :w27) 27)
    ((:x28 :w28) 28)
    ((:x29 :w29) 29)
    ((:x30 :w30) 30)
    ((:wzr :xzr :sp) 31)))

(defun parse-address (address)
  (assert (consp address))
  (case (first address)
    (:constant
     (values :pc :pc
             `(:constant-address ,(second address))))
    (:function
     (values :pc :pc
             `(:constant-address ,(funcall *function-reference-resolver*
                                           (second address)))))
    (:pc
     (assert (and (rest address)
                  (endp (cddr address))))
     (values :pc :pc
             (second address)))
    (:pre
     (destructuring-bind (base imm)
         (rest address)
       (assert (member (register-class base) '(:gpr-64 :sp)) ()
               "Expected 64-bit integer register or sp as base address register.")
       (assert (not (register-class imm)))
       (values :pre base imm)))
    (:post
     (destructuring-bind (base imm)
         (rest address)
       (assert (member (register-class base) '(:gpr-64 :sp)) ()
               "Expected 64-bit integer register or sp as base address register.")
       (assert (not (register-class imm)))
       (values :post base imm)))
    (t
     ;; Base register only, or base plus some offset (displacement or scaled index).
     (let* ((base (first address)))
       (assert (member (register-class base) '(:gpr-64 :sp)) ()
               "Expected 64-bit integer register or sp as base register in address ~S." address)
       ;; Followed either by an immediate, an index register or nothing.
       (case (register-class (second address))
         (:gpr-64
          ;; Optionally followed by :SCALE.
          (cond ((eql (third address) :scale)
                 (values :base-plus-scaled-index64 base (second address)))
                ((endp (cddr address))
                 (values :base-plus-index64 base (second address)))
                (t
                 (error "Invalid address ~S." address))))
         #+(or)
         (:gpr-32
          ;; Followed by either :SXTW or :UXTW, then optionally followed by :SCALE.
          (setf index (second address))
          (let ((stuff (cddr address)))
            (when (member (first stuff) '(:uxtw :sxtw))
              (setf index-extend (pop stuff)))
            (when (eql (first stuff) :scale)
              (setf index-scale t)
              (pop stuff))
            (case (first stuff)
              (:pre
               (pop stuff)
               (setf writeback t))
              (:post
               (pop stuff)
               (setf writeback t
                     postindex t)))
            (assert (endp stuff))))
         ((nil)
          ;; Immediate.
          (assert (endp (cddr address)))
          (values :base-plus-immediate base (if (rest address)
                                                (second address)
                                                0)))
         (t (error "Invalid index or displacement value ~S." (second address))))))))

(defconstant +ldst-size-64-bit+ #x40000000)
(defconstant +ldst-size-32-bit+ #x00000000)

(defconstant +preindex-bit+ #x00000000)
(defconstant +postindex-bit+ #x00000800)

(defconstant +rd-shift+ 0)
(defconstant +rt-shift+ 0)
(defconstant +rn-shift+ 5)
(defconstant +rt2-shift+ 10)
(defconstant +rm-shift+ 16)

(defun encode-index-option (mode)
  (ecase mode
    (:base-plus-index64
     #x00006000)
    (:base-plus-scaled-index64
     #x00007000)))

(define-instruction ldr (reg value)
  (let* ((class (register-class reg))
         (size-bit (ecase class
                     (:gpr-64 +ldst-size-64-bit+)
                     (:gpr-32 +ldst-size-32-bit+))))
    (multiple-value-bind (mode base offset)
        (parse-address value)
      (ecase mode
        (:base-plus-immediate
         (let ((imm-value (or (resolve-immediate offset) 0)))
           (cond ((and (eql class :gpr-32)
                       (<= 0 imm-value 16380)
                       (zerop (logand imm-value #b11)))
                  ;; LDR (immediate, unsigned offset).
                  (emit-instruction (logior #xB9400000
                                            size-bit
                                            (ash (ash imm-value -2) 12)
                                            (ash (register-number base) +rn-shift+)
                                            (ash (register-number reg) +rt-shift+)))
                  (return-from instruction t))
                 ((and (eql class :gpr-64)
                       (<= 0 imm-value 32760)
                       (zerop (logand imm-value #b111)))
                  ;; LDR (immediate, unsigned offset).
                  (emit-instruction (logior #xB9400000
                                            size-bit
                                            (ash (ash imm-value -3) 12)
                                            (ash (register-number base) +rn-shift+)
                                            (ash (register-number reg) +rt-shift+)))
                  (return-from instruction t))
                 ((<= -256 imm-value 255)
                  ;; LDUR.
                  (emit-instruction (logior #xB8400000
                                            size-bit
                                            (ash (ldb (byte 9 0) imm-value) 12)
                                            (ash (register-number base) +rn-shift+)
                                            (ash (register-number reg) +rt-shift+)))
                  (return-from instruction t)))))
        ((:base-plus-index64
          :base-plus-scaled-index64)
         (emit-instruction (logior #xB8600800
                                   size-bit
                                   (ash (register-number offset) +rm-shift+)
                                   (encode-index-option mode)
                                   (ash (register-number base) +rn-shift+)
                                   (ash (register-number reg) +rt-shift+)))
         (return-from instruction t))
        (:pre
         (let ((imm-value (or (resolve-immediate offset) 0)))
           (when (<= -256 imm-value 255)
             ;; LDR (immediate, pre-index).
             (emit-instruction (logior #xB8400C00
                                       size-bit
                                       (ash (ldb (byte 9 0) imm-value) 12)
                                       (ash (register-number base) +rn-shift+)
                                       (ash (register-number reg) +rt-shift+)))
             (return-from instruction t))))
        (:post
         (let ((imm-value (or (resolve-immediate offset) 0)))
           (when (<= -256 imm-value 255)
             ;; LDR (immediate, pre-index).
             (emit-instruction (logior #xB8400400
                                       size-bit
                                       (ash (ldb (byte 9 0) imm-value) 12)
                                       (ash (register-number base) +rn-shift+)
                                       (ash (register-number reg) +rt-shift+)))
             (return-from instruction t))))
        (:pc
         (let ((imm-value (- (or (resolve-immediate offset) (current-address))
                             (current-address))))
           (when (and (not (logtest imm-value #b11))
                      (<= -1048576 imm-value 1048575))
             ;; LDR (literal)
             (emit-instruction (logior #x18000000
                                       size-bit
                                       (ash (ldb (byte 19 2) imm-value) 5)
                                       (ash (register-number reg) +rt-shift+)))
             (return-from instruction t))))))))

(define-instruction str (reg value)
  (let* ((class (register-class reg))
         (is-64-bit (member class '(:gpr-64 :xzr)))
         (size-bit (if is-64-bit
                       +ldst-size-64-bit+
                       +ldst-size-32-bit+)))
    (check-register-class reg :gpr-64 :gpr-32 :xzr :wzr)
    (multiple-value-bind (mode base offset)
        (parse-address value)
      (ecase mode
        (:base-plus-immediate
         (let ((imm-value (or (resolve-immediate offset) 0)))
           (cond ((and (not is-64-bit)
                       (<= 0 imm-value 16380)
                       (zerop (logand imm-value #b11)))
                  ;; STR (immediate, unsigned offset).
                  (emit-instruction (logior #xB9000000
                                            size-bit
                                            (ash (ash imm-value -2) 12)
                                            (ash (register-number base) +rn-shift+)
                                            (ash (register-number reg) +rt-shift+)))
                  (return-from instruction t))
                 ((and is-64-bit
                       (<= 0 imm-value 32760)
                       (zerop (logand imm-value #b111)))
                  ;; STR (immediate, unsigned offset).
                  (emit-instruction (logior #xB9000000
                                            size-bit
                                            (ash (ash imm-value -3) 12)
                                            (ash (register-number base) +rn-shift+)
                                            (ash (register-number reg) +rt-shift+)))
                  (return-from instruction t))
                 ((<= -256 imm-value 255)
                  ;; STUR.
                  (emit-instruction (logior #xB8000000
                                            size-bit
                                            (ash (ldb (byte 9 0) imm-value) 12)
                                            (ash (register-number base) +rn-shift+)
                                            (ash (register-number reg) +rt-shift+)))
                  (return-from instruction t)))))
        ((:base-plus-index64
          :base-plus-scaled-index64)
         (emit-instruction (logior #xB8200800
                                   size-bit
                                   (ash (register-number offset) +rm-shift+)
                                   (encode-index-option mode)
                                   (ash (register-number base) +rn-shift+)
                                   (ash (register-number reg) +rt-shift+)))
         (return-from instruction t))
        (:pre
         (let ((imm-value (or (resolve-immediate offset) 0)))
           (when (<= -256 imm-value 255)
             ;; STR (immediate, pre-index).
             (emit-instruction (logior #xB8000C00
                                       size-bit
                                       (ash (ldb (byte 9 0) imm-value) 12)
                                       (ash (register-number base) +rn-shift+)
                                       (ash (register-number reg) +rt-shift+)))
             (return-from instruction t))))
        (:post
         (let ((imm-value (or (resolve-immediate offset) 0)))
           (when (<= -256 imm-value 255)
             ;; STR (immediate, pre-index).
             (emit-instruction (logior #xB8000400
                                       size-bit
                                       (ash (ldb (byte 9 0) imm-value) 12)
                                       (ash (register-number base) +rn-shift+)
                                       (ash (register-number reg) +rt-shift+)))
             (return-from instruction t))))))))

(define-instruction adr (reg address)
  (let ((imm-value (- (or (resolve-immediate address) (current-address))
                      (current-address))))
    (check-register-class reg :gpr-64 :xzr)
    (assert (<= -1048576 imm-value 1048575))
    (emit-instruction (logior #x10000000
                              (ash (ldb (byte 19 2) imm-value) 5)
                              (ash (ldb (byte 2 0) imm-value) 29)
                              (ash (register-number reg) +rd-shift+)))
    (return-from instruction t)))

(defun emit-ldstp-instruction (load-bit r1 r2 address)
  (multiple-value-bind (mode base offset)
      (parse-address address)
    (let* ((class (register-class r1))
           (position (ecase class
                      (:gpr-64 3)
                      (:gpr-32 2)))
           (opc (logior (ecase class
                          (:gpr-64 #x80000000)
                          (:gpr-32 #x00000000))
                        (ecase mode
                          (:pre
                           #x01800000)
                          (:post
                           #x00800000)
                          (:base-plus-immediate
                           #x01000000))))
           (imm-value (or (resolve-immediate offset) 0)))
      (assert (eql class (register-class r2)))
      (ecase class
        (:gpr-64
         (assert (not (logtest imm-value #b111)))
         (assert (<= -512 imm-value 504)))
        (:gpr-32
         (assert (not (logtest imm-value #b11)))
         (assert (<= -256 imm-value 252))))
      (emit-instruction (logior #x28000000
                                opc
                                load-bit
                                (ash (ldb (byte 7 position) imm-value) 15)
                                (ash (register-number r1) +rt-shift+)
                                (ash (register-number r2) +rt2-shift+)
                                (ash (register-number base) +rn-shift+)))
      t)))

(define-instruction ldp (r1 r2 address)
  (when (emit-ldstp-instruction #x00400000 r1 r2 address)
    (return-from instruction t)))

(define-instruction stp (r1 r2 address)
  (when (emit-ldstp-instruction #x00000000 r1 r2 address)
    (return-from instruction t)))

(defun emit-addsub-instruction (opcode s-bit dst lhs rhs extend amount)
  (let* ((dst-class (register-class dst))
         (is-64-bit (member dst-class (if s-bit
                                          '(:gpr-64 :xzr)
                                          '(:gpr-64 :sp))))
         (opc (logior (ash opcode 30)
                      (if s-bit
                          #x20000000
                          #x00000000)
                      (if is-64-bit
                          #x80000000
                          #x00000000)))
         (lhs-class (register-class lhs))
         (rhs-class (register-class rhs))
         (sp-involved (or (member dst-class '(:sp :wsp))
                          (member lhs-class '(:sp :wsp)))))
    (if s-bit
        (check-register-class dst :gpr-64 :gpr-32 :xzr :wzr)
        (check-register-class dst :gpr-64 :gpr-32 :sp :wsp))
    (cond ((null rhs-class)
           ;; Add (immediate)
           (if is-64-bit
               (check-register-class lhs :gpr-64 :sp)
               (check-register-class lhs :gpr-32 :wsp))
           (when (null extend)
             (setf extend :lsl))
           (when (null amount)
             (setf amount 0))
           (assert (eql extend :lsl))
           (assert (or (eql amount 0)
                       (eql amount 12)))
           (let ((imm-value (or (resolve-immediate rhs) 0)))
             (assert (<= 0 imm-value 4095))
             (emit-instruction (logior #x11000000
                                       opc
                                       (if (eql amount 12)
                                           #x00400000
                                           #x00000000)
                                       (ash imm-value 10)
                                       (ash (register-number lhs) +rn-shift+)
                                       (ash (register-number dst) +rd-shift+)))
             t))
          ((or sp-involved
               (member extend '(:uxtb :uxth :uxtw :uxtx
                                :sxtb :sxth :sxtw :sxtx)))
           ;; Add (extended register)
           (if is-64-bit
               (check-register-class lhs :gpr-64 :sp)
               (check-register-class lhs :gpr-32 :wsp))
           (when sp-involved
             (assert (member extend '(:uxtb :uxth :uxtw :uxtx
                                      :sxtb :sxth :sxtw :sxtx
                                      :lsl nil))))
           (when (not amount)
             (setf amount 0))
           (assert (<= 0 amount 4))
           (when (member extend '(nil :lsl))
             (setf extend (if is-64-bit
                              :uxtx
                              :uxtw)))
           (cond ((not is-64-bit)
                  (check-register-class rhs :gpr-32 :wzr))
                 ((member extend '(:uxtx :sxtx))
                  (check-register-class rhs :gpr-64 :xzr))
                 (t
                  (check-register-class rhs :gpr-32 :wzr)))
           (emit-instruction (logior #x0b200000
                                     opc
                                     (ash (ecase extend
                                            (:uxtb #b000)
                                            (:uxth #b001)
                                            (:uxtw #b010)
                                            (:uxtx #b011)
                                            (:sxtb #b100)
                                            (:sxth #b101)
                                            (:sxtw #b110)
                                            (:sxtx #b111))
                                          13)
                                     (ash amount 10)
                                     (ash (register-number rhs) +rm-shift+)
                                     (ash (register-number lhs) +rn-shift+)
                                     (ash (register-number dst) +rd-shift+)))
           t)
          ((and (not sp-involved)
                (member extend '(nil :lsl :lsr :asr)))
           ;; Add (shifted register)
           (if is-64-bit
               (check-register-class lhs :gpr-64 :xzr)
               (check-register-class lhs :gpr-32 :wzr))
           (when (null extend)
             (setf extend :lsl
                   amount 0))
           (cond (is-64-bit
                  (assert (<= 0 amount 63))
                  (check-register-class rhs :gpr-64 :xzr))
                 (t
                  (assert (<= 0 amount 31))
                  (check-register-class rhs :gpr-32 :wzr)))
           (emit-instruction (logior #x0b000000
                                     opc
                                     (ash (ecase extend
                                            (:lsl #b00)
                                            (:lsr #b01)
                                            (:asr #b10))
                                          22)
                                     (ash amount 10)
                                     (ash (register-number rhs) +rm-shift+)
                                     (ash (register-number lhs) +rn-shift+)
                                     (ash (register-number dst) +rd-shift+)))
           t))))

(defmacro define-addsub-instruction (name op sflag)
  `(define-instruction ,name (dst lhs rhs &optional extend amount)
     (when (emit-addsub-instruction ',op ',sflag dst lhs rhs extend amount)
       (return-from instruction t))))

(define-addsub-instruction add  0 nil)
(define-addsub-instruction adds 0 t)
(define-addsub-instruction sub  1 nil)
(define-addsub-instruction subs 1 t)

(defun shifted-mask-p (value)
  (let ((v (logior value (1- value))))
    (zerop (logand (1+ v) v))))

(defun count-zeros-from-lsb (value)
  (assert (not (zerop value)))
  (let ((count 0))
    (loop
       (when (logtest value 1)
         (return))
       (setf value (ash value -1))
       (incf count))
    count))

(defun encode-bit-mask (imm reg-size)
  (when (minusp imm)
    (setf imm (ldb (byte reg-size 0) imm)))
  ;; Not all zeros or all ones, and within range.
  (assert (< 0 imm (1- (ash 1 reg-size))))
  ;; Must be a single contiguous run of bits.
  ;; TODO: Support masks that wrap.
  (assert (shifted-mask-p imm))
  (let* ((shift (count-zeros-from-lsb imm))
         (width (count-zeros-from-lsb (lognot (ash imm (- shift))))))
    (logior (ash 1 12)
            (ash (logand (- 64 shift) #x3f) 6)
            (1- width))))

(defun emit-logical-instruction (opcode negate-bit dst lhs rhs shift amount)
  (let* ((dst-class (register-class dst))
         (is-64-bit (member dst-class '(:gpr-64 :sp :xzr)))
         (sf (if is-64-bit
                 #x80000000
                 #x00000000))
         (sp-involved (member dst-class '(:sp :wsp))))
    (check-register-class dst :gpr-64 :gpr-32 :sp :wsp :xzr :wzr)
    (if is-64-bit
        (check-register-class lhs :gpr-64 :xzr)
        (check-register-class :gpr-32 :wzr))
    (cond ((and (not negate-bit)
                (null (register-class rhs))
                (null shift)
                (null amount)
                (not (member dst-class '(:xzr :wzr))))
           ;; Orr (immediate)
           (let* ((imm-value (or (resolve-immediate rhs) 0))
                  (encoded-bitmask (encode-bit-mask imm-value (if is-64-bit 64 32))))
             (emit-instruction (logior #x32000000
                                       (ash opcode 29)
                                       sf
                                       (ash encoded-bitmask 10)
                                       (ash (register-number lhs) +rn-shift+)
                                       (ash (register-number dst) +rd-shift+)))
             t))
          ((and (not sp-involved)
                (member shift '(nil :lsl :lsr :asr :ror)))
           (when (null shift)
             (setf shift :lsl
                   amount 0))
           (cond (is-64-bit
                  (assert (<= 0 amount 63))
                  (check-register-class rhs :gpr-64 :xzr))
                 (t
                  (assert (<= 0 amount 31))
                  (check-register-class rhs :gpr-32 :wzr)))
           (emit-instruction (logior #x0a000000
                                     (ash opcode 29)
                                     sf
                                     (if negate-bit
                                         #x00200000
                                         #x00000000)
                                     (ash (ecase shift
                                            (:lsl #b00)
                                            (:lsr #b01)
                                            (:asr #b10)
                                            (:ror #b11))
                                          22)
                                     (ash amount 10)
                                     (ash (register-number rhs) +rm-shift+)
                                     (ash (register-number lhs) +rn-shift+)
                                     (ash (register-number dst) +rd-shift+)))
           t))))

(defmacro define-logical-instruction (name inverted-name opcode)
  `(progn
     (define-instruction ,name (dst lhs rhs &optional shift amount)
       (when (emit-logical-instruction ',opcode nil dst lhs rhs shift amount)
         (return-from instruction t)))
     (define-instruction ,inverted-name (dst lhs rhs &optional shift amount)
       (when (emit-logical-instruction ',opcode t dst lhs rhs shift amount)
         (return-from instruction t)))))

(define-logical-instruction and bic #b00)
(define-logical-instruction orr orn #b01)
(define-logical-instruction eor eon #b10)
(define-logical-instruction ands bics #b11)

(defun emit-conditional-branch (condition target)
  (let ((imm-value (- (or (resolve-immediate target) (current-address))
                      (current-address))))
    (assert (not (logtest imm-value #b11)))
    (assert (<= -1048576 imm-value 1048575))
    (emit-instruction (logior #x54000000
                              (ash (ldb (byte 26 2) imm-value) 5)
                              condition))
    t))

(defmacro define-conditional-branch (name condition)
  `(define-instruction ,name (target)
     (when (emit-conditional-branch ',condition target)
       (return-from instruction t))))

(define-conditional-branch b.eq #b0000)
(define-conditional-branch b.ne #b0001)
(define-conditional-branch b.cs #b0010)
(define-conditional-branch b.hs #b0010)
(define-conditional-branch b.cc #b0011)
(define-conditional-branch b.lo #b0011)
(define-conditional-branch b.mi #b0100)
(define-conditional-branch b.pl #b0101)
(define-conditional-branch b.vs #b0110)
(define-conditional-branch b.vc #b0111)
(define-conditional-branch b.hi #b1000)
(define-conditional-branch b.ls #b1001)
(define-conditional-branch b.ge #b1010)
(define-conditional-branch b.lt #b1011)
(define-conditional-branch b.gt #b1100)
(define-conditional-branch b.le #b1101)
(define-conditional-branch b.al #b1110)

(define-instruction b (target)
  (let ((imm-value (- (or (resolve-immediate target) (current-address))
                      (current-address))))
    (assert (not (logtest imm-value #b11)))
    (assert (<= -134217728 imm-value 134217727))
    (emit-instruction (logior #x14000000
                              (ldb (byte 19 2) imm-value)))
    (return-from instruction t)))

(define-instruction br (target)
  (check-register-class target :gpr-64)
  (emit-instruction (logior #xd61f0000
                            (ash (register-number target) +rn-shift+)))
  (return-from instruction t))

(define-instruction blr (target)
  (check-register-class target :gpr-64)
  (emit-instruction (logior #xd63f0000
                            (ash (register-number target) +rn-shift+)))
  (return-from instruction t))

(define-instruction ret (&optional (reg :x30))
  (check-register-class reg :gpr-64)
  (emit-instruction (logior #xd65f0000
                            (ash (register-number reg) +rn-shift+)))
  (return-from instruction t))

(define-instruction cbz (reg target)
  (check-register-class reg :gpr-64 :gpr-32)
  (let ((imm-value (- (or (resolve-immediate target) (current-address))
                      (current-address))))
    (assert (not (logtest imm-value #b11)))
    (assert (<= -134217728 imm-value 134217727))
    (emit-instruction (logior #x34000000
                              (if (eql (register-class reg) :gpr-64)
                                  #x80000000
                                  #x00000000)
                              (ash (ldb (byte 19 2) imm-value) 5)
                              (ash (register-number reg) +rt-shift+)))
    (return-from instruction t)))

(define-instruction cbnz (reg target)
  (check-register-class reg :gpr-64 :gpr-32)
  (let ((imm-value (- (or (resolve-immediate target) (current-address))
                      (current-address))))
    (assert (not (logtest imm-value #b11)))
    (assert (<= -134217728 imm-value 134217727))
    (emit-instruction (logior #x35000000
                              (if (eql (register-class reg) :gpr-64)
                                  #x80000000
                                  #x00000000)
                              (ash (ldb (byte 19 2) imm-value) 5)
                              (ash (register-number reg) +rt-shift+)))
    (return-from instruction t)))

(define-instruction tbnz (reg bit target)
  (check-register-class reg :gpr-64 :gpr-32)
  (let ((is-64-bit (eql (register-class reg) :gpr-64))
        (imm-value (- (or (resolve-immediate target) (current-address))
                      (current-address))))
    (assert (not (logtest imm-value #b11)))
    (assert (<= -32767 imm-value 32767))
    (if is-64-bit
        (assert (<= 0 bit 63))
        (assert (<= 0 bit 31)))
    (emit-instruction (logior #x37000000
                              (ash (ldb (byte 1 5) bit) 31)
                              (ash (ldb (byte 5 0) bit) 19)
                              (ash (ldb (byte 14 2) imm-value) 5)
                              (ash (register-number reg) +rt-shift+)))
    (return-from instruction t)))

(defun emit-conditional-select (condition dst true false)
  (check-register-class dst :gpr-64 :gpr-32)
  (let ((is-64-bit (eql (register-class dst) :gpr-64)))
    (cond (is-64-bit
           (check-register-class true :gpr-64 :xzr)
           (check-register-class false :gpr-64 :xzr))
          (t
           (check-register-class true :gpr-32 :wzr)
           (check-register-class false :gpr-32 :wzr)))
    (emit-instruction (logior #x1A800000
                              (if is-64-bit
                                  #x80000000
                                  #x00000000)
                              (ash condition 12)
                              (ash false +rm-shift+)
                              (ash true +rn-shift+)
                              (ash dst +rd-shift+))))
  t)

(defmacro define-conditional-select (name condition)
  `(define-instruction ,name (dst true false)
     (when (emit-conditional-select ',condition dst true false)
       (return-from instruction t))))

(define-conditional-select csel.eq #b0000)
(define-conditional-select csel.ne #b0001)
(define-conditional-select csel.cs #b0010)
(define-conditional-select csel.hs #b0010)
(define-conditional-select csel.cc #b0011)
(define-conditional-select csel.lo #b0011)
(define-conditional-select csel.mi #b0100)
(define-conditional-select csel.pl #b0101)
(define-conditional-select csel.vs #b0110)
(define-conditional-select csel.vc #b0111)
(define-conditional-select csel.hi #b1000)
(define-conditional-select csel.ls #b1001)
(define-conditional-select csel.ge #b1010)
(define-conditional-select csel.lt #b1011)
(define-conditional-select csel.gt #b1100)
(define-conditional-select csel.le #b1101)
(define-conditional-select csel.al #b1110)

(define-instruction brk (imm)
  (let ((imm-value (or (resolve-immediate imm) 0)))
    (assert (<= 0 imm-value 65535))
    (emit-instruction (logior #xd4200000
                              (ash imm-value 5)))
    (return-from instruction t)))

(define-instruction hlt (imm)
  (let ((imm-value (or (resolve-immediate imm) 0)))
    (assert (<= 0 imm-value 65535))
    (emit-instruction (logior #xd4400000
                              (ash imm-value 5)))
    (return-from instruction t)))

(define-instruction movz (dst value &optional (shift 0))
  (check-register-class dst :gpr-64 :gpr-32)
  (let ((is-64-bit (eql (register-class dst) :gpr-64))
        (imm-value (or (resolve-immediate value) 0)))
    (assert (<= 0 imm-value 65535))
    (if is-64-bit
        (assert (member shift '(0 16 32 48)))
        (assert (member shift '(0 16))))
    (emit-instruction (logior (if is-64-bit
                                  #x80000000
                                  #x00000000)
                              #x52800000
                              (ash (truncate shift 16) 21)
                              (ash (ldb (byte 16 0) imm-value) 5)
                              (ash (register-number dst) +rd-shift+)))
    (return-from instruction t)))

;; Returns op0, op1, crn, crm, op2.
(defun decode-msr-name (name)
  (ecase name
    (:nzcv
     (values #b11 #b011 #b0100 #b0010 #b000))))

(define-instruction msr (name reg)
  (check-register-class reg :gpr-64)
  (multiple-value-bind (op0 op1 crn crm op2)
      (decode-msr-name name)
    (check-type op0 (unsigned-byte 2))
    (check-type op1 (unsigned-byte 3))
    (check-type crn (unsigned-byte 4))
    (check-type crm (unsigned-byte 4))
    (check-type op2 (unsigned-byte 3))
    (assert (logtest op0 #b10))
    (emit-instruction (logior #xD5100000
                              (ash (- op0 2) 19)
                              (ash op1 16)
                              (ash crn 12)
                              (ash crm 8)
                              (ash op2 5)
                              (ash (register-number reg) +rt-shift+)))
    (return-from instruction t)))

(define-instruction lslv (dst lhs rhs)
  (let ((is-64-bit (eql (register-class dst) :gpr-64)))
    (cond (is-64-bit
           (check-register-class dst :gpr-64)
           (check-register-class lhs :gpr-64)
           (check-register-class rhs :gpr-64))
          (t
           (check-register-class dst :gpr-32)
           (check-register-class lhs :gpr-32)
           (check-register-class rhs :gpr-32)))
    (emit-instruction (logior (if is-64-bit
                                  #x80000000
                                  #x00000000)
                              #x1AC02000
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))
