;;;; ARM64 assembler.

(in-package :mezzano.lap.arm64)

;; FIXME: This is not entirely correct... This table is modified when this
;; file is loaded, which can happen concurrently with reads. It is on
;; a hot path, so the risk seems worth it for now...
(defparameter *instruction-assemblers* (make-hash-table :synchronized nil :enforce-gc-invariant-keys t))

(defmethod mezzano.lap:perform-assembly-using-target ((target mezzano.compiler:arm64-target) code-list &rest args &key &allow-other-keys)
  (apply 'mezzano.lap:perform-assembly *instruction-assemblers* code-list args))

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

(defmacro define-macro-instruction (name lambda-list &body body)
  (let ((insn (gensym)))
    `(add-instruction ',name `(:macro ,#'(lambda (,insn)
                                           #+mezzano (declare (sys.int::lambda-name (instruction ,name)))
                                           (destructuring-bind ,lambda-list (rest ,insn)
                                             (block ,name
                                               ,@body)))))))

(defun find-arm64-lap-definitions (name)
  (let ((assembler (gethash name *instruction-assemblers*)))
    (when assembler
      (let ((loc (mezzano.debug:function-source-location (symbol-function assembler))))
        (when loc
          (list (list `(define-instruction ,name)
                      loc)))))))
(mezzano.extensions:add-find-definitions-hook 'find-arm64-lap-definitions)

(defun emit-byte (value)
  (check-type value (unsigned-byte 8))
  (mezzano.lap:emit value))

(defun emit-instruction (value)
  (check-type value (unsigned-byte 32))
  (assert (not (logtest mezzano.lap:*current-address* #b11)) ()
          "Instruction stream is misaligned.")
  (mezzano.lap:emit (ldb (byte 8 0) value)
                    (ldb (byte 8 8) value)
                    (ldb (byte 8 16) value)
                    (ldb (byte 8 24) value)))

(defun resolve-immediate (value)
  "Convert an immediate value to an integer."
  (cond ((and (consp value)
              (eql (first value) :object-literal))
         (let ((slot (second value)))
           ;; subtract +tag-object+, skip object header.
           ;; Return an expression, so slot goes through symbol resolution, etc.
           (+ (- sys.int::+tag-object+) 8 (* (mezzano.lap:resolve-immediate slot) 8))))
        (t
         (mezzano.lap:resolve-immediate value))))

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
    ((:b0 :b1 :b2 :b3 :b4 :b5 :b6 :b7 :b8 :b9
      :b10 :b11 :b12 :b13 :b14 :b15 :b16 :b17 :b18 :b19
      :b20 :b21 :b22 :b23 :b24 :b25 :b26 :b27 :b28 :b29
      :b30 :b31)
     :fp-8)
    ((:h0 :h1 :h2 :h3 :h4 :h5 :h6 :h7 :h8 :h9
      :h10 :h11 :h12 :h13 :h14 :h15 :h16 :h17 :h18 :h19
      :h20 :h21 :h22 :h23 :h24 :h25 :h26 :h27 :h28 :h29
      :h30 :h31)
     :fp-16)
    ((:s0 :s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9
      :s10 :s11 :s12 :s13 :s14 :s15 :s16 :s17 :s18 :s19
      :s20 :s21 :s22 :s23 :s24 :s25 :s26 :s27 :s28 :s29
      :s30 :s31)
     :fp-32)
    ((:d0 :d1 :d2 :d3 :d4 :d5 :d6 :d7 :d8 :d9
      :d10 :d11 :d12 :d13 :d14 :d15 :d16 :d17 :d18 :d19
      :d20 :d21 :d22 :d23 :d24 :d25 :d26 :d27 :d28 :d29
      :d30 :d31)
     :fp-64)
    ((:q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7 :q8 :q9
      :q10 :q11 :q12 :q13 :q14 :q15 :q16 :q17 :q18 :q19
      :q20 :q21 :q22 :q23 :q24 :q25 :q26 :q27 :q28 :q29
      :q30 :q31)
     :fp-128)
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
    ((:x0  :w0  :d0  :s0  :q0  :b0  :h0) 0)
    ((:x1  :w1  :d1  :s1  :q1  :b1  :h1) 1)
    ((:x2  :w2  :d2  :s2  :q2  :b2  :h2) 2)
    ((:x3  :w3  :d3  :s3  :q3  :b3  :h3) 3)
    ((:x4  :w4  :d4  :s4  :q4  :b4  :h4) 4)
    ((:x5  :w5  :d5  :s5  :q5  :b5  :h5) 5)
    ((:x6  :w6  :d6  :s6  :q6  :b6  :h6) 6)
    ((:x7  :w7  :d7  :s7  :q7  :b7  :h7) 7)
    ((:x8  :w8  :d8  :s8  :q8  :b8  :h8) 8)
    ((:x9  :w9  :d9  :s9  :q9  :b9  :h9) 9)
    ((:x10 :w10 :d10 :s10 :q10 :b10 :h10) 10)
    ((:x11 :w11 :d11 :s11 :q11 :b11 :h11) 11)
    ((:x12 :w12 :d12 :s12 :q12 :b12 :h12) 12)
    ((:x13 :w13 :d13 :s13 :q13 :b13 :h13) 13)
    ((:x14 :w14 :d14 :s14 :q14 :b14 :h14) 14)
    ((:x15 :w15 :d15 :s15 :q15 :b15 :h15) 15)
    ((:x16 :w16 :d16 :s16 :q16 :b16 :h16) 16)
    ((:x17 :w17 :d17 :s17 :q17 :b17 :h17) 17)
    ((:x18 :w18 :d18 :s18 :q18 :b18 :h18) 18)
    ((:x19 :w19 :d19 :s19 :q19 :b19 :h19) 19)
    ((:x20 :w20 :d20 :s20 :q20 :b20 :h20) 20)
    ((:x21 :w21 :d21 :s21 :q21 :b21 :h21) 21)
    ((:x22 :w22 :d22 :s22 :q22 :b22 :h22) 22)
    ((:x23 :w23 :d23 :s23 :q23 :b23 :h23) 23)
    ((:x24 :w24 :d24 :s24 :q24 :b24 :h24) 24)
    ((:x25 :w25 :d25 :s25 :q25 :b25 :h25) 25)
    ((:x26 :w26 :d26 :s26 :q26 :b26 :h26) 26)
    ((:x27 :w27 :d27 :s27 :q27 :b27 :h27) 27)
    ((:x28 :w28 :d28 :s28 :q28 :b28 :h28) 28)
    ((:x29 :w29 :d29 :s29 :q29 :b29 :h29) 29)
    ((:x30 :w30 :d30 :s30 :q30 :b30 :h30) 30)
    ((:xzr :wzr :d31 :s31 :q31 :b31 :h31 :sp) 31)))

(defun convert-width (reg width)
  (dolist (conv '((:w0  :x0)
                  (:w1  :x1)
                  (:w2  :x2)
                  (:w3  :x3)
                  (:w4  :x4)
                  (:w5  :x5)
                  (:w6  :x6)
                  (:w7  :x7)
                  (:w8  :x8)
                  (:w9  :x9)
                  (:w10 :x10)
                  (:w11 :x11)
                  (:w12 :x12)
                  (:w13 :x13)
                  (:w14 :x14)
                  (:w15 :x15)
                  (:w16 :x16)
                  (:w17 :x17)
                  (:w18 :x18)
                  (:w19 :x19)
                  (:w20 :x20)
                  (:w21 :x21)
                  (:w22 :x22)
                  (:w23 :x23)
                  (:w24 :x24)
                  (:w25 :x25)
                  (:w26 :x26)
                  (:w27 :x27)
                  (:w28 :x28)
                  (:w29 :x29)
                  (:w30 :x30)
                  (:wzr :xzr)
                  (:s0  :d0  :q0  :b0  :h0)
                  (:s1  :d1  :q1  :b1  :h1)
                  (:s2  :d2  :q2  :b2  :h2)
                  (:s3  :d3  :q3  :b3  :h3)
                  (:s4  :d4  :q4  :b4  :h4)
                  (:s5  :d5  :q5  :b5  :h5)
                  (:s6  :d6  :q6  :b6  :h6)
                  (:s7  :d7  :q7  :b7  :h7)
                  (:s8  :d8  :q8  :b8  :h8)
                  (:s9  :d9  :q9  :b9  :h9)
                  (:s10 :d10 :q10 :b10 :h10)
                  (:s11 :d11 :q11 :b11 :h11)
                  (:s12 :d12 :q12 :b12 :h12)
                  (:s13 :d13 :q13 :b13 :h13)
                  (:s14 :d14 :q14 :b14 :h14)
                  (:s15 :d15 :q15 :b15 :h15)
                  (:s16 :d16 :q16 :b16 :h16)
                  (:s17 :d17 :q17 :b17 :h17)
                  (:s18 :d18 :q18 :b18 :h18)
                  (:s19 :d19 :q19 :b19 :h19)
                  (:s20 :d20 :q20 :b20 :h20)
                  (:s21 :d21 :q21 :b21 :h21)
                  (:s22 :d22 :q22 :b22 :h22)
                  (:s23 :d23 :q23 :b23 :h23)
                  (:s24 :d24 :q24 :b24 :h24)
                  (:s25 :d25 :q25 :b25 :h25)
                  (:s26 :d26 :q26 :b26 :h26)
                  (:s27 :d27 :q27 :b27 :h27)
                  (:s28 :d28 :q28 :b28 :h28)
                  (:s29 :d29 :q29 :b29 :h29)
                  (:s30 :d30 :q30 :b30 :h30)
                  (:s31 :d31 :q31 :b31 :h31))
           (error "Unknown register ~S." reg))
    (when (member reg conv)
      (return (ecase width
                (32 (first conv))
                (64 (second conv))
                (128 (third conv))
                (8 (fourth conv))
                (16 (fifth conv)))))))

(defun parse-address (address)
  (assert (consp address))
  (case (first address)
    (:constant
     (values :pc :pc
             `(:constant-address ,(second address))))
    (:function
     (values :pc :pc
             `(:constant-address ,(sys.int::function-reference (second address)))))
    (:symbol-global-cell
     (values :pc :pc
             `(:constant-address ,(mezzano.runtime::symbol-global-value-cell
                                   (second address)))))
    (:object
     (destructuring-bind (base &optional (slot 0))
         (rest address)
       (values :base-plus-immediate
               base
               ;; subtract +tag-object+, skip object header.
               ;; Return an expression, so slot goes through symbol resolution, etc.
               `(+ (- ,sys.int::+tag-object+) 8 (* ,slot 8)))))
    (:object-location
     (destructuring-bind (base &optional (slot 0))
         (rest address)
       (values :base-plus-immediate
               base
               ;; subtract +tag-object+, skip object header.
               ;; Return an expression, so slot goes through symbol resolution, etc.
               `(+ (- ,sys.int::+tag-object+) 8 (mezzano.runtime::location-offset ,slot)))))
    (:car
     (destructuring-bind (base)
         (rest address)
       (values :base-plus-immediate
               base
               (- sys.int::+tag-cons+))))
    (:cdr
     (destructuring-bind (base)
         (rest address)
       (values :base-plus-immediate
               base
               (+ (- sys.int::+tag-cons+) 8))))
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
       (cond ((eql (register-class imm) :gpr-64)
              ;; ld/st multiple/single structure instruction support this
              (values :post-register-offset base imm))
             (t
              (assert (not (register-class imm)))
              (values :post base imm)))))
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

;; These two clobber x7, x6, and x9.
(define-macro-instruction named-call (function-name)
  (list
   ;; Load fref into x7, formerly the fref call register.
   `(ldr :x7 (:function ,function-name))
   ;; Read the function out of the fref.
   `(ldr :x6 (:object :x7 #.sys.int::+fref-function+))
   ;; Read the function entry point and call it.
   `(ldr :x9 (:object :x6 #.sys.int::+function-entry-point+))
   `(blr :x9)))

(define-macro-instruction named-tail-call (function-name)
  (list
   ;; Load fref into x7, formerly the fref call register.
   `(ldr :x7 (:function ,function-name))
   ;; Read the function out of the fref.
   `(ldr :x6 (:object :x7 #.sys.int::+fref-function+))
   ;; Read the function entry point and call it.
   `(ldr :x9 (:object :x6 #.sys.int::+function-entry-point+))
   `(br :x9)))

(define-macro-instruction add-imm (dst s1 s2-imm)
  "Like ADD, but generate SUB if S2-IMM is negative"
  (if (minusp s2-imm)
      (list `(sub ,dst ,s1 ,(- s2-imm)))
      (list `(add ,dst ,s1 ,s2-imm))))

(define-macro-instruction adds-imm (dst s1 s2-imm)
  "Like ADDS, but generate SUBS if S2-IMM is negative"
  (if (minusp s2-imm)
      (list `(subs ,dst ,s1 ,(- s2-imm)))
      (list `(adds ,dst ,s1 ,s2-imm))))

(define-macro-instruction sub-imm (dst s1 s2-imm)
  "Like SUB, but generate ADD if S2-IMM is negative"
  (if (minusp s2-imm)
      (list `(add ,dst ,s1 ,(- s2-imm)))
      (list `(sub ,dst ,s1 ,s2-imm))))

(define-macro-instruction subs-imm (dst s1 s2-imm)
  "Like SUBS, but generate ADDS if S2-IMM is negative"
  (if (minusp s2-imm)
      (list `(adds ,dst ,s1 ,(- s2-imm)))
      (list `(subs ,dst ,s1 ,s2-imm))))

(define-macro-instruction mov (dst src)
  (cond ((register-class src)
         (if (eql dst :sp)
             (list `(add ,dst ,src 0))
             (list `(orr ,dst :xzr ,src))))
        ((encodable-bit-mask-p src 64)
         (list `(orr ,dst :xzr ,src)))
        ((integerp src)
         (cond ((zerop src)
                (list `(orr ,dst :xzr 0)))
               (t
                (loop
                  with did-movz = nil
                  for value = src then (ash value -16)
                  for shift from 0 by 16
                  while (not (zerop value))
                  if (logtest value #xFFFF)
                    collect (cond (did-movz
                                   `(movk ,dst ,(ldb (byte 16 0) value) ,shift))
                                  (t
                                   (setf did-movz t)
                                   `(movz ,dst ,(ldb (byte 16 0) value) ,shift)))))))
        (t
         (list `(movz ,dst ,src)))))

(define-macro-instruction cmp (dst src)
  (list `(subs :xzr ,dst ,src)))

(define-macro-instruction lsl (dst src count)
  (if (register-class count)
      (list `(lslv ,dst ,src ,count))
      (list `(orr ,dst :xzr ,src :lsl ,count))))

(define-macro-instruction asr (dst src count)
  (if (register-class count)
      (list `(asrv ,dst ,src ,count))
      (list `(orr ,dst :xzr ,src :asr ,count))))

(define-macro-instruction lsr (dst src count)
  (if (register-class count)
      (list `(lsrv ,dst ,src ,count))
      (list `(orr ,dst :xzr ,src :lsr ,count))))

(defconstant +ldst-size-64-bit+ #x40000000)
(defconstant +ldst-size-32-bit+ #x00000000)

(defconstant +preindex-bit+ #x00000000)
(defconstant +postindex-bit+ #x00000800)

(defconstant +rd-shift+ 0)
(defconstant +rt-shift+ 0)
(defconstant +rn-shift+ 5)
(defconstant +ra-shift+ 10)
(defconstant +rt2-shift+ 10)
(defconstant +rm-shift+ 16)
(defconstant +rs-shift+ 16)
(defconstant +imms-shift+ 10)
(defconstant +immr-shift+ 16)
(defconstant +n-shift+ 22)

(defun encode-index-option (mode)
  (ecase mode
    (:base-plus-index64
     #x00006000)
    (:base-plus-scaled-index64
     #x00007000)))

(defun emit-ldr/str (reg address size opcode &key pcrel-opcode)
  (multiple-value-bind (mode base offset)
      (parse-address address)
    (ecase mode
      (:base-plus-immediate
       (let ((imm-value (or (resolve-immediate offset) 0)))
         (cond ((and (<= 0 imm-value (* 4095 size))
                     (zerop (logand imm-value (1- size))))
                ;; unsigned offset
                (emit-instruction (logior opcode
                                          #x39000000
                                          (ash (/ imm-value size) 10)
                                          (ash (register-number base) +rn-shift+)
                                          (ash (register-number reg) +rt-shift+)))
                (return-from emit-ldr/str t))
               ((<= -256 imm-value 255)
                ;; unscaled offset
                (emit-instruction (logior opcode
                                          #x38000000
                                          (ash (ldb (byte 9 0) imm-value) 12)
                                          (ash (register-number base) +rn-shift+)
                                          (ash (register-number reg) +rt-shift+)))
                (return-from emit-ldr/str t)))))
      ((:base-plus-index64
        :base-plus-scaled-index64)
       ;; register
       (emit-instruction (logior opcode
                                 #x38200800
                                 (ash (register-number offset) +rm-shift+)
                                 (encode-index-option mode)
                                 (ash (register-number base) +rn-shift+)
                                 (ash (register-number reg) +rt-shift+)))
       (return-from emit-ldr/str t))
      (:pre
       (let ((imm-value (or (resolve-immediate offset) 0)))
         (when (<= -256 imm-value 255)
           ;; pre-index
           (emit-instruction (logior opcode
                                     #x38000C00
                                     (ash (ldb (byte 9 0) imm-value) 12)
                                     (ash (register-number base) +rn-shift+)
                                     (ash (register-number reg) +rt-shift+)))
           (return-from emit-ldr/str t))))
      (:post
       (let ((imm-value (or (resolve-immediate offset) 0)))
         (when (<= -256 imm-value 255)
           ;; post-index
           (emit-instruction (logior opcode
                                     #x38000400
                                     (ash (ldb (byte 9 0) imm-value) 12)
                                     (ash (register-number base) +rn-shift+)
                                     (ash (register-number reg) +rt-shift+)))
           (return-from emit-ldr/str t))))
      (:pc
       (when pcrel-opcode
         (let ((imm-value (resolve-immediate offset)))
           ;; literal/pcrel
           (mezzano.lap:emit-relocation :arm-pcrel
                                        (or imm-value offset)
                                        (logior pcrel-opcode
                                                #x18000000
                                                (ash (register-number reg) +rt-shift+)))
           (emit-instruction 0)
           (return-from emit-ldr/str t)))))))

(define-instruction ldr (reg value)
  (ecase (register-class reg)
    (:gpr-32
     (when (emit-ldr/str reg value 4  #x80400000 :pcrel-opcode #x00000000)
       (return-from instruction t)))
    (:gpr-64
     (when (emit-ldr/str reg value 8  #xC0400000 :pcrel-opcode #x40000000)
       (return-from instruction t)))
    (:fp-32
     (when (emit-ldr/str reg value 4  #x84400000 :pcrel-opcode #x04000000)
       (return-from instruction t)))
    (:fp-64
     (when (emit-ldr/str reg value 8  #xC4400000 :pcrel-opcode #x44000000)
       (return-from instruction t)))
    (:fp-128
     (when (emit-ldr/str reg value 16 #x04C00000 :pcrel-opcode #x84000000)
       (return-from instruction t)))))

;; This is ldr but forces a 32-bit operand size
(define-instruction ldrw (reg value)
  (ecase (register-class reg)
    ((:gpr-32 :gpr-64)
     (when (emit-ldr/str reg value 4  #x80400000 :pcrel-opcode #x00000000)
       (return-from instruction t)))))

(define-instruction ldrb (reg value)
  (ecase (register-class reg)
    ((:gpr-32 :gpr-64)
     (when (emit-ldr/str reg value 1 #x00400000)
       (return-from instruction t)))))

(define-instruction ldrsb (reg value)
  (ecase (register-class reg)
    (:gpr-32
     (when (emit-ldr/str reg value 1 #x00C00000)
       (return-from instruction t)))
    (:gpr-64
     (when (emit-ldr/str reg value 1 #x00800000)
       (return-from instruction t)))))

(define-instruction ldrh (reg value)
  (ecase (register-class reg)
    ((:gpr-32 :gpr-64)
     (when (emit-ldr/str reg value 2 #x40400000)
       (return-from instruction t)))))

(define-instruction ldrsh (reg value)
  (ecase (register-class reg)
    (:gpr-32
     (when (emit-ldr/str reg value 2 #x40C00000)
       (return-from instruction t)))
    (:gpr-64
     (when (emit-ldr/str reg value 2 #x40800000)
       (return-from instruction t)))))

(define-instruction ldrsw (reg value)
  (ecase (register-class reg)
    (:gpr-64
     (when (emit-ldr/str reg value 4 #x80800000 :pcrel-opcode #x800000000)
       (return-from instruction t)))))

(define-instruction str (reg value)
  (ecase (register-class reg)
    ((:gpr-32 :wzr)
     (when (emit-ldr/str reg value 4 #x80000000)
       (return-from instruction t)))
    ((:gpr-64 :xzr)
     (when (emit-ldr/str reg value 8 #xC0000000)
       (return-from instruction t)))
    (:fp-32
     (when (emit-ldr/str reg value 4 #x84000000)
       (return-from instruction t)))
    (:fp-64
     (when (emit-ldr/str reg value 8 #xC4000000)
       (return-from instruction t)))
    (:fp-128
     (when (emit-ldr/str reg value 16 #x04800000)
       (return-from instruction t)))))

;; This is str but forces a 32-bit operand size
(define-instruction strw (reg value)
  (ecase (register-class reg)
    ((:gpr-32 :wzr :gpr-64 :xzr)
     (when (emit-ldr/str reg value 4 #x80000000)
       (return-from instruction t)))))

(define-instruction strb (reg value)
  (ecase (register-class reg)
    ((:gpr-32 :wzr :gpr-64 :xzr)
     (when (emit-ldr/str reg value 1 #x00000000)
       (return-from instruction t)))))

(define-instruction strh (reg value)
  (ecase (register-class reg)
    ((:gpr-32 :wzr :gpr-64 :xzr)
     (when (emit-ldr/str reg value 2 #x40000000)
       (return-from instruction t)))))

(define-instruction adr (reg address)
  (let ((imm-value (resolve-immediate address)))
    (check-register-class reg :gpr-64 :xzr)
    (mezzano.lap:emit-relocation :arm-pcrel-adr
                             (or imm-value address)
                             (logior #x10000000
                                     (ash (register-number reg) +rd-shift+)))
    (emit-instruction 0)
    (return-from instruction t)))

(defun emit-ldstp-instruction (load-bit r1 r2 address)
  (multiple-value-bind (mode base offset)
      (parse-address address)
    (let* ((class (register-class r1))
           (addressing-mode (ecase mode
                              (:pre
                               #x01800000)
                              (:post
                               #x00800000)
                              (:base-plus-immediate
                               #x01000000)))
           (imm-value (or (resolve-immediate offset) 0)))
      (multiple-value-bind (opc position)
          (ecase class
            (:gpr-64 (values #x80000000 3))
            (:gpr-32 (values #x00000000 2))
            (:fp-32  (values #x04000000 2))
            (:fp-64  (values #x44000000 3))
            (:fp-128 (values #x84000000 4)))
        (assert (eql class (register-class r2)))
        (ecase class
          (:fp-128
           (assert (not (logtest imm-value #b1111)))
           (assert (<= -1024 imm-value 1008)))
          ((:gpr-64 :fp-64)
           (assert (not (logtest imm-value #b111)))
           (assert (<= -512 imm-value 504)))
          ((:gpr-32 :fp-32)
           (assert (not (logtest imm-value #b11)))
           (assert (<= -256 imm-value 252))))
        (emit-instruction (logior #x28000000
                                  opc
                                  addressing-mode
                                  load-bit
                                  (ash (ldb (byte 7 position) imm-value) 15)
                                  (ash (register-number r1) +rt-shift+)
                                  (ash (register-number r2) +rt2-shift+)
                                  (ash (register-number base) +rn-shift+))))
      t)))

(define-instruction ldp (r1 r2 address)
  (when (emit-ldstp-instruction #x00400000 r1 r2 address)
    (return-from instruction t)))

(define-instruction stp (r1 r2 address)
  (when (emit-ldstp-instruction #x00000000 r1 r2 address)
    (return-from instruction t)))

(define-instruction ldaxrb (reg address)
  (destructuring-bind (base) address
    (check-register-class base :gpr-64 :sp)
    (check-register-class reg :gpr-64 :xzr :gpr-32 :wzr)
    (emit-instruction
     (logior #x085FFC00
             (ash (register-number reg) +rt-shift+)
             (ash (register-number base) +rn-shift+)))
    (return-from instruction t)))

(define-instruction ldaxrh (reg address)
  (destructuring-bind (base) address
    (check-register-class base :gpr-64 :sp)
    (check-register-class reg :gpr-64 :xzr :gpr-32 :wzr)
    (emit-instruction
     (logior #x485FFC00
             (ash (register-number reg) +rt-shift+)
             (ash (register-number base) +rn-shift+)))
    (return-from instruction t)))

(define-instruction ldaxrw (reg address)
  (destructuring-bind (base) address
    (check-register-class base :gpr-64 :sp)
    (check-register-class reg :gpr-64 :xzr :gpr-32 :wzr)
    (emit-instruction
     (logior #x885FFC00
             (ash (register-number reg) +rt-shift+)
             (ash (register-number base) +rn-shift+)))
    (return-from instruction t)))

(define-instruction ldaxr (reg address)
  (destructuring-bind (base) address
    (check-register-class base :gpr-64 :sp)
    (check-register-class reg :gpr-64 :xzr :gpr-32 :wzr)
    (let ((is-64-bit (member (register-class reg) '(:gpr-64 :xzr))))
      (emit-instruction
       (logior #x885FFC00
               (if is-64-bit
                   #x40000000
                   #x00000000)
               (ash (register-number reg) +rt-shift+)
               (ash (register-number base) +rn-shift+)))
      (return-from instruction t))))

(define-instruction ldaxp (r1 r2 address)
  (destructuring-bind (base) address
    (check-register-class base :gpr-64 :sp)
    (check-register-class r1 :gpr-64 :xzr :gpr-32 :wzr)
    (let ((is-64-bit (member (register-class r1) '(:gpr-64 :xzr))))
      (if is-64-bit
          (check-register-class r2 :gpr-64 :xzr)
          (check-register-class r2 :gpr-32 :wzr))
      (emit-instruction
       (logior #x887F8000
               (if is-64-bit
                   #x40000000
                   #x00000000)
               (ash (register-number r1) +rt-shift+)
               (ash (register-number r2) +rt2-shift+)
               (ash (register-number base) +rn-shift+)))
      (return-from instruction t))))

(define-instruction stlxrb (status reg address)
  (destructuring-bind (base) address
    (check-register-class status :gpr-64 :xzr :gpr-32 :wzr)
    (check-register-class base :gpr-64 :sp)
    (check-register-class reg :gpr-64 :xzr :gpr-32 :wzr)
    (emit-instruction
     (logior #x0800FC00
             (ash (register-number status) +rs-shift+)
             (ash (register-number reg) +rt-shift+)
             (ash (register-number base) +rn-shift+)))
    (return-from instruction t)))

(define-instruction stlxrh (status reg address)
  (destructuring-bind (base) address
    (check-register-class status :gpr-64 :xzr :gpr-32 :wzr)
    (check-register-class base :gpr-64 :sp)
    (check-register-class reg :gpr-64 :xzr :gpr-32 :wzr)
    (emit-instruction
     (logior #x4800FC00
             (ash (register-number status) +rs-shift+)
             (ash (register-number reg) +rt-shift+)
             (ash (register-number base) +rn-shift+)))
    (return-from instruction t)))

(define-instruction stlxrw (status reg address)
  (destructuring-bind (base) address
    (check-register-class status :gpr-64 :xzr :gpr-32 :wzr)
    (check-register-class base :gpr-64 :sp)
    (check-register-class reg :gpr-64 :xzr :gpr-32 :wzr)
    (emit-instruction
     (logior #x8800FC00
             (ash (register-number status) +rs-shift+)
             (ash (register-number reg) +rt-shift+)
             (ash (register-number base) +rn-shift+)))
    (return-from instruction t)))

(define-instruction stlxr (status reg address)
  (destructuring-bind (base) address
    (check-register-class status :gpr-64 :xzr :gpr-32 :wzr)
    (check-register-class base :gpr-64 :sp)
    (check-register-class reg :gpr-64 :xzr :gpr-32 :wzr)
    (let ((is-64-bit (member (register-class reg) '(:gpr-64 :xzr))))
      (emit-instruction
       (logior #x8800FC00
               (if is-64-bit
                   #x40000000
                   #x00000000)
               (ash (register-number status) +rs-shift+)
               (ash (register-number reg) +rt-shift+)
               (ash (register-number base) +rn-shift+)))
      (return-from instruction t))))

(define-instruction stlxp (status r1 r2 address)
  (destructuring-bind (base) address
    (check-register-class status :gpr-64 :xzr :gpr-32 :wzr)
    (check-register-class base :gpr-64 :sp)
    (check-register-class r1 :gpr-64 :xzr :gpr-32 :wzr)
    (let ((is-64-bit (member (register-class r1) '(:gpr-64 :xzr))))
      (if is-64-bit
          (check-register-class r2 :gpr-64 :xzr)
          (check-register-class r2 :gpr-32 :wzr))
      (emit-instruction
       (logior #x88208000
               (if is-64-bit
                   #x40000000
                   #x00000000)
               (ash (register-number status) +rs-shift+)
               (ash (register-number r1) +rt-shift+)
               (ash (register-number r2) +rt2-shift+)
               (ash (register-number base) +rn-shift+)))
      (return-from instruction t))))

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
             (assert (<= -4095 imm-value 4095))
             ;; switch add/sub around for negative values
             (when (minusp imm-value)
               (setf imm-value (- imm-value))
               (setf opc (logxor opc (ash 1 30))))
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

(defmacro define-addsub-carry-instruction (name op sflag)
  `(define-instruction ,name (dst lhs rhs)
     (let* ((dst-class (register-class dst))
            (is-64-bit (member dst-class '(:gpr-64 :xzr)))
            (sf (if is-64-bit
                    #x80000000
                    #x00000000)))
       (cond (is-64-bit
              (check-register-class dst :gpr-64 :xzr)
              (check-register-class lhs :gpr-64 :xzr)
              (check-register-class rhs :gpr-64 :xzr))
             (t
              (check-register-class dst :gpr-32 :wzr)
              (check-register-class lhs :gpr-32 :wzr)
              (check-register-class rhs :gpr-32 :wzr)))
       (emit-instruction (logior sf
                                 ,(ash op 30)
                                 ,(ash sflag 29)
                                 #x1A000000
                                 (ash (register-number rhs) +rm-shift+)
                                 (ash (register-number lhs) +rn-shift+)
                                 (ash (register-number dst) +rd-shift+)))
       (return-from instruction t))))

(define-addsub-carry-instruction adc  0 0)
(define-addsub-carry-instruction adcs 0 1)
(define-addsub-carry-instruction sbc  1 0)
(define-addsub-carry-instruction sbcs 1 1)

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

(defun encodable-bit-mask-p (imm reg-size)
  (ignore-errors (encode-bit-mask imm reg-size)))

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
        (check-register-class lhs :gpr-32 :wzr))
    (cond ((and (not negate-bit)
                (null (register-class rhs))
                (null shift)
                (null amount)
                (or (eql opcode #b11) ; ANDS
                    (not (member dst-class '(:xzr :wzr)))))
           ;; Orr (immediate)
           (let* ((imm-value (or (resolve-immediate rhs) 0))
                  (encoded-bitmask (encode-bit-mask imm-value (if is-64-bit 64 32))))
             (emit-instruction (logior #x12000000
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

(define-instruction orr.v (size dst lhs rhs)
  (let ((q (ecase size
             (:8b 0)
             (:16b 1))))
    (emit-instruction
     (logior #x0EA01C00
             (ash q 30)
             (ash (register-number rhs) +rm-shift+)
             (ash (register-number lhs) +rn-shift+)
             (ash (register-number dst) +rd-shift+)))
    (return-from instruction t)))

(defun emit-conditional-branch (condition target)
  (let ((imm-value (resolve-immediate target)))
    (mezzano.lap:emit-relocation :arm-pcrel
                             (or imm-value target)
                             (logior #x54000000
                                     condition))
    (emit-instruction 0)
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
  (let ((imm-value (resolve-immediate target)))
    (mezzano.lap:emit-relocation :arm-pcrel-b
                             (or imm-value target)
                             #x14000000)
    (emit-instruction 0)
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
  (let ((imm-value (resolve-immediate target)))
    (mezzano.lap:emit-relocation :arm-pcrel
                             (or imm-value target)
                             (logior #x34000000
                                     (if (eql (register-class reg) :gpr-64)
                                         #x80000000
                                         #x00000000)
                                     (ash (register-number reg) +rt-shift+)))
    (emit-instruction 0)
    (return-from instruction t)))

(define-instruction cbnz (reg target)
  (check-register-class reg :gpr-64 :gpr-32)
  (let ((imm-value (resolve-immediate target)))
    (mezzano.lap:emit-relocation :arm-pcrel
                             (or imm-value target)
                             (logior #x35000000
                                     (if (eql (register-class reg) :gpr-64)
                                         #x80000000
                                         #x00000000)
                                     (ash (register-number reg) +rt-shift+)))
    (emit-instruction 0)
    (return-from instruction t)))

(define-instruction tbz (reg bit target)
  (check-register-class reg :gpr-64 :gpr-32)
  (let ((is-64-bit (eql (register-class reg) :gpr-64))
        (imm-value (resolve-immediate target)))
    (if is-64-bit
        (assert (<= 0 bit 63))
        (assert (<= 0 bit 31)))
    (mezzano.lap:emit-relocation :arm-pcrel-imm14
                             (or imm-value target)
                             (logior #x36000000
                                     (ash (ldb (byte 1 5) bit) 31)
                                     (ash (ldb (byte 5 0) bit) 19)
                                     (ash (register-number reg) +rt-shift+)))
    (emit-instruction 0)
    (return-from instruction t)))

(define-instruction tbnz (reg bit target)
  (check-register-class reg :gpr-64 :gpr-32)
  (let ((is-64-bit (eql (register-class reg) :gpr-64))
        (imm-value (resolve-immediate target)))
    (if is-64-bit
        (assert (<= 0 bit 63))
        (assert (<= 0 bit 31)))
    (mezzano.lap:emit-relocation :arm-pcrel-imm14
                             (or imm-value target)
                             (logior #x37000000
                                     (ash (ldb (byte 1 5) bit) 31)
                                     (ash (ldb (byte 5 0) bit) 19)
                                     (ash (register-number reg) +rt-shift+)))
    (emit-instruction 0)
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
                              (ash (register-number false) +rm-shift+)
                              (ash (register-number true) +rn-shift+)
                              (ash (register-number dst) +rd-shift+))))
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

(defmacro define-exception-instruction (name opc ll)
  `(define-instruction ,name (imm)
     (let ((imm-value (or (resolve-immediate imm) 0)))
       (assert (<= 0 imm-value 65535))
       (emit-instruction (logior #xd4000000
                                 (ash ,opc 21)
                                 (ash imm-value 5)
                                 ,ll))
       (return-from instruction t))))

(define-exception-instruction svc #b000 #b01)
(define-exception-instruction hvc #b000 #b10)
(define-exception-instruction smc #b000 #b11)
(define-exception-instruction brk #b001 #b00)
(define-exception-instruction hlt #b010 #b00)

(define-instruction movk (dst value &optional (shift 0))
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
                              #x72800000
                              (ash (truncate shift 16) 21)
                              (ash (ldb (byte 16 0) imm-value) 5)
                              (ash (register-number dst) +rd-shift+)))
    (return-from instruction t)))

(define-instruction movn (dst value &optional (shift 0))
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
                              #x12800000
                              (ash (truncate shift 16) 21)
                              (ash (ldb (byte 16 0) imm-value) 5)
                              (ash (register-number dst) +rd-shift+)))
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

(defmacro emit-system-instruction (op0 op1 crn crm op2 reg)
  `(progn
     (emit-instruction (logior #xD5080000
                               (ash ,op0 19)
                               (ash ,op1 16)
                               (ash ,crn 12)
                               (ash ,crm 8)
                               (ash ,op2 5)
                               (if ,reg
                                   (register-number ,reg)
                                   #b11111)))
     (return-from instruction t)))

(defparameter *system-registers*
  ;; op0, op1, crn, crm, op2.
  '((:nzcv             (#b11 #b011 #b0100 #b0010 #b000))
    (:spsel            (#b11 #b000 #b0100 #b0010 #b000))
    (:current-el       (#b11 #b000 #b0100 #b0010 #b010))
    (:sp-el0           (#b11 #b000 #b0100 #b0001 #b000))
    (:elr-el1          (#b11 #b000 #b0100 #b0000 #b001))
    (:elr-el2          (#b11 #b100 #b0100 #b0000 #b001))
    (:esr-el1          (#b11 #b000 #b0101 #b0010 #b000))
    (:far-el1          (#b11 #b000 #b0110 #b0000 #b000))
    (:spsr-el1         (#b11 #b000 #b0100 #b0000 #b000))
    (:spsr-el2         (#b11 #b100 #b0100 #b0000 #b000))
    (:vbar-el1         (#b11 #b000 #b1100 #b0000 #b000))
    (:ttbr0-el1        (#b11 #b000 #b0010 #b0000 #b000))
    (:ttbr1-el1        (#b11 #b000 #b0010 #b0000 #b001))
    (:tcr-el1          (#b11 #b000 #b0010 #b0000 #b010))
    (:hcr-el2          (#b11 #b100 #b0001 #b0001 #b000))
    (:daif             (#b11 #b011 #b0100 #b0010 #b001))
    (:cntfrq-el0       (#b11 #b011 #b1110 #b0000 #b000))
    (:cntkctl-el1      (#b11 #b000 #b1110 #b0001 #b000))
    (:cntp-ctl-el0     (#b11 #b011 #b1110 #b0010 #b001))
    (:cntp-cval-el0    (#b11 #b011 #b1110 #b0010 #b010))
    (:cntp-tval-el0    (#b11 #b011 #b1110 #b0010 #b000))
    (:cntpct-el0       (#b11 #b011 #b1110 #b0000 #b001))
    (:cntv-ctl-el0     (#b11 #b011 #b1110 #b0011 #b001))
    (:cntv-cval-el0    (#b11 #b011 #b1110 #b0011 #b010))
    (:cntv-tval-el0    (#b11 #b011 #b1110 #b0011 #b000))
    (:cntvct-el0       (#b11 #b011 #b1110 #b0000 #b010))
    (:fpcr             (#b11 #b011 #b0100 #b0100 #b000))
    (:fpsr             (#b11 #b011 #b0100 #b0100 #b001))
    (:midr-el1         (#b11 #b000 #b0000 #b0000 #b000))
    (:mpidr-el1        (#b11 #b000 #b0000 #b0000 #b101))
    (:revidr-el1       (#b11 #b000 #b0000 #b0000 #b110))
    (:id-pfr0-el1      (#b11 #b000 #b0000 #b0001 #b000))
    (:id-pfr1-el1      (#b11 #b000 #b0000 #b0001 #b001))
    (:id-dfr0-el1      (#b11 #b000 #b0000 #b0001 #b010))
    (:id-afr0-el1      (#b11 #b000 #b0000 #b0001 #b011))
    (:id-mmfr0-el1     (#b11 #b000 #b0000 #b0001 #b100))
    (:id-mmfr1-el1     (#b11 #b000 #b0000 #b0001 #b101))
    (:id-mmfr2-el1     (#b11 #b000 #b0000 #b0001 #b110))
    (:id-mmfr3-el1     (#b11 #b000 #b0000 #b0001 #b111))
    (:id-isar0-el1     (#b11 #b000 #b0000 #b0010 #b000))
    (:id-isar1-el1     (#b11 #b000 #b0000 #b0010 #b001))
    (:id-isar2-el1     (#b11 #b000 #b0000 #b0010 #b010))
    (:id-isar3-el1     (#b11 #b000 #b0000 #b0010 #b011))
    (:id-isar4-el1     (#b11 #b000 #b0000 #b0010 #b100))
    (:id-isar5-el1     (#b11 #b000 #b0000 #b0010 #b101))
    (:mvfr0-el1        (#b11 #b000 #b0000 #b0011 #b000))
    (:mvfr1-el1        (#b11 #b000 #b0000 #b0011 #b001))
    (:mvfr2-el1        (#b11 #b000 #b0000 #b0011 #b010))
    (:id-aa64pfr0-el1  (#b11 #b000 #b0000 #b0100 #b000))
    (:id-aa64pfr1-el1  (#b11 #b000 #b0000 #b0100 #b001))
    (:id-aa64dfr0-el1  (#b11 #b000 #b0000 #b0101 #b000))
    (:id-aa64dfr1-el1  (#b11 #b000 #b0000 #b0101 #b001))
    (:id-aa64afr0-el1  (#b11 #b000 #b0000 #b0101 #b100))
    (:id-aa64afr1-el1  (#b11 #b000 #b0000 #b0101 #b101))
    (:id-aa64isar0-el1 (#b11 #b000 #b0000 #b0110 #b000))
    (:id-aa64isar1-el1 (#b11 #b000 #b0000 #b0110 #b001))
    (:id-aa64mmfr0-el1 (#b11 #b000 #b0000 #b0111 #b000))
    (:id-aa64mmfr1-el1 (#b11 #b000 #b0000 #b0111 #b001))
    (:ccsidr-el1       (#b11 #b001 #b0000 #b0000 #b000))
    (:clidr-el1        (#b11 #b001 #b0000 #b0000 #b001))
    (:aidr-el1         (#b11 #b001 #b0000 #b0000 #b111))
    (:csselr-el1       (#b11 #b010 #b0000 #b0000 #b000))
    (:ctr-el0          (#b11 #b011 #b0000 #b0000 #b001))
    (:dczid-el0        (#b11 #b011 #b0000 #b0000 #b111))
    (:sctlr-el1        (#b11 #b000 #b0001 #b0000 #b000))
    (:actlr-el1        (#b11 #b000 #b0001 #b0000 #b001))
    (:cpacr-el1        (#b11 #b000 #b0001 #b0000 #b010))
    (:mdscr-el1        (#b10 #b000 #b0000 #b0010 #b010))
    (:mair-el1         (#b11 #b000 #b1010 #b0010 #b000))
    ))

(defun decode-msr-name (name)
  "Returns op0, op1, crn, crm, op2."
  (let ((def (assoc name *system-registers*)))
    (unless def
      (error "Unknown MSR ~S" name))
    (values-list (second def))))

(define-instruction msr (name reg)
  (cond ((and (member name '(:spsel :daifset :daifclr))
              (null (register-class reg)))
         ;; Immediate.
         (multiple-value-bind (op1 op2)
             (ecase name
               (:spsel (values #b000 #b101))
               (:daifset (values #b011 #b110))
               (:daifclr (values #b011 #b111)))
           (let ((imm-value (or (resolve-immediate reg) 0)))
             (assert (<= 0 imm-value 15))
             (emit-instruction (logior #xD500401F
                                       (ash op1 16)
                                       (ash op2 5)
                                       (ash imm-value 8)))
             (return-from instruction t))))
        (t
         (multiple-value-bind (op0 op1 crn crm op2)
             (decode-msr-name name)
           (check-register-class reg :gpr-64)
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
           (return-from instruction t)))))

(define-instruction mrs (reg name)
  (multiple-value-bind (op0 op1 crn crm op2)
      (decode-msr-name name)
    (check-register-class reg :gpr-64)
    (check-type op0 (unsigned-byte 2))
    (check-type op1 (unsigned-byte 3))
    (check-type crn (unsigned-byte 4))
    (check-type crm (unsigned-byte 4))
    (check-type op2 (unsigned-byte 3))
    (assert (logtest op0 #b10))
    (emit-instruction (logior #xD5300000
                              (ash (- op0 2) 19)
                              (ash op1 16)
                              (ash crn 12)
                              (ash crm 8)
                              (ash op2 5)
                              (ash (register-number reg) +rt-shift+)))
    (return-from instruction t)))

(define-instruction isb ()
  (emit-instruction #xD5033FDF)
  (return-from instruction t))

(defmacro define-data-barrier-instruction (name value)
  `(progn
     (define-instruction ,(intern (format nil "DMB.~A" name)) ()
       (emit-instruction (logior #xD50330BF
                                 (ash ,value 8)))
       (return-from instruction t))
     (define-instruction ,(intern (format nil "DSB.~A" name)) ()
       (emit-instruction (logior #xD503309F
                                 (ash ,value 8)))
       (return-from instruction t))))

(define-data-barrier-instruction :sy    #b1111)
(define-data-barrier-instruction :st    #b1110)
(define-data-barrier-instruction :ld    #b1101)
(define-data-barrier-instruction :ish   #b1011)
(define-data-barrier-instruction :ishst #b1010)
(define-data-barrier-instruction :ishld #b1001)
(define-data-barrier-instruction :nsh   #b0111)
(define-data-barrier-instruction :nshst #b0110)
(define-data-barrier-instruction :nshld #b0101)
(define-data-barrier-instruction :osh   #b0011)
(define-data-barrier-instruction :oshst #b0010)
(define-data-barrier-instruction :oshld #b0001)

(defmacro define-cache-maintainance-instruction (name op1 crn crm op2 argument)
  `(define-instruction ,name (,@(if argument (list argument)))
     ,@(when argument
         (list `(check-register-class ,argument :gpr-64)))
     (emit-system-instruction 1 ,op1 ,crn ,crm ,op2 ,argument)))

(define-cache-maintainance-instruction ic.ialluis 0 7  1 0 nil)
(define-cache-maintainance-instruction ic.iallu   0 7  5 0 nil)
(define-cache-maintainance-instruction ic.ivau    3 7  5 1 address)
(define-cache-maintainance-instruction dc.ivac    0 7  6 1 address)
(define-cache-maintainance-instruction dc.isw     0 7  6 2 address)
(define-cache-maintainance-instruction dc.csw     0 7 10 2 address)
(define-cache-maintainance-instruction dc.cisw    0 7 14 2 address)
(define-cache-maintainance-instruction dc.cvac    3 7 10 1 address)
(define-cache-maintainance-instruction dc.cvau    3 7 11 1 address)
(define-cache-maintainance-instruction dc.civac   3 7 14 1 address)
(define-cache-maintainance-instruction dc.zva     3 7  4 1 address)

(define-instruction eret ()
  (emit-instruction #xD69F03E0)
  (return-from instruction t))

(defmacro define-tlbi-instruction (name op1 crn crm op2 has-address)
  `(define-instruction ,name (,@(when has-address '(address)))
     ,@(when has-address
         (list `(check-register-class address :gpr-64)))
     (emit-system-instruction 1 ,op1 ,crn ,crm ,op2 ,(if has-address 'address nil))))

;; Invalidate all entries.
(define-tlbi-instruction tlbi.vmalle1   0 8 7 0 nil)
;; Invalidate by virtual address and ASID.
(define-tlbi-instruction tlbi.vae1      0 8 7 1 t)
;; Invalidate by ASID.
(define-tlbi-instruction tlbi.aside1    0 8 7 2 t)
;; Invalidate by virtual address across all ASIDs.
(define-tlbi-instruction tlbi.vaae1     0 8 7 3 t)
;; Invalidate by virtual address and ASID, last level.
(define-tlbi-instruction tlbi.vale1     0 8 7 5 t)
;; Invalidate by virtual address across all ASIDs, last level.
(define-tlbi-instruction tlbi.vaale1    0 8 7 7 t)
;; Same as above, but executed on all PEs in the same IS domain.
(define-tlbi-instruction tlbi.vmalle1is 0 8 3 0 nil)
(define-tlbi-instruction tlbi.vae1is    0 8 3 1 t)
(define-tlbi-instruction tlbi.aside1is  0 8 3 2 t)
(define-tlbi-instruction tlbi.vaae1is   0 8 3 3 t)
(define-tlbi-instruction tlbi.vale1is   0 8 3 5 t)
(define-tlbi-instruction tlbi.vaale1is  0 8 3 7 t)

(defmacro define-hint-instruction (name op)
  `(define-instruction ,name ()
     (emit-instruction (logior #xD503201F
                               (ash ',op 5)))
     (return-from instruction t)))
(define-hint-instruction nop 0)
(define-hint-instruction yield 1)
(define-hint-instruction wfe 2)
(define-hint-instruction wfi 3)
(define-hint-instruction sev 4)
(define-hint-instruction sevl 5)

(defun emit-shift-variable (op2 dst lhs rhs)
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
                              (ash op2 10)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    t))

(defmacro define-shift-variable (name opcode)
  `(define-instruction ,name (dst lhs rhs)
     (when (emit-shift-variable ',opcode dst lhs rhs)
       (return-from instruction t))))

(define-shift-variable lslv #b00)
(define-shift-variable lsrv #b01)
(define-shift-variable asrv #b10)
(define-shift-variable rorv #b11)

(defun emit-divide (op2 dst lhs rhs)
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
                              #x1AC00800
                              (ash op2 10)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    t))

(defmacro define-divide (name opcode)
  `(define-instruction ,name (dst lhs rhs)
     (when (emit-divide ',opcode dst lhs rhs)
       (return-from instruction t))))

(define-divide udiv 0)
(define-divide sdiv 1)

(define-instruction msub (dst lhs mhs rhs)
  (let ((is-64-bit (eql (register-class dst) :gpr-64)))
    (cond (is-64-bit
           (check-register-class dst :gpr-64)
           (check-register-class lhs :gpr-64 :xzr)
           (check-register-class mhs :gpr-64 :xzr)
           (check-register-class rhs :gpr-64 :xzr))
          (t
           (check-register-class dst :gpr-32)
           (check-register-class lhs :gpr-32 :wzr)
           (check-register-class mhs :gpr-32 :wzr)
           (check-register-class rhs :gpr-32 :wzr)))
    (emit-instruction (logior (if is-64-bit
                                  #x80000000
                                  #x00000000)
                              #x1B008000
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +ra-shift+)
                              (ash (register-number mhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction madd (dst lhs mhs rhs)
  (let ((is-64-bit (eql (register-class dst) :gpr-64)))
    (cond (is-64-bit
           (check-register-class dst :gpr-64)
           (check-register-class lhs :gpr-64 :xzr)
           (check-register-class mhs :gpr-64 :xzr)
           (check-register-class rhs :gpr-64 :xzr))
          (t
           (check-register-class dst :gpr-32)
           (check-register-class lhs :gpr-32 :wzr)
           (check-register-class mhs :gpr-32 :wzr)
           (check-register-class rhs :gpr-32 :wzr)))
    (emit-instruction (logior (if is-64-bit
                                  #x80000000
                                  #x00000000)
                              #x1B000000
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +ra-shift+)
                              (ash (register-number mhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction smaddl (dst lhs mhs rhs)
  (check-register-class dst :gpr-64)
  (check-register-class lhs :gpr-64 :xzr)
  (check-register-class mhs :gpr-32 :xzr)
  (check-register-class rhs :gpr-32 :xzr)
  (emit-instruction (logior #x9B200000
                            (ash (register-number dst) +rd-shift+)
                            (ash (register-number lhs) +ra-shift+)
                            (ash (register-number mhs) +rn-shift+)
                            (ash (register-number rhs) +rm-shift+)))
  (return-from instruction t))

(define-instruction smulh (dst lhs rhs)
  (check-register-class dst :gpr-64)
  (check-register-class lhs :gpr-64 :xzr)
  (check-register-class rhs :gpr-64 :xzr)
  (emit-instruction (logior #x9B400000
                            (ash (register-number dst) +rd-shift+)
                            (ash #b11111 +ra-shift+)
                            (ash (register-number lhs) +rn-shift+)
                            (ash (register-number rhs) +rm-shift+)))
  (return-from instruction t))

(define-instruction umulh (dst lhs rhs)
  (check-register-class dst :gpr-64)
  (check-register-class lhs :gpr-64 :xzr)
  (check-register-class rhs :gpr-64 :xzr)
  (emit-instruction (logior #x9BC00000
                            (ash (register-number dst) +rd-shift+)
                            (ash #b11111 +ra-shift+)
                            (ash (register-number lhs) +rn-shift+)
                            (ash (register-number rhs) +rm-shift+)))
  (return-from instruction t))

(defmacro define-bitfield-instruction (name opcode)
  `(define-instruction ,name (lhs src immr imms)
    (let ((is-64-bit (eql (register-class lhs) :gpr-64)))
      (cond (is-64-bit
             (check-register-class lhs :gpr-64)
             (check-register-class src :gpr-64 :xzr)
             (assert (<= 0 immr 63))
             (assert (<= 0 imms 63)))
            (t
             (check-register-class lhs :gpr-32)
             (check-register-class src :gpr-32 :wzr)
             (assert (<= 0 immr 31))
             (assert (<= 0 imms 31))))
      (emit-instruction (logior (if is-64-bit
                                    #x80000000
                                    #x00000000)
                                #x13000000
                                ,(ash opcode 29)
                                (if is-64-bit (ash 1 +n-shift+) 0)
                                (ash imms +imms-shift+)
                                (ash immr +immr-shift+)
                                (ash (register-number src) +rn-shift+)
                                (ash (register-number lhs) +rd-shift+)))
      (return-from instruction t))))

(define-bitfield-instruction sbfm 0)
(define-bitfield-instruction bfm 1)
(define-bitfield-instruction ubfm 2)

(define-macro-instruction bfc (lhs lsb width)
  (let* ((is-64-bit (eql (register-class lhs) :gpr-64))
         (lim (if is-64-bit 64 32)))
    (assert (<= lsb (1- lim)))
    (assert (<= 1 width (- lim lsb)))
    (list `(bfm ,lhs ,(if is-64-bit :xzr :wzr) ,(mod (- lsb) lim) ,(1- width)))))

(define-macro-instruction bfi (lhs src lsb width)
  (let ((lim (if (eql (register-class lhs) :gpr-64)
                 64
                 32)))
    (assert (<= lsb (1- lim)))
    (assert (<= 1 width (- lim lsb)))
    (list `(bfm ,lhs ,src ,(- lim lsb) ,(1- width)))))

(define-instruction ubfx (lhs src lsb width)
  (let ((is-64-bit (eql (register-class lhs) :gpr-64)))
    (cond (is-64-bit
           (check-register-class lhs :gpr-64)
           (check-register-class src :gpr-64 :xzr)
           (assert (<= 0 lsb 63))
           (assert (<= 1 width (- 64 lsb))))
          (t
           (check-register-class lhs :gpr-32)
           (check-register-class src :gpr-32 :wzr)
           (assert (<= 0 lsb 31))
           (assert (<= 1 width (- 32 lsb)))))
    (emit-instruction (logior (if is-64-bit
                                  #x80000000
                                  #x00000000)
                              #x53000000
                              (if is-64-bit (ash 1 +n-shift+) 0)
                              (ash (+ lsb (1- width)) +imms-shift+)
                              (ash lsb +immr-shift+)
                              (ash (register-number src) +rn-shift+)
                              (ash (register-number lhs) +rd-shift+)))
    (return-from instruction t)))

(define-instruction extr (dst lhs rhs lsb)
  (let ((is-64-bit (eql (register-class lhs) :gpr-64)))
    (cond (is-64-bit
           (check-register-class dst :gpr-64)
           (check-register-class lhs :gpr-64 :xzr)
           (check-register-class rhs :gpr-64 :xzr)
           (assert (<= 0 lsb 63)))
          (t
           (check-register-class dst :gpr-32)
           (check-register-class lhs :gpr-32 :wzr)
           (check-register-class rhs :gpr-32 :wzr)
           (assert (<= 0 lsb 31))))
    (emit-instruction (logior (if is-64-bit
                                  #x80000000
                                  #x00000000)
                              #x13800000
                              (if is-64-bit (ash 1 +n-shift+) 0)
                              (ash lsb +imms-shift+)
                              (ash (register-number rhs) +rm-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number dst) +rd-shift+)))
    (return-from instruction t)))

(define-instruction scvtf (lhs rhs)
  (check-register-class lhs :fp-64 :fp-32)
  (check-register-class rhs :gpr-64 :gpr-32)
  (emit-instruction (logior (if (eql (register-class rhs) :gpr-64)
                                #x80000000
                                #x00000000)
                            #x1E220000
                            (if (eql (register-class lhs) :fp-64)
                                #x00400000
                                #x00000000)
                            (ash (register-number rhs) +rn-shift+)
                            (ash (register-number lhs) +rd-shift+)))
  (return-from instruction t))

(define-instruction fmov (lhs rhs)
  (let ((lhs-class (register-class lhs))
        (rhs-class (register-class rhs)))
    (cond
      ((and (member lhs-class '(:fp-32 :fp-64))
            (eql lhs-class rhs-class))
       (let ((type (ecase lhs-class
                     (:fp-32 0)
                     (:fp-64 1))))
         (emit-instruction
          (logior (ash type 22)
                  #x1E204000
                  (ash (register-number rhs) +rn-shift+)
                  (ash (register-number lhs) +rd-shift+)))
         (return-from instruction t)))
      (t
       (multiple-value-bind (sf type rmode opcode)
           (cond ((and (eql lhs-class :fp-32)
                       (eql rhs-class :gpr-32))
                  (values #b0 #b00 #b00 #b111))
                 ((and (eql lhs-class :gpr-32)
                       (eql rhs-class :fp-32))
                  (values #b0 #b00 #b00 #b110))
                 ((and (eql lhs-class :fp-64)
                       (eql rhs-class :gpr-64))
                  (values #b1 #b01 #b00 #b111))
                 ((and (eql lhs-class :gpr-64)
                       (eql rhs-class :fp-64))
                  (values #b1 #b01 #b00 #b110))
                 (t
                  (error "Unsupported FMOV operand combination ~S and ~S."
                         lhs rhs)))
         (emit-instruction (logior (ash sf 31)
                                   #x1E260000
                                   (ash type 22)
                                   (ash rmode 19)
                                   (ash opcode 16)
                                   (ash (register-number rhs) +rn-shift+)
                                   (ash (register-number lhs) +rd-shift+)))
         (return-from instruction t))))))

(define-instruction fcmp (lhs rhs)
  (let ((lhs-class (register-class lhs))
        (rhs-class (register-class rhs)))
    (multiple-value-bind (type rm opc)
        (cond ((and (eql lhs-class :fp-32)
                    (eql rhs-class :fp-32))
               (values #b00 (register-number rhs) #b00))
              ((and (eql lhs-class :fp-32)
                    (eql rhs 0))
               (values #b00 #b00000 #b01))
              ((and (eql lhs-class :fp-64)
                    (eql rhs-class :fp-64))
               (values #b01 (register-number rhs) #b00))
              ((and (eql lhs-class :fp-64)
                    (eql rhs 0))
               (values #b01 #b00000 #b01))
              (t
               (error "Unsupported FCMP operand combination ~S and ~S."
                      lhs rhs)))
      (emit-instruction (logior #x1E202000
                                (ash type 22)
                                (ash rm +rm-shift+)
                                (ash opc 3)
                                (ash (register-number lhs) +rn-shift+)))
      (return-from instruction t))))

(define-instruction fcvtns (lhs rhs)
  (let ((lhs-class (register-class lhs))
        (rhs-class (register-class rhs)))
    (multiple-value-bind (sf type rmode opcode)
        (cond ((and (eql lhs-class :gpr-32)
                    (eql rhs-class :fp-32))
               (values #b0 #b00 #b00 #b000))
              ((and (eql lhs-class :gpr-64)
                    (eql rhs-class :fp-32))
               (values #b1 #b00 #b00 #b000))
              ((and (eql lhs-class :gpr-32)
                    (eql rhs-class :fp-64))
               (values #b0 #b01 #b00 #b000))
              ((and (eql lhs-class :gpr-64)
                    (eql rhs-class :fp-64))
               (values #b1 #b01 #b00 #b000))
              (t
               (error "Unsupported FCVTNS operand combination ~S and ~S."
                      lhs rhs)))
      (emit-instruction (logior (ash sf 31)
                                #x1E200000
                                (ash type 22)
                                (ash rmode 19)
                                (ash opcode 16)
                                (ash (register-number rhs) +rn-shift+)
                                (ash (register-number lhs) +rd-shift+)))
      (return-from instruction t))))

(define-instruction fcvtzs (lhs rhs)
  (let ((lhs-class (register-class lhs))
        (rhs-class (register-class rhs)))
    (multiple-value-bind (sf type rmode opcode)
        (cond ((and (eql lhs-class :gpr-32)
                    (eql rhs-class :fp-32))
               (values #b0 #b00 #b11 #b000))
              ((and (eql lhs-class :gpr-64)
                    (eql rhs-class :fp-32))
               (values #b1 #b00 #b11 #b000))
              ((and (eql lhs-class :gpr-32)
                    (eql rhs-class :fp-64))
               (values #b0 #b01 #b11 #b000))
              ((and (eql lhs-class :gpr-64)
                    (eql rhs-class :fp-64))
               (values #b1 #b01 #b11 #b000))
              (t
               (error "Unsupported FCVTZS operand combination ~S and ~S."
                      lhs rhs)))
      (emit-instruction (logior (ash sf 31)
                                #x1E380000
                                (ash type 22)
                                (ash rmode 19)
                                (ash opcode 16)
                                (ash (register-number rhs) +rn-shift+)
                                (ash (register-number lhs) +rd-shift+)))
      (return-from instruction t))))

(defmacro define-3op-float (name opcode)
  `(define-instruction ,name (dst lhs rhs)
     (let ((is-64-bit (eql (register-class dst) :fp-64)))
       (cond (is-64-bit
              (check-register-class dst :fp-64)
              (check-register-class lhs :fp-64)
              (check-register-class rhs :fp-64))
             (t
              (check-register-class dst :fp-32)
              (check-register-class lhs :fp-32)
              (check-register-class rhs :fp-32)))
       (emit-instruction (logior (if is-64-bit
                                     #x00400000
                                     #x00000000)
                                 ',opcode
                                 (ash (register-number dst) +rd-shift+)
                                 (ash (register-number lhs) +rn-shift+)
                                 (ash (register-number rhs) +rm-shift+)))
       (return-from instruction t))))

(define-3op-float fadd #x1E202800)
(define-3op-float fsub #x1E203800)
(define-3op-float fmul #x1E200800)
(define-3op-float fdiv #x1E201800)

(define-instruction fcvt (lhs rhs)
  (let ((lhs-class (register-class lhs))
        (rhs-class (register-class rhs)))
    (multiple-value-bind (type opc)
        (cond
          ((and (eql lhs-class :fp-64)
                (eql rhs-class :fp-32))
           (values #b00 #b01))
          ((and (eql lhs-class :fp-32)
                (eql rhs-class :fp-64))
           (values #b01 #b00))
          (t
           (error "Unsupported FCVT operand combination ~S and ~S."
                  lhs rhs)))
      (emit-instruction (logior #x1E224000
                                (ash type 22)
                                (ash opc 15)
                                (ash (register-number rhs) +rn-shift+)
                                (ash (register-number lhs) +rd-shift+)))
      (return-from instruction t))))

(define-instruction fsqrt (lhs rhs)
  (let* ((lhs-class (register-class lhs))
         (rhs-class (register-class rhs))
         (type (ecase lhs-class
                 (:fp-32 #b00)
                 (:fp-64 #b01))))
    (assert (eql lhs-class rhs-class))
    (emit-instruction (logior #x1E21C000
                              (ash type 22)
                              (ash (register-number rhs) +rn-shift+)
                              (ash (register-number lhs) +rd-shift+)))
    (return-from instruction t)))

(defmacro define-cas-instruction (name l o0 size class single-bit)
  `(define-instruction ,name (old new address)
     (destructuring-bind (base) address
       (check-register-class base :gpr-64 :sp)
       (check-register-class old ,class)
       (assert (eql (register-class old) (register-class new)))
       (emit-instruction (logior #x08207C00
                                 ,(ash single-bit 23)
                                 ,(ash size 30)
                                 ,(ash l 22)
                                 ,(ash o0 15)
                                 (ash (register-number base) +rn-shift+)
                                 (ash (register-number new) +rt-shift+)
                                 (ash (register-number old) +rs-shift+)))
       (return-from instruction t))))

;; Always full 64-bit registers
(define-cas-instruction cas 0 0 #b11 :gpr-64 1)
(define-cas-instruction casa 1 0 #b11 :gpr-64 1)
(define-cas-instruction casal 1 1 #b11 :gpr-64 1)
(define-cas-instruction casl 0 1 #b11 :gpr-64 1)

;; Always 32-bit registers.
(define-cas-instruction casw 0 0 #b10 :gpr-32 1)
(define-cas-instruction casaw 1 0 #b10 :gpr-32 1)
(define-cas-instruction casalw 1 1 #b10 :gpr-32 1)
(define-cas-instruction caslw 0 1 #b10 :gpr-32 1)

(define-cas-instruction casb 0 0 #b00 :gpr-32 1)
(define-cas-instruction casab 1 0 #b00 :gpr-32 1)
(define-cas-instruction casalb 1 1 #b00 :gpr-32 1)
(define-cas-instruction caslb 0 1 #b00 :gpr-32 1)

(define-cas-instruction cash 0 0 #b01 :gpr-32 1)
(define-cas-instruction casah 1 0 #b01 :gpr-32 1)
(define-cas-instruction casalh 1 1 #b01 :gpr-32 1)
(define-cas-instruction caslh 0 1 #b01 :gpr-32 1)

(define-cas-instruction casp 0 0 #b01 :gpr-64 0)
(define-cas-instruction caspa 1 0 #b01 :gpr-64 0)
(define-cas-instruction caspal 1 1 #b01 :gpr-64 0)
(define-cas-instruction caspl 0 1 #b01 :gpr-64 0)

(defmacro define-atomic-op (name a r size opc class &key allow-zr)
  `(define-instruction ,name (xs xt address)
     (destructuring-bind (base) address
       (check-register-class base :gpr-64 :sp)
       (check-register-class xs ,class)
       ,(if allow-zr
            `(assert (or (eql (register-class xs) (register-class xt))
                         (eql (register-class xt) ,(ecase class (:gpr-64 :xzr) (:gpr-32 :wzr)))))
            `(assert (eql (register-class xs) (register-class xt))))
       (emit-instruction (logior #x38200000
                                 ,(ash size 30)
                                 ,(ash a 23)
                                 ,(ash r 22)
                                 ,(ash opc 12)
                                 (ash (register-number base) +rn-shift+)
                                 (ash (register-number xt) +rt-shift+)
                                 (ash (register-number xs) +rs-shift+)))
       (return-from instruction t))))

(defmacro define-atomic-ops (name opc &key allow-zr)
  (list*
   'progn
   (loop
     for (size-name size class) in '(("" #b11 :gpr-64) ("W" #b10 :gpr-32)
                                     ("H" #b01 :gpr-32) ("B" #b00 :gpr-32))
     append (loop for (order-name a r) in '(("" 0 0) ("A" 1 0) ("L" 0 1) ("AL" 1 1))
                  for final-name = (intern (format nil "~A~A~A" name order-name size-name))
                  collect `(define-atomic-op ,final-name ,a ,r ,size ,opc ,class :allow-zr ,allow-zr)))))

(define-atomic-ops swp #b1000)
(define-atomic-ops ldadd #b000 :allow-zr t)
(define-atomic-ops ldset #b011 :allow-zr t)
(define-atomic-ops ldeor #b010 :allow-zr t)

(define-instruction movi.v (type dst imm &optional (shift :lsl) (amount 0))
  (check-register-class dst :fp-128)
  (check-type imm (unsigned-byte 8))
  (multiple-value-bind (q op cmode)
      (cond ((and (member type '(:8b :16b))
                  (eql shift :lsl)
                  (eql amount 0))
             ;; 8-bit variant.
             (values (if (eql type :16b) 1 0)
                     0
                     #b1110))
            ((and (member type '(:4h :8h))
                  (eql shift :lsl))
             ;; 16-bit variant.
             (values (if (eql type :8h) 1 0)
                     0
                     (ecase amount
                       (0 #b1000)
                       (8 #b1010))))
            ((and (member type '(:2s :4s))
                  (eql shift :lsl))
             ;; 32-bit variant.
             (values (if (eql type :4s) 1 0)
                     0
                     (ecase amount
                       (0  #b0000)
                       (8  #b0010)
                       (16 #b0100)
                       (32 #b0110))))
            ((and (member type '(:2s :4s))
                  (eql shift :msl))
             ;; 32-bit shifting ones variant, the shifted-in bits are '1' rather than '0'
             (values (if (eql type :4s) 1 0)
                     0
                     (ecase amount
                       (8  #b1100)
                       (16 #b1101))))
            ((and (member type (:1d :2d))
                  (eql shift :lsl)
                  (eql amount 0))
             (values (if (eql type :2d) 1 0)
                     1
                     #b1110)))
    (when cmode
      (emit-instruction (logior #x0F000400
                                (ash q 30)
                                (ash op 29)
                                (ash cmode 12)
                                (ash (ldb (byte 3 5) imm) 16)
                                (ash (ldb (byte 5 0) imm) 5)
                                (ash (register-number dst) +rd-shift+)))
      (return-from instruction t))))

(define-instruction mvni.v (type dst imm &optional (shift :lsl) (amount 0))
  (check-register-class dst :fp-128)
  (check-type imm (unsigned-byte 8))
  (multiple-value-bind (q op cmode)
      (cond ((and (member type '(:4h :8h))
                  (eql shift :lsl))
             ;; 16-bit variant.
             (values (if (eql type :8h) 1 0)
                     1
                     (ecase amount
                       (0 #b1000)
                       (8 #b1010))))
            ((and (member type '(:2s :4s))
                  (eql shift :lsl))
             ;; 32-bit variant.
             (values (if (eql type :4s) 1 0)
                     1
                     (ecase amount
                       (0  #b0000)
                       (8  #b0010)
                       (16 #b0100)
                       (32 #b0110))))
            ((and (member type '(:2s :4s))
                  (eql shift :msl))
             ;; 32-bit shifting ones variant, the shifted-in bits are '1' rather than '0'
             (values (if (eql type :4s) 1 0)
                     1
                     (ecase amount
                       (8  #b1100)
                       (16 #b1101)))))
    (when cmode
      (emit-instruction (logior #x0F000400
                                (ash q 30)
                                (ash op 29)
                                (ash cmode 12)
                                (ash (ldb (byte 3 5) imm) 16)
                                (ash (ldb (byte 5 0) imm) 5)
                                (ash (register-number dst) +rd-shift+)))
      (return-from instruction t))))

(define-instruction dup.v (type dst src &optional index)
  (cond ((member (register-class src) '(:gpr-64 :xzr :gpr-32 :wzr))
         ;; dup (general)
         (assert (not index))
         (multiple-value-bind (imm5 q)
             (ecase type
               (:8b  (values #b00001 0))
               (:16b (values #b00001 1))
               (:4h  (values #b00010 0))
               (:8h  (values #b00010 1))
               (:2s  (values #b00100 0))
               (:4s  (values #b00100 1))
               (:2d  (values #b01000 1)))
           (check-register-class dst :fp-128)
           (if (eql imm5 #b01000)
               (check-register-class src :gpr-64 :xzr)
               (check-register-class src :gpr-32 :wzr))
           (emit-instruction (logior #x0E000C00
                                     (ash q 30)
                                     (ash imm5 16)
                                     (ash (register-number dst) +rd-shift+)
                                     (ash (register-number src) +rn-shift+)))
           (return-from instruction t)))
        ((eql (register-class dst) ':fp-128)
         ;; dup (vector)
         (check-register-class src :fp-128)
         (multiple-value-bind (imm5 q shift max-index)
             (ecase type
               (:8b  (values #b00001 0 1 8))
               (:16b (values #b00001 1 1 16))
               (:4h  (values #b00010 0 2 4))
               (:8h  (values #b00010 1 2 8))
               (:2s  (values #b00100 0 3 2))
               (:4s  (values #b00100 1 3 4))
               (:2d  (values #b01000 1 4 2)))
           (assert (and (<= 0 index) (< index max-index)))
           (let ((imm5 (logior imm5 (ash index shift))))
             (emit-instruction (logior #x0E000400
                                       (ash q 30)
                                       (ash imm5 16)
                                       (ash (register-number dst) +rd-shift+)
                                       (ash (register-number src) +rn-shift+)))
             (return-from instruction t))))
        (t
         ;; dup (scalar)
         ;; I feel like this is perversely named, it's effectively an extract
         ;; operation, not a broadcast. Oh well.
         (check-register-class src :fp-128)
         (multiple-value-bind (imm5 shift max-index class)
             (ecase type
               (:b (values #b00001 1 16 :fp-8))
               (:h (values #b00010 2 8 :fp-16))
               (:s (values #b00100 3 4 :fp-32))
               (:d (values #b01000 4 2 :fp-64)))
           (check-register-class dst class)
           (assert (and (<= 0 index) (< index max-index)))
           (let ((imm5 (logior imm5 (ash index shift))))
             (emit-instruction (logior #x5E000400
                                       (ash imm5 16)
                                       (ash (register-number dst) +rd-shift+)
                                       (ash (register-number src) +rn-shift+)))
             (return-from instruction t))))))

(define-instruction umov.v (type dst src lane)
  (check-register-class src :fp-128)
  (multiple-value-bind (imm5 q shift class)
      (ecase type
        (:b (values #b00001 0 1 :gpr-32))
        (:h (values #b00010 0 2 :gpr-32))
        (:s (values #b00100 0 3 :gpr-32))
        (:d (values #b01000 1 4 :gpr-64)))
    (check-register-class dst class)
    (let ((imm5 (logior imm5 (ash lane shift))))
      (emit-instruction (logior #x0E003C00
                                (ash imm5 16)
                                (ash q 30)
                                (ash (register-number dst) +rd-shift+)
                                (ash (register-number src) +rn-shift+)))
      (return-from instruction t))))

(define-instruction smov.v (type dst src lane)
  (check-register-class src :fp-128)
  (multiple-value-bind (imm5 shift)
      (ecase type
        (:b (values #b00001 1))
        (:h (values #b00010 2))
        (:s (values #b00100 3)))
    (if (eql type :s)
        (check-register-class dst :gpr-64)
        (check-register-class dst :gpr-64 :gpr-32))
    (let ((imm5 (logior imm5 (ash lane shift)))
          (q (if (eql (register-class dst) :gpr-64) 1 0)))
      (emit-instruction (logior #x0E002C00
                                (ash imm5 16)
                                (ash q 30)
                                (ash (register-number dst) +rd-shift+)
                                (ash (register-number src) +rn-shift+)))
      (return-from instruction t))))

(define-instruction fadd.v (dst lhs rhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (check-register-class rhs :fp-128)
  (multiple-value-bind (sz q)
      (ecase size
        (:2s (values 0 0))
        (:4s (values 0 1))
        (:2d (values 1 1)))
    (emit-instruction (logior #x0E20D400
                              (ash sz 22)
                              (ash q 30)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction fsub.v (dst lhs rhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (check-register-class rhs :fp-128)
  (multiple-value-bind (sz q)
      (ecase size
        (:2s (values 0 0))
        (:4s (values 0 1))
        (:2d (values 1 1)))
    (emit-instruction (logior #x0EA0D400
                              (ash sz 22)
                              (ash q 30)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction not.v (dst lhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (let ((q (ecase size
             (:8b  0)
             (:16b 1))))
    (emit-instruction (logior #x2E205800
                              (ash q 30)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)))
    (return-from instruction t)))

(define-instruction umull.v (dst lhs rhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (check-register-class rhs :fp-128)
  (let ((q 0)
        (sz (ecase size
              (:8b #b00)
              (:4h #b01)
              (:2s #b10))))
    (emit-instruction (logior #x2E20C000
                              (ash q 30)
                              (ash sz 22)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction umull2.v (dst lhs rhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (check-register-class rhs :fp-128)
  (let ((q 1)
        (sz (ecase size
              (:16b #b00)
              (:8h #b01)
              (:4s #b10))))
    (emit-instruction (logior #x2E20C000
                              (ash q 30)
                              (ash sz 22)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction smull.v (dst lhs rhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (check-register-class rhs :fp-128)
  (let ((q 0)
        (sz (ecase size
              (:8b #b00)
              (:4h #b01)
              (:2s #b10))))
    (emit-instruction (logior #x0E20C000
                              (ash q 30)
                              (ash sz 22)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction smull2.v (dst lhs rhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (check-register-class rhs :fp-128)
  (let ((q 1)
        (sz (ecase size
              (:16b #b00)
              (:8h #b01)
              (:4s #b10))))
    (emit-instruction (logior #x0E20C000
                              (ash q 30)
                              (ash sz 22)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction add.v (dst lhs rhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (check-register-class rhs :fp-128)
  (multiple-value-bind (sz q)
      (ecase size
        (:8b  (values #b00 0))
        (:16b (values #b00 1))
        (:4h  (values #b01 0))
        (:8h  (values #b01 1))
        (:2s  (values #b10 0))
        (:4s  (values #b10 1))
        (:2d  (values #b11 1)))
    (emit-instruction (logior #x0E208400
                              (ash sz 22)
                              (ash q 30)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction uqadd.v (dst lhs rhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (check-register-class rhs :fp-128)
  (multiple-value-bind (sz q)
      (ecase size
        (:8b  (values #b00 0))
        (:16b (values #b00 1))
        (:4h  (values #b01 0))
        (:8h  (values #b01 1))
        (:2s  (values #b10 0))
        (:4s  (values #b10 1))
        (:2d  (values #b11 1)))
    (emit-instruction (logior #x2E200C00
                              (ash sz 22)
                              (ash q 30)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction sqadd.v (dst lhs rhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (check-register-class rhs :fp-128)
  (multiple-value-bind (sz q)
      (ecase size
        (:8b  (values #b00 0))
        (:16b (values #b00 1))
        (:4h  (values #b01 0))
        (:8h  (values #b01 1))
        (:2s  (values #b10 0))
        (:4s  (values #b10 1))
        (:2d  (values #b11 1)))
    (emit-instruction (logior #x0E200C00
                              (ash sz 22)
                              (ash q 30)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction sub.v (dst lhs rhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (check-register-class rhs :fp-128)
  (multiple-value-bind (sz q)
      (ecase size
        (:8b  (values #b00 0))
        (:16b (values #b00 1))
        (:4h  (values #b01 0))
        (:8h  (values #b01 1))
        (:2s  (values #b10 0))
        (:4s  (values #b10 1))
        (:2d  (values #b11 1)))
    (emit-instruction (logior #x2E208400
                              (ash sz 22)
                              (ash q 30)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction and.v (dst lhs rhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (check-register-class rhs :fp-128)
  (let ((q (ecase size
             (:8b  0)
             (:16b 1))))
    (emit-instruction (logior #x0E201C00
                              (ash q 30)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction mul.v (dst lhs rhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (check-register-class rhs :fp-128)
  (multiple-value-bind (sz q)
      (ecase size
        (:8b  (values #b00 0))
        (:16b (values #b00 1))
        (:4h  (values #b01 0))
        (:8h  (values #b01 1))
        (:2s  (values #b10 0))
        (:4s  (values #b10 1)))
    (emit-instruction (logior #x0E209C00
                              (ash sz 22)
                              (ash q 30)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)
                              (ash (register-number rhs) +rm-shift+)))
    (return-from instruction t)))

(define-instruction ushr.v (dst lhs count size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (multiple-value-bind (immh q esize)
      (ecase size
        (:8b  (values #b0001 0 8))
        (:16b (values #b0001 1 8))
        (:4h  (values #b0010 0 16))
        (:8h  (values #b0010 1 16))
        (:2s  (values #b0100 0 32))
        (:4s  (values #b0100 1 32))
        (:2d  (values #b1000 1 64)))
    (assert (<= 1 count esize))
    (emit-instruction (logior #x2F000400
                              (ash immh 19)
                              (ash q 30)
                              (ash (- (* esize 2) count) 16)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)))
    (return-from instruction t)))

(define-instruction sshr.v (dst lhs count size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (multiple-value-bind (immh q esize)
      (ecase size
        (:8b  (values #b0001 0 8))
        (:16b (values #b0001 1 8))
        (:4h  (values #b0010 0 16))
        (:8h  (values #b0010 1 16))
        (:2s  (values #b0100 0 32))
        (:4s  (values #b0100 1 32))
        (:2d  (values #b1000 1 64)))
    (assert (<= 1 count esize))
    (emit-instruction (logior #x0F000400
                              (ash immh 19)
                              (ash q 30)
                              (ash (- (* esize 2) count) 16)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)))
    (return-from instruction t)))

(define-instruction xtn.v (dst lhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (let ((q 0)
        (sz (ecase size
              (:8b #b00)
              (:4h #b01)
              (:2s #b10))))
    (emit-instruction (logior #x0E212800
                              (ash q 30)
                              (ash sz 22)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)))
    (return-from instruction t)))

(define-instruction xtn2.v (dst lhs size)
  (check-register-class dst :fp-128)
  (check-register-class lhs :fp-128)
  (let ((q 1)
        (sz (ecase size
              (:16b #b00)
              (:8h #b01)
              (:4s #b10))))
    (emit-instruction (logior #x0E212800
                              (ash q 30)
                              (ash sz 22)
                              (ash (register-number dst) +rd-shift+)
                              (ash (register-number lhs) +rn-shift+)))
    (return-from instruction t)))

(defun emit-ld/st-multiple (size loadp opcode address first-register &rest other-registers)
  (multiple-value-bind (mode base offset)
      (parse-address address)
    (multiple-value-bind (sz q)
        (ecase size
          (:8b  (values #b00 0))
          (:16b (values #b00 1))
          (:4h  (values #b01 0))
          (:8h  (values #b01 1))
          (:2s  (values #b10 0))
          (:4s  (values #b10 1))
          (:2d  (values #b11 1)))
      (check-register-class first-register :fp-128)
      (loop for i from 1
            for reg in other-registers
            do (check-register-class reg :fp-128)
               (assert (eql (register-number first-register) (- (register-number reg) i))))
      (ecase mode
        (:base-plus-immediate
         (assert (eql offset 0))
         (emit-instruction (logior #x0C000000
                                   (if loadp #x00400000 0)
                                   (ash q 30)
                                   (ash sz 10)
                                   (ash opcode 12)
                                   (ash (register-number first-register) +rt-shift+)
                                   (ash (register-number base) +rn-shift+)))
         t)
        (:post
         (if (zerop q)
             (assert (eql offset 32))
             (assert (eql offset 64)))
         (emit-instruction (logior #x0C800000
                                   (if loadp #x00400000 0)
                                   (ash q 30)
                                   (ash sz 10)
                                   (ash opcode 12)
                                   (ash 31 +rm-shift+)
                                   (ash (register-number first-register) +rt-shift+)
                                   (ash (register-number base) +rn-shift+)))
         t)
        (:post-register-offset
         (emit-instruction (logior #x0C800000
                                   (if loadp #x00400000 0)
                                   (ash q 30)
                                   (ash sz 10)
                                   (ash opcode 12)
                                   (ash (register-number offset) +rm-shift+)
                                   (ash (register-number first-register) +rt-shift+)
                                   (ash (register-number base) +rn-shift+)))
         t)))))

(define-instruction ld1 (size d1 &rest rest)
  (ecase (length rest)
    (1
     (destructuring-bind (address) rest
       (when (emit-ld/st-multiple size t #b0111 address d1)
         (return-from instruction t))))
    (2
     (destructuring-bind (d2 address) rest
       (when (emit-ld/st-multiple size t #b1010 address d1 d2)
         (return-from instruction t))))
    (3
     (destructuring-bind (d2 d3 address) rest
       (when (emit-ld/st-multiple size t #b0110 address d1 d2 d3)
         (return-from instruction t))))
    (4
     (destructuring-bind (d2 d3 d4 address) rest
       (when (emit-ld/st-multiple size t #b0010 address d1 d2 d3 d4)
         (return-from instruction t))))))

(define-instruction st1 (size d1 &rest rest)
  (ecase (length rest)
    (1
     (destructuring-bind (address) rest
       (when (emit-ld/st-multiple size nil #b0111 address d1)
         (return-from instruction t))))
    (2
     (destructuring-bind (d2 address) rest
       (when (emit-ld/st-multiple size nil #b1010 address d1 d2)
         (return-from instruction t))))
    (3
     (destructuring-bind (d2 d3 address) rest
       (when (emit-ld/st-multiple size nil #b0110 address d1 d2 d3)
         (return-from instruction t))))
    (4
     (destructuring-bind (d2 d3 d4 address) rest
       (when (emit-ld/st-multiple size nil #b0010 address d1 d2 d3 d4)
         (return-from instruction t))))))

(define-instruction ld2 (size d1 d2 address)
  (when (emit-ld/st-multiple size t #b1000 address d1 d2)
    (return-from instruction t)))

(define-instruction st2 (size d1 d2 address)
  (when (emit-ld/st-multiple size nil #b1000 address d1 d2)
    (return-from instruction t)))

(define-instruction ld3 (size d1 d2 d3 address)
  (when (emit-ld/st-multiple size t #b0100 address d1 d2 d3)
    (return-from instruction t)))

(define-instruction st3 (size d1 d2 d3 address)
  (when (emit-ld/st-multiple size nil #b0100 address d1 d2 d3)
    (return-from instruction t)))

(define-instruction ld4 (size d1 d2 d3 d4 address)
  (when (emit-ld/st-multiple size t #b0000 address d1 d2 d3 d4)
    (return-from instruction t)))

(define-instruction st4 (size d1 d2 d3 d4 address)
  (when (emit-ld/st-multiple size nil #b0000 address d1 d2 d3 d4)
    (return-from instruction t)))
