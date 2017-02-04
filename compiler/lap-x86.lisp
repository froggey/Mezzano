;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.lap-x86)

(defparameter *instruction-assemblers* (make-hash-table))
(defvar *cpu-mode* nil "The CPU mode to assemble for.")
(defvar *fixup-target*)

(defconstant +operand-size-override+ #x66
  "The operand size override prefix.")
(defconstant +address-size-override+ #x67
  "The address size override prefix.")

(defun assemble (code-list &rest args &key (cpu-mode 64) &allow-other-keys)
  (declare (dynamic-extent args))
  (let ((*cpu-mode* cpu-mode))
    (apply 'perform-assembly *instruction-assemblers* code-list args)))

(defmacro define-instruction (name lambda-list &body body)
  (let ((insn (gensym)))
    `(add-instruction ',name #'(lambda (,insn)
                                 #+mezzano (declare (sys.int::lambda-name (instruction ,name)))
                                 (destructuring-bind ,lambda-list (rest ,insn)
                                   (block instruction
                                     ,@body
                                     (error "Could not encode instruction ~S." ,insn)))))))

(defun add-instruction (name function)
  (export name '#:sys.lap-x86)
  (setf (gethash name *instruction-assemblers*) function))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun concat-symbols (a b &optional (package *package*))
    (intern (format nil "~A~A" (symbol-name a) (symbol-name b)) package)))

(defun reg-class (reg)
  (case reg
    ((:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15) :gpr-64)
    ((:eax :ecx :edx :ebx :esp :ebp :esi :edi :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d) :gpr-32)
    ((:ax :cx :dx :bx :sp :bp :si :di :r8w :r9w :r10w :r11w :r12w :r13w :r14w :r15w) :gpr-16)
    ((:al :cl :dl :bl :spl :bpl :sil :dil :r8l :r9l :r10l :r11l :r12l :r13l :r14l :r15l :ah :ch :dh :bh) :gpr-8)
    ((:mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7) :mm)
    ((:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8 :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15) :xmm)
    ((:cr0 :cr1 :cr2 :cr3 :cr4 :cr5 :cr6 :cr7) :cr)
    ((:dr0 :dr1 :dr2 :dr3 :dr4 :dr5 :dr6 :dr7) :dr)
    ((:cs :ss :ds :es :fs :gs) :segment)))

(defun reg-number (reg)
  (ecase reg
    ((:rax :eax  :ax   :al   :mm0 :xmm0  :cr0 :dr0 :es         ) 0)
    ((:rcx :ecx  :cx   :cl   :mm1 :xmm1  :cr1 :dr1 :cs         ) 1)
    ((:rdx :edx  :dx   :dl   :mm2 :xmm2  :cr2 :dr2 :ss         ) 2)
    ((:rbx :ebx  :bx   :bl   :mm3 :xmm3  :cr3 :dr3 :ds         ) 3)
    ((:rsp :esp  :sp   :spl  :mm4 :xmm4  :cr4 :dr4 :fs :ah     ) 4)
    ((:rbp :ebp  :bp   :bpl  :mm5 :xmm5  :cr5 :dr5 :gs :ch     ) 5)
    ((:rsi :esi  :si   :sil  :mm6 :xmm6  :cr6 :dr6     :dh     ) 6)
    ((:rdi :edi  :di   :dil  :mm7 :xmm7  :cr7 :dr7     :bh     ) 7)
    ((:r8  :r8d  :r8w  :r8l       :xmm8                        ) 8)
    ((:r9  :r9d  :r9w  :r9l       :xmm9                        ) 9)
    ((:r10 :r10d :r10w :r10l      :xmm10                       ) 10)
    ((:r11 :r11d :r11w :r11l      :xmm11                       ) 11)
    ((:r12 :r12d :r12w :r12l      :xmm12                       ) 12)
    ((:r13 :r13d :r13w :r13l      :xmm13                       ) 13)
    ((:r14 :r14d :r14w :r14l      :xmm14                       ) 14)
    ((:r15 :r15d :r15w :r15l      :xmm15                       ) 15)))

(defun convert-width (reg width)
  (dolist (conv '((:rax :eax  :ax   :al)
                  (:rcx :ecx  :cx   :cl)
                  (:rdx :edx  :dx   :dl)
                  (:rbx :ebx  :bx   :bl)
                  (:rsp :esp  :sp   :spl)
                  (:rbp :ebp  :bp   :bpl)
                  (:rsi :esi  :si   :sil)
                  (:rdi :edi  :di   :dil)
                  (:r8  :r8d  :r8w  :r8l)
                  (:r9  :r9d  :r9w  :r9l)
                  (:r10 :r10d :r10w :r10l)
                  (:r11 :r11d :r11w :r11l)
                  (:r12 :r12d :r12w :r12l)
                  (:r13 :r13d :r13w :r13l)
                  (:r14 :r14d :r14w :r14l)
                  (:r15 :r15d :r15w :r15l))
           (error "Unknown register ~S." reg))
    (when (member reg conv)
      (return (ecase width
                (8 (fourth conv))
                (16 (third conv))
                (32 (second conv))
                (64 (first conv)))))))

(defun is-bp (reg) (find reg '(:bpl :bp :ebp :rbp)))
(defun is-sp (reg) (find reg '(:spl :sp :esp :rsp)))

(defun short-form-valid (class value)
  (and (not (eql value :fixup))
       (multiple-value-bind (umin umax)
           (ecase class
             (:gpr-64 (values #xffffffffffffff80 #xffffffffffffffff))
             (:gpr-32 (values #xffffff80 #xffffffff))
             (:gpr-16 (values #xff80 #xffff)))
         (or (> 128 value -128) (>= umax value umin)))))

(defun maybe-emit-operand-size-override (class)
  (ecase class
    ((:gpr-32 :gpr-64)
     (when (= *cpu-mode* 16)
       (emit +operand-size-override+)))
    (:gpr-16
     (unless (= *cpu-mode* 16)
       (emit +operand-size-override+)))
    (:gpr-8)))

(defun encode-register (reg)
  "Return the register number and if a rex flag must be set."
  (let ((nr (if (integerp reg) reg (reg-number reg))))
    (if (>= nr 8)
        (values (- nr 8) t)
        (values nr nil))))

(defun encode-modrm (mod r/m reg)
  "Combine seperate mod/rm/reg values into one modr/m byte"
  (logior (ash mod 6) r/m (ash reg 3)))

(defun encode-sib (base index scale)
  "Combine seperate base/index/scale values into one SIB byte"
  (logior base (ash index 3) (ash scale 6)))

(defun encode-rex (&key w r x b)
  (logior #b01000000
          (if w #b1000 0)
          (if r #b0100 0)
          (if x #b0010 0)
          (if b #b0001 0)))

(defun emit-rex (&key w r x b)
  (emit (encode-rex :w w :r r :x x :b b)))

(defun register-conflicts-with-rex (reg)
  (member reg '(:ah :bh :ch :dh)))

(defun register-requires-rex-p (reg)
  (>= (reg-number reg) 8))

(defun extended-8-bit-register-p (reg)
  (member reg '(:spl :bpl :sil :dil)))

(defun pick-operand-size (class)
  (ecase class
    (:gpr-64 64)
    (:gpr-32 32)
    (:gpr-16 16)
    (:gpr-8 8)))

(defun encode-disp32 (displacement)
  (check-type displacement (signed-byte 32))
  (list (ldb (byte 8 0) displacement)
        (ldb (byte 8 8) displacement)
        (ldb (byte 8 16) displacement)
        (ldb (byte 8 24) displacement)))

(defun emit-modrm-address (class opcode reg displacement base index scale
                           &key rex-w)
  (let ((force-rex nil)
        (operand-size-override nil)
        (address-size-override nil)
        (rex nil)
        (encoded-modrm-sib '()))
    (multiple-value-bind (reg-nr rex-r)
        (encode-register reg)
      (multiple-value-bind (base-nr rex-b)
          (when base (encode-register base))
        (multiple-value-bind (index-nr rex-x)
            (when index (encode-register index))
          (ecase class
            ((:xmm :mm))
            (:gpr-64 (assert (= *cpu-mode* 64) (*cpu-mode*)
                        "64-bit operand-size only supported in 64-bit mode.")
                     (setf rex-w t))
            (:gpr-32 (when (= *cpu-mode* 16)
                       (setf operand-size-override t)))
            (:gpr-16 (when (/= *cpu-mode* 16)
                       (setf operand-size-override t)))
            (:gpr-8 (when (extended-8-bit-register-p reg)
                      (setf force-rex t))))
          ;; Compute the address size
          (let* ((base-class (when base (reg-class base)))
                 (index-class (when index (reg-class index)))
                 (address-class (or base-class index-class)))
            (when (and base-class index-class
                       (not (eql base-class index-class)))
              (error "Impossible addressing mode."))
            (ecase address-class
              (:gpr-64 (assert (= *cpu-mode* 64) (*cpu-mode*)
                               "64-bit address-size only supported in 64-bit mode."))
              (:gpr-32 (ecase *cpu-mode*
                         ((64 16) (setf address-size-override t))
                         (32 nil)))
              (:gpr-16 (ecase *cpu-mode*
                         (64 (error "Cannot use 16-bit addressing modes in 64-bit mode."))
                         (32 (setf address-size-override t))
                         (16 nil)))
              ((nil) nil)))
          ;; Compute rex byte and check for conflicts
          (when (or force-rex rex-w rex-r rex-b rex-x)
            (when (register-conflicts-with-rex reg)
              (error "Cannot encode ~S with REX prefix." reg))
            (setf rex (encode-rex :w rex-w :r rex-r :b rex-b :x rex-x)))
          ;; Build ModR/M and SIB bytes
          (if (and (null index) (null scale))
              ;; Encodings that only use base & displacement
              (cond
                ((and (or (null displacement) (= displacement 0))
                      base)
                 (cond ((= base-nr #b100)
                        ;; Force SIB
                        (setf encoded-modrm-sib (list (encode-modrm #b00 #b100 reg-nr)
                                                      (encode-sib base-nr #b100 #b00))))
                       ((= base-nr #b101)
                        ;; Force displacement
                        (setf encoded-modrm-sib (list (encode-modrm #b01 base-nr reg-nr)
                                                      #x00)))
                       (t (setf encoded-modrm-sib (list (encode-modrm #b00 base-nr reg-nr))))))
                ((and displacement (/= displacement 0)
                      base)
                 (if (= base-nr #b100)
                     (if (typep displacement '(signed-byte 8))
                         (setf encoded-modrm-sib (list (encode-modrm #b01 #b100 reg-nr)
                                                       (encode-sib base-nr #b100 #b00)
                                                       (ldb (byte 8 0) displacement)))
                         (setf encoded-modrm-sib (list* (encode-modrm #b10 #b100 reg-nr)
                                                        (encode-sib base-nr #b100 #b00)
                                                        (encode-disp32 displacement))))
                     (if (typep displacement '(signed-byte 8))
                         (setf encoded-modrm-sib (list (encode-modrm #b01 base-nr reg-nr)
                                                       (ldb (byte 8 0) displacement)))
                         (setf encoded-modrm-sib (list* (encode-modrm #b10 base-nr reg-nr)
                                                        (encode-disp32 displacement))))))
                ((and (null base))
                 ;; Avoid the rip-relative encoding when in 64-bit mode.
                 (if (and (not address-size-override)
                          (= *cpu-mode* 64))
                     (setf encoded-modrm-sib (list* (encode-modrm #b00 #b100 reg-nr)
                                                    (encode-sib #b101 #b100 #b00)
                                                    (encode-disp32 (or displacement 0))))
                     (setf encoded-modrm-sib (list* (encode-modrm #b00 #b101 reg-nr)
                                                    (encode-disp32 (or displacement 0))))))
                (t (error "Unknown/impossible addressing mode.")))
              (let ((ss (ecase scale ((nil 1) #b00) (2 #b01) (4 #b10) (8 #b11))))
                (when (null index)
                  (error "Scale without index."))
                (when (is-sp index)
                  (error "Impossible index register ~S." index))
                (cond
                  ((null base)
                   (setf encoded-modrm-sib (list* (encode-modrm #b00 #b100 reg-nr)
                                                  (encode-sib #b101 index-nr ss)
                                                  (encode-disp32 (or displacement 0)))))
                  ((or (is-bp base)
                       (and displacement (/= displacement 0)))
                   (if (typep displacement '(or null (signed-byte 8)))
                       (setf encoded-modrm-sib (list (encode-modrm #b01 #b100 reg-nr)
                                                     (encode-sib base-nr index-nr ss)
                                                     (ldb (byte 8 0) (or displacement 0))))
                       (setf encoded-modrm-sib (list* (encode-modrm #b10 #b100 reg-nr)
                                                      (encode-sib base-nr index-nr ss)
                                                      (encode-disp32 (or displacement 0))))))
                  ((or (null displacement) (= displacement 0))
                   (setf encoded-modrm-sib (list (encode-modrm #b00 #b100 reg-nr)
                                                 (encode-sib base-nr index-nr ss))))
                  (t (error "Unknown/impossible addressing mode.")))))
          (when operand-size-override
            (emit +operand-size-override+))
          (when address-size-override
            (emit +address-size-override+))
          (when rex (emit rex))
          (if (listp opcode)
              (apply #'emit opcode)
              (emit opcode))
          (apply #'emit encoded-modrm-sib))))))

(defun emit-modrm-register (class opcode reg r/m-reg
                            &key rex-w)
  (when (eql class :gpr-64)
    (assert (= *cpu-mode* 64) (*cpu-mode*) "64-bit operand-size only supported in 64-bit mode."))
  (let ((force-rex nil)
        (operand-size-override nil)
        (rex 'nil))
    (multiple-value-bind (reg-nr rex-r)
        (encode-register reg)
      (multiple-value-bind (r/m-nr rex-b)
          (encode-register r/m-reg)
        (ecase class
          ((:xmm :mm))
          (:gpr-64 (setf rex-w t))
          (:gpr-32 (when (= *cpu-mode* 16)
                     (setf operand-size-override t)))
          (:gpr-16 (when (/= *cpu-mode* 16)
                     (setf operand-size-override t)))
          (:gpr-8 (when (extended-8-bit-register-p reg)
                    (setf force-rex t))
                  (when (extended-8-bit-register-p r/m-reg)
                    (setf force-rex t))))
        (when (or force-rex rex-w rex-r rex-b)
          (when (register-conflicts-with-rex reg)
            (error "Cannot encode ~S with REX prefix." reg))
          (when (register-conflicts-with-rex r/m-reg)
            (error "Cannot encode ~S with REX prefix." r/m-reg))
          (setf rex (encode-rex :w rex-w :r rex-r :b rex-b)))
        (when operand-size-override
          (emit +operand-size-override+))
        (when rex (emit rex))
        (if (listp opcode)
            (apply #'emit opcode)
            (emit opcode))
        (emit (encode-modrm #b11 r/m-nr reg-nr))))))

(defun emit-modrm-rip-relative (class opcode reg displacement &key rex-w)
  (assert (= *cpu-mode* 64) (*cpu-mode*) "RIP-relative addressing only supported in 64-bit mode.")
  (let ((force-rex nil)
        (operand-size-override nil)
        (rex nil))
    (multiple-value-bind (reg-nr rex-r)
        (encode-register reg)
      (ecase class
        ((:xmm :mm))
        (:gpr-64 (setf rex-w t))
        (:gpr-32 nil)
        (:gpr-16 (setf operand-size-override t))
        (:gpr-8 (when (extended-8-bit-register-p reg)
                  (setf force-rex t))))
      (when (or force-rex rex-w rex-r)
        (setf rex (encode-rex :w rex-w :r rex-r)))
      (when operand-size-override
        (emit +operand-size-override+))
      (when rex (emit rex))
      (if (listp opcode)
          (apply #'emit opcode)
          (emit opcode))
      (emit (encode-modrm #b00 #b101 reg-nr))
      (apply #'emit (encode-disp32 (- (or displacement 0)
                                      (+ *current-address* 4)))))))

(defun emit-reg (operand-size opcode reg
                 &key imm imm-length rex-w)
  (when (= operand-size 64)
    (assert (= *cpu-mode* 64) (*cpu-mode*) "64-bit operand-size only supported in 64-bit mode."))
  (let ((force-rex nil)
        (operand-size-override nil)
        (rex '()))
    (multiple-value-bind (reg-nr rex-b)
        (encode-register reg)
      (ecase operand-size
        (64 (setf rex-w t))
        (32 (when (= *cpu-mode* 16)
              (setf operand-size-override t)))
        (16 (when (/= *cpu-mode* 16)
              (setf operand-size-override t)))
        (8 (when (extended-8-bit-register-p reg)
             (setf force-rex t))))
      (when (or force-rex rex-w rex-b)
        (when (register-conflicts-with-rex reg)
          (error "Cannot encode ~S with REX prefix." reg))
        (setf rex (list (encode-rex :w rex-w :b rex-b))))
      (nconc (when operand-size-override
               (list #x66))
             rex
             (list (logior opcode reg-nr))
             (when imm-length
               (encode-imm imm imm-length))))))

(defun parse-r/m (form)
  "Parse a register or effective address into a bunch of values.
First value is the register, or false if the expression is an effective address.
Remaining values describe the effective address: base index scale disp rip-relative"
  (cond ((not (consp form))
         form)
        ((and (= (length form) 2)
              (eql (first form) :constant))
         ;; Transform (:constant foo) into (:rip (:constant-address foo))
         (values nil nil nil nil (list :constant-address (second form)) t))
        ((and (= (length form) 2)
              (eql (first form) :function))
         ;; Transform (:function foo) into (:rip (:constant-address (fref foo)))
         (values nil nil nil nil (list :constant-address (funcall sys.lap:*function-reference-resolver* (second form))) t))
        ((and (= (length form) 2)
              (eql (first form) :stack)
              (integerp (second form)))
         ;; Transform (:stack n) into (:rbp (- (* (1+ n) 8))).
         (values nil :rbp nil nil (- (* (1+ (second form)) 8)) nil))
        ((and (= (length form) 2)
              (keywordp (first form))
              (not (reg-class (first form)))
              (reg-class (second form)))
         (ecase (first form)
           (:car (values nil (second form) nil nil -3))
           (:cdr (values nil (second form) nil nil (+ -3 8)))))
        ((eql (first form) :object)
         (destructuring-bind (base slot &optional index (scale 8))
             (rest form)
           (values nil
                   base
                   index
                   (if index scale nil)
                   ;; subtract +tag-object+, skip object header.
                   ;; Return an expression, so slot goes through symbol resolution, etc.
                   `(+ (- #b1001) 8 (* ,slot 8))
                   nil)))
        (t (let (base index scale disp rip-relative)
             (dolist (elt form)
               (cond ((eql elt :rip)
                      (assert (null rip-relative) () "Multiple :RIP forms in r/m form ~S." form)
                      (assert (and (null base) (null index) (null scale)) ()
                              "RIP-relative addressing only supports displacements.")
                      (assert (= *cpu-mode* 64) () "RIP-relative addressing is only supported in 64-bit mode.")
                      (setf rip-relative t))
                     ((reg-class elt)
                      (assert (null rip-relative) ()
                              "RIP-relative addressing only supports displacements.")
                      (cond ((not base) (setf base elt))
                            ((not index) (setf index elt))
                            (t (error "Too many registers in r/m form ~S." form))))
                     ((and (listp elt)
                           (eql (length elt) 2)
                           (or (eql (reg-class (first elt)) :gpr-16)
                               (eql (reg-class (first elt)) :gpr-32)
                               (eql (reg-class (first elt)) :gpr-64))
                           (integerp (second elt)))
                      (assert (null rip-relative) ()
                              "RIP-relative addressing only supports displacements.")
                      (assert (not (or index scale)) () "Too many index/scale values in r/m form ~S." form)
                      (setf index (first elt)
                            scale (second elt))
                      (when (and scale (not (integerp scale)))
                        (error "Scale value must be an integer in r/m form ~S." form)))
                     (t (assert (null disp) () "Multiple displacements in r/m form ~S." form)
                        (setf disp elt))))
             (values nil base index scale disp rip-relative)))))

(defun generate-modrm (class r/m reg opc)
  (multiple-value-bind (r/m-reg base index scale disp rip-relative)
      (parse-r/m r/m)
    (let ((disp-value (when disp (resolve-immediate disp))))
      (when (or (not disp) disp-value)
        (cond
          (r/m-reg
           (emit-modrm-register class opc reg r/m-reg :rex-w (eql class :gpr-64)))
          (rip-relative
           (emit-modrm-rip-relative class opc reg disp-value :rex-w (eql class :gpr-64)))
          (t (emit-modrm-address class opc reg disp-value base index scale :rex-w (eql class :gpr-64))))
        t))))

(defun generate-jmp (dest short-opc long-opc)
  (let ((value (resolve-immediate dest)))
    (when value
      (setf value (- value *current-address* 2))
      (cond ((<= -128 value 127)
             (emit short-opc)
             (emit (ldb (byte 8 0) value)))
            ((listp long-opc)
             (apply #'emit long-opc)
             (apply #'emit (encode-disp32 (- value 4))))
            (t
             (emit long-opc)
             (apply #'emit (encode-disp32 (- value 3)))))
      t)))

(defun emit-imm (width imm &optional (signedp t))
  (unless (integerp width)
    (setf width (ecase width
                  ((:gpr-64 :gpr-32) 4)
                  (:gpr-16 2)
                  ((:gpr-8 :mm :xmm) 1))))
  (when (eql imm :fixup)
    (assert (and (= width 4) signedp))
    (note-fixup *fixup-target*)
    (emit #xFF #xFF #xFF #xFF)
    (return-from emit-imm))
  (let ((limit (ecase width
                 (1 #x100)
                 (2 #x10000)
                 (4 #x100000000)
                 (8 #x10000000000000000))))
    (unless (<= (- limit) imm (1- limit))
      (error "Value out of bounds."))
    (dotimes (i width)
      (emit (ldb (byte 8 (* i 8)) imm)))))

(defun generate-imm-ax (class reg imm opc)
  (declare (ignore reg))
  (let ((imm-value (resolve-immediate imm)))
    (when imm-value
      (when (eql class :gpr-64)
        (emit-rex :w t))
      (maybe-emit-operand-size-override class)
      (emit opc)
      (emit-imm class imm-value)
      t)))

(defmacro modrm (class r/m reg opc)
  `(when (and (eql ,class (reg-class ,reg))
              (or (eql (reg-class ,r/m) ,class)
                  (consp ,r/m))
              ,(if (eql class :gpr-64) '(= *cpu-mode* 64) 't))
     (return-from instruction
       (generate-modrm ,class ,r/m ,reg ,opc))))

(defmacro modrm-single (class r/m opc opc-minor)
  `(when ,(if (eql class :gpr-64) '(= *cpu-mode* 64) 't)
     (return-from instruction
       (generate-modrm ,class ,r/m ,opc-minor ,opc))))

(defmacro modrm-imm8 (class r/m reg imm8 opc)
  `(when (and (eql ,class (reg-class ,reg))
              (or (eql (reg-class ,r/m) ,class)
                  (consp ,r/m))
              ,(if (eql class :gpr-64) '(= *cpu-mode* 64) 't)
              (immediatep ,imm8))
     (let ((value (resolve-immediate ,imm8)))
       (when value
         (generate-modrm ,class ,r/m ,reg ,opc)
         (emit-imm 1 value))
       (return-from instruction t))))

(defun generate-imm (class r/m imm opc opc-minor)
  (let ((imm-value (resolve-immediate imm)))
    (when imm-value
      ;; HACK to make rip-relative addresses work properly
      (incf *current-address* (ecase class
                                ((:gpr-64 :gpr-32) 4)
                                (:gpr-16 2)
                                ((:gpr-8 :xmm :mm) 1)))
      (generate-modrm class r/m opc-minor opc)
      (decf *current-address* (ecase class
                                ((:gpr-64 :gpr-32) 4)
                                (:gpr-16 2)
                                ((:gpr-8 :xmm :mm) 1)))
      (emit-imm class imm-value)
      t)))

(defmacro imm (class dst src opc opc-minor)
  `(when (and (not (reg-class ,src))
              (immediatep ,src)
              (eql ,class (or (reg-class ,dst) ,class))
              ,(if (eql class :gpr-64) '(= *cpu-mode* 64) 't))
     (return-from instruction
       (let ((*fixup-target* ,src))
         (generate-imm ,class ,dst ,src ,opc ,opc-minor)))))

(defun generate-imm-short (class r/m imm opc opc-minor)
  (let ((imm-value (resolve-immediate imm)))
    (when imm-value
      ;; HACK to make rip-relative addresses work properly
      (incf *current-address*)
      (generate-modrm class r/m opc-minor opc)
      (decf *current-address*)
      (emit-imm 1 imm-value)
      t)))

(defmacro imm-short (class dst imm opc opc-minor)
  `(when (and (not (reg-class ,imm))
              (immediatep ,imm)
              (resolve-immediate ,imm)
              (short-form-valid ,class (resolve-immediate ,imm))
              (eql ,class (or (reg-class ,dst) ,class))
              ,(if (eql class :gpr-64) '(= *cpu-mode* 64) 't))
     (return-from instruction
       (generate-imm-short ,class ,dst ,imm ,opc ,opc-minor))))

(defmacro imm-ax (class reg imm opc)
  (let ((ax-reg (ecase class
                  (:gpr-64 :rax)
                  (:gpr-32 :eax)
                  (:gpr-16 :ax)
                  (:gpr-8 :al))))
    `(when (and (not (reg-class ,imm))
                (immediatep ,imm)
                (eq ,reg ,ax-reg)
                ,(if (eql class :gpr-64) '(= *cpu-mode* 64) 't))
       (return-from instruction
         (generate-imm-ax ,class ,reg ,imm ,opc)))))

(defun generate-shift-imm (class r/m amount 1-opc n-opc opc-minor)
  (let ((value (resolve-immediate amount)))
    (cond ((eql value 1)
           (generate-modrm class r/m opc-minor 1-opc)
           t)
          (value
           (incf *current-address*)
           (generate-modrm class r/m opc-minor n-opc)
           (decf *current-address*)
           (emit-imm 1 value)
           t))))

(defmacro shift-imm (class dst amount 1-opc n-opc opc-minor)
  `(when (and (not (reg-class ,amount))
              (immediatep ,amount)
              (eql ,class (or (reg-class ,dst) ,class))
              ,(if (eql class :gpr-64) '(= *cpu-mode* 64) 't))
     (return-from instruction
       (generate-shift-imm ,class ,dst ,amount ,1-opc ,n-opc ,opc-minor))))

(defun generate-big-shift-imm (class r/m reg amount opc)
  (let ((value (resolve-immediate amount)))
    (incf *current-address*)
    (generate-modrm class r/m reg opc)
    (decf *current-address*)
    (emit-imm 1 value)
    t))

(defmacro big-shift-imm (class dst src amount opc)
  `(when (and (not (reg-class ,amount))
              (immediatep ,amount)
              (eql ,class (or (reg-class ,dst) ,class))
              ,(if (eql class :gpr-64) '(= *cpu-mode* 64) 't))
     (return-from instruction
       (generate-big-shift-imm ,class ,dst ,src ,amount ,opc))))

(defmacro jmp-imm (dst short-opc long-opc)
  `(when (and (not (reg-class ,dst))
              (immediatep ,dst))
     (return-from instruction
       (generate-jmp ,dst ,short-opc ,long-opc))))

(defmacro define-simple-instruction (name opc)
  `(define-instruction ,name ()
     ,@(if (listp opc)
           (mapcar (lambda (x) `(emit ,x)) opc)
           (list `(emit ,opc)))
     (return-from instruction t)))

(define-instruction !code16 ()
  (setf *cpu-mode* 16)
  (return-from instruction t))

(define-instruction !code32 ()
  (setf *cpu-mode* 32)
  (return-from instruction t))

(define-instruction !code64 ()
  (setf *cpu-mode* 64)
  (return-from instruction t))

;;; Prefixes, not real instructions.
(define-simple-instruction lock #xF0)
(define-simple-instruction repne #xF2)
(define-simple-instruction repnz #xF2)
(define-simple-instruction rep #xF3)
(define-simple-instruction cs #x2E)
(define-simple-instruction ss #x36)
(define-simple-instruction ds #x3E)
(define-simple-instruction es #x26)
(define-simple-instruction fs #x64)
(define-simple-instruction gs #x65)
(define-simple-instruction hint-not-taken #x2E)
(define-simple-instruction hint-taken #x3E)
(define-simple-instruction operand-size-override #x66)
(define-simple-instruction address-size-override #x67)

(define-simple-instruction fwait #x9B)
(define-simple-instruction pushf #x9C)
(define-simple-instruction popf #x9D)
(define-simple-instruction sahf #x9E)
(define-simple-instruction lahf #x9F)
(define-simple-instruction ret #xC3)
(define-simple-instruction leave #xC9)
(define-simple-instruction hlt #xF4)
(define-simple-instruction cmc #xF5)
(define-simple-instruction clc #xF8)
(define-simple-instruction stc #xF9)
(define-simple-instruction cli #xFA)
(define-simple-instruction sti #xFB)
(define-simple-instruction cld #xFC)
(define-simple-instruction std #xFD)
(define-simple-instruction syscall (#x0F #x05))
(define-simple-instruction clts (#x0F #x06))
(define-simple-instruction sysret (#x0F #x07))
(define-simple-instruction invd (#x0F #x08))
(define-simple-instruction wbinvd (#x0F #x09))
(define-simple-instruction ud2 (#x0F #x0B))
(define-simple-instruction wrmsr (#x0F #x30))
(define-simple-instruction rdtsc (#x0F #x31))
(define-simple-instruction rdmsr (#x0F #x32))
(define-simple-instruction rdpmc (#x0F #x33))
(define-simple-instruction sysenter (#x0F #x34))
(define-simple-instruction sysexit (#x0F #x35))
(define-simple-instruction getsec (#x0F #x37))
(define-simple-instruction emms (#x0F #x77))
(define-simple-instruction cpuid (#x0F #xA2))
(define-simple-instruction rsm (#x0F #xAA))
(define-simple-instruction pause (#xF3 #x90))

(define-simple-instruction fninit (#xDB #xE3))

(defmacro define-integer-define-instruction (name lambda-list (bitness class) &body body)
  `(defmacro ,name ,lambda-list
     `(progn ,@(mapcar (lambda (,bitness ,class) ,@body)
                       '(8 16 32 64)
                       '(:gpr-8 :gpr-16 :gpr-32 :gpr-64)))))

(define-integer-define-instruction define-arithmetic-instruction (name n) (bitness class)
  (let ((width-flag (if (= bitness 8) 0 1))
        (opc (ash n 3)))
    `(define-instruction ,(intern (format nil "~A~D" (symbol-name name) bitness)) (dst src)
       (modrm ,class dst src ,(logior opc width-flag))
       (modrm ,class src dst ,(logior opc width-flag 2))
       ,@(unless (eql bitness 8)
           (list `(imm-short ,class dst src #x83 ,n)))
       (imm-ax ,class dst src ,(logior opc 4 width-flag))
       (imm ,class dst src ,(logior #x80 width-flag) ,n))))

(define-arithmetic-instruction add 0)
(define-arithmetic-instruction or  1)
(define-arithmetic-instruction adc 2)
(define-arithmetic-instruction sbb 3)
(define-arithmetic-instruction and 4)
(define-arithmetic-instruction sub 5)
(define-arithmetic-instruction xor 6)
(define-arithmetic-instruction cmp 7)

(defmacro define-conditional-instruction (name lambda-list (condition-bits) &body body)
  (flet ((frob (cond bits)
           `(define-instruction ,(concat-symbols name cond) ,lambda-list
              (let ((,condition-bits ,bits))
                ,@body))))
    (let ((conditions '(o no (b nae c) (nb ae nc) (e z) (ne nz) (be na) (nbe a)
                        s ns (p pe) (np po) (l nge) (nl ge) (le ng) (nle g)))
          (i 0))
      (list* 'progn (mapcar #'(lambda (cond)
                                (prog1
                                    (if (symbolp cond)
                                        (frob cond i)
                                        (list* 'progn (mapcar (lambda (cond) (frob cond i)) cond)))
                                  (incf i)))
                            conditions)))))

(define-conditional-instruction j (dst) (condition-bits)
  (jmp-imm dst (logior #x70 condition-bits) (list #x0F (logior #x80 condition-bits))))

(define-instruction jmp (dst)
  (jmp-imm dst #xEB #xE9)
  (when (= *cpu-mode* 16)
    (modrm-single :gpr-16 dst #xff 4))
  (when (= *cpu-mode* 32)
    (modrm-single :gpr-32 dst #xff 4))
  (when (= *cpu-mode* 64)
    (modrm-single :gpr-64 dst #xff 4)))

(define-conditional-instruction set (dst) (condition-bits)
  (modrm-single :gpr-8 dst (list #x0F (logior #x90 condition-bits)) 0))

;;; The generated names are bit off here. cmov64ne instead of cmovne64. :(
(define-conditional-instruction cmov16 (dst src) (condition-bits)
  (modrm :gpr-16 src dst (list #x0F (logior #x40 condition-bits))))
(define-conditional-instruction cmov32 (dst src) (condition-bits)
  (modrm :gpr-32 src dst (list #x0F (logior #x40 condition-bits))))
(define-conditional-instruction cmov64 (dst src) (condition-bits)
  (modrm :gpr-64 src dst (list #x0F (logior #x40 condition-bits))))

(define-integer-define-instruction define-shift-instruction (name n) (bitness class)
  (let ((width-flag (if (= bitness 8) 0 1)))
    `(define-instruction ,(intern (format nil "~A~D" (symbol-name name) bitness)) (dst amount)
       (shift-imm ,class dst amount ,(logior #xD0 width-flag) ,(logior #xC0 width-flag) ,n)
       (when (eql :cl amount)
         (modrm-single ,class dst ,(logior #xD2 width-flag) ,n)))))

(define-shift-instruction rol 0)
(define-shift-instruction ror 1)
(define-shift-instruction rcl 2)
(define-shift-instruction rcr 3)
(define-shift-instruction shl 4)
(define-shift-instruction shr 5)
(define-shift-instruction sar 7)

(define-integer-define-instruction define-unary-integer-instruction (name n) (bitness class)
  (let ((width-flag (if (= bitness 8) 0 1)))
    `(define-instruction ,(intern (format nil "~A~D" (symbol-name name) bitness)) (loc)
       (modrm-single ,class loc ,(logior #xF6 width-flag) ,n))))

(define-unary-integer-instruction not 2)
(define-unary-integer-instruction neg 3)

(defmacro define-simple-instruction-with-operand-size (name opc &optional (valid-classes '(:gpr-8 :gpr-16 :gpr-32 :gpr-64)))
  (list* 'progn (mapcar (lambda (class)
                          `(define-instruction ,(if (= (length valid-classes) 1)
                                                    name
                                                    (intern (format nil "~A~D" (symbol-name name)
                                                                    (ecase class
                                                                      (:gpr-8 8)
                                                                      (:gpr-16 16)
                                                                      (:gpr-32 32)
                                                                      (:gpr-64 64)))))
                               ()
                             ,@(ecase class
                                      (:gpr-8 nil)
                                      ((:gpr-16 :gpr-32)
                                       (list `(maybe-emit-operand-size-override ,class)))
                                      (:gpr-64
                                       (list `(emit-rex :w t))))
                             (emit ,(logior opc (if (eql class :gpr-8)
                                                    0
                                                    1)))
                             (return-from instruction t)))
                        valid-classes)))

(define-simple-instruction-with-operand-size movs #xA4)
(define-simple-instruction-with-operand-size cmps #xA6)
(define-simple-instruction-with-operand-size stos #xAA)
(define-simple-instruction-with-operand-size lods #xAC)
(define-simple-instruction-with-operand-size scas #xAE)

(define-simple-instruction-with-operand-size cwd #x99 (:gpr-16))
(define-simple-instruction-with-operand-size cdq #x99 (:gpr-32))
(define-simple-instruction-with-operand-size cqo #x99 (:gpr-64))

(defmacro define-integer-instruction (name lambda-list (class) &body body)
  (list* 'progn
         (mapcar (lambda (the-class bitness)
                   `(define-instruction ,(intern (format nil "~A~D" (symbol-name name) bitness))
                        ,lambda-list
                      (let ((,class ,the-class))
                        ,@body)))
                 '(:gpr-8 :gpr-16 :gpr-32 :gpr-64)
                 '(8 16 32 64))))

(define-integer-instruction test (dst src) (class)
  (let ((width-bit (if (eql class :gpr-8) 0 1)))
    (modrm class dst src (logior #x84 width-bit))
    (imm class dst src (logior #xF6 width-bit) 0)))

(define-integer-instruction xchg (lhs rhs) (class)
  (let ((width-bit (if (eql class :gpr-8) 0 1)))
    (modrm class lhs rhs (logior #x86 width-bit))))

(define-integer-instruction xadd (lhs rhs) (class)
  (let ((width-bit (if (eql class :gpr-8) 0 1)))
    (modrm class lhs rhs `(#x0F ,(logior #xC1 width-bit)))))

(define-integer-instruction shld (dst src count) (class)
  (when (eql count :cl)
    (modrm class dst src '(#x0F #xA5)))
  (big-shift-imm class dst src count '(#x0F #xA4)))

(define-integer-instruction shrd (dst src count) (class)
  (when (eql count :cl)
    (modrm class dst src '(#x0F #xAD)))
  (big-shift-imm class dst src count '(#x0F #xAC)))

(define-integer-instruction mov (dst src) (class)
  (let ((width-bit (if (eql class :gpr-8) 0 1)))
    (when (and (eql class :gpr-64)
               (not (reg-class src))
               (immediatep src)
               (= *cpu-mode* 64)
               (eql (reg-class dst) :gpr-64))
      (let ((value (resolve-immediate src)))
        (unless (eql value :fixup)
          (return-from instruction
            (when value
              (multiple-value-bind (nr rex-b)
                  (encode-register dst)
                (emit-rex :w t :b rex-b)
                (emit (+ #xB8 nr))
                (emit-imm 8 value))
              t)))))
    (modrm class dst src (logior #x88 width-bit))
    (modrm class src dst (logior #x8A width-bit))
    (unless (eql class :gpr-64)
      (when (and (immediatep src)
                 (eql (reg-class dst) class))
        (let ((imm-value (resolve-immediate src))
              (*fixup-target* src))
          (return-from instruction
            (when imm-value
              (maybe-emit-operand-size-override class)
              (multiple-value-bind (nr rex-b)
                  (encode-register dst)
                (when rex-b
                  (when (register-conflicts-with-rex dst)
                    (error "Cannot encode ~S with REX prefix." dst))
                  (emit-rex :b rex-b))
                (emit (+ (if (eql class :gpr-8) #xB0 #xB8) nr))
                (emit-imm class imm-value)
                t))))))
    (imm class dst src (logior #xC6 width-bit) 0)))

(define-integer-instruction out (port) (class)
  (unless (eql class :gpr-64)
    (when (eql :dx port)
      (maybe-emit-operand-size-override class)
      (emit (logior #xEE (if (eql class :gpr-8) 0 1)))
      (return-from instruction t))
    (when (immediatep port)
      (let ((value (resolve-immediate port)))
        (return-from instruction
          (when value
            (maybe-emit-operand-size-override class)
            (emit (logior #xE6 (if (eql class :gpr-8) 0 1)))
            (emit-imm 1 value)))))))

(define-integer-instruction in (port) (class)
  (unless (eql class :gpr-64)
    (when (eql :dx port)
      (maybe-emit-operand-size-override class)
      (emit (logior #xEC (if (eql class :gpr-8) 0 1)))
      (return-from instruction t))
    (when (immediatep port)
      (let ((value (resolve-immediate port)))
        (return-from instruction
          (when value
            (maybe-emit-operand-size-override class)
            (emit (logior #xE4 (if (eql class :gpr-8) 0 1)))
            (emit-imm 1 value)))))))

(define-instruction movcr (dst src)
  (when (and (eql (reg-class dst) :cr)
             (eql (reg-class src) (if (= *cpu-mode* 64) :gpr-64 :gpr-32)))
    ;; set cr
    (emit #x0F #x22 (encode-modrm 3 (reg-number src) (reg-number dst)))
    (return-from instruction t))
  (when (and (eql (reg-class src) :cr)
             (eql (reg-class dst) (if (= *cpu-mode* 64) :gpr-64 :gpr-32)))
    ;; get cr
    (emit #x0F #x20 (encode-modrm 3 (reg-number dst) (reg-number src)))
    (return-from instruction t)))

(define-instruction movdr (dst src)
  (when (and (eql (reg-class dst) :dr)
             (eql (reg-class src) (if (= *cpu-mode* 64) :gpr-64 :gpr-32)))
    ;; set dr
    (emit #x0F #x23 (encode-modrm 3 (reg-number src) (reg-number dst)))
    (return-from instruction t))
  (when (and (eql (reg-class src) :dr)
             (eql (reg-class dst) (if (= *cpu-mode* 64) :gpr-64 :gpr-32)))
    ;; get dr
    (emit #x0F #x21 (encode-modrm 3 (reg-number dst) (reg-number src)))
    (return-from instruction t)))

(define-instruction movseg (dst src)
  (let ((dst-class (reg-class dst))
        (src-class (reg-class src)))
    (cond ((eql dst-class :segment)
           (modrm-single :gpr-16 src #x8E (reg-number dst))
           (modrm-single :gpr-32 src #x8E (reg-number dst))
           (modrm-single :gpr-64 src #x8E (reg-number dst)))
          ((eql src-class :segment)
           (modrm-single :gpr-16 dst #x8C (reg-number src))
           (modrm-single :gpr-32 dst #x8C (reg-number src))
           (modrm-single :gpr-64 dst #x8C (reg-number src))))))

(define-instruction iret ()
  (if (= *cpu-mode* 64)
      (emit #x48 #xCF)
      (emit #xCF))
  (return-from instruction t))

(define-instruction retf ()
  (if (= *cpu-mode* 64)
      (emit #x48 #xCB)
      (emit #xCB))
  (return-from instruction t))

(define-instruction int (vector)
  (when (immediatep vector)
    (let ((value (resolve-immediate vector)))
      (return-from instruction
        (when value
          (if (= value 3)
              (emit #xCC)
              (emit #xCD value))
          t)))))

(define-instruction call (dst)
  (when (and (not (reg-class dst))
             (immediatep dst))
    (let ((value (resolve-immediate dst)))
      (return-from instruction
        (when value
          (emit #xE8)
          (emit-imm 4 (- value *current-address* 4))
          t))))
  (modrm-single (ecase *cpu-mode*
                  (64 :gpr-64)
                  (32 :gpr-32)
                  (16 :gpr-16))
                dst #xFF 2))

(define-instruction push (value)
  (when (eql (reg-class value) (ecase *cpu-mode*
                                 (64 :gpr-64)
                                 (32 :gpr-32)
                                 (16 :gpr-16)))
    (multiple-value-bind (nr rex-b)
        (encode-register value)
      (when rex-b
        (emit-rex :b t))
      (emit (+ #x50 nr))
      (return-from instruction t)))
  (when (immediatep value)
    ;; TODO: short form.
    (let ((value (resolve-immediate value)))
      (return-from instruction
        (when value
          (emit #x68)
          (emit-imm (if (eql *cpu-mode* 16) 2 4) value)
          t))))
  (modrm-single (ecase *cpu-mode*
                  (64 :gpr-64)
                  (32 :gpr-32)
                  (16 :gpr-16))
                value #xFF 6))

(define-instruction pop (value)
  (when (eql (reg-class value) (ecase *cpu-mode*
                                 (64 :gpr-64)
                                 (32 :gpr-32)
                                 (16 :gpr-16)))
    (multiple-value-bind (nr rex-b)
        (encode-register value)
      (when rex-b
        (emit-rex :b t))
      (emit (+ #x58 nr))
      (return-from instruction t)))
  (modrm-single (ecase *cpu-mode*
                  (64 :gpr-64)
                  (32 :gpr-32)
                  (16 :gpr-16))
                value #x8F 0))

(define-instruction lea32 (dst src)
  (when (not (reg-class src))
    (modrm :gpr-32 src dst #x8D)))

(define-instruction lea64 (dst src)
  (when (not (reg-class src))
    (modrm :gpr-64 src dst #x8D)))

(define-instruction lgdt (gdtr)
  (when (consp gdtr)
    (modrm-single :gpr-32 gdtr '(#x0F #x01) 2)))

(define-instruction lidt (idtr)
  (when (consp idtr)
    (modrm-single :gpr-32 idtr '(#x0f #x01) 3)))

(define-instruction ltr (selector)
  (modrm-single :gpr-16 selector '(#x0F 00) 3))

(define-instruction idiv8 (rhs)
  (modrm-single :gpr-8 rhs #xF6 7))
(define-instruction idiv16 (rhs)
  (modrm-single :gpr-16 rhs #xF7 7))
(define-instruction idiv32 (rhs)
  (modrm-single :gpr-32 rhs #xF7 7))
(define-instruction idiv64 (rhs)
  (modrm-single :gpr-64 rhs #xF7 7))

(define-instruction imul8 (rhs)
  (modrm-single :gpr-8 rhs #xF6 5))

(defmacro define-imul (name class)
  `(define-instruction ,name (operand-one &optional operand-two)
     (cond (operand-two
            (modrm-imm8 ,class operand-one operand-one operand-two #x6B)
            (modrm ,class operand-two operand-one '(#x0F #xAF)))
           (t (modrm-single ,class operand-one #xF7 5)))))
(define-imul imul16 :gpr-16)
(define-imul imul32 :gpr-32)
(define-imul imul64 :gpr-64)

(define-instruction mul8 (rhs)
  (modrm-single :gpr-8 rhs #xF6 4))
(define-instruction mul16 (rhs)
  (modrm-single :gpr-16 rhs #xF7 4))
(define-instruction mul32 (rhs)
  (modrm-single :gpr-32 rhs #xF7 4))
(define-instruction mul64 (rhs)
  (modrm-single :gpr-64 rhs #xF7 4))

(define-instruction shrd16 (low high count)
  (when (eql count :cl)
    (modrm :gpr-16 low high '(#x0F #xAD)))
  (modrm-imm8 :gpr-16 low high count '(#x0F #xAC)))
(define-instruction shrd32 (low high count)
  (when (eql count :cl)
    (modrm :gpr-32 low high '(#x0F #xAD)))
  (modrm-imm8 :gpr-32 low high count '(#x0F #xAC)))
(define-instruction shrd64 (low high count)
  (when (eql count :cl)
    (modrm :gpr-64 low high '(#x0F #xAD)))
  (modrm-imm8 :gpr-64 low high count '(#x0F #xAC)))

(define-instruction movd (dst src)
  (when (and (eql (reg-class dst) :xmm)
             (or (eql (reg-class src) :gpr-32)
                 (consp src)))
    (emit #x66)
    (return-from instruction
      (generate-modrm :xmm src dst '(#x0F #x6E))))
  (when (and (eql (reg-class src) :xmm)
             (or (eql (reg-class dst) :gpr-32)
                 (consp dst)))
    (emit #x66)
    (return-from instruction
      (generate-modrm :xmm dst src '(#x0F #x7E))))
  (when (and (eql (reg-class dst) :mm)
             (or (eql (reg-class src) :gpr-32)
                 (consp src)))
    (return-from instruction
      (generate-modrm :mm src dst '(#x0F #x6E))))
  (when (and (eql (reg-class src) :mm)
             (or (eql (reg-class dst) :gpr-32)
                 (consp dst)))
    (return-from instruction
      (generate-modrm :mm dst src '(#x0F #x7E)))))

(define-instruction movq (dst src)
  (when (and (eql (reg-class dst) :xmm)
             (or (eql (reg-class src) :gpr-64)
                 (consp src)))
    (emit #x66)
    (return-from instruction
      (generate-modrm :gpr-64 src dst '(#x0F #x6E))))
  (when (and (eql (reg-class src) :xmm)
             (or (eql (reg-class dst) :gpr-64)
                 (consp dst)))
    (emit #x66)
    (return-from instruction
      (generate-modrm :gpr-64 dst src '(#x0F #x7E))))
  (when (and (eql (reg-class dst) :mm)
             (or (eql (reg-class src) :gpr-64)
                 (consp src)))
    (return-from instruction
      (generate-modrm :gpr-64 src dst '(#x0F #x6E))))
  (when (and (eql (reg-class src) :mm)
             (or (eql (reg-class dst) :gpr-64)
                 (consp dst)))
    (return-from instruction
      (generate-modrm :gpr-64 dst src '(#x0F #x7E)))))

(define-instruction ucomiss (lhs rhs)
  (modrm :xmm rhs lhs '(#x0F #x2E)))

(define-instruction ucomisd (lhs rhs)
  (emit #x66)
  (modrm :xmm rhs lhs '(#x0F #x2E)))

(defmacro define-sse-float-op (name opcode)
  (list 'progn
        `(define-instruction ,(intern (format nil "~ASS" name)) (lhs rhs)
           (emit #xF3)
           (modrm :xmm rhs lhs '(#x0F ,opcode)))
        `(define-instruction ,(intern (format nil "~ASD" name)) (lhs rhs)
           (emit #xF2)
           (modrm :xmm rhs lhs '(#x0F ,opcode)))
        `(define-instruction ,(intern (format nil "~APS" name)) (lhs rhs)
           (modrm :xmm rhs lhs '(#x0F ,opcode)))
        `(define-instruction ,(intern (format nil "~APD" name)) (lhs rhs)
           (emit #x66)
           (modrm :xmm rhs lhs '(#x0F ,opcode)))))

(define-sse-float-op add #x58)
(define-sse-float-op div #x5E)
(define-sse-float-op mul #x59)
(define-sse-float-op sub #x5C)
(define-sse-float-op sqrt #x51)

(define-instruction cvtss2si64 (dst src)
  (when (and (eql (reg-class src) :xmm)
             (or (eql (reg-class dst) :gpr-64)
                 (consp dst)))
    (emit #xF3)
    (return-from instruction
      (generate-modrm :gpr-64 dst src '(#x0F #x2D)))))

(define-instruction cvttss2si64 (dst src)
  (when (and (eql (reg-class src) :xmm)
             (or (eql (reg-class dst) :gpr-64)
                 (consp dst)))
    (emit #xF3)
    (return-from instruction
      (generate-modrm :gpr-64 dst src '(#x0F #x2C)))))

(define-instruction cvtsi2ss64 (dst src)
  (when (and (eql (reg-class dst) :xmm)
             (or (eql (reg-class src) :gpr-64)
                 (consp src)))
    (emit #xF3)
    (return-from instruction
      (generate-modrm :gpr-64 src dst '(#x0F #x2A)))))

(define-instruction cvtsd2si64 (dst src)
  (when (and (eql (reg-class src) :xmm)
             (or (eql (reg-class dst) :gpr-64)
                 (consp dst)))
    (emit #xF2)
    (return-from instruction
      (generate-modrm :gpr-64 dst src '(#x0F #x2D)))))

(define-instruction cvttsd2si64 (dst src)
  (when (and (eql (reg-class src) :xmm)
             (or (eql (reg-class dst) :gpr-64)
                 (consp dst)))
    (emit #xF2)
    (return-from instruction
      (generate-modrm :gpr-64 dst src '(#x0F #x2C)))))

(define-instruction cvtsi2sd64 (dst src)
  (when (and (eql (reg-class dst) :xmm)
             (or (eql (reg-class src) :gpr-64)
                 (consp src)))
    (emit #xF2)
    (return-from instruction
      (generate-modrm :gpr-64 src dst '(#x0F #x2A)))))

(define-instruction cvtss2sd64 (dst src)
  (when (and (eql (reg-class dst) :xmm)
             (or (eql (reg-class src) :xmm)
                 (consp src)))
    (emit #xF3)
    (return-from instruction
      (generate-modrm :gpr-64 src dst '(#x0F #x5A)))))

(define-instruction cvtsd2ss64 (dst src)
  (when (and (eql (reg-class dst) :xmm)
             (or (eql (reg-class src) :xmm)
                 (consp src)))
    (emit #xF2)
    (return-from instruction
      (generate-modrm :gpr-64 src dst '(#x0F #x5A)))))

(define-instruction fxrstor (area)
  (when (consp area)
    (modrm-single :gpr-32 area '(#x0F #xAE) 1)))

(define-instruction fxsave (area)
  (when (consp area)
    (modrm-single :gpr-32 area '(#x0F #xAE) 0)))

(define-instruction ldmxcsr (area)
  (when (consp area)
    (modrm-single :gpr-32 area '(#x0F #xAE) 2)))

(define-instruction stmxcsr (area)
  (when (consp area)
    (modrm-single :gpr-32 area '(#x0F #xAE) 3)))

(defmacro mmx-integer-op (lhs rhs opcode)
  `(when (eql (reg-class ,lhs) :mm)
     (modrm :mm ,rhs ,lhs ',opcode)))

(defmacro xmm-integer-op (lhs rhs opcode)
  `(when (eql (reg-class ,lhs) :xmm)
     (emit #x66)
     (modrm :xmm ,rhs ,lhs ',opcode)))

(defmacro define-simd-integer-op (name opcode)
  `(define-instruction ,name (lhs rhs)
     (mmx-integer-op lhs rhs ,opcode)
     (xmm-integer-op lhs rhs ,opcode)))

(define-simd-integer-op pxor (#x0F #xEF))
(define-simd-integer-op pshufb (#x0F #x38 #x00))
(define-simd-integer-op psubb (#x0F #xF8))
(define-simd-integer-op punpcklbw (#x0F #x60))
(define-simd-integer-op pmulhuw (#x0F #xE4))
(define-simd-integer-op pmulhw (#x0F #xE5))
(define-simd-integer-op pmullw (#x0F #xD5))
(define-simd-integer-op pmuludq (#x0F #xF4))
(define-simd-integer-op paddw (#x0F #xFD))
(define-simd-integer-op paddusw (#x0F #xDD))
(define-simd-integer-op packsswb (#x0F #x63))
(define-simd-integer-op packuswb (#x0F #x67))

(define-instruction psrlw (lhs rhs)
  (when (eql (reg-class lhs) :mm)
    (imm :mm lhs rhs '(#x0F #x71) 2)
    (modrm :mm rhs lhs '(#x0F #xD1)))
  (when (eql (reg-class lhs) :xmm)
    (emit #x66)
    (imm :xmm lhs rhs '(#x0F #x71) 2)
    (modrm :xmm rhs lhs '(#x0F #xD1))))

(define-instruction psrld (lhs rhs)
  (when (eql (reg-class lhs) :mm)
    (imm :mm lhs rhs '(#x0F #x72) 2)
    (modrm :mm rhs lhs '(#x0F #xD2)))
  (when (eql (reg-class lhs) :xmm)
    (emit #x66)
    (imm :xmm lhs rhs '(#x0F #x72) 2)
    (modrm :xmm rhs lhs '(#x0F #xD2))))

(defmacro modrm-two-classes (class r/m-class r/m reg opc)
  `(when (and (eql ,class (reg-class ,reg))
              (or (eql (reg-class ,r/m) ,r/m-class)
                  (consp ,r/m))
              ,(if (eql class :gpr-64) '(= *cpu-mode* 64) 't))
     (return-from instruction
       (generate-modrm ,class ,r/m ,reg ,opc))))

(define-instruction movsx8 (dst src)
  (when (or (consp src)
            (eql (reg-class src) :gpr-8))
    (ecase (reg-class dst)
      (:gpr-16
       (modrm-two-classes :gpr-16 :gpr-8 src dst '(#x0F #xBE)))
      (:gpr-32
       (modrm-two-classes :gpr-32 :gpr-8 src dst '(#x0F #xBE)))
      (:gpr-64
       (modrm-two-classes :gpr-64 :gpr-8 src dst '(#x0F #xBE))))))

(define-instruction movsx16 (dst src)
  (when (or (consp src)
            (eql (reg-class src) :gpr-16))
    (ecase (reg-class dst)
      (:gpr-32
       (modrm-two-classes :gpr-32 :gpr-16 src dst '(#x0F #xBF)))
      (:gpr-64
       (modrm-two-classes :gpr-64 :gpr-16 src dst '(#x0F #xBF))))))

(define-instruction movsx32 (dst src)
  (when (or (consp src)
            (eql (reg-class src) :gpr-32))
    (ecase (reg-class dst)
      (:gpr-64
       (modrm-two-classes :gpr-64 :gpr-32 src dst '#x63)))))

(define-instruction movzx8 (dst src)
  (when (or (consp src)
            (eql (reg-class src) :gpr-8))
    (ecase (reg-class dst)
      (:gpr-16
       (modrm-two-classes :gpr-16 :gpr-8 src dst '(#x0F #xB6)))
      (:gpr-32
       (modrm-two-classes :gpr-32 :gpr-8 src dst '(#x0F #xB6)))
      (:gpr-64
       (modrm-two-classes :gpr-64 :gpr-8 src dst '(#x0F #xB6))))))

(define-instruction movzx16 (dst src)
  (when (or (consp src)
            (eql (reg-class src) :gpr-16))
    (ecase (reg-class dst)
      (:gpr-32
       (modrm-two-classes :gpr-32 :gpr-16 src dst '(#x0F #xB7)))
      (:gpr-64
       (modrm-two-classes :gpr-64 :gpr-16 src dst '(#x0F #xB7))))))

(define-instruction cmpxchg (place new)
  (ecase (reg-class new)
    (:gpr-8  (modrm :gpr-8  place new '(#x0F #xB0)))
    (:gpr-16 (modrm :gpr-16 place new '(#x0F #xB1)))
    (:gpr-32 (modrm :gpr-32 place new '(#x0F #xB1)))
    (:gpr-64 (modrm :gpr-64 place new '(#x0F #xB1)))))

(define-instruction cmpxchg16b (place)
  (modrm-single :gpr-64 place '(#x0F #xC7) 1))

(define-instruction btr64 (bit-base bit-offset)
  (modrm :gpr-64 bit-base bit-offset '(#x0F #xB3))
  (imm-short :gpr-64 bit-base bit-offset '(#x0F #xBA) 6))

(define-instruction invlpg (address)
  (when (and (not (keywordp address))
             (not (immediatep address)))
    (modrm-single :gpr-32 address '(#x0F #x01) 7)))
