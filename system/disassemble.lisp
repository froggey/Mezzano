;;;; Copyright (c) 2018 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.disassemble
  (:use :cl)
  (:export #:disassemble #:disassemble-function))

(in-package :mezzano.disassemble)

(defun disassemble (fn)
  (disassemble-function fn))

(defun disassemble-function (fn &key gc-metadata debug-metadata)
  (when (and (consp fn) (eql (first fn) 'lambda))
    (setf fn (compile nil fn)))
  (when (not (functionp fn))
    (setf fn (fdefinition fn)))
  (check-type fn function)
  (let ((fundamental-fn fn))
    ;; If this is a funcallable-instance, peel it apart to get the juicy bit
    ;; inside. Bail out if there are multiple levels to funcallable-instances.
    (when (sys.int::funcallable-std-instance-p fundamental-fn)
      (setf fundamental-fn (sys.int::funcallable-std-instance-function fundamental-fn)))
    (when (sys.int::funcallable-std-instance-p fundamental-fn)
      (format t "~S:~%" fn)
      (format t "  nested call to ~S~%" fundamental-fn)
      (return-from disassemble-function))
    (when (sys.int::closure-p fundamental-fn)
      (setf fundamental-fn (sys.int::%closure-function fundamental-fn)))
    (assert (sys.int::%object-of-type-p fundamental-fn sys.int::+object-tag-function+))
    (cond ((eql fundamental-fn fn)
           (format t "~S:~%" fn))
          (t
           (format t "~S (implemented by ~S):~%" fn fundamental-fn)))
    (let ((base-address (logand (sys.int::lisp-object-address fundamental-fn) -16))
          (offset 16)
          (context (make-instance 'disassembler-context :function fundamental-fn)))
      (loop
         (when (>= offset (sys.int::function-code-size fundamental-fn))
           (return))
         (multiple-value-bind (decoded len)
             (disassemble-one-instruction context)
           (cond (decoded
                  (format t "  ~8,'0X: ~{~2,'0X ~}~46T~S~%"
                          (+ base-address offset)
                          (loop
                             repeat len
                             for i from offset
                             collect (sys.int::function-code-byte fundamental-fn i))
                          decoded)
                  (incf offset len))
                 (t
                  (format t "  ~8,'0X: <bad ~2,'0X>~%"
                          (+ base-address offset)
                          (sys.int::function-code-byte fundamental-fn offset))
                  (incf offset 1)))))))
  nil)

(defun decode-gpr8 (reg rex-field)
  (elt #(:al :cl :dl :bl :spl :bpl :sil :dil
         :r8l :r9l :r10l :r11l :r12l :r13l :r14l :r15l)
       (+ reg (if rex-field 8 0))))

(defun decode-gpr8-or-mem (r/m rex-field)
  (if (integerp r/m)
      (decode-gpr8 r/m rex-field)
      r/m))

(defun decode-gpr16 (reg rex-field)
  (elt #(:ax :cx :dx :bx :sp :bp :si :di
         :r8w :r9w :r10w :r11w :r12w :r13w :r14w :r15w)
       (+ reg (if rex-field 8 0))))

(defun decode-gpr16-or-mem (r/m rex-field)
  (if (integerp r/m)
      (decode-gpr16 r/m rex-field)
      r/m))

(defun decode-gpr32 (reg rex-field)
  (elt #(:eax :ecx :edx :ebx :esp :ebp :esi :edi
         :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d)
       (+ reg (if rex-field 8 0))))

(defun decode-gpr32-or-mem (r/m rex-field)
  (if (integerp r/m)
      (decode-gpr32 r/m rex-field)
      r/m))

(defun decode-gpr64 (reg rex-field)
  (elt #(:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi
         :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15)
       (+ reg (if rex-field 8 0))))

(defun decode-gpr64-or-mem (r/m rex-field)
  (if (integerp r/m)
      (decode-gpr64 r/m rex-field)
      r/m))

(defconstant +modr/m-mod+ (byte 2 6))
(defconstant +modr/m-reg+ (byte 3 3))
(defconstant +modr/m-r/m+ (byte 3 0))

(defconstant +sib-ss+ (byte 2 6))
(defconstant +sib-index+ (byte 3 3))
(defconstant +sib-base+ (byte 3 0))

(defclass instruction ()
  ((%opcode :initarg :opcode :reader inst-opcode)
   (%operands :initarg :operands :reader inst-operands)))

(defun make-instruction (opcode &rest operands)
  (make-instance 'instruction :opcode opcode :operands operands))

(defclass effective-address ()
  ((%base :initarg :base :reader ea-base)
   (%index :initarg :index :reader ea-index)
   (%scale :initarg :scale :reader ea-scale)
   (%disp :initarg :disp :reader ea-disp)
   (%segment :initarg :segment :reader ea-segment))
  (:default-initargs :base nil :index nil :scale nil :disp 0 :segment nil))

(defclass disassembler-context ()
  ((%function :initarg :function :reader context-function)
   (%offset :initform 16 :accessor context-code-offset)))

(defun consume-octet (context)
  (prog1
      (sys.int::function-code-byte (context-function context) (context-code-offset context))
    (incf (context-code-offset context))))

(defun consume-word/le (context n-octets signedp)
  (loop
     with result = 0
     for i from 0 by 8
     repeat n-octets
     do
       (setf result (logior result (ash (consume-octet context) i)))
     finally
       (return (if signedp
                   (sys.int::sign-extend result (* n-octets 8))
                   result))))

(defun consume-ub8 (context)
  (consume-octet context))

(defun consume-ub16/le (context)
  (consume-word/le context 2 nil))

(defun consume-ub32/le (context)
  (consume-word/le context 4 nil))

(defun consume-ub64/le (context)
  (consume-word/le context 8 nil))

(defun consume-sb8 (context)
  (sys.int::sign-extend (consume-octet context) 8))

(defun consume-sb16/le (context)
  (consume-word/le context 2 t))

(defun consume-sb32/le (context)
  (consume-word/le context 4 t))

(defun consume-sb64/le (context)
  (consume-word/le context 8 t))

(defun disassemble-ordinary-modr/m (context rex)
  ;; Returns reg, r/m, len.
  ;; r/m will either be an undecoded register or a decoded effective address.
  (let* ((modr/m (consume-ub8 context))
         (mod (ldb +modr/m-mod+ modr/m))
         (rex-b (rex-b rex))
         (rex-x (rex-x rex)))
    (values
     (ldb +modr/m-reg+ modr/m)
     (ecase (ldb +modr/m-mod+ modr/m)
       (#b00
        (case (ldb +modr/m-r/m+ modr/m)
          (#b100
           (let ((sib (consume-ub8 context)))
             (cond ((eql (ldb +sib-base+ sib) 5)
                    ;; No base, disp32 follows.
                    (let ((disp32 (consume-sb32/le context)))
                      (cond ((and (not rex-x) (eql (ldb +sib-index+ sib) 4))
                             (make-instance 'effective-address
                                            :disp disp32))
                            (t
                             (make-instance 'effective-address
                                            :index (decode-gpr64 (ldb +sib-index+ sib) rex-x)
                                            :scale (ash 1 (ldb +sib-ss+ sib))
                                            :disp disp32)))))
                   (t
                    (cond ((and (not rex-x) (eql (ldb +sib-index+ sib) 4))
                           (make-instance 'effective-address
                                          :base (decode-gpr64 (ldb +sib-base+ sib) rex-b)))
                          (t
                           (make-instance 'effective-address
                                          :base (decode-gpr64 (ldb +sib-base+ sib) rex-b)
                                          :index (decode-gpr64 (ldb +sib-index+ sib) rex-x)
                                          :scale (ash 1 (ldb +sib-ss+ sib)))))))))
          (#b101
           (let ((disp32 (consume-sb32/le context)))
             (make-instance 'effective-address
                            :base :rip
                            :disp disp32)))
          (t
           (make-instance 'effective-address
                          :base (decode-gpr64 (ldb +modr/m-r/m+ modr/m) rex-b)))))
       (#b01
        (case (ldb +modr/m-r/m+ modr/m)
          (#b100
           (let ((sib (consume-ub8 context))
                 (disp8 (consume-sb8 context)))
             (cond ((and (not rex-x) (eql (ldb +sib-index+ sib) 4))
                    (make-instance 'effective-address
                                   :base (decode-gpr64 (ldb +sib-base+ sib) rex-b)
                                   :disp disp8))
                   (t
                    (make-instance 'effective-address
                                   :base (decode-gpr64 (ldb +sib-base+ sib) rex-b)
                                   :index (decode-gpr64 (ldb +sib-index+ sib) rex-x)
                                   :scale (ash 1 (ldb +sib-ss+ sib))
                                   :disp disp8)))))
          (t
           (let ((disp8 (consume-sb8 context)))
             (make-instance 'effective-address
                            :base (decode-gpr64 (ldb +modr/m-r/m+ modr/m) rex-b)
                            :disp disp8)))))
       (#b10
        (case (ldb +modr/m-r/m+ modr/m)
          (#b100
           (let ((sib (consume-ub8 context))
                 (disp32 (consume-sb32/le context)))
             (cond ((and (not rex-x) (eql (ldb +sib-index+ sib) 4))
                    (make-instance 'effective-address
                                   :base (decode-gpr64 (ldb +sib-base+ sib) rex-b)
                                   :disp disp32))
                   (t
                    (make-instance 'effective-address
                                   :base (decode-gpr64 (ldb +sib-base+ sib) rex-b)
                                   :index (decode-gpr64 (ldb +sib-index+ sib) rex-x)
                                   :scale (ash 1 (ldb +sib-ss+ sib))
                                   :disp disp32)))))
          (t
           (let ((disp32 (consume-sb32/le context)))
             (make-instance 'effective-address
                            :base (decode-gpr64 (ldb +modr/m-r/m+ modr/m) rex-b)
                            :disp disp32)))))
       (#b11
        (ldb +modr/m-r/m+ modr/m))))))

(defparameter *instruction-table*
  #((decode-eb-gb sys.lap-x86:add8) ; 00
    (decode-ev-gv sys.lap-x86:add16 sys.lap-x86:add32 sys.lap-x86:add64)
    (decode-gb-eb sys.lap-x86:add8)
    (decode-gv-ev sys.lap-x86:add16 sys.lap-x86:add32 sys.lap-x86:add64)
    (decode-al-ib sys.lap-x86:add8)
    (decode-ax-iz sys.lap-x86:add16 sys.lap-x86:add32 sys.lap-x86:add64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:or8) ; 08
    (decode-ev-gv sys.lap-x86:or16 sys.lap-x86:or32 sys.lap-x86:or64)
    (decode-gb-eb sys.lap-x86:or8)
    (decode-gv-ev sys.lap-x86:or16 sys.lap-x86:or32 sys.lap-x86:or64)
    (decode-al-ib sys.lap-x86:or8)
    (decode-ax-iz sys.lap-x86:or16 sys.lap-x86:or32 sys.lap-x86:or64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:adc8) ; 10
    (decode-ev-gv sys.lap-x86:adc16 sys.lap-x86:adc32 sys.lap-x86:adc64)
    (decode-gb-eb sys.lap-x86:adc8)
    (decode-gv-ev sys.lap-x86:adc16 sys.lap-x86:adc32 sys.lap-x86:adc64)
    (decode-al-ib sys.lap-x86:adc8)
    (decode-ax-iz sys.lap-x86:adc16 sys.lap-x86:adc32 sys.lap-x86:adc64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:sbb8) ; 18
    (decode-ev-gv sys.lap-x86:sbb16 sys.lap-x86:sbb32 sys.lap-x86:sbb64)
    (decode-gb-eb sys.lap-x86:sbb8)
    (decode-gv-ev sys.lap-x86:sbb16 sys.lap-x86:sbb32 sys.lap-x86:sbb64)
    (decode-al-ib sys.lap-x86:sbb8)
    (decode-ax-iz sys.lap-x86:sbb16 sys.lap-x86:sbb32 sys.lap-x86:sbb64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:and8) ; 20
    (decode-ev-gv sys.lap-x86:and16 sys.lap-x86:and32 sys.lap-x86:and64)
    (decode-gb-eb sys.lap-x86:and8)
    (decode-gv-ev sys.lap-x86:and16 sys.lap-x86:and32 sys.lap-x86:and64)
    (decode-al-ib sys.lap-x86:and8)
    (decode-ax-iz sys.lap-x86:and16 sys.lap-x86:and32 sys.lap-x86:and64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:sub8) ; 28
    (decode-ev-gv sys.lap-x86:sub16 sys.lap-x86:sub32 sys.lap-x86:sub64)
    (decode-gb-eb sys.lap-x86:sub8)
    (decode-gv-ev sys.lap-x86:sub16 sys.lap-x86:sub32 sys.lap-x86:sub64)
    (decode-al-ib sys.lap-x86:sub8)
    (decode-ax-iz sys.lap-x86:sub16 sys.lap-x86:sub32 sys.lap-x86:sub64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:xor8) ; 30
    (decode-ev-gv sys.lap-x86:xor16 sys.lap-x86:xor32 sys.lap-x86:xor64)
    (decode-gb-eb sys.lap-x86:xor8)
    (decode-gv-ev sys.lap-x86:xor16 sys.lap-x86:xor32 sys.lap-x86:xor64)
    (decode-al-ib sys.lap-x86:xor8)
    (decode-ax-iz sys.lap-x86:xor16 sys.lap-x86:xor32 sys.lap-x86:xor64)
    nil
    nil
    (decode-eb-gb sys.lap-x86:cmp8) ; 38
    (decode-ev-gv sys.lap-x86:cmp16 sys.lap-x86:cmp32 sys.lap-x86:cmp64)
    (decode-gb-eb sys.lap-x86:cmp8)
    (decode-gv-ev sys.lap-x86:cmp16 sys.lap-x86:cmp32 sys.lap-x86:cmp64)
    (decode-al-ib sys.lap-x86:cmp8)
    (decode-ax-iz sys.lap-x86:cmp16 sys.lap-x86:cmp32 sys.lap-x86:cmp64)
    nil
    nil
    nil ; 40 (start of REX)
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 48
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    (decode+rgv64 sys.lap-x86:push) ; 50
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:push)
    (decode+rgv64 sys.lap-x86:pop) ; 58
    (decode+rgv64 sys.lap-x86:pop)
    (decode+rgv64 sys.lap-x86:pop)
    (decode+rgv64 sys.lap-x86:pop)
    (decode+rgv64 sys.lap-x86:pop)
    (decode+rgv64 sys.lap-x86:pop)
    (decode+rgv64 sys.lap-x86:pop)
    (decode+rgv64 sys.lap-x86:pop)
    nil ; 60 (various prefixes and obsolete instructions)
    nil
    nil
    nil ; movsxd (?)
    nil
    nil
    nil
    nil
    (decode-iz sys.lap-x86:push) ; 68
    (decode-gv-ev-iz sys.lap-x86:imul16 sys.lap-x86:imul32 sys.lap-x86:imul64)
    (decode-ib sys.lap-x86:push)
    (decode-gv-ev-ib sys.lap-x86:imul16 sys.lap-x86:imul32 sys.lap-x86:imul64)
    nil
    nil
    nil
    nil
    (decode-jb sys.lap-x86:jo) ; 70
    (decode-jb sys.lap-x86:jno)
    (decode-jb sys.lap-x86:jb)
    (decode-jb sys.lap-x86:jae)
    (decode-jb sys.lap-x86:jz)
    (decode-jb sys.lap-x86:jnz)
    (decode-jb sys.lap-x86:jbe)
    (decode-jb sys.lap-x86:ja)
    (decode-jb sys.lap-x86:js) ; 78
    (decode-jb sys.lap-x86:jns)
    (decode-jb sys.lap-x86:jp)
    (decode-jb sys.lap-x86:jnp)
    (decode-jb sys.lap-x86:jl)
    (decode-jb sys.lap-x86:jge)
    (decode-jb sys.lap-x86:jle)
    (decode-jb sys.lap-x86:jg)
    (decode-eb-ib *group-1*) ; 80
    (decode-ev-iz *group-1*)
    nil
    (decode-ev-ib *group-1*)
    (decode-eb-gb sys.lap-x86:test8)
    (decode-ev-gv sys.lap-x86:test16 sys.lap-x86:test32 sys.lap-x86:test64)
    (decode-eb-gb sys.lap-x86:xchg8)
    (decode-ev-gv sys.lap-x86:xchg16 sys.lap-x86:xchg32 sys.lap-x86:xchg64)
    (decode-eb-gb sys.lap-x86:mov8) ; 88
    (decode-ev-gv sys.lap-x86:mov16 sys.lap-x86:mov32 sys.lap-x86:mov64)
    (decode-gb-eb sys.lap-x86:mov8)
    (decode-gv-ev sys.lap-x86:mov16 sys.lap-x86:mov32 sys.lap-x86:mov64)
    (decode-ev-sw sys.lap-x86:movseg)
    (decode-gv-ev nil sys.lap-x86:lea32 sys.lap-x86:lea64)
    (decode-sw-ew sys.lap-x86:movseg)
    (decode-ev *group-1a*)
    (decode-xchg+r) ; 90
    (decode-xchg+r)
    (decode-xchg+r)
    (decode-xchg+r)
    (decode-xchg+r)
    (decode-xchg+r)
    (decode-xchg+r)
    (decode-xchg+r)
    nil ; 98
    (decode-simple-sized sys.lap-x86:cwd sys.lap-x86:cdq sys.lap-x86:cqo)
    nil
    (decode-simple sys.lap-x86:fwait)
    (decode-simple sys.lap-x86:pushf)
    (decode-simple sys.lap-x86:popf)
    (decode-simple sys.lap-x86:sahf)
    (decode-simple sys.lap-x86:lahf)
    (decode-al-ob sys.lap-x86:mov8) ; A0
    (decode-ax-ov sys.lap-x86:mov16 sys.lap-x86:mov32 sys.lap-x86:mov64)
    (decode-ob-al sys.lap-x86:mov8)
    (decode-ov-ax sys.lap-x86:mov16 sys.lap-x86:mov32 sys.lap-x86:mov64)
    (decode-simple sys.lap-x86:movs8)
    (decode-simple-sized sys.lap-x86:movs16 sys.lap-x86:movs32 sys.lap-x86:movs64)
    (decode-simple sys.lap-x86:cmps8)
    (decode-simple-sized sys.lap-x86:cmps16 sys.lap-x86:cmps32 sys.lap-x86:cmps64)
    (decode-al-ib sys.lap-x86:test8) ; A8
    (decode-ax-iz sys.lap-x86:test16 sys.lap-x86:test32 sys.lap-x86:test64)
    (decode-simple sys.lap-x86:stos8)
    (decode-simple-sized sys.lap-x86:stos16 sys.lap-x86:stos32 sys.lap-x86:stos64)
    (decode-simple sys.lap-x86:lods8)
    (decode-simple-sized sys.lap-x86:lods16 sys.lap-x86:lods32 sys.lap-x86:lods64)
    (decode-simple sys.lap-x86:scas8)
    (decode-simple-sized sys.lap-x86:scas16 sys.lap-x86:scas32 sys.lap-x86:scas64)
    (decode-mov+r-ib) ; B0
    (decode-mov+r-ib)
    (decode-mov+r-ib)
    (decode-mov+r-ib)
    (decode-mov+r-ib)
    (decode-mov+r-ib)
    (decode-mov+r-ib)
    (decode-mov+r-ib)
    (decode-mov+r-iv) ; B8
    (decode-mov+r-iv)
    (decode-mov+r-iv)
    (decode-mov+r-iv)
    (decode-mov+r-iv)
    (decode-mov+r-iv)
    (decode-mov+r-iv)
    (decode-mov+r-iv)
    (decode-eb-ib *group-2*) ; C0
    (decode-ev-ib *group-2*)
    nil
    (decode-simple sys.lap-x86:ret)
    nil
    nil
    (decode-eb-ib *group-11*)
    (decode-ev-iz *group-11*)
    nil ; C8
    (decode-simple sys.lap-x86:leave)
    nil
    (decode-simple sys.lap-x86:retf)
    nil
    (decode-ib sys.lap-x86:int)
    nil
    (decode-simple sys.lap-x86:iret)
    (decode-eb-1 *group-2*) ; D0
    (decode-ev-1 *group-2*)
    (decode-eb-cl *group-2*)
    (decode-ev-cl *group-2*)
    nil
    nil
    nil
    nil
    nil ; D8
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; E0
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    (decode-jz sys.lap-x86:call) ; E8
    (decode-jz sys.lap-x86:jmp)
    nil
    (decode-jb sys.lap-x86:jmp)
    nil
    nil
    nil
    nil
    nil ; F0
    nil
    nil
    nil
    (decode-simple sys.lap-x86:hlt)
    (decode-simple sys.lap-x86:cmc)
    (decode-group-3-eb)
    (decode-group-3-ev)
    (decode-simple sys.lap-x86:clc) ; F8
    (decode-simple sys.lap-x86:stc)
    (decode-simple sys.lap-x86:cli)
    (decode-simple sys.lap-x86:sti)
    (decode-simple sys.lap-x86:cld)
    (decode-simple sys.lap-x86:std)
    nil
    (decode-ev64 *group-5*)))

(defparameter *extended-instruction-table*
  #(nil ; 00
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 08
    nil
    nil
    (decode-simple sys.lap-x86:ud2)
    nil
    nil
    nil
    nil
    nil ; 10
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 18
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 20
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 28
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 30
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 38
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    (decode-gv-ev sys.lap-x86:cmov16o sys.lap-x86:cmov32o sys.lap-x86:cmov64o) ; 40
    (decode-gv-ev sys.lap-x86:cmov16no sys.lap-x86:cmov32no sys.lap-x86:cmov64no)
    (decode-gv-ev sys.lap-x86:cmov16b sys.lap-x86:cmov32b sys.lap-x86:cmov64b)
    (decode-gv-ev sys.lap-x86:cmov16ae sys.lap-x86:cmov32ae sys.lap-x86:cmov64ae)
    (decode-gv-ev sys.lap-x86:cmov16z sys.lap-x86:cmov32z sys.lap-x86:cmov64z)
    (decode-gv-ev sys.lap-x86:cmov16nz sys.lap-x86:cmov32nz sys.lap-x86:cmov64nz)
    (decode-gv-ev sys.lap-x86:cmov16be sys.lap-x86:cmov32be sys.lap-x86:cmov64be)
    (decode-gv-ev sys.lap-x86:cmov16a sys.lap-x86:cmov32a sys.lap-x86:cmov64a)
    (decode-gv-ev sys.lap-x86:cmov16s sys.lap-x86:cmov32s sys.lap-x86:cmov64s) ; 48
    (decode-gv-ev sys.lap-x86:cmov16ns sys.lap-x86:cmov32ns sys.lap-x86:cmov64ns)
    (decode-gv-ev sys.lap-x86:cmov16p sys.lap-x86:cmov32p sys.lap-x86:cmov64p)
    (decode-gv-ev sys.lap-x86:cmov16np sys.lap-x86:cmov32np sys.lap-x86:cmov64np)
    (decode-gv-ev sys.lap-x86:cmov16l sys.lap-x86:cmov32l sys.lap-x86:cmov64l)
    (decode-gv-ev sys.lap-x86:cmov16ge sys.lap-x86:cmov32ge sys.lap-x86:cmov64ge)
    (decode-gv-ev sys.lap-x86:cmov16le sys.lap-x86:cmov32le sys.lap-x86:cmov64le)
    (decode-gv-ev sys.lap-x86:cmov16g sys.lap-x86:cmov32g sys.lap-x86:cmov64g)
    nil ; 50
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 58
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 60
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 68
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 70
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 78
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    (decode-jz sys.lap-x86:jo) ; 80
    (decode-jz sys.lap-x86:jno)
    (decode-jz sys.lap-x86:jb)
    (decode-jz sys.lap-x86:jae)
    (decode-jz sys.lap-x86:jz)
    (decode-jz sys.lap-x86:jnz)
    (decode-jz sys.lap-x86:jbe)
    (decode-jz sys.lap-x86:ja)
    (decode-jz sys.lap-x86:js) ; 88
    (decode-jz sys.lap-x86:jns)
    (decode-jz sys.lap-x86:jp)
    (decode-jz sys.lap-x86:jnp)
    (decode-jz sys.lap-x86:jl)
    (decode-jz sys.lap-x86:jge)
    (decode-jz sys.lap-x86:jle)
    (decode-jz sys.lap-x86:jg)
    nil ; 90
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; 98
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; A0
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; A8
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; B0
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; B8
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; C0
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; C8
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; D0
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; D8
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; E0
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; E8
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; F0
    nil
    nil
    nil
    nil
    nil
    nil
    nil
    nil ; F8
    nil
    nil
    nil
    nil
    nil
    nil
    nil))

(defparameter *group-1*
  #((sys.lap-x86:add8 sys.lap-x86:add16 sys.lap-x86:add32 sys.lap-x86:add64)
    (sys.lap-x86:or8  sys.lap-x86:or16  sys.lap-x86:or32  sys.lap-x86:or64)
    (sys.lap-x86:adc8 sys.lap-x86:adc16 sys.lap-x86:adc32 sys.lap-x86:adc64)
    (sys.lap-x86:sbb8 sys.lap-x86:sbb16 sys.lap-x86:sbb32 sys.lap-x86:sbb64)
    (sys.lap-x86:and8 sys.lap-x86:and16 sys.lap-x86:and32 sys.lap-x86:and64)
    (sys.lap-x86:sub8 sys.lap-x86:sub16 sys.lap-x86:sub32 sys.lap-x86:sub64)
    (sys.lap-x86:xor8 sys.lap-x86:xor16 sys.lap-x86:xor32 sys.lap-x86:xor64)
    (sys.lap-x86:cmp8 sys.lap-x86:cmp16 sys.lap-x86:cmp32 sys.lap-x86:cmp64)))

(defparameter *group-2*
  #((sys.lap-x86:rol8 sys.lap-x86:rol16 sys.lap-x86:rol32 sys.lap-x86:rol64)
    (sys.lap-x86:ror8 sys.lap-x86:ror16 sys.lap-x86:ror32 sys.lap-x86:ror64)
    (sys.lap-x86:rcl8 sys.lap-x86:rcl16 sys.lap-x86:rcl32 sys.lap-x86:rcl64)
    (sys.lap-x86:rcr8 sys.lap-x86:rcr16 sys.lap-x86:rcr32 sys.lap-x86:rcr64)
    (sys.lap-x86:shl8 sys.lap-x86:shl16 sys.lap-x86:shl32 sys.lap-x86:shl64)
    (sys.lap-x86:shr8 sys.lap-x86:shr16 sys.lap-x86:shr32 sys.lap-x86:shr64)
    nil
    (sys.lap-x86:sar8 sys.lap-x86:sar16 sys.lap-x86:sar32 sys.lap-x86:sar64)))

(defparameter *group-5*
  #(nil
    nil
    sys.lap-x86:call
    nil
    sys.lap-x86:jmp
    nil
    sys.lap-x86:push
    nil))

(defparameter *group-11*
  #((sys.lap-x86:mov8 sys.lap-x86:mov16 sys.lap-x86:mov32 sys.lap-x86:mov64)
    nil
    nil
    nil
    nil
    nil
    nil
    nil))

(defun rex-w (rex)
  (and rex (logbitp 3 rex)))

(defun rex-r (rex)
  (and rex (logbitp 2 rex)))

(defun rex-x (rex)
  (and rex (logbitp 1 rex)))

(defun rex-b (rex)
  (and rex (logbitp 0 rex)))

(defun decode+rgv64 (context opcode-byte rex opcode)
  (declare (ignore context))
  `(,opcode ,(decode-gpr64 (ldb (byte 3 0) opcode-byte) (rex-b rex))))

(defun decode-ev-gv (context opcode-byte rex opcode16 opcode32 opcode64)
  (declare (ignore opcode-byte opcode16))
  (multiple-value-bind (reg r/m)
      (disassemble-ordinary-modr/m context rex)
    (if (rex-w rex)
        (and opcode64
             `(,opcode64 ,(decode-gpr64-or-mem r/m (rex-b rex))
                         ,(decode-gpr64 reg (rex-r rex))))
        (and opcode32
             `(,opcode32 ,(decode-gpr32-or-mem r/m (rex-b rex))
                         ,(decode-gpr32 reg (rex-r rex)))))))

(defun decode-gv-ev (context opcode-byte rex opcode16 opcode32 opcode64)
  (declare (ignore opcode-byte opcode16))
  (multiple-value-bind (reg r/m)
      (disassemble-ordinary-modr/m context rex)
    (if (rex-w rex)
        (and opcode64
             `(,opcode64 ,(decode-gpr64 reg (rex-r rex))
                         ,(decode-gpr64-or-mem r/m (rex-b rex))))
        (and opcode32
             `(,opcode32 ,(decode-gpr32 reg (rex-r rex))
                         ,(decode-gpr32-or-mem r/m (rex-b rex)))))))

(defun decode-ev64 (context opcode-byte rex group-table)
  (declare (ignore opcode-byte))
  (multiple-value-bind (reg r/m)
      (disassemble-ordinary-modr/m context rex)
    (let ((opcode (aref (symbol-value group-table) reg)))
      (cond (opcode
             `(,opcode
               ,(decode-gpr64-or-mem r/m (rex-b rex))))
            (t nil)))))

(defun decode-al-ib (context opcode-byte rex opcode)
  (declare (ignore opcode-byte rex))
  (let ((imm (consume-sb8 context)))
    `(,opcode :al ,imm)))

(defun decode-eb-ib (context opcode-byte rex group-table)
  (declare (ignore opcode-byte))
  (multiple-value-bind (reg r/m)
      (disassemble-ordinary-modr/m context rex)
    (let ((opcodes (aref (symbol-value group-table) reg)))
      (cond (opcodes
             (let ((imm (consume-sb8 context)))
               (and (first opcodes)
                    `(,(first opcodes)
                       ,(decode-gpr8-or-mem r/m (rex-b rex))
                       ,imm))))
            (t nil)))))

(defun decode-gb-eb (context opcode-byte rex opcode)
  (declare (ignore opcode-byte))
  (multiple-value-bind (reg r/m)
      (disassemble-ordinary-modr/m context rex)
    `(,opcode ,(decode-gpr8 reg (rex-r rex))
              ,(decode-gpr8-or-mem r/m (rex-b rex)))))

(defun decode-ev-1 (context opcode-byte rex group-table)
  (declare (ignore opcode-byte))
  (multiple-value-bind (reg r/m)
      (disassemble-ordinary-modr/m context rex)
    (let ((opcodes (aref (symbol-value group-table) reg)))
      (cond (opcodes
             (if (rex-w rex)
                 (and (fourth opcodes)
                      `(,(fourth opcodes)
                         ,(decode-gpr64-or-mem r/m (rex-b rex))
                         1))
                 (and (third opcodes)
                      `(,(third opcodes)
                         ,(decode-gpr32-or-mem r/m (rex-b rex))
                         1))))
            (t nil)))))

(defun decode-ev-ib (context opcode-byte rex group-table)
  (declare (ignore opcode-byte))
  (multiple-value-bind (reg r/m)
      (disassemble-ordinary-modr/m context rex)
    (let ((opcodes (aref (symbol-value group-table) reg)))
      (cond (opcodes
             (let ((imm (consume-sb8 context)))
               (if (rex-w rex)
                   (and (fourth opcodes)
                        `(,(fourth opcodes)
                           ,(decode-gpr64-or-mem r/m (rex-b rex))
                           ,imm))
                   (and (third opcodes)
                        `(,(third opcodes)
                           ,(decode-gpr32-or-mem r/m (rex-b rex))
                           ,imm)))))
            (t nil)))))

(defun decode-ev-iz (context opcode-byte rex group-table)
  (declare (ignore opcode-byte))
  (multiple-value-bind (reg r/m)
      (disassemble-ordinary-modr/m context rex)
    (let ((opcodes (aref (symbol-value group-table) reg)))
      (cond (opcodes
             (let ((imm (consume-sb32/le context)))
               (if (rex-w rex)
                   (and (fourth opcodes)
                        `(,(fourth opcodes)
                           ,(decode-gpr64-or-mem r/m (rex-b rex))
                           ,imm))
                   (and (third opcodes)
                        `(,(third opcodes)
                           ,(decode-gpr32-or-mem r/m (rex-b rex))
                           ,imm)))))
            (t nil)))))

(defun decode-jb (context opcode-byte rex opcode)
  (declare (ignore opcode-byte rex))
  `(,opcode ,(consume-sb8 context)))

(defun decode-jz (context opcode-byte rex opcode)
  (declare (ignore opcode-byte rex))
  `(,opcode ,(consume-sb32/le context)))

(defun decode-group-3-ev (context opcode-byte rex)
  (declare (ignore opcode-byte))
  (multiple-value-bind (reg r/m)
      (disassemble-ordinary-modr/m context rex)
    (case reg
      (0
       (let ((imm (consume-sb32/le context)))
         (if (rex-w rex)
             `(sys.lap-x86:test64 ,(decode-gpr64-or-mem r/m (rex-b rex)) ,imm)
             `(sys.lap-x86:test32 ,(decode-gpr32-or-mem r/m (rex-b rex)) ,imm))))
      (t nil))))

(defun decode-mov+r-iv (context opcode-byte rex)
  (let ((reg (ldb (byte 3 0) opcode-byte)))
    (cond ((rex-w rex)
           (let ((imm (consume-sb64/le context)))
             `(sys.lap-x86:mov64 ,(decode-gpr64 reg (rex-b rex))
                                 ,imm)))
          (t
           (let ((imm (consume-sb32/le context)))
             `(sys.lap-x86:mov32 ,(decode-gpr32 reg (rex-b rex))
                                 ,imm))))))

(defun decode-simple (context opcode-byte rex opcode)
  (declare (ignore context opcode-byte rex))
  `(,opcode))

(defun disassemble-one-instruction-1 (context)
  (let* ((byte (consume-ub8 context))
         (rex nil)
         (table *instruction-table*))
    (when (eql (logand byte #xF0) #x40)
      ;; REX prefix.
      (setf rex byte)
      (setf byte (consume-ub8 context)))
    (when (eql byte #x0F)
      (setf table *extended-instruction-table*)
      (setf byte (consume-ub8 context)))
    (let ((entry (aref table byte)))
      (cond (entry
             (apply (first entry) context byte rex (rest entry)))
            (t nil)))))

(defun disassemble-one-instruction (context)
  (let ((start (context-code-offset context))
        (inst (disassemble-one-instruction-1 context)))
    (cond (inst
           (values inst (- (context-code-offset context) start)))
          (t
           nil))))
