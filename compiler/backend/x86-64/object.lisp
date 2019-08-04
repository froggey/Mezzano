;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.x86-64)

(define-builtin mezzano.runtime::%car ((cons) result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list result `(:car ,cons))
                       :inputs (list cons)
                       :outputs (list result))))

(define-builtin mezzano.runtime::%cdr ((cons) result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list result `(:cdr ,cons))
                       :inputs (list cons)
                       :outputs (list result))))

(define-builtin (setf mezzano.runtime::%car) ((value cons) result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list `(:car ,cons) value)
                       :inputs (list cons value)
                       :outputs '()))
  (emit (make-instance 'ir:move-instruction
                       :destination result
                       :source value)))

(define-builtin (setf mezzano.runtime::%cdr) ((value cons) result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list `(:cdr ,cons) value)
                       :inputs (list cons value)
                       :outputs '()))
  (emit (make-instance 'ir:move-instruction
                       :destination result
                       :source value)))

(define-builtin sys.int::%value-has-tag-p ((object (:constant tag (typep tag '(unsigned-byte 4))))
                                           :z
                                           :has-wrapper nil)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:lea64
                         :operands (list temp `(,object ,(- tag)))
                         :inputs (list object)
                         :outputs (list temp)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:test64
                         :operands (list temp #b1111)
                         :inputs (list temp)
                         :outputs '()))))

(define-builtin sys.int::%value-has-immediate-tag-p ((object (:constant tag (typep tag '(unsigned-byte 2))))
                                                     :z
                                                     :has-wrapper nil)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:lea64
                         :operands (list temp `(,object ,(- (logior (ash tag 4) sys.int::+tag-immediate+))))
                         :inputs (list object)
                         :outputs (list temp)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:test64
                         :operands (list temp #b111111)
                         :inputs (list temp)
                         :outputs '()))))

(define-builtin mezzano.runtime::%%object-of-type-p ((object (:constant object-tag (typep object-tag '(unsigned-byte 6))))
                                                     :e
                                                     :has-wrapper nil)
  (cond ((eql object-tag 0)
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:test8
                              :operands (list `(:object ,object -1) (ash (1- (ash 1 sys.int::+object-type-size+))
                                                                         sys.int::+object-type-shift+))
                              :inputs (list object)
                              :outputs (list))))
        (t
         ;; TODO: Use an integer vreg instead of rax here. x86-instruction must be extended to support converting allocated pregs to their 8-bit counterparts.
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:mov8
                              :operands (list :al `(:object ,object -1))
                              :inputs (list object)
                              :outputs (list :rax)))
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:and8
                              :operands (list :al (ash (1- (ash 1 sys.int::+object-type-size+))
                                                       sys.int::+object-type-shift+))
                              :inputs (list :rax)
                              :outputs (list :rax)))
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:cmp8
                              :operands (list :al (ash object-tag sys.int::+object-type-shift+))
                              :inputs (list :rax)
                              :outputs '())))))

(define-builtin mezzano.runtime::%%object-of-type-range-p ((object
                                                            (:constant first-tag (typep first-tag '(unsigned-byte 6)))
                                                            (:constant last-tag (typep last-tag '(unsigned-byte 6))))
                                                           :be
                                                           :has-wrapper nil)
  ;; TODO: Use an integer vreg instead of rax here. x86-instruction must be extended to support converting allocated pregs to their 8-bit counterparts.
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov8
                       :operands (list :al `(:object ,object -1))
                       :inputs (list object)
                       :outputs (list :rax)))
  (when (not (eql first-tag 0))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:sub8
                         :operands (list :al (ash first-tag
                                                  sys.int::+object-type-shift+))
                         :inputs (list :rax)
                         :outputs (list :rax))))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:and8
                       :operands (list :al (ash (1- (ash 1 sys.int::+object-type-size+))
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp8
                       :operands (list :al (ash (- last-tag first-tag)
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs '())))

(define-builtin sys.int::%instance-or-funcallable-instance-p ((object) :e)
  ;; TODO: Use an integer vreg instead of rax here. x86-instruction must be extended to support converting allocated pregs to their 8-bit counterparts.
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov8
                       :operands (list :al `(:object ,object -1))
                       :inputs (list object)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:and8
                       :operands (list :al (ash (logxor (1- (ash 1 sys.int::+object-type-size+))
                                                        sys.int::+object-tag-instance+
                                                        sys.int::+object-tag-funcallable-instance+)
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp8
                       :operands (list :al (ash (logand sys.int::+object-tag-instance+
                                                        sys.int::+object-tag-funcallable-instance+)
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs '())))

(define-builtin sys.int::%object-tag ((object) result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:movzx8
                       :operands (list :eax `(:object ,object -1))
                       :inputs (list object)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:and32
                       :operands (list :eax -4)
                       :inputs (list :rax)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:shr32
                       :operands (list :eax 1)
                       :inputs (list :rax)
                       :outputs (list :rax)))
  (emit (make-instance 'ir:move-instruction
                       :source :rax
                       :destination result)))

;;; Constructing and deconstructing Lisp values.

(define-builtin sys.int::lisp-object-address ((value) result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:lea64
                       :operands (list result (list value value))
                       :inputs (list value)
                       :outputs (list result))))

(define-builtin sys.int::%%assemble-value ((pointer tag) result)
  (cond ((constant-value-p tag '(eql 0))
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:sar64
                              :result result
                              :lhs pointer
                              :rhs sys.int::+n-fixnum-bits+)))
        ((constant-value-p tag '(signed-byte 29))
         (let ((raw-pointer (make-instance 'ir:virtual-register)))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:sar64
                                :result raw-pointer
                                :lhs pointer
                                :rhs sys.int::+n-fixnum-bits+))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:or64
                                :result result
                                :lhs raw-pointer
                                :rhs (fetch-constant-value tag)))))
        (t
         (let ((raw-pointer (make-instance 'ir:virtual-register))
               (raw-tag (make-instance 'ir:virtual-register :kind :integer)))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:sar64
                                :result raw-pointer
                                :lhs pointer
                                :rhs sys.int::+n-fixnum-bits+))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:sar64
                                :result raw-tag
                                :lhs tag
                                :rhs sys.int::+n-fixnum-bits+))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:or64
                                :result result
                                :lhs raw-pointer
                                :rhs raw-tag))))))

(define-builtin sys.int::%pointer-field ((value) result)
  (let ((temp (make-instance 'ir:virtual-register)))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:and64
                         :result temp
                         :lhs value
                         :rhs -16))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:sar64
                         :result result
                         :lhs temp
                         :rhs (- (byte-size sys.int::+tag-field+)
                                 sys.int::+n-fixnum-bits+)))))

(define-builtin sys.int::%tag-field ((value) result)
  (let ((temp (make-instance 'ir:virtual-register)))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:shl64
                         :result temp
                         :lhs value
                         :rhs sys.int::+n-fixnum-bits+))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:and64
                         :result result
                         :lhs temp
                         :rhs (ash (1- (ash 1 (byte-size sys.int::+tag-field+)))
                                   sys.int::+n-fixnum-bits+)))))

;;; Support objects

(defmacro define-support-object (name symbol)
  (let ((predicate-name (intern (format nil "~A-P" name) (symbol-package name))))
    `(progn
       (define-builtin ,predicate-name ((object) :e)
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:cmp64
                              :operands (list object ,symbol)
                              :inputs (list object)
                              :outputs (list))))
       (define-builtin ,name (() result)
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:mov64
                              :operands (list result ,symbol)
                              :inputs (list)
                              :outputs (list result)))))))

(define-support-object sys.int::%unbound-value :unbound-value)
(define-support-object sys.int::%undefined-function :undefined-function)
(define-support-object sys.int::%closure-trampoline :closure-trampoline)
(define-support-object sys.int::%funcallable-instance-trampoline :funcallable-instance-trampoline)
(define-support-object sys.int::%symbol-binding-cache-sentinel :symbol-binding-cache-sentinel)

(define-builtin eq ((lhs rhs) :e)
  (cond ((constant-value-p rhs '(eql nil))
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:cmp64
                              :operands (list lhs nil)
                              :inputs (list lhs)
                              :outputs '())))
        ((constant-value-p rhs '(eql t))
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:cmp64
                              :operands (list lhs t)
                              :inputs (list lhs)
                              :outputs '())))
        ((constant-value-p rhs '(eql 0))
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:test64
                              :operands (list lhs lhs)
                              :inputs (list lhs)
                              :outputs '())))
        ((constant-value-p rhs '(signed-byte 31))
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:cmp64
                              :operands (list lhs (ash (fetch-constant-value rhs)
                                                       sys.int::+n-fixnum-bits+))
                              :inputs (list lhs)
                              :outputs '())))
        (t
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:cmp64
                              :operands (list lhs rhs)
                              :inputs (list lhs rhs)
                              :outputs '())))))

(define-builtin sys.int::%object-ref-t ((object index) result)
  (cond ((constant-value-p index '(signed-byte 29))
         (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list result `(:object ,object ,(fetch-constant-value index)))
                       :inputs (list object)
                       :outputs (list result))))
        (t
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:mov64
                              :operands (list result `(:object ,object 0 ,index 4))
                              :inputs (list object index)
                              :outputs (list result))))))

(define-builtin (setf sys.int::%object-ref-t) ((value object index) result)
  (cond ((constant-value-p index '(signed-byte 29))
         (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list `(:object ,object ,(fetch-constant-value index)) value)
                       :inputs (list value object)
                       :outputs (list))))
        (t
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:mov64
                              :operands (list `(:object ,object 0 ,index 4) value)
                              :inputs (list value object index)
                              :outputs (list)))))
  (emit (make-instance 'ir:move-instruction
                       :source value
                       :destination result)))

(define-builtin sys.int::%object-header-data ((object) result)
  (let ((temp1 (make-instance 'ir:virtual-register))
        (temp2 (make-instance 'ir:virtual-register)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list temp1 `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list temp1)))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:and64
                         :result temp2
                         :lhs temp1
                         :rhs (lognot (1- (ash 1 sys.int::+object-data-shift+)))))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:shr64
                         :result result
                         :lhs temp2
                         :rhs (- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+)))))

(define-builtin (setf sys.int::%object-header-data) ((value object) result)
  (emit (make-instance 'ir:move-instruction
                       :destination :rax
                       :source value))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:shl64
                       :operands (list :rax (- sys.int::+object-data-shift+
                                               sys.int::+n-fixnum-bits+))
                       :inputs (list :rax)
                       :outputs (list :rax)
                       :clobbers (list :rax)))
  ;; low 8 bits of the header only.
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov8
                       :operands (list :al `(:object ,object -1))
                       :inputs (list :rax object)
                       :outputs (list :rax)
                       :clobbers (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list `(:object ,object -1) :rax)
                       :inputs (list :rax object)
                       :outputs (list)
                       :clobbers (list)))
  (emit (make-instance 'ir:move-instruction
                       :destination result
                       :source value)))

(define-builtin sys.int::%instance-layout ((object) result)
  (let ((temp1 (make-instance 'ir:virtual-register)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list temp1 `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list temp1)))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:shr64
                         :result result
                         :lhs temp1
                         :rhs sys.int::+object-data-shift+))
    ;; The object must be kept live over the shift, as the header will
    ;; initially be read as a fixnum. If the object is the only thing keeping
    ;; the structure definition live there is a possibility that it and the
    ;; structure definition could be end up being GC'd between the load & shift.
    ;; Then the shift would resurrect a dead object, leading to trouble.
    (emit (make-instance 'ir:spice-instruction :value object))))

(define-builtin sys.int::%fast-instance-layout-eq-p ((object instance-header) :e)
  (let ((temp1 (make-instance 'ir:virtual-register :kind :integer))
        (temp2 (make-instance 'ir:virtual-register :kind :integer)))
    ;; Read the object header.
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list temp1 `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list temp1)))
    ;; Set the two low bits to potentially convert the header to a
    ;; structure-header. This must be performed in an integer register
    ;; as this will construct some random bad value if the object isn't a
    ;; structure-object.
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:or64
                         :result temp2
                         :lhs temp1
                         :rhs 3))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cmp64
                         :operands (list temp2 instance-header)
                         :inputs (list temp2 instance-header)
                         :outputs '()))))

(define-builtin sys.int::%%object-ref-unsigned-byte-8 ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:movzx8
                         :operands (list :eax `(:object ,object 0 ,unboxed-index 1))
                         :inputs (list object unboxed-index)
                         :outputs (list :rax)
                         :clobbers '(:rax)))
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-unsigned-byte-8) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov8
                         :operands (list `(:object ,object 0 ,unboxed-index 1) :al)
                         :inputs (list object unboxed-index :rax)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-unsigned-byte-16 ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:movzx16
                         :operands (list :eax `(:object ,object 0 ,index 1))
                         :inputs (list object index)
                         :outputs (list :rax)
                         :clobbers '(:rax)))
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-unsigned-byte-16) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov16
                         :operands (list `(:object ,object 0 ,index 1) :ax)
                         :inputs (list object index :rax)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-unsigned-byte-32 ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov32
                         :operands (list :eax `(:object ,object 0 ,index 2))
                         :inputs (list object index)
                         :outputs (list :rax)
                         :clobbers '(:rax)))
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-unsigned-byte-32) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov32
                         :operands (list `(:object ,object 0 ,index 2) :eax)
                         :inputs (list object index :rax)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-unsigned-byte-64 ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list temp `(:object ,object 0 ,index 4))
                         :inputs (list object index)
                         :outputs (list temp)))
    (emit (make-instance 'ir:box-unsigned-byte-64-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-unsigned-byte-64) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-unsigned-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list `(:object ,object 0 ,index 4) temp)
                         :inputs (list object index temp)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-signed-byte-8 ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:movsx8
                         :operands (list temp `(:object ,object 0 ,unboxed-index 1))
                         :inputs (list object unboxed-index)
                         :outputs (list temp)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-signed-byte-8) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov8
                         :operands (list `(:object ,object 0 ,unboxed-index 1) :al)
                         :inputs (list object unboxed-index :rax)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-signed-byte-16 ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:movsx16
                         :operands (list temp `(:object ,object 0 ,index 1))
                         :inputs (list object index)
                         :outputs (list temp)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-signed-byte-16) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov16
                         :operands (list `(:object ,object 0 ,index 1) :ax)
                         :inputs (list object index :rax)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-signed-byte-32 ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:movsx32
                         :operands (list temp `(:object ,object 0 ,index 2))
                         :inputs (list object index)
                         :outputs (list temp)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-signed-byte-32) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov32
                         :operands (list `(:object ,object 0 ,index 2) :eax)
                         :inputs (list object index :rax)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-signed-byte-64 ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list temp `(:object ,object 0 ,index 4))
                         :inputs (list object index)
                         :outputs (list temp)))
    (emit (make-instance 'ir:box-signed-byte-64-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-signed-byte-64) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-signed-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list `(:object ,object 0 ,index 4) temp)
                         :inputs (list object index temp)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-unsigned-byte-8-unscaled ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:movzx8
                         :operands (list :eax `(:object ,object 0 ,unboxed-index 1))
                         :inputs (list object unboxed-index)
                         :outputs (list :rax)
                         :clobbers '(:rax)))
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-unsigned-byte-8-unscaled) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov8
                         :operands (list `(:object ,object 0 ,unboxed-index 1) :al)
                         :inputs (list object unboxed-index :rax)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-unsigned-byte-16-unscaled ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:movzx16
                         :operands (list :eax `(:object ,object 0 ,unboxed-index 1))
                         :inputs (list object unboxed-index)
                         :outputs (list :rax)
                         :clobbers '(:rax)))
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-unsigned-byte-16-unscaled) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov16
                         :operands (list `(:object ,object 0 ,unboxed-index 1) :ax)
                         :inputs (list object unboxed-index :rax)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-unsigned-byte-32-unscaled ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov32
                         :operands (list :eax `(:object ,object 0 ,unboxed-index 1))
                         :inputs (list object unboxed-index)
                         :outputs (list :rax)
                         :clobbers '(:rax)))
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-unsigned-byte-32-unscaled) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov32
                         :operands (list `(:object ,object 0 ,unboxed-index 1) :eax)
                         :inputs (list object unboxed-index :rax)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-unsigned-byte-64-unscaled ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list temp `(:object ,object 0 ,unboxed-index 1))
                         :inputs (list object unboxed-index)
                         :outputs (list temp)))
    (emit (make-instance 'ir:box-unsigned-byte-64-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-unsigned-byte-64-unscaled) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'ir:unbox-unsigned-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list `(:object ,object 0 ,unboxed-index 1) temp)
                         :inputs (list object unboxed-index temp)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-signed-byte-8-unscaled ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:movsx8
                         :operands (list :eax `(:object ,object 0 ,unboxed-index 1))
                         :inputs (list object unboxed-index)
                         :outputs (list :rax)
                         :clobbers '(:rax)))
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-signed-byte-8-unscaled) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov8
                         :operands (list `(:object ,object 0 ,unboxed-index 1) :al)
                         :inputs (list object unboxed-index :rax)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-signed-byte-16-unscaled ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:movsx16
                         :operands (list :eax `(:object ,object 0 ,unboxed-index 1))
                         :inputs (list object unboxed-index)
                         :outputs (list :rax)
                         :clobbers '(:rax)))
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-signed-byte-16-unscaled) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov16
                         :operands (list `(:object ,object 0 ,unboxed-index 1) :ax)
                         :inputs (list object unboxed-index :rax)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-signed-byte-32-unscaled ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:movsx32
                         :operands (list temp `(:object ,object 0 ,unboxed-index 1))
                         :inputs (list object unboxed-index)
                         :outputs (list temp)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-signed-byte-32-unscaled) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:move-instruction
                         :source temp
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov32
                         :operands (list `(:object ,object 0 ,unboxed-index 1) :eax)
                         :inputs (list object unboxed-index :rax)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-signed-byte-64-unscaled ((object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list temp `(:object ,object 0 ,unboxed-index 1))
                         :inputs (list object unboxed-index)
                         :outputs (list temp)))
    (emit (make-instance 'ir:box-signed-byte-64-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-signed-byte-64-unscaled) ((value object index) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source index
                         :destination unboxed-index))
    (emit (make-instance 'ir:unbox-signed-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list `(:object ,object 0 ,unboxed-index 1) temp)
                         :inputs (list object unboxed-index temp)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

;;; Atomic operations.
;;; These functions index into the object like %OBJECT-REF-T.
;;; There are no atomic functions that access memory like MEMREF.

;; Add DELTA to the slot at SLOT in OBJECT.
;; Returns the old value of the slot.
;; DELTA and the value of the slot must both be fixnums.
;; (defun fixnum-add (object slot delta)
;;   (prog1 (%object-ref-t object slot)
;;     (incf (%object-ref-t object slot) delta)))
(define-builtin sys.int::%atomic-fixnum-add-object ((object offset delta) result)
  (emit (make-instance 'x86-atomic-instruction
                       :opcode 'lap:xadd64
                       :object object
                       :index (if (constant-value-p offset '(signed-byte 29))
                                  (fetch-constant-value offset)
                                  offset)
                       :rhs delta
                       :result result
                       :prefix '(lap:lock))))

;; Set the value in SLOT to NEW, and return the old value.
;; (defun xchg (object slot new)
;;   (prog1 (%object-ref-t object slot)
;;     (setf (%object-ref-t object slot) new)))
(define-builtin sys.int::%xchg-object ((object offset new) result)
  (emit (make-instance 'x86-atomic-instruction
                       :opcode 'lap:xchg64
                       :object object
                       :index (if (constant-value-p offset '(signed-byte 29))
                                  (fetch-constant-value offset)
                                  offset)
                       :rhs new
                       :result result)))

;; If the value in SLOT matches OLD, set it to NEW; otherwise do nothing.
;; Returns true as the primary value if the slot was modified, false otherwise.
;; Additionally returns the old value of SLOT as the second value.
;; (defun cas (object offset old new)
;;   (let ((slot-value (%object-ref-t object slot)))
;;     (values (cond ((eq slot-value old)
;;                    (setf (%object-ref-t object slot) new)
;;                    t)
;;                   (t nil))
;;             slot-value)))
(define-builtin sys.int::%cas-object ((object offset old new) (:z result))
  (emit (make-instance 'x86-cmpxchg-instruction
                       :object object
                       :index (if (constant-value-p offset '(signed-byte 29))
                                  (fetch-constant-value offset)
                                  offset)
                       :old old
                       :new new
                       :result result
                       :prefix '(lap:lock))))

;; Similar to %CAS-OBJECT, but performs two CAS operations on adjacent slots.
;; Returns the two old slot values and a success boolean.
;; (defun dcas (object offset old-1 old-2 new-1 new-2)
;;   (let ((slot-value-1 (%object-ref-t object slot))
;;         (slot-value-2 (%object-ref-t object (1+ slot))))
;;     (values (cond ((and (eq slot-value-1 old-1)
;;                         (eq slot-value-2 old-2))
;;                    (setf (%object-ref-t object slot) new-1
;;                          (%object-ref-t object (1+ slot)) new-2)
;;                    t)
;;                   (t nil))
;;             slot-value-1
;;             slot-value-2)))
(define-builtin sys.int::%dcas-object ((object offset old-1 old-2 new-1 new-2) (:z result-1 result-2))
  (emit (make-instance 'x86-cmpxchg16b-instruction
                       :object object
                       :index (if (constant-value-p offset '(signed-byte 29))
                                  (fetch-constant-value offset)
                                  offset)
                       :old-1 old-1
                       :old-2 old-2
                       :new-1 new-1
                       :new-2 new-2
                       :result-1 result-1
                       :result-2 result-2
                       :prefix '(lap:lock))))
