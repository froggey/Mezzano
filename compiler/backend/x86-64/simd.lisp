;;;; Builtins wrapping x86-64 SIMD instructions

(in-package :mezzano.compiler.backend.x86-64)

;;; MMX operations.

(define-builtin simd::%make-mmx-vector ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-unsigned-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-mmx-vector-instruction
                         :source temp
                         :destination result))))

(define-builtin simd::%make-mmx-vector/fixnum ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-mmx-vector-instruction
                         :source temp
                         :destination result))))

(define-builtin simd::%mmx-vector-value ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-mmx-vector-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:box-unsigned-byte-64-instruction
                         :source temp
                         :destination result))))

(define-builtin simd::%mmx-vector-value/fixnum ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-mmx-vector-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(macrolet ((frob (fn inst)
             `(define-builtin ,fn ((lhs rhs) result)
                (cond ((constant-value-p rhs 'simd:mmx-vector)
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :mmx))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :mmx)))
                         (emit (make-instance 'unbox-mmx-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs `(:literal ,(simd:mmx-vector-value (fetch-constant-value rhs)))))
                         (emit (make-instance 'box-mmx-vector-instruction
                                              :source result-unboxed
                                              :destination result))))
                      (t
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :mmx))
                             (rhs-unboxed (make-instance 'ir:virtual-register :kind :mmx))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :mmx)))
                         (emit (make-instance 'unbox-mmx-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'unbox-mmx-vector-instruction
                                              :source rhs
                                              :destination rhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs rhs-unboxed))
                         (emit (make-instance 'box-mmx-vector-instruction
                                              :source result-unboxed
                                              :destination result)))))))
           (defshift (fn inst)
             `(define-builtin ,fn ((lhs rhs) result)
                (cond ((constant-value-p rhs 'simd:mmx-vector)
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :mmx))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :mmx)))
                         (emit (make-instance 'unbox-mmx-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs `(:literal ,(simd:mmx-vector-value (fetch-constant-value rhs)))))
                         (emit (make-instance 'box-mmx-vector-instruction
                                              :source result-unboxed
                                              :destination result))))
                      ((constant-value-p rhs '(unsigned-byte 8))
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :mmx))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :mmx)))
                         (emit (make-instance 'unbox-mmx-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs (fetch-constant-value rhs)))
                         (emit (make-instance 'box-mmx-vector-instruction
                                              :source result-unboxed
                                              :destination result))))
                      (t
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :mmx))
                             (rhs-unboxed (make-instance 'ir:virtual-register :kind :mmx))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :mmx)))
                         (emit (make-instance 'unbox-mmx-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'unbox-mmx-vector-instruction
                                              :source rhs
                                              :destination rhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs rhs-unboxed))
                         (emit (make-instance 'box-mmx-vector-instruction
                                              :source result-unboxed
                                              :destination result))))))))
  ;; MMX
  (frob simd::%packssdw/mmx lap:packssdw)
  (frob simd::%packsswb/mmx lap:packsswb)
  (frob simd::%packuswb/mmx lap:packuswb)
  (frob simd::%paddb/mmx lap:paddb)
  (frob simd::%paddw/mmx lap:paddw)
  (frob simd::%paddd/mmx lap:paddd)
  (frob simd::%paddsb/mmx lap:paddsb)
  (frob simd::%paddsw/mmx lap:paddsw)
  (frob simd::%paddusb/mmx lap:paddusb)
  (frob simd::%paddusw/mmx lap:paddusw)
  (frob simd::%pand/mmx lap:pand)
  (frob simd::%pandn/mmx lap:pandn)
  (frob simd::%pcmpeqb/mmx lap:pcmpeqb)
  (frob simd::%pcmpeqw/mmx lap:pcmpeqw)
  (frob simd::%pcmpeqd/mmx lap:pcmpeqd)
  (frob simd::%pcmpgtb/mmx lap:pcmpgtb)
  (frob simd::%pcmpgtw/mmx lap:pcmpgtw)
  (frob simd::%pcmpgtd/mmx lap:pcmpgtd)
  (frob simd::%pmaddwd/mmx lap:pmaddwd)
  (frob simd::%pmulhuw/mmx lap:pmulhuw)
  (frob simd::%pmulhw/mmx lap:pmulhw)
  (frob simd::%pmullw/mmx lap:pmullw)
  (frob simd::%por/mmx lap:por)
  (defshift simd::%psllw/mmx lap:psllw)
  (defshift simd::%pslld/mmx lap:pslld)
  (defshift simd::%psllq/mmx lap:psllq)
  (defshift simd::%psraw/mmx lap:psraw)
  (defshift simd::%psrad/mmx lap:psrad)
  (defshift simd::%psrlw/mmx lap:psrlw)
  (defshift simd::%psrld/mmx lap:psrld)
  (defshift simd::%psrlq/mmx lap:psrlq)
  (frob simd::%psubb/mmx lap:psubb)
  (frob simd::%psubw/mmx lap:psubw)
  (frob simd::%psubd/mmx lap:psubd)
  (frob simd::%psubsb/mmx lap:psubsb)
  (frob simd::%psubsw/mmx lap:psubsw)
  (frob simd::%psubusb/mmx lap:psubusb)
  (frob simd::%psubusw/mmx lap:psubusw)
  (frob simd::%punpckhbw/mmx lap:punpckhbw)
  (frob simd::%punpckhwd/mmx lap:punpckhwd)
  (frob simd::%punpckhdq/mmx lap:punpckhdq)
  (frob simd::%punpcklbw/mmx lap:punpcklbw)
  (frob simd::%punpcklwd/mmx lap:punpcklwd)
  (frob simd::%punpckldq/mmx lap:punpckldq)
  (frob simd::%pxor/mmx lap:pxor)

  ;; SSE1
  (frob simd::%pavgb/mmx lap:pavgb)
  (frob simd::%pavgw/mmx lap:pavgw)
  (frob simd::%pmaxsw/mmx lap:pmaxsw)
  (frob simd::%pmaxub/mmx lap:pmaxub)
  (frob simd::%pminsw/mmx lap:pminsw)
  (frob simd::%pminub/mmx lap:pminub)
  (frob simd::%psadbw/mmx lap:psadbw)

  ;; SSE2
  (frob simd::%paddq/mmx lap:paddq)
  (frob simd::%pmuludq/mmx lap:pmuludq)
  (frob simd::%psubq/mmx lap:psubq)
  )

(define-builtin simd::%pshufw/mmx ((a b control) result :has-wrapper nil)
  (when (not (constant-value-p control '(unsigned-byte 8)))
    (give-up))
  (cond ((constant-value-p b 'simd:mmx-vector)
         (let ((a-unboxed (make-instance 'ir:virtual-register :kind :mmx))
               (result-unboxed (make-instance 'ir:virtual-register :kind :mmx)))
           (emit (make-instance 'unbox-mmx-vector-instruction
                                :source a
                                :destination a-unboxed))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:pshufw
                                :result result-unboxed
                                :lhs a-unboxed
                                :rhs `(:literal/64 ,(simd:mmx-vector-value (fetch-constant-value b)))
                                :imm (fetch-constant-value control)))
           (emit (make-instance 'box-mmx-vector-instruction
                                :source result-unboxed
                                :destination result))))
        (t
         (let ((a-unboxed (make-instance 'ir:virtual-register :kind :mmx))
               (b-unboxed (make-instance 'ir:virtual-register :kind :mmx))
               (result-unboxed (make-instance 'ir:virtual-register :kind :mmx)))
           (emit (make-instance 'unbox-mmx-vector-instruction
                                :source a
                                :destination a-unboxed))
           (emit (make-instance 'unbox-mmx-vector-instruction
                                :source b
                                :destination b-unboxed))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:pshufw
                                :result result-unboxed
                                :lhs a-unboxed
                                :rhs b-unboxed
                                :imm (fetch-constant-value control)))
           (emit (make-instance 'box-mmx-vector-instruction
                                :source result-unboxed
                                :destination result))))))

(define-builtin simd::%pmovmskb/mmx ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :mmx))
        (result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-mmx-vector-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:pmovmskb
                         :operands (list result-unboxed value-unboxed)
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin simd::%pextrw/mmx ((value control) result :has-wrapper nil)
  (when (not (constant-value-p control '(unsigned-byte 8)))
    (give-up))
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :mmx))
        (result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-mmx-vector-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:pextrw
                         :operands (list result-unboxed value-unboxed (fetch-constant-value control))
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin simd::%pinsrw/mmx ((a b control) result :has-wrapper nil)
  (when (not (constant-value-p control '(unsigned-byte 8)))
    (give-up))
  (let ((a-unboxed (make-instance 'ir:virtual-register :kind :mmx))
        (b-unboxed (make-instance 'ir:virtual-register :kind :integer))
        (result-unboxed (make-instance 'ir:virtual-register :kind :mmx)))
    (emit (make-instance 'unbox-mmx-vector-instruction
                         :source a
                         :destination a-unboxed))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source b
                         :destination b-unboxed))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:pinsrw
                         :result result-unboxed
                         :lhs a-unboxed
                         :rhs b-unboxed
                         :imm (fetch-constant-value control)))
    (emit (make-instance 'box-mmx-vector-instruction
                         :source result-unboxed
                         :destination result))))

;;; SSE operations.

(define-builtin simd::%make-sse-vector/fixnum ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-sse-vector-instruction
                         :source temp
                         :destination result))))

(define-builtin simd::%make-sse-vector/ub64 ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-unsigned-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-sse-vector-instruction
                         :source temp
                         :destination result))))

(define-builtin simd::%sse-vector-value/fixnum ((value) result)
  (let ((xmm-temp (make-instance 'ir:virtual-register :kind :sse))
        (temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-sse-vector-instruction
                         :source value
                         :destination xmm-temp))
    (emit (make-instance 'ir:move-instruction
                         :source xmm-temp
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin simd::%sse-vector-value/ub64 ((value) result)
  (let ((xmm-temp (make-instance 'ir:virtual-register :kind :sse))
        (temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-sse-vector-instruction
                         :source value
                         :destination xmm-temp))
    (emit (make-instance 'ir:move-instruction
                         :source xmm-temp
                         :destination temp))
    (emit (make-instance 'ir:box-unsigned-byte-64-instruction
                         :source temp
                         :destination result))))

(define-builtin simd::%sse-vector-to-single-float ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :sse))
        (result-unboxed (make-instance 'ir:virtual-register :kind :single-float)))
    (emit (make-instance 'unbox-sse-vector-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'ir:move-instruction
                         :source value-unboxed
                         :destination result-unboxed))
    (emit (make-instance 'ir:box-single-float-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin simd::%single-float-to-sse-vector ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :single-float))
        (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
    (emit (make-instance 'ir:unbox-single-float-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'ir:move-instruction
                         :source value-unboxed
                         :destination result-unboxed))
    (emit (make-instance 'box-sse-vector-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin simd::%sse-vector-to-double-float ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :sse))
        (result-unboxed (make-instance 'ir:virtual-register :kind :double-float)))
    (emit (make-instance 'unbox-sse-vector-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'ir:move-instruction
                         :source value-unboxed
                         :destination result-unboxed))
    (emit (make-instance 'ir:box-double-float-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin simd::%double-float-to-sse-vector ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :double-float))
        (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
    (emit (make-instance 'ir:unbox-double-float-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'ir:move-instruction
                         :source value-unboxed
                         :destination result-unboxed))
    (emit (make-instance 'box-sse-vector-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin simd::%%object-ref-sse-vector/32-unscaled ((object index) result)
  (let ((result-unboxed (make-instance 'ir:virtual-register :kind :sse))
        (index-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (cond ((constant-value-p index '(signed-byte 32))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movd
                                :operands (list result-unboxed `(:object-unscaled ,object ,(fetch-constant-value index)))
                                :inputs (list object)
                                :outputs (list result-unboxed))))
          (t
           (emit (make-instance 'ir:unbox-fixnum-instruction
                                :destination index-unboxed
                                :source index))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movd
                                :operands (list result-unboxed `(:object ,object 0 ,index-unboxed 1))
                                :inputs (list object index-unboxed)
                                :outputs (list result-unboxed)))))
    (emit (make-instance 'box-sse-vector-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin (setf simd::%%object-ref-sse-vector/32-unscaled) ((value object index) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :sse))
        (index-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-sse-vector-instruction
                         :source value
                         :destination value-unboxed))
    (cond ((constant-value-p index '(signed-byte 32))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movd
                                :operands (list `(:object-unscaled ,object ,(fetch-constant-value index)) value-unboxed)
                                :inputs (list object value-unboxed)
                                :outputs (list))))
          (t
           (emit (make-instance 'ir:unbox-fixnum-instruction
                                :destination index-unboxed
                                :source index))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movd
                                :operands (list `(:object ,object 0 ,index-unboxed 1) value-unboxed)
                                :inputs (list object index-unboxed value-unboxed)
                                :outputs (list)))))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin simd::%%object-ref-sse-vector/64-unscaled ((object index) result)
  (let ((result-unboxed (make-instance 'ir:virtual-register :kind :sse))
        (index-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (cond ((constant-value-p index '(signed-byte 32))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movq
                                :operands (list result-unboxed `(:object-unscaled ,object ,(fetch-constant-value index)))
                                :inputs (list object)
                                :outputs (list result-unboxed))))
          (t
           (emit (make-instance 'ir:unbox-fixnum-instruction
                                :destination index-unboxed
                                :source index))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movq
                                :operands (list result-unboxed `(:object ,object 0 ,index-unboxed 1))
                                :inputs (list object index-unboxed)
                                :outputs (list result-unboxed)))))
    (emit (make-instance 'box-sse-vector-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin (setf simd::%%object-ref-sse-vector/64-unscaled) ((value object index) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :sse))
        (index-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-sse-vector-instruction
                         :source value
                         :destination value-unboxed))
    (cond ((constant-value-p index '(signed-byte 32))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movq
                                :operands (list `(:object-unscaled ,object ,(fetch-constant-value index)) value-unboxed)
                                :inputs (list object value-unboxed)
                                :outputs (list))))
          (t
           (emit (make-instance 'ir:unbox-fixnum-instruction
                                :destination index-unboxed
                                :source index))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movq
                                :operands (list `(:object ,object 0 ,index-unboxed 1) value-unboxed)
                                :inputs (list object index-unboxed value-unboxed)
                                :outputs (list)))))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin simd::%%object-ref-sse-vector/128-unscaled ((object index) result)
  (let ((result-unboxed (make-instance 'ir:virtual-register :kind :sse))
        (index-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (cond ((constant-value-p index '(signed-byte 32))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movdqu
                                :operands (list result-unboxed `(:object-unscaled ,object ,(fetch-constant-value index)))
                                :inputs (list object)
                                :outputs (list result-unboxed))))
          (t
           (emit (make-instance 'ir:unbox-fixnum-instruction
                                :destination index-unboxed
                                :source index))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movdqu
                                :operands (list result-unboxed `(:object ,object 0 ,index-unboxed 1))
                                :inputs (list object index-unboxed)
                                :outputs (list result-unboxed)))))
    (emit (make-instance 'box-sse-vector-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin (setf simd::%%object-ref-sse-vector/128-unscaled) ((value object index) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :sse))
        (index-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-sse-vector-instruction
                         :source value
                         :destination value-unboxed))
    (cond ((constant-value-p index '(signed-byte 32))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movdqu
                                :operands (list `(:object-unscaled ,object ,(fetch-constant-value index)) value-unboxed)
                                :inputs (list object value-unboxed)
                                :outputs (list))))
          (t
           (emit (make-instance 'ir:unbox-fixnum-instruction
                                :destination index-unboxed
                                :source index))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movdqu
                                :operands (list `(:object ,object 0 ,index-unboxed 1) value-unboxed)
                                :inputs (list object index-unboxed value-unboxed)
                                :outputs (list)))))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(macrolet ((def1 (fn inst)
             `(define-builtin ,fn ((value) result)
                (cond ((constant-value-p value 'simd:sse-vector)
                       (let ((result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'x86-instruction
                                              :opcode ',inst
                                              :operands (list result-unboxed `(:literal/128 ,(simd:sse-vector-value (fetch-constant-value value))))
                                              :inputs (list)
                                              :outputs (list result-unboxed)))
                         (emit (make-instance 'box-sse-vector-instruction
                                              :source result-unboxed
                                              :destination result))))
                      (t
                       (let ((value-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source value
                                              :destination value-unboxed))
                         (emit (make-instance 'x86-instruction
                                              :opcode ',inst
                                              :operands (list result-unboxed value-unboxed)
                                              :inputs (list value-unboxed)
                                              :outputs (list result-unboxed)))
                         (emit (make-instance 'box-sse-vector-instruction
                                              :source result-unboxed
                                              :destination result)))))))
           (def2 (fn inst &key (allow-memory-operand t))
             `(define-builtin ,fn ((lhs rhs) result)
                (cond ((and ',allow-memory-operand
                            (constant-value-p rhs 'simd:sse-vector))
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs `(:literal/128 ,(simd:sse-vector-value (fetch-constant-value rhs)))))
                         (emit (make-instance 'box-sse-vector-instruction
                                              :source result-unboxed
                                              :destination result))))
                      (t
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (rhs-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source rhs
                                              :destination rhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs rhs-unboxed))
                         (emit (make-instance 'box-sse-vector-instruction
                                              :source result-unboxed
                                              :destination result)))))))
           (defshift (fn inst)
             `(define-builtin ,fn ((lhs rhs) result)
                (cond ((constant-value-p rhs 'simd:sse-vector)
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs `(:literal/128 ,(simd:sse-vector-value (fetch-constant-value rhs)))))
                         (emit (make-instance 'box-sse-vector-instruction
                                              :source result-unboxed
                                              :destination result))))
                      ((constant-value-p rhs '(unsigned-byte 8))
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs (fetch-constant-value rhs)))
                         (emit (make-instance 'box-sse-vector-instruction
                                              :source result-unboxed
                                              :destination result))))
                      (t
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (rhs-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source rhs
                                              :destination rhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs rhs-unboxed))
                         (emit (make-instance 'box-sse-vector-instruction
                                              :source result-unboxed
                                              :destination result)))))))
           ;; Immediate rhs only
           (defshift2 (fn inst)
             `(define-builtin ,fn ((lhs (:constant shift (typep shift '(unsigned-byte 8)))) result :has-wrapper nil)
                (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :sse))
                      (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                  (emit (make-instance 'unbox-sse-vector-instruction
                                       :source lhs
                                       :destination lhs-unboxed))
                  (emit (make-instance 'x86-fake-three-operand-instruction
                                       :opcode ',inst
                                       :result result-unboxed
                                       :lhs lhs-unboxed
                                       :rhs shift))
                  (emit (make-instance 'box-sse-vector-instruction
                                       :source result-unboxed
                                       :destination result))))))
  ;; MMX
  (def2 simd::%packssdw/sse lap:packssdw)
  (def2 simd::%packsswb/sse lap:packsswb)
  (def2 simd::%packuswb/sse lap:packuswb)
  (def2 simd::%paddb/sse lap:paddb)
  (def2 simd::%paddw/sse lap:paddw)
  (def2 simd::%paddd/sse lap:paddd)
  (def2 simd::%paddsb/sse lap:paddsb)
  (def2 simd::%paddsw/sse lap:paddsw)
  (def2 simd::%paddusb/sse lap:paddusb)
  (def2 simd::%paddusw/sse lap:paddusw)
  (def2 simd::%pand/sse lap:pand)
  (def2 simd::%pandn/sse lap:pandn)
  (def2 simd::%pcmpeqb/sse lap:pcmpeqb)
  (def2 simd::%pcmpeqw/sse lap:pcmpeqw)
  (def2 simd::%pcmpeqd/sse lap:pcmpeqd)
  (def2 simd::%pcmpgtb/sse lap:pcmpgtb)
  (def2 simd::%pcmpgtw/sse lap:pcmpgtw)
  (def2 simd::%pcmpgtd/sse lap:pcmpgtd)
  (def2 simd::%pmaddwd/sse lap:pmaddwd)
  (def2 simd::%pmulhuw/sse lap:pmulhuw)
  (def2 simd::%pmulhw/sse lap:pmulhw)
  (def2 simd::%pmullw/sse lap:pmullw)
  (def2 simd::%por/sse lap:por)
  (defshift simd::%psllw/sse lap:psllw)
  (defshift simd::%pslld/sse lap:pslld)
  (defshift simd::%psllq/sse lap:psllq)
  (defshift simd::%psraw/sse lap:psraw)
  (defshift simd::%psrad/sse lap:psrad)
  (defshift simd::%psrlw/sse lap:psrlw)
  (defshift simd::%psrld/sse lap:psrld)
  (defshift simd::%psrlq/sse lap:psrlq)
  (def2 simd::%psubb/sse lap:psubb)
  (def2 simd::%psubw/sse lap:psubw)
  (def2 simd::%psubd/sse lap:psubd)
  (def2 simd::%psubsb/sse lap:psubsb)
  (def2 simd::%psubsw/sse lap:psubsw)
  (def2 simd::%psubusb/sse lap:psubusb)
  (def2 simd::%psubusw/sse lap:psubusw)
  (def2 simd::%punpckhbw/sse lap:punpckhbw)
  (def2 simd::%punpckhwd/sse lap:punpckhwd)
  (def2 simd::%punpckhdq/sse lap:punpckhdq)
  (def2 simd::%punpcklbw/sse lap:punpcklbw)
  (def2 simd::%punpcklwd/sse lap:punpcklwd)
  (def2 simd::%punpckldq/sse lap:punpckldq)
  (def2 simd::%pxor/sse lap:pxor)

  ;; SSE1
  (def2 simd::%pavgb/sse lap:pavgb)
  (def2 simd::%pavgw/sse lap:pavgw)
  (def2 simd::%pmaxsw/sse lap:pmaxsw)
  (def2 simd::%pmaxub/sse lap:pmaxub)
  (def2 simd::%pminsw/sse lap:pminsw)
  (def2 simd::%pminub/sse lap:pminub)
  (def2 simd::%psadbw/sse lap:psadbw)

  (def2 simd::%addps/sse lap:addps)
  (def2 simd::%addss/sse lap:addss)
  (def2 simd::%andnps/sse lap:andnps)
  (def2 simd::%andps/sse lap:andps)
  (def2 simd::%divps/sse lap:divps)
  (def2 simd::%divss/sse lap:divss)
  (def2 simd::%maxps/sse lap:maxps)
  (def2 simd::%maxss/sse lap:maxss)
  (def2 simd::%minps/sse lap:minps)
  (def2 simd::%minss/sse lap:minss)
  (def2 simd::%movhlps/sse lap:movhlps)
  (def2 simd::%movlhps/sse lap:movlhps)
  (def1 simd::%movq/sse lap:movq)
  (def2 simd::%movss/sse lap:movss :allow-memory-operand nil)
  (def2 simd::%mulps/sse lap:mulps)
  (def2 simd::%mulss/sse lap:mulss)
  (def2 simd::%orps/sse lap:orps)
  (def1 simd::%rcpps/sse lap:rcpps)
  (def1 simd::%rcpss/sse lap:rcpss)
  (def1 simd::%rsqrtps/sse lap:rsqrtps)
  (def1 simd::%rsqrtss/sse lap:rsqrtss)
  (def1 simd::%sqrtps/sse lap:sqrtps)
  (def1 simd::%sqrtss/sse lap:sqrtss)
  (def2 simd::%subps/sse lap:subps)
  (def2 simd::%subss/sse lap:subss)
  (def2 simd::%unpckhps/sse lap:unpckhps)
  (def2 simd::%unpcklps/sse lap:unpcklps)
  (def2 simd::%xorps/sse lap:xorps)

  (def2 simd::%cmpeqss/sse lap:cmpeqss)
  (def2 simd::%cmpeqps/sse lap:cmpeqps)
  (def2 simd::%cmpltss/sse lap:cmpltss)
  (def2 simd::%cmpltps/sse lap:cmpltps)
  (def2 simd::%cmpless/sse lap:cmpless)
  (def2 simd::%cmpleps/sse lap:cmpleps)
  (def2 simd::%cmpunordss/sse lap:cmpunordss)
  (def2 simd::%cmpunordps/sse lap:cmpunordps)
  (def2 simd::%cmpnewss/sse lap:cmpnewss)
  (def2 simd::%cmpnewps/sse lap:cmpnewps)
  (def2 simd::%cmpnltss/sse lap:cmpnltss)
  (def2 simd::%cmpnltps/sse lap:cmpnltps)
  (def2 simd::%cmpnless/sse lap:cmpnless)
  (def2 simd::%cmpnleps/sse lap:cmpnleps)
  (def2 simd::%cmpordss/sse lap:cmpordss)
  (def2 simd::%cmpordps/sse lap:cmpordps)

  ;; SSE2
  (def2 simd::%paddq/sse lap:paddq)
  (def2 simd::%pmuludq/sse lap:pmuludq)
  (def2 simd::%psubq/sse lap:psubq)
  (def2 simd::%punpckhqdq/sse lap:punpckhqdq)
  (def2 simd::%punpcklqdq/sse lap:punpcklqdq)
  (defshift2 simd::%pslldq/sse lap:pslldq)
  (defshift2 simd::%psrldq/sse lap:psrldq)

  (def2 simd::%addpd/sse lap:addpd)
  (def2 simd::%addsd/sse lap:addsd)
  (def2 simd::%andnpd/sse lap:andnpd)
  (def1 simd::%cvtpd2ps/sse lap:cvtpd2ps)
  (def1 simd::%cvtps2pd/sse lap:cvtps2pd)
  (def2 simd::%cvtsd2ss/sse lap:cvtsd2ss)
  (def2 simd::%cvtss2sd/sse lap:cvtss2sd)
  (def1 simd::%cvtpd2dq/sse lap:cvtpd2dq)
  (def1 simd::%cvttpd2dq/sse lap:cvttpd2dq)
  (def1 simd::%cvtdq2pd/sse lap:cvtdq2pd)
  (def1 simd::%cvtps2dq/sse lap:cvtps2dq)
  (def1 simd::%cvttps2dq/sse lap:cvttps2dq)
  (def1 simd::%cvtdq2ps/sse lap:cvtdq2ps)
  (def2 simd::%divpd/sse lap:divpd)
  (def2 simd::%divsd/sse lap:divsd)
  (def2 simd::%maxpd/sse lap:maxpd)
  (def2 simd::%maxsd/sse lap:maxsd)
  (def2 simd::%minpd/sse lap:minpd)
  (def2 simd::%minsd/sse lap:minsd)
  (def2 simd::%movsd/sse lap:movsd :allow-memory-operand nil)
  (def2 simd::%mulpd/sse lap:mulpd)
  (def2 simd::%mulsd/sse lap:mulsd)
  (def2 simd::%orpd/sse lap:orpd)
  (def1 simd::%sqrtpd/sse lap:sqrtpd)
  (def1 simd::%sqrtsd/sse lap:sqrtsd)
  (def2 simd::%subpd/sse lap:subpd)
  (def2 simd::%subsd/sse lap:subsd)
  (def2 simd::%unpckhpd/sse lap:unpckhpd)
  (def2 simd::%unpcklpd/sse lap:unpcklpd)
  (def2 simd::%xorpd/sse lap:xorpd)

  (def2 simd::%cmpeqsd/sse lap:cmpeqsd)
  (def2 simd::%cmpeqpd/sse lap:cmpeqpd)
  (def2 simd::%cmpltsd/sse lap:cmpltsd)
  (def2 simd::%cmpltpd/sse lap:cmpltpd)
  (def2 simd::%cmplesd/sse lap:cmplesd)
  (def2 simd::%cmplepd/sse lap:cmplepd)
  (def2 simd::%cmpunordsd/sse lap:cmpunordsd)
  (def2 simd::%cmpunordpd/sse lap:cmpunordpd)
  (def2 simd::%cmpnewsd/sse lap:cmpnewsd)
  (def2 simd::%cmpnewpd/sse lap:cmpnewpd)
  (def2 simd::%cmpnltsd/sse lap:cmpnltsd)
  (def2 simd::%cmpnltpd/sse lap:cmpnltpd)
  (def2 simd::%cmpnlesd/sse lap:cmpnlesd)
  (def2 simd::%cmpnlepd/sse lap:cmpnlepd)
  (def2 simd::%cmpordsd/sse lap:cmpordsd)
  (def2 simd::%cmpordpd/sse lap:cmpordpd)
  )

;;; Comparisons.
;;; These match the intrinsics as there's no sensible way to represent a flags result.
(macrolet ((def (fn inst result)
             `(define-builtin ,fn ((lhs rhs) ,result)
                (cond ((constant-value-p rhs 'simd:sse-vector)
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-instruction
                                              :opcode ',inst
                                              :operands (list lhs-unboxed `(:literal/128 ,(simd:sse-vector-value (fetch-constant-value rhs))))
                                              :inputs (list lhs-unboxed)
                                              :outputs '()))))
                      (t
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (rhs-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source rhs
                                              :destination rhs-unboxed))
                         (emit (make-instance 'x86-instruction
                                              :opcode ',inst
                                              :operands (list lhs-unboxed rhs-unboxed)
                                              :inputs (list lhs-unboxed rhs-unboxed)
                                              :outputs '()))))))))
  (def simd::%comieqss/sse lap:comiss :e)
  (def simd::%comigtss/sse lap:comiss :be)
  (def simd::%comigess/sse lap:comiss :b)
  (def simd::%comiless/sse lap:comiss :a)
  (def simd::%comiltss/sse lap:comiss :ae)
  (def simd::%comineqss/sse lap:comiss :ne)
  (def simd::%comieqsd/sse lap:comisd :e)
  (def simd::%comigtsd/sse lap:comisd :be)
  (def simd::%comigesd/sse lap:comisd :b)
  (def simd::%comilesd/sse lap:comisd :a)
  (def simd::%comiltsd/sse lap:comisd :ae)
  (def simd::%comineqsd/sse lap:comisd :ne)
  (def simd::%ucomieqss/sse lap:ucomiss :e)
  (def simd::%ucomigtss/sse lap:ucomiss :be)
  (def simd::%ucomigess/sse lap:ucomiss :b)
  (def simd::%ucomiless/sse lap:ucomiss :a)
  (def simd::%ucomiltss/sse lap:ucomiss :ae)
  (def simd::%ucomineqss/sse lap:ucomiss :ne)
  (def simd::%ucomieqsd/sse lap:ucomisd :e)
  (def simd::%ucomigtsd/sse lap:ucomisd :be)
  (def simd::%ucomigesd/sse lap:ucomisd :b)
  (def simd::%ucomilesd/sse lap:ucomisd :a)
  (def simd::%ucomiltsd/sse lap:ucomisd :ae)
  (def simd::%ucomineqsd/sse lap:ucomisd :ne))

;; Shuffles
(macrolet ((def (name op)
             `(define-builtin ,name ((a b control) result :has-wrapper nil)
                (when (not (constant-value-p control '(unsigned-byte 8)))
                  (give-up))
                (cond ((constant-value-p b 'simd:sse-vector)
                       (let ((a-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source a
                                              :destination a-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',op
                                              :result result-unboxed
                                              :lhs a-unboxed
                                              :rhs `(:literal/128 ,(simd:sse-vector-value (fetch-constant-value b)))
                                              :imm (fetch-constant-value control)))
                         (emit (make-instance 'box-sse-vector-instruction
                                              :source result-unboxed
                                              :destination result))))
                      (t
                       (let ((a-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (b-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source a
                                              :destination a-unboxed))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source b
                                              :destination b-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',op
                                              :result result-unboxed
                                              :lhs a-unboxed
                                              :rhs b-unboxed
                                              :imm (fetch-constant-value control)))
                         (emit (make-instance 'box-sse-vector-instruction
                                              :source result-unboxed
                                              :destination result))))))))
  (def simd::%shufps/sse lap:shufps)
  (def simd::%shufpd/sse lap:shufpd)
  (def simd::%pshufd/sse lap:pshufd)
  (def simd::%pshufhw/sse lap:pshufhw)
  (def simd::%pshuflw/sse lap:pshuflw))

(define-builtin simd::%pmovmskb/sse ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :sse))
        (result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-sse-vector-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:pmovmskb
                         :operands (list result-unboxed value-unboxed)
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin simd::%pmovmskps/sse ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :sse))
        (result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-sse-vector-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:pmovmskb
                         :operands (list result-unboxed value-unboxed)
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin simd::%pmovmskpd/sse ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :sse))
        (result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-sse-vector-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:pmovmskb
                         :operands (list result-unboxed value-unboxed)
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin simd::%pextrw/sse ((value control) result :has-wrapper nil)
  (when (not (constant-value-p control '(unsigned-byte 8)))
    (give-up))
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :sse))
        (result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-sse-vector-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:pextrw
                         :operands (list result-unboxed value-unboxed (fetch-constant-value control))
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin simd::%pinsrw/sse ((a b control) result :has-wrapper nil)
  (when (not (constant-value-p control '(unsigned-byte 8)))
    (give-up))
  (let ((a-unboxed (make-instance 'ir:virtual-register :kind :sse))
        (b-unboxed (make-instance 'ir:virtual-register :kind :integer))
        (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
    (emit (make-instance 'unbox-sse-vector-instruction
                         :source a
                         :destination a-unboxed))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source b
                         :destination b-unboxed))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:pinsrw
                         :result result-unboxed
                         :lhs a-unboxed
                         :rhs b-unboxed
                         :imm (fetch-constant-value control)))
    (emit (make-instance 'box-sse-vector-instruction
                         :source result-unboxed
                         :destination result))))
