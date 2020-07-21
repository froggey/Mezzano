;;;; Builtins wrapping x86-64 SIMD instructions

(in-package :mezzano.compiler.backend.x86-64)

;;; MMX operations.

(define-builtin mezzano.simd::%make-mmx-vector ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-unsigned-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-mmx-vector-instruction
                         :source temp
                         :destination result))))

(define-builtin mezzano.simd::%make-mmx-vector/fixnum ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-mmx-vector-instruction
                         :source temp
                         :destination result))))

(define-builtin mezzano.simd::%mmx-vector-value ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-mmx-vector-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:box-unsigned-byte-64-instruction
                         :source temp
                         :destination result))))

(define-builtin mezzano.simd::%mmx-vector-value/fixnum ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'unbox-mmx-vector-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(macrolet ((frob (fn inst)
             `(define-builtin ,fn ((lhs rhs) result)
                (cond ((constant-value-p rhs 'mezzano.simd:mmx-vector)
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :mmx))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :mmx)))
                         (emit (make-instance 'unbox-mmx-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs `(:literal ,(mezzano.simd:mmx-vector-value (fetch-constant-value rhs)))))
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
                (cond ((constant-value-p rhs 'mezzano.simd:mmx-vector)
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :mmx))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :mmx)))
                         (emit (make-instance 'unbox-mmx-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs `(:literal ,(mezzano.simd:mmx-vector-value (fetch-constant-value rhs)))))
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
  (frob mezzano.simd::%packssdw/mmx lap:packssdw)
  (frob mezzano.simd::%packsswb/mmx lap:packsswb)
  (frob mezzano.simd::%packuswb/mmx lap:packuswb)
  (frob mezzano.simd::%paddb/mmx lap:paddb)
  (frob mezzano.simd::%paddw/mmx lap:paddw)
  (frob mezzano.simd::%paddd/mmx lap:paddd)
  (frob mezzano.simd::%paddsb/mmx lap:paddsb)
  (frob mezzano.simd::%paddsw/mmx lap:paddsw)
  (frob mezzano.simd::%paddusb/mmx lap:paddusb)
  (frob mezzano.simd::%paddusw/mmx lap:paddusw)
  (frob mezzano.simd::%pand/mmx lap:pand)
  (frob mezzano.simd::%pandn/mmx lap:pandn)
  (frob mezzano.simd::%pcmpeqb/mmx lap:pcmpeqb)
  (frob mezzano.simd::%pcmpeqw/mmx lap:pcmpeqw)
  (frob mezzano.simd::%pcmpeqd/mmx lap:pcmpeqd)
  (frob mezzano.simd::%pcmpgtb/mmx lap:pcmpgtb)
  (frob mezzano.simd::%pcmpgtw/mmx lap:pcmpgtw)
  (frob mezzano.simd::%pcmpgtd/mmx lap:pcmpgtd)
  (frob mezzano.simd::%pmaddwd/mmx lap:pmaddwd)
  (frob mezzano.simd::%pmulhuw/mmx lap:pmulhuw)
  (frob mezzano.simd::%pmulhw/mmx lap:pmulhw)
  (frob mezzano.simd::%pmullw/mmx lap:pmullw)
  (frob mezzano.simd::%por/mmx lap:por)
  (defshift mezzano.simd::%psllw/mmx lap:psllw)
  (defshift mezzano.simd::%pslld/mmx lap:pslld)
  (defshift mezzano.simd::%psllq/mmx lap:psllq)
  (defshift mezzano.simd::%psraw/mmx lap:psraw)
  (defshift mezzano.simd::%psrad/mmx lap:psrad)
  (defshift mezzano.simd::%psrlw/mmx lap:psrlw)
  (defshift mezzano.simd::%psrld/mmx lap:psrld)
  (defshift mezzano.simd::%psrlq/mmx lap:psrlq)
  (frob mezzano.simd::%psubb/mmx lap:psubb)
  (frob mezzano.simd::%psubw/mmx lap:psubw)
  (frob mezzano.simd::%psubd/mmx lap:psubd)
  (frob mezzano.simd::%psubsb/mmx lap:psubsb)
  (frob mezzano.simd::%psubsw/mmx lap:psubsw)
  (frob mezzano.simd::%psubusb/mmx lap:psubusb)
  (frob mezzano.simd::%psubusw/mmx lap:psubusw)
  (frob mezzano.simd::%punpckhbw/mmx lap:punpckhbw)
  (frob mezzano.simd::%punpckhwd/mmx lap:punpckhwd)
  (frob mezzano.simd::%punpckhdq/mmx lap:punpckhdq)
  (frob mezzano.simd::%punpcklbw/mmx lap:punpcklbw)
  (frob mezzano.simd::%punpcklwd/mmx lap:punpcklwd)
  (frob mezzano.simd::%punpckldq/mmx lap:punpckldq)
  (frob mezzano.simd::%pxor/mmx lap:pxor)

  ;; SSE1
  (frob mezzano.simd::%pavgb/mmx lap:pavgb)
  (frob mezzano.simd::%pavgw/mmx lap:pavgw)
  (frob mezzano.simd::%pmaxsw/mmx lap:pmaxsw)
  (frob mezzano.simd::%pmaxub/mmx lap:pmaxub)
  (frob mezzano.simd::%pminsw/mmx lap:pminsw)
  (frob mezzano.simd::%pminub/mmx lap:pminub)
  (frob mezzano.simd::%psadbw/mmx lap:psadbw)

  ;; SSE2
  (frob mezzano.simd::%paddq/mmx lap:paddq)
  (frob mezzano.simd::%pmuludq/mmx lap:pmuludq)
  (frob mezzano.simd::%psubq/mmx lap:psubq)
  )

(define-builtin mezzano.simd::%pshufw/mmx ((a b control) result :has-wrapper nil)
  (when (not (constant-value-p control '(unsigned-byte 8)))
    (give-up))
  (cond ((constant-value-p b 'mezzano.simd:mmx-vector)
         (let ((a-unboxed (make-instance 'ir:virtual-register :kind :mmx))
               (result-unboxed (make-instance 'ir:virtual-register :kind :mmx)))
           (emit (make-instance 'unbox-mmx-vector-instruction
                                :source a
                                :destination a-unboxed))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:pshufw
                                :result result-unboxed
                                :lhs a-unboxed
                                :rhs `(:literal/64 ,(mezzano.simd:mmx-vector-value (fetch-constant-value b)))
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

(define-builtin mezzano.simd::%pmovmskb/mmx ((value) result)
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

(define-builtin mezzano.simd::%pextrw/mmx ((value control) result :has-wrapper nil)
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

(define-builtin mezzano.simd::%pinsrw/mmx ((a b control) result :has-wrapper nil)
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

(define-builtin mezzano.simd::%make-sse-vector/fixnum ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-sse-vector-instruction
                         :source temp
                         :destination result))))

(define-builtin mezzano.simd::%make-sse-vector/ub64 ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-unsigned-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-sse-vector-instruction
                         :source temp
                         :destination result))))

(define-builtin mezzano.simd::%sse-vector-value/fixnum ((value) result)
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

(define-builtin mezzano.simd::%sse-vector-value/ub64 ((value) result)
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

(define-builtin mezzano.simd::%sse-vector-to-single-float ((value) result)
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

(define-builtin mezzano.simd::%single-float-to-sse-vector ((value) result)
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

(define-builtin mezzano.simd::%sse-vector-to-double-float ((value) result)
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

(define-builtin mezzano.simd::%double-float-to-sse-vector ((value) result)
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

(define-builtin mezzano.simd::%%object-ref-sse-vector/32-unscaled ((object index) result)
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
                                :operands (list result-unboxed `(:object-unscaled ,object 0 ,index-unboxed))
                                :inputs (list object index-unboxed)
                                :outputs (list result-unboxed)))))
    (emit (make-instance 'box-sse-vector-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin (setf mezzano.simd::%%object-ref-sse-vector/32-unscaled) ((value object index) result)
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
                                :operands (list `(:object-unscaled ,object 0 ,index-unboxed) value-unboxed)
                                :inputs (list object index-unboxed value-unboxed)
                                :outputs (list)))))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin mezzano.simd::%%object-ref-sse-vector/64-unscaled ((object index) result)
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
                                :operands (list result-unboxed `(:object-unscaled ,object 0 ,index-unboxed))
                                :inputs (list object index-unboxed)
                                :outputs (list result-unboxed)))))
    (emit (make-instance 'box-sse-vector-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin (setf mezzano.simd::%%object-ref-sse-vector/64-unscaled) ((value object index) result)
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
                                :operands (list `(:object-unscaled ,object 0 ,index-unboxed) value-unboxed)
                                :inputs (list object index-unboxed value-unboxed)
                                :outputs (list)))))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin mezzano.simd::%%object-ref-sse-vector/128-unscaled ((object index) result)
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
                                :operands (list result-unboxed `(:object-unscaled ,object 0 ,index-unboxed))
                                :inputs (list object index-unboxed)
                                :outputs (list result-unboxed)))))
    (emit (make-instance 'box-sse-vector-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin (setf mezzano.simd::%%object-ref-sse-vector/128-unscaled) ((value object index) result)
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
                                :operands (list `(:object-unscaled ,object 0 ,index-unboxed) value-unboxed)
                                :inputs (list object index-unboxed value-unboxed)
                                :outputs (list)))))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(macrolet ((def1 (fn inst)
             `(define-builtin ,fn ((value) result)
                (cond ((constant-value-p value 'mezzano.simd:sse-vector)
                       (let ((result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'x86-instruction
                                              :opcode ',inst
                                              :operands (list result-unboxed `(:literal/128 ,(mezzano.simd:sse-vector-value (fetch-constant-value value))))
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
                            (constant-value-p rhs 'mezzano.simd:sse-vector))
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs `(:literal/128 ,(mezzano.simd:sse-vector-value (fetch-constant-value rhs)))))
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
                (cond ((constant-value-p rhs 'mezzano.simd:sse-vector)
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',inst
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs `(:literal/128 ,(mezzano.simd:sse-vector-value (fetch-constant-value rhs)))))
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
  (def2 mezzano.simd::%packssdw/sse lap:packssdw)
  (def2 mezzano.simd::%packsswb/sse lap:packsswb)
  (def2 mezzano.simd::%packuswb/sse lap:packuswb)
  (def2 mezzano.simd::%paddb/sse lap:paddb)
  (def2 mezzano.simd::%paddw/sse lap:paddw)
  (def2 mezzano.simd::%paddd/sse lap:paddd)
  (def2 mezzano.simd::%paddsb/sse lap:paddsb)
  (def2 mezzano.simd::%paddsw/sse lap:paddsw)
  (def2 mezzano.simd::%paddusb/sse lap:paddusb)
  (def2 mezzano.simd::%paddusw/sse lap:paddusw)
  (def2 mezzano.simd::%pand/sse lap:pand)
  (def2 mezzano.simd::%pandn/sse lap:pandn)
  (def2 mezzano.simd::%pcmpeqb/sse lap:pcmpeqb)
  (def2 mezzano.simd::%pcmpeqw/sse lap:pcmpeqw)
  (def2 mezzano.simd::%pcmpeqd/sse lap:pcmpeqd)
  (def2 mezzano.simd::%pcmpgtb/sse lap:pcmpgtb)
  (def2 mezzano.simd::%pcmpgtw/sse lap:pcmpgtw)
  (def2 mezzano.simd::%pcmpgtd/sse lap:pcmpgtd)
  (def2 mezzano.simd::%pmaddwd/sse lap:pmaddwd)
  (def2 mezzano.simd::%pmulhuw/sse lap:pmulhuw)
  (def2 mezzano.simd::%pmulhw/sse lap:pmulhw)
  (def2 mezzano.simd::%pmullw/sse lap:pmullw)
  (def2 mezzano.simd::%por/sse lap:por)
  (defshift mezzano.simd::%psllw/sse lap:psllw)
  (defshift mezzano.simd::%pslld/sse lap:pslld)
  (defshift mezzano.simd::%psllq/sse lap:psllq)
  (defshift mezzano.simd::%psraw/sse lap:psraw)
  (defshift mezzano.simd::%psrad/sse lap:psrad)
  (defshift mezzano.simd::%psrlw/sse lap:psrlw)
  (defshift mezzano.simd::%psrld/sse lap:psrld)
  (defshift mezzano.simd::%psrlq/sse lap:psrlq)
  (def2 mezzano.simd::%psubb/sse lap:psubb)
  (def2 mezzano.simd::%psubw/sse lap:psubw)
  (def2 mezzano.simd::%psubd/sse lap:psubd)
  (def2 mezzano.simd::%psubsb/sse lap:psubsb)
  (def2 mezzano.simd::%psubsw/sse lap:psubsw)
  (def2 mezzano.simd::%psubusb/sse lap:psubusb)
  (def2 mezzano.simd::%psubusw/sse lap:psubusw)
  (def2 mezzano.simd::%punpckhbw/sse lap:punpckhbw)
  (def2 mezzano.simd::%punpckhwd/sse lap:punpckhwd)
  (def2 mezzano.simd::%punpckhdq/sse lap:punpckhdq)
  (def2 mezzano.simd::%punpcklbw/sse lap:punpcklbw)
  (def2 mezzano.simd::%punpcklwd/sse lap:punpcklwd)
  (def2 mezzano.simd::%punpckldq/sse lap:punpckldq)
  (def2 mezzano.simd::%pxor/sse lap:pxor)

  ;; SSE1
  (def2 mezzano.simd::%pavgb/sse lap:pavgb)
  (def2 mezzano.simd::%pavgw/sse lap:pavgw)
  (def2 mezzano.simd::%pmaxsw/sse lap:pmaxsw)
  (def2 mezzano.simd::%pmaxub/sse lap:pmaxub)
  (def2 mezzano.simd::%pminsw/sse lap:pminsw)
  (def2 mezzano.simd::%pminub/sse lap:pminub)
  (def2 mezzano.simd::%psadbw/sse lap:psadbw)

  (def2 mezzano.simd::%addps/sse lap:addps)
  (def2 mezzano.simd::%addss/sse lap:addss)
  (def2 mezzano.simd::%andnps/sse lap:andnps)
  (def2 mezzano.simd::%andps/sse lap:andps)
  (def2 mezzano.simd::%divps/sse lap:divps)
  (def2 mezzano.simd::%divss/sse lap:divss)
  (def2 mezzano.simd::%maxps/sse lap:maxps)
  (def2 mezzano.simd::%maxss/sse lap:maxss)
  (def2 mezzano.simd::%minps/sse lap:minps)
  (def2 mezzano.simd::%minss/sse lap:minss)
  (def2 mezzano.simd::%movhlps/sse lap:movhlps)
  (def2 mezzano.simd::%movlhps/sse lap:movlhps)
  (def1 mezzano.simd::%movq/sse lap:movq)
  (def2 mezzano.simd::%movss/sse lap:movss :allow-memory-operand nil)
  (def2 mezzano.simd::%mulps/sse lap:mulps)
  (def2 mezzano.simd::%mulss/sse lap:mulss)
  (def2 mezzano.simd::%orps/sse lap:orps)
  (def1 mezzano.simd::%rcpps/sse lap:rcpps)
  (def1 mezzano.simd::%rcpss/sse lap:rcpss)
  (def1 mezzano.simd::%rsqrtps/sse lap:rsqrtps)
  (def1 mezzano.simd::%rsqrtss/sse lap:rsqrtss)
  (def1 mezzano.simd::%sqrtps/sse lap:sqrtps)
  (def1 mezzano.simd::%sqrtss/sse lap:sqrtss)
  (def2 mezzano.simd::%subps/sse lap:subps)
  (def2 mezzano.simd::%subss/sse lap:subss)
  (def2 mezzano.simd::%unpckhps/sse lap:unpckhps)
  (def2 mezzano.simd::%unpcklps/sse lap:unpcklps)
  (def2 mezzano.simd::%xorps/sse lap:xorps)

  (def2 mezzano.simd::%cmpeqss/sse lap:cmpeqss)
  (def2 mezzano.simd::%cmpeqps/sse lap:cmpeqps)
  (def2 mezzano.simd::%cmpltss/sse lap:cmpltss)
  (def2 mezzano.simd::%cmpltps/sse lap:cmpltps)
  (def2 mezzano.simd::%cmpless/sse lap:cmpless)
  (def2 mezzano.simd::%cmpleps/sse lap:cmpleps)
  (def2 mezzano.simd::%cmpunordss/sse lap:cmpunordss)
  (def2 mezzano.simd::%cmpunordps/sse lap:cmpunordps)
  (def2 mezzano.simd::%cmpnewss/sse lap:cmpnewss)
  (def2 mezzano.simd::%cmpnewps/sse lap:cmpnewps)
  (def2 mezzano.simd::%cmpnltss/sse lap:cmpnltss)
  (def2 mezzano.simd::%cmpnltps/sse lap:cmpnltps)
  (def2 mezzano.simd::%cmpnless/sse lap:cmpnless)
  (def2 mezzano.simd::%cmpnleps/sse lap:cmpnleps)
  (def2 mezzano.simd::%cmpordss/sse lap:cmpordss)
  (def2 mezzano.simd::%cmpordps/sse lap:cmpordps)

  ;; SSE2
  (def2 mezzano.simd::%paddq/sse lap:paddq)
  (def2 mezzano.simd::%pmuludq/sse lap:pmuludq)
  (def2 mezzano.simd::%psubq/sse lap:psubq)
  (def2 mezzano.simd::%punpckhqdq/sse lap:punpckhqdq)
  (def2 mezzano.simd::%punpcklqdq/sse lap:punpcklqdq)
  (defshift2 mezzano.simd::%pslldq/sse lap:pslldq)
  (defshift2 mezzano.simd::%psrldq/sse lap:psrldq)

  (def2 mezzano.simd::%addpd/sse lap:addpd)
  (def2 mezzano.simd::%addsd/sse lap:addsd)
  (def2 mezzano.simd::%andnpd/sse lap:andnpd)
  (def1 mezzano.simd::%cvtpd2ps/sse lap:cvtpd2ps)
  (def1 mezzano.simd::%cvtps2pd/sse lap:cvtps2pd)
  (def2 mezzano.simd::%cvtsd2ss/sse lap:cvtsd2ss)
  (def2 mezzano.simd::%cvtss2sd/sse lap:cvtss2sd)
  (def1 mezzano.simd::%cvtpd2dq/sse lap:cvtpd2dq)
  (def1 mezzano.simd::%cvttpd2dq/sse lap:cvttpd2dq)
  (def1 mezzano.simd::%cvtdq2pd/sse lap:cvtdq2pd)
  (def1 mezzano.simd::%cvtps2dq/sse lap:cvtps2dq)
  (def1 mezzano.simd::%cvttps2dq/sse lap:cvttps2dq)
  (def1 mezzano.simd::%cvtdq2ps/sse lap:cvtdq2ps)
  (def2 mezzano.simd::%divpd/sse lap:divpd)
  (def2 mezzano.simd::%divsd/sse lap:divsd)
  (def2 mezzano.simd::%maxpd/sse lap:maxpd)
  (def2 mezzano.simd::%maxsd/sse lap:maxsd)
  (def2 mezzano.simd::%minpd/sse lap:minpd)
  (def2 mezzano.simd::%minsd/sse lap:minsd)
  (def2 mezzano.simd::%movsd/sse lap:movsd :allow-memory-operand nil)
  (def2 mezzano.simd::%mulpd/sse lap:mulpd)
  (def2 mezzano.simd::%mulsd/sse lap:mulsd)
  (def2 mezzano.simd::%orpd/sse lap:orpd)
  (def1 mezzano.simd::%sqrtpd/sse lap:sqrtpd)
  (def1 mezzano.simd::%sqrtsd/sse lap:sqrtsd)
  (def2 mezzano.simd::%subpd/sse lap:subpd)
  (def2 mezzano.simd::%subsd/sse lap:subsd)
  (def2 mezzano.simd::%unpckhpd/sse lap:unpckhpd)
  (def2 mezzano.simd::%unpcklpd/sse lap:unpcklpd)
  (def2 mezzano.simd::%xorpd/sse lap:xorpd)

  (def2 mezzano.simd::%cmpeqsd/sse lap:cmpeqsd)
  (def2 mezzano.simd::%cmpeqpd/sse lap:cmpeqpd)
  (def2 mezzano.simd::%cmpltsd/sse lap:cmpltsd)
  (def2 mezzano.simd::%cmpltpd/sse lap:cmpltpd)
  (def2 mezzano.simd::%cmplesd/sse lap:cmplesd)
  (def2 mezzano.simd::%cmplepd/sse lap:cmplepd)
  (def2 mezzano.simd::%cmpunordsd/sse lap:cmpunordsd)
  (def2 mezzano.simd::%cmpunordpd/sse lap:cmpunordpd)
  (def2 mezzano.simd::%cmpnewsd/sse lap:cmpnewsd)
  (def2 mezzano.simd::%cmpnewpd/sse lap:cmpnewpd)
  (def2 mezzano.simd::%cmpnltsd/sse lap:cmpnltsd)
  (def2 mezzano.simd::%cmpnltpd/sse lap:cmpnltpd)
  (def2 mezzano.simd::%cmpnlesd/sse lap:cmpnlesd)
  (def2 mezzano.simd::%cmpnlepd/sse lap:cmpnlepd)
  (def2 mezzano.simd::%cmpordsd/sse lap:cmpordsd)
  (def2 mezzano.simd::%cmpordpd/sse lap:cmpordpd)
  )

;;; Comparisons.
;;; These match the intrinsics as there's no sensible way to represent a flags result.
(macrolet ((def (fn inst result)
             `(define-builtin ,fn ((lhs rhs) ,result)
                (cond ((constant-value-p rhs 'mezzano.simd:sse-vector)
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-instruction
                                              :opcode ',inst
                                              :operands (list lhs-unboxed `(:literal/128 ,(mezzano.simd:sse-vector-value (fetch-constant-value rhs))))
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
  (def mezzano.simd::%comieqss/sse lap:comiss :e)
  (def mezzano.simd::%comigtss/sse lap:comiss :be)
  (def mezzano.simd::%comigess/sse lap:comiss :b)
  (def mezzano.simd::%comiless/sse lap:comiss :a)
  (def mezzano.simd::%comiltss/sse lap:comiss :ae)
  (def mezzano.simd::%comineqss/sse lap:comiss :ne)
  (def mezzano.simd::%comieqsd/sse lap:comisd :e)
  (def mezzano.simd::%comigtsd/sse lap:comisd :be)
  (def mezzano.simd::%comigesd/sse lap:comisd :b)
  (def mezzano.simd::%comilesd/sse lap:comisd :a)
  (def mezzano.simd::%comiltsd/sse lap:comisd :ae)
  (def mezzano.simd::%comineqsd/sse lap:comisd :ne)
  (def mezzano.simd::%ucomieqss/sse lap:ucomiss :e)
  (def mezzano.simd::%ucomigtss/sse lap:ucomiss :be)
  (def mezzano.simd::%ucomigess/sse lap:ucomiss :b)
  (def mezzano.simd::%ucomiless/sse lap:ucomiss :a)
  (def mezzano.simd::%ucomiltss/sse lap:ucomiss :ae)
  (def mezzano.simd::%ucomineqss/sse lap:ucomiss :ne)
  (def mezzano.simd::%ucomieqsd/sse lap:ucomisd :e)
  (def mezzano.simd::%ucomigtsd/sse lap:ucomisd :be)
  (def mezzano.simd::%ucomigesd/sse lap:ucomisd :b)
  (def mezzano.simd::%ucomilesd/sse lap:ucomisd :a)
  (def mezzano.simd::%ucomiltsd/sse lap:ucomisd :ae)
  (def mezzano.simd::%ucomineqsd/sse lap:ucomisd :ne))

;; Shuffles
(macrolet ((def (name op)
             `(define-builtin ,name ((a b control) result :has-wrapper nil)
                (when (not (constant-value-p control '(unsigned-byte 8)))
                  (give-up))
                (cond ((constant-value-p b 'mezzano.simd:sse-vector)
                       (let ((a-unboxed (make-instance 'ir:virtual-register :kind :sse))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :sse)))
                         (emit (make-instance 'unbox-sse-vector-instruction
                                              :source a
                                              :destination a-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',op
                                              :result result-unboxed
                                              :lhs a-unboxed
                                              :rhs `(:literal/128 ,(mezzano.simd:sse-vector-value (fetch-constant-value b)))
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
  (def mezzano.simd::%shufps/sse lap:shufps)
  (def mezzano.simd::%shufpd/sse lap:shufpd)
  (def mezzano.simd::%pshufd/sse lap:pshufd)
  (def mezzano.simd::%pshufhw/sse lap:pshufhw)
  (def mezzano.simd::%pshuflw/sse lap:pshuflw))

(define-builtin mezzano.simd::%pmovmskb/sse ((value) result)
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

(define-builtin mezzano.simd::%pmovmskps/sse ((value) result)
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

(define-builtin mezzano.simd::%pmovmskpd/sse ((value) result)
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

(define-builtin mezzano.simd::%pextrw/sse ((value control) result :has-wrapper nil)
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

(define-builtin mezzano.simd::%pinsrw/sse ((a b control) result :has-wrapper nil)
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
