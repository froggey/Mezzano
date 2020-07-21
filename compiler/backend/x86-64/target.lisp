;;;; x86-64 target definitions

(in-package :mezzano.compiler.backend.x86-64)

(defmethod ra:architectural-physical-registers ((architecture c:x86-64-target))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod ra:target-argument-registers ((target c:x86-64-target))
  '(:r8 :r9 :r10 :r11 :r12))

(defmethod ra:target-return-register ((target c:x86-64-target))
  :r8)

(defmethod ra:target-funcall-register ((target c:x86-64-target))
  :rbx)

(defmethod ra:target-count-register ((target c:x86-64-target))
  :rcx)

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :value)) (architecture c:x86-64-target))
  '(:r8 :r9 :r10 :r11 :r12 :r13 :rbx))

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :integer)) (architecture c:x86-64-target))
  '(:rax :rcx :rdx :rsi :rdi))

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :single-float)) (architecture c:x86-64-target))
  '(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7
    :xmm8 :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :double-float)) (architecture c:x86-64-target))
  '(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7
    :xmm8 :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :mmx)) (architecture c:x86-64-target))
  '(:mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7))

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :sse)) (architecture c:x86-64-target))
  '(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7
    :xmm8 :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod ra:spill/fill-register-kinds-compatible (kind1 kind2 (architecture c:x86-64-target))
  (or (eql kind1 kind2)
      ;; These register kinds are all mutually compatible.
      ;; :sse is not compatible with anything but :see as it is 128 bits wide.
      (and (member kind1 '(:value :integer :single-float :double-float :mmx))
           (member kind2 '(:value :integer :single-float :double-float :mmx)))))

(defmethod ra:instruction-clobbers ((instruction ir::base-call-instruction) (architecture c:x86-64-target))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod ra:instruction-clobbers ((instruction ir:argument-setup-instruction) (architecture c:x86-64-target))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod ra:instruction-clobbers ((instruction ir:save-multiple-instruction) (architecture c:x86-64-target))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod ra:instruction-clobbers ((instruction ir:restore-multiple-instruction) (architecture c:x86-64-target))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod ra:instruction-clobbers ((instruction ir:nlx-entry-instruction) (architecture c:x86-64-target))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod ra:instruction-clobbers ((instruction ir:nlx-entry-multiple-instruction) (architecture c:x86-64-target))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod ra:instruction-clobbers ((instruction ir:values-instruction) (architecture c:x86-64-target))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod ra:instruction-clobbers ((instruction ir:multiple-value-bind-instruction) (architecture c:x86-64-target))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod ra:instruction-clobbers ((instruction ir:switch-instruction) (architecture c:x86-64-target))
  '(:rax))

(defmethod ra:instruction-clobbers ((instruction ir:flush-binding-cache-entry-instruction) (architecture c:x86-64-target))
  '(:rax))

(defmethod ra:instruction-clobbers ((instruction ir:unbind-instruction) (architecture c:x86-64-target))
  '(:rbx :r13 :rax))

(defmethod ra:instruction-clobbers ((instruction ir:disestablish-block-or-tagbody-instruction) (architecture c:x86-64-target))
  '(:rbx :r13 :rax))

(defmethod ra:instruction-clobbers ((instruction ir:disestablish-unwind-protect-instruction) (architecture c:x86-64-target))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod ra:instruction-clobbers ((instruction ir:make-dx-typed-vector-instruction) (architecture c:x86-64-target))
  (if (ir:make-dx-typed-vector-zero-fill-p instruction)
      '(:rax :rcx)
      '()))

(defmethod ra:instruction-clobbers ((instruction ir:make-dx-closure-instruction) (architecture c:x86-64-target))
  '(:rax :rcx))

(defmethod ra:instruction-clobbers ((instruction ir:box-single-float-instruction) (architecture c:x86-64-target))
  '(:rax))

(defmethod ra:instruction-clobbers ((instruction ir:unbox-single-float-instruction) (architecture c:x86-64-target))
  '(:rax))

(defmethod ra:allow-memory-operand-p ((instruction ir:call-instruction) operand (architecture c:x86-64-target))
  (not (or (eql (ir:call-result instruction) operand)
           (eql (first (ir:call-arguments instruction)) operand)
           (eql (second (ir:call-arguments instruction)) operand)
           (eql (third (ir:call-arguments instruction)) operand)
           (eql (fourth (ir:call-arguments instruction)) operand)
           (eql (fifth (ir:call-arguments instruction)) operand))))

(defmethod ra:allow-memory-operand-p ((instruction ir:call-multiple-instruction) operand (architecture c:x86-64-target))
  (not (or (eql (first (ir:call-arguments instruction)) operand)
           (eql (second (ir:call-arguments instruction)) operand)
           (eql (third (ir:call-arguments instruction)) operand)
           (eql (fourth (ir:call-arguments instruction)) operand)
           (eql (fifth (ir:call-arguments instruction)) operand))))

(defmethod ra:allow-memory-operand-p ((instruction ir:tail-call-instruction) operand (architecture c:x86-64-target))
  (not (or (eql (first (ir:call-arguments instruction)) operand)
           (eql (second (ir:call-arguments instruction)) operand)
           (eql (third (ir:call-arguments instruction)) operand)
           (eql (fourth (ir:call-arguments instruction)) operand)
           (eql (fifth (ir:call-arguments instruction)) operand))))

(defmethod ra:allow-memory-operand-p ((instruction ir:funcall-instruction) operand (architecture c:x86-64-target))
  (not (or (eql (ir:call-result instruction) operand)
           (eql (ir:call-function instruction) operand)
           (eql (first (ir:call-arguments instruction)) operand)
           (eql (second (ir:call-arguments instruction)) operand)
           (eql (third (ir:call-arguments instruction)) operand)
           (eql (fourth (ir:call-arguments instruction)) operand)
           (eql (fifth (ir:call-arguments instruction)) operand))))

(defmethod ra:allow-memory-operand-p ((instruction ir:funcall-multiple-instruction) operand (architecture c:x86-64-target))
  (not (or (eql (ir:call-function instruction) operand)
           (eql (first (ir:call-arguments instruction)) operand)
           (eql (second (ir:call-arguments instruction)) operand)
           (eql (third (ir:call-arguments instruction)) operand)
           (eql (fourth (ir:call-arguments instruction)) operand)
           (eql (fifth (ir:call-arguments instruction)) operand))))

(defmethod ra:allow-memory-operand-p ((instruction ir:tail-funcall-instruction) operand (architecture c:x86-64-target))
  (not (or (eql (ir:call-function instruction) operand)
           (eql (first (ir:call-arguments instruction)) operand)
           (eql (second (ir:call-arguments instruction)) operand)
           (eql (third (ir:call-arguments instruction)) operand)
           (eql (fourth (ir:call-arguments instruction)) operand)
           (eql (fifth (ir:call-arguments instruction)) operand))))

(defmethod ra:allow-memory-operand-p ((instruction ir:argument-setup-instruction) operand (architecture c:x86-64-target))
  t)

(defmethod ra:allow-memory-operand-p ((instruction ir:finish-nlx-instruction) operand (architecture c:x86-64-target))
  t)

(defmethod ra:allow-memory-operand-p ((instruction ir:nlx-entry-instruction) operand (architecture c:x86-64-target))
  (not (eql operand (ir:nlx-entry-value instruction))))

(defmethod ra:allow-memory-operand-p ((instruction ir:nlx-entry-multiple-instruction) operand (architecture c:x86-64-target))
  t)

(defmethod ra:allow-memory-operand-p ((instruction ir:values-instruction) operand (architecture c:x86-64-target))
  t)

(defmethod ra:allow-memory-operand-p ((instruction ir:multiple-value-bind-instruction) operand (architecture c:x86-64-target))
  t)
