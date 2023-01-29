;;;; Target definitions for the ARM64 backend

(in-package :mezzano.compiler.backend.arm64)

(defmethod ra:architectural-physical-registers ((architecture c:arm64-target))
  ;; Does not include x29 (frame pointer) or x30 (link register).
  '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
    :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
    :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
    :x24 :x25 :x26 :x27 :x28
    :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
    :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
    :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
    :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))

(defmethod ra:target-argument-registers ((target c:arm64-target))
  '(:x0 :x1 :x2 :x3 :x4))

(defmethod ra:target-return-register ((target c:arm64-target))
  :x0)

(defmethod ra:target-funcall-register ((target c:arm64-target))
  :x6)

(defmethod ra:target-count-register ((target c:arm64-target))
  :x5)

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :value)) (architecture c:arm64-target))
  '(:x0 :x1 :x2 :x3 :x4 :x6 :x7))

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :integer)) (architecture c:arm64-target))
  ;; x12 not used here. Reserved for memory indexes.
  '(:x5 :x9 :x10 :x11))

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :single-float)) (architecture c:arm64-target))
  '(:q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
    :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
    :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
    :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :double-float)) (architecture c:arm64-target))
  '(:q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
    :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
    :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
    :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))

(defmethod ra:spill/fill-register-kinds-compatible (kind1 kind2 (architecture c:arm64-target))
  (or (eql kind1 kind2)
      ;; These register kinds are all mutually compatible as they are
      ;; at most 64 bits wide.
      (and (member kind1 '(:value :integer :single-float :double-float))
           (member kind2 '(:value :integer :single-float :double-float)))))

(defmethod ra:instruction-clobbers ((instruction ir::base-call-instruction) (architecture c:arm64-target))
  '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
    :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
    :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
    :x24 :x25
    :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
    :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
    :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
    :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))

(defmethod ra:instruction-clobbers ((instruction ir:argument-setup-instruction) (architecture c:arm64-target))
  '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
    :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
    :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
    :x24 :x25
    :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
    :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
    :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
    :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))

(defmethod ra:instruction-clobbers ((instruction ir:save-multiple-instruction) (architecture c:arm64-target))
  '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
    :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
    :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
    :x24 :x25
    :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
    :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
    :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
    :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))

(defmethod ra:instruction-clobbers ((instruction ir:restore-multiple-instruction) (architecture c:arm64-target))
  '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
    :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
    :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
    :x24 :x25
    :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
    :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
    :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
    :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))

(defmethod ra:instruction-clobbers ((instruction ir:forget-multiple-instruction) (architecture c:arm64-target))
  '(:x9))

(defmethod ra:instruction-clobbers ((instruction ir:nlx-entry-instruction) (architecture c:arm64-target))
  '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
    :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
    :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
    :x24 :x25
    :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
    :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
    :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
    :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))

(defmethod ra:instruction-clobbers ((instruction ir:nlx-entry-multiple-instruction) (architecture c:arm64-target))
  '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
    :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
    :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
    :x24 :x25
    :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
    :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
    :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
    :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))

(defmethod ra:instruction-clobbers ((instruction ir:values-instruction) (architecture c:arm64-target))
  '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
    :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
    :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
    :x24 :x25
    :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
    :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
    :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
    :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))

(defmethod ra:instruction-clobbers ((instruction ir:multiple-value-bind-instruction) (architecture c:arm64-target))
  '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
    :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
    :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
    :x24 :x25
    :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
    :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
    :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
    :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))

(defmethod ra:instruction-clobbers ((instruction ir:switch-instruction) (architecture c:arm64-target))
  '(:x9 :x10))

(defmethod ra:instruction-clobbers ((instruction ir:push-special-stack-instruction) (architecture c:arm64-target))
  '(:x9))

(defmethod ra:instruction-clobbers ((instruction ir:flush-binding-cache-entry-instruction) (architecture c:arm64-target))
  '(:x9))

(defmethod ra:instruction-clobbers ((instruction ir:unbind-instruction) (architecture c:arm64-target))
  '(:x6 :x7 :x9 :x10))

(defmethod ra:instruction-clobbers ((instruction ir:disestablish-block-or-tagbody-instruction) (architecture c:arm64-target))
  '(:x9 :x6 :x7))

(defmethod ra:instruction-clobbers ((instruction ir:disestablish-unwind-protect-instruction) (architecture c:arm64-target))
  '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
    :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
    :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
    :x24 :x25
    :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
    :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
    :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
    :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31))

(defmethod ra:instruction-clobbers ((instruction ir:make-dx-simple-vector-instruction) (architecture c:arm64-target))
  '(:x9))

(defmethod ra:instruction-clobbers ((instruction ir:make-dx-typed-vector-instruction) (architecture c:arm64-target))
  '(:x9))

(defmethod ra:instruction-clobbers ((instruction ir:make-dx-cons-instruction) (architecture c:arm64-target))
  '(:x9))

(defmethod ra:instruction-clobbers ((instruction ir:make-dx-closure-instruction) (architecture c:arm64-target))
  '(:x9))

(defmethod ra:instruction-clobbers ((instruction ir:box-single-float-instruction) (architecture c:arm64-target))
  '(:x9))

(defmethod ra:instruction-clobbers ((instruction ir:unbox-single-float-instruction) (architecture c:arm64-target))
  '(:x9))

(defmethod ra:allow-memory-operand-p ((instruction ir:call-instruction) operand (architecture c:arm64-target))
  (not (or (eql (ir:call-result instruction) operand)
           (eql (first (ir:call-arguments instruction)) operand)
           (eql (second (ir:call-arguments instruction)) operand)
           (eql (third (ir:call-arguments instruction)) operand)
           (eql (fourth (ir:call-arguments instruction)) operand)
           (eql (fifth (ir:call-arguments instruction)) operand))))

(defmethod ra:allow-memory-operand-p ((instruction ir:call-multiple-instruction) operand (architecture c:arm64-target))
  (not (or (eql (first (ir:call-arguments instruction)) operand)
           (eql (second (ir:call-arguments instruction)) operand)
           (eql (third (ir:call-arguments instruction)) operand)
           (eql (fourth (ir:call-arguments instruction)) operand)
           (eql (fifth (ir:call-arguments instruction)) operand))))

(defmethod ra:allow-memory-operand-p ((instruction ir:tail-call-instruction) operand (architecture c:arm64-target))
  (not (or (eql (first (ir:call-arguments instruction)) operand)
           (eql (second (ir:call-arguments instruction)) operand)
           (eql (third (ir:call-arguments instruction)) operand)
           (eql (fourth (ir:call-arguments instruction)) operand)
           (eql (fifth (ir:call-arguments instruction)) operand))))

(defmethod ra:allow-memory-operand-p ((instruction ir:funcall-instruction) operand (architecture c:arm64-target))
  (not (or (eql (ir:call-result instruction) operand)
           (eql (ir:call-function instruction) operand)
           (eql (first (ir:call-arguments instruction)) operand)
           (eql (second (ir:call-arguments instruction)) operand)
           (eql (third (ir:call-arguments instruction)) operand)
           (eql (fourth (ir:call-arguments instruction)) operand)
           (eql (fifth (ir:call-arguments instruction)) operand))))

(defmethod ra:allow-memory-operand-p ((instruction ir:funcall-multiple-instruction) operand (architecture c:arm64-target))
  (not (or (eql (ir:call-function instruction) operand)
           (eql (first (ir:call-arguments instruction)) operand)
           (eql (second (ir:call-arguments instruction)) operand)
           (eql (third (ir:call-arguments instruction)) operand)
           (eql (fourth (ir:call-arguments instruction)) operand)
           (eql (fifth (ir:call-arguments instruction)) operand))))

(defmethod ra:allow-memory-operand-p ((instruction ir:tail-funcall-instruction) operand (architecture c:arm64-target))
  (not (or (eql (ir:call-function instruction) operand)
           (eql (first (ir:call-arguments instruction)) operand)
           (eql (second (ir:call-arguments instruction)) operand)
           (eql (third (ir:call-arguments instruction)) operand)
           (eql (fourth (ir:call-arguments instruction)) operand)
           (eql (fifth (ir:call-arguments instruction)) operand))))

(defmethod ra:allow-memory-operand-p ((instruction ir:argument-setup-instruction) operand (architecture c:arm64-target))
  t)

(defmethod ra:allow-memory-operand-p ((instruction ir:finish-nlx-instruction) operand (architecture c:arm64-target))
  t)

(defmethod ra:allow-memory-operand-p ((instruction ir:nlx-entry-instruction) operand (architecture c:arm64-target))
  (not (eql operand (ir:nlx-entry-value instruction))))

(defmethod ra:allow-memory-operand-p ((instruction ir:nlx-entry-multiple-instruction) operand (architecture c:arm64-target))
  t)

(defmethod ra:allow-memory-operand-p ((instruction ir:values-instruction) operand (architecture c:arm64-target))
  t)

(defmethod ra:allow-memory-operand-p ((instruction ir:multiple-value-bind-instruction) operand (architecture c:arm64-target))
  t)

(defmethod ra:adjust-register-for-vreg-width ((kind (eql :single-float)) physical (architecture c:arm64-target))
  (ecase physical
    (:q0 :s0)
    (:q1 :s1)
    (:q2 :s2)
    (:q3 :s3)
    (:q4 :s4)
    (:q5 :s5)
    (:q6 :s6)
    (:q7 :s7)
    (:q8 :s8)
    (:q9 :s9)
    (:q10 :s10)
    (:q11 :s11)
    (:q12 :s12)
    (:q13 :s13)
    (:q14 :s14)
    (:q15 :s15)
    (:q16 :s16)
    (:q17 :s17)
    (:q18 :s18)
    (:q19 :s19)
    (:q20 :s20)
    (:q21 :s21)
    (:q22 :s22)
    (:q23 :s23)
    (:q24 :s24)
    (:q25 :s25)
    (:q26 :s26)
    (:q27 :s27)
    (:q28 :s28)
    (:q29 :s29)
    (:q30 :s30)
    (:q31 :s31)))
