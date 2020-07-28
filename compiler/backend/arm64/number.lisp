;;;; Number related ARM64 builtin operations

(in-package :mezzano.compiler.backend.arm64)

(define-builtin sys.int::fixnump ((object) :eq)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:ands
                       :operands (list :xzr object sys.int::+fixnum-tag-mask+)
                       :inputs (list object)
                       :outputs (list))))

(define-builtin mezzano.runtime::%fixnum-+ ((lhs rhs) result)
  (let ((out (make-instance 'ir:label :phis (list result)))
        (overflow (make-instance 'ir:label :name :+-overflow))
        (no-overflow (make-instance 'ir:label :name :+-no-overflow))
        (fixnum-result (make-instance 'ir:virtual-register))
        (bignum-result (make-instance 'ir:virtual-register)))
    (cond ((and (constant-value-p rhs 'integer)
                (<= 0 (ash (fetch-constant-value rhs) sys.int::+n-fixnum-bits+) 4095))
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:adds
                                :operands (list fixnum-result
                                                lhs
                                                (ash (fetch-constant-value rhs)
                                                     sys.int::+n-fixnum-bits+))
                                :inputs (list lhs)
                                :outputs (list fixnum-result))))
          ((and (constant-value-p rhs 'integer)
                (<= 0 (ash (- (fetch-constant-value rhs)) sys.int::+n-fixnum-bits+) 4095))
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:subs
                                :operands (list fixnum-result
                                                lhs
                                                (ash (- (fetch-constant-value rhs))
                                                     sys.int::+n-fixnum-bits+))
                                :inputs (list lhs)
                                :outputs (list fixnum-result))))
          (t
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:adds
                                :operands (list fixnum-result lhs rhs)
                                :inputs (list lhs rhs)
                                :outputs (list fixnum-result)))))
    (emit (make-instance 'arm64-branch-instruction
                         :opcode 'lap:b.vs
                         :true-target overflow
                         :false-target no-overflow))
    (emit no-overflow)
    (emit (make-instance 'ir:jump-instruction
                         :target out
                         :values (list fixnum-result)))
    ;; Build a bignum on overflow.
    ;; Recover the full value using the carry bit.
    (emit overflow)
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:adc
                         :operands (list :x9 :xzr :xzr)
                         :inputs (list)
                         :outputs (list :x9)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:add
                         :operands (list :x9 :xzr :x9 :lsl 63)
                         :inputs (list :x9)
                         :outputs (list :x9)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:add
                         :operands (list :x10 :x9 fixnum-result :lsr 1)
                         :inputs (list :x9 fixnum-result)
                         :outputs (list :x10)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:named-call
                         :operands '(sys.int::%%make-bignum-64-x10)
                         :inputs '(:x10)
                         :outputs (list :x0)
                         :clobbers '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
                                     :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
                                     :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
                                     :x24 :x25
                                     :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
                                     :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
                                     :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
                                     :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31)))
    (emit (make-instance 'ir:move-instruction
                         :destination bignum-result
                         :source :x0))
    (emit (make-instance 'ir:jump-instruction :target out :values (list bignum-result)))
    (emit out)))

(define-builtin mezzano.runtime::%fixnum-- ((lhs rhs) result)
  (let ((out (make-instance 'ir:label :phis (list result)))
        (overflow (make-instance 'ir:label :name :--overflow))
        (no-overflow (make-instance 'ir:label :name :--no-overflow))
        (fixnum-result (make-instance 'ir:virtual-register))
        (bignum-result (make-instance 'ir:virtual-register)))
    (cond ((and (constant-value-p rhs 'integer)
                (<= 0 (ash (fetch-constant-value rhs) sys.int::+n-fixnum-bits+) 4095))
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:subs
                                :operands (list fixnum-result
                                                lhs
                                                (ash (fetch-constant-value rhs)
                                                     sys.int::+n-fixnum-bits+))
                                :inputs (list lhs)
                                :outputs (list fixnum-result))))
          ((and (constant-value-p rhs 'integer)
                (<= 0 (ash (- (fetch-constant-value rhs)) sys.int::+n-fixnum-bits+) 4095))
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:adds
                                :operands (list fixnum-result
                                                lhs
                                                (ash (- (fetch-constant-value rhs))
                                                     sys.int::+n-fixnum-bits+))
                                :inputs (list lhs)
                                :outputs (list fixnum-result))))
          (t
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:subs
                                :operands (list fixnum-result lhs rhs)
                                :inputs (list lhs rhs)
                                :outputs (list fixnum-result)))))
    (emit (make-instance 'arm64-branch-instruction
                         :opcode 'lap:b.vs
                         :true-target overflow
                         :false-target no-overflow))
    (emit no-overflow)
    (emit (make-instance 'ir:jump-instruction
                         :target out
                         :values (list fixnum-result)))
    ;; Build a bignum on overflow.
    ;; Recover the full value using the carry bit.
    (emit overflow)
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:adc
                         :operands (list :x9 :xzr :xzr)
                         :inputs (list)
                         :outputs (list :x9)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:add
                         :operands (list :x9 :xzr :x9 :lsl 63)
                         :inputs (list :x9)
                         :outputs (list :x9)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:add
                         :operands (list :x10 :x9 fixnum-result :lsr 1)
                         :inputs (list :x9 fixnum-result)
                         :outputs (list :x10)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:named-call
                         :operands '(sys.int::%%make-bignum-64-x10)
                         :inputs '(:x10)
                         :outputs (list :x0)
                         :clobbers '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
                                     :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
                                     :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
                                     :x24 :x25
                                     :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
                                     :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
                                     :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
                                     :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31)))
    (emit (make-instance 'ir:move-instruction
                         :destination bignum-result
                         :source :x0))
    (emit (make-instance 'ir:jump-instruction :target out :values (list bignum-result)))
    (emit out)))

(define-builtin mezzano.runtime::%fixnum-logand ((lhs rhs) result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:and
                       :operands (list result lhs rhs)
                       :inputs (list lhs rhs)
                       :outputs (list result))))

(define-builtin mezzano.runtime::%fixnum-logior ((lhs rhs) result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:orr
                       :operands (list result lhs rhs)
                       :inputs (list lhs rhs)
                       :outputs (list result))))

(define-builtin mezzano.runtime::%fixnum-logxor ((lhs rhs) result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:eor
                       :operands (list result lhs rhs)
                       :inputs (list lhs rhs)
                       :outputs (list result))))

(define-builtin mezzano.runtime::%fixnum-< ((lhs rhs) :lt)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:subs
                       :operands (list :xzr lhs rhs)
                       :inputs (list lhs rhs)
                       :outputs '())))
