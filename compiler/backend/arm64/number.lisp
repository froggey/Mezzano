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

(define-builtin mezzano.compiler::%fast-fixnum-+ ((lhs rhs) result)
  (cond ((and (constant-value-p rhs 'integer)
              (<= 0 (ash (fetch-constant-value rhs) sys.int::+n-fixnum-bits+) 4095))
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:add
                              :operands (list result
                                              lhs
                                              (ash (fetch-constant-value rhs)
                                                   sys.int::+n-fixnum-bits+))
                              :inputs (list lhs)
                              :outputs (list result))))
        ((and (constant-value-p rhs 'integer)
              (<= 0 (ash (- (fetch-constant-value rhs)) sys.int::+n-fixnum-bits+) 4095))
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:sub
                              :operands (list result
                                              lhs
                                              (ash (- (fetch-constant-value rhs))
                                                   sys.int::+n-fixnum-bits+))
                              :inputs (list lhs)
                              :outputs (list result))))
          (t
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:add
                                :operands (list result lhs rhs)
                                :inputs (list lhs rhs)
                                :outputs (list result))))))

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

(define-builtin mezzano.compiler::%fast-fixnum-- ((lhs rhs) result)
  (cond ((and (constant-value-p rhs 'integer)
              (<= 0 (ash (fetch-constant-value rhs) sys.int::+n-fixnum-bits+) 4095))
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:sub
                              :operands (list result
                                              lhs
                                              (ash (fetch-constant-value rhs)
                                                   sys.int::+n-fixnum-bits+))
                              :inputs (list lhs)
                              :outputs (list result))))
        ((and (constant-value-p rhs 'integer)
              (<= 0 (ash (- (fetch-constant-value rhs)) sys.int::+n-fixnum-bits+) 4095))
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:add
                              :operands (list result
                                              lhs
                                              (ash (- (fetch-constant-value rhs))
                                                   sys.int::+n-fixnum-bits+))
                              :inputs (list lhs)
                              :outputs (list result))))
        (t
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:sub
                              :operands (list result lhs rhs)
                              :inputs (list lhs rhs)
                              :outputs (list result))))))

(define-builtin mezzano.runtime::%fixnum-logand ((lhs rhs) result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:and
                       :operands (list result lhs rhs)
                       :inputs (list lhs rhs)
                       :outputs (list result))))

(define-builtin mezzano.compiler::%fast-fixnum-logand ((lhs rhs) result)
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

(define-builtin mezzano.compiler::%fast-fixnum-logior ((lhs rhs) result)
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

(define-builtin mezzano.compiler::%fast-fixnum-logxor ((lhs rhs) result)
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

(define-builtin mezzano.runtime::%fixnum-<-unsigned ((lhs rhs) :cc)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:subs
                       :operands (list :xzr lhs rhs)
                       :inputs (list lhs rhs)
                       :outputs '())))

(define-builtin mezzano.compiler::%fast-fixnum-left-shift ((integer count) result)
  (cond ((constant-value-p count '(integer 0))
         (let ((count-value (fetch-constant-value count)))
           (cond ((>= count-value (- 64 sys.int::+n-fixnum-bits+))
                  ;; All bits shifted out.
                  ;; Turn INTEGER into 0.
                  (emit (make-instance 'ir:constant-instruction
                                       :destination result
                                       :value 0)))
                 ((zerop count-value)
                  (emit (make-instance 'ir:move-instruction
                                       :destination result
                                       :source integer)))
                 (t
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:lsl
                                       :operands (list result integer count-value)
                                       :inputs (list integer)
                                       :outputs (list result)))))))
        (t
         ;; No need to test for overlong shift counts here. The result must fit
         ;; in a fixnum, so obviously the count is within range or the input
         ;; is 0.
         (let ((count-value (make-instance 'ir:virtual-register :kind :integer)))
           (emit (make-instance 'ir:unbox-fixnum-instruction
                                :source count
                                :destination count-value))
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:lsl
                                :operands (list result integer count-value)
                                :inputs (list integer count-value)
                                :outputs (list result)))))))

(define-builtin mezzano.runtime::%fixnum-right-shift ((integer count) result)
  ;; INTEGER and COUNT must both be fixnums.
  (cond ((constant-value-p count '(integer 0))
         (let ((count-value (fetch-constant-value count))
               (temp (make-instance 'ir:virtual-register :kind :integer)))
           (cond ((>= count-value (- 64 sys.int::+n-fixnum-bits+))
                  ;; All bits shifted out.
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:add
                                       :operands (list temp :xzr integer :asr 63)
                                       :inputs (list integer)
                                       :outputs (list temp))))
                 (t
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:add
                                       :operands (list temp :xzr integer :asr count-value)
                                       :inputs (list integer)
                                       :outputs (list temp)))))
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:and
                                :operands (list result temp (- (ash 1 sys.int::+n-fixnum-bits+)))
                                :inputs (list temp)
                                :outputs (list result)))))
        (t
         ;; Shift right by arbitrary count.
         (let ((done-label (make-instance 'ir:label :name :fixnum-right-shift-done :phis (list result)))
               (sign-extend (make-instance 'ir:label :name :fixnum-right-shift-sign-extend))
               (no-extend (make-instance 'ir:label :name :fixnum-right-shift-no-extend))
               (extend-temp (make-instance 'ir:virtual-register :kind :integer))
               (extend-result (make-instance 'ir:virtual-register))
               (unboxed-shift-count (make-instance 'ir:virtual-register :kind :integer))
               (shift-temp (make-instance 'ir:virtual-register :kind :integer))
               (shift-result (make-instance 'ir:virtual-register)))
           ;; A64 masks the shift count to 6 bits, test if all the bits were shifted out.
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:subs
                                :operands (list :xzr count (c::fixnum-to-raw 64))
                                :inputs (list count)
                                :outputs (list)))
           (emit (make-instance 'arm64-branch-instruction
                                :opcode 'lap:b.cs
                                :true-target sign-extend
                                :false-target no-extend))
           (emit sign-extend)
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:add
                                :operands (list extend-temp :xzr integer :asr 63)
                                :inputs (list integer)
                                :outputs (list extend-temp)))
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:and
                                :operands (list extend-result extend-temp (- (ash 1 sys.int::+n-fixnum-bits+)))
                                :inputs (list extend-temp)
                                :outputs (list extend-result)))
           (emit (make-instance 'ir:jump-instruction
                                :target done-label
                                :values (list extend-result)))
           (emit no-extend)
           ;; Unbox count.
           (emit (make-instance 'ir:unbox-fixnum-instruction
                                :source count
                                :destination unboxed-shift-count))
           ;; Shift, mask & produce result.
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:asrv
                                :operands (list shift-temp integer unboxed-shift-count)
                                :inputs (list integer unboxed-shift-count)
                                :outputs (list shift-temp)))
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:and
                                :operands (list shift-result shift-temp (- (ash 1 sys.int::+n-fixnum-bits+)))
                                :inputs (list shift-temp)
                                :outputs (list shift-result)))
           (emit (make-instance 'ir:jump-instruction
                                :target done-label
                                :values (list shift-result)))
           (emit done-label)))))

(define-builtin mezzano.runtime::%ub64-right-shift-in-limits ((integer count) result)
  (when (constant-value-p count '(eql 0))
    ;; Not shifting by anything.
    (emit (make-instance 'ir:move-instruction
                         :source integer
                         :destination result))
    (finish))
  (let ((integer-unboxed (make-instance 'ir:virtual-register :kind :integer))
        (result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-unsigned-byte-64-instruction
                         :source integer
                         :destination integer-unboxed))
    (cond ((constant-value-p count '(unsigned-byte 6))
           ;; The perfect size to use as a shift constant.
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:add
                                :operands (list result-unboxed :xzr integer-unboxed :lsr (fetch-constant-value count))
                                :inputs (list integer-unboxed)
                                :outputs (list result-unboxed))))
          (t
           (let ((count-unboxed (make-instance 'ir:virtual-register :kind :integer)))
             (emit (make-instance 'ir:unbox-fixnum-instruction
                                  :source count
                                  :destination count-unboxed))
             ;; Shift, mask & produce result.
             (emit (make-instance 'arm64-instruction
                                  :opcode 'lap:asrv
                                  :operands (list result-unboxed integer-unboxed count-unboxed)
                                  :inputs (list integer-unboxed count-unboxed)
                                  :outputs (list result-unboxed))))))
    (emit (make-instance 'ir:box-unsigned-byte-64-instruction
                         :source result-unboxed
                         :destination result))))

;; overflow occurs if bits 127-63 of a signed 64*64=128bit multiply are not all
;; ones or all zeros.
(define-builtin mezzano.runtime::%fixnum-* ((x y) result)
  (let ((x-unboxed (make-instance 'ir:virtual-register :kind :integer))
        (low-half (make-instance 'ir:virtual-register :kind :integer))
        (high-half (make-instance 'ir:virtual-register :kind :integer))
        (sign-word (make-instance 'ir:virtual-register :kind :integer))
        (bignum-result (make-instance 'ir:virtual-register))
        (overflow (make-instance 'ir:label :name :fixnum-*-overflow))
        (no-overflow (make-instance 'ir:label :name :fixnum-*-no-overflow))
        (out-label (make-instance 'ir:label :name :fixnum-*-done :phis (list result))))
    ;; Unbox one operand, this will produce a boxed result.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source x
                         :destination x-unboxed))
    ;; Produce the high bits. This must match the sign-extended low bits.
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:smulh
                         :operands (list high-half x-unboxed y)
                         :inputs (list x-unboxed y)
                         :outputs (list high-half)))
    ;; Produce the low bits.
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:madd
                         :operands (list low-half :xzr x-unboxed y)
                         :inputs (list x-unboxed y)
                         :outputs (list low-half)))
    ;; Sign extend the low 64-bits so we can check it against the high 64-bits
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:add
                         :operands (list sign-word :xzr low-half :asr 63)
                         :inputs (list low-half)
                         :outputs (list sign-word)))
    ;; Compare the two
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr sign-word high-half)
                         :inputs (list sign-word high-half)
                         :outputs (list)))
    (emit (make-instance 'arm64-branch-instruction
                         :opcode 'lap:b.eq
                         :true-target no-overflow
                         :false-target overflow))
    (emit no-overflow)
    (emit (make-instance 'ir:jump-instruction
                         :target out-label
                         :values (list low-half)))
    (emit overflow)
    ;; Overflow occured, we have the full result in the hi/lo registers.
    ;; Punt to the helper function.
    (emit (make-instance 'ir:move-instruction
                         :source low-half
                         :destination :x10))
    (emit (make-instance 'ir:move-instruction
                         :source high-half
                         :destination :x11))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:named-call
                         :operands '(sys.int::%%fixnum-multiply-overflow)
                         :inputs '(:x10 :x11)
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
    (emit (make-instance 'ir:jump-instruction
                         :target out-label
                         :values (list bignum-result)))
    (emit out-label)))

(define-builtin mezzano.compiler::%fast-fixnum-* ((lhs rhs) result)
  (cond ((or (constant-value-p lhs '(eql 0))
             (constant-value-p rhs '(eql 0)))
         (emit (make-instance 'ir:constant-instruction
                              :value 0
                              :destination result))
         (finish))
        ((constant-value-p rhs '(eql 1))
         (emit (make-instance 'ir:move-instruction
                              :source lhs
                              :destination result))
         (finish))
        ((constant-value-p lhs '(eql 1))
         (emit (make-instance 'ir:move-instruction
                              :source rhs
                              :destination result))
         (finish)))
  (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    ;; Unbox one operand, this will produce a boxed result.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source lhs
                         :destination lhs-unboxed))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:madd
                         :operands (list result :xzr lhs-unboxed rhs)
                         :inputs (list lhs-unboxed rhs)
                         :outputs (list result)))))

(define-builtin mezzano.runtime::%fixnum-truncate ((number divisor) (quotient remainder))
  (let ((quotient-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:sdiv
                         :operands (list quotient-unboxed number divisor)
                         :inputs (list number divisor)
                         :outputs (list quotient-unboxed)))
    ;; Compute the remainder.
    ;; remainder = numerator - (quotient * denominator)
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:msub
                         :operands (list remainder number quotient-unboxed divisor)
                         :inputs (list number quotient-unboxed divisor)
                         :outputs (list remainder)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source quotient-unboxed
                         :destination quotient))))

;;; Floats!

(define-builtin mezzano.runtime::%%coerce-fixnum-to-single-float ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :integer))
        (value-float (make-instance 'ir:virtual-register :kind :single-float)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:scvtf
                         :operands (list `(:fp-32 ,value-float) value-unboxed)
                         :inputs (list value-unboxed)
                         :outputs (list value-float)))
    (emit (make-instance 'ir:box-single-float-instruction
                         :source value-float
                         :destination result))))
