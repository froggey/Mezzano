;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for dealing with numbers.

(in-package :mezzano.compiler.codegen.arm64)

(defbuiltin sys.int::fixnump (object) ()
  (load-in-reg :x0 object t)
  (emit `(lap:ands :xzr :x0 ,sys.int::+fixnum-tag-mask+))
  (predicate-result :eq))

(defmacro define-two-arg-bitwise-op (name instruction support-function)
  `(defbuiltin ,name (x y) ()
     (let ((helper (gensym))
           (resume (gensym)))
       ;; Constants on the left hand side.
       (when (constant-type-p y 'fixnum)
         (rotatef x y))
       ;; Call out to the support function for non-fixnums.
       (emit-trailer (helper)
         (when (constant-type-p x 'fixnum)
           (load-constant :x1 (second x)))
         (call-support-function ',support-function 2)
         (emit `(lap:b ,resume)))
       (cond ((constant-type-p x 'fixnum)
              (load-in-x0 y t)
              (smash-x0)
              (emit `(lap:ands :xzr :x0 ,sys.int::+fixnum-tag-mask+)
                    `(lap:b.ne ,helper))
              ;; TODO: Detect bitfields and use them directly.
              (load-literal :x9 (fixnum-to-raw (second x)))
              (emit `(,',instruction :x0 :x0 :x9))
              (emit resume)
              (setf *x0-value* (list (gensym))))
             (t (load-in-reg :x1 y t)
                (load-in-reg :x0 x t)
                (smash-x0)
                (emit `(lap:ands :xzr :x0 ,sys.int::+fixnum-tag-mask+)
                      `(lap:b.ne ,helper)
                      `(lap:ands :xzr :x1 ,sys.int::+fixnum-tag-mask+)
                      `(lap:b.ne ,helper)
                      `(,',instruction :x0 :x0 :x1)
                      resume)
                (setf *x0-value* (list (gensym))))))))

(define-two-arg-bitwise-op sys.int::binary-logior lap:orr sys.int::generic-logior)
(define-two-arg-bitwise-op sys.int::binary-logxor lap:eor sys.int::generic-logxor)
(define-two-arg-bitwise-op sys.int::binary-logand lap:and sys.int::generic-logand)

(defbuiltin mezzano.runtime::%fixnum-right-shift (integer count) ()
  ;; INTEGER and COUNT must both be fixnums.
  (load-in-reg :x9 integer t)
  (cond ((constant-type-p count 'fixnum)
         (smash-x0)
         (let ((count-value (second count)))
           (cond ((>= count-value (- 64 sys.int::+n-fixnum-bits+))
                  ;; All bits shifted out.
                  (emit `(lap:add :x9 :xzr :x9 :asr 63)
                        `(lap:and :x0 :x9 ,(- (ash 1 sys.int::+n-fixnum-bits+)))))
                 (t
                  (emit `(lap:add :x9 :xzr :x9 :asr ,count-value)
                        `(lap:and :x0 :x9 ,(- (ash 1 sys.int::+n-fixnum-bits+))))))))
        (t
         ;; Shift right by arbitrary count.
         (let ((done-label (gensym))
               (sign-extend (gensym)))
           (load-in-reg :x10 count t)
           (smash-x0)
           (emit-trailer (sign-extend)
             (emit `(lap:add :x9 :xzr :x9 :asr 63)
                   `(lap:and :x0 :x9 ,(- (ash 1 sys.int::+n-fixnum-bits+)))
                   `(lap:b ,done-label)))
           ;; A64 masks the shift count to 6 bits, test if all the bits were shifted out.
           (emit `(lap:subs :xzr :x10 ,(fixnum-to-raw 64))
                 `(lap:b.cs ,sign-extend))
           ;; Unbox count.
           (emit `(lap:add :x10 :xzr :x10 :asr 1))
           ;; Shift, mask & produce result.
           (emit `(lap:asrv :x9 :x9 :x10)
                 `(lap:and :x0 :x9 ,(- (ash 1 sys.int::+n-fixnum-bits+))))
           (emit done-label))))
  (setf *x0-value* (list (gensym))))

;;; Arithmetic.

(defbuiltin sys.int::binary-+ (x y) ()
  (let ((ovfl (gensym "+ovfl"))
        (resume (gensym "+resume"))
        (full-add (gensym "+full")))
    (when (constant-type-p y 'fixnum)
      (rotatef x y))
    (emit-trailer (ovfl)
      ;; Recover the full value using the carry bit.
      (emit `(lap:adc :x9 :xzr :xzr)
            `(lap:add :x9 :xzr :x9 :lsl 63)
            `(lap:add :x10 :x9 :x0 :lsr 1))
      ;; Call assembly helper function.
      (emit `(lap:ldr :x7 (:function sys.int::%%make-bignum-64-x10)))
      (emit-object-load :x9 :x7 :slot sys.int::+fref-entry-point+)
      (emit `(lap:blr :x9)
            `(lap:b ,resume)))
    (emit-trailer (full-add)
      (when (constant-type-p x 'fixnum)
        (load-constant :x1 (second x)))
      (call-support-function 'sys.int::generic-+ 2)
      (emit `(lap:b ,resume)))
    (cond ((constant-type-p x 'fixnum)
           (load-in-x0 y t)
           (smash-x0)
           (emit `(lap:ands :xzr :x0 ,sys.int::+fixnum-tag-mask+)
                 `(lap:b.ne ,full-add))
           (cond ((<= -4095 (fixnum-to-raw (second x)) 4095)
                  ;; Small integers can be encoded directly into the instruction.
                  (if (minusp (second x))
                      (emit `(lap:subs :x0 :x0 ,(- (fixnum-to-raw (second x)))))
                      (emit `(lap:adds :x0 :x0 ,(fixnum-to-raw (second x))))))
                 (t
                  (load-literal :x9 (fixnum-to-raw (second x)))
                  (emit `(lap:adds :x0 :x0 :x9)))))
          (t (load-in-reg :x1 y t)
             (load-in-reg :x0 x t)
             (smash-x0)
             (emit `(lap:ands :xzr :x1 ,sys.int::+fixnum-tag-mask+)
                   `(lap:b.ne ,full-add)
                   `(lap:ands :xzr :x0 ,sys.int::+fixnum-tag-mask+)
                   `(lap:b.ne ,full-add))
             (emit `(lap:adds :x0 :x0 :x1))))
    (emit `(lap:b.vs ,ovfl)
          resume)
    (setf *x0-value* (list (gensym)))))

(defbuiltin sys.int::binary-- (x y) ()
  (let ((ovfl (gensym "-ovfl"))
        (resume (gensym "-resume"))
        (full-sub (gensym "-full")))
    (emit-trailer (ovfl)
      ;; Recover the full value using the carry bit.
      ;; Carry does not need to be inverted, unlike x86.
      (emit `(lap:adc :x9 :xzr :xzr)
            `(lap:add :x9 :xzr :x9 :lsl 63)
            `(lap:add :x10 :x9 :x0 :lsr 1))
      ;; Call assembly helper function.
      (emit `(lap:ldr :x7 (:function sys.int::%%make-bignum-64-x10)))
      (emit-object-load :x9 :x7 :slot sys.int::+fref-entry-point+)
      (emit `(lap:blr :x9)
            `(lap:b ,resume)))
    (emit-trailer (full-sub)
      (call-support-function 'sys.int::generic-- 2)
      (emit `(lap:b ,resume)))
    (load-in-reg :x0 x t)
    (load-in-reg :x1 y t)
    (smash-x0)
    (emit `(lap:ands :xzr :x1 ,sys.int::+fixnum-tag-mask+)
          `(lap:b.ne ,full-sub)
          `(lap:ands :xzr :x0 ,sys.int::+fixnum-tag-mask+)
          `(lap:b.ne ,full-sub))
    (emit `(lap:subs :x0 :x0 :x1)
          `(lap:b.vs ,ovfl)
          resume)
    (setf *x0-value* (list (gensym)))))

;; TODO: Overflow.
;; overflow occurs if bits 127-63 of a signed 64*64=128bit multiply are not all
;; ones or all zeros.
(defbuiltin sys.int::binary-* (x y) ()
  (let ((resume (gensym "*resume"))
        (full-mul (gensym "*full")))
    (emit-trailer (full-mul)
      (call-support-function 'sys.int::generic-* 2)
      (emit `(lap:b ,resume)))
    (load-in-reg :x1 y t)
    (load-in-reg :x0 x t)
    (smash-x0)
    (emit `(lap:ands :xzr :x1 ,sys.int::+fixnum-tag-mask+)
          `(lap:b.ne ,full-mul)
          `(lap:ands :xzr :x0 ,sys.int::+fixnum-tag-mask+)
          `(lap:b.ne ,full-mul))
    ;; Convert X0 to raw integer, leaving X1 as a fixnum.
    ;; This will cause the result to be a fixnum.
    (emit `(lap:add :x9 :xzr :x0 :asr 1)
          `(lap:madd :x0 :xzr :x9 :x1)
          resume)
    (setf *x0-value* (list (gensym)))))

(defbuiltin sys.int::%truncate (number divisor) ()
  (let ((full-truncate (gensym "full-truncate"))
        (resume (gensym "resume-truncate")))
    (emit-trailer (full-truncate)
      (call-support-function 'sys.int::generic-truncate 2)
      (emit `(lap:b ,resume)))
    (load-in-reg :x1 divisor t)
    (load-in-reg :x0 number t)
    (smash-x0)
    (emit `(lap:ands :xzr :x0 ,sys.int::+fixnum-tag-mask+)
          `(lap:b.ne ,full-truncate)
          `(lap:ands :xzr :x1 ,sys.int::+fixnum-tag-mask+)
          `(lap:b.ne ,full-truncate)
          ;; Bail out to the full truncate when /0 or /-1.
          `(lap:cbz :x1 ,full-truncate)
          `(lap:adds :xzr :x1 ,(ash 1 sys.int::+n-fixnum-bits+))
          `(lap:b.eq ,full-truncate)
          `(lap:sdiv :x9 :x0 :x1)
          ;; Compute the remainder.
          ;; remainder = numerator - (quotient * denominator)
          ;; :x9 holds the quotient as a integer.
          ;; :x1 holds the remainder as a fixnum.
          `(lap:msub :x1 :x0 :x9 :x1)
          `(lap:add :x0 :xzr :x9 :lsl ,sys.int::+n-fixnum-bits+))
    (prog1
        (cond ((member *for-value* '(:multiple :tail))
               (load-constant :x5 2)
               :multiple)
              (t
               (setf *x0-value* (list (gensym)))))
      (emit resume))))

;;; Comparisons.

(defbuiltin mezzano.runtime::%fixnum-< (x y) ()
  (load-in-reg :x1 y t)
  (load-in-reg :x0 x t)
  (emit `(lap:subs :xzr :x0 :x1))
  (predicate-result :lt))

;;; Single floats.

(defbuiltin mezzano.runtime::%%coerce-fixnum-to-single-float (value) ()
  (load-in-reg :x0 value)
  (emit `(lap:add :x9 :xzr :x0 :asr ,sys.int::+n-fixnum-bits+)
        `(lap:scvtf :s0 :x9)
        `(lap:fmov :w9 :s0)
        `(lap:add :x0 :xzr :x9 :lsl 32)
        `(lap:add :x0 :x0 ,sys.int::+tag-single-float+))
  (setf *x0-value* (list (gensym))))
