;;;; Floating point numbers

(in-package :mezzano.internals)

;;; Short-Floats

(defun %integer-as-short-float (value)
  (check-type value (unsigned-byte 16))
  (let ((result (mezzano.runtime::%allocate-object
                 sys.int::+object-tag-short-float+ 0 1 nil)))
    (setf (%object-ref-unsigned-byte-16 result 0) value)
    result))

(defun %short-float-as-integer (value)
  (check-type value short-float)
  (%object-ref-unsigned-byte-16 value 0))

(defun mezzano.runtime::%%coerce-fixnum-to-short-float (value)
  (mezzano.runtime::%%coerce-single-float-to-short-float
   (mezzano.runtime::%%coerce-fixnum-to-single-float value)))

;; see https://gist.github.com/rygorous/2156668

;; float_to_half_fast3_rtne
(defun mezzano.runtime::%%coerce-single-float-to-short-float (value)
  (let* ((f32infty (%single-float-as-integer
                    single-float-positive-infinity))
         (f16max (ash (+ 127 16) 23))
         (denorm-magicu (ash (+ (- 127 15) (- 23 10) 1) 23))
         (denorm-magic (%integer-as-single-float
                        denorm-magicu))
         (sign-mask #x80000000)
         (o nil)
         (fu (%single-float-as-integer value))
         (ff value)
         (sign (logand fu sign-mask)))
    (setf fu (logxor fu sign)
          ff (%integer-as-single-float fu))
    (cond ((>= fu f16max)
           ;; result is Inf or NaN (all exponent bits set)
           (setf o (if (> fu f32infty) ; NaN->qNaN and Inf->Inf
                       #x7E00
                       #x7C00)))
          ((< fu (ash 113 23)) ; resulting FP16 is subnormal or zero
           ;; use a magic value to align our 10 mantissa bits at the bottom of
           ;; the float. as long as FP addition is round-to-nearest-even this
           ;; just works.
           (incf ff denorm-magic)
           (setf fu (%single-float-as-integer ff))
           ;; and one integer subtract of the bias later, we have our final float!
           (setf o (- fu denorm-magicu)))
          (t
           (let ((mant-odd (logand (ash fu 13) 1))) ; resulting mantissa is odd
             ;; update exponent, rounding bias part 1
             (incf fu (+ (ash (- 15 127) 23) #xfff))
             ;; rounding bias part 2
             (incf fu mant-odd)
             ;; take the bits!
             (setf o (ash fu -13)))))
    (%integer-as-short-float (logior (ash sign -16) o))))

;; half_to_float
(defun mezzano.runtime::%%coerce-short-float-to-single-float (value)
  (let* ((magic (%integer-as-single-float (ash 113 23)))
         (shifted-exp (ash #x7C00 13)) ; exponent mask after shift
         (hu (%short-float-as-integer value))
         (o nil))
    (setf o (ash (logand hu #x7FFF) 13)) ; exponent/mantissa bits
    (let ((exp (logand shifted-exp o))) ; just the exponent
      (incf o (ash (- 127 15) 23)) ; exponent adjust
      ;; handle exponent special cases
      (cond ((eql exp shifted-exp) ; Inf/NaN?
             (incf o (ash (- 128 16) 23))) ; extra exp adjust
            ((eql exp 0) ; Zero/Denormal?
             (incf o (ash 1 23)) ; extra exp adjust
             (let ((of (%integer-as-single-float o)))
               (decf of magic) ; renormalize
               (setf o (%single-float-as-integer of)))))
      (setf o (logior o (ash (logand hu #x8000) 16))) ; sign bit
      (%integer-as-single-float o))))

(defun mezzano.runtime::%%coerce-double-float-to-short-float (value)
  (mezzano.runtime::%%coerce-single-float-to-short-float
   (mezzano.runtime::%%coerce-double-float-to-single-float value)))

(defun mezzano.runtime::%%coerce-short-float-to-double-float (value)
  (mezzano.runtime::%%coerce-single-float-to-double-float
   (mezzano.runtime::%%coerce-short-float-to-single-float value)))

(macrolet ((def (name op)
             `(defun ,name (x y)
                (float (,op (float x 0.0f0)
                            (float y 0.0f0))
                       0.0s0)))
           (def-pred (name op)
             `(defun ,name (x y)
                (,op (float x 0.0f0)
                     (float y 0.0f0)))))
  (def-pred %%short-float-< <)
  (def-pred %%short-float-= =)
  (def %%short-float-+ +)
  (def %%short-float-- -)
  (def %%short-float-* *)
  (def %%short-float-/ /))

(defun %%truncate-short-float (val)
  (multiple-value-bind (quot rem)
      (truncate (float val 0.0f0))
    (values quot (float rem 0.0s0))))

(defun %%short-float-sqrt (value)
  (float (sqrt (float value 0.0f0)) 0.0s0))
