;;;; Lower-level number functions

(in-package :mezzano.internals)

(defun %double-float-as-integer (double-float)
  (%object-ref-unsigned-byte-64 double-float 0))

(defun %integer-as-double-float (integer)
  (let ((result (mezzano.runtime::%allocate-object
                 sys.int::+object-tag-double-float+ 0 1 nil)))
    (setf (%object-ref-unsigned-byte-64 result 0) integer)
    result))

(declaim (inline call-with-float-contagion))
(defun call-with-float-contagion (x y single-fn double-fn short-fn)
  (cond ((or (double-float-p x)
             (double-float-p y))
         (funcall double-fn
                  (float x 1.0d0)
                  (float y 1.0d0)))
        ((or (single-float-p x)
             (single-float-p y))
         (funcall single-fn
                  (float x 1.0f0)
                  (float y 1.0f0)))
        (t
         (funcall short-fn
                  (float x 1.0s0)
                  (float y 1.0s0)))))

(defun sys.int::full-truncate (number divisor)
  (check-type number real)
  (check-type divisor real)
  (assert (/= divisor 0) (number divisor) 'division-by-zero)
  (cond ((and (sys.int::fixnump number)
              (sys.int::fixnump divisor))
         (error "FIXNUM/FIXNUM case hit GENERIC-TRUNCATE"))
        ((and (sys.int::fixnump number)
              (sys.int::bignump divisor))
         (sys.int::%%bignum-truncate number divisor))
        ((and (sys.int::bignump number)
              (sys.int::fixnump divisor))
         (sys.int::%%bignum-truncate number divisor))
        ((and (sys.int::bignump number)
              (sys.int::bignump divisor))
         (sys.int::%%bignum-truncate number divisor))
        ((or (floatp number)
             (floatp divisor))
         (let* ((val (/ number divisor))
                (integer-part (etypecase val
                                (short-float
                                 (mezzano.runtime::%truncate-short-float val))
                                (single-float
                                 (mezzano.runtime::%truncate-single-float val))
                                (double-float
                                 (mezzano.runtime::%truncate-double-float val)))))
           (values integer-part (* (- val integer-part) divisor))))
        ((or (typep number 'ratio)
             (typep divisor 'ratio))
         (mezzano.internals.numbers.ratio:ratio-truncate number divisor))
        (t (check-type number number)
           (check-type divisor number)
           (error "Argument combination ~S and ~S not supported." number divisor))))

(defun sys.int::full-/ (x y)
  (cond ((and (typep x 'integer)
              (typep y 'integer))
         (when (and (eql x 0) (not (eql y 0)))
           (return-from sys.int::full-/ 0))
         (multiple-value-bind (quot rem)
             (truncate x y)
           (cond ((zerop rem)
                  ;; Remainder is zero, result is an integer.
                  quot)
                 (t ;; Remainder is non-zero, produce a ratio.
                  (let ((negative (if (minusp x)
                                      (not (minusp y))
                                      (minusp y)))
                        (gcd (gcd x y)))
                    (mezzano.internals.numbers.ratio:%make-ratio
                     (if negative
                         (- (/ (abs x) gcd))
                         (/ (abs x) gcd))
                     (/ (abs y) gcd)))))))
        ((or (complexp x)
             (complexp y))
         (mezzano.internals.numbers.complex:complex-/ x y))
        ((or (floatp x)
             (floatp y))
         (call-with-float-contagion x y #'%%single-float-/ #'%%double-float-/ #'%%short-float-/))
        ((or (typep x 'ratio)
             (typep y 'ratio))
         (mezzano.internals.numbers.ratio:ratio-/ x y))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination ~S and ~S not supported." x y))))

(defun sys.int::full-+ (x y)
  (cond ((and (sys.int::fixnump x)
              (sys.int::fixnump y))
         (error "FIXNUM/FIXNUM case hit GENERIC-+"))
        ((and (sys.int::fixnump x)
              (sys.int::bignump y))
         (sys.int::%%bignum-+ (sys.int::%make-bignum-from-fixnum x) y))
        ((and (sys.int::bignump x)
              (sys.int::fixnump y))
         (sys.int::%%bignum-+ x (sys.int::%make-bignum-from-fixnum y)))
        ((and (sys.int::bignump x)
              (sys.int::bignump y))
         (sys.int::%%bignum-+ x y))
        ((or (complexp x)
             (complexp y))
         (mezzano.internals.numbers.complex:complex-+ x y))
        ((or (floatp x)
             (floatp y))
         (call-with-float-contagion x y #'%%single-float-+ #'%%double-float-+ #'%%short-float-+))
        ((or (typep x 'ratio)
             (typep y 'ratio))
         (mezzano.internals.numbers.ratio:ratio-+ x y))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination ~S and ~S not supported." x y))))

(defun sys.int::full-- (x y)
  (cond ((and (sys.int::fixnump x)
              (sys.int::fixnump y))
         (error "FIXNUM/FIXNUM case hit GENERIC--"))
        ((and (sys.int::fixnump x)
              (sys.int::bignump y))
         (sys.int::%%bignum-- (sys.int::%make-bignum-from-fixnum x) y))
        ((and (sys.int::bignump x)
              (sys.int::fixnump y))
         (sys.int::%%bignum-- x (sys.int::%make-bignum-from-fixnum y)))
        ((and (sys.int::bignump x)
              (sys.int::bignump y))
         (sys.int::%%bignum-- x y))
        ((or (complexp x)
             (complexp y))
         (mezzano.internals.numbers.complex:complex-- x y))
        ((or (floatp x)
             (floatp y))
         (call-with-float-contagion x y #'%%single-float-- #'%%double-float-- #'%%short-float--))
        ((or (typep x 'ratio)
             (typep y 'ratio))
         (mezzano.internals.numbers.ratio:ratio-- x y))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination ~S and ~S not supported." x y))))

(defun sys.int::full-* (x y)
  (cond ((and (sys.int::fixnump x)
              (sys.int::fixnump y))
         (error "FIXNUM/FIXNUM case hit GENERIC-*"))
        ((and (sys.int::fixnump x)
              (sys.int::bignump y))
         (sys.int::%%bignum-multiply-signed x y))
        ((and (sys.int::bignump x)
              (sys.int::fixnump y))
         (sys.int::%%bignum-multiply-signed x y))
        ((and (sys.int::bignump x)
              (sys.int::bignump y))
         (sys.int::%%bignum-multiply-signed x y))
        ((or (complexp x)
             (complexp y))
         (mezzano.internals.numbers.complex:complex-* x y))
        ((or (floatp x)
             (floatp y))
         (call-with-float-contagion x y #'%%single-float-* #'%%double-float-* #'%%short-float-*))
        ((or (typep x 'ratio)
             (typep y 'ratio))
         (mezzano.internals.numbers.ratio:ratio-* x y))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination ~S and ~S not supported." x y))))

(defun two-arg-gcd (a b)
  (check-type a integer)
  (check-type b integer)
  (setf a (abs a))
  (setf b (abs b))
  (loop (when (zerop b)
          (return a))
     (psetf b (mod a b)
            a b)))

(defun fix-fdiv-quotient (quotient number divisor)
  (cond ((or (double-float-p number)
             (double-float-p divisor))
         (float quotient 0.0d0))
        ((or (single-float-p number)
             (single-float-p divisor))
         (float quotient 0.0f0))
        ((or (short-float-p number)
             (short-float-p divisor))
         (float quotient 0.0s0))
        (t
         (float quotient 0.0f0))))

(defun ffloor (number &optional (divisor 1))
  (multiple-value-bind (quotient remainder)
      (floor number divisor)
    (values (fix-fdiv-quotient quotient number divisor)
            remainder)))

(defun fceiling (number &optional (divisor 1))
  (multiple-value-bind (quotient remainder)
      (ceiling number divisor)
    (values (fix-fdiv-quotient quotient number divisor)
            remainder)))

(defun ftruncate (number &optional (divisor 1))
  (multiple-value-bind (quotient remainder)
      (truncate number divisor)
    (values (fix-fdiv-quotient quotient number divisor)
            remainder)))

(defun fround (number &optional (divisor 1))
  (multiple-value-bind (quotient remainder)
      (round number divisor)
    (values (fix-fdiv-quotient quotient number divisor)
            remainder)))

;;; INTEGER-DECODE-FLOAT from SBCL.

(defconstant +single-float-significand-byte+ (byte 23 0))
(defconstant +single-float-exponent-byte+ (byte 8 23))
(defconstant +single-float-hidden-bit+ #x800000)
(defconstant +single-float-bias+ 126)
(defconstant +single-float-digits+ 24)
(defconstant +single-float-normal-exponent-max+ 254)
(defconstant +single-float-normal-exponent-min+ 1)

;;; Handle the denormalized case of INTEGER-DECODE-FLOAT for SINGLE-FLOAT.
(defun integer-decode-single-denorm (x)
  (let* ((bits (%single-float-as-integer (abs x)))
         (sig (ash (ldb +single-float-significand-byte+ bits) 1))
         (extra-bias 0))
    (loop
      (unless (zerop (logand sig +single-float-hidden-bit+))
        (return))
      (setq sig (ash sig 1))
      (incf extra-bias))
    (values sig
            (- (- +single-float-bias+)
               +single-float-digits+
               extra-bias)
            (if (minusp (float-sign x)) -1 1))))

;;; Handle the single-float case of INTEGER-DECODE-FLOAT. If an infinity or
;;; NaN, error. If a denorm, call i-d-s-DENORM to handle it.
(defun integer-decode-single-float (x)
  (let* ((bits (%single-float-as-integer (abs x)))
         (exp (ldb +single-float-exponent-byte+ bits))
         (sig (ldb +single-float-significand-byte+ bits))
         (sign (if (minusp (float-sign x)) -1 1))
         (biased (- exp +single-float-bias+ +single-float-digits+)))
    (unless (<= exp +single-float-normal-exponent-max+)
      (error "can't decode NaN or infinity: ~S" x))
    (cond ((and (zerop exp) (zerop sig))
           (values 0 biased sign))
          ((< exp +single-float-normal-exponent-min+)
           (integer-decode-single-denorm x))
          (t
           (values (logior sig +single-float-hidden-bit+) biased sign)))))

(defconstant +double-float-significand-byte+ (byte 20 0))
(defconstant +double-float-exponent-byte+ (byte 11 20))
(defconstant +double-float-hidden-bit+ #x100000)
(defconstant +double-float-bias+ 1022)
(defconstant +double-float-digits+ 53)
(defconstant +double-float-normal-exponent-max+ 2046)
(defconstant +double-float-normal-exponent-min+ 1)

;;; like INTEGER-DECODE-SINGLE-DENORM, only doubly so
(defun integer-decode-double-denorm (x)
  (let* ((bits (%double-float-as-integer (abs x)))
         (high-bits (ldb (byte 32 32) bits))
         (sig-high (ldb +double-float-significand-byte+ high-bits))
         (low-bits (ldb (byte 32 0) bits))
         (sign (if (minusp (float-sign x)) -1 1))
         (biased (- (- +double-float-bias+) +double-float-digits+)))
    (if (zerop sig-high)
        (let ((sig low-bits)
              (extra-bias (- +double-float-digits+ 33))
              (bit (ash 1 31)))
          (loop
            (unless (zerop (logand sig bit)) (return))
            (setq sig (ash sig 1))
            (incf extra-bias))
          (values (ash sig (- +double-float-digits+ 32))
                  (- biased extra-bias)
                  sign))
        (let ((sig (ash sig-high 1))
              (extra-bias 0))
          (loop
            (unless (zerop (logand sig +double-float-hidden-bit+))
              (return))
            (setq sig (ash sig 1))
            (incf extra-bias))
          (values (logior (ash sig 32) (ash low-bits (1- extra-bias)))
                  (- biased extra-bias)
                  sign)))))

;;; like INTEGER-DECODE-SINGLE-FLOAT, only doubly so
(defun integer-decode-double-float (x)
  (let* ((abs (abs x))
         (bits (%double-float-as-integer abs))
         (hi (ldb (byte 32 32) bits))
         (lo (ldb (byte 32 0) bits))
         (exp (ldb +double-float-exponent-byte+ hi))
         (sig (ldb +double-float-significand-byte+ hi))
         (sign (if (minusp (float-sign x)) -1 1))
         (biased (- exp +double-float-bias+ +double-float-digits+)))
    (unless (<= exp +double-float-normal-exponent-max+)
      (error "Can't decode NaN or infinity: ~S." x))
    (cond ((and (zerop exp) (zerop sig) (zerop lo))
           (values 0 biased sign))
          ((< exp +double-float-normal-exponent-min+)
           (integer-decode-double-denorm x))
          (t
           (values
            (logior (ash (logior (ldb +double-float-significand-byte+ hi)
                                 +double-float-hidden-bit+)
                         32)
                    lo)
            biased sign)))))

(defconstant +short-float-significand-byte+ (byte 10 0))
(defconstant +short-float-exponent-byte+ (byte 5 10))
(defconstant +short-float-hidden-bit+ #x0400)
(defconstant +short-float-bias+ 14)
(defconstant +short-float-digits+ 11)
(defconstant +short-float-normal-exponent-max+ 30)
(defconstant +short-float-normal-exponent-min+ 1)

;;; like INTEGER-DECODE-SINGLE-DENORM, only half as much so
(defun integer-decode-short-denorm (x)
  (let* ((bits (%short-float-as-integer (abs x)))
         (sig (ash (ldb +short-float-significand-byte+ bits) 1))
         (extra-bias 0))
    (loop
      (unless (zerop (logand sig +short-float-hidden-bit+))
        (return))
      (setq sig (ash sig 1))
      (incf extra-bias))
    (values sig
            (- (- +short-float-bias+)
               +short-float-digits+
               extra-bias)
            (if (minusp (float-sign x)) -1 1))))

;;; like INTEGER-DECODE-SINGLE-FLOAT, only doubly so
(defun integer-decode-short-float (x)
  (let* ((bits (%short-float-as-integer (abs x)))
         (exp (ldb +short-float-exponent-byte+ bits))
         (sig (ldb +short-float-significand-byte+ bits))
         (sign (if (minusp (float-sign x)) -1 1))
         (biased (- exp +short-float-bias+ +short-float-digits+)))
    (unless (<= exp +short-float-normal-exponent-max+)
      (error "can't decode NaN or infinity: ~S" x))
    (cond ((and (zerop exp) (zerop sig))
           (values 0 biased sign))
          ((< exp +short-float-normal-exponent-min+)
           (integer-decode-short-denorm x))
          (t
           (values (logior sig +short-float-hidden-bit+) biased sign)))))

(defun integer-decode-float (float)
  (etypecase float
    (short-float (integer-decode-short-float float))
    (single-float (integer-decode-single-float float))
    (double-float (integer-decode-double-float float))))

(defun float-sign (float1 &optional (float2 (float 1 float1)))
  "Return a floating-point number that has the same sign as
   FLOAT1 and, if FLOAT2 is given, has the same absolute value
   as FLOAT2."
  (check-type float1 float)
  (check-type float2 float)
  (* (if (etypecase float1
           (short-float (logbitp 15 (%short-float-as-integer float1)))
           (single-float (logbitp 31 (%single-float-as-integer float1)))
           (double-float (logbitp 63 (%double-float-as-integer float1))))
         (float -1 float1)
         (float 1 float1))
     (abs float2)))

(defun float-digits (f)
  (check-type f float)
  (etypecase f
    (short-float +short-float-digits+)
    (single-float +single-float-digits+)
    (double-float +double-float-digits+)))

(defun float-radix (x)
  "Return (as an integer) the radix b of its floating-point argument."
  (check-type x float)
  2)

(defun float-denormalized-p (x)
  "Return true if the float X is denormalized."
  (check-type x float)
  (etypecase x
    (short-float
     (and (zerop (ldb +short-float-exponent-byte+ (%short-float-as-integer x)))
          (not (zerop x))))
    (single-float
     (and (zerop (ldb +single-float-exponent-byte+ (%single-float-as-integer x)))
          (not (zerop x))))
    ((double-float)
     (and (zerop (ldb +double-float-exponent-byte+
                      (ash (%double-float-as-integer x) -32)))
          (not (zerop x))))))

(defun float-precision (f)
  "Return a non-negative number of significant digits in its float argument.
  Will be less than FLOAT-DIGITS if denormalized or zero."
  (check-type f float)
  (macrolet ((frob (digits bias decode)
               `(cond ((zerop f) 0)
                      ((float-denormalized-p f)
                       (multiple-value-bind (ignore exp) (,decode f)
                         (declare (ignore ignore))
                         (the fixnum
                                    (+ ,digits (1- ,digits) ,bias exp))))
                      (t
                       ,digits))))
    (etypecase f
      (short-float
       (frob +short-float-digits+ +short-float-bias+
         integer-decode-short-denorm))
      (single-float
       (frob +single-float-digits+ +single-float-bias+
         integer-decode-single-denorm))
      (double-float
       (frob +double-float-digits+ +double-float-bias+
         integer-decode-double-denorm)))))

(defun scale-float (float integer)
  (* float (expt (float (float-radix float) float) integer)))

;;; Handle the denormalized case of DECODE-SINGLE-FLOAT. We call
;;; INTEGER-DECODE-SINGLE-DENORM and then make the result into a float.
(defun decode-single-denorm (x)
  (check-type x single-float)
  (multiple-value-bind (sig exp sign)
      (integer-decode-single-denorm x)
    (values (%integer-as-single-float
             (dpb sig +single-float-significand-byte+
                  (dpb +single-float-bias+
                       +single-float-exponent-byte+
                       0)))
            (+ exp +single-float-digits+)
            (float sign x))))

;;; Handle the single-float case of DECODE-FLOAT. If an infinity or NaN,
;;; error. If a denorm, call d-s-DENORM to handle it.
(defun decode-single-float (x)
  (check-type x single-float)
  (let* ((bits (%single-float-as-integer (abs x)))
         (exp (ldb +single-float-exponent-byte+ bits))
         (sign (float-sign x))
         (biased (- exp +single-float-bias+)))
    (unless (<= exp +single-float-normal-exponent-max+)
      (error "can't decode NaN or infinity: ~S" x))
    (cond ((zerop x)
           (values 0.0f0 biased sign))
          ((< exp +single-float-normal-exponent-min+)
           (decode-single-denorm x))
          (t
           (values (%integer-as-single-float
                    (dpb +single-float-bias+
                         +single-float-exponent-byte+
                         bits))
                   biased sign)))))

;;; like DECODE-SINGLE-DENORM, only doubly so
(defun decode-double-denorm (x)
  (check-type x double-float)
  (multiple-value-bind (sig exp sign)
      (integer-decode-double-denorm x)
    (values (%integer-as-double-float
             (logior
              (ash (dpb (logand (ash sig -32)
                                (lognot +double-float-hidden-bit+))
                        +double-float-significand-byte+
                        (dpb +double-float-bias+
                             +double-float-exponent-byte+
                             0))
                   32)
              (ldb (byte 32 0) sig)))
            (+ exp +double-float-digits+)
            (float sign x))))

;;; like DECODE-SINGLE-FLOAT, only doubly so
(defun decode-double-float (x)
  (check-type x double-float)
  (let* ((abs (abs x))
         (hi (ldb (byte 32 32) (%double-float-as-integer abs)))
         (lo (ldb (byte 32 0) (%double-float-as-integer abs)))
         (exp (ldb +double-float-exponent-byte+ hi))
         (sign (float-sign x))
         (biased (- exp +double-float-bias+)))
    (unless (<= exp +double-float-normal-exponent-max+)
      (error "can't decode NaN or infinity: ~S" x))
    (cond ((zerop x)
           (values 0.0d0 biased sign))
          ((< exp +double-float-normal-exponent-min+)
           (decode-double-denorm x))
          (t
           (values (%integer-as-double-float
                    (logior
                     (ash (dpb +double-float-bias+
                               +double-float-exponent-byte+ hi)
                          32)
                     lo))
                   biased sign)))))

;;; Handle the denormalized case of DECODE-SHORT-FLOAT. We call
;;; INTEGER-DECODE-SHORT-DENORM and then make the result into a float.
(defun decode-short-denorm (x)
  (check-type x short-float)
  (multiple-value-bind (sig exp sign)
      (integer-decode-short-denorm x)
    (values (%integer-as-short-float
             (dpb sig +short-float-significand-byte+
                  (dpb +short-float-bias+
                       +short-float-exponent-byte+
                       0)))
            (+ exp +short-float-digits+)
            (float sign x))))

;;; Handle the short-float case of DECODE-FLOAT. If an infinity or NaN,
;;; error. If a denorm, call d-s-DENORM to handle it.
(defun decode-short-float (x)
  (check-type x short-float)
  (let* ((bits (%short-float-as-integer (abs x)))
         (exp (ldb +short-float-exponent-byte+ bits))
         (sign (float-sign x))
         (biased (- exp +short-float-bias+)))
    (unless (<= exp +short-float-normal-exponent-max+)
      (error "can't decode NaN or infinity: ~S" x))
    (cond ((zerop x)
           (values 0.0f0 biased sign))
          ((< exp +short-float-normal-exponent-min+)
           (decode-short-denorm x))
          (t
           (values (%integer-as-short-float
                    (dpb +short-float-bias+
                         +short-float-exponent-byte+
                         bits))
                   biased sign)))))

;;; Dispatch to the appropriate type-specific function.
(defun decode-float (f)
  "Return three values:
   1) a floating-point number representing the significand. This is always
      between 0.5 (inclusive) and 1.0 (exclusive).
   2) an integer representing the exponent.
   3) -1.0 or 1.0 (i.e. the sign of the argument.)"
  (check-type f float)
  (etypecase f
    (single-float
     (decode-single-float f))
    (double-float
     (decode-double-float f))
    (short-float
     (decode-short-float f))))

(defun rational (number)
  (check-type number real)
  (etypecase number
    (rational
     number)
    (float
     (multiple-value-bind (significand exponent sign)
         (integer-decode-float number)
       (if (eql significand 0)
           0
           (let ((signed-significand (if (minusp sign)
                                         (- significand)
                                         significand)))
             (if (minusp exponent)
                 (* signed-significand (/ (ash 1 (- exponent))))
                 (ash signed-significand exponent))))))))

(defun rationalize (number)
  (rational number))

(defun short-float-to-ieee-binary16 (short-float)
  "Reinterpret SHORT-FLOAT as an (unsigned-byte 16).
This returns the raw IEEE binary representation of the float as an integer.
0.0s0 => #x0000, 1.0s0 => #x3C00, etc."
  (check-type short-float short-float)
  (%short-float-as-integer short-float))

(defun ieee-binary16-to-short-float (ieee-binary16)
  "Reinterpret the (unsigned-byte 16) IEEE-BINARY16 as a short-float.
This converts the raw IEEE binary representation to a float.
#x0000 => 0.0s0, #x3C00 => 1.0s0, etc."
  (check-type ieee-binary16 (unsigned-byte 16))
  (%integer-as-short-float ieee-binary16))

(defun single-float-to-ieee-binary32 (single-float)
  "Reinterpret SINGLE-FLOAT as an (unsigned-byte 32).
This returns the raw IEEE binary representation of the float as an integer.
0.0f0 => #x00000000, 1.0f0 => #x3F800000, etc."
  (check-type single-float single-float)
  (%single-float-as-integer single-float))

(defun ieee-binary32-to-single-float (ieee-binary32)
  "Reinterpret the (unsigned-byte 32) IEEE-BINARY32 as a single-float.
This converts the raw IEEE binary representation to a float.
#x00000000 => 0.0f0, #x3F800000 => 1.0f0, etc."
  (check-type ieee-binary32 (unsigned-byte 32))
  (%integer-as-single-float ieee-binary32))

(defun double-float-to-ieee-binary64 (double-float)
  "Reinterpret DOUBLE-FLOAT as an (unsigned-byte 64).
This returns the raw IEEE binary representation of the float as an integer.
0.0d0 => #x0000000000000000, 1.0d0 => #x3FF0000000000000, etc."
  (check-type double-float double-float)
  (%double-float-as-integer double-float))

(defun ieee-binary64-to-double-float (ieee-binary64)
  "Reinterpret the (unsigned-byte 64) IEEE-BINARY64 as a double-float.
This converts the raw IEEE binary representation to a float.
#x0000000000000000 => 0.0d0, #x3FF0000000000000 => 1.0d0, etc."
  (check-type ieee-binary64 (unsigned-byte 64))
  (%integer-as-double-float ieee-binary64))
