;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defun ratiop (object)
  (%object-of-type-p object +object-tag-ratio+))

(defun numerator (rational)
  (etypecase rational
    (ratio (%object-ref-t rational +ratio-numerator+))
    (integer rational)))

(defun denominator (rational)
  (etypecase rational
    (ratio (%object-ref-t rational +ratio-denominator+))
    (integer 1)))

(defun complexp (object)
  (or (%object-of-type-p object +object-tag-complex-rational+)
      (%object-of-type-p object +object-tag-complex-single-float+)
      (%object-of-type-p object +object-tag-complex-double-float+)))

(defun complex (realpart &optional imagpart)
  (check-type realpart real)
  (check-type imagpart (or real null))
  (unless imagpart
    (setf imagpart (coerce 0 (type-of realpart))))
  (cond ((or (typep realpart 'double-float)
             (typep imagpart 'double-float))
         (let ((r (%double-float-as-integer (float realpart 0.0d0)))
               (i (%double-float-as-integer (float imagpart 0.0d0)))
               (result (mezzano.runtime::%allocate-object +object-tag-complex-double-float+ 0 2 nil)))
           (setf (%object-ref-unsigned-byte-64 result sys.int::+complex-realpart+) r
                 (%object-ref-unsigned-byte-64 result sys.int::+complex-imagpart+) i)
           result))
        ((or (typep realpart 'single-float)
             (typep imagpart 'single-float))
         (let ((r (%single-float-as-integer (float realpart 0.0s0)))
               (i (%single-float-as-integer (float imagpart 0.0s0)))
               (result (mezzano.runtime::%allocate-object +object-tag-complex-single-float+ 0 1 nil)))
           (setf (%object-ref-unsigned-byte-32 result sys.int::+complex-realpart+) r
                 (%object-ref-unsigned-byte-32 result sys.int::+complex-imagpart+) i)
           result))
        ((not (zerop imagpart))
         (let ((result (mezzano.runtime::%allocate-object +object-tag-complex-rational+ 0 2 nil)))
           (setf (%object-ref-t result sys.int::+complex-realpart+) realpart
                 (%object-ref-t result sys.int::+complex-imagpart+) imagpart)
           result))
        (t
         realpart)))

(defun realpart (number)
  (cond
    ((%object-of-type-p number +object-tag-complex-rational+)
     (%object-ref-t number +complex-realpart+))
    ((%object-of-type-p number +object-tag-complex-single-float+)
     (%integer-as-single-float (%object-ref-unsigned-byte-32 number +complex-realpart+)))
    ((%object-of-type-p number +object-tag-complex-double-float+)
     (%integer-as-double-float (%object-ref-unsigned-byte-64 number +complex-realpart+)))
    (t
     (check-type number number)
     number)))

(defun imagpart (number)
  (cond
    ((%object-of-type-p number +object-tag-complex-rational+)
     (%object-ref-t number +complex-imagpart+))
    ((%object-of-type-p number +object-tag-complex-single-float+)
     (%integer-as-single-float (%object-ref-unsigned-byte-32 number +complex-imagpart+)))
    ((%object-of-type-p number +object-tag-complex-double-float+)
     (%integer-as-double-float (%object-ref-unsigned-byte-64 number +complex-imagpart+)))
    (t
     (check-type number number)
     0)))

(defun upgraded-complex-part-type (typespec &optional environment)
  (cond
    ((subtypep typespec 'nil environment)
     nil)
    ((subtypep typespec 'single-float environment)
     'single-float)
    ((subtypep typespec 'double-float environment)
     'double-float)
    ((subtypep typespec 'rational environment)
     'rational)
    ((subtypep typespec 'real environment)
     'real)
    (t
     (error "Type specifier ~S is not a subtype of REAL." typespec))))

(defun expt (base power)
  (etypecase power
    (integer
     (cond ((minusp power)
            (/ (expt base (- power))))
           (t (let ((accum 1))
                (dotimes (i power accum)
                  (setf accum (* accum base)))))))
    (float
     (cond ((eql (float (truncate power) power) power)
            ;; Moderately integer-like?
            (expt base (truncate power)))
           (t
            ;; Slower...
            (exp (* power (log base))))))))

(defstruct (large-byte (:constructor make-large-byte (size position)))
  (size 0 :type (integer 0) :read-only t)
  (position 0 :type (integer 0) :read-only t))

;; Stuff size & position into the low 32-bits.
(defconstant +byte-size+ (byte 14 4))
(defconstant +byte-position+ (byte 14 18))

(deftype byte ()
  `(satisfies bytep))

(defun small-byte-p (object)
  (%value-has-tag-p object +tag-byte-specifier+))

(defun bytep (object)
  (or (small-byte-p object)
      (large-byte-p object)))

(defun fits-in-field-p (bytespec integer)
  "Test if INTEGER fits in the byte defined by BYTESPEC."
  (eql integer (logand integer
                       (1- (ash 1 (byte-size bytespec))))))

(defun byte (size position)
  (if (and (fits-in-field-p +byte-size+ size)
           (fits-in-field-p +byte-position+ position))
      (%%assemble-value (logior (ash size (byte-position +byte-size+))
                                (ash position (byte-position +byte-position+)))
                        +tag-byte-specifier+)
      (make-large-byte size position)))

(defun byte-size (byte-specifier)
  (if (small-byte-p byte-specifier)
      (ldb +byte-size+ (lisp-object-address byte-specifier))
      (large-byte-size byte-specifier)))

(defun byte-position (byte-specifier)
  (if (small-byte-p byte-specifier)
      (ldb +byte-position+ (lisp-object-address byte-specifier))
      (large-byte-position byte-specifier)))

(declaim (inline %ldb ldb %dpb dpb %ldb-test ldb-test logbitp
                 %mask-field mask-field %deposit-field deposit-field))
(defun %ldb (size position integer)
  (logand (ash integer (- position))
          (1- (ash 1 size))))

(defun ldb (bytespec integer)
  (%ldb (byte-size bytespec) (byte-position bytespec) integer))

(defun %dpb (newbyte size position integer)
  (let ((mask (1- (ash 1 size))))
    (logior (ash (logand newbyte mask) position)
            (logand integer (lognot (ash mask position))))))

(defun dpb (newbyte bytespec integer)
  (%dpb newbyte (byte-size bytespec) (byte-position bytespec) integer))

(defun %ldb-test (size position integer)
  (not (eql 0 (%ldb size position integer))))

(defun ldb-test (bytespec integer)
  (%ldb-test (byte-size bytespec) (byte-position bytespec) integer))

(defun logbitp (index integer)
  (ldb-test (byte 1 index) integer))

(defun %mask-field (size position integer)
  (logand integer (%dpb -1 size position 0)))

(defun mask-field (bytespec integer)
  (%mask-field (byte-size bytespec) (byte-position bytespec) integer))

(defun %deposit-field (newbyte size position integer)
  (let ((mask (%dpb -1 size position 0)))
    (logior (logand integer (lognot mask))
            (logand newbyte mask))))

(defun deposit-field (newbyte bytespec integer)
  (%deposit-field newbyte (byte-size bytespec) (byte-position bytespec) integer))

(defun float-nan-p (float)
  (etypecase float
    (single-float
     (let* ((bits (%single-float-as-integer float))
            (exp (ldb (byte 8 23) bits))
            (sig (ldb (byte 23 0) bits)))
       (and (eql exp #xFF)
            (not (zerop sig)))))
    (double-float
     (let* ((bits (%double-float-as-integer float))
            (exp (ldb (byte 11 52) bits))
            (sig (ldb (byte 52 0) bits)))
       (and (eql exp #x7FF)
            (not (zerop sig)))))))

(defun float-trapping-nan-p (float)
  (etypecase float
    (single-float
     (let* ((bits (%single-float-as-integer float))
            (exp (ldb (byte 8 23) bits))
            (sig (ldb (byte 23 0) bits)))
       (and (eql exp #xFF)
            (not (zerop sig))
            (not (logbitp 22 sig)))))
    (double-float
     (let* ((bits (%double-float-as-integer float))
            (exp (ldb (byte 11 52) bits))
            (sig (ldb (byte 52 0) bits)))
       (and (eql exp #x7FF)
            (not (zerop sig))
            (not (logbitp 51 sig)))))))

(defun float-infinity-p (float)
  (etypecase float
    (single-float
     (let* ((bits (%single-float-as-integer float))
            (exp (ldb (byte 8 23) bits))
            (sig (ldb (byte 23 0) bits)))
       (and (eql exp #xFF)
            (zerop sig))))
    (double-float
     (let* ((bits (%double-float-as-integer float))
            (exp (ldb (byte 11 52) bits))
            (sig (ldb (byte 52 0) bits)))
       (and (eql exp #x7FF)
            (zerop sig))))))

(defun %double-float-as-integer (double-float)
  (%object-ref-unsigned-byte-64 double-float 0))

(defun %integer-as-double-float (integer)
  (let ((result (mezzano.runtime::%allocate-object
                 sys.int::+object-tag-double-float+ 0 1 nil)))
    (setf (%object-ref-unsigned-byte-64 result 0) integer)
    result))

(declaim (inline bignump
                 %n-bignum-fragments
                 %bignum-fragment
                 (setf %bignum-fragment)))

(defun bignump (object)
  (%object-of-type-p object +object-tag-bignum+))

(defun %n-bignum-fragments (bignum)
  (%object-header-data bignum))

;; Watch out - this can create another bignum to hold the fragment.
(defun %bignum-fragment (bignum n)
  (%object-ref-unsigned-byte-64 bignum n))

(defun (setf %bignum-fragment) (value bignum n)
  (setf (%object-ref-unsigned-byte-64 bignum n) value))

(defun bignum-to-float (bignum float-zero digits)
  (let* ((negative (minusp bignum))
         (bignum (if negative (- bignum) bignum))
         (length (integer-length bignum))
         (sig (ldb (byte digits (- length digits)) bignum))
         (exp (expt (float 2 float-zero) (- length digits))))
    (* (float sig float-zero) exp)))

(defun mezzano.runtime::%%coerce-bignum-to-single-float (bignum)
  (bignum-to-float bignum 0.0f0 24))

(defun mezzano.runtime::%%coerce-bignum-to-double-float (bignum)
  (bignum-to-float bignum 0.0d0 53))

(declaim (inline call-with-float-contagion))
(defun call-with-float-contagion (x y single-fn double-fn)
  (if (or (double-float-p x)
          (double-float-p y))
      (funcall double-fn
               (float x 1.0d0)
               (float y 1.0d0))
      (funcall single-fn
               (float x 1.0f0)
               (float y 1.0f0))))

(defun sys.int::full-< (x y)
  (check-type x real)
  (check-type y real)
  (cond
    ((and (sys.int::fixnump x)
          (sys.int::fixnump y))
     ;; Should be handled by binary-<.
     (error "FIXNUM/FIXNUM case hit GENERIC-<"))
    ((and (sys.int::fixnump x)
          (sys.int::bignump y))
     (sys.int::%%bignum-< (sys.int::%make-bignum-from-fixnum x) y))
    ((and (sys.int::bignump x)
          (sys.int::fixnump y))
     (sys.int::%%bignum-< x (sys.int::%make-bignum-from-fixnum y)))
    ((and (sys.int::bignump x)
          (sys.int::bignump y))
     (sys.int::%%bignum-< x y))
    ((or (floatp x)
         (floatp y))
     (call-with-float-contagion x y #'%%single-float-< #'%%double-float-<))
    ((or (sys.int::ratiop x)
         (sys.int::ratiop y))
       (< (* (numerator x) (denominator y))
          (* (numerator y) (denominator x))))
    (t (error "TODO... Argument combination ~S and ~S not supported." x y))))

(defun sys.int::full-= (x y)
  (check-type x number)
  (check-type y number)
  ;; Must not use EQ when the arguments are floats.
  (cond
    ((or (complexp x)
         (complexp y))
     (and (= (realpart x) (realpart y))
          (= (imagpart x) (imagpart y))))
    ((or (floatp x)
         (floatp y))
     (call-with-float-contagion x y #'%%single-float-= #'%%double-float-=))
    ((or (sys.int::fixnump x)
         (sys.int::fixnump y))
     (eq x y))
    ((and (sys.int::bignump x)
          (sys.int::bignump y))
     (or (eq x y) (sys.int::%%bignum-= x y)))
    ((or (sys.int::ratiop x)
         (sys.int::ratiop y))
     (and (= (numerator x) (numerator y))
          (= (denominator x) (denominator y))))
    (t (error "TODO... Argument combination ~S and ~S not supported." x y))))

(defun %%bignum-truncate (a b)
  "Divide two integers.
Implements the dumb mp_div algorithm from BigNum Math."
  (when (eql b 0)
    (error 'division-by-zero
           :operands (list a b)
           :operation 'truncate))
  (let ((ta (abs a))
        (tb (abs b))
        (tq 1)
        (q 0)
        (n nil))
    ;; Check for the easy case. |a| < |b| => 0, a
    (when (< ta tb)
      (return-from %%bignum-truncate
        (values 0 a)))
    (setf n (- (integer-length ta) (integer-length tb)))
    (setf tb (ash tb n))
    (setf tq (ash tq n))
    ;; Divide bit-by-bit.
    (dotimes (i (1+ n))
      (when (not (> tb ta))
        (setf ta (- ta tb))
        (setf q (+ tq q)))
      (setf tb (ash tb -1)
            tq (ash tq -1)))
    ;; Quotient in Q, remainder in TA.
    ;; Correct sign.
    (when (not (eql (minusp a) (minusp b)))
      (setf q (- q)))
    (when (minusp a)
      (setf ta (- ta)))
    (values q ta)))

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
                (integer-part (if (< most-negative-fixnum
                                     val
                                     most-positive-fixnum)
                                  ;; Fits in a fixnum, convert quickly.
                                  (etypecase val
                                    (single-float
                                     (%%truncate-single-float val))
                                    (double-float
                                     (%%truncate-double-float val)))
                                  ;; Grovel inside the float
                                  (multiple-value-bind (significand exponent)
                                      (integer-decode-float val)
                                    (ash significand exponent)))))
           (values integer-part (* (- val integer-part) divisor))))
        ((or (sys.int::ratiop number)
             (sys.int::ratiop divisor))
         (let ((val (/ number divisor)))
           (multiple-value-bind (quot rem)
               (truncate (numerator val) (denominator val))
             (values quot (/ rem (denominator val))))))
        (t (check-type number number)
           (check-type divisor number)
           (error "Argument combination ~S and ~S not supported." number divisor))))

(defun sys.int::full-/ (x y)
  (cond ((and (typep x 'integer)
              (typep y 'integer))
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
                    (sys.int::make-ratio (if negative
                                             (- (/ (abs x) gcd))
                                             (/ (abs x) gcd))
                                         (/ (abs y) gcd)))))))
        ((or (complexp x)
             (complexp y))
         (complex (/ (+ (* (realpart x) (realpart y))
                        (* (imagpart x) (imagpart y)))
                     (+ (expt (realpart y) 2)
                        (expt (imagpart y) 2)))
                  (/ (- (* (imagpart x) (realpart y))
                        (* (realpart x) (imagpart y)))
                     (+ (expt (realpart y) 2)
                        (expt (imagpart y) 2)))))
        ((or (floatp x)
             (floatp y))
         (call-with-float-contagion x y #'%%single-float-/ #'%%double-float-/))
        ((or (sys.int::ratiop x) (sys.int::ratiop y))
         (/ (* (numerator x) (denominator y))
            (* (denominator x) (numerator y))))
        (t (error "Argument complex ~S and ~S not supported." x y))))

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
         (complex (+ (realpart x) (realpart y))
                  (+ (imagpart x) (imagpart y))))
        ((or (floatp x)
             (floatp y))
         (call-with-float-contagion x y #'%%single-float-+ #'%%double-float-+))
        ((or (sys.int::ratiop x)
             (sys.int::ratiop y))
         (/ (+ (* (numerator x) (denominator y))
               (* (numerator y) (denominator x)))
            (* (denominator x) (denominator y))))
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
         (complex (- (realpart x) (realpart y))
                  (- (imagpart x) (imagpart y))))
        ((or (floatp x)
             (floatp y))
         (call-with-float-contagion x y #'%%single-float-- #'%%double-float--))
        ((or (sys.int::ratiop x)
             (sys.int::ratiop y))
         (/ (- (* (numerator x) (denominator y))
               (* (numerator y) (denominator x)))
            (* (denominator x) (denominator y))))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination ~S and ~S not supported." x y))))

(defun %%bignum-multiply-unsigned (a b)
  (assert (bignump a))
  (assert (bignump b))
  (let* ((digs (+ (%n-bignum-fragments a)
                  (%n-bignum-fragments b)
                  1))
         (c (%make-bignum-of-length digs)))
    (dotimes (i digs)
      (setf (%bignum-fragment c i) 0))
    (loop for ix from 0 below (%n-bignum-fragments a) do
         (let ((u 0)
               (pb (min (%n-bignum-fragments b)
                        (- digs ix))))
           (when (< pb 1)
             (return))
           (loop for iy from 0 to (1- pb) do
                (let ((r-hat (+ (%bignum-fragment c (+ iy ix))
                                (%%bignum-multiply-step
                                 (%bignum-fragment a ix)
                                 (%bignum-fragment b iy))
                                u)))
                  (setf (%bignum-fragment c (+ iy ix))
                        (ldb (byte 64 0) r-hat))
                  (setf u (ash r-hat -64))))
           (when (< (+ ix pb) digs)
             (setf (%bignum-fragment c (+ ix pb)) u))))
    (%%canonicalize-bignum c)))

(defun %%bignum-multiply-signed (a b)
  "Multiply two integers together. A and B can be bignums or fixnums."
  (let ((a-negative (< a 0))
        (b-negative (< b 0))
        (c nil))
    (when a-negative
      (setf a (- a)))
    (when b-negative
      (setf b (- b)))
    (when (fixnump a)
      (setf a (%make-bignum-from-fixnum a)))
    (when (fixnump b)
      (setf b (%make-bignum-from-fixnum b)))
    (setf c (%%bignum-multiply-unsigned a b))
    (when (not (eql a-negative b-negative))
      (setf c (- c)))
    c))

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
         (complex (- (* (realpart x) (realpart y))
                     (* (imagpart x) (imagpart y)))
                  (+ (* (imagpart x) (realpart y))
                     (* (realpart x) (imagpart y)))))
        ((or (floatp x)
             (floatp y))
         (call-with-float-contagion x y #'%%single-float-* #'%%double-float-*))
        ((or (sys.int::ratiop x)
             (sys.int::ratiop y))
         (/ (* (numerator x) (numerator y))
            (* (denominator x) (denominator y))))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination ~S and ~S not supported." x y))))

(defun abs (number)
  (check-type number number)
  (etypecase number
    (complex (sqrt (+ (expt (realpart number) 2)
                      (expt (imagpart number) 2))))
    (real (if (minusp number)
              (- number)
              number))))

(defun sqrt (number)
  (check-type number number)
  (etypecase number
    (double-float
     (%%double-float-sqrt (float number 0.0d0)))
    (real
     (%%single-float-sqrt (float number 0.0f0)))))

(defun isqrt (number)
  (values (floor (sqrt number))))

(macrolet ((def (name bignum-name)
             `(defun ,name (x y)
                (cond ((and (fixnump x)
                            (fixnump y))
                       (error "FIXNUM/FIXNUM case hit ~S." ',name))
                      ((and (fixnump x)
                            (bignump y))
                       (,bignum-name (%make-bignum-from-fixnum x) y))
                      ((and (bignump x)
                            (fixnump y))
                       (,bignum-name x (%make-bignum-from-fixnum y)))
                      ((and (bignump x)
                            (bignump y))
                       (,bignum-name x y))
                      (t (check-type x integer)
                         (check-type y integer)
                         (error "Argument combination not supported."))))))
  (def generic-logand %%bignum-logand)
  (def generic-logior %%bignum-logior)
  (def generic-logxor %%bignum-logxor))

(defun generic-lognot (integer)
  (logxor integer -1))

(defun logandc1 (integer-1 integer-2)
  "AND complement of INTEGER-1 with INTEGER-2."
  (logand (lognot integer-1) integer-2))

(defun logandc2 (integer-1 integer-2)
  "AND INTEGER-1 with complement of INTEGER-2."
  (logand integer-1 (lognot integer-2)))

(defun lognand (integer-1 integer-2)
  "Complement of INTEGER-1 AND INTEGER-2."
  (lognot (logand integer-1 integer-2)))

(defun lognor (integer-1 integer-2)
  "Complement of INTEGER-1 OR INTEGER-2."
  (lognot (logior integer-1 integer-2)))

(defun logorc1 (integer-1 integer-2)
  "OR complement of INTEGER-1 with INTEGER-2."
  (logior (lognot integer-1) integer-2))

(defun logorc2 (integer-1 integer-2)
  "OR INTEGER-1 with complement of INTEGER-2."
  (logior integer-1 (lognot integer-2)))

(defconstant boole-1 'boole-1 "integer-1")
(defconstant boole-2 'boole-2 "integer-2")
(defconstant boole-andc1 'boole-andc1 "and complement of integer-1 with integer-2")
(defconstant boole-andc2 'boole-andc2 "and integer-1 with complement of integer-2")
(defconstant boole-and 'boole-and "and")
(defconstant boole-c1 'boole-c1 "complement of integer-1")
(defconstant boole-c2 'boole-c2 "complement of integer-2")
(defconstant boole-clr 'boole-clr "always 0 (all zero bits)")
(defconstant boole-eqv 'boole-eqv "equivalence (exclusive nor)")
(defconstant boole-ior 'boole-ior "inclusive or")
(defconstant boole-nand 'boole-nand "not-and")
(defconstant boole-nor 'boole-nor "not-or")
(defconstant boole-orc1 'boole-orc1 "or complement of integer-1 with integer-2")
(defconstant boole-orc2 'boole-orc2 "or integer-1 with complement of integer-2")
(defconstant boole-set 'boole-set "always -1 (all one bits)")
(defconstant boole-xor 'boole-xor "exclusive or")

(defun boole (op integer-1 integer-2)
  "Perform bit-wise logical OP on INTEGER-1 and INTEGER-2."
  (check-type integer-1 integer)
  (check-type integer-2 integer)
  (ecase op
    (boole-1 integer-1)
    (boole-2 integer-2)
    (boole-andc1 (logandc1 integer-1 integer-2))
    (boole-andc2 (logandc2 integer-1 integer-2))
    (boole-and (logand integer-1 integer-2))
    (boole-c1 (lognot integer-1))
    (boole-c2 (lognot integer-2))
    (boole-clr 0)
    (boole-eqv (logeqv integer-1 integer-2))
    (boole-ior (logior integer-1 integer-2))
    (boole-nand (lognand integer-1 integer-2))
    (boole-nor (lognor integer-1 integer-2))
    (boole-orc1 (logorc1 integer-1 integer-2))
    (boole-orc2 (logorc2 integer-1 integer-2))
    (boole-set -1)
    (boole-xor (logxor integer-1 integer-2))))

(defun signum (number)
  (if (zerop number)
      number
      (/ number (abs number))))

;;; Mathematical horrors!

(defconstant pi 3.14159265358979323846264338327950288419716939937511d0)

;;; Derived from SLEEF: https://github.com/shibatch/sleef

(defconstant +sleef-pi4-af+ 0.78515625f0)
(defconstant +sleef-pi4-bf+ 0.00024187564849853515625f0)
(defconstant +sleef-pi4-cf+ 3.7747668102383613586f-08)
(defconstant +sleef-pi4-df+ 1.2816720341285448015f-12)

(defun sleef-mulsignf (x y)
  (sys.int::%integer-as-single-float
   (logxor
    (sys.int::%single-float-as-integer x)
    (logand (sys.int::%single-float-as-integer y)
            (ash 1 31)))))

(defun sleef-signf (d)
  (sleef-mulsignf 1.0f0 d))

(defun sleef-mlaf (x y z)
  (+ (* x y) z))

(defun sleef-rintf (x)
  (if (< x 0)
      (truncate (- x 0.5f0))
      (truncate (+ x 0.5f0))))

(defconstant +sleef-pi4-a+ 0.78539816290140151978d0)
(defconstant +sleef-pi4-b+ 4.9604678871439933374d-10)
(defconstant +sleef-pi4-c+ 1.1258708853173288931d-18)
(defconstant +sleef-pi4-d+ 1.7607799325916000908d-27)

(defun sleef-mla (x y z)
  (+ (* x y) z))

(defun sleef-rint (x)
  (if (< x 0)
      (truncate (- x 0.5d0))
      (truncate (+ x 0.5d0))))

(defun sin-single-float (d)
  (let ((q 0)
        (u 0.0f0)
        (s 0.0f0))
    (setf q (sleef-rintf (* d (/ (float pi 0.0f0)))))

    (setf d (sleef-mlaf q (* +sleef-pi4-af+ -4) d))
    (setf d (sleef-mlaf q (* +sleef-pi4-bf+ -4) d))
    (setf d (sleef-mlaf q (* +sleef-pi4-cf+ -4) d))
    (setf d (sleef-mlaf q (* +sleef-pi4-df+ -4) d))

    (setf s (* d d))

    (when (logtest q 1)
      (setf d (- d)))

    (finish-sincos-single-float s d)))

(defun cos-single-float (d)
  (let ((q 0)
        (u 0.0f0)
        (s 0.0f0))
    (setf q (+ 1 (* 2 (sleef-rintf (- (* d (/ (float pi 0.0f0))) 0.5f0)))))

    (setf d (sleef-mlaf q (* +sleef-pi4-af+ -2) d))
    (setf d (sleef-mlaf q (* +sleef-pi4-bf+ -2) d))
    (setf d (sleef-mlaf q (* +sleef-pi4-cf+ -2) d))
    (setf d (sleef-mlaf q (* +sleef-pi4-df+ -2) d))

    (setf s (* d d))

    (when (not (logtest q 2))
      (setf d (- d)))

    (finish-sincos-single-float s d)))

(defun finish-sincos-single-float (s d)
  (let ((u 2.6083159809786593541503f-06))
    (setf u (sleef-mlaf u s -0.0001981069071916863322258f0))
    (setf u (sleef-mlaf u s 0.00833307858556509017944336f0))
    (setf u (sleef-mlaf u s -0.166666597127914428710938f0))

    (setf u (sleef-mlaf s (* u d) d))

    (cond ((float-infinity-p d)
           (/ 0.0f0 0.0f0))
          (t
           u))))

(defun sin-double-float (d)
  (let ((q 0)
        (u 0.0d0)
        (s 0.0d0))
    (setf q (sleef-rint (* d (/ (float pi 0.0d0)))))

    (setf d (sleef-mla q (* +sleef-pi4-a+ -4) d))
    (setf d (sleef-mla q (* +sleef-pi4-b+ -4) d))
    (setf d (sleef-mla q (* +sleef-pi4-c+ -4) d))
    (setf d (sleef-mla q (* +sleef-pi4-d+ -4) d))

    (setf s (* d d))

    (when (logtest q 1)
      (setf d (- d)))

    (finish-sincos-double-float s d)))

(defun cos-double-float (d)
  (let ((q 0)
        (u 0.0d0)
        (s 0.0d0))
    (setf q (+ 1 (* 2 (sleef-rint (- (* d (/ (float pi 0.0d0))) 0.5d0)))))

    (setf d (sleef-mla q (* +sleef-pi4-a+ -2) d))
    (setf d (sleef-mla q (* +sleef-pi4-b+ -2) d))
    (setf d (sleef-mla q (* +sleef-pi4-c+ -2) d))
    (setf d (sleef-mla q (* +sleef-pi4-d+ -2) d))

    (setf s (* d d))

    (when (not (logtest q 2))
      (setf d (- d)))

    (finish-sincos-double-float s d)))

(defun finish-sincos-double-float (s d)
  (let ((u -7.97255955009037868891952d-18))
    (setf u (sleef-mla u s 2.81009972710863200091251d-15))
    (setf u (sleef-mla u s -7.64712219118158833288484d-13))
    (setf u (sleef-mla u s 1.60590430605664501629054d-10))
    (setf u (sleef-mla u s -2.50521083763502045810755d-08))
    (setf u (sleef-mla u s 2.75573192239198747630416d-06))
    (setf u (sleef-mla u s -0.000198412698412696162806809d0))
    (setf u (sleef-mla u s 0.00833333333333332974823815d0))
    (setf u (sleef-mla u s -0.166666666666666657414808d0))

    (sleef-mla s (* u d) d)))

(defun sin (x)
  (etypecase x
    (complex
     (let ((real (realpart x))
           (imag (imagpart x)))
       (complex (* (sin real) (cosh imag))
                (* (cos real) (sinh imag)))))
    (double-float
     (sin-double-float x))
    (real
     (sin-single-float (float x 0.0f0)))))

(defun cos (x)
  (etypecase x
    (complex
     (let ((real (realpart x))
           (imag (imagpart x)))
       (complex (* (cos real) (cosh imag))
                (- (* (sin real) (sinh imag))))))
    (double-float
     (cos-double-float x))
    (real
     (cos-single-float (float x 0.0f0)))))

(defun tan (d)
  (let (q u s x)
    (setf q (sleef-rintf (* d 2 (/ (float pi 0.0f0)))))

    (setf x d)

    (setf x (sleef-mlaf q (* +sleef-pi4-af+ -4 0.5f0) x))
    (setf x (sleef-mlaf q (* +sleef-pi4-bf+ -4 0.5f0) x))
    (setf x (sleef-mlaf q (* +sleef-pi4-cf+ -4 0.5f0) x))
    (setf x (sleef-mlaf q (* +sleef-pi4-df+ -4 0.5f0) x))

    (setf s (* x x))

    (when (not (eql (logand q 1) 0))
      (setf x (- x)))

    (setf u 0.00927245803177356719970703f0)
    (setf u (sleef-mlaf u s 0.00331984995864331722259521f0))
    (setf u (sleef-mlaf u s 0.0242998078465461730957031f0))
    (setf u (sleef-mlaf u s 0.0534495301544666290283203f0))
    (setf u (sleef-mlaf u s 0.133383005857467651367188f0))
    (setf u (sleef-mlaf u s 0.333331853151321411132812f0))

    (setf u (sleef-mlaf s (* u x) x))

    (when (not (eql (logand q 1) 0))
      (setf u (/ u)))

    (when (float-infinity-p d)
      (setf u (/ 0.0f0 0.0f0)))

    u))

(defconstant +sleef-r-ln2f+ 1.442695040888963407359924681001892137426645954152985934135449406931f0)
(defconstant +sleef-l2uf+ 0.693145751953125f0)
(defconstant +sleef-l2lf+ 1.428606765330187045f-06)

(defun sleef-ldexpkf (x q)
  (let (u m)
    (setf m (ash q -31))
    (setf m (ash (- (ash (+ m q) -6) m) 4))
    (setf q (- q (ash m 2)))
    (incf m 127)
    (setf m (if (< m 0) 0 m))
    (setf m (if (> m 255) 255 m))
    (setf u (sys.int::%integer-as-single-float (ash m 23)))
    (setf x (* x u u u u))
    (setf u (sys.int::%integer-as-single-float (ash (+ q #x7f) 23)))
    (* x u)))

(defun sleef-ilogbkf (d)
  (let (m q)
    (setf m (< d 5.421010862427522f-20))
    (setf d (if m
                (* 1.8446744073709552f19 d)
                d))
    (setf q (logand (ash (sys.int::%single-float-as-integer d) -23) #xFF))
    (if m
        (- q (+ 64 #x7F))
        (- q #x7F))))

(defun exp (number)
  (let ((d (float number 0.0f0))
        q s u)
    (setf q (sleef-rintf (* d +sleef-r-ln2f+)))
    (setf s (sleef-mlaf q (- +sleef-l2uf+) d))
    (setf s (sleef-mlaf q (- +sleef-l2lf+) s))

    (setf u 0.000198527617612853646278381f0)
    (setf u (sleef-mlaf u s 0.00139304355252534151077271f0))
    (setf u (sleef-mlaf u s 0.00833336077630519866943359f0))
    (setf u (sleef-mlaf u s 0.0416664853692054748535156f0))
    (setf u (sleef-mlaf u s 0.166666671633720397949219f0))
    (setf u (sleef-mlaf u s 0.5f0))

    (setf u (+ (* s s u) s 1.0f0))
    (setf u (sleef-ldexpkf u q))

    (if (< d -104) (setf u 0))
    (if (> d  104) (setf u single-float-positive-infinity))

    (if (floatp number)
        (float u number)
        u)))

(defun log-e (number)
  (let ((d (float number 0.0f0))
        x x2 tt m e)
    (setf e (sleef-ilogbkf (* d (/ 0.75f0))))
    (setf m (sleef-ldexpkf d (- e)))

    (setf x (/ (- m 1.0f0) (+ m 1.0f0)))
    (setf x2 (* x x))

    (setf tt 0.2392828464508056640625f0)
    (setf tt (sleef-mlaf tt x2 0.28518211841583251953125f0))
    (setf tt (sleef-mlaf tt x2 0.400005877017974853515625f0))
    (setf tt (sleef-mlaf tt x2 0.666666686534881591796875f0))
    (setf tt (sleef-mlaf tt x2 2.0f0))

    (setf x (+ (* x tt) (* 0.693147180559945286226764f0 e)))

    (when (float-infinity-p d)
      (setf x single-float-positive-infinity))
    (when (< d 0)
      (setf x (/ 0.0f0 0.0f0)))
    (when (= d 0)
      (setf x single-float-negative-infinity))

    (if (floatp number)
        (float x number)
        x)))

(defun log (number &optional base)
  (if base
      (/ (log number) (log base))
      (log-e number)))

(defun atan (number1 &optional number2)
  (if number2
      (atan2 number1 number2)
      (let ((s (float number1 0.0f0))
            (q 0)
            tt u)
        (when (= (sleef-signf s) -1)
          (setf s (- s))
          (setf q 2))
        (when (> s 1)
          (setf s (/ s))
          (setf q (logior q 1)))

        (setf tt (* s s))

        (setf u 0.00282363896258175373077393f0)
        (setf u (sleef-mlaf u tt -0.0159569028764963150024414f0))
        (setf u (sleef-mlaf u tt 0.0425049886107444763183594f0))
        (setf u (sleef-mlaf u tt -0.0748900920152664184570312f0))
        (setf u (sleef-mlaf u tt 0.106347933411598205566406f0))
        (setf u (sleef-mlaf u tt -0.142027363181114196777344f0))
        (setf u (sleef-mlaf u tt 0.199926957488059997558594f0))
        (setf u (sleef-mlaf u tt -0.333331018686294555664062f0))

        (setf tt (+ s (* s tt u)))

        (when (not (eql (logand q 1) 0))
          (setf tt (- 1.570796326794896557998982f0 tt)))
        (when (not (eql (logand q 2) 0))
          (setf tt (- tt)))
        (if (floatp number1)
            (float tt number1)
            tt))))

(defun atan2 (y x)
  (cond ((> x 0) (atan (/ y x)))
        ((and (>= y 0) (< x 0))
         (+ (atan (/ y x)) pi))
        ((and (< y 0) (< x 0))
         (- (atan (/ y x)) pi))
        ((and (> y 0) (zerop x))
         (/ pi 2))
        ((and (< y 0) (zerop x))
         (- (/ pi 2)))
        (t 0)))

(defun two-arg-gcd (a b)
  (check-type a integer)
  (check-type b integer)
  (setf a (abs a))
  (setf b (abs b))
  (loop (when (zerop b)
          (return a))
     (psetf b (mod a b)
            a b)))

(defun conjugate (number)
  (if (complexp number)
      (complex (realpart number)
               (- (imagpart number)))
      number))

(defun phase (number)
  (atan (imagpart number) (realpart number)))

(defun ffloor (number &optional (divisor 1))
  (multiple-value-bind (quotient remainder)
      (floor number divisor)
    (values (float quotient (if (or (double-float-p number)
                                    (double-float-p divisor))
                                0.0d0
                                0.0f0))
            remainder)))

(defun fceiling (number &optional (divisor 1))
  (multiple-value-bind (quotient remainder)
      (ceiling number divisor)
    (values (float quotient (if (or (double-float-p number)
                                    (double-float-p divisor))
                                0.0d0
                                0.0f0))
            remainder)))

(defun ftruncate (number &optional (divisor 1))
  (multiple-value-bind (quotient remainder)
      (truncate number divisor)
    (values (float quotient (if (or (double-float-p number)
                                    (double-float-p divisor))
                                0.0d0
                                0.0f0))
            remainder)))

(defun fround (number &optional (divisor 1))
  (multiple-value-bind (quotient remainder)
      (round number divisor)
    (values (float quotient (if (or (double-float-p number)
                                    (double-float-p divisor))
                                0.0d0
                                0.0f0))
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

(defun integer-decode-float (float)
  (etypecase float
    (single-float (integer-decode-single-float float))
    (double-float (integer-decode-double-float float))))

(defun float-sign (float1 &optional (float2 (float 1 float1)))
  "Return a floating-point number that has the same sign as
   FLOAT1 and, if FLOAT2 is given, has the same absolute value
   as FLOAT2."
  (check-type float1 float)
  (check-type float2 float)
  (* (if (etypecase float1
           (single-float (logbitp 31 (%single-float-as-integer float1)))
           (double-float (logbitp 63 (%double-float-as-integer float1))))
         (float -1 float1)
         (float 1 float1))
     (abs float2)))

(defun float-digits (f)
  (check-type f float)
  (etypecase f
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
     (decode-double-float f))))

(defun rational (number)
  (check-type number real)
  (etypecase number
    (rational
     number)
    (float
     (multiple-value-bind (significand exponent sign)
         (integer-decode-float number)
       (* significand (expt (float-radix number) exponent) sign)))))

(defun rationalize (number)
  (rational number))
