;;;; Ratios

(in-package :mezzano.internals.numbers.ratio)

(declaim (inline ratiop))
(defun ratiop (object)
  (int::%object-of-type-p object int::+object-tag-ratio+))

(defun numerator (rational)
  (etypecase rational
    (ratio (int::%object-ref-t rational int::+ratio-numerator+))
    (integer rational)))

(defun denominator (rational)
  (etypecase rational
    (ratio (int::%object-ref-t rational int::+ratio-denominator+))
    (integer 1)))

(defun %make-ratio (numerator denominator)
  (let ((value (mezzano.runtime::%allocate-object
                int::+object-tag-ratio+ 0 2 nil)))
    (setf (int::%object-ref-t value int::+ratio-numerator+) numerator
          (int::%object-ref-t value int::+ratio-denominator+) denominator)
    value))

(declaim (inline ratio-= ratio-<))
(defun ratio-= (x y)
  (and (= (numerator x) (numerator y))
       (= (denominator x) (denominator y))))

(defun ratio-< (x y)
  (< (* (numerator x) (denominator y))
     (* (numerator y) (denominator x))))

(defun ratio-+ (x y)
  (cond ((eql x 0) y)
        ((eql y 0) x)
        (t
         (/ (+ (* (numerator x) (denominator y))
               (* (numerator y) (denominator x)))
            (* (denominator x) (denominator y))))))

(defun ratio-- (x y)
  (cond ((eql y 0) x)
        ((eql x 0)
         (%make-ratio (- (numerator y)) (denominator y)))
        (t
         (/ (- (* (numerator x) (denominator y))
               (* (numerator y) (denominator x)))
            (* (denominator x) (denominator y))))))

(defun ratio-* (x y)
  (cond ((eql x 1) y)
        ((eql y 1) x)
        ((or (eql x 0) (eql y 0)) 0)
        (t
         (/ (* (numerator x) (numerator y))
            (* (denominator x) (denominator y))))))

(defun ratio-/ (x y)
  (cond ((eql x 0) 0)
        ((eql y 1) x)
        ((eql y -1) (- x))
        (t
         (/ (* (numerator x) (denominator y))
            (* (denominator x) (numerator y))))))

(defun ratio-truncate (number divisor)
  (cond ((integerp number)
         (multiple-value-bind (quot rem)
             (truncate (* number (denominator divisor))
                       (numerator divisor))
           (values quot (/ rem (denominator divisor)))))
        (t
         (let ((q (truncate (numerator number)
                            (* (denominator number) divisor))))
           (values q (- number (* q divisor)))))))

;;; Ratio to float conversion from SBCL 1.4.2

;;; These functions let us create floats from bits with the
;;; significand uniformly represented as an integer. This is less
;;; efficient for double floats, but is more convenient when making
;;; special values, etc.
(defun single-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 24) sig)
           (type (unsigned-byte 8) exp))
  (int::%integer-as-single-float
   (dpb exp int::+single-float-exponent-byte+
        (dpb sig int::+single-float-significand-byte+
             (if (zerop sign) 0 #x80000000)))))
(defun double-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 53) sig)
           (type (unsigned-byte 11) exp))
  (int::%integer-as-double-float
   (logior (ash (dpb exp int::+double-float-exponent-byte+
                     (dpb (ash sig -32)
                          int::+double-float-significand-byte+
                          (if (zerop sign) 0 #x80000000)))
                32)
           (ldb (byte 32 0) sig))))
(defun short-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 11) sig)
           (type (unsigned-byte 5) exp))
  (int::%integer-as-short-float
   (dpb exp int::+short-float-exponent-byte+
        (dpb sig int::+short-float-significand-byte+
             (if (zerop sign) 0 #x8000)))))

;;; Convert a ratio to a float. We avoid any rounding error by doing an
;;; integer division. Accuracy is important to preserve print-read
;;; consistency, since this is ultimately how the reader reads a float. We
;;; scale the numerator by a power of two until the division results in the
;;; desired number of fraction bits, then do round-to-nearest.
(defun ratio-to-float (x format)
  (let* ((signed-num (numerator x))
         (plusp (plusp signed-num))
         (num (if plusp signed-num (- signed-num)))
         (den (denominator x))
         (digits (ecase format
                   (short-float int::+short-float-digits+)
                   (single-float int::+single-float-digits+)
                   (double-float int::+double-float-digits+)))
         (scale 0))
    (declare (type fixnum digits scale))
    ;; Strip any trailing zeros from the denominator and move it into the scale
    ;; factor (to minimize the size of the operands.)
    (let ((den-twos (1- (integer-length (logxor den (1- den))))))
      (declare (type fixnum den-twos))
      (decf scale den-twos)
      (setq den (ash den (- den-twos))))
    ;; Guess how much we need to scale by from the magnitudes of the numerator
    ;; and denominator. We want one extra bit for a guard bit.
    (let* ((num-len (integer-length num))
           (den-len (integer-length den))
           (delta (- den-len num-len))
           (shift (1+ (the fixnum (+ delta digits))))
           (shifted-num (ash num shift)))
      (declare (type fixnum delta shift))
      (decf scale delta)
      (labels ((float-and-scale (bits)
                 (let* ((bits (ash bits -1))
                        (len (integer-length bits)))
                   (cond ((> len digits)
                          (assert (= len (the fixnum (1+ digits))))
                          (scale-float (floatit (ash bits -1)) (1+ scale)))
                         (t
                          (scale-float (floatit bits) scale)))))
               (floatit (bits)
                 (let ((sign (if plusp 0 1)))
                   (case format
                     (short-float
                      (short-from-bits sign int::+short-float-bias+ bits))
                     (single-float
                      (single-from-bits sign int::+single-float-bias+ bits))
                     (double-float
                      (double-from-bits sign int::+double-float-bias+ bits))))))
        (loop
          (multiple-value-bind (fraction-and-guard rem)
              (truncate shifted-num den)
            (let ((extra (- (integer-length fraction-and-guard) digits)))
              (declare (type fixnum extra))
              (cond ((/= extra 1)
                     (assert (> extra 1)))
                    ((oddp fraction-and-guard)
                     (return
                      (if (zerop rem)
                          (float-and-scale
                           (if (zerop (logand fraction-and-guard 2))
                               fraction-and-guard
                               (1+ fraction-and-guard)))
                          (float-and-scale (1+ fraction-and-guard)))))
                    (t
                     (return (float-and-scale fraction-and-guard)))))
            (setq shifted-num (ash shifted-num -1))
            (incf scale)))))))
