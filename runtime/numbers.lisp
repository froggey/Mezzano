;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

(declaim (inline integerp))
(defun integerp (object)
  (or (sys.int::fixnump object)
      (sys.int::bignump object)))

(declaim (inline sys.int::single-float-p sys.int::double-float-p))
(defun sys.int::single-float-p (object)
  (sys.int::%value-has-tag-p object sys.int::+tag-single-float+))

(defun sys.int::double-float-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-double-float+))

(defun floatp (object)
  (or (sys.int::single-float-p object)
      (sys.int::double-float-p object)))

(defun rationalp (object)
  (or (integerp object)
      (sys.int::ratiop object)))

(defun realp (object)
  (or (rationalp object)
      (floatp object)))

(defun numberp (object)
  (or (realp object)
      (complexp object)))

(defun float (number &optional prototype)
  (etypecase number
    (single-float
     (etypecase prototype
       ((or null single-float)
        number)
       (double-float
        (%%coerce-single-float-to-double-float number))))
    (double-float
     (etypecase prototype
       (single-float
        (%%coerce-double-float-to-single-float number))
       ((or null double-float)
        number)))
    (fixnum
     (etypecase prototype
       ((or null single-float)
        (%%coerce-fixnum-to-single-float number))
       (double-float
        (%%coerce-fixnum-to-double-float number))))
    (bignum
     (etypecase prototype
       ((or null single-float)
        (%%coerce-bignum-to-single-float number))
       (double-float
        (%%coerce-bignum-to-double-float number))))
    (ratio
     (/ (float (numerator number) prototype)
        (float (denominator number) prototype)))))

(defun sys.int::generic-< (x y)
  (cond
    ((or (and (sys.int::single-float-p x)
              (sys.int::fixnump y))
         (and (sys.int::fixnump x)
              (sys.int::single-float-p y))
         (and (sys.int::single-float-p x)
              (sys.int::single-float-p y)))
     (let ((x* (if (sys.int::single-float-p y)
                   (float x y)
                   x))
           (y* (if (sys.int::single-float-p x)
                   (float y x)
                   y)))
       (sys.int::%%single-float-< x* y*)))
    (t (sys.int::full-< x y))))

;; Implement these in terms of <.
(defun sys.int::generic->= (x y)
  (not (sys.int::generic-< x y)))

(defun sys.int::generic-> (x y)
  (sys.int::generic-< y x))

(defun sys.int::generic-<= (x y)
  (not (sys.int::generic-< y x)))

(defun sys.int::generic-= (x y)
  (cond
    ((or (and (sys.int::single-float-p x)
              (sys.int::fixnump y))
         (and (sys.int::fixnump x)
              (sys.int::single-float-p y))
         (and (sys.int::single-float-p x)
              (sys.int::single-float-p y)))
     ;; Convert both arguments to the same kind of float.
     (let ((x* (if (sys.int::single-float-p y)
                   (float x y)
                   x))
           (y* (if (sys.int::single-float-p x)
                   (float y x)
                   y)))
       (sys.int::%%single-float-= x* y*)))
    (t (sys.int::full-= x y))))

(defun sys.int::generic-truncate (number divisor)
  (assert (/= divisor 0) (number divisor) 'division-by-zero)
  (cond
    ((and (sys.int::fixnump number)
          (eq divisor -1))
     ;; This is needed as most-negative-fixnum / -1 will overflow and produce
     ;; a bignum.
     (values (- number)
             0))
    ((or (and (sys.int::single-float-p number)
              (sys.int::fixnump divisor))
         (and (sys.int::fixnump number)
              (sys.int::single-float-p divisor))
         (and (sys.int::single-float-p number)
              (sys.int::single-float-p divisor)))
     (let* ((val (/ number divisor))
            (integer-part (if (< most-negative-fixnum
                                 val
                                 most-positive-fixnum)
                              ;; Fits in a fixnum, convert quickly.
                              (sys.int::%%truncate-single-float val)
                              ;; Grovel inside the float
                              (multiple-value-bind (significand exponent)
                                  (integer-decode-float val)
                                (ash significand exponent)))))
       (values integer-part (* (- val integer-part) divisor))))
    (t (sys.int::full-truncate number divisor))))

;;; From SBCL 1.0.55
(defun ceiling (number &optional (divisor 1))
  ;; If the numbers do not divide exactly and the result of
  ;; (/ NUMBER DIVISOR) would be positive then increment the quotient
  ;; and decrement the remainder by the divisor.
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
             (if (minusp divisor)
                 (minusp number)
                 (plusp number)))
        (values (+ tru 1) (- rem divisor))
        (values tru rem))))

;;; From SBCL 1.0.55
(defun floor (number &optional (divisor 1))
  ;; If the numbers do not divide exactly and the result of
  ;; (/ NUMBER DIVISOR) would be negative then decrement the quotient
  ;; and augment the remainder by the divisor.
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
             (if (minusp divisor)
                 (plusp number)
                 (minusp number)))
        (values (1- tru) (+ rem divisor))
        (values tru rem))))

;; From SBCL 1.0.55
(defun round (number &optional (divisor 1))
  "Rounds number (or number/divisor) to nearest integer.
  The second returned value is the remainder."
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (zerop rem)
        (values tru rem)
        (let ((thresh (/ (abs divisor) 2)))
          (cond ((or (> rem thresh)
                     (and (= rem thresh) (oddp tru)))
                 (if (minusp divisor)
                     (values (- tru 1) (+ rem divisor))
                     (values (+ tru 1) (- rem divisor))))
                ((let ((-thresh (- thresh)))
                   (or (< rem -thresh)
                       (and (= rem -thresh) (oddp tru))))
                 (if (minusp divisor)
                     (values (+ tru 1) (- rem divisor))
                     (values (- tru 1) (+ rem divisor))))
                (t (values tru rem)))))))

(defun sys.int::generic-rem (number divisor)
  (multiple-value-bind (quot rem)
      (sys.int::generic-truncate number divisor)
    (declare (ignore quot))
    rem))

(defun mod (number divisor)
  (multiple-value-bind (quot rem)
      (floor number divisor)
    (declare (ignore quot))
    rem))

(defun sys.int::binary-/ (x y)
  (cond ((or (and (sys.int::single-float-p x)
                  (sys.int::fixnump y))
             (and (sys.int::fixnump x)
                  (sys.int::single-float-p y))
             (and (sys.int::single-float-p x)
                  (sys.int::single-float-p y)))
         (sys.int::%%single-float-/ (float x) (float y)))
        (t (sys.int::full-/ x y))))

(defun sys.int::generic-+ (x y)
  (cond ((or (and (sys.int::single-float-p x)
                  (sys.int::fixnump y))
             (and (sys.int::fixnump x)
                  (sys.int::single-float-p y))
             (and (sys.int::single-float-p x)
                  (sys.int::single-float-p y)))
         ;; Convert both arguments to the same kind of float.
         (let ((x* (if (sys.int::single-float-p y)
                       (float x y)
                       x))
               (y* (if (sys.int::single-float-p x)
                       (float y x)
                       y)))
           (sys.int::%%single-float-+ x* y*)))
        (t (sys.int::full-+ x y))))

(defun sys.int::generic-- (x y)
  (cond ((or (and (sys.int::single-float-p x)
                  (sys.int::fixnump y))
             (and (sys.int::fixnump x)
                  (sys.int::single-float-p y))
             (and (sys.int::single-float-p x)
                  (sys.int::single-float-p y)))
         ;; Convert both arguments to the same kind of float.
         (let ((x* (if (sys.int::single-float-p y)
                       (float x y)
                       x))
               (y* (if (sys.int::single-float-p x)
                       (float y x)
                       y)))
           (sys.int::%%single-float-- x* y*)))
        (t (sys.int::full-- x y))))

(defun sys.int::generic-* (x y)
  (cond ((or (and (sys.int::single-float-p x)
                  (sys.int::fixnump y))
             (and (sys.int::fixnump x)
                  (sys.int::single-float-p y))
             (and (sys.int::single-float-p x)
                  (sys.int::single-float-p y)))
         ;; Convert both arguments to the same kind of float.
         (let ((x* (if (sys.int::single-float-p y)
                       (float x y)
                       x))
               (y* (if (sys.int::single-float-p x)
                       (float y x)
                       y)))
           (sys.int::%%single-float-* x* y*)))
        (t (sys.int::full-* x y))))

(defun integer-length (integer)
  (let ((negativep (minusp integer)))
    (when negativep
      (setf integer (- integer)))
    (do ((len 0 (1+ len))
         (original integer))
        ((zerop integer)
         ;; Negative powers of two require one less bit.
         (if (and negativep
                  ;; Test if original is power-of-two.
                  (zerop (logand original (1- original))))
             (1- len)
             len))
      (setf integer (ash integer -1)))))

(declaim (inline left-shift right-shift))

(defun left-shift (integer count)
  (cond ((and (sys.int::fixnump integer)
              (sys.int::fixnump count))
         (%fixnum-left-shift integer count))
        (t
         (generic-left-shift integer count))))

(defun right-shift (integer count)
  (cond ((and (sys.int::fixnump integer)
              (sys.int::fixnump count))
         (%fixnum-right-shift integer count))
        (t
         (generic-right-shift integer count))))

(defun generic-left-shift (integer count)
  (cond ((not (sys.int::fixnump count))
         (check-type count integer)
         (error "TODO: Bignum LEFT-SHIFT count not implemented yet."))
        ((sys.int::fixnump integer)
         (%fixnum-left-shift integer count))
        ((sys.int::bignump integer)
         (multiple-value-bind (quot rem)
             (truncate count 32)
           (dotimes (i quot)
             (setf integer (sys.int::%%bignum-left-shift integer 32)))
           (sys.int::%%bignum-left-shift integer rem)))
        (t
         (check-type integer integer))))

(defun generic-right-shift (integer count)
  (cond ((not (sys.int::fixnump count))
         (check-type count integer)
         (check-type integer integer)
         (if (minusp integer)
             -1
             0))
        ((sys.int::fixnump integer)
         (%fixnum-right-shift integer count))
        ((sys.int::bignump integer)
         (multiple-value-bind (quot rem)
             (truncate count 32)
           (dotimes (i quot)
             (setf integer (sys.int::%%bignum-right-shift integer 32))
             (cond ((eql integer 0)
                    (return-from generic-right-shift 0))
                   ((eql integer -1)
                    (return-from generic-right-shift -1))
                   ((sys.int::fixnump integer)
                    (setf integer (sys.int::%make-bignum-from-fixnum integer)))))
           (sys.int::%%bignum-right-shift integer rem)))
        (t
         (check-type integer integer))))

(defun ash (integer count)
  (cond
    ((> count 0)
     (left-shift integer count))
    ((< count 0)
     (right-shift integer (- count)))
    (t
     (check-type integer integer)
     (check-type count integer)
     (assert (eql count 0))
     integer)))

(declaim (inline sys.int::binary-=
                 sys.int::binary-< sys.int::binary-<=
                 sys.int::binary-> sys.int::binary->=))

(defun sys.int::binary-= (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (eq lhs rhs)
      (sys.int::generic-= lhs rhs)))

(defun sys.int::binary-< (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (%fixnum-< lhs rhs)
      (sys.int::generic-< lhs rhs)))

(defun sys.int::binary-<= (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (not (%fixnum-< rhs lhs))
      (sys.int::generic-<= lhs rhs)))

(defun sys.int::binary-> (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (%fixnum-< rhs lhs)
      (sys.int::generic-> lhs rhs)))

(defun sys.int::binary->= (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (not (%fixnum-< lhs rhs))
      (sys.int::generic->= lhs rhs)))
