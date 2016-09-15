;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

(declaim (inline integerp))
(defun integerp (object)
  (or (system:fixnump object)
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

#+x86-64
(sys.int::define-lap-function %%coerce-fixnum-to-single-float ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:cvtsi2ss64 :xmm0 :rax)
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

#+x86-64
(sys.int::define-lap-function %%coerce-double-float-to-single-float ()
  (sys.lap-x86:cvtsd2ss64 :xmm0 (:object :r8 0))
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

#+x86-64
(sys.int::define-lap-function %%coerce-fixnum-to-double-float ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:cvtsi2sd64 :xmm0 :rax)
  (sys.lap-x86:movq :rax :xmm0)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-double-float-rax))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

#+x86-64
(sys.int::define-lap-function %%coerce-single-float-to-double-float ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:cvtss2sd64 :xmm0 :xmm0)
  (sys.lap-x86:movq :rax :xmm0)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-double-float-rax))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

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

#+x86-64
(sys.int::define-lap-function sys.int::%%single-float-< ()
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Compare.
  (sys.lap-x86:ucomiss :xmm0 :xmm1)
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:mov64 :r9 t)
  (sys.lap-x86:cmov64b :r8 :r9)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

#+x86-64
(sys.int::define-lap-function sys.int::%%double-float-< ()
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  (sys.lap-x86:ucomisd :xmm0 (:object :r9 0))
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:mov64 :r9 t)
  (sys.lap-x86:cmov64b :r8 :r9)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

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

#+x86-64
(sys.int::define-lap-function sys.int::%%single-float-= ()
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Compare.
  (sys.lap-x86:ucomiss :xmm0 :xmm1)
  (sys.lap-x86:mov64 :r8 t)
  (sys.lap-x86:mov64 :r9 nil)
  ;; If the P bit is set then the values are unorderable.
  (sys.lap-x86:cmov64p :r8 :r9)
  (sys.lap-x86:cmov64ne :r8 :r9)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

#+x86-64
(sys.int::define-lap-function sys.int::%%double-float-= ()
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  (sys.lap-x86:ucomisd :xmm0 (:object :r9 0))
  (sys.lap-x86:mov64 :r8 t)
  (sys.lap-x86:mov64 :r9 nil)
  ;; If the P bit is set then the values are unorderable.
  (sys.lap-x86:cmov64p :r8 :r9)
  (sys.lap-x86:cmov64ne :r8 :r9)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

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

#+x86-64
(sys.int::define-lap-function sys.int::%%truncate-single-float ()
  ;; Unbox the float.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  ;; Convert to unboxed integer.
  (sys.lap-x86:cvttss2si64 :rax :xmm0)
  ;; Box fixnum.
  (sys.lap-x86:lea64 :r8 ((:rax #.(ash 1 sys.int::+n-fixnum-bits+))))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

#+x86-64
(sys.int::define-lap-function sys.int::%%truncate-double-float ()
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  ;; Convert to unboxed integer.
  (sys.lap-x86:cvttsd2si64 :rax :xmm0)
  ;; Box fixnum.
  (sys.lap-x86:lea64 :r8 ((:rax #.(ash 1 sys.int::+n-fixnum-bits+))))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

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

#+x86-64
(sys.int::define-lap-function sys.int::%%single-float-/ ()
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Divide.
  (sys.lap-x86:divss :xmm0 :xmm1)
  ;; Box.
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

#+x86-64
(sys.int::define-lap-function sys.int::%%double-float-/ ()
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  (sys.lap-x86:divsd :xmm0 (:object :r9 0))
  (sys.lap-x86:movq :rax :xmm0)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-double-float-rax))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

(defun sys.int::binary-/ (x y)
  (cond ((or (and (sys.int::single-float-p x)
                  (sys.int::fixnump y))
             (and (sys.int::fixnump x)
                  (sys.int::single-float-p y))
             (and (sys.int::single-float-p x)
                  (sys.int::single-float-p y)))
         (sys.int::%%single-float-/ (float x) (float y)))
        (t (sys.int::full-/ x y))))

#+x86-64
(sys.int::define-lap-function sys.int::%%single-float-+ ()
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Add.
  (sys.lap-x86:addss :xmm0 :xmm1)
  ;; Box.
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

#+x86-64
(sys.int::define-lap-function sys.int::%%double-float-+ ()
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  (sys.lap-x86:addsd :xmm0 (:object :r9 0))
  (sys.lap-x86:movq :rax :xmm0)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-double-float-rax))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

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

#+x86-64
(sys.int::define-lap-function sys.int::%%single-float-- ()
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Subtract.
  (sys.lap-x86:subss :xmm0 :xmm1)
  ;; Box.
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

#+x86-64
(sys.int::define-lap-function sys.int::%%double-float-- ()
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  (sys.lap-x86:subsd :xmm0 (:object :r9 0))
  (sys.lap-x86:movq :rax :xmm0)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-double-float-rax))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

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

#+x86-64
(sys.int::define-lap-function sys.int::%%single-float-* ()
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Multiply.
  (sys.lap-x86:mulss :xmm0 :xmm1)
  ;; Box.
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

#+x86-64
(sys.int::define-lap-function sys.int::%%double-float-* ()
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  (sys.lap-x86:mulsd :xmm0 (:object :r9 0))
  (sys.lap-x86:movq :rax :xmm0)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-double-float-rax))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

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
