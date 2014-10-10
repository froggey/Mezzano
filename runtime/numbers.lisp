(in-package :mezzanine.runtime)

(declaim (inline integerp))
(defun integerp (object)
  (or (system:fixnump object)
      (sys.int::bignump object)))

(defun rationalp (object)
  (or (integerp object)
      (sys.int::ratiop object)))

(defun realp (object)
  (or (rationalp object)
      (floatp object)))

(defun numberp (object)
  (or (realp object)
      (complexp object)))

(sys.int::define-lap-function %%coerce-fixnum-to-float ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:cvtsi2ss64 :xmm0 :rax)
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(defun float (number &optional prototype)
  (declare (ignore prototype))
  (etypecase number
    (float number)
    (fixnum (%%coerce-fixnum-to-float number))
    (ratio (/ (float (numerator number) prototype)
              (float (denominator number) prototype)))))

(sys.int::define-lap-function %%float-< ()
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

(defun sys.int::generic-< (x y)
  (cond
    ((or (and (floatp x)
              (sys.int::fixnump y))
         (and (sys.int::fixnump x)
              (floatp y))
         (and (floatp x)
              (floatp y)))
     (let ((x* (if (floatp y)
                   (float x y)
                   x))
           (y* (if (floatp x)
                   (float y x)
                   y)))
       (%%float-< x* y*)))
    (t (sys.int::full-< x y))))

;; Implement these in terms of <.
(defun sys.int::generic->= (x y)
  (not (sys.int::generic-< x y)))

(defun sys.int::generic-> (x y)
  (sys.int::generic-< y x))

(defun sys.int::generic-<= (x y)
  (not (sys.int::generic-< y x)))

(sys.int::define-lap-function %%float-= ()
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

(defun sys.int::generic-= (x y)
  (cond
    ((or (and (floatp x)
              (sys.int::fixnump y))
         (and (sys.int::fixnump x)
              (floatp y))
         (and (floatp x)
              (floatp y)))
     ;; Convert both arguments to the same kind of float.
     (let ((x* (if (floatp y)
                   (float x y)
                   x))
           (y* (if (floatp x)
                   (float y x)
                   y)))
       (%%float-= x* y*)))
    (t (sys.int::full-= x y))))

(sys.int::define-lap-function %%truncate-float ()
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

(defun sys.int::generic-truncate (number divisor)
  (assert (/= divisor 0) (number divisor) 'division-by-zero)
  (cond
    ((or (and (floatp number)
              (sys.int::fixnump divisor))
         (and (sys.int::fixnump number)
              (floatp divisor))
         (and (floatp number)
              (floatp divisor)))
     (let* ((val (/ number divisor))
            (integer-part (if (< most-negative-fixnum
                                 val
                                 most-positive-fixnum)
                              ;; Fits in a fixnum, convert quickly.
                              (%%truncate-float val)
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

(defun generic-rem (number divisor)
  (multiple-value-bind (quot rem)
      (generic-truncate number divisor)
    (declare (ignore quot))
    rem))

(defun mod (number divisor)
  (multiple-value-bind (quot rem)
      (floor number divisor)
    (declare (ignore quot))
    rem))

(sys.int::define-lap-function %%float-/ ()
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

(defun sys.int::binary-/ (x y)
  (cond ((or (and (floatp x)
                  (sys.int::fixnump y))
             (and (sys.int::fixnump x)
                  (floatp y))
             (and (floatp x)
                  (floatp y)))
         (%%float-/ (float x) (float y)))
        (t (sys.int::full-/ x y))))

(sys.int::define-lap-function %%float-+ ()
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

(defun sys.int::generic-+ (x y)
  (cond ((or (and (floatp x)
                  (sys.int::fixnump y))
             (and (sys.int::fixnump x)
                  (floatp y))
             (and (floatp x)
                  (floatp y)))
         ;; Convert both arguments to the same kind of float.
         (let ((x* (if (floatp y)
                       (float x y)
                       x))
               (y* (if (floatp x)
                       (float y x)
                       y)))
           (%%float-+ x* y*)))
        (t (sys.int::full-+ x y))))

(sys.int::define-lap-function %%float-- ()
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

(defun sys.int::generic-- (x y)
  (cond ((or (and (floatp x)
                  (sys.int::fixnump y))
             (and (sys.int::fixnump x)
                  (floatp y))
             (and (floatp x)
                  (floatp y)))
         ;; Convert both arguments to the same kind of float.
         (let ((x* (if (floatp y)
                       (float x y)
                       x))
               (y* (if (floatp x)
                       (float y x)
                       y)))
           (%%float-- x* y*)))
        (t (sys.int::full-- x y))))

(sys.int::define-lap-function %%float-* ()
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

(defun sys.int::generic-* (x y)
  (cond ((or (and (floatp x)
                  (sys.int::fixnump y))
             (and (sys.int::fixnump x)
                  (floatp y))
             (and (floatp x)
                  (floatp y)))
         ;; Convert both arguments to the same kind of float.
         (let ((x* (if (floatp y)
                       (float x y)
                       x))
               (y* (if (floatp x)
                       (float y x)
                       y)))
           (%%float-* x* y*)))
        (t (sys.int::full-* x y))))

(defun integer-length (integer)
  (when (minusp integer) (setf integer (- integer)))
  (do ((len 0 (1+ len)))
      ((zerop integer)
       len)
    (setf integer (ash integer -1))))
