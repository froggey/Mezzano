;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(deftype short-float () 'float)
(deftype single-float () 'float)
(deftype double-float () 'float)
(deftype long-float () 'float)
(deftype bignum () '(satisfies bignump))

(defstruct (ratio
             (:constructor make-ratio (numerator denominator))
             (:predicate ratiop))
  numerator
  denominator)

(defun numerator (rational)
  (etypecase rational
    (ratio (ratio-numerator rational))
    (integer rational)))

(defun denominator (rational)
  (etypecase rational
    (ratio (ratio-denominator rational))
    (integer 1)))

(defstruct (complex
             (:constructor make-complex (realpart imagpart))
             (:predicate complexp))
  realpart
  imagpart)

(defun complex (realpart &optional imagpart)
  (check-type realpart real)
  (check-type imagpart (or real null))
  (unless imagpart (setf imagpart (coerce 0 (type-of realpart))))
  (if (and (integerp realpart) (zerop imagpart))
      realpart
      (make-complex realpart imagpart)))

(defun realpart (number)
  (if (complexp number)
      (complex-realpart number)
      number))

(defun imagpart (number)
  (if (complexp number)
      (complex-imagpart number)
      0))

(defun expt (base power)
  (check-type power integer)
  (cond ((minusp power)
         (/ (expt base (- power))))
        (t (let ((accum 1))
             (dotimes (i power accum)
               (setf accum (* accum base)))))))

(defstruct (byte (:constructor byte (size position)))
  (size 0 :type (integer 0) :read-only t)
  (position 0 :type (integer 0) :read-only t))

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
  (let* ((bits (%single-float-as-integer float))
         (exp (ldb (byte 8 23) bits))
         (sig (ldb (byte 23 0) bits)))
    (and (eql exp #xFF)
         (not (zerop sig)))))

(defun float-trapping-nan-p (float)
  (let* ((bits (%single-float-as-integer float))
         (exp (ldb (byte 8 23) bits))
         (sig (ldb (byte 23 0) bits)))
    (and (eql exp #xFF)
         (not (zerop sig))
         (not (ldb-test (byte 1 22) sig)))))

(defun float-infinity-p (float)
  (let* ((bits (%single-float-as-integer float))
         (exp (ldb (byte 8 23) bits))
         (sig (ldb (byte 23 0) bits)))
    (and (eql exp #xFF)
         (zerop sig))))

(define-lap-function %single-float-as-integer ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:shl64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(define-lap-function %integer-as-single-float ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

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

(define-lap-function %%bignum-< ()
  ;; Read lengths.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-object+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-object+)))
  (sys.lap-x86:shr64 :rax #.+object-data-shift+)
  (sys.lap-x86:shr64 :rdx #.+object-data-shift+)
  ;; Pick the longest length.
  (sys.lap-x86:mov64 :rcx :rax)
  (sys.lap-x86:cmp64 :rax :rdx)
  (sys.lap-x86:cmov64ng :rcx :rdx)
  (sys.lap-x86:xor64 :rbx :rbx) ; offset
  (sys.lap-x86:xor64 :r10 :r10) ; CF save register.
  (sys.lap-x86:shl64 :rax 3)
  (sys.lap-x86:shl64 :rdx 3)
  loop
  (sys.lap-x86:cmp64 :rbx :rax)
  (sys.lap-x86:jae sx-left)
  (sys.lap-x86:mov64 :rsi (:r8 #.(+ (- +tag-object+) 8) :rbx))
  sx-left-resume
  (sys.lap-x86:cmp64 :rbx :rdx)
  (sys.lap-x86:jae sx-right)
  (sys.lap-x86:mov64 :rdi (:r9 #.(+ (- +tag-object+) 8) :rbx))
  sx-right-resume
  (sys.lap-x86:add64 :rbx 8)
  (sys.lap-x86:sub64 :rcx 1)
  (sys.lap-x86:jz last-compare)
  (sys.lap-x86:clc) ; Avoid setting low bits in r10.
  (sys.lap-x86:rcl64 :r10 1) ; Restore saved carry.
  (sys.lap-x86:sbb64 :rsi :rdi)
  (sys.lap-x86:rcr64 :r10 1) ; Save carry.
  (sys.lap-x86:jmp loop)
  last-compare
  (sys.lap-x86:clc) ; Avoid setting low bits in r10.
  (sys.lap-x86:rcl64 :r10 1) ; Restore saved carry.
  (sys.lap-x86:sbb64 :rsi :rdi)
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:mov64 :r9 t)
  (sys.lap-x86:cmov64l :r8 :r9)
  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:ret)
  sx-left
  ;; Sign extend the left argument.
  ;; Previous value is not in RSI. Pull from the last word in the bignum.
  (sys.lap-x86:mov64 :rsi (:r8 #.(- +tag-object+) :rax))
  (sys.lap-x86:sar64 :rsi 63)
  (sys.lap-x86:jmp sx-left-resume)
  sx-right
  ;; Sign extend the right argument (previous value in RDI).
  (sys.lap-x86:sar64 :rdi 63)
  (sys.lap-x86:jmp sx-right-resume))

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
     ;; Convert both arguments to the same kind of float.
     (let ((x* (if (floatp y)
                   (float x y)
                   x))
           (y* (if (floatp x)
                   (float y x)
                   y)))
       (%%float-< x* y*)))
    ((or (sys.int::ratiop x)
         (sys.int::ratiop y))
       (< (* (numerator x) (denominator y))
          (* (numerator y) (denominator x))))
    (t (error "TODO... Argument combination ~S and ~S not supported." x y))))

(define-lap-function %%bignum-= ()
  ;; Read headers.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-object+)))
  (sys.lap-x86:cmp64 :rax (:r9 #.(- +tag-object+)))
  (sys.lap-x86:jne different)
  ;; Same length, compare words.
  (sys.lap-x86:shr64 :rax #.+object-data-shift+)
  (sys.lap-x86:xor32 :ecx :ecx)
  loop
  (sys.lap-x86:mov64 :rdx (:r8 #.(+ (- +tag-object+) 8) (:rcx 8)))
  (sys.lap-x86:cmp64 :rdx (:r9 #.(+ (- +tag-object+) 8) (:rcx 8)))
  (sys.lap-x86:jne different)
  test
  (sys.lap-x86:add64 :rcx 1)
  (sys.lap-x86:cmp64 :rcx :rax)
  (sys.lap-x86:jb loop)
  (sys.lap-x86:mov64 :r8 t)
  done
  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:ret)
  different
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:jmp done))

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
     ;; Convert both arguments to the same kind of float.
     (let ((x* (if (floatp y)
                   (float x y)
                   x))
           (y* (if (floatp x)
                   (float y x)
                   y)))
       (%%float-= x* y*)))
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
        (q 0))
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
                                  (sys.int::%%truncate-float val)
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
        ((or (floatp x) (floatp y))
         (sys.int::%%float-/ (float x) (float y)))
        ((or (sys.int::ratiop x) (sys.int::ratiop y))
         (/ (* (numerator x) (denominator y))
            (* (denominator x) (numerator y))))
        (t (error "Argument complex ~S and ~S not supported." x y))))

(define-lap-function %%bignum-+ ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:push :r8)
  (:gc :frame :layout #*1)
  (sys.lap-x86:push :r9)
  (:gc :frame :layout #*11)
  ;; Read lengths.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-object+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-object+)))
  (sys.lap-x86:shr64 :rax #.+object-data-shift+)
  (sys.lap-x86:shr64 :rdx #.+object-data-shift+)
  ;; Allocate a new bignum large enough to hold the result.
  (sys.lap-x86:cmp32 :eax :edx)
  (sys.lap-x86:cmov32na :eax :edx)
  (sys.lap-x86:add32 :eax 1)
  (sys.lap-x86:jc bignum-overflow)
  (sys.lap-x86:mov64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:lea64 :r8 ((:rax #.(ash 1 +n-fixnum-bits+)))) ; fixnumize
  (sys.lap-x86:mov64 :r13 (:function %make-bignum-of-length))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+)
                               8
                               (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:mov64 :r10 :r8)
  ;; Reread lengths.
  (sys.lap-x86:mov64 :r9 (:stack 1))
  (sys.lap-x86:mov64 :r8 (:stack 0))
  (:gc :frame)
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-object+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-object+)))
  (sys.lap-x86:shr64 :rax #.+object-data-shift+)
  (sys.lap-x86:shr64 :rdx #.+object-data-shift+)
  ;; X in r8. Y in r9. Result in r10.
  ;; Pick the longest length.
  (sys.lap-x86:mov64 :rcx :rax)
  (sys.lap-x86:cmp64 :rax :rdx)
  (sys.lap-x86:cmov64ng :rcx :rdx)
  (sys.lap-x86:xor64 :rbx :rbx) ; offset
  (sys.lap-x86:xor64 :r11 :r11) ; CF save register.
  (sys.lap-x86:shl64 :rax 3)
  (sys.lap-x86:shl64 :rdx 3)
  loop
  (sys.lap-x86:cmp64 :rbx :rax)
  (sys.lap-x86:jae sx-left)
  (sys.lap-x86:mov64 :rsi (:r8 #.(+ (- +tag-object+) 8) :rbx))
  sx-left-resume
  (sys.lap-x86:cmp64 :rbx :rdx)
  (sys.lap-x86:jae sx-right)
  (sys.lap-x86:mov64 :rdi (:r9 #.(+ (- +tag-object+) 8) :rbx))
  sx-right-resume
  (sys.lap-x86:add64 :rbx 8)
  (sys.lap-x86:sub64 :rcx 1)
  (sys.lap-x86:jz last)
  (sys.lap-x86:clc) ; Avoid setting low bits in r11.
  (sys.lap-x86:rcl64 :r11 1) ; Restore saved carry.
  (sys.lap-x86:adc64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-object+) :rbx) :rsi)
  (sys.lap-x86:rcr64 :r11 1) ; Save carry.
  (sys.lap-x86:jmp loop)
  last
  (sys.lap-x86:clc) ; Avoid setting low bits in r11.
  (sys.lap-x86:rcl64 :r11 1) ; Restore saved carry.
  (sys.lap-x86:adc64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-object+) :rbx) :rsi)
  (sys.lap-x86:jo sign-changed)
  ;; Sign didn't change.
  (sys.lap-x86:sar64 :rsi 63)
  sign-fixed
  (sys.lap-x86:mov64 (:r10 #.(+ (- +tag-object+) 8) :rbx) :rsi)
  (sys.lap-x86:mov64 :r8 :r10)
  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:mov64 :r13 (:function %%canonicalize-bignum))
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:jmp (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (:gc :frame)
  sx-left
  ;; Sign extend the left argument.
  ;; Previous value is not in RSI. Pull from the last word in the bignum.
  (sys.lap-x86:mov64 :rsi (:r8 #.(- +tag-object+) :rax))
  (sys.lap-x86:sar64 :rsi 63)
  (sys.lap-x86:jmp sx-left-resume)
  sx-right
  ;; Sign extend the right argument (previous value in RDI).
  (sys.lap-x86:sar64 :rdi 63)
  (sys.lap-x86:jmp sx-right-resume)
  sign-changed
  (sys.lap-x86:rcr64 :rsi 1)
  (sys.lap-x86:sar64 :rsi 63)
  (sys.lap-x86:jmp sign-fixed)
  bignum-overflow
  (sys.lap-x86:mov64 :r8 (:constant "Aiee! Bignum overflow."))
  (sys.lap-x86:mov64 :r13 (:function error))
  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:ud2))

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
         ;; Convert both arguments to the same kind of float.
         (let ((x* (if (floatp y)
                       (float x y)
                       x))
               (y* (if (floatp x)
                       (float y x)
                       y)))
           (sys.int::%%float-+ x* y*)))
        ((or (sys.int::ratiop x)
             (sys.int::ratiop y))
         (/ (+ (* (numerator x) (denominator y))
               (* (numerator y) (denominator x)))
            (* (denominator x) (denominator y))))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination ~S and ~S not supported." x y))))

(define-lap-function %%bignum-- ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:push :r8)
  (:gc :frame :layout #*1)
  (sys.lap-x86:push :r9)
  (:gc :frame :layout #*11)
  ;; Read lengths.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-object+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-object+)))
  (sys.lap-x86:shr64 :rax #.+object-data-shift+)
  (sys.lap-x86:shr64 :rdx #.+object-data-shift+)
  ;; Allocate a new bignum large enough to hold the result.
  (sys.lap-x86:cmp64 :rax :rdx)
  (sys.lap-x86:cmov64ng :rax :rdx)
  (sys.lap-x86:add32 :eax 1)
  (sys.lap-x86:jc bignum-overflow)
  (sys.lap-x86:mov64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:lea64 :r8 ((:rax #.(ash 1 +n-fixnum-bits+)))) ; fixnumize
  (sys.lap-x86:mov64 :r13 (:function %make-bignum-of-length))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:mov64 :r10 :r8)
  ;; Reread lengths.
  (sys.lap-x86:mov64 :r9 (:stack 1))
  (sys.lap-x86:mov64 :r8 (:stack 0))
  (:gc :frame)
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-object+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-object+)))
  (sys.lap-x86:shr64 :rax 8)
  (sys.lap-x86:shr64 :rdx 8)
  ;; X in r8. Y in r9. Result in r10.
  ;; Pick the longest length.
  (sys.lap-x86:mov64 :rcx :rax)
  (sys.lap-x86:cmp64 :rax :rdx)
  (sys.lap-x86:cmov64ng :rcx :rdx)
  (sys.lap-x86:xor64 :rbx :rbx) ; offset
  (sys.lap-x86:xor64 :r11 :r11) ; CF save register.
  (sys.lap-x86:shl64 :rax 3)
  (sys.lap-x86:shl64 :rdx 3)
  loop
  (sys.lap-x86:cmp64 :rbx :rax)
  (sys.lap-x86:jae sx-left)
  (sys.lap-x86:mov64 :rsi (:r8 #.(+ (- +tag-object+) 8) :rbx))
  sx-left-resume
  (sys.lap-x86:cmp64 :rbx :rdx)
  (sys.lap-x86:jae sx-right)
  (sys.lap-x86:mov64 :rdi (:r9 #.(+ (- +tag-object+) 8) :rbx))
  sx-right-resume
  (sys.lap-x86:add64 :rbx 8)
  (sys.lap-x86:sub64 :rcx 1)
  (sys.lap-x86:jz last)
  (sys.lap-x86:clc) ; Avoid setting low bits in r11.
  (sys.lap-x86:rcl64 :r11 1) ; Restore saved carry.
  (sys.lap-x86:sbb64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-object+) :rbx) :rsi)
  (sys.lap-x86:rcr64 :r11 1) ; Save carry.
  (sys.lap-x86:jmp loop)
  last
  (sys.lap-x86:clc) ; Avoid setting low bits in r11.
  (sys.lap-x86:rcl64 :r11 1) ; Restore saved carry.
  (sys.lap-x86:sbb64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-object+) :rbx) :rsi)
  (sys.lap-x86:jo sign-changed)
  ;; Sign didn't change.
  (sys.lap-x86:sar64 :rsi 63)
  sign-fixed
  (sys.lap-x86:mov64 (:r10 #.(+ (- +tag-object+) 8) :rbx) :rsi)
  (sys.lap-x86:mov64 :r8 :r10)
  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:mov64 :r13 (:function %%canonicalize-bignum))
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:jmp (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (:gc :frame)
  sx-left
  ;; Sign extend the left argument.
  ;; Previous value is not in RSI. Pull from the last word in the bignum.
  (sys.lap-x86:mov64 :rsi (:r8 #.(- +tag-object+) :rax))
  (sys.lap-x86:sar64 :rsi 63)
  (sys.lap-x86:jmp sx-left-resume)
  sx-right
  ;; Sign extend the right argument (previous value in RDI).
  (sys.lap-x86:sar64 :rdi 63)
  (sys.lap-x86:jmp sx-right-resume)
  sign-changed
  (sys.lap-x86:cmc)
  (sys.lap-x86:rcr64 :rsi 1)
  (sys.lap-x86:sar64 :rsi 63)
  (sys.lap-x86:jmp sign-fixed)
  bignum-overflow
  (sys.lap-x86:mov64 :r8 (:constant "Aiee! Bignum overflow."))
  (sys.lap-x86:mov64 :r13 (:function error))
  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:ud2))

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
         ;; Convert both arguments to the same kind of float.
         (let ((x* (if (floatp y)
                       (float x y)
                       x))
               (y* (if (floatp x)
                       (float y x)
                       y)))
           (sys.int::%%float-- x* y*)))
        ((or (sys.int::ratiop x)
             (sys.int::ratiop y))
         (/ (- (* (numerator x) (denominator y))
               (* (numerator y) (denominator x)))
            (* (denominator x) (denominator y))))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination ~S and ~S not supported." x y))))

;;; Unsigned multiply X & Y, must be of type (UNSIGNED-BYTE 64)
;;; This can be either a fixnum, a length-one bignum or a length-two bignum.
;;; Always returns an (UNSIGNED-BYTE 128) in a length-three bignum.
(define-lap-function %%bignum-multiply-step ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  ;; Read X.
  (sys.lap-x86:test64 :r8 #.+fixnum-tag-mask+)
  (sys.lap-x86:jnz read-bignum-x)
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax #.+n-fixnum-bits+)
  ;; Read Y.
  read-y
  (sys.lap-x86:test64 :r9 #.+fixnum-tag-mask+)
  (sys.lap-x86:jnz read-bignum-y)
  (sys.lap-x86:mov64 :rcx :r9)
  (sys.lap-x86:shr64 :rcx #.+n-fixnum-bits+)
  perform-multiply
  (sys.lap-x86:mul64 :rcx)
  ;; RDX:RAX holds the 128-bit result.
  ;; Prepare to allocate the result.
  (sys.lap-x86:push :rdx) ; High half.
  (sys.lap-x86:push :rax) ; Low half.
  (sys.lap-x86:mov64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r8 #.(ash 3 +n-fixnum-bits+)) ; fixnum 3
  (sys.lap-x86:mov64 :r13 (:function %make-bignum-of-length))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:pop (:r8 #.(+ (- +tag-object+) 8)))
  (sys.lap-x86:pop (:r8 #.(+ (- +tag-object+) 16)))
  (sys.lap-x86:mov64 (:r8 #.(+ (- +tag-object+) 24)) 0)
  ;; Single value return
  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:ret)
  (:gc :frame)
  read-bignum-x
  (sys.lap-x86:mov64 :rax (:r8 #.(+ (- +tag-object+) 8)))
  (sys.lap-x86:jmp read-y)
  read-bignum-y
  (sys.lap-x86:mov64 :rcx (:r9 #.(+ (- +tag-object+) 8)))
  (sys.lap-x86:jmp perform-multiply))

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
         ;; Convert both arguments to the same kind of float.
         (let ((x* (if (floatp y)
                       (float x y)
                       x))
               (y* (if (floatp x)
                       (float y x)
                       y)))
           (sys.int::%%float-* x* y*)))
        ((or (sys.int::ratiop x)
             (sys.int::ratiop y))
         (/ (* (numerator x) (numerator y))
            (* (denominator x) (denominator y))))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination ~S and ~S not supported." x y))))

(macrolet ((def (name bignum-name op)
             `(progn
                (define-lap-function ,bignum-name ()
                  (sys.lap-x86:push :rbp)
                  (:gc :no-frame :layout #*0)
                  (sys.lap-x86:mov64 :rbp :rsp)
                  (:gc :frame)
                  ;; Save objects.
                  (sys.lap-x86:push :r8)
                  (:gc :frame :layout #*1)
                  (sys.lap-x86:push :r9)
                  (:gc :frame :layout #*11)
                  ;; Read lengths.
                  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-object+)))
                  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-object+)))
                  (sys.lap-x86:shr64 :rax #.+object-data-shift+)
                  (sys.lap-x86:shr64 :rdx #.+object-data-shift+)
                  ;; Allocate a new bignum large enough to hold the result.
                  (sys.lap-x86:cmp32 :eax :edx)
                  (sys.lap-x86:cmov32na :eax :edx)
                  (sys.lap-x86:mov64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
                  (sys.lap-x86:lea64 :r8 ((:rax #.(ash 1 +n-fixnum-bits+)))) ; fixnumize
                  (sys.lap-x86:mov64 :r13 (:function %make-bignum-of-length))
                  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
                  (sys.lap-x86:mov64 :r10 :r8)
                  ;; Reread lengths.
                  (sys.lap-x86:mov64 :r9 (:stack 1))
                  (sys.lap-x86:mov64 :r8 (:stack 0))
                  (:gc :frame)
                  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-object+)))
                  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-object+)))
                  (sys.lap-x86:shr64 :rax #.+object-data-shift+)
                  (sys.lap-x86:shr64 :rdx #.+object-data-shift+)
                  ;; X in r8. Y in r9. Result in r10.
                  ;; Pick the longest length.
                  (sys.lap-x86:mov64 :rcx :rax)
                  (sys.lap-x86:cmp64 :rax :rdx)
                  (sys.lap-x86:cmov64ng :rcx :rdx)
                  (sys.lap-x86:xor64 :rbx :rbx) ; offset
                  (sys.lap-x86:shl64 :rax 3)
                  (sys.lap-x86:shl64 :rdx 3)
                  loop
                  (sys.lap-x86:cmp64 :rbx :rax)
                  (sys.lap-x86:jae sx-left)
                  (sys.lap-x86:mov64 :rsi (:r8 #.(+ (- +tag-object+) 8) :rbx))
                  sx-left-resume
                  (sys.lap-x86:cmp64 :rbx :rdx)
                  (sys.lap-x86:jae sx-right)
                  (sys.lap-x86:mov64 :rdi (:r9 #.(+ (- +tag-object+) 8) :rbx))
                  sx-right-resume
                  (sys.lap-x86:add64 :rbx 8)
                  (sys.lap-x86:sub64 :rcx 1)
                  (sys.lap-x86:jz last)
                  (,op :rsi :rdi)
                  (sys.lap-x86:mov64 (:r10 #.(- +tag-object+) :rbx) :rsi)
                  (sys.lap-x86:jmp loop)
                  last
                  (,op :rsi :rdi)
                  (sys.lap-x86:mov64 (:r10 #.(- +tag-object+) :rbx) :rsi)
                  (sys.lap-x86:mov64 :r8 :r10)
                  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+))
                  (sys.lap-x86:mov64 :r13 (:function %%canonicalize-bignum))
                  (sys.lap-x86:leave)
                  (:gc :no-frame)
                  (sys.lap-x86:jmp (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
                  (:gc :frame)
                  sx-left
                  ;; Sign extend the left argument.
                  ;; Previous value is not in RSI. Pull from the last word in the bignum.
                  (sys.lap-x86:mov64 :rsi (:r8 #.(- +tag-object+) :rax))
                  (sys.lap-x86:sar64 :rsi 63)
                  (sys.lap-x86:jmp sx-left-resume)
                  sx-right
                  ;; Sign extend the right argument (previous value in RDI).
                  (sys.lap-x86:sar64 :rdi 63)
                  (sys.lap-x86:jmp sx-right-resume))
                (defun ,name (x y)
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
                        (t (check-type x number)
                           (check-type y number)
                           (error "Argument combination not supported.")))))))
  (def generic-logand %%bignum-logand sys.lap-x86:and64)
  (def generic-logior %%bignum-logior sys.lap-x86:or64)
  (def generic-logxor %%bignum-logxor sys.lap-x86:xor64))

(define-lap-function %%bignum-left-shift ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  ;; Save objects.
  (sys.lap-x86:push :r8) ; src
  (:gc :frame :layout #*1)
  (sys.lap-x86:push :r9) ; count
  (:gc :frame :layout #*11)
  ;; Allocate a new bignum large enough to hold the result.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-object+)))
  (sys.lap-x86:shr64 :rax #.+object-data-shift+)
  (sys.lap-x86:mov64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:lea64 :r8 ((:rax #.(ash 1 +n-fixnum-bits+))
                          #.(ash 1 +n-fixnum-bits+))) ; fixnumize +1
  (sys.lap-x86:mov64 :r13 (:function %make-bignum-of-length))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  ;; R8: dest
  ;; R9: src
  ;; CL: count
  ;; RBX: n words.
  ;; R10: current word.
  (sys.lap-x86:mov64 :rcx (:stack 1))
  (sys.lap-x86:mov64 :r9 (:stack 0))
  (:gc :frame)
  (sys.lap-x86:mov64 :rbx (:r9 #.(- +tag-object+)))
  (sys.lap-x86:sar64 :rcx #.+n-fixnum-bits+)
  (sys.lap-x86:and64 :rbx #.(lognot (1- (ash 1 +object-data-shift+))))
  (sys.lap-x86:shr64 :rbx #.(- +object-data-shift+ +n-fixnum-bits+))
  (sys.lap-x86:mov32 :r10d #.(ash 1 +n-fixnum-bits+))
  loop
  (sys.lap-x86:cmp64 :r10 :rbx)
  (sys.lap-x86:jae last-word)
  (sys.lap-x86:mov64 :rax (:r9 #.(+ (- +tag-object+) 8) (:r10 #.(/ 8 (ash 1 +n-fixnum-bits+)))))
  (sys.lap-x86:mov64 :rdx (:r9 #.(+ (- +tag-object+)) (:r10 #.(/ 8 (ash 1 +n-fixnum-bits+)))))
  (sys.lap-x86:shld64 :rax :rdx :cl)
  (sys.lap-x86:mov64 (:r8 #.(+ (- +tag-object+) 8) (:r10 #.(/ 8 (ash 1 +n-fixnum-bits+)))) :rax)
  (sys.lap-x86:add64 :r10 #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:jmp loop)
  last-word
  (sys.lap-x86:mov64 :rax (:r9 #.(- +tag-object+) (:rbx #.(/ 8 (ash 1 +n-fixnum-bits+)))))
  (sys.lap-x86:cqo)
  (sys.lap-x86:shld64 :rdx :rax :cl)
  (sys.lap-x86:mov64 (:r8 #.(+ (- +tag-object+) 8) (:rbx #.(/ 8 (ash 1 +n-fixnum-bits+)))) :rdx)
  (sys.lap-x86:mov64 :rax (:r9 #.(+ (- +tag-object+) 8)))
  (sys.lap-x86:shl64 :rax :cl)
  (sys.lap-x86:mov64 (:r8 #.(+ (- +tag-object+) 8)) :rax)
  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:mov64 :r13 (:function %%canonicalize-bignum))
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:jmp (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8)))))

(define-lap-function %%bignum-right-shift ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  ;; Save objects.
  (sys.lap-x86:push :r8) ; src
  (:gc :frame :layout #*1)
  (sys.lap-x86:push :r9) ; count
  (:gc :frame :layout #*11)
  ;; Allocate a new bignum large enough to hold the result.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-object+)))
  (sys.lap-x86:shr64 :rax #.+object-data-shift+)
  (sys.lap-x86:mov64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:lea64 :r8 ((:rax #.(ash 1 +n-fixnum-bits+)))) ; fixnumize
  (sys.lap-x86:mov64 :r13 (:function %make-bignum-of-length))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  ;; R8: dest
  ;; R9: src
  ;; CL: count (raw)
  ;; RBX: n words. (fixnum)
  ;; R10: current word. (fixnum)
  (sys.lap-x86:pop :rcx)
  (sys.lap-x86:pop :r9)
  (:gc :frame)
  (sys.lap-x86:sar64 :rcx #.+n-fixnum-bits+)
  (sys.lap-x86:mov64 :rbx (:r9 #.(- +tag-object+)))
  (sys.lap-x86:and64 :rbx #.(lognot (1- (ash 1 +object-data-shift+))))
  (sys.lap-x86:shr64 :rbx #.(- +object-data-shift+ +n-fixnum-bits+))
  (sys.lap-x86:mov32 :r10d #.(ash 1 +n-fixnum-bits+))
  loop
  (sys.lap-x86:cmp64 :r10 :rbx)
  (sys.lap-x86:jae last-word)
  ;; current+1
  (sys.lap-x86:mov64 :rax (:r9 #.(+ (- +tag-object+) 8) (:r10 #.(/ 8 (ash 1 +n-fixnum-bits+)))))
  ;; current
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-object+) (:r10 #.(/ 8 (ash 1 +n-fixnum-bits+)))))
  (sys.lap-x86:shrd64 :rdx :rax :cl)
  (sys.lap-x86:mov64 (:r8 #.(- +tag-object+) (:r10 #.(/ 8 (ash 1 +n-fixnum-bits+)))) :rdx)
  (sys.lap-x86:add64 :r10 #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:jmp loop)
  last-word
  (sys.lap-x86:mov64 :rax (:r9 #.(- +tag-object+) (:rbx #.(/ 8 (ash 1 +n-fixnum-bits+)))))
  (sys.lap-x86:sar64 :rax :cl)
  (sys.lap-x86:mov64 (:r8 #.(- +tag-object+) (:rbx #.(/ 8 (ash 1 +n-fixnum-bits+)))) :rax)
  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:mov64 :r13 (:function %%canonicalize-bignum))
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:jmp (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8)))))

(defun %ash (integer count)
  (cond ((not (fixnump count))
         (check-type count integer)
         (error "TODO: Bignum ASH count not implemented yet."))
        ((bignump integer)
         (cond
           ((plusp count)
            (multiple-value-bind (quot rem)
                (truncate count 32)
              (dotimes (i quot)
                (setf integer (%%bignum-left-shift integer 32)))
              (%%bignum-left-shift integer rem)))
           ((minusp count)
            (setf count (- count))
            (multiple-value-bind (quot rem)
                (truncate count 32)
              (dotimes (i quot)
                (setf integer (%%bignum-right-shift integer 32))
                (cond ((eql integer 0) (return-from %ash 0))
                      ((fixnump integer)
                       (setf integer (%make-bignum-from-fixnum integer)))))
              (%%bignum-right-shift integer rem)))
           (t integer)))
        (t (check-type integer integer)
           (ash integer count))))

(defun abs (number)
  (check-type number number)
  (etypecase number
    (complex (sqrt (+ (expt (realpart number) 2)
                      (expt (imagpart number) 2))))
    (real (if (minusp number)
              (- number)
              number))))

(define-lap-function %%float-sqrt ()
  ;; Unbox the float.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  ;; Sqrt.
  (sys.lap-x86:sqrtss :xmm0 :xmm0)
  ;; Box.
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:ret))

(defun sqrt (number)
  (check-type number number)
  (etypecase number
    (real (%%float-sqrt (float number)))))

;;; Convert a bignum to canonical form.
;;; If it can be represented as a fixnum it is converted,
;;; otherwise it is converted to the shortest possible bignum
;;; by removing redundant sign-extension bits.
(define-lap-function %%canonicalize-bignum ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-object+)))
  (sys.lap-x86:shr64 :rax #.+object-data-shift+) ; RAX = number of fragments (raw).
  ;; Zero-size bignums are zero.
  (sys.lap-x86:jz return-zero)
  ;; Read the sign bit.
  (sys.lap-x86:mov64 :rcx (:r8 #.(- +tag-object+) (:rax 8)))
  (sys.lap-x86:sar64 :rcx 63) ; rcx = sign-extended sign-bit.
  crunch-loop
  (sys.lap-x86:cmp64 :rax 1)
  (sys.lap-x86:je maybe-fixnumize)
  ;; Read the last fragment.
  (sys.lap-x86:mov64 :rsi (:r8 #.(- +tag-object+) (:rax 8)))
  ;; Compare against the extended sign bit.
  ;; Finish if they're not equal.
  (sys.lap-x86:cmp64 :rsi :rcx)
  (sys.lap-x86:jne maybe-resize-bignum)
  ;; Read the sign bit of the second-to-last fragment
  (sys.lap-x86:mov64 :rsi (:r8 #.(+ (- +tag-object+) -8) (:rax 8)))
  (sys.lap-x86:sar64 :rsi 63)
  ;; Compare against the original sign bit. If equal, then this
  ;; fragment can be dropped.
  (sys.lap-x86:cmp64 :rsi :rcx)
  (sys.lap-x86:jne maybe-resize-bignum)
  (sys.lap-x86:sub64 :rax 1)
  (sys.lap-x86:jmp crunch-loop)
  ;; Final size of the bignum has been determined.
  maybe-resize-bignum
  ;; Test if the size actually changed.
  (sys.lap-x86:mov64 :rdx (:r8 #.(- +tag-object+)))
  (sys.lap-x86:shr64 :rdx #.+object-data-shift+)
  (sys.lap-x86:cmp64 :rax :rdx)
  ;; If it didn't change, return the original bignum.
  ;; TODO: eventually the bignum code will pass in stack-allocated
  ;; bignum objects, this'll have to allocate anyway...
  (sys.lap-x86:je do-return)
  ;; Resizing.
  ;; Save original bignum.
  (sys.lap-x86:push :r8)
  (:gc :frame :layout #*1)
  ;; Save new size.
  (sys.lap-x86:push :rax)
  (:gc :frame :layout #*10)
  ;; RAX = new size.
  (sys.lap-x86:lea64 :r8 ((:rax #.(ash 1 +n-fixnum-bits+))))
  (sys.lap-x86:mov64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r13 (:function %make-bignum-of-length))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  ;; Fetch the original bignum.
  (sys.lap-x86:mov64 :rcx (:rsp))
  (sys.lap-x86:mov64 :r9 (:rsp 8))
  (:gc :frame)
  ;; Copy words, we know there will always be at least one.
  copy-loop
  (sys.lap-x86:mov64 :rax (:r9 #.(- +tag-object+) (:rcx 8)))
  (sys.lap-x86:mov64 (:r8 #.(- +tag-object+) (:rcx 8)) :rax)
  (sys.lap-x86:sub64 :rcx 1)
  (sys.lap-x86:jnz copy-loop)
  do-return
  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:ret)
  ;; Attempt to convert a size-1 bignum to a fixnum.
  maybe-fixnumize
  (:gc :frame)
  (sys.lap-x86:mov64 :rdx (:r8 #.(+ (- +tag-object+) 8)))
  (sys.lap-x86:imul64 :rdx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:jo maybe-resize-bignum)
  (sys.lap-x86:mov64 :r8 :rdx)
  (sys.lap-x86:jmp do-return)
  return-zero
  (sys.lap-x86:xor32 :r8d :r8d)
  (sys.lap-x86:jmp do-return))

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

(defconstant pi 3.14159265359)

;;; http://devmaster.net/forums/topic/4648-fast-and-accurate-sinecosine/
(defun sin (x)
  (setf x (- (mod (+ x pi) (* 2 pi)) pi))
  (let* ((b (/ 4 (float pi 0.0)))
         (c (/ -4 (* (float pi 0.0) (float pi 0.0))))
         (y (+ (* b x) (* c x (abs x))))
         (p 0.225))
    (+ (* p (- (* y (abs y)) y)) y)))

(defun cos (x)
  (sin (+ x (/ pi 2))))

;;; http://en.literateprograms.org/Logarithm_Function_(Python)
(defun log-e (x)
  (let ((base 2.71828)
        (epsilon 0.000000000001)
        (integer 0)
        (partial 0.5)
        (decimal 0.0))
    (loop (when (>= x 1) (return))
       (decf integer)
       (setf x (* x base)))
    (loop (when (< x base) (return))
       (incf integer)
       (setf x (/ x base)))
    (setf x (* x x))
    (loop (when (<= partial epsilon) (return))
       (when (>= x base) ;If X >= base then a_k is 1
         (incf decimal partial) ;Insert partial to the front of the list
         (setf x (/ x base))) ;Since a_k is 1, we divide the number by the base
       (setf partial (* partial 0.5))
       (setf x (* x x)))
    (+ integer decimal)))

(defun log (number &optional base)
  (if base
      (/ (log number) (log base))
      (log-e number)))

;;; http://forums.devshed.com/c-programming-42/implementing-an-atan-function-200106.html
(defun atan (number1 &optional number2)
  (if number2
      (atan2 number1 number2)
      (let ((x number1)
            (y 0.0))
        (when (zerop number1)
          (return-from atan 0))
        (when (< x 0)
          (return-from atan (- (atan (- x)))))
        (setf x (/ (- x 1.0) (+ x 1.0))
              y (* x x))
        (setf x (* (+ (* (- (* (+ (* (- (* (+ (* (- (* (+ (* (- (* 0.0028662257 y) 0.0161657367) y) 0.0429096138) y) 0.0752896400) y) 0.1065626393) y) 0.1420889944) y) 0.1999355085) y) 0.3333314528) y) 1) x))
        (setf x (+ 0.785398163397 x))
        x)))

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
