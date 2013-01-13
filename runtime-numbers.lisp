(in-package #:sys.int)

(declaim (inline integerp))
(defun integerp (object)
  (or (system:fixnump object)
      (bignump object)))

(defun realp (object)
  (or (integerp object)
      (floatp object)))

(defun numberp (object)
  (or (realp object)
      (complexp object)))

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
  (let ((accum 1))
    (dotimes (i power accum)
      (setf accum (* accum base)))))

(defstruct (byte (:constructor byte (size position)))
  (size 0 :type (integer 0) :read-only t)
  (position 0 :type (integer 0) :read-only t))

(defun ldb (bytespec integer)
  (logand (ash integer (- (byte-position bytespec)))
          (1- (ash 1 (byte-size bytespec)))))

(defun dpb (newbyte bytespec integer)
  (let ((mask (1- (ash 1 (byte-size bytespec)))))
    (logior (ash (logand newbyte mask) (byte-position bytespec))
            (logand integer (lognot (ash mask (byte-position bytespec)))))))

(defun ldb-test (bytespec integer)
  (not (zerop (ldb bytespec integer))))

(defun logbitp (index integer)
  (ldb-test (byte 1 index) integer))

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

(define-lap-function %%coerce-fixnum-to-float ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax 3)
  (sys.lap-x86:cvtsi2ss64 :xmm0 :rax)
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.+tag-single-float+))
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun float (number &optional prototype)
  (declare (ignore prototype))
  (etypecase number
    (float number)
    (fixnum (%%coerce-fixnum-to-float number))))

(define-lap-function %single-float-as-integer ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:shl64 :rax 3)
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(define-lap-function %integer-as-single-float ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 3)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.+tag-single-float+))
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

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

(define-lap-function %%bignum-< ()
  (sys.lap-x86:push 0) ; align
  ;; Read lengths.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:shr64 :rax 8)
  (sys.lap-x86:shr64 :rdx 8)
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
  (sys.lap-x86:mov64 :rsi (:r8 #.(+ (- +tag-array-like+) 8) :rbx))
  sx-left-resume
  (sys.lap-x86:cmp64 :rbx :rdx)
  (sys.lap-x86:jae sx-right)
  (sys.lap-x86:mov64 :rdi (:r9 #.(+ (- +tag-array-like+) 8) :rbx))
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
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:add64 :csp 8)
  (sys.lap-x86:ret)
  sx-left
  ;; Sign extend the left argument.
  ;; Previous value is not in RSI. Pull from the last word in the bignum.
  (sys.lap-x86:mov64 :rsi (:r8 #.(- +tag-array-like+) :rax))
  (sys.lap-x86:sar64 :rsi 63)
  (sys.lap-x86:jmp sx-left-resume)
  sx-right
  ;; Sign extend the right argument (previous value in RDI).
  (sys.lap-x86:sar64 :rdi 63)
  (sys.lap-x86:jmp sx-right-resume))

(define-lap-function %%float-< ()
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
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun generic-< (x y)
  (check-type x real)
  (check-type y real)
  (cond
    ((and (fixnump x)
          (fixnump y))
     ;; Should be handled by binary-<.
     (error "FIXNUM/FIXNUM case hit GENERIC-<"))
    ((and (fixnump x)
          (bignump y))
     (%%bignum-< (%make-bignum-from-fixnum x) y))
    ((and (bignump x)
          (fixnump y))
     (%%bignum-< x (%make-bignum-from-fixnum y)))
    ((and (bignump x)
          (bignump y))
     (%%bignum-< x y))
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
    (t (error "TODO... Argument combination not supported."))))

;; Implement these in terms of <.
(defun generic->= (x y)
  (not (generic-< x y)))

(defun generic-> (x y)
  (generic-< y x))

(defun generic-<= (x y)
  (not (generic-< y x)))

(define-lap-function %%bignum-= ()
  (sys.lap-x86:push 0) ; align
  ;; Read headers.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:cmp64 :rax (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:jne different)
  ;; Same length, compare words.
  (sys.lap-x86:shr64 :rax 8)
  (sys.lap-x86:xor32 :ecx :ecx)
  loop
  (sys.lap-x86:mov64 :rdx (:r8 #.(+ (- +tag-array-like+) 8) (:rcx 8)))
  (sys.lap-x86:cmp64 :rdx (:r9 #.(+ (- +tag-array-like+) 8) (:rcx 8)))
  (sys.lap-x86:jne different)
  test
  (sys.lap-x86:add64 :rcx 1)
  (sys.lap-x86:cmp64 :rcx :rax)
  (sys.lap-x86:jb loop)
  (sys.lap-x86:mov64 :r8 t)
  done
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret)
  different
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:jmp done))

(define-lap-function %%float-= ()
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
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun generic-= (x y)
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
    ((or (fixnump x)
         (fixnump y))
     (eq x y))
    ((and (bignump x)
          (bignump y))
     (or (eq x y) (%%bignum-= x y)))
    (t (error "TODO... Argument combination not supported."))))

(define-lap-function %%bignum-truncate ()
  ;; Read headers.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:shr64 :rax 8)
  (sys.lap-x86:shr64 :rdx 8)
  ;; TODO: Full division...
  (sys.lap-x86:cmp64 :rdx 1)
  (sys.lap-x86:jne not-implemented)
  (sys.lap-x86:cmp64 :rax 2)
  (sys.lap-x86:je 128-bit-number)
  (sys.lap-x86:cmp64 :rax 1)
  (sys.lap-x86:jne not-implemented)
  ;; 64-bit divide.
  (sys.lap-x86:mov64 :rax (:r8 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:mov64 :rcx (:r9 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:cqo)
  ;; Quotient in RAX, remainder in RDX.
  do-divide
  (sys.lap-x86:idiv64 :rcx)
  (sys.lap-x86:push :rdx)
  ;; Attempt to convert quotient to a fixnum.
  (sys.lap-x86:mov64 :rcx :rax)
  (sys.lap-x86:imul64 :rax 8)
  (sys.lap-x86:jo quotient-bignum)
  (sys.lap-x86:mov64 :r8 :rax)
  done-quotient
  (sys.lap-x86:mov64 (:lsp -8) nil)
  (sys.lap-x86:sub64 :lsp 8)
  (sys.lap-x86:mov64 (:lsp) :r8)
  ;; Attempt to convert remainder to a fixnum.
  (sys.lap-x86:mov64 :rax (:csp))
  (sys.lap-x86:imul64 :rax 8)
  (sys.lap-x86:jo remainder-bignum)
  (sys.lap-x86:mov64 :r9 :rax)
  done-remainder
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:mov64 :r8 (:lsp))
  (sys.lap-x86:add64 :lsp 8)
  (sys.lap-x86:mov32 :ecx 16)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret)
  ;; Allocate a bignum for the quotient in RCX.
  quotient-bignum
  (sys.lap-x86:mov64 :rax :rcx)
  (sys.lap-x86:mov64 :r13 (:constant %%make-bignum-64-rax))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  (sys.lap-x86:jmp done-quotient)
  ;; Allocate a bignum for the quotient in RCX.
  remainder-bignum
  (sys.lap-x86:mov64 :rax (:csp))
  (sys.lap-x86:mov64 :r13 (:constant %%make-bignum-64-rax))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  (sys.lap-x86:mov64 :r9 :r8)
  (sys.lap-x86:jmp done-remainder)
  128-bit-number
  ;; 128 bit number, 64 bit quotient.
  (sys.lap-x86:mov64 :rax (:r8 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:mov64 :rdx (:r8 #.(+ (- +tag-array-like+) 16)))
  (sys.lap-x86:mov64 :rcx (:r9 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:jmp do-divide)
  not-implemented
  (sys.lap-x86:mov64 :r8 (:constant "Full bignum TRUNCATE not implemented."))
  (sys.lap-x86:mov64 :r13 (:constant error))
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:push 0)
  (sys.lap-x86:call (:symbol-function :r13)))

(define-lap-function %%truncate-float ()
  ;; Unbox the float.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  ;; Convert to unboxed integer.
  (sys.lap-x86:cvttss2si64 :rax :xmm0)
  ;; Box fixnum.
  (sys.lap-x86:lea64 :r8 ((:rax 8)))
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun generic-truncate (number divisor)
  (check-type number real)
  (check-type divisor real)
  (assert (/= divisor 0) (number divisor) 'division-by-zero)
  ;; Avoid overflow when doing fixnum arithmetic.
  ;; ????
  (when (and (eq divisor -1)
             (integerp number))
    (return-from generic-truncate
      (values (- number) 0)))
  (cond ((and (fixnump number)
              (fixnump divisor))
         (error "FIXNUM/FIXNUM case hit GENERIC-TRUNCATE"))
        ((and (fixnump number)
              (bignump divisor))
         (%%bignum-truncate (%make-bignum-from-fixnum number)
                            divisor))
        ((and (bignump number)
              (fixnump divisor))
         (%%bignum-truncate number
                            (%make-bignum-from-fixnum divisor)))
        ((and (bignump number)
              (bignump divisor))
         (%%bignum-truncate number divisor))
        ((or (floatp number)
             (floatp divisor))
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
           (values integer-part (- val integer-part))))
        (t (check-type number number)
           (check-type divisor number)
           (error "Argument combination not supported."))))

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

(define-lap-function %%float-/ ()
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
  (sys.lap-x86:lea64 :r8 (:rax #.+tag-single-float+))
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun binary-/ (x y)
  (cond ((and (typep x 'integer)
              (typep y 'integer))
         (multiple-value-bind (quot rem)
             (truncate x y)
           (if (zerop rem)
               quot
               (%%float-/ (float x) (float y)))))
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
        (t (%%float-/ (float x) (float y)))))

(define-lap-function %%bignum-+ ()
  ;; Save on the lisp stack.
  (sys.lap-x86:mov64 (:lsp -8) nil)
  (sys.lap-x86:mov64 (:lsp -16) nil)
  (sys.lap-x86:sub64 :lsp 16)
  (sys.lap-x86:mov64 (:lsp) :r8)
  (sys.lap-x86:mov64 (:lsp 8) :r9)
  ;; Read lengths.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:shr64 :rax 8)
  (sys.lap-x86:shr64 :rdx 8)
  ;; Allocate a new bignum large enough to hold the result.
  (sys.lap-x86:pushf)
  (sys.lap-x86:cli)
  (sys.lap-x86:cmp32 :eax :edx)
  (sys.lap-x86:cmov32na :eax :edx)
  (sys.lap-x86:add32 :eax 1)
  (sys.lap-x86:jc bignum-overflow)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:push 0)
  (sys.lap-x86:shl64 :rax 3)
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:test64 :r8 8)
  (sys.lap-x86:jz count-even)
  (sys.lap-x86:add64 :r8 8) ; one word for the header, no alignment.
  (sys.lap-x86:jmp do-allocate)
  count-even
  (sys.lap-x86:add64 :r8 16) ; one word for the header, one word for alignment.
  do-allocate
  (sys.lap-x86:mov64 :r9 (:constant :static))
  (sys.lap-x86:mov64 :rcx 16)
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:shl64 :rax 8)
  (sys.lap-x86:or64 :rax #.(ash +array-type-bignum+ +array-type-shift+))
  (sys.lap-x86:mov64 (:r8) :rax)
  (sys.lap-x86:lea64 :r10 (:r8 #.+tag-array-like+))
  (sys.lap-x86:popf)
  ;; Reread lengths.
  (sys.lap-x86:mov64 :r8 (:lsp))
  (sys.lap-x86:mov64 :r9 (:lsp 8))
  (sys.lap-x86:add64 :lsp 16)
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+)))
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
  (sys.lap-x86:mov64 :rsi (:r8 #.(+ (- +tag-array-like+) 8) :rbx))
  sx-left-resume
  (sys.lap-x86:cmp64 :rbx :rdx)
  (sys.lap-x86:jae sx-right)
  (sys.lap-x86:mov64 :rdi (:r9 #.(+ (- +tag-array-like+) 8) :rbx))
  sx-right-resume
  (sys.lap-x86:add64 :rbx 8)
  (sys.lap-x86:sub64 :rcx 1)
  (sys.lap-x86:jz last)
  (sys.lap-x86:clc) ; Avoid setting low bits in r11.
  (sys.lap-x86:rcl64 :r11 1) ; Restore saved carry.
  (sys.lap-x86:adc64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+) :rbx) :rsi)
  (sys.lap-x86:rcr64 :r11 1) ; Save carry.
  (sys.lap-x86:jmp loop)
  last
  (sys.lap-x86:clc) ; Avoid setting low bits in r11.
  (sys.lap-x86:rcl64 :r11 1) ; Restore saved carry.
  (sys.lap-x86:adc64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+) :rbx) :rsi)
  (sys.lap-x86:jo sign-changed)
  ;; Sign didn't change.
  (sys.lap-x86:sar64 :rsi 63)
  sign-fixed
  (sys.lap-x86:mov64 (:r10 #.(+ (- +tag-array-like+) 8) :rbx) :rsi)
  (sys.lap-x86:cmp64 :rbx 8)
  (sys.lap-x86:je maybe-make-fixnum)
  not-fixnum
  (sys.lap-x86:mov64 :r8 :r10)
  finish
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret)
  maybe-make-fixnum
  ;; Attempt to convert the result to a fixnum.
  (sys.lap-x86:mov64 :rax (:r10 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:imul64 :rax 8)
  (sys.lap-x86:jo not-fixnum)
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:jmp finish)
  sx-left
  ;; Sign extend the left argument.
  ;; Previous value is not in RSI. Pull from the last word in the bignum.
  (sys.lap-x86:mov64 :rsi (:r8 #.(- +tag-array-like+) :rax))
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
  (sys.lap-x86:push 0) ; align
  (sys.lap-x86:mov64 :r8 (:constant "Aiee! Bignum overflow."))
  (sys.lap-x86:mov64 :r13 (:constant error))
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:call (:symbol-function :r13)))

(define-lap-function %%float-+ ()
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
  (sys.lap-x86:lea64 :r8 (:rax #.+tag-single-float+))
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun generic-+ (x y)
  (cond ((and (fixnump x)
              (fixnump y))
         (error "FIXNUM/FIXNUM case hit GENERIC-+"))
        ((and (fixnump x)
              (bignump y))
         (%%bignum-+ (%make-bignum-from-fixnum x) y))
        ((and (bignump x)
              (fixnump y))
         (%%bignum-+ x (%make-bignum-from-fixnum y)))
        ((and (bignump x)
              (bignump y))
         (%%bignum-+ x y))
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
           (%%float-+ x* y*)))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination not supported."))))

(define-lap-function %%bignum-- ()
  ;; Save on the lisp stack.
  (sys.lap-x86:mov64 (:lsp -8) nil)
  (sys.lap-x86:mov64 (:lsp -16) nil)
  (sys.lap-x86:sub64 :lsp 16)
  (sys.lap-x86:mov64 (:lsp) :r8)
  (sys.lap-x86:mov64 (:lsp 8) :r9)
  ;; Read lengths.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:shr64 :rax 8)
  (sys.lap-x86:shr64 :rdx 8)
  ;; Allocate a new bignum large enough to hold the result.
  (sys.lap-x86:pushf)
  (sys.lap-x86:cli)
  (sys.lap-x86:cmp64 :rax :rdx)
  (sys.lap-x86:cmov64ng :rax :rdx)
  (sys.lap-x86:add32 :eax 1)
  (sys.lap-x86:jc bignum-overflow)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:push 0)
  (sys.lap-x86:shl64 :rax 3)
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:test64 :r8 8)
  (sys.lap-x86:jz count-even)
  (sys.lap-x86:add64 :r8 8) ; one word for the header, no alignment.
  (sys.lap-x86:jmp do-allocate)
  count-even
  (sys.lap-x86:add64 :r8 16) ; one word for the header, one word for alignment.
  do-allocate
  (sys.lap-x86:mov64 :r9 (:constant :static))
  (sys.lap-x86:mov64 :rcx 16)
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:shl64 :rax 8)
  (sys.lap-x86:or64 :rax #.(ash +array-type-bignum+ +array-type-shift+))
  (sys.lap-x86:mov64 (:r8) :rax)
  (sys.lap-x86:lea64 :r10 (:r8 #.+tag-array-like+))
  (sys.lap-x86:popf)
  ;; Reread lengths.
  (sys.lap-x86:mov64 :r8 (:lsp))
  (sys.lap-x86:mov64 :r9 (:lsp 8))
  (sys.lap-x86:add64 :lsp 16)
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+)))
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
  (sys.lap-x86:mov64 :rsi (:r8 #.(+ (- +tag-array-like+) 8) :rbx))
  sx-left-resume
  (sys.lap-x86:cmp64 :rbx :rdx)
  (sys.lap-x86:jae sx-right)
  (sys.lap-x86:mov64 :rdi (:r9 #.(+ (- +tag-array-like+) 8) :rbx))
  sx-right-resume
  (sys.lap-x86:add64 :rbx 8)
  (sys.lap-x86:sub64 :rcx 1)
  (sys.lap-x86:jz last)
  (sys.lap-x86:clc) ; Avoid setting low bits in r11.
  (sys.lap-x86:rcl64 :r11 1) ; Restore saved carry.
  (sys.lap-x86:sbb64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+) :rbx) :rsi)
  (sys.lap-x86:rcr64 :r11 1) ; Save carry.
  (sys.lap-x86:jmp loop)
  last
  (sys.lap-x86:clc) ; Avoid setting low bits in r11.
  (sys.lap-x86:rcl64 :r11 1) ; Restore saved carry.
  (sys.lap-x86:sbb64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+) :rbx) :rsi)
  (sys.lap-x86:jo sign-changed)
  ;; Sign didn't change.
  (sys.lap-x86:sar64 :rsi 63)
  sign-fixed
  (sys.lap-x86:mov64 (:r10 #.(+ (- +tag-array-like+) 8) :rbx) :rsi)
  (sys.lap-x86:cmp64 :rbx 8)
  (sys.lap-x86:je maybe-make-fixnum)
  not-fixnum
  (sys.lap-x86:mov64 :r8 :r10)
  finish
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret)
  maybe-make-fixnum
  ;; Attempt to convert the result to a fixnum.
  (sys.lap-x86:mov64 :rax (:r10 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:imul64 :rax 8)
  (sys.lap-x86:jo not-fixnum)
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:jmp finish)
  sx-left
  ;; Sign extend the left argument.
  ;; Previous value is not in RSI. Pull from the last word in the bignum.
  (sys.lap-x86:mov64 :rsi (:r8 #.(- +tag-array-like+) :rax))
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
  (sys.lap-x86:push 0) ; align
  (sys.lap-x86:mov64 :r8 (:constant "Aiee! Bignum overflow."))
  (sys.lap-x86:mov64 :r13 (:constant error))
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:call (:symbol-function :r13)))

(define-lap-function %%float-- ()
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
  (sys.lap-x86:lea64 :r8 (:rax #.+tag-single-float+))
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun generic-- (x y)
  (cond ((and (fixnump x)
              (fixnump y))
         (error "FIXNUM/FIXNUM case hit GENERIC--"))
        ((and (fixnump x)
              (bignump y))
         (%%bignum-- (%make-bignum-from-fixnum x) y))
        ((and (bignump x)
              (fixnump y))
         (%%bignum-- x (%make-bignum-from-fixnum y)))
        ((and (bignump x)
              (bignump y))
         (%%bignum-- x y))
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
           (%%float-- x* y*)))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination not supported."))))

;; Cheating here as well... Only deals with 1 word bignums.
(define-lap-function %%bignum-* ()
  ;; Read headers.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:shr64 :rax 8)
  (sys.lap-x86:shr64 :rdx 8)
  ;; TODO: Full multiplication...
  (sys.lap-x86:cmp64 :rax 1)
  (sys.lap-x86:jne not-simple)
  (sys.lap-x86:cmp64 :rdx 1)
  (sys.lap-x86:jne not-implemented)
  ;; 64-bit multiply.
  (sys.lap-x86:mov64 :rax (:r8 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:mov64 :rcx (:r9 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:imul64 :rcx)
  (sys.lap-x86:jo 128-bit-result)
  ;; One-word result. Attempt to convert it to a fixnum.
  (sys.lap-x86:mov64 :rcx :rax)
  (sys.lap-x86:imul64 :rcx 8)
  (sys.lap-x86:jo 64-bit-result)
  ;; Fixnum result
  (sys.lap-x86:mov64 :r8 :rcx)
  done
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret)
  64-bit-result
  ;; Value in RAX.
  (sys.lap-x86:mov64 :r13 (:constant %%make-bignum-64-rax))
  (sys.lap-x86:jmp (:symbol-function :r13))
  128-bit-result
  ;; Value in RDX:RAX.
  (sys.lap-x86:mov64 :r13 (:constant sys.int::%%make-bignum-128-rdx-rax))
  (sys.lap-x86:jmp (:symbol-function :r13))
  ;; Special hack for unsigned 64-bit values.
  not-simple
  (sys.lap-x86:cmp64 :rax 2)
  (sys.lap-x86:jne not-implemented)
  (sys.lap-x86:cmp64 (:r8 #.(+ (- +tag-array-like+) 16)) 0)
  (sys.lap-x86:jne not-implemented)
  (sys.lap-x86:mov64 :rax (:r9 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:cqo)
  (sys.lap-x86:test64 :rdx :rdx)
  (sys.lap-x86:jnz not-implemented)
  ;; Unsigned multiply.
  (sys.lap-x86:mov64 :rax (:r8 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:mul64 :rdx)
  ;; FIXME: Can this set the sign bit?
  (sys.lap-x86:jmp 128-bit-result)
  not-implemented
  (sys.lap-x86:mov64 :r8 (:constant "Full bignum * not implemented."))
  (sys.lap-x86:mov64 :r13 (:constant error))
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:push 0)
  (sys.lap-x86:call (:symbol-function :r13)))

(define-lap-function %%float-* ()
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
  (sys.lap-x86:lea64 :r8 (:rax #.+tag-single-float+))
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun generic-* (x y)
  (cond ((and (fixnump x)
              (fixnump y))
         (error "FIXNUM/FIXNUM case hit GENERIC-*"))
        ((and (fixnump x)
              (bignump y))
         (%%bignum-* (%make-bignum-from-fixnum x) y))
        ((and (bignump x)
              (fixnump y))
         (%%bignum-* x (%make-bignum-from-fixnum y)))
        ((and (bignump x)
              (bignump y))
         (%%bignum-* x y))
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
           (%%float-* x* y*)))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination not supported."))))

(define-lap-function %%bignum-logand ()
  ;; Save on the lisp stack.
  (sys.lap-x86:mov64 (:lsp -8) nil)
  (sys.lap-x86:mov64 (:lsp -16) nil)
  (sys.lap-x86:sub64 :lsp 16)
  (sys.lap-x86:mov64 (:lsp) :r8)
  (sys.lap-x86:mov64 (:lsp 8) :r9)
  ;; Read lengths.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:shr64 :rax 8)
  (sys.lap-x86:shr64 :rdx 8)
  ;; Allocate a new bignum large enough to hold the result.
  (sys.lap-x86:pushf)
  (sys.lap-x86:cli)
  (sys.lap-x86:mov64 :rcx :rax)
  (sys.lap-x86:cmp64 :rax :rdx)
  (sys.lap-x86:cmov64ng :rcx :rdx)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:push 0)
  (sys.lap-x86:lea64 :r8 ((:rcx 8)))
  (sys.lap-x86:test64 :r8 8)
  (sys.lap-x86:jz count-even)
  (sys.lap-x86:add64 :r8 8) ; one word for the header, no alignment.
  (sys.lap-x86:jmp do-allocate)
  count-even
  (sys.lap-x86:add64 :r8 16) ; one word for the header, one word for alignment.
  do-allocate
  (sys.lap-x86:mov64 :r9 (:constant :static))
  (sys.lap-x86:mov64 :rcx 16)
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:shl64 :rax 8)
  (sys.lap-x86:or64 :rax #.(ash +array-type-bignum+ +array-type-shift+))
  (sys.lap-x86:mov64 (:r8) :rax)
  (sys.lap-x86:lea64 :r10 (:r8 #.+tag-array-like+))
  (sys.lap-x86:popf)
  ;; Reread lengths.
  (sys.lap-x86:mov64 :r8 (:lsp))
  (sys.lap-x86:mov64 :r9 (:lsp 8))
  (sys.lap-x86:add64 :lsp 16)
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:shr64 :rax 8)
  (sys.lap-x86:shr64 :rdx 8)
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
  (sys.lap-x86:mov64 :rsi (:r8 #.(+ (- +tag-array-like+) 8) :rbx))
  sx-left-resume
  (sys.lap-x86:cmp64 :rbx :rdx)
  (sys.lap-x86:jae sx-right)
  (sys.lap-x86:mov64 :rdi (:r9 #.(+ (- +tag-array-like+) 8) :rbx))
  sx-right-resume
  (sys.lap-x86:add64 :rbx 8)
  (sys.lap-x86:sub64 :rcx 1)
  (sys.lap-x86:jz last)
  (sys.lap-x86:and64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+) :rbx) :rsi)
  (sys.lap-x86:jmp loop)
  last
  (sys.lap-x86:and64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+) :rbx) :rsi)
  ;; Crunch the bignum down to the correct size.
  ;; TODO: Not really correct...
  (sys.lap-x86:cmp64 :rbx 8)
  (sys.lap-x86:je stop-crunching)
  keep-crunching
  (sys.lap-x86:cmp64 (:r10 #.(- +tag-array-like+) :rbx) 0)
  (sys.lap-x86:je crunch)
  (sys.lap-x86:cmp64 (:r10 #.(- +tag-array-like+) :rbx) -1)
  (sys.lap-x86:je crunch)
  stop-crunching
  (sys.lap-x86:shl64 :rbx 5)
  (sys.lap-x86:mov64 :rax :rbx)
  (sys.lap-x86:or64 :rax #.(ash +array-type-bignum+ +array-type-shift+))
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+)) :rax)
  (sys.lap-x86:cmp64 :rbx #x100)
  (sys.lap-x86:je maybe-make-fixnum)
  not-fixnum
  (sys.lap-x86:mov64 :r8 :r10)
  finish
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret)
  crunch
  (sys.lap-x86:sub64 :rbx 8)
  (sys.lap-x86:cmp64 :rbx 8)
  (sys.lap-x86:je stop-crunching)
  (sys.lap-x86:jmp keep-crunching)
  maybe-make-fixnum
  ;; Attempt to convert the result to a fixnum.
  (sys.lap-x86:mov64 :rax (:r10 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:imul64 :rax 8)
  (sys.lap-x86:jo not-fixnum)
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:jmp finish)
  sx-left
  ;; Sign extend the left argument.
  ;; Previous value is not in RSI. Pull from the last word in the bignum.
  (sys.lap-x86:mov64 :rsi (:r8 #.(- +tag-array-like+) :rax))
  (sys.lap-x86:sar64 :rsi 63)
  (sys.lap-x86:jmp sx-left-resume)
  sx-right
  ;; Sign extend the right argument (previous value in RDI).
  (sys.lap-x86:sar64 :rdi 63)
  (sys.lap-x86:jmp sx-right-resume))

(defun generic-logand (x y)
  (cond ((and (fixnump x)
              (fixnump y))
         (error "FIXNUM/FIXNUM case hit GENERIC-LOGAND"))
        ((and (fixnump x)
              (bignump y))
         (%%bignum-logand (%make-bignum-from-fixnum x) y))
        ((and (bignump x)
              (fixnump y))
         (%%bignum-logand x (%make-bignum-from-fixnum y)))
        ((and (bignump x)
              (bignump y))
         (%%bignum-logand x y))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination not supported."))))

(define-lap-function %%bignum-logior ()
  ;; Save on the lisp stack.
  (sys.lap-x86:mov64 (:lsp -8) nil)
  (sys.lap-x86:mov64 (:lsp -16) nil)
  (sys.lap-x86:sub64 :lsp 16)
  (sys.lap-x86:mov64 (:lsp) :r8)
  (sys.lap-x86:mov64 (:lsp 8) :r9)
  ;; Read lengths.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:shr64 :rax 8)
  (sys.lap-x86:shr64 :rdx 8)
  ;; Allocate a new bignum large enough to hold the result.
  (sys.lap-x86:pushf)
  (sys.lap-x86:cli)
  (sys.lap-x86:mov64 :rcx :rax) ; rcx = len1
  (sys.lap-x86:cmp64 :rax :rdx) ; rdx = len2
  (sys.lap-x86:cmov64ng :rcx :rdx) ; rcx = !(len1 > len2) ? len2 : len1
  (sys.lap-x86:push :rax)
  (sys.lap-x86:push 0)
  (sys.lap-x86:lea64 :r8 ((:rcx 8)))
  (sys.lap-x86:test64 :r8 8)
  (sys.lap-x86:jz count-even)
  (sys.lap-x86:add64 :r8 8) ; one word for the header, no alignment.
  (sys.lap-x86:jmp do-allocate)
  count-even
  (sys.lap-x86:add64 :r8 16) ; one word for the header, one word for alignment.
  do-allocate
  (sys.lap-x86:mov64 :r9 (:constant :static))
  (sys.lap-x86:mov64 :rcx 16)
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:shl64 :rax 8)
  (sys.lap-x86:or64 :rax #.(ash +array-type-bignum+ +array-type-shift+))
  (sys.lap-x86:mov64 (:r8) :rax)
  (sys.lap-x86:lea64 :r10 (:r8 #.+tag-array-like+))
  (sys.lap-x86:popf)
  ;; Reread lengths.
  (sys.lap-x86:mov64 :r8 (:lsp))
  (sys.lap-x86:mov64 :r9 (:lsp 8))
  (sys.lap-x86:add64 :lsp 16)
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:shr64 :rax 8)
  (sys.lap-x86:shr64 :rdx 8)
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
  (sys.lap-x86:mov64 :rsi (:r8 #.(+ (- +tag-array-like+) 8) :rbx))
  sx-left-resume
  (sys.lap-x86:cmp64 :rbx :rdx)
  (sys.lap-x86:jae sx-right)
  (sys.lap-x86:mov64 :rdi (:r9 #.(+ (- +tag-array-like+) 8) :rbx))
  sx-right-resume
  (sys.lap-x86:add64 :rbx 8)
  (sys.lap-x86:sub64 :rcx 1)
  (sys.lap-x86:jz last)
  (sys.lap-x86:or64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+) :rbx) :rsi)
  (sys.lap-x86:jmp loop)
  last
  (sys.lap-x86:or64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+) :rbx) :rsi)
  ;; Crunch the bignum down to the correct size.
  ;; TODO: Not really correct...
  (sys.lap-x86:cmp64 :rbx 8)
  (sys.lap-x86:je stop-crunching)
  keep-crunching
  (sys.lap-x86:cmp64 (:r10 #.(- +tag-array-like+) :rbx) 0)
  (sys.lap-x86:je crunch)
  (sys.lap-x86:cmp64 (:r10 #.(- +tag-array-like+) :rbx) -1)
  (sys.lap-x86:je crunch)
  stop-crunching
  (sys.lap-x86:shl64 :rbx 5)
  (sys.lap-x86:mov64 :rax :rbx)
  (sys.lap-x86:or64 :rax #.(ash +array-type-bignum+ +array-type-shift+))
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+)) :rax)
  (sys.lap-x86:cmp64 :rbx #x100)
  (sys.lap-x86:je maybe-make-fixnum)
  not-fixnum
  (sys.lap-x86:mov64 :r8 :r10)
  finish
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret)
  crunch
  (sys.lap-x86:sub64 :rbx 8)
  (sys.lap-x86:cmp64 :rbx 8)
  (sys.lap-x86:je stop-crunching)
  (sys.lap-x86:jmp keep-crunching)
  maybe-make-fixnum
  ;; Attempt to convert the result to a fixnum.
  (sys.lap-x86:mov64 :rax (:r10 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:imul64 :rax 8)
  (sys.lap-x86:jo not-fixnum)
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:jmp finish)
  sx-left
  ;; Sign extend the left argument.
  ;; Previous value is not in RSI. Pull from the last word in the bignum.
  (sys.lap-x86:mov64 :rsi (:r8 #.(- +tag-array-like+) :rax))
  (sys.lap-x86:sar64 :rsi 63)
  (sys.lap-x86:jmp sx-left-resume)
  sx-right
  ;; Sign extend the right argument (previous value in RDI).
  (sys.lap-x86:sar64 :rdi 63)
  (sys.lap-x86:jmp sx-right-resume))

(defun generic-logior (x y)
  (cond ((and (fixnump x)
              (fixnump y))
         (error "FIXNUM/FIXNUM case hit GENERIC-LOGIOR"))
        ((and (fixnump x)
              (bignump y))
         (%%bignum-logior (%make-bignum-from-fixnum x) y))
        ((and (bignump x)
              (fixnump y))
         (%%bignum-logior x (%make-bignum-from-fixnum y)))
        ((and (bignump x)
              (bignump y))
         (%%bignum-logior x y))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination not supported."))))


(define-lap-function %%bignum-logxor ()
  ;; Save on the lisp stack.
  (sys.lap-x86:mov64 (:lsp -8) nil)
  (sys.lap-x86:mov64 (:lsp -16) nil)
  (sys.lap-x86:sub64 :lsp 16)
  (sys.lap-x86:mov64 (:lsp) :r8)
  (sys.lap-x86:mov64 (:lsp 8) :r9)
  ;; Read lengths.
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:shr64 :rax 8)
  (sys.lap-x86:shr64 :rdx 8)
  ;; Allocate a new bignum large enough to hold the result.
  (sys.lap-x86:pushf)
  (sys.lap-x86:cli)
  (sys.lap-x86:mov64 :rcx :rax)
  (sys.lap-x86:cmp64 :rax :rdx)
  (sys.lap-x86:cmov64ng :rcx :rdx)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:push 0)
  (sys.lap-x86:lea64 :r8 ((:rcx 8)))
  (sys.lap-x86:test64 :r8 8)
  (sys.lap-x86:jz count-even)
  (sys.lap-x86:add64 :r8 8) ; one word for the header, no alignment.
  (sys.lap-x86:jmp do-allocate)
  count-even
  (sys.lap-x86:add64 :r8 16) ; one word for the header, one word for alignment.
  do-allocate
  (sys.lap-x86:mov64 :r9 (:constant :static))
  (sys.lap-x86:mov64 :rcx 16)
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:shl64 :rax 8)
  (sys.lap-x86:or64 :rax #.(ash +array-type-bignum+ +array-type-shift+))
  (sys.lap-x86:mov64 (:r8) :rax)
  (sys.lap-x86:lea64 :r10 (:r8 #.+tag-array-like+))
  (sys.lap-x86:popf)
  ;; Reread lengths.
  (sys.lap-x86:mov64 :r8 (:lsp))
  (sys.lap-x86:mov64 :r9 (:lsp 8))
  (sys.lap-x86:add64 :lsp 16)
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:shr64 :rax 8)
  (sys.lap-x86:shr64 :rdx 8)
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
  (sys.lap-x86:mov64 :rsi (:r8 #.(+ (- +tag-array-like+) 8) :rbx))
  sx-left-resume
  (sys.lap-x86:cmp64 :rbx :rdx)
  (sys.lap-x86:jae sx-right)
  (sys.lap-x86:mov64 :rdi (:r9 #.(+ (- +tag-array-like+) 8) :rbx))
  sx-right-resume
  (sys.lap-x86:add64 :rbx 8)
  (sys.lap-x86:sub64 :rcx 1)
  (sys.lap-x86:jz last)
  (sys.lap-x86:xor64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+) :rbx) :rsi)
  (sys.lap-x86:jmp loop)
  last
  (sys.lap-x86:xor64 :rsi :rdi)
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+) :rbx) :rsi)
  ;; Crunch the bignum down to the correct size.
  ;; TODO: Not really correct...
  (sys.lap-x86:cmp64 :rbx 8)
  (sys.lap-x86:je stop-crunching)
  keep-crunching
  (sys.lap-x86:cmp64 (:r10 #.(- +tag-array-like+) :rbx) 0)
  (sys.lap-x86:je crunch)
  (sys.lap-x86:cmp64 (:r10 #.(- +tag-array-like+) :rbx) -1)
  (sys.lap-x86:je crunch)
  stop-crunching
  (sys.lap-x86:shl64 :rbx 5)
  (sys.lap-x86:mov64 :rax :rbx)
  (sys.lap-x86:or64 :rax #.(ash +array-type-bignum+ +array-type-shift+))
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+)) :rax)
  (sys.lap-x86:cmp64 :rbx #x100)
  (sys.lap-x86:je maybe-make-fixnum)
  not-fixnum
  (sys.lap-x86:mov64 :r8 :r10)
  finish
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret)
  crunch
  (sys.lap-x86:sub64 :rbx 8)
  (sys.lap-x86:cmp64 :rbx 8)
  (sys.lap-x86:je stop-crunching)
  (sys.lap-x86:jmp keep-crunching)
  maybe-make-fixnum
  ;; Attempt to convert the result to a fixnum.
  (sys.lap-x86:mov64 :rax (:r10 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:imul64 :rax 8)
  (sys.lap-x86:jo not-fixnum)
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:jmp finish)
  sx-left
  ;; Sign extend the left argument.
  ;; Previous value is not in RSI. Pull from the last word in the bignum.
  (sys.lap-x86:mov64 :rsi (:r8 #.(- +tag-array-like+) :rax))
  (sys.lap-x86:sar64 :rsi 63)
  (sys.lap-x86:jmp sx-left-resume)
  sx-right
  ;; Sign extend the right argument (previous value in RDI).
  (sys.lap-x86:sar64 :rdi 63)
  (sys.lap-x86:jmp sx-right-resume))

(defun generic-logxor (x y)
  (cond ((and (fixnump x)
              (fixnump y))
         (error "FIXNUM/FIXNUM case hit GENERIC-LOGXOR"))
        ((and (fixnump x)
              (bignump y))
         (%%bignum-logxor (%make-bignum-from-fixnum x) y))
        ((and (bignump x)
              (fixnump y))
         (%%bignum-logxor x (%make-bignum-from-fixnum y)))
        ((and (bignump x)
              (bignump y))
         (%%bignum-logxor x y))
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination not supported."))))

(define-lap-function %%bignum-left-shift ()
  ;; Save on the lisp stack.
  (sys.lap-x86:mov64 (:lsp -8) nil)
  (sys.lap-x86:mov64 (:lsp -16) nil)
  (sys.lap-x86:sub64 :lsp 16)
  (sys.lap-x86:mov64 (:lsp) :r8)
  (sys.lap-x86:mov64 (:lsp 8) :r9)
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:shr64 :rax 8)
    ;; Allocate a new bignum large enough to hold the result.
  (sys.lap-x86:pushf)
  (sys.lap-x86:cli)
  (sys.lap-x86:shl64 :rax 3)
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:test64 :r8 8)
  (sys.lap-x86:jz count-even)
  (sys.lap-x86:add64 :r8 24) ; one word for the header, one extra, no alignment.
  (sys.lap-x86:jmp do-allocate)
  count-even
  (sys.lap-x86:add64 :r8 32) ; one word for the header, one extra, one word for alignment.
  do-allocate
  (sys.lap-x86:mov64 :r9 (:constant :static))
  (sys.lap-x86:mov64 :rcx 16)
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:mov64 :r9 (:lsp))
  (sys.lap-x86:mov64 :rbx (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:add64 :rbx #.(ash 1 8))
  (sys.lap-x86:mov64 (:r8) :rbx)
  (sys.lap-x86:sub64 :rbx #.(ash 1 8))
  (sys.lap-x86:add64 :r8 #.+tag-array-like+)
  (sys.lap-x86:popf)
  ;; R8: dest
  ;; R9: src
  ;; CL: count
  ;; RBX: n words.
  ;; R10: current word.
  (sys.lap-x86:mov64 :rcx (:lsp 8))
  (sys.lap-x86:sar64 :rcx 3)
  (sys.lap-x86:shr64 :rbx 8)
  (sys.lap-x86:mov32 :r10d 1)
  loop
  (sys.lap-x86:cmp64 :r10 :rbx)
  (sys.lap-x86:jae last-word)
  (sys.lap-x86:mov64 :rax (:r9 #.(+ (- +tag-array-like+) 8) (:r10 8)))
  (sys.lap-x86:mov64 :rdx (:r9 #.(+ (- +tag-array-like+)) (:r10 8)))
  (sys.lap-x86:shld64 :rax :rdx :cl)
  (sys.lap-x86:mov64 (:r8 #.(+ (- +tag-array-like+) 8) (:r10 8)) :rax)
  (sys.lap-x86:add64 :r10 1)
  (sys.lap-x86:jmp loop)
  last-word
  (sys.lap-x86:mov64 :rax (:r9 #.(- +tag-array-like+) (:rbx 8)))
  (sys.lap-x86:cqo)
  (sys.lap-x86:shld64 :rdx :rax :cl)
  (sys.lap-x86:mov64 (:r8 #.(+ (- +tag-array-like+) 8) (:rbx 8)) :rdx)
  (sys.lap-x86:mov64 :rax (:r9 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:shl64 :rax :cl)
  (sys.lap-x86:mov64 (:r8 #.(+ (- +tag-array-like+) 8)) :rax)
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(define-lap-function %%bignum-right-shift ()
  ;; Save on the lisp stack.
  (sys.lap-x86:mov64 (:lsp -8) nil)
  (sys.lap-x86:mov64 (:lsp -16) nil)
  (sys.lap-x86:sub64 :lsp 16)
  (sys.lap-x86:mov64 (:lsp) :r8)
  (sys.lap-x86:mov64 (:lsp 8) :r9)
  (sys.lap-x86:mov64 :rax (:r8 #.(- +tag-array-like+)))
  (sys.lap-x86:shr64 :rax 8)
    ;; Allocate a new bignum large enough to hold the result.
  (sys.lap-x86:pushf)
  (sys.lap-x86:cli)
  (sys.lap-x86:shl64 :rax 3)
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:test64 :r8 8)
  (sys.lap-x86:jz count-even)
  (sys.lap-x86:add64 :r8 8) ; one word for the header, no alignment.
  (sys.lap-x86:jmp do-allocate)
  count-even
  (sys.lap-x86:add64 :r8 16) ; one word for the header, one word for alignment.
  do-allocate
  (sys.lap-x86:mov64 :r9 (:constant :static))
  (sys.lap-x86:mov64 :rcx 16)
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:mov64 :r9 (:lsp))
  (sys.lap-x86:mov64 :rbx (:r9 #.(- +tag-array-like+)))
  (sys.lap-x86:mov64 (:r8) :rbx)
  (sys.lap-x86:add64 :r8 #.+tag-array-like+)
  (sys.lap-x86:popf)
  ;; R8: dest
  ;; R9: src
  ;; CL: count (raw)
  ;; RBX: n words. (fixnum)
  ;; R10: current word. (fixnum)
  (sys.lap-x86:mov64 :rcx (:lsp 8))
  (sys.lap-x86:sar64 :rcx 3)
  (sys.lap-x86:and64 :rbx #.(lognot #xFF))
  (sys.lap-x86:shr64 :rbx 5)
  (sys.lap-x86:mov32 :r10d 8)
  loop
  (sys.lap-x86:cmp64 :r10 :rbx)
  (sys.lap-x86:jae last-word)
  ;; current+1
  (sys.lap-x86:mov64 :rax (:r9 #.(+ (- +tag-array-like+) 8) :r10))
  ;; current
  (sys.lap-x86:mov64 :rdx (:r9 #.(- +tag-array-like+) :r10))
  (sys.lap-x86:shrd64 :rdx :rax :cl)
  (sys.lap-x86:mov64 (:r8 #.(- +tag-array-like+) :r10) :rdx)
  (sys.lap-x86:add64 :r10 1)
  (sys.lap-x86:jmp loop)
  last-word
  (sys.lap-x86:mov64 :rax (:r9 #.(- +tag-array-like+) :rbx))
  (sys.lap-x86:sar64 :rax :cl)
  (sys.lap-x86:mov64 (:r8 #.(- +tag-array-like+) :rbx) :rax)
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun %ash (integer count)
  (cond ((not (fixnump count))
         (check-type count integer)
         (error "TODO: Bignum ASH count not implemented yet."))
        ((bignump integer)
         (cond
           ((plusp count)
            (when (>= count 64)
              (error "TODO: Bignum left shift with count >= 64"))
            (%%bignum-left-shift integer count))
           ((minusp count)
            (when (>= (- count) 64)
              (error "TODO: Bignum right shift with count >= 64"))
            (%%bignum-right-shift integer (- count)))
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
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun sqrt (number)
  (check-type number number)
  (etypecase number
    (real (%%float-sqrt (float number)))))
