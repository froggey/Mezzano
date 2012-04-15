(in-package #:sys.int)

(declaim (inline integerp))
(defun integerp (object)
  (or (system:fixnump object)
      (bignump object)))

(defun realp (object)
  (integerp object))

(defun numberp (object)
  (realp object))

(defun expt (base power)
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

(define-compiler-macro ldb (&whole whole bytespec integer)
  (cond ((and (listp bytespec)
              (= (length bytespec) 3)
              (eql (first bytespec) 'byte)
              (integerp (second bytespec))
              (not (minusp (second bytespec)))
              (integerp (third bytespec))
              (not (minusp (third bytespec))))
         `(logand (ash ,integer ,(- (third bytespec)))
                  ,(1- (ash 1 (second bytespec)))))
        (t whole)))

(define-compiler-macro dpb (&whole whole newbyte bytespec integer)
  (cond ((and (listp bytespec)
              (= (length bytespec) 3)
              (eql (first bytespec) 'byte)
              (integerp (second bytespec))
              (not (minusp (second bytespec)))
              (integerp (third bytespec))
              (not (minusp (third bytespec))))
         ;; Maintain correct order of evaluation for NEWBYTE and INTEGER.
         (let ((mask (1- (ash 1 (second bytespec)))))
           `(logior (ash (logand ,newbyte ,mask) ,(third bytespec))
                    (logand ,integer ,(lognot (ash mask (third bytespec)))))))
        (t whole)))

;;; From SBCL 1.0.55
(defun ceiling (number divisor)
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
  ;; Sign extend the left argument (previous value in RSI).
  (sys.lap-x86:sar64 :rsi 63)
  (sys.lap-x86:jmp sx-left-resume)
  sx-right
  ;; Sign extend the right argument (previous value in RDI).
  (sys.lap-x86:sar64 :rdi 63)
  (sys.lap-x86:jmp sx-right-resume))

(defun generic-< (x y)
  (check-type x number)
  (check-type y number)
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

(defun generic-= (x y)
  (check-type x number)
  (check-type y number)
  (when (eq x y)
    (return-from generic-= t))
  (cond
    ((or (fixnump x)
         (fixnump y))
     ;; Would have been caught by the EQ.
     ;; TODO: comparing against floats.
     nil)
    ((and (bignump x)
          (bignump y))
     (%%bignum-= x y))
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

(defun generic-truncate (number divisor)
  (assert (/= divisor 0) (number divisor) 'division-by-zero)
  ;; Avoid overflow when doing fixnum arithmetic.
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
        (t (check-type number number)
           (check-type divisor number)
           (error "Argument combination not supported."))))

(defun generic-rem (number divisor)
  (multiple-value-bind (quot rem)
      (generic-truncate number divisor)
    (declare (ignore quot))
    rem))

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
  (sys.lap-x86:add32 :eax :edx)
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
  (sys.lap-x86:mov64 :rcx 8)
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:shl64 :rax 8)
  (sys.lap-x86:or64 :rax #.(ash +array-type-bignum+ 1))
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
  ;; Crunch the bignum down to the correct size.
  (sys.lap-x86:shl64 :rbx 5)
  (sys.lap-x86:mov64 :rax :rbx)
  (sys.lap-x86:or64 :rax #.(ash +array-type-bignum+ 1))
  (sys.lap-x86:mov64 (:r10 #.(- +tag-array-like+)) :rax)
  (sys.lap-x86:cmp64 :rbx #x100)
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
  ;; Sign extend the left argument (previous value in RSI).
  (sys.lap-x86:sar64 :rsi 63)
  (sys.lap-x86:jmp sx-left-resume)
  sx-right
  ;; Sign extend the right argument (previous value in RDI).
  (sys.lap-x86:sar64 :rdi 63)
  (sys.lap-x86:jmp sx-right-resume)
  bignum-overflow
  (sys.lap-x86:push 0) ; align
  (sys.lap-x86:mov64 :r8 (:constant "Aiee! Bignum overflow."))
  (sys.lap-x86:mov64 :r13 (:constant error))
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:call (:symbol-function :r13)))

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
  ;; Special hack.
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
        (t (check-type x number)
           (check-type y number)
           (error "Argument combination not supported."))))
