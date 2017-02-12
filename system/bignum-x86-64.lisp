;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(define-lap-function %%bignum-< ()
  (:gc :no-frame :layout #*0)
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

(define-lap-function %%bignum-= ()
  (:gc :no-frame :layout #*0)
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

(define-lap-function %%bignum-+ ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
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
  (:gc :no-frame :layout #*0)
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

(define-lap-function %%bignum-- ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
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
  (:gc :no-frame :layout #*0)
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

;;; Unsigned multiply X & Y, must be of type (UNSIGNED-BYTE 64)
;;; This can be either a fixnum, a length-one bignum or a length-two bignum.
;;; Always returns an (UNSIGNED-BYTE 128) in a length-three bignum.
(define-lap-function %%bignum-multiply-step ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
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
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret)
  (:gc :frame)
  read-bignum-x
  (sys.lap-x86:mov64 :rax (:r8 #.(+ (- +tag-object+) 8)))
  (sys.lap-x86:jmp read-y)
  read-bignum-y
  (sys.lap-x86:mov64 :rcx (:r9 #.(+ (- +tag-object+) 8)))
  (sys.lap-x86:jmp perform-multiply))

(macrolet ((def (bignum-name op)
             `(define-lap-function ,bignum-name ()
                (:gc :no-frame :layout #*0)
                (sys.lap-x86:push :rbp)
                (:gc :no-frame :layout #*00)
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
                (:gc :no-frame :layout #*0)
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
                (sys.lap-x86:jmp sx-right-resume))))
  (def %%bignum-logand sys.lap-x86:and64)
  (def %%bignum-logior sys.lap-x86:or64)
  (def %%bignum-logxor sys.lap-x86:xor64))

(define-lap-function %%bignum-left-shift ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
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
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:jmp (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8)))))

(define-lap-function %%bignum-right-shift ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
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
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:jmp (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8)))))

;;; Convert a bignum to canonical form.
;;; If it can be represented as a fixnum it is converted,
;;; otherwise it is converted to the shortest possible bignum
;;; by removing redundant sign-extension bits.
(define-lap-function %%canonicalize-bignum ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
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
  (:gc :no-frame :layout #*0)
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

(define-lap-function %%make-bignum-128-rdx-rax ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:push :rdx)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:mov64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r8 #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:mov64 :r13 (:function %make-bignum-of-length))
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:pop (:object :r8 0))
  (sys.lap-x86:pop (:object :r8 1))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%make-bignum-64-rax ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:push 0)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:mov64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r8 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r13 (:function sys.int::%make-bignum-of-length))
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:pop (:object :r8 0))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret))
