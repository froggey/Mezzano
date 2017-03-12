;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for accessing memory.

(in-package :mezzano.compiler.codegen.x86-64)

;;; Direct access to memory.
;;; (MEMREF-type base offset) directly accesses memory at BASE + OFFSET * type-width.
;;; Valid types are: (un)signed-byte-{8,16,32,64} and t.
;;; MEMREF-T reads or writes a Lisp object.

(defmacro define-u-b-memref (name width read-op write-op register)
  `(progn
     (defbuiltin ,name (base offset) ()
       (load-in-reg :r9 base t)
       (fixnum-check :r9)
       (emit `(sys.lap-x86:mov64 :rdx :r9))
       (load-in-reg :r9 offset t)
       (fixnum-check :r9)
       (emit `(sys.lap-x86:mov64 :rcx :r9))
       (smash-r8)
       ;; BASE to raw integer.
       (emit '(sys.lap-x86:sar64 :rdx ,sys.int::+n-fixnum-bits+))
       ;; Convert OFFSET to a raw integer.
       (emit '(sys.lap-x86:sar64 :rcx ,sys.int::+n-fixnum-bits+))
       ;; Read
       (emit '(,read-op :eax (:rdx (:rcx ,width))))
       ;; Convert to fixnum.
       (emit '(sys.lap-x86:lea64 :r8 ((:rax ,(ash 1 sys.int::+n-fixnum-bits+)))))
       (setf *r8-value* (list (gensym))))
     (defbuiltin (setf ,name) (new-value base offset) ()
       (let ((type-error-label (gensym)))
         (emit-trailer (type-error-label)
           (raise-type-error :r8 '(unsigned-byte ,(* width 8))))
         (load-in-reg :r9 base t)
         (fixnum-check :r9)
         (emit `(sys.lap-x86:mov64 :rdx :r9))
         (load-in-reg :r9 offset t)
         (fixnum-check :r9)
         (emit `(sys.lap-x86:mov64 :rcx :r9))
         (load-in-r8 new-value t)
         (emit '(sys.lap-x86:mov64 :rax :r8)
               '(sys.lap-x86:test64 :rax ,sys.int::+fixnum-tag-mask+)
               `(sys.lap-x86:jnz ,type-error-label)
               '(sys.lap-x86:mov64 :rsi ,(fixnum-to-raw (ash 1 (* width 8))))
               '(sys.lap-x86:cmp64 :rax :rsi)
               `(sys.lap-x86:jae ,type-error-label)
               ;; Convert to raw integers.
               '(sys.lap-x86:sar64 :rdx ,sys.int::+n-fixnum-bits+)
               '(sys.lap-x86:sar64 :rcx ,sys.int::+n-fixnum-bits+)
               '(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+)
               ;; Write.
               '(,write-op (:rdx (:rcx ,width)) ,register))
         *r8-value*))))

(define-u-b-memref sys.int::%memref-unsigned-byte-8 1 sys.lap-x86:movzx8 sys.lap-x86:mov8 :al)
(define-u-b-memref sys.int::%memref-unsigned-byte-16 2 sys.lap-x86:movzx16 sys.lap-x86:mov16 :ax)
(define-u-b-memref sys.int::%memref-unsigned-byte-32 4 sys.lap-x86:mov32 sys.lap-x86:mov32 :eax)

(defbuiltin sys.int::%memref-unsigned-byte-64 (base offset) ()
  (load-in-reg :r8 base t)
  (fixnum-check :r8)
  (load-in-reg :r9 offset t)
  (fixnum-check :r9)
  (smash-r8)
  (emit `(sys.lap-x86:mov64 :rax :r8)
        `(sys.lap-x86:mov64 :rcx :r9)
        ;; Convert to raw integers.
        `(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+)
        `(sys.lap-x86:sar64 :rcx ,sys.int::+n-fixnum-bits+)
          ;; Read.
        `(sys.lap-x86:mov64 :rax (:rax (:rcx 8))))
  (box-unsigned-byte-64-rax)
  (setf *r8-value* (list (gensym))))

(defbuiltin (setf sys.int::%memref-unsigned-byte-64) (new-value base offset) ()
  (let ((type-error-label (gensym))
        (bignum-path (gensym "mr-ub64-bignum"))
        (len-2-bignum (gensym "mr-ub64-len-2-bignum"))
        (value-extracted (gensym "mr-ub64-value-extracted")))
    (emit-trailer (bignum-path)
      ;; Check for bignumness.
      (emit `(sys.lap-x86:and8 :dl #b1111)
            `(sys.lap-x86:cmp8 :dl ,sys.int::+tag-object+)
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot -1))
            `(sys.lap-x86:and8 :dl ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:cmp8 :dl ,(ash sys.int::+object-tag-bignum+
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:shr64 :rdx 8)
            ;; RDX = bignum length.
            `(sys.lap-x86:cmp64 :rdx 2)
            `(sys.lap-x86:je ,len-2-bignum)
            ;; Not length 2, must be length 1.
            `(sys.lap-x86:cmp64 :rdx 1)
            `(sys.lap-x86:jne ,type-error-label)
            ;; And the sign bit must be clear.
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot 0))
            `(sys.lap-x86:shl64 :rdx 1)
            `(sys.lap-x86:jc ,type-error-label)
            `(sys.lap-x86:rcr64 :rdx 1)
            `(sys.lap-x86:jmp ,value-extracted)
            len-2-bignum
            ;; Length 2 bignums must have the high word be 0.
            `(sys.lap-x86:cmp64 ,(object-ea :r8 :slot 1) 0)
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot 0))
            `(sys.lap-x86:jmp ,value-extracted)
            type-error-label)
      (raise-type-error :r8 '(unsigned-byte 64)))
    (load-in-reg :r9 base t)
    (fixnum-check :r9)
    (emit `(sys.lap-x86:mov64 :rax :r9))
    (load-in-reg :r9 offset t)
    (fixnum-check :r9)
    (emit `(sys.lap-x86:mov64 :rcx :r9))
    (load-in-r8 new-value t)
    (emit `(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+)
	  `(sys.lap-x86:sar64 :rcx ,sys.int::+n-fixnum-bits+)
          `(sys.lap-x86:mov64 :rdx :r8)
	  `(sys.lap-x86:test64 :rdx ,sys.int::+fixnum-tag-mask+)
	  `(sys.lap-x86:jnz ,bignum-path)
          `(sys.lap-x86:cmp64 :r8 0)
          `(sys.lap-x86:jl ,type-error-label)
	  ;; Convert to raw integers.
	  `(sys.lap-x86:sar64 :rdx ,sys.int::+n-fixnum-bits+)
          value-extracted
	  ;; Write.
	  `(sys.lap-x86:mov64 (:rax (:rcx 8)) :rdx))
    *r8-value*))

(defmacro define-s-b-memref (name width read-op write-op register)
  `(progn
     (defbuiltin ,name (base offset) ()
       (load-in-reg :r9 base t)
       (fixnum-check :r9)
       (emit `(sys.lap-x86:mov64 :rdx :r9))
       (load-in-reg :r9 offset t)
       (fixnum-check :r9)
       (emit `(sys.lap-x86:mov64 :rcx :r9))
       (smash-r8)
       ;; BASE & OFFSET to raw integer.
       (emit '(sys.lap-x86:sar64 :rdx ,sys.int::+n-fixnum-bits+)
             '(sys.lap-x86:sar64 :rcx ,sys.int::+n-fixnum-bits+))
       ;; Read it.
       (emit '(,read-op :rax (:rdx (:rcx ,width))))
       ;; Convert to fixnum.
       (emit '(sys.lap-x86:lea64 :r8 ((:rax ,(ash 1 sys.int::+n-fixnum-bits+)))))
       (setf *r8-value* (list (gensym))))
     (defbuiltin (setf ,name) (new-value base offset) ()
       (let ((type-error-label (gensym)))
         (emit-trailer (type-error-label)
           (raise-type-error :r8 '(signed-byte ,(* width 8))))
         (load-in-reg :r9 base t)
         (fixnum-check :r9)
         (emit `(sys.lap-x86:mov64 :rdx :r9))
         (load-in-reg :r9 offset t)
         (fixnum-check :r9)
         (emit `(sys.lap-x86:mov64 :rcx :r9))
         (load-in-r8 new-value t)
         (emit '(sys.lap-x86:mov64 :rax :r8)
               '(sys.lap-x86:test64 :rax ,sys.int::+fixnum-tag-mask+)
               `(sys.lap-x86:jnz ,type-error-label)
               '(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+)
               '(sys.lap-x86:mov64 :rsi :rax)
               '(sys.lap-x86:mov64 :rdi ,(ash 1 (1- (* width 8))))
               '(sys.lap-x86:cmp64 :rsi :rdi)
               `(sys.lap-x86:jge ,type-error-label)
               '(sys.lap-x86:mov64 :rdi ,(- (ash 1 (1- (* width 8)))))
               '(sys.lap-x86:cmp64 :rsi :rdi)
               `(sys.lap-x86:jl ,type-error-label)
               ;; Convert to raw integers.
               '(sys.lap-x86:sar64 :rdx ,sys.int::+n-fixnum-bits+)
               '(sys.lap-x86:sar64 :rcx ,sys.int::+n-fixnum-bits+)
               ;; Write.
               '(,write-op (:rdx (:rcx ,width)) ,register))
         *r8-value*))))

(define-s-b-memref sys.int::%memref-signed-byte-8 1 sys.lap-x86:movsx8 sys.lap-x86:mov8 :al)
(define-s-b-memref sys.int::%memref-signed-byte-16 2 sys.lap-x86:movsx16 sys.lap-x86:mov16 :ax)
(define-s-b-memref sys.int::%memref-signed-byte-32 4 sys.lap-x86:movsx32 sys.lap-x86:mov32 :eax)

(defbuiltin sys.int::%memref-signed-byte-64 (base offset) ()
  (let ((overflow-label (gensym))
        (resume (gensym)))
    (emit-trailer (overflow-label)
      (emit `(sys.lap-x86:rcr64 :rax 1)
            `(sys.lap-x86:mov64 :r13 (:function sys.int::%%make-bignum-64-rax))
            `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
            `(sys.lap-x86:jmp ,resume)))
    (load-in-reg :r8 base t)
    (fixnum-check :r8)
    (load-in-reg :r9 offset t)
    (fixnum-check :r9)
    (smash-r8)
    (emit `(sys.lap-x86:mov64 :rax :r8)
          `(sys.lap-x86:mov64 :rcx :r9)
	  ;; Convert to raw integers.
	  `(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+)
	  `(sys.lap-x86:sar64 :rcx ,sys.int::+n-fixnum-bits+)
          ;; Read.
          `(sys.lap-x86:mov64 :rax (:rax (:rcx 8)))
          ;; Convert to fixnum & check for signed overflow.
          ;; Assumes fixnum size of 1!
          `(sys.lap-x86:shl64 :rax 1)
          `(sys.lap-x86:jo ,overflow-label)
          `(sys.lap-x86:mov64 :r8 :rax)
          resume)
    (setf *r8-value* (list (gensym)))))

(defbuiltin (setf sys.int::%memref-signed-byte-64) (new-value base offset) ()
  (let ((type-error-label (gensym))
        (bignum-path (gensym "mr-sb64-bignum"))
        (value-extracted (gensym "mr-sb64-value-extracted")))
    (emit-trailer (bignum-path)
      ;; Check for bignumness.
      (emit `(sys.lap-x86:and8 :dl #b1111)
            `(sys.lap-x86:cmp8 :dl ,sys.int::+tag-object+)
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot -1))
            `(sys.lap-x86:and8 :dl ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:cmp8 :dl ,(ash sys.int::+object-tag-bignum+
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:jne ,type-error-label)
            ;; Length check.
            `(sys.lap-x86:and64 :rdx ,(lognot (1- (ash 1 sys.int::+object-data-shift+))))
            `(sys.lap-x86:cmp64 :rdx ,(ash 1 sys.int::+object-data-shift+))
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot 0))
            `(sys.lap-x86:jmp ,value-extracted)
            type-error-label)
      (raise-type-error :r8 '(signed-byte 64)))
    (load-in-reg :r9 base t)
    (fixnum-check :r9)
    (emit `(sys.lap-x86:mov64 :rax :r9))
    (load-in-reg :r9 offset t)
    (fixnum-check :r9)
    (emit `(sys.lap-x86:mov64 :rcx :r9))
    (load-in-r8 new-value t)
    (emit `(sys.lap-x86:mov64 :rdx :r8)
	  `(sys.lap-x86:test64 :rdx ,sys.int::+fixnum-tag-mask+)
	  `(sys.lap-x86:jnz ,bignum-path)
	  ;; Convert to raw integers
	  `(sys.lap-x86:sar64 :rdx ,sys.int::+n-fixnum-bits+)
          value-extracted
	  `(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+)
	  `(sys.lap-x86:sar64 :rcx ,sys.int::+n-fixnum-bits+)
	  ;; Write.
	  `(sys.lap-x86:mov64 (:rax (:rcx 8)) :rdx))
    *r8-value*))

(defbuiltin sys.int::%memref-t (base offset) ()
  (load-in-reg :r9 base t)
  (fixnum-check :r9)
  (emit `(sys.lap-x86:mov64 :rax :r9))
  (load-in-reg :r9 offset t)
  (fixnum-check :r9)
  (emit `(sys.lap-x86:mov64 :rcx :r9))
  (smash-r8)
  (emit ;; Convert to raw integers.
   `(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+)
   `(sys.lap-x86:sar64 :rcx ,sys.int::+n-fixnum-bits+)
   ;; Read.
   `(sys.lap-x86:mov64 :r8 (:rax (:rcx 8))))
  (setf *r8-value* (list (gensym))))

(defbuiltin (setf sys.int::%memref-t) (new-value base offset) ()
  (load-in-reg :r9 base t)
  (fixnum-check :r9)
  (emit `(sys.lap-x86:mov64 :rax :r9))
  (load-in-reg :r9 offset t)
  (fixnum-check :r9)
  (emit `(sys.lap-x86:mov64 :rcx :r9))
  (load-in-r8 new-value t)
  (emit ;; Convert to raw integers.
   `(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+)
   `(sys.lap-x86:sar64 :rcx ,sys.int::+n-fixnum-bits+)
   ;; Write.
   `(sys.lap-x86:mov64 (:rax (:rcx 8)) :r8))
  *r8-value*)

;;; Access to slots in homogeneous objects.
;;; (%OBJECT-REF-type object slot) accesses the value in OBJECT at SLOT.
;;; Slots are stored linearly after the object's header and are assumed to all be the same size.
;;; This does not account for the additional alignment constraints of float arrays & similar.
;;; Valid types are: (un)signed-byte-{8,16,32,64} and t.
;;; %OBJECT-REF-T reads or writes a Lisp object.
;;; These functions are only valid on objects tagged with +TAG-OBJECT+, and do not test this.
;;; These functions are GC safe, while accessing a slot using MEMREF-type and
;;; LISP-OBJECT-ADDRESS is not.

(defmacro define-u-b-object-ref (name width read-op write-op register)
  `(progn
     (defbuiltin ,name (object offset) ()
       (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                                   (second offset))))
         (unless constant-offset
           (load-in-reg :rdi offset t)
           ;; Convert to unboxed integer.
           (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
         (load-in-reg :r9 object t)
         (smash-r8)
         ;; Read.
         (cond (constant-offset
                (emit `(,',read-op :eax (:r9 ,(+ (- sys.int::+tag-object+) 8 (* constant-offset ,width))))))
               (t
                (emit `(,',read-op :eax ,(object-ea :r9 :index `(:rdi ,',width))))))
         ;; Convert to fixnum.
         (emit '(sys.lap-x86:lea64 :r8 ((:rax ,(ash 1 sys.int::+n-fixnum-bits+)))))
         (setf *r8-value* (list (gensym)))))
     (defbuiltin ,(intern (format nil "~A-UNSCALED" name) (symbol-package name)) (object offset) ()
       (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                                   (second offset))))
         (unless constant-offset
           (load-in-reg :rdi offset t)
           ;; Convert to unboxed integer.
           (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
         (load-in-reg :r9 object t)
         (smash-r8)
         ;; Read.
         (cond (constant-offset
                (emit `(,',read-op :eax (:r9 ,(+ (- sys.int::+tag-object+) 8 constant-offset)))))
               (t
                (emit `(,',read-op :eax ,(object-ea :r9 :index `(:rdi 1))))))
         ;; Convert to fixnum.
         (emit '(sys.lap-x86:lea64 :r8 ((:rax ,(ash 1 sys.int::+n-fixnum-bits+)))))
         (setf *r8-value* (list (gensym)))))
     (defbuiltin (setf ,name) (new-value object offset) ()
       (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                                   (second offset))))
         (unless constant-offset
           (load-in-reg :rdi offset t)
           ;; Convert to unboxed integer.
           (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
         (load-in-reg :r9 object t)
         ;; Convert to raw integer.
         (load-in-r8 new-value t)
         (emit '(sys.lap-x86:mov64 :rax :r8)
               '(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+))
         ;; Write.
         (cond (constant-offset
                (emit `(,',write-op ,(list :r9 (+ (- sys.int::+tag-object+) 8 (* constant-offset ,width))) ,',register)))
               (t
                (emit '(,write-op ,(object-ea :r9 :index `(:rdi ,width)) ,register))))
         *r8-value*))
     (defbuiltin (setf ,(intern (format nil "~A-UNSCALED" name) (symbol-package name))) (new-value object offset) ()
       (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                                   (second offset))))
         (unless constant-offset
           (load-in-reg :rdi offset t)
           ;; Convert to unboxed integer.
           (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
         (load-in-reg :r9 object t)
         ;; Convert to raw integer.
         (load-in-r8 new-value t)
         (emit '(sys.lap-x86:mov64 :rax :r8)
               '(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+))
         ;; Write.
         (cond (constant-offset
                (emit `(,',write-op ,(list :r9 (+ (- sys.int::+tag-object+) 8 constant-offset)) ,',register)))
               (t
                (emit '(,write-op ,(object-ea :r9 :index `(:rdi 1)) ,register))))
         *r8-value*))))

(define-u-b-object-ref sys.int::%%object-ref-unsigned-byte-8  1 sys.lap-x86:movzx8  sys.lap-x86:mov8  :al)
(define-u-b-object-ref sys.int::%%object-ref-unsigned-byte-16 2 sys.lap-x86:movzx16 sys.lap-x86:mov16 :ax)
(define-u-b-object-ref sys.int::%%object-ref-unsigned-byte-32 4 sys.lap-x86:mov32   sys.lap-x86:mov32 :eax)

(defbuiltin sys.int::%object-ref-unsigned-byte-64 (object offset) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r8 object t)
    ;; Read.
    (cond (constant-offset
           (emit `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :slot constant-offset))))
          (t
           (emit `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :index '(:rdi 8))))))
    (box-unsigned-byte-64-rax)
    (setf *r8-value* (list (gensym)))))

(defbuiltin (setf sys.int::%object-ref-unsigned-byte-64) (new-value object offset) ()
  (let ((type-error-label (gensym))
        (bignum-path (gensym "alr-ub64-bignum"))
        (len-2-bignum (gensym "alr-ub64-len-2-bignum"))
        (value-extracted (gensym "alr-ub64-value-extracted"))
        (constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (emit-trailer (bignum-path)
      ;; Check for bignumness.
      (emit `(sys.lap-x86:and8 :dl #b1111)
            `(sys.lap-x86:cmp8 :dl ,sys.int::+tag-object+)
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot -1))
            `(sys.lap-x86:and8 :dl ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:cmp8 :dl ,(ash sys.int::+object-tag-bignum+
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:shr64 :rdx ,sys.int::+object-data-shift+)
            ;; RDX = bignum length.
            `(sys.lap-x86:cmp64 :rdx 2)
            `(sys.lap-x86:je ,len-2-bignum)
            ;; Not length 2, must be length 1.
            `(sys.lap-x86:cmp64 :rdx 1)
            `(sys.lap-x86:jne ,type-error-label)
            ;; And the sign bit must be clear.
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot 0))
            `(sys.lap-x86:shl64 :rdx 1)
            `(sys.lap-x86:jc ,type-error-label)
            `(sys.lap-x86:rcr64 :rdx 1)
            `(sys.lap-x86:jmp ,value-extracted)
            len-2-bignum
            ;; Length 2 bignums must have the high word be 0.
            `(sys.lap-x86:cmp64 ,(object-ea :r8 :slot 1) 0)
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot 0))
            `(sys.lap-x86:jmp ,value-extracted)
            type-error-label)
      (raise-type-error :r8 '(unsigned-byte 64)))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r9 object t)
    (load-in-r8 new-value t)
    (emit `(sys.lap-x86:mov64 :rdx :r8)
	  `(sys.lap-x86:test64 :rdx ,sys.int::+fixnum-tag-mask+)
	  `(sys.lap-x86:jnz ,bignum-path)
          `(sys.lap-x86:cmp64 :r8 0)
          `(sys.lap-x86:jl ,type-error-label)
	  ;; Convert to raw integer.
	  `(sys.lap-x86:sar64 :rdx ,sys.int::+n-fixnum-bits+)
          value-extracted)
    (cond (constant-offset
           (emit `(sys.lap-x86:mov64 ,(object-ea :r9 :slot constant-offset) :rdx)))
          (t
           (emit `(sys.lap-x86:mov64 ,(object-ea :r9 :index '(:rdi 8)) :rdx))))
    *r8-value*))

(defbuiltin sys.int::%object-ref-unsigned-byte-64-unscaled (object offset) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r8 object t)
    ;; Read.
    (cond (constant-offset
           (emit `(sys.lap-x86:mov64 :rax (:r8 ,(+ (- sys.int::+tag-object+) 8 constant-offset)))))
          (t
           (emit `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :index '(:rdi 1))))))
    (box-unsigned-byte-64-rax)
    (setf *r8-value* (list (gensym)))))

(defbuiltin (setf sys.int::%object-ref-unsigned-byte-64-unscaled) (new-value object offset) ()
  (let ((type-error-label (gensym))
        (bignum-path (gensym "alr-ub64-bignum"))
        (len-2-bignum (gensym "alr-ub64-len-2-bignum"))
        (value-extracted (gensym "alr-ub64-value-extracted"))
        (constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (emit-trailer (bignum-path)
      ;; Check for bignumness.
      (emit `(sys.lap-x86:and8 :dl #b1111)
            `(sys.lap-x86:cmp8 :dl ,sys.int::+tag-object+)
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot -1))
            `(sys.lap-x86:and8 :dl ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:cmp8 :dl ,(ash sys.int::+object-tag-bignum+
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:shr64 :rdx ,sys.int::+object-data-shift+)
            ;; RDX = bignum length.
            `(sys.lap-x86:cmp64 :rdx 2)
            `(sys.lap-x86:je ,len-2-bignum)
            ;; Not length 2, must be length 1.
            `(sys.lap-x86:cmp64 :rdx 1)
            `(sys.lap-x86:jne ,type-error-label)
            ;; And the sign bit must be clear.
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot 0))
            `(sys.lap-x86:shl64 :rdx 1)
            `(sys.lap-x86:jc ,type-error-label)
            `(sys.lap-x86:rcr64 :rdx 1)
            `(sys.lap-x86:jmp ,value-extracted)
            len-2-bignum
            ;; Length 2 bignums must have the high word be 0.
            `(sys.lap-x86:cmp64 ,(object-ea :r8 :slot 1) 0)
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot 0))
            `(sys.lap-x86:jmp ,value-extracted)
            type-error-label)
      (raise-type-error :r8 '(unsigned-byte 64)))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r9 object t)
    (load-in-r8 new-value t)
    (emit `(sys.lap-x86:mov64 :rdx :r8)
	  `(sys.lap-x86:test64 :rdx ,sys.int::+fixnum-tag-mask+)
	  `(sys.lap-x86:jnz ,bignum-path)
          `(sys.lap-x86:cmp64 :r8 0)
          `(sys.lap-x86:jl ,type-error-label)
	  ;; Convert to raw integer.
	  `(sys.lap-x86:sar64 :rdx ,sys.int::+n-fixnum-bits+)
          value-extracted)
    (cond (constant-offset
           (emit `(sys.lap-x86:mov64 (:r9 ,(+ (- sys.int::+tag-object+) 8 constant-offset)) :rdx)))
          (t
           (emit `(sys.lap-x86:mov64 ,(object-ea :r9 :index '(:rdi 1)) :rdx))))
    *r8-value*))

(defmacro define-s-b-object-ref (name width read-op write-op register)
  `(progn
     ;; Read function.
     (defbuiltin ,name (object offset) ()
       (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                                   (second offset))))
         (unless constant-offset
           (load-in-reg :rdi offset t)
           ;; Convert to unboxed integer.
           (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
         (load-in-reg :r9 object t)
         (smash-r8)
         ;; Read.
         (cond (constant-offset
                (emit `(,',read-op :rax (:r9 ,(+ (- sys.int::+tag-object+) 8 (* constant-offset ,width))))))
               (t
                (emit '(,read-op :rax ,(object-ea :r9 :index `(:rdi ,width))))))
         ;; Convert to fixnum.
         (emit '(sys.lap-x86:lea64 :r8 ((:rax ,(ash 1 sys.int::+n-fixnum-bits+)))))
         (setf *r8-value* (list (gensym)))))
     (defbuiltin ,(intern (format nil "~A-UNSCALED" name) (symbol-package name)) (object offset) ()
       (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                                   (second offset))))
         (unless constant-offset
           (load-in-reg :rdi offset t)
           ;; Convert to unboxed integer.
           (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
         (load-in-reg :r9 object t)
         (smash-r8)
         ;; Read.
         (cond (constant-offset
                (emit `(,',read-op :rax (:r9 ,(+ (- sys.int::+tag-object+) 8 constant-offset)))))
               (t
                (emit '(,read-op :rax ,(object-ea :r9 :index `(:rdi 1))))))
         ;; Convert to fixnum.
         (emit '(sys.lap-x86:lea64 :r8 ((:rax ,(ash 1 sys.int::+n-fixnum-bits+)))))
         (setf *r8-value* (list (gensym)))))
     ;; Write function.
     (defbuiltin (setf ,name) (new-value object offset) ()
       (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                                   (second offset))))
         (unless constant-offset
           (load-in-reg :rcx offset t)
           ;; Convert to unboxed integer.
           (emit `(sys.lap-x86:sar64 :rcx ,sys.int::+n-fixnum-bits+)))
         (load-in-reg :r9 object t)
         (load-in-r8 new-value t)
         (emit '(sys.lap-x86:mov64 :rax :r8)
               '(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+))
         ;; Write
         (cond (constant-offset
                (emit `(,',write-op ,(list :r9 (+ (- sys.int::+tag-object+) 8 (* constant-offset ,width))) ,',register)))
               (t
                (emit '(,write-op ,(object-ea :r9 :index `(:rcx ,width)) ,register))))
         *r8-value*))
     (defbuiltin (setf ,(intern (format nil "~A-UNSCALED" name) (symbol-package name))) (new-value object offset) ()
       (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                                   (second offset))))
         (unless constant-offset
           (load-in-reg :rcx offset t)
           ;; Convert to unboxed integer.
           (emit `(sys.lap-x86:sar64 :rcx ,sys.int::+n-fixnum-bits+)))
         (load-in-reg :r9 object t)
         (load-in-r8 new-value t)
         (emit '(sys.lap-x86:mov64 :rax :r8)
               '(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+))
         ;; Write
         (cond (constant-offset
                (emit `(,',write-op ,(list :r9 (+ (- sys.int::+tag-object+) 8 constant-offset)) ,',register)))
               (t
                (emit '(,write-op ,(object-ea :r9 :index `(:rcx 1)) ,register))))
         *r8-value*))))

(define-s-b-object-ref sys.int::%%object-ref-signed-byte-8  1 sys.lap-x86:movsx8  sys.lap-x86:mov8  :al)
(define-s-b-object-ref sys.int::%%object-ref-signed-byte-16 2 sys.lap-x86:movsx16 sys.lap-x86:mov16 :ax)
(define-s-b-object-ref sys.int::%%object-ref-signed-byte-32 4 sys.lap-x86:movsx32 sys.lap-x86:mov32 :eax)

(defbuiltin sys.int::%object-ref-signed-byte-64 (object offset) ()
  (let ((overflow-label (gensym))
        (resume (gensym))
        (constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (emit-trailer (overflow-label)
      ;; Undo the shift, then call the helper.
      (emit `(sys.lap-x86:rcr64 :rax 1))
      (emit `(sys.lap-x86:mov64 :r13 (:function sys.int::%%make-bignum-64-rax))
            `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
            `(sys.lap-x86:jmp ,resume)))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r8 object t)
    (smash-r8)
    ;; Read.
    (cond (constant-offset
           (emit `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :slot constant-offset))))
          (t
           (emit `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :index '(:rdi 8))))))
    ;; Convert to fixnum & check for signed overflow.
    ;; Assumes fixnum size of 1!
    (emit `(sys.lap-x86:shl64 :rax 1)
          `(sys.lap-x86:jo ,overflow-label)
          `(sys.lap-x86:mov64 :r8 :rax)
          resume)
    (setf *r8-value* (list (gensym)))))

(defbuiltin (setf sys.int::%object-ref-signed-byte-64) (new-value object offset) ()
  (let ((type-error-label (gensym))
        (bignum-path (gensym "alr-sb64-bignum"))
        (value-extracted (gensym "alr-sb64-value-extracted"))
        (constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (emit-trailer (bignum-path)
      ;; Check for bignumness.
      (emit `(sys.lap-x86:and8 :dl #b1111)
            `(sys.lap-x86:cmp8 :dl ,sys.int::+tag-object+)
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot -1))
            `(sys.lap-x86:and8 :dl ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:cmp8 :dl ,(ash sys.int::+object-tag-bignum+
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:and64 :rdx ,(lognot (1- (ash 1 sys.int::+object-data-shift+))))
            `(sys.lap-x86:cmp64 :rdx ,(ash 1 sys.int::+object-data-shift+))
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot 0))
            `(sys.lap-x86:jmp ,value-extracted)
            type-error-label)
      (raise-type-error :r8 '(signed-byte 64)))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r9 object t)
    (load-in-r8 new-value t)
    (emit `(sys.lap-x86:mov64 :rdx :r8)
	  `(sys.lap-x86:test64 :rdx ,sys.int::+fixnum-tag-mask+)
	  `(sys.lap-x86:jnz ,bignum-path)
	  ;; Convert to raw integer.
	  `(sys.lap-x86:sar64 :rdx ,sys.int::+n-fixnum-bits+)
          value-extracted)
    ;; Write.
    (cond (constant-offset
           (emit `(sys.lap-x86:mov64 ,(object-ea :r9 :slot constant-offset) :rdx)))
          (t
           (emit `(sys.lap-x86:mov64 ,(object-ea :r9 :index '(:rdi 8)) :rdx))))
    *r8-value*))

(defbuiltin sys.int::%object-ref-signed-byte-64-unscaled (object offset) ()
  (let ((overflow-label (gensym))
        (resume (gensym))
        (constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (emit-trailer (overflow-label)
      ;; Undo the shift, then call the helper.
      (emit `(sys.lap-x86:rcr64 :rax 1))
      (emit `(sys.lap-x86:mov64 :r13 (:function sys.int::%%make-bignum-64-rax))
            `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
            `(sys.lap-x86:jmp ,resume)))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r8 object t)
    (smash-r8)
    ;; Read.
    (cond (constant-offset
           (emit `(sys.lap-x86:mov64 :rax (:r8 ,(+ (- sys.int::+tag-object+) 8 constant-offset)))))
          (t
           (emit `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :index '(:rdi 1))))))
    ;; Convert to fixnum & check for signed overflow.
    ;; Assumes fixnum size of 1!
    (emit `(sys.lap-x86:shl64 :rax 1)
          `(sys.lap-x86:jo ,overflow-label)
          `(sys.lap-x86:mov64 :r8 :rax)
          resume)
    (setf *r8-value* (list (gensym)))))

(defbuiltin (setf sys.int::%object-ref-signed-byte-64-unscaled) (new-value object offset) ()
  (let ((type-error-label (gensym))
        (bignum-path (gensym "alr-sb64-bignum"))
        (value-extracted (gensym "alr-sb64-value-extracted"))
        (constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (emit-trailer (bignum-path)
      ;; Check for bignumness.
      (emit `(sys.lap-x86:and8 :dl #b1111)
            `(sys.lap-x86:cmp8 :dl ,sys.int::+tag-object+)
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot -1))
            `(sys.lap-x86:and8 :dl ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:cmp8 :dl ,(ash sys.int::+object-tag-bignum+
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:and64 :rdx ,(lognot (1- (ash 1 sys.int::+object-data-shift+))))
            `(sys.lap-x86:cmp64 :rdx ,(ash 1 sys.int::+object-data-shift+))
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:mov64 :rdx ,(object-ea :r8 :slot 0))
            `(sys.lap-x86:jmp ,value-extracted)
            type-error-label)
      (raise-type-error :r8 '(signed-byte 64)))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r9 object t)
    (load-in-r8 new-value t)
    (emit `(sys.lap-x86:mov64 :rdx :r8)
	  `(sys.lap-x86:test64 :rdx ,sys.int::+fixnum-tag-mask+)
	  `(sys.lap-x86:jnz ,bignum-path)
	  ;; Convert to raw integer.
	  `(sys.lap-x86:sar64 :rdx ,sys.int::+n-fixnum-bits+)
          value-extracted)
    ;; Write.
    (cond (constant-offset
           (emit `(sys.lap-x86:mov64 (:r9 ,(+ (- sys.int::+tag-object+) 8 constant-offset)) :rdx)))
          (t
           (emit `(sys.lap-x86:mov64 ,(object-ea :r9 :index '(:rdi 1)) :rdx))))
    *r8-value*))

(defbuiltin sys.int::%object-ref-t (object offset) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r8 object t)
    (smash-r8)
    ;; Read.
    (cond (constant-offset
           (emit `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :slot constant-offset))))
          (t
           (emit `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :index '(:rdi 8))))))
    (setf *r8-value* (list (gensym)))))

(defbuiltin (setf sys.int::%object-ref-t) (new-value object offset) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r9 object t)
    (load-in-r8 new-value t)
    ;; Write.
    (cond (constant-offset
           (emit `(sys.lap-x86:mov64 ,(object-ea :r9 :slot constant-offset) :r8)))
          (t
           (emit `(sys.lap-x86:mov64 ,(object-ea :r9 :index '(:rdi 8)) :r8))))
    *r8-value*))

;;; Atomic operations.
;;; These functions index into the object like %OBJECT-REF-T.
;;; There are no atomic functions that access memory like MEMREF.

;; Add DELTA to the slot at SLOT in OBJECT.
;; Returns the old value of the slot.
;; DELTA and the value of the slot must both be fixnums.
;; (defun fixnum-add (object slot delta)
;;   (prog1 (%object-ref-t object slot)
;;     (incf (%object-ref-t object slot) delta)))
(defbuiltin sys.int::%atomic-fixnum-add-object (object offset delta) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r9 object t)
    (load-in-reg :r8 delta t)
    (fixnum-check :r8)
    (smash-r8)
    ;; Atomic add.
    (cond (constant-offset
           (emit `(sys.lap-x86:lock)
                 `(sys.lap-x86:xadd64 ,(object-ea :r9 :slot constant-offset) :r8)))
          (t
           (emit `(sys.lap-x86:lock)
                 `(sys.lap-x86:xadd64 ,(object-ea :r9 :index '(:rdi 8)) :r8))))
    (setf *r8-value* (list (gensym)))))

;; Set the value in SLOT to NEW, and return the old value.
;; (defun xchg (object slot new)
;;   (prog1 (%object-ref-t object slot)
;;     (setf (%object-ref-t object slot) new)))
(defbuiltin sys.int::%xchg-object (object offset new) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r9 object t)
    (load-in-reg :r8 new t)
    (smash-r8)
    ;; Do the swap. xchg has an implicit lock prefix.
    (cond (constant-offset
           (emit `(sys.lap-x86:xchg64 ,(object-ea :r9 :slot constant-offset) :r8)))
          (t
           (emit `(sys.lap-x86:xchg64 ,(object-ea :r9 :index '(:rdi 8)) :r8))))
    (setf *r8-value* (list (gensym)))))

;; If the value in SLOT matches OLD, set it to NEW; otherwise do nothing.
;; Returns true as the primary value if the slot was modified, false otherwise.
;; Additionally returns the old value of SLOT as the second value.
;; (defun cas (object offset old new)
;;   (let ((slot-value (%object-ref-t object slot)))
;;     (values (cond ((eq slot-value old)
;;                    (setf (%object-ref-t object slot) new)
;;                    t)
;;                   (t nil))
;;             slot-value)))
(defbuiltin sys.int::%cas-object (object offset old new) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r9 object t)
    (load-in-reg :r11 new t)
    (load-in-reg :r8 old t)
    (smash-r8)
    (emit `(sys.lap-x86:mov64 :rax :r8))
    (emit-gc-info :extra-registers :rax)
    (cond (constant-offset
           (emit `(sys.lap-x86:lock)
                 `(sys.lap-x86:cmpxchg ,(object-ea :r9 :slot constant-offset) :r11)))
          (t
           (emit `(sys.lap-x86:lock)
                 `(sys.lap-x86:cmpxchg ,(object-ea :r9 :index '(:rdi 8)) :r11))))
    (cond ((member *for-value* '(:multiple :tail))
           ;; Return success and the old value.
           (emit `(sys.lap-x86:mov64 :r9 :rax))
           (emit-gc-info)
           (emit `(sys.lap-x86:mov64 :r8 nil)
                 `(sys.lap-x86:cmov64z :r8 (:constant t)))
           (load-constant :rcx 2)
           :multiple)
          (t ;; Just return the success state.
           (emit-gc-info)
           (predicate-result :z)))))

;; Similar to %CAS-OBJECT, but performs two CAS operations on adjacent slots.
;; Returns the two old slot values and a success boolean.
;; (defun dcas (object offset old-1 old-2 new-1 new-2)
;;   (let ((slot-value-1 (%object-ref-t object slot))
;;         (slot-value-2 (%object-ref-t object (1+ slot))))
;;     (values (cond ((and (eq slot-value-1 old-1)
;;                         (eq slot-value-2 old-2))
;;                    (setf (%object-ref-t object slot) new-1
;;                          (%object-ref-t object (1+ slot)) new-2)
;;                    t)
;;                   (t nil))
;;             slot-value-1
;;             slot-value-2)))
(defbuiltin sys.int::%dcas-object (object offset old-1 old-2 new-1 new-2) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 32 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (unless constant-offset
      (load-in-reg :rdi offset t)
      ;; Convert to unboxed integer.
      (emit `(sys.lap-x86:sar64 :rdi ,sys.int::+n-fixnum-bits+)))
    (load-in-reg :r8 object t)
    ;; Carefully load registers to avoid exposing the GC to a raw value.
    (load-in-reg :rbx new-1 t) ; rbx loaded (rbx is a value register).
    (load-in-reg :rax old-1 t) ; rbx, rax loaded.
    (emit-gc-info :extra-registers :rax)
    (load-in-reg :rcx new-2 t) ; rbx, rax, rcx loaded.
    (emit-gc-info :extra-registers :rax-rcx)
    (load-in-reg :rdx old-2 t) ; all registers loaded.
    (emit-gc-info :extra-registers :rax-rcx-rdx)
    (smash-r8)
    (cond (constant-offset
           (emit `(sys.lap-x86:lock)
                 `(sys.lap-x86:cmpxchg16b ,(object-ea :r8 :slot constant-offset))))
          (t
           (emit `(sys.lap-x86:lock)
                 `(sys.lap-x86:cmpxchg16b ,(object-ea :r8 :index '(:rdi 8))))))
    (cond ((member *for-value* '(:multiple :tail))
           ;; Return status and the old values.
           (emit `(sys.lap-x86:mov64 :r9 :rax))
           (emit `(sys.lap-x86:mov64 :r10 :rdx))
           (emit-gc-info)
           (emit `(sys.lap-x86:mov64 :r8 nil)
                 `(sys.lap-x86:cmov64z :r8 (:constant t)))
           (load-constant :rcx 3)
           :multiple)
          (t ;; Just return the status.
           (emit-gc-info)
           (predicate-result :z)))))
