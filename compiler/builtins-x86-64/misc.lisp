;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.codegen.x86-64)

;;; EQ.

(defbuiltin eq (x y) ()
  ;; Ensure constants are on the right-hand side.
  (when (quoted-constant-p x)
    (rotatef x y))
  (cond ((quoted-constant-p y)
         (let ((constant (second y)))
           (load-in-reg :r8 x t)
           ;; Should characters and single-floats be loaded into a register
           ;; for comparison or should they be compared through the constant
           ;; pool? Currently they go through the constant pool...
           (cond
             ((small-fixnum-p constant)
              (emit `(sys.lap-x86:cmp64 :r8 ,(fixnum-to-raw constant))))
             (t (emit `(sys.lap-x86:cmp64 :r8 (:constant ,constant)))))
           (predicate-result :e)))
        (t (load-in-reg :r9 y t)
           (load-in-reg :r8 x t)
           (emit `(sys.lap-x86:cmp64 :r8 :r9))
           (predicate-result :e))))

;;; Constructing and deconstructing Lisp values.

(defbuiltin sys.int::%%assemble-value (address tag) ()
  (load-in-reg :rax tag t)
  (load-in-reg :r8 address t)
  (smash-r8)
  (emit `(sys.lap-x86:shr32 :eax ,sys.int::+n-fixnum-bits+)
        `(sys.lap-x86:sar64 :r8 ,sys.int::+n-fixnum-bits+)
        `(sys.lap-x86:or64 :r8 :rax))
  (setf *r8-value* (list (gensym))))

(defbuiltin sys.int::%pointer-field (value) ()
  (load-in-reg :r8 value t)
  (smash-r8)
  (emit `(sys.lap-x86:and64 :r8 -16)
        `(sys.lap-x86:sar64 :r8 ,(- 4 sys.int::+n-fixnum-bits+)))
  (setf *r8-value* (list (gensym))))

(defbuiltin sys.int::%tag-field (value) ()
  (load-in-reg :r8 value t)
  (smash-r8)
  (emit `(sys.lap-x86:shl64 :r8 ,sys.int::+n-fixnum-bits+)
        `(sys.lap-x86:and64 :r8 ,(ash (1- (ash 1 4)) sys.int::+n-fixnum-bits+)))
  (setf *r8-value* (list (gensym))))

(defbuiltin sys.int::lisp-object-address (value) ()
  (load-in-reg :r8 value t)
  (smash-r8)
  ;; Convert to fixnum.
  (emit `(sys.lap-x86:shl64 :r8 ,sys.int::+n-fixnum-bits+))
  (setf *r8-value* (list (gensym))))

;; (eql (%tag-field value) tag)
(defbuiltin sys.int::%value-has-tag-p (value tag) ()
  (cond ((constant-type-p tag `(unsigned-byte 4))
         ;; Fast case where the tag is a constant.
         (load-in-reg :r8 value t)
         ;; Test tag
         (emit `(sys.lap-x86:lea64 :rax (:r8 ,(- (second tag))))
               `(sys.lap-x86:test8 :al #b1111))
         (predicate-result :z))
        (t ;; Slow path.
         (load-in-reg :r8 value t)
         (load-in-reg :r10 tag t)
         ;; Convert value to fixnum.
         (emit `(sys.lap-x86:shl8 :r8l 1))
         ;; Mask away all but tag bits (as fixnum).
         (emit `(sys.lap-x86:and8 :r8l #b11110))
         ;; Compare against tag (which must be a fixnum).
         (emit `(sys.lap-x86:cmp8 :r8l :r10l))
         (predicate-result :e))))

;;; x86 IO port accessors.

(defun emit-port-access (instruction port port-reg)
  (cond ((and (quoted-constant-p port)
              (typep (second port) '(unsigned-byte 8)))
         ;; Small port number, fits directly in the instruction.
         (setf *load-list* (delete port *load-list*))
         (emit (list instruction (second port))))
        ((and (quoted-constant-p port)
              (typep (second port) '(unsigned-byte 8)))
         ;; Large port number, needs to be loaded into dx.
         (setf *load-list* (delete port *load-list*))
         (emit `(sys.lap-x86:mov16 :dx ,(second port))
               (list instruction :dx)))
        (t ;; Unknown port.
         (let ((type-error-label (gensym "port-type-error")))
           (emit-trailer (type-error-label)
             (raise-type-error port-reg '(unsigned-byte 16)))
           (load-in-reg port-reg port t)
           (emit `(sys.lap-x86:test64 ,port-reg ,sys.int::+fixnum-tag-mask+)
                 `(sys.lap-x86:cmp64 ,port-reg ,(fixnum-to-raw #x10000))
                 `(sys.lap-x86:jae ,type-error-label)
                 `(sys.lap-x86:mov64 :rdx ,port-reg)
                 ;; Convert to a raw integer.
                 `(sys.lap-x86:sar32 :edx ,sys.int::+n-fixnum-bits+)
                 (list instruction :dx))))))

(defbuiltin sys.int::io-port/8 (port) ()
  (smash-r8)
  (emit `(sys.lap-x86:xor32 :eax :eax))
  (emit-port-access 'sys.lap-x86:in8 port :r8)
  (emit `(sys.lap-x86:shl32 :eax ,sys.int::+n-fixnum-bits+)
        `(sys.lap-x86:mov32 :r8d :eax))
  (setf *r8-value* (list (gensym))))

(defbuiltin (setf sys.int::io-port/8) (value port) ()
  (load-in-r8 value t)
  (let ((value-type-error-label (gensym)))
    (emit-trailer (value-type-error-label)
      (raise-type-error :r8 '(unsigned-byte 8)))
    (emit `(sys.lap-x86:test64 :r8 ,sys.int::+fixnum-tag-mask+)
          `(sys.lap-x86:jnz ,value-type-error-label)
          `(sys.lap-x86:cmp64 :r8 ,(fixnum-to-raw #x100))
          `(sys.lap-x86:jae ,value-type-error-label)
          `(sys.lap-x86:mov64 :rax :r8)
          `(sys.lap-x86:sar32 :eax ,sys.int::+n-fixnum-bits+))
    (emit-port-access 'sys.lap-x86:out8 port :r9)
    value))

(defbuiltin sys.int::io-port/16 (port) ()
  (smash-r8)
  (emit `(sys.lap-x86:xor32 :eax :eax))
  (emit-port-access 'sys.lap-x86:in16 port :r8)
  (emit `(sys.lap-x86:shl32 :eax ,sys.int::+n-fixnum-bits+)
        `(sys.lap-x86:mov32 :r8d :eax))
  (setf *r8-value* (list (gensym))))

(defbuiltin (setf sys.int::io-port/16) (value port) ()
  (load-in-r8 value t)
  (let ((value-type-error-label (gensym)))
    (emit-trailer (value-type-error-label)
      (raise-type-error :r8 '(unsigned-byte 8)))
    (emit `(sys.lap-x86:test64 :r8 ,sys.int::+fixnum-tag-mask+)
          `(sys.lap-x86:jnz ,value-type-error-label)
          `(sys.lap-x86:cmp64 :r8 ,(fixnum-to-raw #x10000))
          `(sys.lap-x86:jae ,value-type-error-label)
          `(sys.lap-x86:mov64 :rax :r8)
          `(sys.lap-x86:sar32 :eax ,sys.int::+n-fixnum-bits+))
    (emit-port-access 'sys.lap-x86:out16 port :r9)
    value))

(defbuiltin sys.int::io-port/32 (port) ()
  (smash-r8)
  (emit `(sys.lap-x86:xor32 :eax :eax))
  (emit-port-access 'sys.lap-x86:in32 port :r8)
  (emit `(sys.lap-x86:shl64 :rax ,sys.int::+n-fixnum-bits+)
        `(sys.lap-x86:mov64 :r8 :rax))
  (setf *r8-value* (list (gensym))))

(defbuiltin (setf sys.int::io-port/32) (value port) ()
  (load-in-r8 value t)
  (let ((value-type-error-label (gensym)))
    (emit-trailer (value-type-error-label)
      (raise-type-error :r8 '(unsigned-byte 8)))
    (emit `(sys.lap-x86:test64 :r8 ,sys.int::+fixnum-tag-mask+)
          `(sys.lap-x86:jnz ,value-type-error-label)
          `(sys.lap-x86:mov64 :rax :r8)
          `(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+)
          `(sys.lap-x86:mov64 :rdx :rax)
          `(sys.lap-x86:sar64 :rdx 32)
          `(sys.lap-x86:jnz ,value-type-error-label))
    (emit-port-access 'sys.lap-x86:out32 port :r9)
    value))

;;; Grovelling in the machine.

(defbuiltin system.internals::read-frame-pointer () (nil)
  (smash-r8)
  (emit `(sys.lap-x86:lea64 :r8 ((:rbp ,(ash 1 sys.int::+n-fixnum-bits+)))))
  (setf *r8-value* (list (gensym))))

;; STI inhibits interrupts until the following instruction completes.
;; STI immediately followed by HLT will stop the CPU until an IRQ occurs.
;; Provide a builtin specifically for this so the compiler doesn't put
;; any instruction between the two.
(defbuiltin sys.int::%stihlt () ()
  (emit `(sys.lap-x86:sti)
        `(sys.lap-x86:hlt))
  ''nil)

(defbuiltin sys.int::%sti () ()
  (emit `(sys.lap-x86:sti))
  ''nil)

(defbuiltin sys.int::%cli () ()
  (emit `(sys.lap-x86:cli))
  ''nil)

(defbuiltin sys.int::%hlt () ()
  (emit `(sys.lap-x86:hlt))
  ''nil)

(defbuiltin sys.int::%interrupt-state () ()
  (emit `(sys.lap-x86:pushf)
        `(sys.lap-x86:pop :rax)
        `(sys.lap-x86:test32 :eax #x200))
  (predicate-result :nz))

;; These functions save & restore the entire state of the flags register.
(defbuiltin sys.int::%save-irq-state () ()
  (smash-r8)
  (emit `(sys.lap-x86:pushf)
        `(sys.lap-x86:shl64 (:rsp) ,sys.int::+n-fixnum-bits+)
        `(sys.lap-x86:pop :r8))
  (setf *r8-value* (list (gensym))))

(defbuiltin sys.int::%restore-irq-state (saved-state) ()
  (load-in-r8 saved-state t)
  (emit `(sys.lap-x86:push :r8)
        `(sys.lap-x86:shr64 (:rsp) ,sys.int::+n-fixnum-bits+)
        `(sys.lap-x86:popf))
  saved-state)

(macrolet ((def (name reg)
             `(progn
                (defbuiltin ,name () ()
                  (smash-r8)
                  (emit `(sys.lap-x86:movcr :rax ,',reg)
                        `(sys.lap-x86:shl64 :rax ,sys.int::+n-fixnum-bits+)
                        `(sys.lap-x86:mov64 :r8 :rax))
                  (setf *r8-value* (list (gensym))))
                (defbuiltin (setf ,name) (value) ()
                  (load-in-r8 value t)
                  (fixnum-check :r8)
                  (emit `(sys.lap-x86:mov64 :rax :r8)
                        `(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+)
                        `(sys.lap-x86:movcr ,',reg :rax))
                  value))))
  (def sys.int::%cr0 :cr0)
  (def sys.int::%cr2 :cr2)
  (def sys.int::%cr3 :cr3)
  (def sys.int::%cr4 :cr4))

(macrolet ((def (name reg)
             `(progn
                (defbuiltin ,name () ()
                  (smash-r8)
                  (emit `(sys.lap-x86:movdr :rax ,',reg)
                        `(sys.lap-x86:shl64 :rax ,sys.int::+n-fixnum-bits+)
                        `(sys.lap-x86:mov64 :r8 :rax))
                  (setf *r8-value* (list (gensym))))
                (defbuiltin (setf ,name) (value) ()
                  (load-in-r8 value t)
                  (fixnum-check :r8)
                  (emit `(sys.lap-x86:mov64 :rax :r8)
                        `(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+)
                        `(sys.lap-x86:movdr ,',reg :rax))
                  value))))
  (def sys.int::%dr0 :dr0)
  (def sys.int::%dr1 :dr1)
  (def sys.int::%dr2 :dr2)
  (def sys.int::%dr3 :dr3)
  (def sys.int::%dr6 :dr6)
  (def sys.int::%dr7 :dr7))

(defbuiltin sys.int::msr (register) ()
  (smash-r8)
  (unpack-ub32-fixnum-into-register register :rcx :r8)
  (emit `(sys.lap-x86:rdmsr)
        ;; Pack result into one register
        `(sys.lap-x86:shl64 :rdx 32)
        `(sys.lap-x86:or64 :rax :rdx))
  (box-unsigned-byte-64-rax)
  (setf *r8-value* (list (gensym))))

(defbuiltin (setf sys.int::msr) (value register) ()
  (smash-r8)
  (unpack-ub32-fixnum-into-register register :rcx :r8)
  (cond ((and (quoted-constant-p value)
              (typep (second value) '(or (unsigned-byte 64)
                                         (signed-byte 64))))
         (emit `(sys.lap-x86:mov32 :eax ,(ldb (byte 32 0) (second value)))
               `(sys.lap-x86:mov32 :edx ,(ldb (byte 32 32) (second value)))))
        (t (let ((type-error-label (gensym))
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
               (raise-type-error :r8 '(or (unsigned-byte 64)
                                          (signed-byte 64))))
             (load-in-r8 value t)
             (emit `(sys.lap-x86:mov64 :rdx :r8)
                   `(sys.lap-x86:test64 :rdx ,sys.int::+fixnum-tag-mask+)
                   `(sys.lap-x86:jnz ,bignum-path)
                   `(sys.lap-x86:sar64 :rdx ,sys.int::+n-fixnum-bits+)
                   value-extracted
                   `(sys.lap-x86:mov64 :rax :rdx)
                   `(sys.lap-x86:shr64 :rdx 32)))))
  ;; ECX, EAX & EDX are set correctly.
  ;; If VALUE is non-constant then it'll be loaded in R8.
  (emit `(sys.lap-x86:wrmsr))
  value)

(defbuiltin sys.int::cpu-relax () ()
  (emit `(sys.lap-x86:pause))
  ;; Return no values.
  (cond ((member *for-value* '(:multiple :tail))
         (smash-r8)
         (emit `(sys.lap-x86:mov64 :r8 nil))
         (load-constant :rcx 0)
         :multiple)
        (t ''nil)))

(defbuiltin sys.int::tsc () ()
  (smash-r8)
  (emit `(sys.lap-x86:rdtsc)
        ;; Pack result into one register
        `(sys.lap-x86:shl64 :rdx 32)
        `(sys.lap-x86:or64 :rax :rdx))
  (box-unsigned-byte-64-rax)
  (setf *r8-value* (list (gensym))))

(defbuiltin sys.int::%invlpg (address) ()
  (load-in-r8 address t)
  (fixnum-check :r8)
  (emit `(sys.lap-x86:mov64 :rax :r8)
        `(sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
        `(sys.lap-x86:invlpg (:rax)))
  address)

;;; Cold-generator-supplied support objects.

(defmacro define-support-object (name symbol)
  (let ((predicate-name (intern (format nil "~A-P" name) (symbol-package name))))
    `(progn
       (defbuiltin ,name () ()
         (smash-r8)
         (emit `(sys.lap-x86:mov32 :r8d ,',symbol))
         (setf *r8-value* (list (gensym))))
       (defbuiltin ,predicate-name (value) ()
         (load-in-r8 value t)
         (emit `(sys.lap-x86:cmp64 :r8 ,',symbol))
         (predicate-result :e)))))

(define-support-object sys.int::%unbound-value :unbound-value)
(define-support-object sys.int::%undefined-function :undefined-function)
(define-support-object sys.int::%closure-trampoline :closure-trampoline)
(define-support-object sys.int::%funcallable-instance-trampoline :funcallable-instance-trampoline)

;;; Fixed-size dynamic-extent object creation.

(defbuiltin sys.c::make-dx-closure (code env) (nil)
  (smash-r8)
  (let ((slots (allocate-control-stack-slots 4 t)))
    (load-in-reg :r9 code t)
    (load-in-reg :r10 env t)
    (emit `(sys.lap-x86:lea64 :rax (:stack ,(+ slots 4 -1)))
          ;; Closure tag and size.
          `(sys.lap-x86:mov32 (:rax) ,(logior (ash 3 sys.int::+object-data-shift+)
                                              (ash sys.int::+object-tag-closure+
                                                   sys.int::+object-type-shift+)))
          ;; Constant pool size and slot count.
          `(sys.lap-x86:mov32 (:rax 4) #x00000002)
          ;; Entry point is CODE's entry point.
          `(sys.lap-x86:mov64 :rcx ,(object-ea :r9 :slot 0))
          `(sys.lap-x86:mov64 (:rax 8) :rcx)
          ;; Clear constant pool.
          `(sys.lap-x86:mov64 (:rax 16) nil)
          `(sys.lap-x86:mov64 (:rax 24) nil))
    (emit `(sys.lap-x86:lea64 :r8 (:rax ,sys.int::+tag-object+)))
    ;; Initiaize constant pool.
    (emit `(sys.lap-x86:mov64 ,(object-ea :r8 :slot 1) :r9)
          `(sys.lap-x86:mov64 ,(object-ea :r8 :slot 2) :r10)))
  (setf *r8-value* (list (gensym))))

(defbuiltin mezzano.runtime::fast-symbol-value-cell (symbol) ()
  (smash-r8)
  (when (and (constant-type-p symbol 'symbol)
             (eql (sys.int::symbol-mode (second symbol)) :global))
    ;; This is a known global symbol, pull the global value cell.
    (load-in-r8 symbol t)
    (emit `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :slot sys.int::+symbol-value+)))
    (return-from mezzano.runtime::fast-symbol-value-cell
      (setf *r8-value* (list (gensym)))))
  (let ((cache-miss (gensym "SYMBOL-CACHE-MISS"))
        (resume (gensym "RESUME")))
    (emit-trailer (cache-miss)
      ;; Call the slow function.
      (emit `(sys.lap-x86:mov64 :r8 :r9))
      (call-support-function 'mezzano.runtime::symbol-value-cell 1)
      ;; Log a cache miss.
      (emit `(sys.lap-x86:gs)
            `(sys.lap-x86:add64 ,(object-ea nil :slot 23)
                                ,(ash 1 sys.int::+n-fixnum-bits+)))
      ;; Recompute the hash.
      (emit `(sys.lap-x86:mov64 :rax ,(object-ea :r8
                                                :slot sys.int::+symbol-value-cell-symbol+))
            `(sys.lap-x86:shr32 :eax 4)
            `(sys.lap-x86:and32 :eax ,(1- 128)))
      ;; Write the entry into the cache.
      (emit `(sys.lap-x86:gs)
            `(sys.lap-x86:mov64 ,(object-ea nil
                                            :slot 128
                                            :index '(:rax 8))
                                :r8))
      ;; Done.
      (emit `(sys.lap-x86:jmp ,resume)))
    (load-in-reg :r9 symbol t)
    (when (not (constant-type-p symbol 'symbol))
      ;; Symbol type check.
      (emit `(sys.lap-x86:lea64 :rax (:r9 ,(- sys.int::+tag-object+)))
            `(sys.lap-x86:test8 :al #b1111)
            `(sys.lap-x86:jne ,cache-miss)
            `(sys.lap-x86:mov8 :al ,(object-ea :r9 :slot -1))
            `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:cmp8 :al ,(ash sys.int::+object-tag-symbol+
                                         sys.int::+object-type-shift+))
            `(sys.lap-x86:jne ,cache-miss))
      ;; For global symbols, don't even look at the cache.
      ;; Cache entries exist on the stack, which may not be paged in.
      (emit `(sys.lap-x86:mov8 :al (:r9 ,(+ (- sys.int::+tag-object+) 1)))
            `(sys.lap-x86:and8 :al #b111)
            `(sys.lap-x86:cmp8 :al ,sys.int::+symbol-mode-global+)
            `(sys.lap-x86:cmov64e :r8 ,(object-ea :r9 :slot sys.int::+symbol-value+))
            `(sys.lap-x86:je ,resume)))
    ;; Compute symbol hash. Symbols are wired, so use the address.
    ;; Ignore the low 4 bits.
    (emit `(sys.lap-x86:mov64 :rax :r9)
          `(sys.lap-x86:shr32 :eax 4)
          `(sys.lap-x86:and32 :eax ,(1- 128)))
    ;; Load cache entry.
    (emit `(sys.lap-x86:gs)
          `(sys.lap-x86:mov64 :r8 ,(object-ea nil
                                              :slot 128
                                              :index '(:rax 8))))
    ;; Do symbols match?
    ;; Be careful here. The entry may be 0.
    (emit `(sys.lap-x86:test64 :r8 :r8))
    (emit `(sys.lap-x86:jz ,cache-miss))
    (emit `(sys.lap-x86:cmp64 :r9 ,(object-ea :r8 :slot sys.int::+symbol-value-cell-symbol+))
          `(sys.lap-x86:jne ,cache-miss))
    ;; Cache hit. Log.
    (emit `(sys.lap-x86:gs)
          `(sys.lap-x86:add64 ,(object-ea nil :slot 22)
                              ,(ash 1 sys.int::+n-fixnum-bits+)))
    (emit resume))
  (setf *r8-value* (list (gensym))))
