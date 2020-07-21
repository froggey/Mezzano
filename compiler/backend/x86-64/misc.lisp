;;;; Miscellaneous builtin operations.

(in-package :mezzano.compiler.backend.x86-64)

;;; Touching the special stack directly.

(define-builtin sys.int::%%special-stack-pointer (() result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list result `(:object nil ,mezzano.supervisor::+thread-special-stack-pointer+))
                       :inputs (list)
                       :outputs (list result)
                       :prefix '(lap:gs))))

(define-builtin (setf sys.int::%%special-stack-pointer) ((value) result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list `(:object nil ,mezzano.supervisor::+thread-special-stack-pointer+) value)
                       :inputs (list value)
                       :outputs (list)
                       :prefix '(lap:gs)))
  (emit (make-instance 'ir:move-instruction
                       :destination result
                       :source value)))

;;; Examining non-local exit instances.

(macrolet ((def (name)
             `(define-builtin ,name ((info) result)
                (emit (make-instance 'x86-instruction
                                     :opcode 'lap:mov64
                                     :operands (list result (list info 8))
                                     :inputs (list info)
                                     :outputs (list result))))))
  (def sys.int::%%block-info-binding-stack-pointer)
  (def sys.int::%%tagbody-info-binding-stack-pointer))

;;; Symbol lookup

(define-builtin mezzano.runtime::fast-symbol-value-cell ((symbol) result)
  (cond ((and (constant-value-p symbol 'symbol)
              (eql (sys.int::symbol-mode (fetch-constant-value symbol)) :global))
         ;; This is a known global symbol, return the global value cell.
         (emit (make-instance 'ir:constant-instruction
                              :value (mezzano.runtime::symbol-global-value-cell
                                      (fetch-constant-value symbol))
                              :destination result)))
        (t
         (let ((is-global (make-instance 'ir:label :name :symbol-cache-global))
               (not-global (make-instance 'ir:label :name :symbol-cache-not-global))
               (cache-hit (make-instance 'ir:label :name :symbol-cache-hit))
               (cache-miss (make-instance 'ir:label :name :symbol-cache-miss))
               (resume (make-instance 'ir:label :name :symbol-cache-resume :phis (list result)))
               (global-cell (make-instance 'ir:virtual-register))
               (cache-temp (make-instance 'ir:virtual-register))
               (miss-result (make-instance 'ir:virtual-register)))
           ;; Fetch the symbol's global cell - the cache is keyed on this.
           (cond ((constant-value-p symbol 'symbol)
                  (emit (make-instance 'ir:constant-instruction
                                       :value (mezzano.runtime::symbol-global-value-cell
                                               (fetch-constant-value symbol))
                                       :destination global-cell)))
                 (t
                  (emit (make-instance 'ir:call-instruction
                                       :function 'mezzano.runtime::symbol-global-value-cell
                                       :result global-cell
                                       :arguments (list symbol)))
                  ;; For global symbols, don't even look at the cache.
                  ;; Cache entries exist on the stack, which may not be paged in.
                  (emit (make-instance 'x86-instruction
                                       :opcode 'lap:mov8
                                       :operands (list :al `(,symbol ,(+ (- sys.int::+tag-object+) 1)))
                                       :inputs (list symbol)
                                       :outputs (list :rax)))
                  (emit (make-instance 'x86-instruction
                                       :opcode 'lap:and8
                                       :operands (list :al #b111)
                                       :inputs (list :rax)
                                       :outputs (list :rax)))
                  (emit (make-instance 'x86-instruction
                                       :opcode 'lap:cmp8
                                       :operands (list :al sys.int::+symbol-mode-global+)
                                       :inputs (list :rax)
                                       :outputs '()))
                  (emit (make-instance 'x86-branch-instruction
                                       :opcode 'lap:jne
                                       :true-target not-global
                                       :false-target is-global))
                  (emit is-global)
                  (emit (make-instance 'ir:jump-instruction
                                       :target resume
                                       :values (list global-cell)))
                  (emit not-global)))
           ;; Compute symbol hash. Symbols are wired, so use the address.
           ;; Ignore the low 4 bits.
           (emit (make-instance 'ir:move-instruction
                                :source global-cell
                                :destination :rax))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:shr32
                                :operands (list :eax 4)
                                :inputs (list :rax)
                                :outputs (list :rax)))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:and32
                                :operands (list :eax (1- mezzano.supervisor::+thread-symbol-cache-size+))
                                :inputs (list :rax)
                                :outputs (list :rax)))
           ;; Load cache entry.
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:mov64
                                :operands (list cache-temp `(:object nil ,mezzano.supervisor::+thread-symbol-cache+ :rax 8))
                                :inputs (list :rax)
                                :outputs (list cache-temp)
                                :prefix '(lap:gs)))
           ;; Do symbols match?
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:cmp64
                                :operands (list global-cell `(:object ,cache-temp ,sys.int::+symbol-value-cell-symbol+))
                                :inputs (list global-cell cache-temp)
                                :outputs (list)))
           (emit (make-instance 'x86-branch-instruction
                                :opcode 'lap:jne
                                :true-target cache-miss
                                :false-target cache-hit))
           ;; Cache hit. Log.
           (emit cache-hit)
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:add64
                                :operands (list `(:object nil ,mezzano.supervisor::+thread-symbol-cache-hit-count+) (ash 1 sys.int::+n-fixnum-bits+))
                                :inputs (list)
                                :outputs (list)
                                :prefix '(lap:gs)))
           (emit (make-instance 'ir:jump-instruction
                                :target resume
                                :values (list cache-temp)))
           (emit cache-miss)
           ;; Call the slow function.
           (emit (make-instance 'ir:call-instruction
                                :function 'mezzano.runtime::%symbol-value-cell-by-cell
                                :result miss-result
                                :arguments (list global-cell)))
           ;; Log a cache miss.
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:add64
                                :operands (list `(:object nil ,mezzano.supervisor::+thread-symbol-cache-miss-count+) (ash 1 sys.int::+n-fixnum-bits+))
                                :inputs (list)
                                :outputs (list)
                                :prefix '(lap:gs)))
           ;; Recompute the hash.
           (emit (make-instance 'ir:move-instruction
                                :source global-cell
                                :destination :rax))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:shr32
                                :operands (list :eax 4)
                                :inputs (list :rax)
                                :outputs (list :rax)))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:and32
                                :operands (list :eax (1- mezzano.supervisor::+thread-symbol-cache-size+))
                                :inputs (list :rax)
                                :outputs (list :rax)))
           ;; Write the entry into the cache.
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:mov64
                                :operands (list `(:object nil ,mezzano.supervisor::+thread-symbol-cache+ :rax 8) miss-result)
                                :inputs (list :rax miss-result)
                                :outputs (list)
                                :prefix '(lap:gs)))
           (emit (make-instance 'ir:jump-instruction
                                :target resume
                                :values (list miss-result)))
           ;; Done.
           (emit resume)))))

(define-builtin mezzano.runtime::symbol-global-value-cell (((:constant symbol symbol))
                                                           result
                                                           :has-wrapper nil)
  (emit (make-instance 'ir:constant-instruction
                       :destination result
                       :value (mezzano.runtime::symbol-global-value-cell symbol))))

;;; CPU stuff

(define-builtin sys.int::read-frame-pointer (() result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:lea64
                       :operands (list result `((:rbp ,(ash 1 sys.int::+n-fixnum-bits+))))
                       :inputs (list)
                       :outputs (list result))))

(define-builtin sys.int::%msr ((register) result)
  (let ((register-unboxed (make-instance 'ir:virtual-register :kind :integer))
        (result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source register
                         :destination register-unboxed))
    (emit (make-instance 'ir:move-instruction
                         :source register-unboxed
                         :destination :rcx))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:rdmsr
                         :operands (list)
                         :inputs (list :rcx)
                         :outputs (list :rax :rdx)
                         :clobbers (list :rax :rdx)))
    ;; Pack result into one register
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:shl64
                         :operands (list :rdx 32)
                         :inputs (list :rdx)
                         :outputs (list :rdx)
                         :clobbers (list :rdx)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:or64
                         :operands (list :rax :rdx)
                         :inputs (list :rax :rdx)
                         :outputs (list :rax)
                         :clobbers (list :rax)))
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination result-unboxed))
    (emit (make-instance 'ir:box-unsigned-byte-64-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin (setf sys.int::%msr) ((value register) result)
  (let ((register-unboxed (make-instance 'ir:virtual-register :kind :integer))
        (value-unboxed (make-instance 'ir:virtual-register :kind :integer))
        (value-high-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source register
                         :destination register-unboxed))
    (emit (make-instance 'ir:unbox-unsigned-byte-64-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:shr64
                         :result value-high-unboxed
                         :lhs value-unboxed
                         :rhs 32))
    (emit (make-instance 'ir:move-instruction
                         :source register-unboxed
                         :destination :rcx))
    (emit (make-instance 'ir:move-instruction
                         :source value-unboxed
                         :destination :rax))
    (emit (make-instance 'ir:move-instruction
                         :source value-high-unboxed
                         :destination :rdx))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:wrmsr
                         :operands (list)
                         :inputs (list :rcx :rax :rdx)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(defmacro emit-port-access (instruction port)
  `(cond ((constant-value-p ,port '(unsigned-byte 8))
          (emit (make-instance 'x86-instruction
                               :opcode ',instruction
                               :operands (list (fetch-constant-value ,port))
                               :inputs (list :rax)
                               :outputs (list :rax)
                               :clobbers (list :rax))))
         (t
          (let ((port-unboxed (make-instance 'ir:virtual-register :kind :integer)))
            (emit (make-instance 'ir:unbox-fixnum-instruction
                                 :source ,port
                                 :destination port-unboxed))
            (emit (make-instance 'ir:move-instruction
                                 :source port-unboxed
                                 :destination :rdx))
            (emit (make-instance 'x86-instruction
                                 :opcode ',instruction
                                 :operands (list :dx)
                                 :inputs (list :rax :rdx)
                                 :outputs (list :rax)
                                 :clobbers (list :rax)))))))

(define-builtin sys.int::%io-port/8 ((port) result)
  (let ((result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:xor32
                         :operands (list :eax :eax)
                         :inputs (list)
                         :outputs (list :rax)
                         :clobbers (list :rax)))
    (emit-port-access lap:in8 port)
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination result-unboxed))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin (setf sys.int::%io-port/8) ((value port) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'ir:move-instruction
                         :source value-unboxed
                         :destination :rax))
    (emit-port-access lap:out8 port)
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%io-port/16 ((port) result)
  (let ((result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:xor32
                         :operands (list :eax :eax)
                         :inputs (list)
                         :outputs (list :rax)
                         :clobbers (list :rax)))
    (emit-port-access lap:in16 port)
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination result-unboxed))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin (setf sys.int::%io-port/16) ((value port) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'ir:move-instruction
                         :source value-unboxed
                         :destination :rax))
    (emit-port-access lap:out16 port)
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%io-port/32 ((port) result)
  (let ((result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:xor32
                         :operands (list :eax :eax)
                         :inputs (list)
                         :outputs (list :rax)
                         :clobbers (list :rax)))
    (emit-port-access lap:in32 port)
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination result-unboxed))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin (setf sys.int::%io-port/32) ((value port) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'ir:move-instruction
                         :source value-unboxed
                         :destination :rax))
    (emit-port-access lap:out32 port)
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin (setf sys.int::%io-port/32) ((value port) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'ir:move-instruction
                         :source value-unboxed
                         :destination :rax))
    (emit-port-access lap:out32 port)
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

;; STI inhibits interrupts until the following instruction completes.
;; STI immediately followed by HLT will stop the CPU until an IRQ occurs.
;; Provide a builtin specifically for this so the compiler doesn't put
;; any instruction between the two.
(define-builtin sys.int::%stihlt (() ())
  ;; TODO: Make sure the above guarantee still holds.
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:sti
                       :operands (list)
                       :inputs (list)
                       :outputs (list)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:hlt
                       :operands (list)
                       :inputs (list)
                       :outputs (list))))

(define-builtin sys.int::%sti (() ())
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:sti
                       :operands (list)
                       :inputs (list)
                       :outputs (list))))

(define-builtin sys.int::%cli (() ())
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cli
                       :operands (list)
                       :inputs (list)
                       :outputs (list))))

(define-builtin sys.int::%hlt (() ())
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:hlt
                       :operands (list)
                       :inputs (list)
                       :outputs (list))))

(define-builtin sys.int::%interrupt-state (() :nz)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:pushf
                       :operands (list)
                       :inputs (list)
                       :outputs (list)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:pop
                       :operands (list :rax)
                       :inputs (list)
                       :outputs (list :rax)
                       :clobbers (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:test32
                       :operands (list :eax #x200)
                       :inputs (list :rax)
                       :outputs (list))))

;; These functions save & restore the entire state of the flags register.
(define-builtin sys.int::%save-irq-state (() result)
  (let ((result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:pushf
                         :operands (list)
                         :inputs (list)
                         :outputs (list)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:pop
                         :operands (list result-unboxed)
                         :inputs (list)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin sys.int::%restore-irq-state ((saved-state) ())
  (let ((state-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source saved-state
                         :destination state-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:push
                         :operands (list state-unboxed)
                         :inputs (list state-unboxed)
                         :outputs (list)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:popf
                         :operands (list)
                         :inputs (list)
                         :outputs (list)))))

(defmacro define-control-register (name reg inst)
  `(progn
     (define-builtin ,name (() result)
       (let ((result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
         (emit (make-instance 'x86-instruction
                              :opcode ',inst
                              :operands (list result-unboxed ',reg)
                              :inputs (list)
                              :outputs (list result-unboxed)))
         (emit (make-instance 'ir:box-fixnum-instruction
                              :source result-unboxed
                              :destination result))))
     (define-builtin (setf ,name) ((value) result)
       (let ((value-unboxed (make-instance 'ir:virtual-register :kind :integer)))
         (emit (make-instance 'ir:unbox-fixnum-instruction
                              :source value
                              :destination value-unboxed))
         (emit (make-instance 'x86-instruction
                              :opcode ',inst
                              :operands (list ',reg value-unboxed)
                              :inputs (list value-unboxed)
                              :outputs (list)))
         (emit (make-instance 'ir:move-instruction
                              :source value
                              :destination result))))))

(define-control-register sys.int::%cr0 :cr0 lap:movcr)
(define-control-register sys.int::%cr2 :cr2 lap:movcr)
(define-control-register sys.int::%cr3 :cr3 lap:movcr)
(define-control-register sys.int::%cr4 :cr4 lap:movcr)

(define-control-register sys.int::%dr0 :dr0 lap:movdr)
(define-control-register sys.int::%dr1 :dr1 lap:movdr)
(define-control-register sys.int::%dr2 :dr2 lap:movdr)
(define-control-register sys.int::%dr3 :dr3 lap:movdr)
(define-control-register sys.int::%dr6 :dr6 lap:movdr)
(define-control-register sys.int::%dr7 :dr7 lap:movdr)

(define-builtin sys.int::cpu-relax (() ())
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:pause
                       :operands (list)
                       :inputs (list)
                       :outputs (list))))

(define-builtin sys.int::tsc (() result)
  (let ((result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:rdtsc
                         :operands (list)
                         :inputs (list)
                         :outputs (list :rax :rdx)
                         :clobbers (list :rax :rdx)))
    ;; Pack result into one register
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:shl64
                         :operands (list :rdx 32)
                         :inputs (list :rdx)
                         :outputs (list :rdx)
                         :clobbers (list :rdx)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:or64
                         :operands (list :rax :rdx)
                         :inputs (list :rax :rdx)
                         :outputs (list :rax)
                         :clobbers (list :rax)))
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination result-unboxed))
    (emit (make-instance 'ir:box-unsigned-byte-64-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin sys.int::%invlpg ((address) result)
  (let ((address-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source address
                         :destination address-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:invlpg
                         :operands (list (list address-unboxed))
                         :inputs (list address-unboxed)
                         :outputs (list)
                         :clobbers (list)))
    (emit (make-instance 'ir:move-instruction
                         :source address
                         :destination result))))
