;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.x86-64)

(define-builtin sys.int::read-frame-pointer (() result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:lea64
                       :operands (list result `((:rbp ,(ash 1 sys.int::+n-fixnum-bits+))))
                       :inputs (list)
                       :outputs (list result))))

(define-builtin mezzano.runtime::fast-symbol-value-cell ((symbol) result)
  (cond ((and (constant-value-p symbol 'symbol)
              (eql (sys.int::symbol-mode (fetch-constant-value symbol)) :global))
         ;; This is a known global symbol, return the global value cell.
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:mov64
                              :operands (list result `(:object ,symbol ,sys.int::+symbol-value+))
                              :inputs (list symbol)
                              :outputs (list result))))
        (t
         (let ((is-global (make-instance 'ir:label :name :symbol-cache-global))
               (not-global (make-instance 'ir:label :name :symbol-cache-not-global))
               (cache-hit (make-instance 'ir:label :name :symbol-cache-hit))
               (cache-miss (make-instance 'ir:label :name :symbol-cache-miss))
               (resume (make-instance 'ir:label :name :symbol-cache-resume :phis (list result)))
               (global-cell (make-instance 'ir:virtual-register))
               (cache-temp (make-instance 'ir:virtual-register))
               (miss-result (make-instance 'ir:virtual-register)))
           (when (not (constant-value-p symbol 'symbol))
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
             (emit (make-instance 'x86-instruction
                                  :opcode 'lap:mov64
                                  :operands (list global-cell `(:object ,symbol ,sys.int::+symbol-value+))
                                  :inputs (list symbol)
                                  :outputs (list global-cell)))
             (emit (make-instance 'ir:jump-instruction
                                  :target resume
                                  :values (list global-cell)))
             (emit not-global))

           ;; Compute symbol hash. Symbols are wired, so use the address.
           ;; Ignore the low 4 bits.
           (emit (make-instance 'ir:move-instruction
                                :source symbol
                                :destination :rax))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:shr32
                                :operands (list :eax 4)
                                :inputs (list :rax)
                                :outputs (list :rax)))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:and32
                                :operands (list :eax (1- 128))
                                :inputs (list :rax)
                                :outputs (list :rax)))
           ;; Load cache entry.
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:mov64
                                :operands (list cache-temp `(:object nil 128 :rax 8))
                                :inputs (list :rax)
                                :outputs (list cache-temp)
                                :prefix '(lap:gs)))
           ;; Do symbols match?
           ;; Be careful here. The entry may be 0.
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:test64
                                :operands (list cache-temp cache-temp)
                                :inputs (list cache-temp)
                                :outputs (list)))
           (let ((tmp (make-instance 'ir:label)))
             (emit (make-instance 'x86-branch-instruction
                                  :opcode 'lap:jz
                                  :true-target cache-miss
                                  :false-target tmp))
             (emit tmp))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:cmp64
                                :operands (list symbol `(:object ,cache-temp ,sys.int::+symbol-value-cell-symbol+))
                                :inputs (list symbol cache-temp)
                                :outputs (list)))
           (emit (make-instance 'x86-branch-instruction
                                :opcode 'lap:jne
                                :true-target cache-miss
                                :false-target cache-hit))
           ;; Cache hit. Log.
           (emit cache-hit)
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:add64
                                :operands (list `(:object nil 22) (ash 1 sys.int::+n-fixnum-bits+))
                                :inputs (list)
                                :outputs (list)
                                :prefix '(lap:gs)))
           (emit (make-instance 'ir:jump-instruction
                                :target resume
                                :values (list cache-temp)))
           (emit cache-miss)
           ;; Call the slow function.
           (emit (make-instance 'ir:call-instruction
                                :function 'mezzano.runtime::symbol-value-cell
                                :result miss-result
                                :arguments (list symbol)))
           ;; Log a cache miss.
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:add64
                                :operands (list `(:object nil 23) (ash 1 sys.int::+n-fixnum-bits+))
                                :inputs (list)
                                :outputs (list)
                                :prefix '(lap:gs)))
           ;; Recompute the hash.
           (emit (make-instance 'ir:move-instruction
                                :source symbol
                                :destination :rax))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:shr32
                                :operands (list :eax 4)
                                :inputs (list :rax)
                                :outputs (list :rax)))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:and32
                                :operands (list :eax (1- 128))
                                :inputs (list :rax)
                                :outputs (list :rax)))
           ;; Write the entry into the cache.
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:mov64
                                :operands (list `(:object nil 128 :rax 8) miss-result)
                                :inputs (list :rax miss-result)
                                :outputs (list)
                                :prefix '(lap:gs)))
           (emit (make-instance 'ir:jump-instruction
                                :target resume
                                :values (list miss-result)))
           ;; Done.
           (emit resume)))))
