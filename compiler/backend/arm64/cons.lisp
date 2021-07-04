;;;; Cons related ARM64 builtins.

(in-package :mezzano.compiler.backend.arm64)

(define-builtin mezzano.runtime::%car ((cons) result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:ldr
                       :operands (list result `(:car ,cons))
                       :inputs (list cons)
                       :outputs (list result))))

(define-builtin mezzano.runtime::%cdr ((cons) result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:ldr
                       :operands (list result `(:cdr ,cons))
                       :inputs (list cons)
                       :outputs (list result))))

(define-builtin (setf mezzano.runtime::%car) ((value cons) result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:str
                       :operands (list value `(:car ,cons))
                       :inputs (list cons value)
                       :outputs (list)))
  (emit (make-instance 'ir:move-instruction
                       :source value
                       :destination result)))

(define-builtin (setf mezzano.runtime::%cdr) ((value cons) result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:str
                       :operands (list value `(:cdr ,cons))
                       :inputs (list cons value)
                       :outputs (list)))
  (emit (make-instance 'ir:move-instruction
                       :source value
                       :destination result)))
