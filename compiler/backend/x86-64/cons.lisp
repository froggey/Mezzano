;;;; Cons-related primitives

(in-package :mezzano.compiler.backend.x86-64)

(define-builtin mezzano.runtime::%car ((cons) result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list result `(:car ,cons))
                       :inputs (list cons)
                       :outputs (list result))))

(define-builtin mezzano.runtime::%cdr ((cons) result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list result `(:cdr ,cons))
                       :inputs (list cons)
                       :outputs (list result))))

(define-builtin (setf mezzano.runtime::%car) ((value cons) result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list `(:car ,cons) value)
                       :inputs (list cons value)
                       :outputs '()))
  (emit (make-instance 'ir:move-instruction
                       :destination result
                       :source value)))

(define-builtin (setf mezzano.runtime::%cdr) ((value cons) result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list `(:cdr ,cons) value)
                       :inputs (list cons value)
                       :outputs '()))
  (emit (make-instance 'ir:move-instruction
                       :destination result
                       :source value)))

(define-builtin sys.int::%cas-cons-car ((cons old new) (:z result))
  (emit (make-instance 'x86-cmpxchg-instruction
                       :object cons
                       :displacement (- sys.int::+tag-cons+)
                       :index 0
                       :old old
                       :new new
                       :result result
                       :prefix '(lap:lock))))

(define-builtin sys.int::%cas-cons-cdr ((cons old new) (:z result))
  (emit (make-instance 'x86-cmpxchg-instruction
                       :object cons
                       :displacement (+ (- sys.int::+tag-cons+) 8)
                       :index 0
                       :old old
                       :new new
                       :result result
                       :prefix '(lap:lock))))

(define-builtin sys.int::%xchg-car ((cons new) result)
  (emit (make-instance 'x86-atomic-instruction
                       :opcode 'lap:xchg64
                       :object cons
                       :displacement (- sys.int::+tag-cons+)
                       :index 0
                       :rhs new
                       :result result)))

(define-builtin sys.int::%xchg-cdr ((cons new) result)
  (emit (make-instance 'x86-atomic-instruction
                       :opcode 'lap:xchg64
                       :object cons
                       :displacement (+ (- sys.int::+tag-cons+) 8)
                       :index 0
                       :rhs new
                       :result result)))

(define-builtin sys.int::%dcas-cons ((cons old-1 old-2 new-1 new-2) (:z result-1 result-2))
  (emit (make-instance 'x86-cmpxchg16b-instruction
                       :object cons
                       :displacement (- sys.int::+tag-cons+)
                       :index 0
                       :old-1 old-1
                       :old-2 old-2
                       :new-1 new-1
                       :new-2 new-2
                       :result-1 result-1
                       :result-2 result-2
                       :prefix '(lap:lock))))
