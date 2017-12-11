;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.arm64)

(define-builtin sys.int::read-frame-pointer (() result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:add
                       :operands (list result :xzr :x29 :lsl sys.int::+n-fixnum-bits+)
                       :inputs (list)
                       :outputs (list result))))

(define-builtin sys.int::%unbound-value-p ((object) :eq)
  (let ((value (make-instance 'ir:virtual-register)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list value `(:literal :unbound-value))
                         :inputs (list)
                         :outputs (list value)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr object value)
                         :inputs (list object value)
                         :outputs (list)))))

(define-builtin sys.int::%undefined-function-p ((object) :eq)
  (let ((value (make-instance 'ir:virtual-register)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list value `(:literal :undefined-function))
                         :inputs (list)
                         :outputs (list value)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr object value)
                         :inputs (list object value)
                         :outputs (list)))))

(define-builtin eq ((x y) :eq)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:subs
                       :operands (list :xzr x y)
                       :inputs (list x y)
                       :outputs (list))))

(define-builtin mezzano.runtime::fast-symbol-value-cell (((:constant symbol symbol)) result)
  (cond ((eql (sys.int::symbol-mode symbol) :global)
         ;; This is a known global symbol, return the global value cell.
         (let ((temp (make-instance 'ir:virtual-register)))
           (emit (make-instance 'ir:constant-instruction
                                :destination temp
                                :value symbol))
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:ldr
                                :operands (list result `(:object ,temp ,sys.int::+symbol-value+))
                                :inputs (list temp)
                                :outputs (list result)))))
        (t
         ;; Punt.
         (give-up))))
