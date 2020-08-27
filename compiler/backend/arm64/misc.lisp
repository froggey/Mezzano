;;;; Miscellaneous ARM64 builtin operations

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

(define-builtin mezzano.runtime::fast-symbol-value-cell (((:constant symbol symbol)) result :has-wrapper nil)
  (cond ((eql (sys.int::symbol-mode symbol) :global)
         ;; This is a known global symbol, return the global value cell.
         (emit (make-instance 'ir:constant-instruction
                              :destination result
                              :value (mezzano.runtime::symbol-global-value-cell symbol))))
        (t
         ;; Punt.
         (give-up))))

(define-builtin sys.int::%instance-layout ((object) result)
  (let ((temp1 (make-instance 'ir:virtual-register)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list temp1 `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list temp1)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:add
                         :operands (list result :xzr temp1 :lsr sys.int::+object-data-shift+)
                         :inputs (list temp1)
                         :outputs (list result)))
    ;; The object must be kept live over the shift, as the header will
    ;; initially be read as a fixnum. If the object is the only thing keeping
    ;; the structure definition live there is a possibility that it and the
    ;; structure definition could be end up being GC'd between the load & shift.
    ;; Then the shift would resurrect a dead object, leading to trouble.
    (emit (make-instance 'ir:spice-instruction :value object))))

(define-builtin sys.int::%fast-instance-layout-eq-p ((object instance-header) :eq)
  (let ((temp1 (make-instance 'ir:virtual-register :kind :integer))
        (temp2 (make-instance 'ir:virtual-register :kind :integer)))
    ;; Read the object header.
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list temp1 `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list temp1)))
    ;; Set the two low bits to potentially convert the header to a
    ;; structure-header. This must be performed in an integer register
    ;; as this will construct some random bad value if the object isn't a
    ;; structure-object.
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:orr
                         :operands (list temp2 temp1 3)
                         :inputs (list temp1)
                         :outputs (list temp2)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr temp2 instance-header)
                         :inputs (list temp2 instance-header)
                         :outputs '()))))
