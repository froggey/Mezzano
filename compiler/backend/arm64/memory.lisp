;;;; Raw memory related primitives.

(in-package :mezzano.compiler.backend.arm64)

(defmacro with-memory-effective-address ((effective-address additional-inputs base-address index scale) &body body)
  "Generate an effective address that deals properly with scaling and constant indices."
  (check-type scale (member 1 2 4 8))
  (let ((unboxed-address (gensym "UNBOXED-ADDRESS"))
        (scaled-index (gensym "SCALED-INDEX")))
    `(let ((,unboxed-address (make-instance 'ir:virtual-register :kind :integer)))
       (emit (make-instance 'ir:unbox-fixnum-instruction
                            :source ,base-address
                            :destination ,unboxed-address))
       (cond ((constant-value-p ,index '(signed-byte 6)) ; TODO: This could be cleverer based on the transfer size, instructions can support a 12-bit unsigned scaled-by-width immediate
              (let ((,effective-address (list ,unboxed-address (* (fetch-constant-value ,index) ,scale)))
                    (,additional-inputs (list ,unboxed-address)))
                ,@body))
             (t
              (with-scaled-fixnum-index (,scaled-index ,index ,scale)
                (let ((,effective-address (list ,unboxed-address ,scaled-index))
                      (,additional-inputs (list ,unboxed-address ,scaled-index)))
                  ,@body)))))))

(define-builtin sys.int::%memref-t ((address index) result)
  (with-memory-effective-address (ea ea-inputs address index 8)
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list result ea)
                         :inputs ea-inputs
                         :outputs (list result)))))

(define-builtin (setf sys.int::%memref-t) ((value address index) result)
  (with-memory-effective-address (ea ea-inputs address index 8)
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:str
                         :operands (list value ea)
                         :inputs (list* value ea-inputs)
                         :outputs (list))))
  (emit (make-instance 'ir:move-instruction
                       :source value
                       :destination result)))

;; TODO: (cas memref-t) & dcas memref-t. the cmpxchg ir instructions currently only support object-relative accesses

(defmacro define-memref-integer-accessor (name read-op write-op read-ex-op write-ex-op scale box-op unbox-op)
  `(progn
     (define-builtin ,name ((address index) result)
       (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
         (with-memory-effective-address (ea ea-inputs address index ,scale)
           (emit (make-instance 'arm64-instruction
                                :opcode ',read-op
                                :operands (list temp ea)
                                :inputs ea-inputs
                                :outputs (list temp))))
         (emit (make-instance ',box-op
                              :source temp
                              :destination result))))
     (define-builtin (setf ,name) ((value address index) result)
       (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
         (emit (make-instance ',unbox-op
                              :source value
                              :destination temp))
         (with-memory-effective-address (ea ea-inputs address index ,scale)
           (emit (make-instance 'arm64-instruction
                                :opcode ',write-op
                                :operands (list temp ea)
                                :inputs (list* temp ea-inputs)
                                :outputs (list))))
         (emit (make-instance 'ir:move-instruction
                              :source value
                              :destination result))))
     (define-builtin (sys.int::cas ,name) ((old new address index) result)
       (let ((old-unboxed (make-instance 'ir:virtual-register :kind :integer))
             (new-unboxed (make-instance 'ir:virtual-register :kind :integer))
             (current-unboxed (make-instance 'ir:virtual-register :kind :integer))
             (address-unboxed (make-instance 'ir:virtual-register :kind :integer))
             (generated-address (make-instance 'ir:virtual-register :kind :integer))
             (store-status (make-instance 'ir:virtual-register :kind :integer))
             (loop-label (make-instance 'ir:label))
             (store-label (make-instance 'ir:label))
             (exit-label (make-instance 'ir:label)))
         (emit (make-instance ',unbox-op
                              :source old
                              :destination old-unboxed))
         (emit (make-instance ',unbox-op
                              :source new
                              :destination new-unboxed))
         (emit (make-instance 'ir:unbox-fixnum-instruction
                              :source address
                              :destination address-unboxed))
         (with-scaled-fixnum-index (scaled-index index ,scale)
           (emit (make-instance 'arm64-instruction
                                :opcode 'lap:add
                                :operands (list generated-address address-unboxed scaled-index)
                                :inputs (list address-unboxed scaled-index)
                                :outputs (list generated-address))))
         (emit (make-instance 'ir:jump-instruction :target loop-label))
         (emit loop-label)
         (emit (make-instance 'arm64-instruction
                              :opcode ',read-ex-op
                              :operands (list current-unboxed (list generated-address))
                              :inputs (list generated-address)
                              :outputs (list current-unboxed)))
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:subs
                              :operands (list :xzr current-unboxed old-unboxed)
                              :inputs (list current-unboxed old-unboxed)
                              :outputs (list)))
         (emit (make-instance 'arm64-branch-instruction
                              :opcode 'lap:b.ne
                              :true-target exit-label
                              :false-target store-label))
         (emit store-label)
         (emit (make-instance 'arm64-instruction
                              :opcode ',write-ex-op
                              :operands (list store-status new-unboxed (list generated-address))
                              :inputs (list new-unboxed generated-address)
                              :outputs (list store-status)))
         (emit (make-instance 'arm64-branch-instruction
                              :opcode 'lap:cbnz
                              :operands (list store-status)
                              :inputs (list store-status)
                              :true-target loop-label
                              :false-target exit-label))
         (emit exit-label)
         (emit (make-instance ',box-op
                              :source current-unboxed
                              :destination result))))))

(define-memref-integer-accessor sys.int::%memref-unsigned-byte-8  lap:ldrb  lap:strb lap:ldaxrb lap:stlxrb 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-16 lap:ldrh  lap:strh lap:ldaxrh lap:stlxrh 2 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-32 lap:ldrw  lap:strw lap:ldaxrw lap:stlxrw 4 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-64 lap:ldr   lap:str  lap:ldaxr  lap:stlxr  8 ir:box-unsigned-byte-64-instruction ir:unbox-unsigned-byte-64-instruction)

(define-memref-integer-accessor sys.int::%memref-signed-byte-8    lap:ldrsb lap:strb lap:ldaxrb lap:stlxrb 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-16   lap:ldrsh lap:strh lap:ldaxrh lap:stlxrh 2 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-32   lap:ldrsw lap:strw lap:ldaxrw lap:stlxrw 4 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-64   lap:ldr   lap:str  lap:ldaxr  lap:stlxr  8 ir:box-signed-byte-64-instruction ir:unbox-signed-byte-64-instruction)

(define-memref-integer-accessor sys.int::%memref-unsigned-byte-8-unscaled  lap:ldrb  lap:strb lap:ldaxrb lap:stlxrb 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-16-unscaled lap:ldrh  lap:strh lap:ldaxrh lap:stlxrh 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-32-unscaled lap:ldrw  lap:strw lap:ldaxrw lap:stlxrw 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-64-unscaled lap:ldr   lap:str  lap:ldaxr  lap:stlxr  1 ir:box-unsigned-byte-64-instruction ir:unbox-unsigned-byte-64-instruction)

(define-memref-integer-accessor sys.int::%memref-signed-byte-8-unscaled    lap:ldrsb lap:strb lap:ldaxrb lap:stlxrb 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-16-unscaled   lap:ldrsh lap:strh lap:ldaxrh lap:stlxrh 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-32-unscaled   lap:ldrsw lap:strw lap:ldaxrw lap:stlxrw 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-64-unscaled   lap:ldr   lap:str  lap:ldaxr  lap:stlxr  1 ir:box-signed-byte-64-instruction ir:unbox-signed-byte-64-instruction)
