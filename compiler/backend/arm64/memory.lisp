;;;; Raw memory related primitives.

(in-package :mezzano.compiler.backend.arm64)

(defmacro with-memory-effective-address ((effective-address additional-inputs base-address index scale) &body body)
  "Generate an effective address that deals properly with scaling and constant indices."
  (check-type scale (member 1 2 4 8))
  (let ((unboxed-address (gensym "UNBOXED-ADDRESS")))
    `(let ((,unboxed-address (make-instance 'ir:virtual-register :kind :integer)))
       (emit (make-instance 'ir:unbox-fixnum-instruction
                            :source ,base-address
                            :destination ,unboxed-address))
       (cond ((constant-value-p ,index '(signed-byte 6)) ; TODO: This could be cleverer based on the transfer size, instructions can support a 12-bit unsigned scaled-by-width immediate
              (let ((,effective-address (list ,unboxed-address (* (fetch-constant-value ,index) ,scale)))
                    (,additional-inputs (list ,unboxed-address)))
                ,@body))
             (t
              ,(ecase scale
                 (1
                  (let ((unboxed-index (gensym "UNBOXED-INDEX")))
                    `(let ((,unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
                       (emit (make-instance 'ir:unbox-fixnum-instruction
                                            :source ,index
                                            :destination ,unboxed-index))
                       (let ((,effective-address (list ,unboxed-address ,unboxed-index))
                             (,additional-inputs (list ,unboxed-address ,unboxed-index)))
                         ,@body))))
                 (2
                  `(let ((,effective-address (list ,unboxed-address ,index))
                         (,additional-inputs (list ,unboxed-address ,index)))
                     ,@body))
                 ((4 8)
                  ;; Note: Can't used :scale here since the instruction width isn't known
                  ;; and it wouldn't match up anyway since index is boxed.
                  (let ((scaled-index (gensym "SCALED-INDEX"))
                        (scale-shift (ecase scale (4 1) (8 2))))
                    `(let ((,scaled-index (make-instance 'ir:virtual-register :kind :integer)))
                       (emit (make-instance 'arm64-instruction
                                            :opcode 'lap:add
                                            :operands (list ,scaled-index :xzr ,index :lsl ,scale-shift)
                                            :inputs (list ,index)
                                            :outputs (list ,scaled-index)))
                       (let ((,effective-address (list ,unboxed-address ,scaled-index))
                             (,additional-inputs (list ,unboxed-address ,scaled-index)))
                         ,@body))))))))))

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

(defmacro define-memref-integer-accessor (name read-op write-op scale box-op unbox-op)
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
     #+(or) ; TODO
     (define-builtin (sys.int::cas ,name) ((old new address index) result)
       (let ((old-unboxed (make-instance 'ir:virtual-register :kind :integer))
             (new-unboxed (make-instance 'ir:virtual-register :kind :integer))
             (result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
         (emit (make-instance ',unbox-op
                              :source old
                              :destination old-unboxed))
         (emit (make-instance ',unbox-op
                              :source new
                              :destination new-unboxed))
         (emit (make-instance 'ir:move-instruction
                              :source old-unboxed
                              :destination :rax))
         (emit (make-instance 'ir:move-instruction
                              :source new-unboxed
                              :destination :rdx))
         (with-memory-effective-address (ea ea-inputs address index ,scale)
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:cmpxchg
                                :prefix '(lap:lock)
                                :operands (list ea ,(ecase reg
                                                      (:al :dl)
                                                      (:ax :dx)
                                                      (:eax :edx)
                                                      ((nil) :rdx)))
                                :inputs (list* :rax :rdx ea-inputs)
                                :outputs (list :rax)
                                :clobbers '(:rax))))
         ,@(when (not (member read-op '(lap:mov32 lap:mov64)))
             `((emit (make-instance 'x86-instruction
                                    :opcode ',read-op
                                    :operands (list :rax ,reg)
                                    :inputs (list :rax)
                                    :outputs (list :rax)))))
         (emit (make-instance 'ir:move-instruction
                              :source :rax
                              :destination result-unboxed))
         (emit (make-instance ',box-op
                              :source result-unboxed
                              :destination result))))))

(define-memref-integer-accessor sys.int::%memref-unsigned-byte-8  lap:ldrb  lap:strb 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-16 lap:ldrh  lap:strh 2 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-32 lap:ldrw  lap:strw 4 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-64 lap:ldr   lap:str  8 ir:box-unsigned-byte-64-instruction ir:unbox-unsigned-byte-64-instruction)

(define-memref-integer-accessor sys.int::%memref-signed-byte-8    lap:ldrsb lap:strb 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-16   lap:ldrsh lap:strh 2 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-32   lap:ldrsw lap:strw 4 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-64   lap:ldr   lap:str  8 ir:box-signed-byte-64-instruction ir:unbox-signed-byte-64-instruction)

(define-memref-integer-accessor sys.int::%memref-unsigned-byte-8-unscaled  lap:ldrb  lap:strb 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-16-unscaled lap:ldrh  lap:strh 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-32-unscaled lap:ldrw  lap:strw 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-64-unscaled lap:ldr   lap:str  1 ir:box-unsigned-byte-64-instruction ir:unbox-unsigned-byte-64-instruction)

(define-memref-integer-accessor sys.int::%memref-signed-byte-8-unscaled    lap:ldrsb lap:strb 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-16-unscaled   lap:ldrsh lap:strh 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-32-unscaled   lap:ldrsw lap:strw 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-64-unscaled   lap:ldr   lap:str  1 ir:box-signed-byte-64-instruction ir:unbox-signed-byte-64-instruction)
