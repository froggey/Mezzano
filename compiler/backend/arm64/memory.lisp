;;;; Raw memory related primitives.

(in-package :mezzano.compiler.backend.arm64)

(defmacro with-memory-effective-address ((effective-address additional-inputs base-address index scale &key ldp/stp-address) &body body)
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
                ,(if ldp/stp-address
                     ;; Need to add the base & index together for ldp/stp.
                     (let ((final-address (gensym "ADDRESS")))
                       `(let ((,final-address (make-instance 'ir:virtual-register :kind :integer)))
                          (emit (make-instance 'arm64-instruction
                                               :opcode 'lap:add
                                               :operands (list ,final-address ,unboxed-address ,scaled-index)
                                               :inputs (list ,unboxed-address ,scaled-index)
                                               :outputs (list ,final-address)))
                          (let ((,effective-address (list ,final-address))
                                (,additional-inputs (list ,final-address)))
                            ,@body)))
                     `(let ((,effective-address (list ,unboxed-address ,scaled-index))
                            (,additional-inputs (list ,unboxed-address ,scaled-index)))
                        ,@body))))))))

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

(define-builtin sys.int::%memref-t-pair ((address index) (result-1 result-2))
  (with-memory-effective-address (ea ea-inputs address index 8 :ldp/stp-address t)
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldp
                         :operands (list result-1 result-2 ea)
                         :inputs ea-inputs
                         :outputs (list result-1 result-2)))))

(define-builtin sys.int::%set-memref-t-pair ((value-1 value-2 address index) (result-1 result-2))
  (with-memory-effective-address (ea ea-inputs address index 8 :ldp/stp-address t)
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:stp
                         :operands (list value-1 value-2 ea)
                         :inputs (list* value-1 value-2 ea-inputs)
                         :outputs (list))))
  (emit (make-instance 'ir:move-instruction
                       :source value-1
                       :destination result-1))
  (emit (make-instance 'ir:move-instruction
                       :source value-2
                       :destination result-2)))

;; TODO: (cas memref-t) & dcas memref-t. the cmpxchg ir instructions currently only support object-relative accesses

(defmacro define-memref-integer-accessor (name read-op write-op cas-op scale box-op unbox-op)
  (declare (ignore cas-op))
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
     (define-builtin (sys.int::cas ,name) ((old new (:constant so (typep so 'atomic-order)) (:constant fo (typep fo 'atomic-order)) address index) result :has-wrapper nil)
       (let ((old-unboxed (make-instance 'ir:virtual-register :kind :integer))
             (new-unboxed (make-instance 'ir:virtual-register :kind :integer))
             (current-unboxed (make-instance 'ir:virtual-register :kind :integer))
             (address-unboxed (make-instance 'ir:virtual-register :kind :integer))
             (generated-address (make-instance 'ir:virtual-register :kind :integer)))
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
         (emit (make-instance 'arm64-cas-mem-instruction
                              :width ',(* scale 8)
                              :address generated-address
                              :old-value old-unboxed
                              :new-value new-unboxed
                              :current-value current-unboxed
                              :success-order so
                              :failure-order fo))
         (emit (make-instance ',box-op
                              :source current-unboxed
                              :destination result))))))

(define-memref-integer-accessor sys.int::%memref-unsigned-byte-8  lap:ldrb  lap:strb lap:casalb 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-16 lap:ldrh  lap:strh lap:casalh 2 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-32 lap:ldrw  lap:strw lap:casalw 4 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-64 lap:ldr   lap:str  lap:casal  8 ir:box-unsigned-byte-64-instruction ir:unbox-unsigned-byte-64-instruction)

(define-memref-integer-accessor sys.int::%memref-signed-byte-8    lap:ldrsb lap:strb lap:casalb 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-16   lap:ldrsh lap:strh lap:casalh 2 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-32   lap:ldrsw lap:strw lap:casalw 4 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-64   lap:ldr   lap:str  lap:casal   8 ir:box-signed-byte-64-instruction ir:unbox-signed-byte-64-instruction)

(define-memref-integer-accessor sys.int::%memref-unsigned-byte-8-unscaled  lap:ldrb  lap:strb lap:casalb 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-16-unscaled lap:ldrh  lap:strh lap:casalh 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-32-unscaled lap:ldrw  lap:strw lap:casalw 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-64-unscaled lap:ldr   lap:str  lap:casal   1 ir:box-unsigned-byte-64-instruction ir:unbox-unsigned-byte-64-instruction)

(define-memref-integer-accessor sys.int::%memref-signed-byte-8-unscaled    lap:ldrsb lap:strb lap:casalb 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-16-unscaled   lap:ldrsh lap:strh lap:casalh 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-32-unscaled   lap:ldrsw lap:strw lap:casalw 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-64-unscaled   lap:ldr   lap:str  lap:casal   1 ir:box-signed-byte-64-instruction ir:unbox-signed-byte-64-instruction)
