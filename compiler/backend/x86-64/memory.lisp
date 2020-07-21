;;;; Raw memory related primitives.

(in-package :mezzano.compiler.backend.x86-64)

(defmacro with-memory-effective-address ((effective-address additional-inputs base-address index scale) &body body)
  "Generate an effective address that deals properly with scaling and constant indices."
  (check-type scale (member 1 2 4 8))
  (let ((unboxed-address (gensym)))
    `(let ((,unboxed-address (make-instance 'ir:virtual-register :kind :integer)))
       (emit (make-instance 'ir:unbox-fixnum-instruction
                            :source ,base-address
                            :destination ,unboxed-address))
       (cond ((constant-value-p ,index '(signed-byte 29))
              (let ((,effective-address (list ,unboxed-address (* (fetch-constant-value ,index) ,scale)))
                    (,additional-inputs (list ,unboxed-address)))
                ,@body))
             (t
              ,(if (eql scale 1)
                   (let ((unboxed-index (gensym)))
                     `(let ((,unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
                        (emit (make-instance 'ir:unbox-fixnum-instruction
                                             :source ,index
                                             :destination ,unboxed-index))
                        (let ((,effective-address (list ,unboxed-address ,unboxed-index))
                              (,additional-inputs (list ,unboxed-address ,unboxed-index)))
                          ,@body)))
                   `(let ((,effective-address (list ,unboxed-address (list ,index ,(/ scale 2))))
                          (,additional-inputs (list ,unboxed-address ,index)))
                      ,@body)))))))

(define-builtin sys.int::%memref-t ((address index) result)
  (with-memory-effective-address (ea ea-inputs address index 8)
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list result ea)
                         :inputs ea-inputs
                         :outputs (list result)))))

(define-builtin (setf sys.int::%memref-t) ((value address index) result)
  (with-memory-effective-address (ea ea-inputs address index 8)
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list ea value)
                         :inputs (list* value ea-inputs)
                         :outputs (list))))
  (emit (make-instance 'ir:move-instruction
                       :source value
                       :destination result)))

;; TODO: (cas memref-t) & dcas memref-t. the cmpxchg ir instructions currently only support object-relative accesses

(defmacro define-memref-integer-accessor (name read-op write-op reg scale box-op unbox-op)
  `(progn
     (define-builtin ,name ((address index) result)
       (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
         ;; Need to use :eax and a temporary here because it's currently impossible
         ;; to replace vregs with non-64-bit gprs.
         ;; Using a temporary & a move before the box allows the box to safely
         ;; eliminated.
         (with-memory-effective-address (ea ea-inputs address index ,scale)
           (emit (make-instance 'x86-instruction
                                :opcode ',read-op
                                :operands (list ,(if (eql read-op 'lap:mov32) reg 'temp) ea)
                                :inputs ea-inputs
                                :outputs (list ,(if (eql read-op 'lap:mov32) :rax 'temp))
                                :clobbers ,(if (eql read-op 'lap:mov32) ''(:rax) 'nil))))
         ,@(when (eql read-op 'lap:mov32)
             `((emit (make-instance 'ir:move-instruction
                                    :source :rax
                                    :destination temp))))
         (emit (make-instance ',box-op
                              :source temp
                              :destination result))))
     (define-builtin (setf ,name) ((value address index) result)
       (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
         ;; Need to use :eax and a temporary here because it's currently impossible
         ;; to replace vregs with non-64-bit gprs.
         ;; Using a temporary & a move before the box allows the box to safely
         ;; eliminated.
         (emit (make-instance ',unbox-op
                              :source value
                              :destination temp))
         ,@(when reg
             `((emit (make-instance 'ir:move-instruction
                                    :source temp
                                    :destination :rax))))
         (with-memory-effective-address (ea ea-inputs address index ,scale)
           (emit (make-instance 'x86-instruction
                                :opcode ',write-op
                                :operands (list ea ,(or reg 'temp))
                                :inputs (list* ,(if reg :rax 'temp) ea-inputs)
                                :outputs (list))))
         (emit (make-instance 'ir:move-instruction
                              :source value
                              :destination result))))
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

(define-memref-integer-accessor sys.int::%memref-unsigned-byte-8  lap:movzx8  lap:mov8  :al  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-16 lap:movzx16 lap:mov16 :ax  2 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-32 lap:mov32   lap:mov32 :eax 4 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-64 lap:mov64   lap:mov64 nil  8 ir:box-unsigned-byte-64-instruction ir:unbox-unsigned-byte-64-instruction)

(define-memref-integer-accessor sys.int::%memref-signed-byte-8    lap:movsx8  lap:mov8  :al  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-16   lap:movsx16 lap:mov16 :ax  2 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-32   lap:movsx32 lap:mov32 :eax 4 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-64   lap:mov64   lap:mov64 nil  8 ir:box-signed-byte-64-instruction ir:unbox-signed-byte-64-instruction)

(define-memref-integer-accessor sys.int::%memref-unsigned-byte-8-unscaled  lap:movzx8  lap:mov8  :al  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-16-unscaled lap:movzx16 lap:mov16 :ax  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-32-unscaled lap:mov32   lap:mov32 :eax 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-unsigned-byte-64-unscaled lap:mov64   lap:mov64 nil  1 ir:box-unsigned-byte-64-instruction ir:unbox-unsigned-byte-64-instruction)

(define-memref-integer-accessor sys.int::%memref-signed-byte-8-unscaled    lap:movsx8  lap:mov8  :al  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-16-unscaled   lap:movsx16 lap:mov16 :ax  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-32-unscaled   lap:movsx32 lap:mov32 :eax 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-memref-integer-accessor sys.int::%memref-signed-byte-64-unscaled   lap:mov64   lap:mov64 nil  1 ir:box-signed-byte-64-instruction ir:unbox-signed-byte-64-instruction)
