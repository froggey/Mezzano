;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.x86-64)

(define-builtin sys.int::%memref-t ((address index) result)
  (let ((address-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source address
                         :destination address-unboxed))
    (cond ((constant-value-p index '(signed-byte 29))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:mov64
                                :operands (list result `(,address-unboxed ,(* (fetch-constant-value index) 8)))
                                :inputs (list address-unboxed)
                                :outputs (list result))))
          (t
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:mov64
                                :operands (list result `(,address-unboxed (,index 4)))
                                :inputs (list address-unboxed index)
                                :outputs (list result)))))))

(define-builtin (setf sys.int::%memref-t) ((value address index) result)
  (let ((address-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source address
                         :destination address-unboxed))
    (cond ((constant-value-p index '(signed-byte 29))
           (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list `(,address-unboxed ,(* (fetch-constant-value index) 8)) value)
                         :inputs (list address-unboxed value)
                         :outputs (list))))
          (t
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:mov64
                                :operands (list `(,address-unboxed (,index 4)) value)
                                :inputs (list address-unboxed index value)
                                :outputs (list)))))
    (emit (make-instance 'ir:move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%memref-unsigned-byte-8 ((address index) result)
  (let ((address-unboxed (make-instance 'ir:virtual-register :kind :integer))
        (index-unboxed (make-instance 'ir:virtual-register :kind :integer))
        (temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source address
                         :destination address-unboxed))
    (cond ((constant-value-p index '(signed-byte 29))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movzx8
                                :operands (list temp `(,address-unboxed ,(fetch-constant-value index)))
                                :inputs (list address-unboxed)
                                :outputs (list temp))))
          (t
           (emit (make-instance 'ir:unbox-fixnum-instruction
                                :source index
                                :destination index-unboxed))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:movzx8
                                :operands (list temp `(,address-unboxed ,index-unboxed))
                                :inputs (list address-unboxed index-unboxed)
                                :outputs (list temp)))))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))
