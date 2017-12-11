;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.arm64)

(define-builtin sys.int::%value-has-tag-p ((object (:constant tag (typep tag '(unsigned-byte 4)))) :eq)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp object #b1111)
                         :inputs (list object)
                         :outputs (list temp)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr temp tag)
                         :inputs (list temp)
                         :outputs '()))))

(define-builtin mezzano.runtime::%%object-of-type-p ((object (:constant object-tag (typep object-tag '(unsigned-byte 6)))) :eq)
  (let ((header (make-instance 'ir:virtual-register :kind :integer))
        (temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list header `(,object ,(object-slot-displacement -1)))
                         :inputs (list object)
                         :outputs (list header)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp header #b11111100)
                         :inputs (list header)
                         :outputs (list temp)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr temp (ash object-tag sys.int::+object-type-shift+))
                         :inputs (list temp)
                         :outputs (list)))))

(define-builtin mezzano.runtime::%%simple-1d-array-p ((object) :ls)
  (let ((header (make-instance 'ir:virtual-register :kind :integer))
        (temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list header `(,object ,(object-slot-displacement -1)))
                         :inputs (list object)
                         :outputs (list header)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp header #b11111100)
                         :inputs (list header)
                         :outputs (list temp)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr temp (ash sys.int::+last-simple-1d-array-object-tag+
                                                        sys.int::+object-type-shift+))
                         :inputs (list temp)
                         :outputs (list)))))

(define-builtin sys.int::%object-ref-t ((object index) result)
  (cond ((constant-value-p index 'fixnum)
         (let ((disp (object-slot-displacement (fetch-constant-value index))))
           (cond ((or (<= -256 disp 255)
                      (and (<= 0 disp 16380)
                           (zerop (logand disp #b111))))
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:ldr
                                       :operands (list result `(,object ,disp))
                                       :inputs (list object)
                                       :outputs (list result))))
                 (t
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:ldr
                                       :operands (list :x12 `(:literal ,disp))
                                       :inputs (list)
                                       :outputs (list :x12)))
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:ldr
                                       :operands (list result `(,object :x12))
                                       :inputs (list object :x12)
                                       :outputs (list result)))))))
        (t
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:add
                              :operands (list :x12 :xzr index :lsl 2)
                              :inputs (list index)
                              :outputs (list :x12)))
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:sub
                              :operands (list :x12 :x12 (- (+ 8 (- sys.int::+tag-object+))))
                              :inputs (list :x12)
                              :outputs (list :x12)))
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:ldr
                              :operands (list result `(,object :x12))
                              :inputs (list object :x12)
                              :outputs (list result))))))

(define-builtin (setf sys.int::%object-ref-t) ((value object index) result)
  (cond ((constant-value-p index 'fixnum)
         (let ((disp (object-slot-displacement (fetch-constant-value index))))
           (cond ((or (<= -256 disp 255)
                      (and (<= 0 disp 16380)
                           (zerop (logand disp #b111))))
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:str
                                       :operands (list value `(,object ,disp))
                                       :inputs (list value object)
                                       :outputs (list))))
                 (t
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:ldr
                                       :operands (list :x12 `(:literal ,disp))
                                       :inputs (list)
                                       :outputs (list :x12)))
                  (emit (make-instance 'arm64-instruction
                                       :opcode 'lap:str
                                       :operands (list value `(,object :x12))
                                       :inputs (list value object :x12)
                                       :outputs (list)))))))
        (t
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:add
                              :operands (list :x12 :xzr index :lsl 2)
                              :inputs (list index)
                              :outputs (list :x12)))
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:sub
                              :operands (list :x12 :x12 (- (+ 8 (- sys.int::+tag-object+))))
                              :inputs (list :x12)
                              :outputs (list :x12)))
         (emit (make-instance 'arm64-instruction
                              :opcode 'lap:str
                              :operands (list value `(,object :x12))
                              :inputs (list value object :x12)
                              :outputs (list)))))
  (emit (make-instance 'ir:move-instruction
                       :source value
                       :destination result)))

(define-builtin sys.int::%object-header-data ((object) result)
  (let ((temp1 (make-instance 'ir:virtual-register))
        (temp2 (make-instance 'ir:virtual-register)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list temp1 `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list temp1)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp2 temp1 (lognot (1- (ash 1 sys.int::+object-data-shift+))))
                         :inputs (list temp1)
                         :outputs (list temp2)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:add
                         :operands (list result :xzr temp2 :lsr (- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+))
                         :inputs (list temp2)
                         :outputs (list result)))))

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
