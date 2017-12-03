;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.arm64)

(define-builtin sys.int::fixnump ((object) :eq)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:ands
                       :operands (list :xzr object sys.int::+fixnum-tag-mask+)
                       :inputs (list object)
                       :outputs (list))))
