;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.arm64)

(define-builtin sys.int::read-frame-pointer (() result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:add
                       :operands (list result :xzr :x29 :lsl sys.int::+n-fixnum-bits+)
                       :inputs (list)
                       :outputs (list result))))
