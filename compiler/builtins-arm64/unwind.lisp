;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for dealing with binding, unbinding and unwinding.
;;;; Everything to do with the special stack.

(in-package :mezzano.compiler.codegen.arm64)

;;; Touching the special stack directly.

(defbuiltin sys.int::%%special-stack-pointer () ()
  (smash-x0)
  (emit-object-load :x0 :x28 :slot 6) ;; ### special-stack-pointer
  (setf *x0-value* (list (gensym))))

(defbuiltin (setf sys.int::%%special-stack-pointer) (value) ()
  (load-in-x0 value t)
  (emit-object-store :x0 :x28 :slot 6) ;; ### special-stack-pointer
  value)
