;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :cold-generator.arm64)

;; FIXME! Save args.
(defparameter *undefined-function-thunk*
  `(;; Pass invoked-through as the first argument.
    (mezzano.lap.arm64:orr :x0 :xzr :x7)
    (mezzano.lap.arm64:movz :x5 ,(ash 1 sys.int::+n-fixnum-bits+))
    ;; Tail call through to RAISE-UNDEFINED-FUNCTION and let that
    ;; handle the heavy work.
    (mezzano.lap.arm64:ldr :x7 (:function sys.int::raise-undefined-function))
    (mezzano.lap.arm64:ldr :x9 (:object :x7 ,sys.int::+fref-entry-point+))
    (mezzano.lap.arm64:br :x9))
  "Code for the undefined function thunk.")

(defparameter *closure-trampoline*
  `(;; Load the real function from the fref.
    (mezzano.lap.arm64:ldr :x6 (:object :x7 ,sys.int::+fref-function+))
    ;; Invoke the real function via the FUNCTION calling convention.
    ;; This will work even if the fref was altered or made funbound.
    ;; (SETF FUNCTION-REFERENCE-FUNCTION) will set the function to the
    ;; undefined-function thunk in that case, so everything works fine.
    (mezzano.lap.arm64:ldr :x9 (:object :x6 ,sys.int::+function-entry-point+))
    (mezzano.lap.arm64:br :x9))
  "Trampoline used for calling a closure or funcallable-instance via an fref.")

(defparameter *funcallable-instance-trampoline*
  `(;; Load the real function from the funcallable-instance.
    (mezzano.lap.arm64:ldr :x6 (:object :x6 ,sys.int::+funcallable-instance-function+))
    ;; Invoke the real function via the FUNCTION calling convention.
    (mezzano.lap.arm64:ldr :x9 (:object :x6 ,sys.int::+function-entry-point+))
    (mezzano.lap.arm64:br :x9))
  "Trampoline used for calling a closure or funcallable-instance via a funcallable-instance.")
