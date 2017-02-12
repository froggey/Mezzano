;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :cold-generator.arm64)

(defparameter *undefined-function-thunk*
  `(;; Call helper using the function calling convention, leaving fref intact.
    (mezzano.lap.arm64:ldr :x6 (:function sys.int::raise-undefined-function))
    (mezzano.lap.arm64:ldr :x6 (:object :x6 ,sys.int::+fref-function+))
    (mezzano.lap.arm64:ldr :x9 (:object :x6 ,sys.int::+function-entry-point+))
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
