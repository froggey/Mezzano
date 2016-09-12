;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :cold-generator.arm64)

(defparameter *undefined-function-thunk*
  `((:comment todo)
    (mezzano.lap.arm64:hlt 123))
  "Code for the undefined function thunk.")

(defparameter *closure-trampoline*
  `((:comment todo)
    (mezzano.lap.arm64:hlt 124))
  "Trampoline used for calling a closure or funcallable-instance via an fref.")

(defparameter *funcallable-instance-trampoline*
  `((:comment todo)
    (mezzano.lap.arm64:hlt 125))
  "Trampoline used for calling a closure or funcallable-instance via a funcallable-instance.")
