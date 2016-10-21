;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(sys.int::define-lap-function local-cpu-info (())
  (mezzano.lap.arm64:orr :x0 :xzr :x27)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))

(defun sys.int::%interrupt-state ()
  nil)

(defun %disable-interrupts ())

(defun %enable-interrupts ()
  (panic "not implemented"))

(defun %call-on-wired-stack-without-interrupts (function unused &optional arg1 arg2 arg3)
  (funcall function 'sp 'fp arg1 arg2 arg3))
