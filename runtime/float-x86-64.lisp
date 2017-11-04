;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

(defun sys.int::%single-float-as-integer (value)
  (sys.int::%single-float-as-integer value))

(defun sys.int::%integer-as-single-float (value)
  (sys.int::%integer-as-single-float value))

(defun %%coerce-fixnum-to-single-float (value)
  (%%coerce-fixnum-to-single-float value))

(defun %%coerce-double-float-to-single-float (value)
  (%%coerce-double-float-to-single-float value))

(defun %%coerce-fixnum-to-double-float (value)
  (%%coerce-fixnum-to-double-float value))

(defun %%coerce-single-float-to-double-float (value)
  (%%coerce-single-float-to-double-float value))

(defun sys.int::%%single-float-< (lhs rhs)
  (sys.int::%%single-float-< lhs rhs))

(defun sys.int::%%double-float-< (lhs rhs)
  (sys.int::%%double-float-< lhs rhs))

(defun sys.int::%%single-float-= (lhs rhs)
  (sys.int::%%single-float-= lhs rhs))

(defun sys.int::%%double-float-= (lhs rhs)
  (sys.int::%%double-float-= lhs rhs))

(defun sys.int::%%truncate-single-float (value)
  (sys.int::%%truncate-single-float value))

(defun sys.int::%%truncate-double-float (value)
  (sys.int::%%truncate-double-float value))

(defun sys.int::%%single-float-/ (lhs rhs)
  (sys.int::%%single-float-/ lhs rhs))

(defun sys.int::%%double-float-/ (lhs rhs)
  (sys.int::%%double-float-/ lhs rhs))

(defun sys.int::%%single-float-+ (lhs rhs)
  (sys.int::%%single-float-+ lhs rhs))

(defun sys.int::%%double-float-+ (lhs rhs)
  (sys.int::%%double-float-+ lhs rhs))

(defun sys.int::%%single-float-- (lhs rhs)
  (sys.int::%%single-float-- lhs rhs))

(defun sys.int::%%double-float-- (lhs rhs)
  (sys.int::%%double-float-- lhs rhs))

(defun sys.int::%%single-float-* (lhs rhs)
  (sys.int::%%single-float-* lhs rhs))

(defun sys.int::%%double-float-* (lhs rhs)
  (sys.int::%%double-float-* lhs rhs))

(defun sys.int::%%single-float-sqrt (value)
  (sys.int::%%single-float-sqrt value))

(defun sys.int::%%double-float-sqrt (value)
  (sys.int::%%double-float-sqrt value))

(sys.int::define-lap-function sys.int::%%make-double-float-rax ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:push 0)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:mov64 :rcx #.(ash 4 sys.int::+n-fixnum-bits+)) ; fixnum 4
  ;; Tag.
  (sys.lap-x86:mov64 :r8 #.(ash sys.int::+object-tag-double-float+
                                sys.int::+n-fixnum-bits+))
  ;; Header data.
  (sys.lap-x86:xor64 :r9 :r9)
  ;; Words.
  (sys.lap-x86:mov64 :r10 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Area
  (sys.lap-x86:mov64 :r11 nil)
  (sys.lap-x86:mov64 :r13 (:function %allocate-object))
  ;; Allocate object.
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  ;; Set data.
  (sys.lap-x86:pop (:object :r8 0))
  ;; Single-value return.
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret))
