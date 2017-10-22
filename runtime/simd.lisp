;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.simd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.int::%define-type-symbol 'mmx-vector 'mmx-vector-p))

(declaim (inline mmx-vector-p))
(defun mmx-vector-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-mmx-vector+))

(defun make-mmx-vector (value)
  (check-type value (unsigned-byte 64))
  (%make-mmx-vector value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform make-mmx-vector ((value (unsigned-byte 64)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the mmx-vector (sys.c::call %make-mmx-vector ,value)))
  (sys.c::define-transform make-mmx-vector ((value (and fixnum (integer 0))))
      ((:optimize (= safety 0) (= speed 3)))
    `(the mmx-vector (sys.c::call %make-mmx-vector/fixnum ,value))))

(sys.int::define-lap-function %%make-mmx-vector-rax ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:push 0)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:mov64 :rcx #.(ash 4 sys.int::+n-fixnum-bits+)) ; fixnum 4
  ;; Tag.
  (sys.lap-x86:mov64 :r8 #.(ash sys.int::+object-tag-mmx-vector+
                                sys.int::+n-fixnum-bits+))
  ;; Header data.
  (sys.lap-x86:xor64 :r9 :r9)
  ;; Words.
  (sys.lap-x86:mov64 :r10 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Area
  (sys.lap-x86:mov64 :r11 nil)
  (sys.lap-x86:mov64 :r13 (:function mezzano.runtime::%allocate-object))
  ;; Allocate object.
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  ;; Set data.
  (sys.lap-x86:pop (:object :r8 0))
  ;; Single-value return.
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret))

(defun mmx-vector-value (vector)
  (check-type vector mmx-vector)
  (sys.int::%object-ref-unsigned-byte-64 vector 0))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform mmx-vector-value ((value mmx-vector))
      ((:optimize (= safety 0) (= speed 3)))
    `(sys.c::call %mmx-vector-value ,value))
  (sys.c::define-transform %mmx-vector-value ((value mmx-vector))
      ((:result-type fixnum)
       (:optimize (= safety 0) (= speed 3)))
    `(sys.c::call %mmx-vector-value/fixnum ,value)))

(defmethod print-object ((object mmx-vector) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~16,'0X" (mmx-vector-value object))))

(defmethod describe-object ((object mmx-vector) stream)
  (format stream "~S is an MMX vector.~%" object))

(defmacro define-simd-op (name mmx-function)
  `(progn
     (defun ,name (lhs rhs)
       (etypecase lhs
         (mmx-vector
          (check-type rhs mmx-vector)
          (,mmx-function lhs rhs))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (sys.c::define-transform ,name ((lhs mmx-vector) (rhs mmx-vector))
           ((:optimize (= safety 0) (= speed 3)))
         `(the mmx-vector (sys.c::call ,',mmx-function ,lhs ,rhs))))))

(define-simd-op punpcklbw %punpcklbw/mmx)
(define-simd-op packuswb %packuswb/mmx)
(define-simd-op paddusw %paddusw/mmx)
(define-simd-op psubb %psubb/mmx)
(define-simd-op pmullw %pmullw/mmx)
(define-simd-op pmulhuw %pmulhuw/mmx)
(define-simd-op pmuludq %pmuludq/mmx)
