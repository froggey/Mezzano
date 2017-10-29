;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.simd)

;;; MMX (64-bit integer) vectors.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.int::%define-type-symbol 'mmx-vector 'mmx-vector-p))

(declaim (inline mmx-vector-p))
(defun mmx-vector-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-mmx-vector+))

(defun make-mmx-vector (value)
  (check-type value (unsigned-byte 64))
  (%make-mmx-vector value))

(defun %make-mmx-vector/fixnum (value)
  (make-mmx-vector value))

(defun %make-mmx-vector (value)
  (make-mmx-vector value))

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
  (%mmx-vector-value vector))

(defun %mmx-vector-value (vector)
  (sys.int::%object-ref-unsigned-byte-64 vector 0))

(defun %mmx-vector-value/fixnum (vector)
  (%mmx-vector-value vector))

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
     (defun ,mmx-function (lhs rhs)
       (,mmx-function lhs rhs))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name :mezzano.simd)
       (sys.c::define-transform ,name ((lhs mmx-vector) (rhs mmx-vector))
           ((:optimize (= safety 0) (= speed 3)))
         `(the mmx-vector (sys.c::call ,',mmx-function ,lhs ,rhs))))))

;; MMX
(define-simd-op packssdw %packssdw/mmx)
(define-simd-op packsswb %packsswb/mmx)
(define-simd-op packuswb %packuswb/mmx)
(define-simd-op paddb %paddb/mmx)
(define-simd-op paddw %paddw/mmx)
(define-simd-op paddd %paddd/mmx)
(define-simd-op paddsb %paddsb/mmx)
(define-simd-op paddsw %paddsw/mmx)
(define-simd-op paddusb %paddusb/mmx)
(define-simd-op paddusw %paddusw/mmx)
(define-simd-op pand %pand/mmx)
(define-simd-op pandn %pandn/mmx)
(define-simd-op pcmpeqb %pcmpeqb/mmx)
(define-simd-op pcmpeqw %pcmpeqw/mmx)
(define-simd-op pcmpeqd %pcmpeqd/mmx)
(define-simd-op pcmpgtb %pcmpgtb/mmx)
(define-simd-op pcmpgtw %pcmpgtw/mmx)
(define-simd-op pcmpgtd %pcmpgtd/mmx)
(define-simd-op pmaddwd %pmaddwd/mmx)
(define-simd-op pmulhuw %pmulhuw/mmx)
(define-simd-op pmulhw %pmulhw/mmx)
(define-simd-op pmullw %pmullw/mmx)
(define-simd-op por %por/mmx)
(define-simd-op psllw %psllw/mmx)
(define-simd-op pslld %pslld/mmx)
(define-simd-op psllq %psllq/mmx)
(define-simd-op psraw %psraw/mmx)
(define-simd-op psrad %psrad/mmx)
(define-simd-op psrlw %psrlw/mmx)
(define-simd-op psrld %psrld/mmx)
(define-simd-op psrlq %psrlq/mmx)
(define-simd-op psubb %psubb/mmx)
(define-simd-op psubw %psubw/mmx)
(define-simd-op psubd %psubd/mmx)
(define-simd-op psubsb %psubsb/mmx)
(define-simd-op psubsw %psubsw/mmx)
(define-simd-op psubusb %psubusb/mmx)
(define-simd-op psubusw %psubusw/mmx)
(define-simd-op punpckhbw %punpckhbw/mmx)
(define-simd-op punpckhwd %punpckhwd/mmx)
(define-simd-op punpckhdq %punpckhdq/mmx)
(define-simd-op punpcklbw %punpcklbw/mmx)
(define-simd-op punpcklwd %punpcklwd/mmx)
(define-simd-op punpckldq %punpckldq/mmx)
(define-simd-op pxor %pxor/mmx)

;; SSE1
(define-simd-op pavgb %pavgb/mmx)
(define-simd-op pavgw %pavgw/mmx)
(define-simd-op pmaxsw %pmaxsw/mmx)
(define-simd-op pmaxub %pmaxub/mmx)
(define-simd-op pminsw %pminsw/mmx)
(define-simd-op pminub %pminub/mmx)
(define-simd-op psadbw %psadbw/mmx)

;; SSE2
(define-simd-op paddq %paddq/mmx)
(define-simd-op pmuludq %pmuludq/mmx)
(define-simd-op psubq %psubq/mmx)
