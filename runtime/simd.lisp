;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.simd)

;;; MMX (64-bit integer) vectors.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.int::%define-type-symbol 'mmx-vector 'mmx-vector-p))

(declaim (inline mmx-vector-p))
(defun mmx-vector-p (object)
  "Return true if OBJECT is an MMX-VECTOR, NIL otherwise."
  (sys.int::%object-of-type-p object sys.int::+object-tag-mmx-vector+))

(defun make-mmx-vector (value)
  "Convert an unsigned 64-bit integer to an MMX-VECTOR."
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
    `(the mmx-vector (sys.c::call %make-mmx-vector/fixnum ,value)))
  (sys.c::mark-as-constant-foldable 'make-mmx-vector)
  (sys.c::mark-as-constant-foldable '%make-mmx-vector)
  (sys.c::mark-as-constant-foldable '%make-mmx-vector/fixnum))

;; Called by the compiler to box vectors.
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
  "Convert an MMX-VECTOR to an unsigned 64-bit value."
  (check-type vector mmx-vector)
  (%mmx-vector-value vector))

(defun %mmx-vector-value (vector)
  (sys.int::%object-ref-unsigned-byte-64 vector 0))

(defun %mmx-vector-value/fixnum (vector)
  (%mmx-vector-value vector))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform mmx-vector-value ((value mmx-vector))
      ((:result-type (unsigned-byte 64)) ; ### hack
       (:optimize (= safety 0) (= speed 3)))
    `(the (unsigned-byte 64) (sys.c::call %mmx-vector-value ,value)))
  (sys.c::define-transform mmx-vector-value ((value mmx-vector))
      ((:result-type fixnum)
       (:optimize (= safety 0) (= speed 3)))
    `(the (and fixnum (integer 0)) (sys.c::call %mmx-vector-value/fixnum ,value)))
  (sys.c::mark-as-constant-foldable 'mmx-vector-value)
  (sys.c::mark-as-constant-foldable '%mmx-vector-value)
  (sys.c::mark-as-constant-foldable '%mmx-vector-value/fixnum))

(defmethod print-object ((object mmx-vector) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~16,'0X" (mmx-vector-value object))))

(defmethod describe-object ((object mmx-vector) stream)
  (format stream "~S is an MMX vector.~%" object))

(defmethod make-load-form ((object mmx-vector) &optional environment)
  (declare (ignore environment))
  `(locally
       ;; Don't let the compiler constant-fold the call back into a vector.
       (declare (notinline make-mmx-vector))
     (make-mmx-vector ',(mmx-vector-value object))))

;;; SSE (128-bit integer/float) vectors.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.int::%define-type-symbol 'sse-vector 'sse-vector-p))

(declaim (inline sse-vector-p))
(defun sse-vector-p (object)
  "Return true if OBJECT is an SSE-VECTOR, NIL otherwise."
  (sys.int::%object-of-type-p object sys.int::+object-tag-sse-vector+))

(defun make-sse-vector (value)
  "Convert an unsigned 128-bit integer to an SSE-VECTOR."
  (check-type value (unsigned-byte 128))
  (%make-sse-vector value))

(defun %make-sse-vector (value)
  (let ((obj (mezzano.runtime::%allocate-object sys.int::+object-tag-sse-vector+
                                                0
                                                3
                                                nil)))
    (setf (sys.int::%object-ref-unsigned-byte-64 obj 1) (ldb (byte 64 0) value)
          (sys.int::%object-ref-unsigned-byte-64 obj 2) (ldb (byte 64 64) value))
    obj))

(defun %make-sse-vector/ub64 (value)
  (make-sse-vector value))

(defun %make-sse-vector/fixnum (value)
  (make-sse-vector value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform make-sse-vector ((value (unsigned-byte 128)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call %make-sse-vector ,value)))
  (sys.c::define-transform make-sse-vector ((value (unsigned-byte 64)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call %make-sse-vector/ub64 ,value)))
  (sys.c::define-transform make-sse-vector ((value (and fixnum (integer 0))))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call %make-sse-vector/fixnum ,value)))
  (sys.c::mark-as-constant-foldable 'make-sse-vector)
  (sys.c::mark-as-constant-foldable '%make-sse-vector)
  (sys.c::mark-as-constant-foldable '%make-sse-vector/ub64)
  (sys.c::mark-as-constant-foldable '%make-sse-vector/fixnum))

;; Called by the compiler to box vectors.
(sys.int::define-lap-function %%make-sse-vector-xmm0 ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:sub64 :rsp 16)
  (sys.lap-x86:movdqa (:rsp) :xmm0)
  (sys.lap-x86:mov64 :rcx #.(ash 4 sys.int::+n-fixnum-bits+)) ; fixnum 4
  ;; Tag.
  (sys.lap-x86:mov64 :r8 #.(ash sys.int::+object-tag-sse-vector+
                                sys.int::+n-fixnum-bits+))
  ;; Header data.
  (sys.lap-x86:xor64 :r9 :r9)
  ;; Words.
  (sys.lap-x86:mov64 :r10 #.(ash 3 sys.int::+n-fixnum-bits+)) ; fixnum 3
  ;; Area
  (sys.lap-x86:mov64 :r11 nil)
  (sys.lap-x86:mov64 :r13 (:function mezzano.runtime::%allocate-object))
  ;; Allocate object.
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  ;; Set data.
  (sys.lap-x86:movdqa :xmm0 (:rsp))
  (sys.lap-x86:movdqa (:object :r8 1) :xmm0)
  ;; Single-value return.
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret))

(defun sse-vector-value (vector)
  "Convert an SSE-VECTOR to an unsigned 128-bit integer."
  (check-type vector sse-vector)
  (%sse-vector-value vector))

(defun %sse-vector-value (vector)
  (logior (sys.int::%object-ref-unsigned-byte-64 vector 1)
          (ash (sys.int::%object-ref-unsigned-byte-64 vector 2) 64)))

(defun %sse-vector-value/fixnum (vector)
  (%sse-vector-value vector))

(defun %sse-vector-value/ub64 (vector)
  (%sse-vector-value vector))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform sse-vector-value ((value sse-vector))
      ((:optimize (= safety 0) (= speed 3)))
    `(sys.c::call %sse-vector-value ,value))
  (sys.c::define-transform %sse-vector-value ((value sse-vector))
      ((:result-type (unsigned-byte 64))
       (:optimize (= safety 0) (= speed 3)))
    `(sys.c::call %sse-vector-value/ub64 ,value))
  (sys.c::define-transform %sse-vector-value ((value sse-vector))
      ((:result-type fixnum)
       (:optimize (= safety 0) (= speed 3)))
    `(the (and fixnum (integer 0)) (sys.c::call %sse-vector-value/fixnum ,value)))
  (sys.c::mark-as-constant-foldable 'sse-vector-value)
  (sys.c::mark-as-constant-foldable '%sse-vector-value)
  (sys.c::mark-as-constant-foldable '%sse-vector-value/ub64)
  (sys.c::mark-as-constant-foldable '%sse-vector-value/fixnum))

(declaim (inline make-sse-vector-single-float))
(defun make-sse-vector-single-float (&optional (a 0.0) (b 0.0) (c 0.0) (d 0.0))
  "Construct an SSE-VECTOR from 4 SINGLE-FLOAT values.
A is stored in lane 0, B in lane 1, C in 2, and D in 3.
Equivalent to the _mm_set_ps intrinsic."
  (the sse-vector (%make-sse-vector-single-float a b c d)))

(defun %make-sse-vector-single-float (a b c d)
  (check-type a single-float)
  (check-type b single-float)
  (check-type c single-float)
  (check-type d single-float)
  (let ((vector (mezzano.runtime::%allocate-object sys.int::+object-tag-sse-vector+
                                                   0
                                                   3
                                                   nil)))
    (setf (sys.int::%object-ref-single-float vector 2) a
          (sys.int::%object-ref-single-float vector 3) b
          (sys.int::%object-ref-single-float vector 4) c
          (sys.int::%object-ref-single-float vector 5) d)
    vector))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform %make-sse-vector-single-float ((a single-float) (b single-float) (c single-float) (d single-float))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector
          (sys.c::call %unpcklps/sse
                       (sys.c::call %unpcklps/sse
                                    (sys.c::call %single-float-to-sse-vector ,a)
                                    (sys.c::call %single-float-to-sse-vector ,c))
                       (sys.c::call %unpcklps/sse
                                    (sys.c::call %single-float-to-sse-vector ,b)
                                    (sys.c::call %single-float-to-sse-vector ,d)))))

  (sys.c::define-transform %make-sse-vector-single-float ((a single-float) (b single-float) (c (eql 0.0)) (d (eql 0.0)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector
          (sys.c::call %movlhps/sse ; Make sure to clear the high bits.
                       (sys.c::call %unpcklps/sse
                                    (sys.c::call %single-float-to-sse-vector ,a)
                                    (sys.c::call %single-float-to-sse-vector ,b))
                       (sys.c::call %make-sse-vector/fixnum '0))))

  (sys.c::define-transform %make-sse-vector-single-float ((a single-float) (b (eql 0.0)) (c (eql 0.0)) (d (eql 0.0)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call %movss/sse ; Make sure to clear the high bits.
                                  (sys.c::call %make-sse-vector/fixnum '0)
                                  (sys.c::call %single-float-to-sse-vector ,a))))
)

(defun sse-vector-single-float-element (vector index)
  "Extract a SINGLE-FLOAT lane from an SSE-VECTOR."
  (check-type vector sse-vector)
  (%sse-vector-single-float-element vector index))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform sse-vector-single-float-element ((vector sse-vector) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(sys.c::call %sse-vector-single-float-element ,vector ,index)))

(defun %sse-vector-to-single-float (vector)
  (%sse-vector-to-single-float vector))

(defun %single-float-to-sse-vector (vector)
  "Convert a SINGLE-FLOAT to an SSE-VECTOR, storing it in lane 0.
The values in the other lanes of the vector are indeterminate and may not be zero."
  (%single-float-to-sse-vector vector))

(defun sse-vector-single-float-1-ref (vector index)
  "Read 1 single-float from the simple 1D single-float array VECTOR into lane 0 of an sse-vector, with the remaining lanes set to zero."
  (check-type vector (simple-array single-float (*)))
  (make-sse-vector-single-float (aref vector index)))

(defun (setf sse-vector-single-float-1-ref) (sse-vector vector index)
  "Store lane 0 of SSE-VECTOR into the simple 1D single-float array VECTOR."
  (check-type vector (simple-array single-float (*)))
  (check-type sse-vector sse-vector)
  (setf (aref vector index) (sse-vector-single-float-element sse-vector 0))
  sse-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform sse-vector-single-float-1-ref ((vector (simple-array single-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call %%object-ref-sse-vector/32-unscaled ,vector (sys.c::call sys.c::%fast-fixnum-* ,index '4))))
  (sys.c::define-transform (setf sse-vector-single-float-1-ref) ((sse-vector sse-vector) (vector (simple-array single-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call (setf %%object-ref-sse-vector/32-unscaled) ,sse-vector ,vector (sys.c::call sys.c::%fast-fixnum-* ,index '4)))))

(defun sse-vector-single-float-2-ref (vector index)
  "Read 2 single-floats from the simple 1D single-float array VECTOR into lanes 0 and 1 of an sse-vector, with the remaining lanes set to zero."
  (check-type vector (simple-array single-float (*)))
  (make-sse-vector-single-float (aref vector index)
                                (aref vector (1+ index))))

(defun (setf sse-vector-single-float-2-ref) (sse-vector vector index)
  "Store lanes 0 and 1 of SSE-VECTOR into the simple 1D single-float array VECTOR."
  (check-type vector (simple-array single-float (*)))
  (check-type sse-vector sse-vector)
  (setf (aref vector index) (sse-vector-single-float-element sse-vector 0)
        (aref vector (1+ index)) (sse-vector-single-float-element sse-vector 1))
  sse-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform sse-vector-single-float-2-ref ((vector (simple-array single-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call %%object-ref-sse-vector/64-unscaled ,vector (sys.c::call sys.c::%fast-fixnum-* ,index '4))))
  (sys.c::define-transform (setf sse-vector-single-float-2-ref) ((sse-vector sse-vector) (vector (simple-array single-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call (setf %%object-ref-sse-vector/64-unscaled) ,sse-vector ,vector (sys.c::call sys.c::%fast-fixnum-* ,index '4)))))

(defun sse-vector-single-float-4-ref (vector index)
  "Read 4 single-floats from the simple 1D single-float array VECTOR into an sse-vector."
  (check-type vector (simple-array single-float (*)))
  (make-sse-vector-single-float (aref vector (+ index 0))
                                (aref vector (+ index 1))
                                (aref vector (+ index 2))
                                (aref vector (+ index 3))))

(defun (setf sse-vector-single-float-4-ref) (sse-vector vector index)
  "Store SSE-VECTOR into the simple 1D single-float array VECTOR."
  (check-type vector (simple-array single-float (*)))
  (check-type sse-vector sse-vector)
  (setf (aref vector (+ index 0)) (sse-vector-single-float-element sse-vector 0)
        (aref vector (+ index 1)) (sse-vector-single-float-element sse-vector 1)
        (aref vector (+ index 2)) (sse-vector-single-float-element sse-vector 2)
        (aref vector (+ index 3)) (sse-vector-single-float-element sse-vector 3))
  sse-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform sse-vector-single-float-4-ref ((vector (simple-array single-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call %%object-ref-sse-vector/128-unscaled ,vector (sys.c::call sys.c::%fast-fixnum-* ,index '4))))
  (sys.c::define-transform (setf sse-vector-single-float-4-ref) ((sse-vector sse-vector) (vector (simple-array single-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call (setf %%object-ref-sse-vector/128-unscaled) ,sse-vector ,vector (sys.c::call sys.c::%fast-fixnum-* ,index '4)))))

(declaim (inline make-sse-vector-double-float))
(defun make-sse-vector-double-float (&optional (a 0.0d0) (b 0.0d0))
  "Construct an SSE-VECTOR from 2 DOUBLE-FLOAT values.
A is stored in lane 0, and B in lane 1.
Equivalent to the _mm_set_pd intrinsic."
  (the sse-vector (%make-sse-vector-double-float a b)))

(defun %make-sse-vector-double-float (a b)
  (check-type a double-float)
  (check-type b double-float)
  (let ((vector (mezzano.runtime::%allocate-object sys.int::+object-tag-sse-vector+
                                                   0
                                                   3
                                                   nil)))
    (setf (sys.int::%object-ref-double-float vector 1) a
          (sys.int::%object-ref-double-float vector 2) b)
    vector))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform %make-sse-vector-double-float ((a double-float) (b double-float))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector
          (sys.c::call %unpcklpd/sse
                       (sys.c::call %double-float-to-sse-vector ,a)
                       (sys.c::call %double-float-to-sse-vector ,b))))

  (sys.c::define-transform %make-sse-vector-double-float ((a double-float) (b (eql 0.0d0)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call %movsd/sse ; Make sure to clear the high bits.
                                  (sys.c::call %make-sse-vector/fixnum '0)
                                  (sys.c::call %double-float-to-sse-vector ,a))))
)

(defun sse-vector-double-float-element (vector index)
  "Extract a DOUBLE-FLOAT lane from an SSE-VECTOR."
  (check-type vector sse-vector)
  (%sse-vector-double-float-element vector index))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform sse-vector-double-float-element ((vector sse-vector) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(sys.c::call %sse-vector-double-float-element ,vector ,index)))

(defun %sse-vector-to-double-float (vector)
  (%sse-vector-to-double-float vector))

(defun %double-float-to-sse-vector (vector)
  "Convert a DOUBLE-FLOAT to an SSE-VECTOR, storing it in lane 0.
The values in the other lanes of the vector are indeterminate and may not be zero."
  (%double-float-to-sse-vector vector))

(defun sse-vector-double-float-1-ref (vector index)
  "Read 1 double-float from the simple 1D double-float array VECTOR into lane 0 of an sse-vector, with the remaining lanes set to zero."
  (check-type vector (simple-array double-float (*)))
  (make-sse-vector-double-float (aref vector index)))

(defun (setf sse-vector-double-float-1-ref) (sse-vector vector index)
  "Store lane 0 of SSE-VECTOR into the simple 1D double-float array VECTOR."
  (check-type vector (simple-array double-float (*)))
  (check-type sse-vector sse-vector)
  (setf (aref vector index) (sse-vector-double-float-element sse-vector 0))
  sse-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform sse-vector-double-float-1-ref ((vector (simple-array double-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call %%object-ref-sse-vector/64-unscaled ,vector (sys.c::call sys.c::%fast-fixnum-* ,index '8))))
  (sys.c::define-transform (setf sse-vector-double-float-1-ref) ((sse-vector sse-vector) (vector (simple-array double-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call (setf %%object-ref-sse-vector/64-unscaled) ,sse-vector ,vector (sys.c::call sys.c::%fast-fixnum-* ,index '8)))))

(defun sse-vector-double-float-2-ref (vector index)
  "Read 2 double-floats from the simple 1D double-float array VECTOR into lanes 0 and 1 of an sse-vector, with the remaining lanes set to zero."
  (check-type vector (simple-array double-float (*)))
  (make-sse-vector-double-float (aref vector index)
                                (aref vector (1+ index))))

(defun (setf sse-vector-double-float-2-ref) (sse-vector vector index)
  "Store lanes 0 and 1 of SSE-VECTOR into the simple 1D double-float array VECTOR."
  (check-type vector (simple-array double-float (*)))
  (check-type sse-vector sse-vector)
  (setf (aref vector index) (sse-vector-double-float-element sse-vector 0)
        (aref vector (1+ index)) (sse-vector-double-float-element sse-vector 1))
  sse-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::define-transform sse-vector-double-float-2-ref ((vector (simple-array double-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call %%object-ref-sse-vector/128-unscaled ,vector (sys.c::call sys.c::%fast-fixnum-* ,index '8))))
  (sys.c::define-transform (setf sse-vector-double-float-2-ref) ((sse-vector sse-vector) (vector (simple-array double-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call (setf %%object-ref-sse-vector/128-unscaled) ,sse-vector ,vector (sys.c::call sys.c::%fast-fixnum-* ,index '8)))))

(declaim (inline %object-ref-sse-vector/32 %object-ref-sse-vector/64 %object-ref-sse-vector/128
                 (setf %object-ref-sse-vector/32) (setf %object-ref-sse-vector/64) (setf %object-ref-sse-vector/128)))

(defun %object-ref-sse-vector/32 (object &optional (index 0))
  "MOVD"
  (%%object-ref-sse-vector/32-unscaled object (the fixnum (* index 4))))

(defun (setf %object-ref-sse-vector/32) (value object &optional (index 0))
  "MOVD"
  (setf (%%object-ref-sse-vector/32-unscaled object (the fixnum (* index 4))) value))

(defun %object-ref-sse-vector/64 (object &optional (index 0))
  "MOVQ"
  (%%object-ref-sse-vector/64-unscaled object (the fixnum (* index 8))))

(defun (setf %object-ref-sse-vector/64) (value object &optional (index 0))
  "MOVQ"
  (setf (%%object-ref-sse-vector/64-unscaled object (the fixnum (* index 8))) value))

(defun %object-ref-sse-vector/128 (object &optional (index 0))
  "MOVDQU"
  (%%object-ref-sse-vector/128-unscaled object (the fixnum (* index 16))))

(defun (setf %object-ref-sse-vector/128) (value object &optional (index 0))
  "MOVDQU"
  (setf (%%object-ref-sse-vector/128-unscaled object (the fixnum (* index 16))) value))

(defmethod print-object ((object sse-vector) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~32,'0X" (sse-vector-value object))))

(defmethod describe-object ((object sse-vector) stream)
  (format stream "~S is an SSE vector.~%" object))

(defmethod make-load-form ((object sse-vector) &optional environment)
  (declare (ignore environment))
  `(locally
       ;; Don't let the compiler constant-fold the call back into a vector.
       (declare (notinline make-sse-vector))
     (make-sse-vector ',(sse-vector-value object))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.c::mark-as-constant-foldable 'make-sse-vector)
  (sys.c::mark-as-constant-foldable 'sse-vector-value)
  (sys.c::mark-as-constant-foldable 'make-sse-vector-single-float)
  (sys.c::mark-as-constant-foldable '%make-sse-vector-single-float)
  (sys.c::mark-as-constant-foldable 'sse-vector-single-float-element)
  (sys.c::mark-as-constant-foldable '%sse-vector-single-float-element)
  (sys.c::mark-as-constant-foldable '%sse-vector-to-single-float)
  (sys.c::mark-as-constant-foldable '%single-float-to-sse-vector)
  (sys.c::mark-as-constant-foldable 'make-sse-vector-double-float)
  (sys.c::mark-as-constant-foldable 'sse-vector-double-float-element)
  (sys.c::mark-as-constant-foldable '%sse-vector-to-double-float)
  (sys.c::mark-as-constant-foldable '%double-float-to-sse-vector))

(defmacro define-simd-integer-op (name mmx-function sse-function)
  `(progn
     (defun ,name (lhs rhs)
       (etypecase lhs
         (mmx-vector
          (check-type rhs mmx-vector)
          (,mmx-function lhs rhs))
         (sse-vector
          (check-type rhs sse-vector)
          (,sse-function lhs rhs))))
     (defun ,mmx-function (lhs rhs)
       (,mmx-function lhs rhs))
     (defun ,sse-function (lhs rhs)
       (,sse-function lhs rhs))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name :mezzano.simd)
       (sys.c::define-transform ,name ((lhs mmx-vector) (rhs mmx-vector))
           ((:optimize (= safety 0) (= speed 3)))
         `(the mmx-vector (sys.c::call ,',mmx-function ,lhs ,rhs)))
       (sys.c::define-transform ,name ((lhs sse-vector) (rhs sse-vector))
           ((:optimize (= safety 0) (= speed 3)))
         `(the sse-vector (sys.c::call ,',sse-function ,lhs ,rhs)))
       (sys.c::mark-as-constant-foldable ',name)
       (sys.c::mark-as-constant-foldable ',mmx-function)
       (sys.c::mark-as-constant-foldable ',sse-function))
     ',name))

(defmacro define-simd-float-op (name sse-function)
  `(progn
     (defun ,name (lhs rhs)
       (etypecase lhs
         (sse-vector
          (check-type rhs sse-vector)
          (,sse-function lhs rhs))))
     (defun ,sse-function (lhs rhs)
       (,sse-function lhs rhs))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name :mezzano.simd)
       (sys.c::define-transform ,name ((lhs sse-vector) (rhs sse-vector))
           ((:optimize (= safety 0) (= speed 3)))
         `(the sse-vector (sys.c::call ,',sse-function ,lhs ,rhs)))
       (sys.c::mark-as-constant-foldable ',name)
       (sys.c::mark-as-constant-foldable ',sse-function))
     ',name))

(defmacro define-simd-float-op-unary (name sse-function)
  `(progn
     (defun ,name (value)
       (etypecase value
         (sse-vector
          (,sse-function value))))
     (defun ,sse-function (value)
       (,sse-function value))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name :mezzano.simd)
       (sys.c::define-transform ,name ((value sse-vector))
           ((:optimize (= safety 0) (= speed 3)))
         `(the sse-vector (sys.c::call ,',sse-function ,value)))
       (sys.c::mark-as-constant-foldable ',name)
       (sys.c::mark-as-constant-foldable ',sse-function))
     ',name))

;; MMX
(define-simd-integer-op packssdw %packssdw/mmx %packssdw/sse)
(define-simd-integer-op packsswb %packsswb/mmx %packsswb/sse)
(define-simd-integer-op packuswb %packuswb/mmx %packuswb/sse)
(define-simd-integer-op paddb %paddb/mmx %paddb/sse)
(define-simd-integer-op paddw %paddw/mmx %paddw/sse)
(define-simd-integer-op paddd %paddd/mmx %paddd/sse)
(define-simd-integer-op paddsb %paddsb/mmx %paddsb/sse)
(define-simd-integer-op paddsw %paddsw/mmx %paddsw/sse)
(define-simd-integer-op paddusb %paddusb/mmx %paddusb/sse)
(define-simd-integer-op paddusw %paddusw/mmx %paddusw/sse)
(define-simd-integer-op pand %pand/mmx %pand/sse)
(define-simd-integer-op pandn %pandn/mmx %pandn/sse)
(define-simd-integer-op pcmpeqb %pcmpeqb/mmx %pcmpeqb/sse)
(define-simd-integer-op pcmpeqw %pcmpeqw/mmx %pcmpeqw/sse)
(define-simd-integer-op pcmpeqd %pcmpeqd/mmx %pcmpeqd/sse)
(define-simd-integer-op pcmpgtb %pcmpgtb/mmx %pcmpgtb/sse)
(define-simd-integer-op pcmpgtw %pcmpgtw/mmx %pcmpgtw/sse)
(define-simd-integer-op pcmpgtd %pcmpgtd/mmx %pcmpgtd/sse)
(define-simd-integer-op pmaddwd %pmaddwd/mmx %pmaddwd/sse)
(define-simd-integer-op pmulhuw %pmulhuw/mmx %pmulhuw/sse)
(define-simd-integer-op pmulhw %pmulhw/mmx %pmulhw/sse)
(define-simd-integer-op pmullw %pmullw/mmx %pmullw/sse)
(define-simd-integer-op por %por/mmx %por/sse)
(define-simd-integer-op psllw %psllw/mmx %psllw/sse)
(define-simd-integer-op pslld %pslld/mmx %pslld/sse)
(define-simd-integer-op psllq %psllq/mmx %psllq/sse)
(define-simd-integer-op psraw %psraw/mmx %psraw/sse)
(define-simd-integer-op psrad %psrad/mmx %psrad/sse)
(define-simd-integer-op psrlw %psrlw/mmx %psrlw/sse)
(define-simd-integer-op psrld %psrld/mmx %psrld/sse)
(define-simd-integer-op psrlq %psrlq/mmx %psrlq/sse)
(define-simd-integer-op psubb %psubb/mmx %psubb/sse)
(define-simd-integer-op psubw %psubw/mmx %psubw/sse)
(define-simd-integer-op psubd %psubd/mmx %psubd/sse)
(define-simd-integer-op psubsb %psubsb/mmx %psubsb/sse)
(define-simd-integer-op psubsw %psubsw/mmx %psubsw/sse)
(define-simd-integer-op psubusb %psubusb/mmx %psubusb/sse)
(define-simd-integer-op psubusw %psubusw/mmx %psubusw/sse)
(define-simd-integer-op punpckhbw %punpckhbw/mmx %punpckhbw/sse)
(define-simd-integer-op punpckhwd %punpckhwd/mmx %punpckhwd/sse)
(define-simd-integer-op punpckhdq %punpckhdq/mmx %punpckhdq/sse)
(define-simd-integer-op punpcklbw %punpcklbw/mmx %punpcklbw/sse)
(define-simd-integer-op punpcklwd %punpcklwd/mmx %punpcklwd/sse)
(define-simd-integer-op punpckldq %punpckldq/mmx %punpckldq/sse)
(define-simd-integer-op pxor %pxor/mmx %pxor/sse)

;; SSE1
(define-simd-integer-op pavgb %pavgb/mmx %pavgb/sse)
(define-simd-integer-op pavgw %pavgw/mmx %pavgw/sse)
(define-simd-integer-op pmaxsw %pmaxsw/mmx %pmaxsw/sse)
(define-simd-integer-op pmaxub %pmaxub/mmx %pmaxub/sse)
(define-simd-integer-op pminsw %pminsw/mmx %pminsw/sse)
(define-simd-integer-op pminub %pminub/mmx %pminub/sse)
(define-simd-integer-op psadbw %psadbw/mmx %psadbw/sse)

(define-simd-float-op addps %addps/sse)
(define-simd-float-op addss %addss/sse)
(define-simd-float-op andnps %andnps/sse)
(define-simd-float-op andps %andps/sse)
(define-simd-float-op divps %divps/sse)
(define-simd-float-op divss %divss/sse)
(define-simd-float-op maxps %maxps/sse)
(define-simd-float-op maxss %maxss/sse)
(define-simd-float-op minps %minps/sse)
(define-simd-float-op minss %minss/sse)
(define-simd-float-op movhlps %movhlps/sse)
(define-simd-float-op movlhps %movlhps/sse)
(define-simd-float-op movss %mulss/sse)
(define-simd-float-op mulps %mulps/sse)
(define-simd-float-op mulss %mulss/sse)
(define-simd-float-op orps %orps/sse)
(define-simd-float-op rcpps %rcpps/sse)
(define-simd-float-op rcpss %rcpss/sse)
(define-simd-float-op-unary rsqrtps %rsqrtps/sse)
(define-simd-float-op-unary rsqrtss %rsqrtss/sse)
(define-simd-float-op-unary sqrtps %sqrtps/sse)
(define-simd-float-op-unary sqrtss %sqrtss/sse)
(define-simd-float-op subps %subps/sse)
(define-simd-float-op subss %subss/sse)
(define-simd-float-op unpckhps %unpckhps/sse)
(define-simd-float-op unpcklps %unpcklps/sse)
(define-simd-float-op xorps %xorps/sse)

;; SSE2
(define-simd-integer-op paddq %paddq/mmx %paddq/sse)
(define-simd-integer-op pmuludq %pmuludq/mmx %pmuludq/sse)
(define-simd-integer-op psubq %psubq/mmx %psubq/sse)
(define-simd-float-op punpckhqdq %punpckhqdq/sse) ; Integer op, but XMM only.
(define-simd-float-op punpcklqdq %punpcklqdq/sse) ; Integer op, but XMM only.

(define-simd-float-op addpd %addpd/sse)
(define-simd-float-op addsd %addsd/sse)
(define-simd-float-op andnpd %andnpd/sse)
(define-simd-float-op-unary cvtpd2ps %cvtpd2ps/sse)
(define-simd-float-op-unary cvtps2pd %cvtps2pd/sse)
(define-simd-float-op-unary cvtsd2ss %cvtsd2ss/sse)
(define-simd-float-op-unary cvtss2sd %cvtss2sd/sse)
(define-simd-float-op-unary cvtpd2dq %cvtpd2dq/sse)
(define-simd-float-op-unary cvttpd2dq %cvttpd2dq/sse)
(define-simd-float-op-unary cvtdq2pd %cvtdq2pd/sse)
(define-simd-float-op-unary cvtps2dq %cvtps2dq/sse)
(define-simd-float-op-unary cvttps2dq %cvttps2dq/sse)
(define-simd-float-op-unary cvtdq2ps %cvtdq2ps/sse)
(define-simd-float-op divpd %divpd/sse)
(define-simd-float-op divsd %divsd/sse)
(define-simd-float-op maxpd %maxpd/sse)
(define-simd-float-op maxsd %maxsd/sse)
(define-simd-float-op minpd %minpd/sse)
(define-simd-float-op minsd %minsd/sse)
(define-simd-float-op movsd %movsd/sse)
(define-simd-float-op mulpd %mulpd/sse)
(define-simd-float-op mulsd %mulsd/sse)
(define-simd-float-op orpd %orpd/sse)
(define-simd-float-op-unary sqrtpd %sqrtpd/sse)
(define-simd-float-op-unary sqrtsd %sqrtsd/sse)
(define-simd-float-op subpd %subpd/sse)
(define-simd-float-op subsd %subsd/sse)
(define-simd-float-op unpckhpd %unpckhpd/sse)
(define-simd-float-op unpcklpd %unpcklpd/sse)
(define-simd-float-op xorpd %xorpd/sse)

(defun shufps (a b control)
  (check-type a sse-vector)
  (check-type b sse-vector)
  (check-type control (unsigned-byte 8))
  (%shufps/sse a b control))

(defun %shufps/sse (a b control)
  (macrolet ((gen ()
               `(ecase control
                  ,@(loop for i below 256
                         collect `(,i (%shufps/sse a b ,i))))))
    (gen)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'shufps)
  (sys.c::define-transform shufps ((a sse-vector) (b sse-vector) (control (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call %shufps/sse ,a ,b ,control)))
  (sys.c::mark-as-constant-foldable 'shufps)
  (sys.c::mark-as-constant-foldable '%shufps/sse))

(defun shufpd (a b control)
  (check-type a sse-vector)
  (check-type b sse-vector)
  (check-type control (unsigned-byte 8))
  (%shufpd/sse a b control))

(defun %shufpd/sse (a b control)
  (ecase (ldb (byte 2 0) control)
    (0 (%shufpd/sse a b 0))
    (1 (%shufpd/sse a b 1))
    (2 (%shufpd/sse a b 2))
    (3 (%shufpd/sse a b 3))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'shufpd)
  (sys.c::define-transform shufpd ((a sse-vector) (b sse-vector) (control (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (sys.c::call %shufpd/sse ,a ,b ,control))
    (sys.c::mark-as-constant-foldable 'shufpd))
  (sys.c::mark-as-constant-foldable 'shufpd)
  (sys.c::mark-as-constant-foldable '%shufpd/sse))

(declaim (inline %sse-vector-single-float-element))
(defun %sse-vector-single-float-element (vector index)
  (the single-float
       (ecase index
         (0 (%sse-vector-to-single-float vector))
         (1 (%sse-vector-to-single-float (%shufps/sse vector vector #b01)))
         (2 (%sse-vector-to-single-float (%shufps/sse vector vector #b10)))
         (3 (%sse-vector-to-single-float (%shufps/sse vector vector #b11))))))

(declaim (inline %sse-vector-double-float-element))
(defun %sse-vector-double-float-element (vector index)
  (the double-float
       (ecase index
         (0 (%sse-vector-to-double-float vector))
         (1 (%sse-vector-to-double-float (%unpckhpd/sse vector vector))))))
