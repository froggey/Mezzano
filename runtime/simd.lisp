;;;; Functions wrapping x86-64 SIMD instructions

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
  (c::define-transform make-mmx-vector ((value (unsigned-byte 64)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the mmx-vector (c::call %make-mmx-vector ,value)))
  (c::define-transform make-mmx-vector ((value (and fixnum (integer 0))))
      ((:optimize (= safety 0) (= speed 3)))
    `(the mmx-vector (c::call %make-mmx-vector/fixnum ,value)))
  (c::mark-as-constant-foldable 'make-mmx-vector)
  (c::mark-as-constant-foldable '%make-mmx-vector)
  (c::mark-as-constant-foldable '%make-mmx-vector/fixnum))

;; Called by the compiler to box vectors.
(sys.int::define-lap-function %%make-mmx-vector-rax ()
  (:gc :no-frame :layout #*0)
  (lap:push :rbp)
  (:gc :no-frame :layout #*00)
  (lap:mov64 :rbp :rsp)
  (:gc :frame)
  (lap:push 0)
  (lap:push :rax)
  (lap:mov64 :rcx #.(ash 4 sys.int::+n-fixnum-bits+)) ; fixnum 4
  ;; Tag.
  (lap:mov64 :r8 #.(ash sys.int::+object-tag-mmx-vector+
                                sys.int::+n-fixnum-bits+))
  ;; Header data.
  (lap:xor64 :r9 :r9)
  ;; Words.
  (lap:mov64 :r10 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Area
  (lap:mov64 :r11 nil)
  ;; Allocate object.
  (lap:call (:named-call mezzano.runtime::%allocate-object))
  ;; Set data.
  (lap:pop (:object :r8 0))
  ;; Single-value return.
  (lap:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (lap:leave)
  (:gc :no-frame :layout #*0)
  (lap:ret))

(defun mmx-vector-value (vector)
  "Convert an MMX-VECTOR to an unsigned 64-bit value."
  (check-type vector mmx-vector)
  (%mmx-vector-value vector))

(defun %mmx-vector-value (vector)
  (sys.int::%object-ref-unsigned-byte-64 vector 0))

(defun %mmx-vector-value/fixnum (vector)
  (%mmx-vector-value vector))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (c::define-transform mmx-vector-value ((value mmx-vector))
      ((:result-type (unsigned-byte 64)) ; ### hack
       (:optimize (= safety 0) (= speed 3)))
    `(the (unsigned-byte 64) (c::call %mmx-vector-value ,value)))
  (c::define-transform mmx-vector-value ((value mmx-vector))
      ((:result-type fixnum)
       (:optimize (= safety 0) (= speed 3)))
    `(the (and fixnum (integer 0)) (c::call %mmx-vector-value/fixnum ,value)))
  (c::mark-as-constant-foldable 'mmx-vector-value)
  (c::mark-as-constant-foldable '%mmx-vector-value)
  (c::mark-as-constant-foldable '%mmx-vector-value/fixnum))

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

(defun pshufw (a b control)
  (check-type a mmx-vector)
  (check-type b mmx-vector)
  (check-type control (unsigned-byte 8))
  (%pshufw/mmx a b control))

(defun %pshufw/mmx (a b control)
  (macrolet ((gen ()
               `(ecase control
                  ,@(loop for i below 256
                         collect `(,i (%pshufw/mmx a b ,i))))))
    (gen)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'pshufw)
  (c::define-transform pshufw ((a mmx-vector) (b mmx-vector) (control (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the mmx-vector (c::call %pshufw/mmx ,a ,b ,control)))
  (c::mark-as-constant-foldable 'pshufw)
  (c::mark-as-constant-foldable '%pshufw/mmx))

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
  (c::define-transform make-sse-vector ((value (unsigned-byte 128)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (c::call %make-sse-vector ,value)))
  (c::define-transform make-sse-vector ((value (unsigned-byte 64)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (c::call %make-sse-vector/ub64 ,value)))
  (c::define-transform make-sse-vector ((value (and fixnum (integer 0))))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (c::call %make-sse-vector/fixnum ,value)))
  (c::mark-as-constant-foldable 'make-sse-vector)
  (c::mark-as-constant-foldable '%make-sse-vector)
  (c::mark-as-constant-foldable '%make-sse-vector/ub64)
  (c::mark-as-constant-foldable '%make-sse-vector/fixnum))

;; Called by the compiler to box vectors.
(sys.int::define-lap-function %%make-sse-vector-xmm0 ()
  (:gc :no-frame :layout #*0)
  (lap:push :rbp)
  (:gc :no-frame :layout #*00)
  (lap:mov64 :rbp :rsp)
  (:gc :frame)
  (lap:sub64 :rsp 16)
  (lap:movdqa (:rsp) :xmm0)
  (lap:mov64 :rcx #.(ash 4 sys.int::+n-fixnum-bits+)) ; fixnum 4
  ;; Tag.
  (lap:mov64 :r8 #.(ash sys.int::+object-tag-sse-vector+
                                sys.int::+n-fixnum-bits+))
  ;; Header data.
  (lap:xor64 :r9 :r9)
  ;; Words.
  (lap:mov64 :r10 #.(ash 3 sys.int::+n-fixnum-bits+)) ; fixnum 3
  ;; Area
  (lap:mov64 :r11 nil)
  ;; Allocate object.
  (lap:call (:named-call mezzano.runtime::%allocate-object))
  ;; Set data.
  (lap:movdqa :xmm0 (:rsp))
  (lap:movdqa (:object :r8 1) :xmm0)
  ;; Single-value return.
  (lap:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (lap:leave)
  (:gc :no-frame :layout #*0)
  (lap:ret))

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
  (c::define-transform sse-vector-value ((value sse-vector))
      ((:optimize (= safety 0) (= speed 3)))
    `(c::call %sse-vector-value ,value))
  (c::define-transform %sse-vector-value ((value sse-vector))
      ((:result-type (unsigned-byte 64))
       (:optimize (= safety 0) (= speed 3)))
    `(c::call %sse-vector-value/ub64 ,value))
  (c::define-transform %sse-vector-value ((value sse-vector))
      ((:result-type fixnum)
       (:optimize (= safety 0) (= speed 3)))
    `(the (and fixnum (integer 0)) (c::call %sse-vector-value/fixnum ,value)))
  (c::mark-as-constant-foldable 'sse-vector-value)
  (c::mark-as-constant-foldable '%sse-vector-value)
  (c::mark-as-constant-foldable '%sse-vector-value/ub64)
  (c::mark-as-constant-foldable '%sse-vector-value/fixnum))

(defun sse-vector-ref-common (vector n-lanes)
  (let* ((element-type-width (etypecase vector
                               ((simple-array (unsigned-byte 8) (*)) 8)
                               ((simple-array (unsigned-byte 16) (*)) 16)
                               ((simple-array (unsigned-byte 32) (*)) 32)
                               ((simple-array (unsigned-byte 64) (*)) 64)
                               ((simple-array (signed-byte 8) (*)) 8)
                               ((simple-array (signed-byte 16) (*)) 16)
                               ((simple-array (signed-byte 32) (*)) 32)
                               ((simple-array (signed-byte 64) (*)) 64)))
         (access-width (* element-type-width n-lanes)))
    (check-type n-lanes (integer 1))
    (assert (zerop (logand n-lanes (1- n-lanes))) () "N-LANES (~S) must be a power-of-two." n-lanes)
    (assert (<= 8 access-width 128)) ; No instructions can do accesses smaller/larger than this.
    element-type-width))

(defun sse-vector-ref (vector n-lanes index)
  "Read N-LANES integers from VECTOR into an sse-vector.
VECTOR must be a simple 1D specialized on an integer subtype.
N-LANES must be a power of two.
N-LANES must be reasonably constant if you want this to go fast.
The total access width must be at least 8 bits and no larger than 128 bits."
  (let* ((element-type-width (sse-vector-ref-common vector n-lanes))
         (value 0))
    (dotimes (i n-lanes)
      (setf value (logior value
                          (ash (ldb (byte element-type-width 0) ; Snip sign bits aways
                                    (aref vector (+ index i)))
                               (* i element-type-width)))))
    (make-sse-vector value)))

(defun (setf sse-vector-ref) (value vector n-lanes index)
  "Store N-LANES integers into VECTOR from an sse-vector.
VECTOR must be a simple 1D specialized on an integer subtype.
N-LANES must be a power of two.
N-LANES must be reasonably constant if you want this to go fast.
The total access width must be at least 8 bits and no larger than 128 bits."
  (let* ((element-type-width (sse-vector-ref-common vector n-lanes))
         (raw-value (sse-vector-value value)))
    (dotimes (i n-lanes)
      (setf (aref vector (+ index i))
            (sys.int::sign-extend
             (ldb (byte element-type-width (* i element-type-width))
                  raw-value)
             element-type-width))))
  value)

;; Define transforms for SSE-VECTOR-REF
(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((def (width signedp)
               `(progn
                  ;; Emit (def* ...) for each lane count & access width.
                  ,@(loop
                       for n-lanes = 1 then (+ n-lanes n-lanes)
                       for access-width = (* n-lanes width)
                       repeat 128
                       when (<= 8 access-width 128)
                       collect `(def* ,width ,signedp ,n-lanes ,access-width))))
             (def* (width signedp n-lanes access-width)
               (let ((type `(simple-array (,(if signedp 'signed-byte 'unsigned-byte) ,width) (*)))
                     (access-fn (ecase access-width
                                  (8 '%%object-ref-sse-vector/8-unscaled)
                                  (16 '%%object-ref-sse-vector/16-unscaled)
                                  (32 '%%object-ref-sse-vector/32-unscaled)
                                  (64 '%%object-ref-sse-vector/64-unscaled)
                                  (128 '%%object-ref-sse-vector/128-unscaled))))
                 `(progn
                    (c::define-transform sse-vector-ref ((vector ,type array-type) (n-lanes (eql ,n-lanes)) (index fixnum index-type))
                        ((:optimize (= safety 0) (= speed 3)))
                      `(the sse-vector (progn
                                         ,(c::insert-bounds-check vector array-type index index-type :adjust (1- ,n-lanes))
                                         (c::call ,',access-fn ,vector (c::call c::%fast-fixnum-* ,index ',',(/ width 8))))))
                    (c::define-transform (setf sse-vector-ref) ((sse-vector sse-vector) (vector ,type array-type) (n-lanes (eql ,n-lanes)) (index fixnum index-type))
                        ((:optimize (= safety 0) (= speed 3)))
                      `(the sse-vector (progn
                                         ,(c::insert-bounds-check vector array-type index index-type :adjust (1- ,n-lanes))
                                         (c::call ,',access-fn ,sse-vector ,vector (c::call c::%fast-fixnum-* ,index ',',(/ width 8))))))))))
    (def 8 nil)
    (def 16 nil)
    (def 32 nil)
    (def 64 nil)
    (def 8 t)
    (def 16 t)
    (def 32 t)
    (def 64 t)))

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
  (c::define-transform %make-sse-vector-single-float ((a single-float) (b single-float) (c single-float) (d single-float))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector
          (c::call %unpcklps/sse
                       (c::call %unpcklps/sse
                                    (c::call %single-float-to-sse-vector ,a)
                                    (c::call %single-float-to-sse-vector ,c))
                       (c::call %unpcklps/sse
                                    (c::call %single-float-to-sse-vector ,b)
                                    (c::call %single-float-to-sse-vector ,d)))))

  (c::define-transform %make-sse-vector-single-float ((a single-float) (b single-float) (c (eql 0.0)) (d (eql 0.0)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector
          (c::call %movlhps/sse ; Make sure to clear the high bits.
                       (c::call %unpcklps/sse
                                    (c::call %single-float-to-sse-vector ,a)
                                    (c::call %single-float-to-sse-vector ,b))
                       (c::call %make-sse-vector/fixnum '0))))

  (c::define-transform %make-sse-vector-single-float ((a single-float) (b (eql 0.0)) (c (eql 0.0)) (d (eql 0.0)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (c::call %movss/sse ; Make sure to clear the high bits.
                                  (c::call %make-sse-vector/fixnum '0)
                                  (c::call %single-float-to-sse-vector ,a))))
)

(defun sse-vector-single-float-element (vector index)
  "Extract a SINGLE-FLOAT lane from an SSE-VECTOR."
  (check-type vector sse-vector)
  (%sse-vector-single-float-element vector index))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (c::define-transform sse-vector-single-float-element ((vector sse-vector) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(c::call %sse-vector-single-float-element ,vector ,index)))

(defun %sse-vector-to-single-float (vector)
  (check-type vector sse-vector)
  (%sse-vector-to-single-float vector))

(defun %single-float-to-sse-vector (value)
  "Convert a SINGLE-FLOAT to an SSE-VECTOR, storing it in lane 0.
The values in the other lanes of the vector are indeterminate and may not be zero."
  (check-type value single-float)
  (%single-float-to-sse-vector value))

(declaim (inline sse-vector-single-float-ref (setf sse-vector-single-float-ref)))

(defun sse-vector-single-float-ref (vector n-lanes index)
  "Like SSE-VECTOR-REF, but for single-float vectors."
  (ecase n-lanes
    (1 (sse-vector-single-float-1-ref vector index))
    (2 (sse-vector-single-float-2-ref vector index))
    (4 (sse-vector-single-float-4-ref vector index))))

(defun (setf sse-vector-single-float-ref) (value vector n-lanes index)
  "Like SSE-VECTOR-REF, but for single-float vectors."
  (ecase n-lanes
    (1 (setf (sse-vector-single-float-1-ref vector index) value))
    (2 (setf (sse-vector-single-float-2-ref vector index) value))
    (4 (setf (sse-vector-single-float-4-ref vector index) value))))

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
  (c::define-transform sse-vector-single-float-1-ref ((vector (simple-array single-float (*)) array-type) (index fixnum index-type))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       ,(c::insert-bounds-check vector array-type index index-type)
                       (c::call %%object-ref-sse-vector/32-unscaled ,vector (c::call c::%fast-fixnum-* ,index '4)))))
  (c::define-transform (setf sse-vector-single-float-1-ref) ((sse-vector sse-vector) (vector (simple-array single-float (*)) array-type) (index fixnum index-type))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       ,(c::insert-bounds-check vector array-type index index-type)
                       (c::call (setf %%object-ref-sse-vector/32-unscaled) ,sse-vector ,vector (c::call c::%fast-fixnum-* ,index '4))))))

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
  (c::define-transform sse-vector-single-float-2-ref ((vector (simple-array single-float (*)) array-type) (index fixnum index-type))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       ,(c::insert-bounds-check vector array-type index index-type :adjust 1)
                       (c::call %%object-ref-sse-vector/64-unscaled ,vector (c::call c::%fast-fixnum-* ,index '4)))))
  (c::define-transform (setf sse-vector-single-float-2-ref) ((sse-vector sse-vector) (vector (simple-array single-float (*)) array-type) (index fixnum index-type))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       ,(c::insert-bounds-check vector array-type index index-type :adjust 1)
                       (c::call (setf %%object-ref-sse-vector/64-unscaled) ,sse-vector ,vector (c::call c::%fast-fixnum-* ,index '4))))))

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
  (c::define-transform sse-vector-single-float-4-ref ((vector (simple-array single-float (*)) array-type) (index fixnum index-type))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       ,(c::insert-bounds-check vector array-type index index-type :adjust 3)
                       (c::call %%object-ref-sse-vector/128-unscaled ,vector (c::call c::%fast-fixnum-* ,index '4)))))
  (c::define-transform (setf sse-vector-single-float-4-ref) ((sse-vector sse-vector) (vector (simple-array single-float (*)) array-type) (index fixnum index-type))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       ,(c::insert-bounds-check vector array-type index index-type :adjust 3)
                       (c::call (setf %%object-ref-sse-vector/128-unscaled) ,sse-vector ,vector (c::call c::%fast-fixnum-* ,index '4))))))

(defun sse-vector-short-float-1-ref (vector index)
  "Read 1 short-float from the simple 1D short-float array VECTOR into lane 0 of an sse-vector, with the remaining lanes set to zero."
  (check-type vector (simple-array short-float (*)))
  (make-sse-vector (sys.int::%short-float-as-integer (aref vector index))))

(defun (setf sse-vector-short-float-1-ref) (sse-vector vector index)
  "Store lane 0 of SSE-VECTOR into the simple 1D short-float array VECTOR."
  (check-type vector (simple-array short-float (*)))
  (check-type sse-vector sse-vector)
  (setf (aref vector index) (sys.int::%integer-as-short-float (ldb (byte 16 0) (sse-vector-value sse-vector))))
  sse-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (c::define-transform sse-vector-short-float-1-ref ((vector (simple-array short-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       (c::call sys.int::%bounds-check ,vector ,index)
                       (c::call %make-sse-vector
                                (c::call sys.int::%%object-ref-unsigned-byte-16-unscaled
                                         ,vector
                                         (c::call c::%fast-fixnum-* ,index '2))))))
  (c::define-transform (setf sse-vector-short-float-1-ref) ((sse-vector sse-vector) (vector (simple-array short-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       (c::call sys.int::%bounds-check ,vector ,index)
                       (c::call (setf sys.int::%%object-ref-unsigned-byte-16-unscaled)
                                (c::call c::%fast-fixnum-logand (c::call %sse-vector-value/fixnum ,sse-vector) '#xFFFF)
                                ,vector (c::call c::%fast-fixnum-* ,index '2))))))

(defun sse-vector-short-float-2-ref (vector index)
  "Read 2 short-floats from the simple 1D short-float array VECTOR into lanes 0 and 1 of an sse-vector, with the remaining lanes set to zero."
  (check-type vector (simple-array short-float (*)))
  (make-sse-vector (logior (sys.int::%short-float-as-integer (aref vector index))
                           (ash (sys.int::%short-float-as-integer (aref vector (1+ index))) 16))))

(defun (setf sse-vector-short-float-2-ref) (sse-vector vector index)
  "Store lanes 0 and 1 of SSE-VECTOR into the simple 1D short-float array VECTOR."
  (check-type vector (simple-array short-float (*)))
  (check-type sse-vector sse-vector)
  (setf (aref vector index) (sys.int::%integer-as-short-float (ldb (byte 16 0) (sse-vector-value sse-vector)))
        (aref vector (1+ index)) (sys.int::%integer-as-short-float (ldb (byte 16 16) (sse-vector-value sse-vector))))
  sse-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (c::define-transform sse-vector-short-float-2-ref ((vector (simple-array short-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       (c::call sys.int::%bounds-check ,vector (c::call c::%fast-fixnum-+ ,index '1))
                       (c::call %%object-ref-sse-vector/32-unscaled ,vector (c::call c::%fast-fixnum-* ,index '2)))))
  (c::define-transform (setf sse-vector-short-float-2-ref) ((sse-vector sse-vector) (vector (simple-array short-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       (c::call sys.int::%bounds-check ,vector (c::call c::%fast-fixnum-+ ,index '1))
                       (c::call (setf %%object-ref-sse-vector/32-unscaled) ,sse-vector ,vector (c::call c::%fast-fixnum-* ,index '2))))))

(defun sse-vector-short-float-4-ref (vector index)
  "Read 4 short-floats from the simple 1D short-float array VECTOR into an sse-vector."
  (check-type vector (simple-array short-float (*)))
  (make-sse-vector (logior (sys.int::%short-float-as-integer (aref vector index))
                           (ash (sys.int::%short-float-as-integer (aref vector (+ index 1))) 16)
                           (ash (sys.int::%short-float-as-integer (aref vector (+ index 2))) 32)
                           (ash (sys.int::%short-float-as-integer (aref vector (+ index 3))) 48))))

(defun (setf sse-vector-short-float-4-ref) (sse-vector vector index)
  "Store SSE-VECTOR into the simple 1D short-float array VECTOR."
  (check-type vector (simple-array short-float (*)))
  (check-type sse-vector sse-vector)
  (setf (aref vector (+ index 0)) (sys.int::%integer-as-short-float (ldb (byte 16 0) (sse-vector-value sse-vector)))
        (aref vector (+ index 1)) (sys.int::%integer-as-short-float (ldb (byte 16 16) (sse-vector-value sse-vector)))
        (aref vector (+ index 2)) (sys.int::%integer-as-short-float (ldb (byte 16 32) (sse-vector-value sse-vector)))
        (aref vector (+ index 3)) (sys.int::%integer-as-short-float (ldb (byte 16 48) (sse-vector-value sse-vector))))
  sse-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (c::define-transform sse-vector-short-float-4-ref ((vector (simple-array short-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       (c::call sys.int::%bounds-check ,vector (c::call c::%fast-fixnum-+ ,index '3))
                       (c::call %%object-ref-sse-vector/64-unscaled ,vector (c::call c::%fast-fixnum-* ,index '2)))))
  (c::define-transform (setf sse-vector-short-float-4-ref) ((sse-vector sse-vector) (vector (simple-array short-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       (c::call sys.int::%bounds-check ,vector (c::call c::%fast-fixnum-+ ,index '3))
                       (c::call (setf %%object-ref-sse-vector/64-unscaled) ,sse-vector ,vector (c::call c::%fast-fixnum-* ,index '2))))))

(defun sse-vector-short-float-8-ref (vector index)
  "Read 4 short-floats from the simple 1D short-float array VECTOR into an sse-vector."
  (check-type vector (simple-array short-float (*)))
  (make-sse-vector (logior (sys.int::%short-float-as-integer (aref vector index))
                           (ash (sys.int::%short-float-as-integer (aref vector (+ index 1))) 16)
                           (ash (sys.int::%short-float-as-integer (aref vector (+ index 2))) 32)
                           (ash (sys.int::%short-float-as-integer (aref vector (+ index 3))) 48)
                           (ash (sys.int::%short-float-as-integer (aref vector (+ index 4))) 64)
                           (ash (sys.int::%short-float-as-integer (aref vector (+ index 5))) 80)
                           (ash (sys.int::%short-float-as-integer (aref vector (+ index 6))) 96)
                           (ash (sys.int::%short-float-as-integer (aref vector (+ index 7))) 112))))

(defun (setf sse-vector-short-float-8-ref) (sse-vector vector index)
  "Store SSE-VECTOR into the simple 1D short-float array VECTOR."
  (check-type vector (simple-array short-float (*)))
  (check-type sse-vector sse-vector)
  (setf (aref vector (+ index 0)) (sys.int::%integer-as-short-float (ldb (byte 16 0) (sse-vector-value sse-vector)))
        (aref vector (+ index 1)) (sys.int::%integer-as-short-float (ldb (byte 16 16) (sse-vector-value sse-vector)))
        (aref vector (+ index 2)) (sys.int::%integer-as-short-float (ldb (byte 16 32) (sse-vector-value sse-vector)))
        (aref vector (+ index 3)) (sys.int::%integer-as-short-float (ldb (byte 16 48) (sse-vector-value sse-vector)))
        (aref vector (+ index 4)) (sys.int::%integer-as-short-float (ldb (byte 16 64) (sse-vector-value sse-vector)))
        (aref vector (+ index 5)) (sys.int::%integer-as-short-float (ldb (byte 16 80) (sse-vector-value sse-vector)))
        (aref vector (+ index 6)) (sys.int::%integer-as-short-float (ldb (byte 16 96) (sse-vector-value sse-vector)))
        (aref vector (+ index 7)) (sys.int::%integer-as-short-float (ldb (byte 16 112) (sse-vector-value sse-vector))))
  sse-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (c::define-transform sse-vector-short-float-8-ref ((vector (simple-array short-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       (c::call sys.int::%bounds-check ,vector (c::call c::%fast-fixnum-+ ,index '7))
                       (c::call %%object-ref-sse-vector/128-unscaled ,vector (c::call c::%fast-fixnum-* ,index '2)))))
  (c::define-transform (setf sse-vector-short-float-8-ref) ((sse-vector sse-vector) (vector (simple-array short-float (*))) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       (c::call sys.int::%bounds-check ,vector (c::call c::%fast-fixnum-+ ,index '7))
                       (c::call (setf %%object-ref-sse-vector/128-unscaled) ,sse-vector ,vector (c::call c::%fast-fixnum-* ,index '2))))))

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
  (c::define-transform %make-sse-vector-double-float ((a double-float) (b double-float))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector
          (c::call %unpcklpd/sse
                       (c::call %double-float-to-sse-vector ,a)
                       (c::call %double-float-to-sse-vector ,b))))

  (c::define-transform %make-sse-vector-double-float ((a double-float) (b (eql 0.0d0)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (c::call %movsd/sse ; Make sure to clear the high bits.
                                  (c::call %make-sse-vector/fixnum '0)
                                  (c::call %double-float-to-sse-vector ,a))))
)

(defun sse-vector-double-float-element (vector index)
  "Extract a DOUBLE-FLOAT lane from an SSE-VECTOR."
  (check-type vector sse-vector)
  (%sse-vector-double-float-element vector index))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (c::define-transform sse-vector-double-float-element ((vector sse-vector) index)
      ((:optimize (= safety 0) (= speed 3)))
    `(c::call %sse-vector-double-float-element ,vector ,index)))

(defun %sse-vector-to-double-float (vector)
  (%sse-vector-to-double-float vector))

(defun %double-float-to-sse-vector (vector)
  "Convert a DOUBLE-FLOAT to an SSE-VECTOR, storing it in lane 0.
The values in the other lanes of the vector are indeterminate and may not be zero."
  (%double-float-to-sse-vector vector))

(declaim (inline sse-vector-double-float-ref (setf sse-vector-double-float-ref)))

(defun sse-vector-double-float-ref (vector n-lanes index)
  "Like SSE-VECTOR-REF, but for double-float vectors."
  (ecase n-lanes
    (1 (sse-vector-double-float-1-ref vector index))
    (2 (sse-vector-double-float-2-ref vector index))))

(defun (setf sse-vector-double-float-ref) (value vector n-lanes index)
  "Like SSE-VECTOR-REF, but for double-float vectors."
  (ecase n-lanes
    (1 (setf (sse-vector-double-float-1-ref vector index) value))
    (2 (setf (sse-vector-double-float-2-ref vector index) value))))

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
  (c::define-transform sse-vector-double-float-1-ref ((vector (simple-array double-float (*)) array-type) (index fixnum index-type))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       ,(c::insert-bounds-check vector array-type index index-type)
                       (c::call %%object-ref-sse-vector/64-unscaled ,vector (c::call c::%fast-fixnum-* ,index '8)))))
  (c::define-transform (setf sse-vector-double-float-1-ref) ((sse-vector sse-vector) (vector (simple-array double-float (*)) array-type) (index fixnum index-type))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       ,(c::insert-bounds-check vector array-type index index-type)
                       (c::call (setf %%object-ref-sse-vector/64-unscaled) ,sse-vector ,vector (c::call c::%fast-fixnum-* ,index '8))))))

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
  (c::define-transform sse-vector-double-float-2-ref ((vector (simple-array double-float (*)) array-type) (index fixnum index-type))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       ,(c::insert-bounds-check vector array-type index index-type :adjust 1)
                       (c::call %%object-ref-sse-vector/128-unscaled ,vector (c::call c::%fast-fixnum-* ,index '8)))))
  (c::define-transform (setf sse-vector-double-float-2-ref) ((sse-vector sse-vector) (vector (simple-array double-float (*)) array-type) (index fixnum index-type))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (progn
                       ,(c::insert-bounds-check vector array-type index index-type :adjust 1)
                       (c::call (setf %%object-ref-sse-vector/128-unscaled) ,sse-vector ,vector (c::call c::%fast-fixnum-* ,index '8))))))

;; Fake low-level accessors for smaller access widths.

(declaim (inline %%object-ref-sse-vector/8-unscaled %%object-ref-sse-vector/16-unscaled
                 (setf %%object-ref-sse-vector/8-unscaled) (setf %%object-ref-sse-vector/16-unscaled)))

(defun %%object-ref-sse-vector/8-unscaled (object index)
  (%make-sse-vector/fixnum (sys.int::%%object-ref-unsigned-byte-8-unscaled object index)))

(defun (setf %%object-ref-sse-vector/8-unscaled) (value object index)
  (setf (sys.int::%%object-ref-unsigned-byte-8-unscaled object index)
        (%sse-vector-value/fixnum value))
  value)

(defun %%object-ref-sse-vector/16-unscaled (object index)
  (%make-sse-vector/fixnum (sys.int::%%object-ref-unsigned-byte-16-unscaled object index)))

(defun (setf %%object-ref-sse-vector/16-unscaled) (value object index)
  (setf (sys.int::%%object-ref-unsigned-byte-16-unscaled object index)
        (%sse-vector-value/fixnum value))
  value)

(declaim (inline %object-ref-sse-vector/8-unscaled %object-ref-sse-vector/16-unscaled
                 (setf %object-ref-sse-vector/8-unscaled) (setf %object-ref-sse-vector/16-unscaled)))

(defun %object-ref-sse-vector/8-unscaled (object &optional (index 0))
  (%%object-ref-sse-vector/8-unscaled object index))

(defun (setf %object-ref-sse-vector/8-unscaled) (value object &optional (index 0))
  (setf (%%object-ref-sse-vector/8-unscaled object index) value))

(defun %object-ref-sse-vector/16-unscaled (object &optional (index 0))
  (%%object-ref-sse-vector/16-unscaled object index))

(defun (setf %object-ref-sse-vector/16-unscaled) (value object &optional (index 0))
  (setf (%%object-ref-sse-vector/16-unscaled object index) value))

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
  (c::mark-as-constant-foldable 'make-sse-vector)
  (c::mark-as-constant-foldable 'sse-vector-value)
  (c::mark-as-constant-foldable 'make-sse-vector-single-float)
  (c::mark-as-constant-foldable '%make-sse-vector-single-float)
  (c::mark-as-constant-foldable 'sse-vector-single-float-element)
  (c::mark-as-constant-foldable '%sse-vector-single-float-element)
  (c::mark-as-constant-foldable '%sse-vector-to-single-float)
  (c::mark-as-constant-foldable '%single-float-to-sse-vector)
  (c::mark-as-constant-foldable 'make-sse-vector-double-float)
  (c::mark-as-constant-foldable 'sse-vector-double-float-element)
  (c::mark-as-constant-foldable '%sse-vector-to-double-float)
  (c::mark-as-constant-foldable '%double-float-to-sse-vector))

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
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name :mezzano.simd)
       (c::define-transform ,name ((lhs mmx-vector) (rhs mmx-vector))
           ((:optimize (= safety 0) (= speed 3)))
         `(the mmx-vector (c::call ,',mmx-function ,lhs ,rhs)))
       (c::define-transform ,name ((lhs sse-vector) (rhs sse-vector))
           ((:optimize (= safety 0) (= speed 3)))
         `(the sse-vector (c::call ,',sse-function ,lhs ,rhs)))
       (c::mark-as-constant-foldable ',name)
       (c::mark-as-constant-foldable ',mmx-function)
       (c::mark-as-constant-foldable ',sse-function))
     ',name))

(defmacro define-simd-shift-op (name mmx-function sse-function)
  `(progn
     (defun ,name (lhs rhs)
       (if (typep rhs '(unsigned-byte 8))
           (etypecase lhs
             (mmx-vector
              (,mmx-function lhs (make-mmx-vector rhs)))
             (sse-vector
              (,sse-function lhs (make-sse-vector rhs))))
           (etypecase lhs
             (mmx-vector
              (check-type rhs (or mmx-vector (unsigned-byte 8)))
              (,mmx-function lhs rhs))
             (sse-vector
              (check-type rhs (or sse-vector (unsigned-byte 8)))
              (,sse-function lhs rhs)))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name :mezzano.simd)
       (c::define-transform ,name ((lhs mmx-vector) (rhs mmx-vector))
           ((:optimize (= safety 0) (= speed 3)))
         `(the mmx-vector (c::call ,',mmx-function ,lhs ,rhs)))
       (c::define-transform ,name ((lhs mmx-vector) (rhs (unsigned-byte 8)))
           ((:optimize (= safety 0) (= speed 3)))
         `(the mmx-vector (c::call ,',mmx-function ,lhs ,rhs)))
       (c::define-transform ,name ((lhs sse-vector) (rhs sse-vector))
           ((:optimize (= safety 0) (= speed 3)))
         `(the sse-vector (c::call ,',sse-function ,lhs ,rhs)))
       (c::define-transform ,name ((lhs sse-vector) (rhs (unsigned-byte 8)))
           ((:optimize (= safety 0) (= speed 3)))
         `(the sse-vector (c::call ,',sse-function ,lhs ,rhs)))
       (c::mark-as-constant-foldable ',name)
       (c::mark-as-constant-foldable ',mmx-function)
       (c::mark-as-constant-foldable ',sse-function))
     ',name))

(defmacro define-simd-float-op (name sse-function)
  `(progn
     (defun ,name (lhs rhs)
       (etypecase lhs
         (sse-vector
          (check-type rhs sse-vector)
          (,sse-function lhs rhs))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name :mezzano.simd)
       (c::define-transform ,name ((lhs sse-vector) (rhs sse-vector))
           ((:optimize (= safety 0) (= speed 3)))
         `(the sse-vector (c::call ,',sse-function ,lhs ,rhs)))
       (c::mark-as-constant-foldable ',name)
       (c::mark-as-constant-foldable ',sse-function))
     ',name))

(defmacro define-simd-float-op-unary (name sse-function)
  `(progn
     (defun ,name (value)
       (etypecase value
         (sse-vector
          (,sse-function value))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name :mezzano.simd)
       (c::define-transform ,name ((value sse-vector))
           ((:optimize (= safety 0) (= speed 3)))
         `(the sse-vector (c::call ,',sse-function ,value)))
       (c::mark-as-constant-foldable ',name)
       (c::mark-as-constant-foldable ',sse-function))
     ',name))

(defmacro define-simd-float-com-op (name sse-function)
  `(progn
     (defun ,name (lhs rhs)
       (etypecase lhs
         (sse-vector
          (check-type rhs sse-vector)
          (,sse-function lhs rhs))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name :mezzano.simd)
       (c::define-transform ,name ((lhs sse-vector) (rhs sse-vector))
           ((:optimize (= safety 0) (= speed 3)))
         `(c::call ,',sse-function ,lhs ,rhs))
       (c::mark-as-constant-foldable ',name)
       (c::mark-as-constant-foldable ',sse-function))
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
(define-simd-shift-op psllw %psllw/mmx %psllw/sse)
(define-simd-shift-op pslld %pslld/mmx %pslld/sse)
(define-simd-shift-op psllq %psllq/mmx %psllq/sse)
(define-simd-shift-op psraw %psraw/mmx %psraw/sse)
(define-simd-shift-op psrad %psrad/mmx %psrad/sse)
(define-simd-shift-op psrlw %psrlw/mmx %psrlw/sse)
(define-simd-shift-op psrld %psrld/mmx %psrld/sse)
(define-simd-shift-op psrlq %psrlq/mmx %psrlq/sse)
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
(define-simd-float-op-unary movq %movq/sse)
(define-simd-float-op movss %movss/sse)
(define-simd-float-op mulps %mulps/sse)
(define-simd-float-op mulss %mulss/sse)
(define-simd-float-op orps %orps/sse)
(define-simd-float-op-unary rcpps %rcpps/sse)
(define-simd-float-op-unary rcpss %rcpss/sse)
(define-simd-float-op-unary rsqrtps %rsqrtps/sse)
(define-simd-float-op-unary rsqrtss %rsqrtss/sse)
(define-simd-float-op-unary sqrtps %sqrtps/sse)
(define-simd-float-op-unary sqrtss %sqrtss/sse)
(define-simd-float-op subps %subps/sse)
(define-simd-float-op subss %subss/sse)
(define-simd-float-op unpckhps %unpckhps/sse)
(define-simd-float-op unpcklps %unpcklps/sse)
(define-simd-float-op xorps %xorps/sse)

(define-simd-float-op cmpeqss %cmpeqss/sse)
(define-simd-float-op cmpeqps %cmpeqps/sse)
(define-simd-float-op cmpltss %cmpltss/sse)
(define-simd-float-op cmpltps %cmpltps/sse)
(define-simd-float-op cmpless %cmpless/sse)
(define-simd-float-op cmpleps %cmpleps/sse)
(define-simd-float-op cmpunordss %cmpunordss/sse)
(define-simd-float-op cmpunordps %cmpunordps/sse)
(define-simd-float-op cmpnewss %cmpnewss/sse)
(define-simd-float-op cmpnewps %cmpnewps/sse)
(define-simd-float-op cmpnltss %cmpnltss/sse)
(define-simd-float-op cmpnltps %cmpnltps/sse)
(define-simd-float-op cmpnless %cmpnless/sse)
(define-simd-float-op cmpnleps %cmpnleps/sse)
(define-simd-float-op cmpordss %cmpordss/sse)
(define-simd-float-op cmpordps %cmpordps/sse)

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
(define-simd-float-op cvtsd2ss %cvtsd2ss/sse)
(define-simd-float-op cvtss2sd %cvtss2sd/sse)
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

(define-simd-float-op cmpeqsd %cmpeqsd/sse)
(define-simd-float-op cmpeqpd %cmpeqpd/sse)
(define-simd-float-op cmpltsd %cmpltsd/sse)
(define-simd-float-op cmpltpd %cmpltpd/sse)
(define-simd-float-op cmplesd %cmplesd/sse)
(define-simd-float-op cmplepd %cmplepd/sse)
(define-simd-float-op cmpunordsd %cmpunordsd/sse)
(define-simd-float-op cmpunordpd %cmpunordpd/sse)
(define-simd-float-op cmpnewsd %cmpnewsd/sse)
(define-simd-float-op cmpnewpd %cmpnewpd/sse)
(define-simd-float-op cmpnltsd %cmpnltsd/sse)
(define-simd-float-op cmpnltpd %cmpnltpd/sse)
(define-simd-float-op cmpnlesd %cmpnlesd/sse)
(define-simd-float-op cmpnlepd %cmpnlepd/sse)
(define-simd-float-op cmpordsd %cmpordsd/sse)
(define-simd-float-op cmpordpd %cmpordpd/sse)

(define-simd-float-com-op comieqss %comieqss/sse)
(define-simd-float-com-op comieqss %comieqss/sse)
(define-simd-float-com-op comigtss %comigtss/sse)
(define-simd-float-com-op comigess %comigess/sse)
(define-simd-float-com-op comiless %comiless/sse)
(define-simd-float-com-op comiltss %comiltss/sse)
(define-simd-float-com-op comineqss %comineqss/sse)
(define-simd-float-com-op comieqsd %comieqsd/sse)
(define-simd-float-com-op comigtsd %comigtsd/sse)
(define-simd-float-com-op comigesd %comigesd/sse)
(define-simd-float-com-op comilesd %comilesd/sse)
(define-simd-float-com-op comiltsd %comiltsd/sse)
(define-simd-float-com-op comineqsd %comineqsd/sse)
(define-simd-float-com-op ucomieqss %ucomieqss/sse)
(define-simd-float-com-op ucomigtss %ucomigtss/sse)
(define-simd-float-com-op ucomigess %ucomigess/sse)
(define-simd-float-com-op ucomiless %ucomiless/sse)
(define-simd-float-com-op ucomiltss %ucomiltss/sse)
(define-simd-float-com-op ucomineqss %ucomineqss/sse)
(define-simd-float-com-op ucomieqsd %ucomieqsd/sse)
(define-simd-float-com-op ucomigtsd %ucomigtsd/sse)
(define-simd-float-com-op ucomigesd %ucomigesd/sse)
(define-simd-float-com-op ucomilesd %ucomilesd/sse)
(define-simd-float-com-op ucomiltsd %ucomiltsd/sse)
(define-simd-float-com-op ucomineqsd %ucomineqsd/sse)

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
  (c::define-transform shufps ((a sse-vector) (b sse-vector) (control (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (c::call %shufps/sse ,a ,b ,control)))
  (c::mark-as-constant-foldable 'shufps)
  (c::mark-as-constant-foldable '%shufps/sse))

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
  (c::define-transform shufpd ((a sse-vector) (b sse-vector) (control (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (c::call %shufpd/sse ,a ,b ,control))
    (c::mark-as-constant-foldable 'shufpd))
  (c::mark-as-constant-foldable 'shufpd)
  (c::mark-as-constant-foldable '%shufpd/sse))

(defun pshufd (a b control)
  (check-type a sse-vector)
  (check-type b sse-vector)
  (check-type control (unsigned-byte 8))
  (%pshufd/sse a b control))

(defun %pshufd/sse (a b control)
  (macrolet ((gen ()
               `(ecase control
                  ,@(loop for i below 256
                         collect `(,i (%pshufd/sse a b ,i))))))
    (gen)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'pshufd)
  (c::define-transform pshufd ((a sse-vector) (b sse-vector) (control (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (c::call %pshufd/sse ,a ,b ,control)))
  (c::mark-as-constant-foldable 'pshufd)
  (c::mark-as-constant-foldable '%pshufd/sse))

(defun pshufhw (a b control)
  (check-type a sse-vector)
  (check-type b sse-vector)
  (check-type control (unsigned-byte 8))
  (%pshufhw/sse a b control))

(defun %pshufhw/sse (a b control)
  (macrolet ((gen ()
               `(ecase control
                  ,@(loop for i below 256
                         collect `(,i (%pshufhw/sse a b ,i))))))
    (gen)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'pshufhw)
  (c::define-transform pshufhw ((a sse-vector) (b sse-vector) (control (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (c::call %pshufhw/sse ,a ,b ,control)))
  (c::mark-as-constant-foldable 'pshufhw)
  (c::mark-as-constant-foldable '%pshufhw/sse))

(defun pshuflw (a b control)
  (check-type a sse-vector)
  (check-type b sse-vector)
  (check-type control (unsigned-byte 8))
  (%pshuflw/sse a b control))

(defun %pshuflw/sse (a b control)
  (macrolet ((gen ()
               `(ecase control
                  ,@(loop for i below 256
                         collect `(,i (%pshuflw/sse a b ,i))))))
    (gen)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'pshuflw)
  (c::define-transform pshuflw ((a sse-vector) (b sse-vector) (control (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (c::call %pshuflw/sse ,a ,b ,control)))
  (c::mark-as-constant-foldable 'pshuflw)
  (c::mark-as-constant-foldable '%pshuflw/sse))

(defun pslldq (value count)
  (check-type value sse-vector)
  (check-type count (unsigned-byte 8))
  (%pslldq/sse value count))

(defun %pslldq/sse (value count)
  (macrolet ((gen ()
               `(ecase count
                  ,@(loop for i below 256
                         collect `(,i (%pslldq/sse value ,i))))))
    (gen)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'pslldq)
  (c::define-transform pslldq ((value sse-vector) (count (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (c::call %pslldq/sse ,value ,count)))
  (c::mark-as-constant-foldable 'pslldq)
  (c::mark-as-constant-foldable '%pslldq/sse))

(defun psrldq (value count)
  (check-type value sse-vector)
  (check-type count (unsigned-byte 8))
  (%psrldq/sse value count))

(defun %psrldq/sse (value count)
  (macrolet ((gen ()
               `(ecase count
                  ,@(loop for i below 256
                         collect `(,i (%psrldq/sse value ,i))))))
    (gen)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'psrldq)
  (c::define-transform psrldq ((value sse-vector) (count (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (c::call %psrldq/sse ,value ,count)))
  (c::mark-as-constant-foldable 'psrldq)
  (c::mark-as-constant-foldable '%psrldq/sse))

(defun pmovmskb (value)
  (etypecase value
    (mmx-vector (%pmovmskb/mmx value))
    (sse-vector (%pmovmskb/sse value))))

(defun %pmovmskb/mmx (value)
  (%pmovmskb/mmx value))

(defun %pmovmskb/sse (value)
  (%pmovmskb/sse value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'pmovmskb)
  (c::define-transform pmovmskb ((value mmx-vector))
      ((:optimize (= safety 0) (= speed 3)))
    `(the (and fixnum (integer 0)) (c::call %pmovmskb/mmx ,value)))
  (c::define-transform pmovmskb ((value sse-vector))
      ((:optimize (= safety 0) (= speed 3)))
    `(the (and fixnum (integer 0)) (c::call %pmovmskb/sse ,value)))
  (c::mark-as-constant-foldable 'pmovmskb)
  (c::mark-as-constant-foldable '%pmovmskb/mmx)
  (c::mark-as-constant-foldable '%pmovmskb/sse))

(defun movmskps (value)
  (check-type value sse-vector)
  (%movmskps/sse value))

(defun %movmskps/sse (value)
  (%movmskps/sse value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'movmskps)
  (c::define-transform movmskps ((value sse-vector))
      ((:optimize (= safety 0) (= speed 3)))
    `(the (and fixnum (integer 0)) (c::call %movmskps/sse ,value)))
  (c::mark-as-constant-foldable 'movmskps)
  (c::mark-as-constant-foldable '%movmskps/sse))

(defun movmskpd (value)
  (check-type value sse-vector)
  (%movmskpd/sse value))

(defun %movmskpd/sse (value)
  (%movmskpd/sse value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'movmskpd)
  (c::define-transform movmskpd ((value sse-vector))
      ((:optimize (= safety 0) (= speed 3)))
    `(the (and fixnum (integer 0)) (c::call %movmskpd/sse ,value)))
  (c::mark-as-constant-foldable 'movmskpd)
  (c::mark-as-constant-foldable '%movmskpd/sse))

(defun pextrw (a imm)
  (etypecase a
    (mmx-vector (%pextrw/mmx a imm))
    (sse-vector (%pextrw/sse a imm))))

(defun %pextrw/mmx (a imm)
  (macrolet ((gen ()
               `(ecase imm
                  ,@(loop for i below 256
                         collect `(,i (%pextrw/mmx a ,i))))))
    (gen)))

(defun %pextrw/sse (a imm)
  (macrolet ((gen ()
               `(ecase imm
                  ,@(loop for i below 256
                         collect `(,i (%pextrw/sse a ,i))))))
    (gen)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'pextrw)
  (c::define-transform pextrw ((a mmx-vector) (imm (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the (unsigned-byte 16) (c::call %pextrw/mmx ,a ,imm)))
  (c::define-transform pextrw ((a mmx-vector) (imm (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the (unsigned-byte 16) (c::call %pextrw/sse ,a ,imm)))
  (c::mark-as-constant-foldable 'pextrw)
  (c::mark-as-constant-foldable '%pextrw/mmx)
  (c::mark-as-constant-foldable '%pextrw/sse))

(defun pinsrw (a b imm)
  (check-type b fixnum)
  (etypecase a
    (mmx-vector (%pinsrw/mmx a b imm))
    (sse-vector (%pinsrw/sse a b imm))))

(defun %pinsrw/mmx (a b imm)
  (macrolet ((gen ()
               `(ecase imm
                  ,@(loop for i below 256
                         collect `(,i (%pinsrw/mmx a b ,i))))))
    (gen)))

(defun %pinsrw/sse (a b imm)
  (macrolet ((gen ()
               `(ecase imm
                  ,@(loop for i below 256
                         collect `(,i (%pinsrw/sse a b ,i))))))
    (gen)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'pinsrw)
  (c::define-transform pinsrw ((a mmx-vector) (b fixnum) (imm (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the mmx-vector (c::call %pinsrw/mmx ,a ,b ,imm)))
  (c::define-transform pinsrw ((a mmx-vector) (b fixnum) (imm (unsigned-byte 8)))
      ((:optimize (= safety 0) (= speed 3)))
    `(the sse-vector (c::call %pinsrw/sse ,a ,b ,imm)))
  (c::mark-as-constant-foldable 'pinsrw)
  (c::mark-as-constant-foldable '%pinsrw/mmx)
  (c::mark-as-constant-foldable '%pinsrw/sse))

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
