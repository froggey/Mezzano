;;; cl-nibbles-style accessors.

(in-package :mezzano.internals)

(defun %swap-endian-unsigned (value width)
  (check-type width (integer 0))
  (assert (zerop (rem width 8)))
  (check-type value integer)
  (let ((result 0))
    (dotimes (i (truncate width 8))
      (setf result (logior (ash result 8)
                           (logand value #xFF)))
      (setf value (ash value -8)))
    result))

(defun %%swap-endian-unsigned (value width)
  (%swap-endian-unsigned value width))

(defun %swap-endian-signed (value width)
  (sign-extend (%swap-endian-unsigned value width) width))

(defun %%swap-endian-signed (value width)
  (%swap-endian-signed value width))

(macrolet ((def (width)
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (mezzano.compiler::define-transform %swap-endian-unsigned ((value integer) (width (eql ,width)))
                    ((:optimize (= safety 0) (= speed 3)))
                  `(the (unsigned-byte ,',width)
                        (mezzano.compiler::call %%swap-endian-unsigned ,value ',',width)))
                (mezzano.compiler::define-transform %swap-endian-unsigned ((value integer) (width (eql ,width)))
                    ((:optimize (/= safety 0) (= speed 3)))
                  `(progn
                     ,(mezzano.compiler::insert-type-check value 'integer)
                     (the (unsigned-byte ,',width)
                          (mezzano.compiler::call %%swap-endian-unsigned ,value ',',width))))
                (mezzano.compiler::define-transform %swap-endian-signed ((value integer) (width (eql ,width)))
                    ((:optimize (= safety 0) (= speed 3)))
                  `(the (signed-byte ,',width)
                        (mezzano.compiler::call %%swap-endian-signed ,value ',',width)))
                (mezzano.compiler::define-transform %swap-endian-signed ((value integer) (width (eql ,width)))
                    ((:optimize (/= safety 0) (= speed 3)))
                  `(progn
                     ,(mezzano.compiler::insert-type-check value 'integer)
                     (the (signed-byte ,',width)
                          (mezzano.compiler::call %%swap-endian-signed ,value ',',width))))
                (mezzano.compiler::mark-as-constant-foldable '%swap-endian-unsigned)
                (mezzano.compiler::mark-as-constant-foldable '%%swap-endian-unsigned)
                (mezzano.compiler::mark-as-constant-foldable '%swap-endian-signed)
                (mezzano.compiler::mark-as-constant-foldable '%%swap-endian-signed))))
  (def 16)
  (def 32)
  (def 64))

;; Only exists to simplify things for DEFINE-TRANSFORM.
(declaim (inline swap-endian))
(defun swap-endian (value width &key signed)
  "Swap the endian of VALUE.
VALUE must be an integer and the result will be an unsigned-byte or signed-byte
of the specified width."
  (if signed
      (%swap-endian-signed value width)
      (%swap-endian-unsigned value width)))

(declaim (inline %nibble-fixup))
(defun %nibble-fixup (value width endian signed)
  (flet ((must-byteswap ()
           #+little-endian
           (eql endian :big)
           #-little-endian
           (eql endian :little)))
    (if (must-byteswap)
        (swap-endian value width :signed signed)
        value)))

(defmacro define-nibbles-accessor (name width endian signed fast-accessor)
  (flet ((maybe-sign-extend (inner)
           (if signed
               `(sign-extend ,inner ,width)
               inner)))
    (let ((value-type (list (if signed 'signed-byte 'unsigned-byte)
                            width)))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (mezzano.compiler::define-transform ,name ((vector (simple-array (unsigned-byte 8)) array-type) (index fixnum index-type))
               ((:optimize (= safety 0) (= speed 3)))
             `(progn
                ,(mezzano.compiler::insert-bounds-check vector array-type index index-type :adjust ,(1- (truncate width 8)))
                (the ,',value-type (mezzano.compiler::call %nibble-fixup (the ,',value-type (mezzano.compiler::call ,',fast-accessor ,vector ,index)) ',',width ',',endian ',',signed))))
           (mezzano.compiler::define-transform ,name ((vector (simple-array (unsigned-byte 8)) array-type) (index fixnum index-type))
               ((:optimize (/= safety 0) (= speed 3)))
             `(progn
                ,(mezzano.compiler::insert-type-check vector `(simple-array (unsigned-byte 8) (*)))
                ,(mezzano.compiler::insert-bounds-check vector array-type index index-type :adjust ,(1- (truncate width 8)))
                (the ,',value-type (mezzano.compiler::call %nibble-fixup (the ,',value-type (mezzano.compiler::call ,',fast-accessor ,vector ,index)) ',',width ',',endian ',',signed))))
           (mezzano.compiler::define-transform (setf ,name) (value (vector (simple-array (unsigned-byte 8)) array-type) (index fixnum index-type))
               ((:optimize (= safety 0) (= speed 3)))
             `(progn
                ,(mezzano.compiler::insert-bounds-check vector array-type index index-type :adjust ,(1- (truncate width 8)))
                (mezzano.compiler::call (setf ,',fast-accessor) (mezzano.compiler::call %nibble-fixup ,value ',',width ',',endian 'nil) ,vector ,index)
                ,value))
           (mezzano.compiler::define-transform (setf ,name) (value (vector (simple-array (unsigned-byte 8)) array-type) (index fixnum index-type))
               ((:optimize (/= safety 0) (= speed 3)))
             `(progn
                ,(mezzano.compiler::insert-type-check value ',value-type)
                ,(mezzano.compiler::insert-type-check vector `(simple-array (unsigned-byte 8) (*)))
                ,(mezzano.compiler::insert-bounds-check vector array-type index index-type :adjust ,(1- (truncate width 8)))
                (mezzano.compiler::call (setf ,',fast-accessor) (mezzano.compiler::call %nibble-fixup ,value ',',width ',',endian 'nil) ,vector ,index)
                ,value))
           (mezzano.compiler::mark-as-constant-foldable ',name)
           (mezzano.compiler::mark-as-constant-foldable '(setf ,name)))
         (defun ,name (vector index)
           (declare (optimize speed))
           (cond ((and (fixnump index)
                       (typep vector '(simple-array (unsigned-byte 8) (*))))
                  ;; Use the compiler transform for simple accesses.
                  (,name (the (simple-array (unsigned-byte 8) (*)) vector) (the fixnum index)))
                 (t
                  (check-type vector (array (unsigned-byte 8) (*)))
                  ,(maybe-sign-extend
                    `(logior ,@(ecase endian
                                 (:little
                                  (loop
                                     for i below (truncate width 8)
                                     collect `(ash (aref vector (+ index ,i)) ,(* i 8))))
                                 (:big
                                  (loop
                                     for i below (truncate width 8)
                                     collect `(ash (aref vector (+ index ,i)) ,(* (- (truncate width 8) i 1) 8))))))))))
         (defun (setf ,name) (value vector index)
           (declare (optimize speed))
           (cond ((and (fixnump index)
                       (typep vector '(simple-array (unsigned-byte 8) (*))))
                  ;; Use the compiler transform for simple accesses.
                  (setf (,name (the (simple-array (unsigned-byte 8) (*)) vector) (the fixnum index)) value))
                 (t
                  (check-type value ,value-type)
                  (check-type vector (array (unsigned-byte 8) (*)))
                  ,@(ecase endian
                      (:little
                       (loop
                          for i below (truncate width 8)
                          collect `(setf (aref vector (+ index ,i)) (ldb (byte 8 ,(* i 8)) value))))
                      (:big
                       (loop
                          for i below (truncate width 8)
                          collect `(setf (aref vector (+ index ,i)) (ldb (byte 8 ,(* (- (truncate width 8) i 1) 8)) value)))))
                  value)))))))

(define-nibbles-accessor ub16ref/be 16 :big nil %object-ref-unsigned-byte-16-unscaled)
(define-nibbles-accessor ub16ref/le 16 :little nil %object-ref-unsigned-byte-16-unscaled)
(define-nibbles-accessor ub32ref/be 32 :big nil %object-ref-unsigned-byte-32-unscaled)
(define-nibbles-accessor ub32ref/le 32 :little nil %object-ref-unsigned-byte-32-unscaled)
(define-nibbles-accessor ub64ref/be 64 :big nil %object-ref-unsigned-byte-64-unscaled)
(define-nibbles-accessor ub64ref/le 64 :little nil %object-ref-unsigned-byte-64-unscaled)
(define-nibbles-accessor sb16ref/be 16 :big t %object-ref-signed-byte-16-unscaled)
(define-nibbles-accessor sb16ref/le 16 :little t %object-ref-signed-byte-16-unscaled)
(define-nibbles-accessor sb32ref/be 32 :big t %object-ref-signed-byte-32-unscaled)
(define-nibbles-accessor sb32ref/le 32 :little t %object-ref-signed-byte-32-unscaled)
(define-nibbles-accessor sb64ref/be 64 :big t %object-ref-signed-byte-64-unscaled)
(define-nibbles-accessor sb64ref/le 64 :little t %object-ref-signed-byte-64-unscaled)
