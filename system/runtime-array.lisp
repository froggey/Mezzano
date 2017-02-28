;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

;; Required for the cross-compiler.

(eval-when (:compile-toplevel :load-toplevel :execute)
;; Information about the various specialized arrays.
;; A list of (type tag size-in-bits 16-byte-aligned-p zero-value) lists.
;; This must be sorted from most-specific type to least-specific type.
(defvar *array-info*)
;; Info for T and BIT arrays.
(defvar *array-t-info*)
(defvar *array-bit-info*)
(defvar *array-character-info*)
;; A simple-vector mapping simple 1D array object tags to their element type.
(defvar *array-types*)

(defun cold-array-initialization ()
  "Called during cold-load to initialize array support variables."
  (setf *array-t-info* '(t #.+object-tag-array-t+ 64 nil 0)
        *array-bit-info* '(bit #.+object-tag-array-bit+ 1 nil 0)
        *array-character-info* '(character nil nil nil #\Nul))
  (setf *array-info*
        '((bit                    #.+object-tag-array-bit+                    1 nil 0)
          ((unsigned-byte 2)      #.+object-tag-array-unsigned-byte-2+        2 nil 0)
          ((unsigned-byte 4)      #.+object-tag-array-unsigned-byte-4+        4 nil 0)
          ((unsigned-byte 8)      #.+object-tag-array-unsigned-byte-8+        8 nil 0)
          ((unsigned-byte 16)     #.+object-tag-array-unsigned-byte-16+      16 nil 0)
          ((unsigned-byte 32)     #.+object-tag-array-unsigned-byte-32+      32 nil 0)
          ((unsigned-byte 64)     #.+object-tag-array-unsigned-byte-64+      64 nil 0)
          ((signed-byte 1)        #.+object-tag-array-signed-byte-1+          1 nil 0)
          ((signed-byte 2)        #.+object-tag-array-signed-byte-2+          2 nil 0)
          ((signed-byte 4)        #.+object-tag-array-signed-byte-4+          4 nil 0)
          ((signed-byte 8)        #.+object-tag-array-signed-byte-8+          8 nil 0)
          ((signed-byte 16)       #.+object-tag-array-signed-byte-16+        16 nil 0)
          ((signed-byte 32)       #.+object-tag-array-signed-byte-32+        32 nil 0)
          (fixnum                 #.+object-tag-array-fixnum+                64 nil 0)
          ((signed-byte 64)       #.+object-tag-array-signed-byte-64+        64 nil 0)
          (single-float           #.+object-tag-array-single-float+          32 t   0.0f0)
          (double-float           #.+object-tag-array-double-float+          64 t   0.0d0)
          (short-float            #.+object-tag-array-short-float+           16 t   0.0s0)
          (long-float             #.+object-tag-array-long-float+           128 t   0.0l0)
          (xmm-vector             #.+object-tag-array-xmm-vector+           128 t   0)
          ((complex single-float) #.+object-tag-array-complex-single-float+  64 t   #C(0.0f0 0.0f0))
          ((complex double-float) #.+object-tag-array-complex-double-float+ 128 t   #C(0.0d0 0.0d0))
          ((complex short-float)  #.+object-tag-array-complex-short-float+   32 t   #C(0.0s0 0.0s0))
          ((complex long-float)   #.+object-tag-array-complex-long-float+   256 t   #C(0.0l0 0.0l0))
          (character              nil                                       nil nil #\Nul)
          (t                      #.+object-tag-array-t+                     64 nil 0)))
  (setf *array-types*
        #(t
          fixnum
          bit
          (unsigned-byte 2)
          (unsigned-byte 4)
          (unsigned-byte 8)
          (unsigned-byte 16)
          (unsigned-byte 32)
          (unsigned-byte 64)
          (signed-byte 1)
          (signed-byte 2)
          (signed-byte 4)
          (signed-byte 8)
          (signed-byte 16)
          (signed-byte 32)
          (signed-byte 64)
          single-float
          double-float
          short-float
          long-float
          (complex single-float)
          (complex double-float)
          (complex short-float)
          (complex long-float)
          xmm-vector)))

;; Only for the benefit of the cross-compiler,
;; this is the very first thing INITIALIZE-LISP calls.
(cold-array-initialization)

)

(defun make-simple-array-1 (length info area)
  (let* ((total-size (+ (if (fourth info) 64 0) ; padding for alignment.
                        (* length (third info)))))
    ;; Align on a word boundary.
    (unless (zerop (rem total-size 64))
      (incf total-size (- 64 (rem total-size 64))))
    (mezzano.runtime::%allocate-object (second info) length (truncate total-size 64) area)))

(defun sign-extend (value width)
  "Convert an unsigned integer to a signed value."
  (if (logbitp (1- width) value)
      (logior value (lognot (1- (ash 1 width))))
      value))

(defun %simple-array-aref (array index)
  (ecase (%object-tag array)
    ((#.+object-tag-array-t+
      #.+object-tag-array-fixnum+)
     (%object-ref-t array index))
    (#.+object-tag-array-bit+
     (multiple-value-bind (offset bit)
         (truncate index 8)
       (ldb (byte 1 bit)
            (%object-ref-unsigned-byte-8 array offset))))
    (#.+object-tag-array-unsigned-byte-2+
     (multiple-value-bind (offset bit)
         (truncate index 4)
       (ldb (byte 2 (* bit 2))
            (%object-ref-unsigned-byte-8 array offset))))
    (#.+object-tag-array-unsigned-byte-4+
     (multiple-value-bind (offset bit)
         (truncate index 2)
       (ldb (byte 4 (* bit 4))
            (%object-ref-unsigned-byte-8 array offset))))
    (#.+object-tag-array-unsigned-byte-8+
     (%object-ref-unsigned-byte-8 array index))
    (#.+object-tag-array-unsigned-byte-16+
     (%object-ref-unsigned-byte-16 array index))
    (#.+object-tag-array-unsigned-byte-32+
     (%object-ref-unsigned-byte-32 array index))
    (#.+object-tag-array-unsigned-byte-64+
     (%object-ref-unsigned-byte-64 array index))
    (#.+object-tag-array-signed-byte-1+
     (multiple-value-bind (offset bit)
         (truncate index 8)
       (sign-extend (ldb (byte 1 bit)
                         (%object-ref-unsigned-byte-8 array offset))
                    1)))
    (#.+object-tag-array-signed-byte-2+
     (multiple-value-bind (offset bit)
         (truncate index 4)
       (sign-extend (ldb (byte 2 (* bit 2))
                         (%object-ref-unsigned-byte-8 array offset))
                    2)))
    (#.+object-tag-array-signed-byte-4+
     (multiple-value-bind (offset bit)
         (truncate index 2)
       (sign-extend (ldb (byte 4 (* bit 4))
                         (%object-ref-unsigned-byte-8 array offset))
                    4)))
    (#.+object-tag-array-signed-byte-8+
     (%object-ref-signed-byte-8 array index))
    (#.+object-tag-array-signed-byte-16+
     (%object-ref-signed-byte-16 array index))
    (#.+object-tag-array-signed-byte-32+
     (%object-ref-signed-byte-32 array index))
    (#.+object-tag-array-signed-byte-64+
     (%object-ref-signed-byte-64 array index))
    (#.+object-tag-array-single-float+
     (%integer-as-single-float (%object-ref-unsigned-byte-32 array index)))
    (#.+object-tag-array-double-float+
     (%integer-as-double-float (%object-ref-unsigned-byte-64 array index)))
    (#.+object-tag-array-complex-single-float+
     (complex
      (%integer-as-single-float (%object-ref-unsigned-byte-32 array (* index 2)))
      (%integer-as-single-float (%object-ref-unsigned-byte-32 array (1+ (* index 2))))))
    (#.+object-tag-array-complex-double-float+
     (complex
      (%integer-as-double-float (%object-ref-unsigned-byte-64 array (* index 2)))
      (%integer-as-double-float (%object-ref-unsigned-byte-64 array (1+ (* index 2))))))))

(defun (setf %simple-array-aref) (value array index)
  (ecase (%object-tag array)
    (#.+object-tag-array-t+ ;; simple-vector
     (setf (%object-ref-t array index) value))
    (#.+object-tag-array-fixnum+
     (check-type value fixnum)
     (setf (%object-ref-t array index) value))
    (#.+object-tag-array-bit+
     (check-type value bit)
     (multiple-value-bind (offset bit)
         (truncate index 8)
       (setf (ldb (byte 1 bit)
                  (%object-ref-unsigned-byte-8 array offset))
             value)))
    (#.+object-tag-array-unsigned-byte-2+
     (check-type value (unsigned-byte 2))
     (multiple-value-bind (offset bit)
         (truncate index 4)
       (setf (ldb (byte 2 (* bit 2))
                  (%object-ref-unsigned-byte-8 array offset))
             value)))
    (#.+object-tag-array-unsigned-byte-4+
     (check-type value (unsigned-byte 4))
     (multiple-value-bind (offset bit)
         (truncate index 2)
       (setf (ldb (byte 4 (* bit 4))
                  (%object-ref-unsigned-byte-8 array offset))
             value)))
    (#.+object-tag-array-unsigned-byte-8+
     (setf (%object-ref-unsigned-byte-8 array index)
           value))
    (#.+object-tag-array-unsigned-byte-16+
     (setf (%object-ref-unsigned-byte-16 array index)
           value))
    (#.+object-tag-array-unsigned-byte-32+
     (setf (%object-ref-unsigned-byte-32 array index)
           value))
    (#.+object-tag-array-unsigned-byte-64+
     (setf (%object-ref-unsigned-byte-64 array index)
           value))
    (#.+object-tag-array-signed-byte-1+
     (check-type value (signed-byte 1))
     (multiple-value-bind (offset bit)
         (truncate index 8)
       (setf (ldb (byte 1 bit)
                       (%object-ref-unsigned-byte-8 array offset))
             (ldb (byte 1 0) value))))
    (#.+object-tag-array-signed-byte-2+
     (check-type value (signed-byte 2))
     (multiple-value-bind (offset bit)
         (truncate index 4)
       (setf (ldb (byte 2 (* bit 2))
                       (%object-ref-unsigned-byte-8 array offset))
             (ldb (byte 2 0) value))))
    (#.+object-tag-array-signed-byte-4+
     (check-type value (signed-byte 4))
     (multiple-value-bind (offset bit)
         (truncate index 2)
       (setf (ldb (byte 4 (* bit 4))
                  (%object-ref-unsigned-byte-8 array offset))
             (ldb (byte 4 0) value))))
    (#.+object-tag-array-signed-byte-8+
     (setf (%object-ref-signed-byte-8 array index)
           value))
    (#.+object-tag-array-signed-byte-16+
     (setf (%object-ref-signed-byte-16 array index)
           value))
    (#.+object-tag-array-signed-byte-32+
     (setf (%object-ref-signed-byte-32 array index)
           value))
    (#.+object-tag-array-signed-byte-64+
     (setf (%object-ref-signed-byte-64 array index)
           value))
    (#.+object-tag-array-single-float+
     (check-type value single-float)
     (setf (%object-ref-unsigned-byte-32 array index)
           (%single-float-as-integer value)))
    (#.+object-tag-array-double-float+
     (check-type value double-float)
     (setf (%object-ref-unsigned-byte-64 array index)
           (%double-float-as-integer value)))
    (#.+object-tag-array-complex-single-float+
     (check-type value (complex single-float))
     (let ((realpart (%single-float-as-integer (realpart value)))
           (imagpart (%single-float-as-integer (imagpart value))))
       (setf (%object-ref-unsigned-byte-32 array (* index 2)) realpart
             (%object-ref-unsigned-byte-32 array (1+ (* index 2))) imagpart)))
    (#.+object-tag-array-complex-double-float+
     (check-type value (complex double-float))
     (let ((realpart (%double-float-as-integer (realpart value)))
           (imagpart (%double-float-as-integer (imagpart value))))
       (setf (%object-ref-unsigned-byte-64 array (* index 2)) realpart
             (%object-ref-unsigned-byte-64 array (1+ (* index 2))) imagpart)))))

(defun %simple-array-element-type (array)
  (svref *array-types* (%object-tag array)))
