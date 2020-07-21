;;;; Runtime array functions

(in-package :mezzano.internals)

;; Required for the cross-compiler.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defstruct (specialized-array-definition
             (:type vector))
  type
  tag
  element-size
  pad-p
  zero-element)

)

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
  (setf *array-t-info* (make-specialized-array-definition :type 't
                                                          :tag +object-tag-array-t+
                                                          :element-size 64
                                                          :pad-p nil
                                                          :zero-element 0)
        *array-bit-info* (make-specialized-array-definition :type 'bit
                                                            :tag +object-tag-array-bit+
                                                            :element-size 1
                                                            :pad-p nil
                                                            :zero-element 0)
        *array-character-info* (make-specialized-array-definition :type 'character
                                                                  :tag nil
                                                                  :element-size nil
                                                                  :pad-p nil
                                                                  :zero-element #\Nul))
  (setf *array-info*
        (loop for (type tag element-size pad-p zero-element) in
             '((bit)
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
               ((complex single-float) #.+object-tag-array-complex-single-float+  64 t   #C(0.0f0 0.0f0))
               ((complex double-float) #.+object-tag-array-complex-double-float+ 128 t   #C(0.0d0 0.0d0))
               ((complex short-float)  #.+object-tag-array-complex-short-float+   32 t   #C(0.0s0 0.0s0))
               ((complex long-float)   #.+object-tag-array-complex-long-float+   256 t   #C(0.0l0 0.0l0))
               (character)
               (t))
           collect (case type
                     ((bit) *array-bit-info*)
                     ((character) *array-character-info*)
                     ((t) *array-t-info*)
                     (t (make-specialized-array-definition :type type
                                                           :tag tag
                                                           :element-size element-size
                                                           :pad-p pad-p
                                                           :zero-element zero-element)))))
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
          (complex long-float))))

;; Only for the benefit of the cross-compiler,
;; this is the very first thing INITIALIZE-LISP calls.
(cold-array-initialization)
)

(defun make-simple-array-1 (length info area)
  (let* ((total-size (+ (if (specialized-array-definition-pad-p info) 64 0) ; padding for alignment.
                        (* length (specialized-array-definition-element-size info)))))
    ;; Align on a word boundary.
    (unless (zerop (rem total-size 64))
      (incf total-size (- 64 (rem total-size 64))))
    (mezzano.runtime::%allocate-object (specialized-array-definition-tag info) length (truncate total-size 64) area)))

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
    (#.+object-tag-array-short-float+
     (%object-ref-short-float array index))
    (#.+object-tag-array-single-float+
     (%object-ref-single-float array index))
    (#.+object-tag-array-double-float+
     (%object-ref-double-float array index))
    (#.+object-tag-array-complex-short-float+
     (complex
      (%object-ref-short-float array (* index 2))
      (%object-ref-short-float array (1+ (* index 2)))))
    (#.+object-tag-array-complex-single-float+
     (complex
      (%object-ref-single-float array (* index 2))
      (%object-ref-single-float array (1+ (* index 2)))))
    (#.+object-tag-array-complex-double-float+
     (complex
      (%object-ref-double-float array (* index 2))
      (%object-ref-double-float array (1+ (* index 2)))))))

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
    (#.+object-tag-array-short-float+
     (setf (%object-ref-short-float array index)
           value))
    (#.+object-tag-array-single-float+
     (setf (%object-ref-single-float array index)
           value))
    (#.+object-tag-array-double-float+
     (setf (%object-ref-double-float array index)
           value))
    (#.+object-tag-array-complex-short-float+
     (setf (%object-ref-short-float array (* index 2)) (realpart value)
           (%object-ref-short-float array (1+ (* index 2))) (imagpart value)))
    (#.+object-tag-array-complex-single-float+
     (setf (%object-ref-single-float array (* index 2)) (realpart value)
           (%object-ref-single-float array (1+ (* index 2))) (imagpart value)))
    (#.+object-tag-array-complex-double-float+
     (setf (%object-ref-double-float array (* index 2)) (realpart value)
           (%object-ref-double-float array (1+ (* index 2))) (imagpart value))))
  value)

(defun (cas %simple-array-aref) (old new array index)
  (ecase (%object-tag array)
    (#.+object-tag-array-t+ ;; simple-vector
     (cas (%object-ref-t array index) old new))
    (#.+object-tag-array-fixnum+
     (check-type old fixnum)
     (check-type new fixnum)
     (cas (%object-ref-t array index) old new))
    ((#.+object-tag-array-bit+
      #.+object-tag-array-unsigned-byte-2+
      #.+object-tag-array-unsigned-byte-4+
      #.+object-tag-array-signed-byte-1+
      #.+object-tag-array-signed-byte-2+
      #.+object-tag-array-signed-byte-4+)
     ;; ### These could be supported with a RMW CAS loop, but it's not really worth it.
     (error "CAS not supported on sub-octet arrays"))
    (#.+object-tag-array-unsigned-byte-8+
     (cas (%object-ref-unsigned-byte-8 array index) old new))
    (#.+object-tag-array-unsigned-byte-16+
     (cas (%object-ref-unsigned-byte-16 array index) old new))
    (#.+object-tag-array-unsigned-byte-32+
     (cas (%object-ref-unsigned-byte-32 array index) old new))
    (#.+object-tag-array-unsigned-byte-64+
     (cas (%object-ref-unsigned-byte-64 array index) old new))
    (#.+object-tag-array-signed-byte-8+
     (cas (%object-ref-signed-byte-8 array index) old new))
    (#.+object-tag-array-signed-byte-16+
     (cas (%object-ref-signed-byte-16 array index) old new))
    (#.+object-tag-array-signed-byte-32+
     (cas (%object-ref-signed-byte-32 array index) old new))
    (#.+object-tag-array-signed-byte-64+
     (cas (%object-ref-signed-byte-64 array index) old new))
    (#.+object-tag-array-short-float+
     (cas (%object-ref-short-float array index) old new))
    (#.+object-tag-array-single-float+
     (cas (%object-ref-single-float array index) old new))
    (#.+object-tag-array-double-float+
     (cas (%object-ref-double-float array index) old new))
    ((#.+object-tag-array-complex-short-float+
      #.+object-tag-array-complex-single-float+
      #.+object-tag-array-complex-double-float+)
     ;; ### (complex single) needs to do a 64-bit CAS, (complex double) needs 128-bit.
     (error "CAS not supported on complex float arrays"))))

(defun %memory-array-aref (array index)
  (let ((address (%complex-array-storage array)))
    (when (typep address 'mezzano.supervisor:dma-buffer)
      (setf address (mezzano.supervisor:dma-buffer-virtual-address address)))
    (ecase (%complex-array-info array)
      ((#.+object-tag-array-t+
        #.+object-tag-array-fixnum+)
       (memref-t address index))
      (#.+object-tag-array-bit+
       (multiple-value-bind (offset bit)
           (truncate index 8)
         (ldb (byte 1 bit)
              (memref-unsigned-byte-8 address offset))))
      (#.+object-tag-array-unsigned-byte-2+
       (multiple-value-bind (offset bit)
           (truncate index 4)
         (ldb (byte 2 (* bit 2))
              (memref-unsigned-byte-8 address offset))))
      (#.+object-tag-array-unsigned-byte-4+
       (multiple-value-bind (offset bit)
           (truncate index 2)
         (ldb (byte 4 (* bit 4))
              (memref-unsigned-byte-8 address offset))))
      (#.+object-tag-array-unsigned-byte-8+
       (memref-unsigned-byte-8 address index))
      (#.+object-tag-array-unsigned-byte-16+
       (memref-unsigned-byte-16 address index))
      (#.+object-tag-array-unsigned-byte-32+
       (memref-unsigned-byte-32 address index))
      (#.+object-tag-array-unsigned-byte-64+
       (memref-unsigned-byte-64 address index))
      (#.+object-tag-array-signed-byte-1+
       (multiple-value-bind (offset bit)
           (truncate index 8)
         (sign-extend (ldb (byte 1 bit)
                           (memref-unsigned-byte-8 address offset))
                      1)))
      (#.+object-tag-array-signed-byte-2+
       (multiple-value-bind (offset bit)
           (truncate index 4)
         (sign-extend (ldb (byte 2 (* bit 2))
                           (memref-unsigned-byte-8 address offset))
                      2)))
      (#.+object-tag-array-signed-byte-4+
       (multiple-value-bind (offset bit)
           (truncate index 2)
         (sign-extend (ldb (byte 4 (* bit 4))
                           (memref-unsigned-byte-8 address offset))
                      4)))
      (#.+object-tag-array-signed-byte-8+
       (memref-signed-byte-8 address index))
      (#.+object-tag-array-signed-byte-16+
       (memref-signed-byte-16 address index))
      (#.+object-tag-array-signed-byte-32+
       (memref-signed-byte-32 address index))
      (#.+object-tag-array-signed-byte-64+
       (memref-signed-byte-64 address index))
      (#.+object-tag-array-short-float+
       (%integer-as-short-float
        (memref-unsigned-byte-16 index)))
      (#.+object-tag-array-single-float+
       (memref-single-float address index))
      (#.+object-tag-array-double-float+
       (memref-double-float address index))
      (#.+object-tag-array-complex-short-float+
       (complex
        (%integer-as-short-float
         (memref-unsigned-byte-16 address (* index 2)))
        (%integer-as-short-float
         (memref-unsigned-byte-16 address (1+ (* index 2))))))
      (#.+object-tag-array-complex-single-float+
       (complex
        (memref-single-float address (* index 2))
        (memref-single-float address (1+ (* index 2)))))
      (#.+object-tag-array-complex-double-float+
       (complex
        (memref-double-float address (* index 2))
        (memref-double-float address (1+ (* index 2))))))))

(defun (setf %memory-array-aref) (value array index)
  (let ((address (%complex-array-storage array)))
    (when (typep address 'mezzano.supervisor:dma-buffer)
      (setf address (mezzano.supervisor:dma-buffer-virtual-address address)))
    (ecase (%complex-array-info array)
      (#.+object-tag-array-t+ ;; simple-vector
       (setf (memref-t address index) value))
      (#.+object-tag-array-fixnum+
       (check-type value fixnum)
       (setf (memref-t address index) value))
      (#.+object-tag-array-bit+
       (check-type value bit)
       (multiple-value-bind (offset bit)
           (truncate index 8)
         (setf (ldb (byte 1 bit)
                    (memref-unsigned-byte-8 address offset))
               value)))
      (#.+object-tag-array-unsigned-byte-2+
       (check-type value (unsigned-byte 2))
       (multiple-value-bind (offset bit)
           (truncate index 4)
         (setf (ldb (byte 2 (* bit 2))
                    (memref-unsigned-byte-8 address offset))
               value)))
      (#.+object-tag-array-unsigned-byte-4+
       (check-type value (unsigned-byte 4))
       (multiple-value-bind (offset bit)
           (truncate index 2)
         (setf (ldb (byte 4 (* bit 4))
                    (memref-unsigned-byte-8 address offset))
               value)))
      (#.+object-tag-array-unsigned-byte-8+
       (setf (memref-unsigned-byte-8 address index)
             value))
      (#.+object-tag-array-unsigned-byte-16+
       (setf (memref-unsigned-byte-16 address index)
             value))
      (#.+object-tag-array-unsigned-byte-32+
       (setf (memref-unsigned-byte-32 address index)
             value))
      (#.+object-tag-array-unsigned-byte-64+
       (setf (memref-unsigned-byte-64 address index)
             value))
      (#.+object-tag-array-signed-byte-1+
       (check-type value (signed-byte 1))
       (multiple-value-bind (offset bit)
           (truncate index 8)
         (setf (ldb (byte 1 bit)
                    (memref-unsigned-byte-8 address offset))
               (ldb (byte 1 0) value))))
      (#.+object-tag-array-signed-byte-2+
       (check-type value (signed-byte 2))
       (multiple-value-bind (offset bit)
           (truncate index 4)
         (setf (ldb (byte 2 (* bit 2))
                    (memref-unsigned-byte-8 address offset))
               (ldb (byte 2 0) value))))
      (#.+object-tag-array-signed-byte-4+
       (check-type value (signed-byte 4))
       (multiple-value-bind (offset bit)
           (truncate index 2)
         (setf (ldb (byte 4 (* bit 4))
                    (memref-unsigned-byte-8 address offset))
               (ldb (byte 4 0) value))))
      (#.+object-tag-array-signed-byte-8+
       (setf (memref-signed-byte-8 address index)
             value))
      (#.+object-tag-array-signed-byte-16+
       (setf (memref-signed-byte-16 address index)
             value))
      (#.+object-tag-array-signed-byte-32+
       (setf (memref-signed-byte-32 address index)
             value))
      (#.+object-tag-array-signed-byte-64+
       (setf (memref-signed-byte-64 address index)
             value))
      (#.+object-tag-array-short-float+
       (check-type value short-float)
       (setf (memref-unsigned-byte-16 array index)
             (%short-float-as-integer value)))
      (#.+object-tag-array-single-float+
       (setf (memref-single-float address index)
             value))
      (#.+object-tag-array-double-float+
       (setf (memref-double-float address index)
             value))
      (#.+object-tag-array-complex-short-float+
       (check-type value (complex short-float))
       (let ((realpart (%short-float-as-integer (realpart value)))
             (imagpart (%short-float-as-integer (imagpart value))))
         (setf (memref-unsigned-byte-16 array (* index 2)) realpart
               (memref-unsigned-byte-16 array (1+ (* index 2))) imagpart)))
      (#.+object-tag-array-complex-single-float+
       (setf (memref-single-float address (* index 2)) (realpart value)
             (memref-single-float address (1+ (* index 2))) (imagpart value)))
      (#.+object-tag-array-complex-double-float+
       (setf (memref-double-float address (* index 2)) (realpart value)
             (memref-double-float address (1+ (* index 2))) (imagpart value)))))
  value)

(defun (cas %memory-array-aref) (old new array index)
  (let ((address (%complex-array-storage array)))
    (when (typep address 'mezzano.supervisor:dma-buffer)
      (setf address (mezzano.supervisor:dma-buffer-virtual-address address)))
    (ecase (%complex-array-info array)
      (#.+object-tag-array-t+ ;; simple-vector
       (cas (memref-t address index) old new))
      (#.+object-tag-array-fixnum+
       (check-type old fixnum)
       (check-type new fixnum)
       (cas (memref-t address index) old new))
      ((#.+object-tag-array-bit+
        #.+object-tag-array-unsigned-byte-2+
        #.+object-tag-array-unsigned-byte-4+
        #.+object-tag-array-signed-byte-1+
        #.+object-tag-array-signed-byte-2+
        #.+object-tag-array-signed-byte-4+)
       ;; ### These could be supported with a RMW CAS loop, but it's not really worth it.
       (error "CAS not supported on sub-octet arrays"))
      (#.+object-tag-array-unsigned-byte-8+
       (cas (memref-unsigned-byte-8 address index) old new))
      (#.+object-tag-array-unsigned-byte-16+
       (cas (memref-unsigned-byte-16 address index) old new))
      (#.+object-tag-array-unsigned-byte-32+
       (cas (memref-unsigned-byte-32 address index) old new))
      (#.+object-tag-array-unsigned-byte-64+
       (cas (memref-unsigned-byte-64 address index) old new))
      (#.+object-tag-array-signed-byte-8+
       (cas (memref-signed-byte-8 address index) old new))
      (#.+object-tag-array-signed-byte-16+
       (cas (memref-signed-byte-16 address index) old new))
      (#.+object-tag-array-signed-byte-32+
       (cas (memref-signed-byte-32 address index) old new))
      (#.+object-tag-array-signed-byte-64+
       (cas (memref-signed-byte-64 address index) old new))
      (#.+object-tag-array-short-float+
       (check-type old short-float)
       (check-type new short-float)
       (%integer-as-short-float
        (cas (memref-unsigned-byte-16 array index)
             (%short-float-as-integer old)
             (%short-float-as-integer new))))
      (#.+object-tag-array-single-float+
       (cas (memref-single-float address index) old new))
      (#.+object-tag-array-double-float+
       (cas (memref-double-float address index) old new))
      ((#.+object-tag-array-complex-short-float+
        #.+object-tag-array-complex-single-float+
        #.+object-tag-array-complex-double-float+)
       ;; ### (complex single) needs to do a 64-bit CAS, (complex double) needs 128-bit.
       (error "CAS not supported on complex float arrays")))))

(defun %simple-array-element-type (array)
  (svref *array-types* (%object-tag array)))

(defun %simple-array-info (array)
  (dolist (info *array-info*
           (error "~S isn't a simple array?" array))
    (when (%object-of-type-p array (specialized-array-definition-tag info))
      (return info))))
