;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

;; Information about the various specialized arrays.
;; A list of (type tag size-in-bits 16-byte-aligned-p) lists.
;; This must be sorted from most-specific type to least-specific type.
(defvar *array-info*)
;; A simple-vector mapping simple 1D array object tags to their element type.
(defvar *array-types*)

(defun cold-array-initialization ()
  "Called during cold-load to initialize array support variables."
  (setf *array-info*
        '((bit                    #.+object-tag-array-bit+                    1 nil)
          ((unsigned-byte 2)      #.+object-tag-array-unsigned-byte-2+        2 nil)
          ((unsigned-byte 4)      #.+object-tag-array-unsigned-byte-4+        4 nil)
          ((unsigned-byte 8)      #.+object-tag-array-unsigned-byte-8+        8 nil)
          ((unsigned-byte 16)     #.+object-tag-array-unsigned-byte-16+      16 nil)
          ((unsigned-byte 32)     #.+object-tag-array-unsigned-byte-32+      32 nil)
          ((unsigned-byte 64)     #.+object-tag-array-unsigned-byte-64+      64 nil)
          ((signed-byte 1)        #.+object-tag-array-signed-byte-1+          1 nil)
          ((signed-byte 2)        #.+object-tag-array-signed-byte-2+          2 nil)
          ((signed-byte 4)        #.+object-tag-array-signed-byte-4+          4 nil)
          ((signed-byte 8)        #.+object-tag-array-signed-byte-8+          8 nil)
          ((signed-byte 16)       #.+object-tag-array-signed-byte-16+        16 nil)
          ((signed-byte 32)       #.+object-tag-array-signed-byte-32+        32 nil)
          (fixnum                 #.+object-tag-array-fixnum+                64 nil)
          ((signed-byte 64)       #.+object-tag-array-signed-byte-64+        64 nil)
          (single-float           #.+object-tag-array-single-float+          32 t)
          (double-float           #.+object-tag-array-double-float+          64 t)
          (short-float            #.+object-tag-array-short-float+           16 t)
          (long-float             #.+object-tag-array-long-float+           128 t)
          (xmm-vector             #.+object-tag-array-xmm-vector+           128 t)
          ((complex single-float) #.+object-tag-array-complex-single-float+  64 t)
          ((complex double-float) #.+object-tag-array-complex-double-float+ 128 t)
          ((complex short-float)  #.+object-tag-array-complex-short-float+   32 t)
          ((complex long-float)   #.+object-tag-array-complex-long-float+   256 t)
          (t                      #.+object-tag-array-t+                     64 nil)))
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

(defun make-simple-array-1 (length real-element-type area)
  (let* ((info (assoc real-element-type *array-info* :test 'equal))
         (total-size (+ (if (fourth info) 64 0) ; padding for alignment.
                        (* length (third info)))))
    ;; Align on a word boundary.
    (unless (zerop (rem total-size 64))
      (incf total-size (- 64 (rem total-size 64))))
    (%allocate-object (second info) (truncate total-size 64) length area)))

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
       (ldb (byte 2 bit)
            (%object-ref-unsigned-byte-8 array offset))))
    (#.+object-tag-array-unsigned-byte-4+
     (multiple-value-bind (offset bit)
         (truncate index 2)
       (ldb (byte 4 bit)
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
       (sign-extend (ldb (byte 2 bit)
                         (%object-ref-unsigned-byte-8 array offset))
                    2)))
    (#.+object-tag-array-signed-byte-4+
     (multiple-value-bind (offset bit)
         (truncate index 2)
       (sign-extend (ldb (byte 4 bit)
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
     (%integer-as-single-float (%object-ref-unsigned-byte-32 array index)))))

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
       (setf (ldb (byte 2 bit)
                  (%object-ref-unsigned-byte-8 array offset))
             value)))
    (#.+object-tag-array-unsigned-byte-4+
     (check-type value (unsigned-byte 4))
     (multiple-value-bind (offset bit)
         (truncate index 2)
       (setf (ldb (byte 4 bit)
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
                  (ldb (byte 1 0)
                       (%object-ref-unsigned-byte-8 array offset)))
             value)))
    (#.+object-tag-array-signed-byte-2+
     (check-type value (signed-byte 2))
     (multiple-value-bind (offset bit)
         (truncate index 4)
       (setf (ldb (byte 2 bit)
                  (ldb (byte 2 0)
                       (%object-ref-unsigned-byte-8 array offset)))
             value)))
    (#.+object-tag-array-signed-byte-4+
     (check-type value (signed-byte 4))
     (multiple-value-bind (offset bit)
         (truncate index 2)
       (setf (ldb (byte 4 bit)
                  (ldb (byte 4 0)
                       (%object-ref-unsigned-byte-8 array offset)))
             value)))
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
           (%single-float-as-integer value)))))

(defun %simple-array-element-type (array)
  (svref *array-types* (%object-tag array)))

;;; (destination source count)
(define-lap-function %fast-copy ()
  (sys.lap-x86:mov64 :rdi :r8)
  (sys.lap-x86:mov64 :rsi :r9)
  (sys.lap-x86:mov64 :rdx :r10)
  (sys.lap-x86:sar64 :rdi #.+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rsi #.+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rdx #.+n-fixnum-bits+)
  (sys.lap-x86:mov64 :rcx :rdx)
  (sys.lap-x86:sar64 :rcx 3)
  (sys.lap-x86:rep)
  (sys.lap-x86:movs64)
  (sys.lap-x86:mov32 :ecx :edx)
  (sys.lap-x86:and32 :ecx 7)
  (sys.lap-x86:rep)
  (sys.lap-x86:movs8)
  (sys.lap-x86:ret))

;; (to-storage from-storage bytes-per-col to-stride from-stride nrows)
(define-lap-function %%bitblt ()
  (sys.lap-x86:mov64 :rdi :r8) ; to-storage
  (sys.lap-x86:mov64 :rsi :r9) ; from-storage
  (sys.lap-x86:sar64 :rsi #.+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rdi #.+n-fixnum-bits+)
  (sys.lap-x86:mov64 :r8 (:rsp 8)) ; nrows
  (sys.lap-x86:sub64 :r11 :r10)
  (sys.lap-x86:sub64 :r12 :r10)
  (sys.lap-x86:jmp loop-test)
  loop-head
  (sys.lap-x86:mov64 :rcx :r10) ; bytes-per-col
  (sys.lap-x86:sar64 :rcx #.(+ 3 #.+n-fixnum-bits+))
  (sys.lap-x86:rep)
  (sys.lap-x86:movs64)
  (sys.lap-x86:mov64 :rcx :r10) ; bytes-per-col
  (sys.lap-x86:sar32 :ecx #.+n-fixnum-bits+)
  (sys.lap-x86:and32 :ecx 7)
  (sys.lap-x86:rep)
  (sys.lap-x86:movs8)
  (sys.lap-x86:mov64 :rax :r11) ; to-stride
  (sys.lap-x86:sar64 :rax #.+n-fixnum-bits+)
  (sys.lap-x86:add64 :rdi :rax)
  (sys.lap-x86:mov64 :rax :r12) ; from-stride
  (sys.lap-x86:sar64 :rax #.+n-fixnum-bits+)
  (sys.lap-x86:add64 :rsi :rax)
  loop-test
  (sys.lap-x86:sub64 :r8 #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:jge loop-head)
  (sys.lap-x86:mov32 :ecx #.(ash 0 +n-fixnum-bits+))
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:ret))

(defun %bitblt (nrows ncols from-array from-row from-col to-array to-row to-col)
  (mezzano.supervisor:with-pseudo-atomic
    (let ((to-offset 0)
          (from-offset 0))
      (when (integerp (%complex-array-info to-array))
        ;; Undisplace array.
        (setf to-offset (%complex-array-info to-array)
              to-array (%complex-array-storage to-array)))
      (when (integerp (%complex-array-info from-array))
        ;; Undisplace array.
        (setf from-offset (%complex-array-info from-array)
              from-array (%complex-array-storage from-array)))
      (let* ((to-storage (%complex-array-storage to-array))
             (from-storage (%complex-array-storage from-array))
             (to-width (array-dimension to-array 1))
             (from-width (array-dimension from-array 1))
             (type (array-element-type to-array))
             (stride (cond
                       ((equal type '(unsigned-byte 8)) 1)
                       ((equal type '(unsigned-byte 16)) 2)
                       ((equal type '(unsigned-byte 32)) 4)
                       ((equal type '(unsigned-byte 64)) 8)
                       (t (error "Unsupported array type."))))
             (to-stride (* to-width stride))
             (from-stride (* from-width stride))
             (bytes-per-col (* ncols stride)))
        (assert (equal (array-element-type from-array) type))
        ;; Objects to addresses.
        (setf to-storage (+ (lisp-object-address to-storage) (- +tag-object+) 8))
        (setf from-storage (+ (lisp-object-address from-storage) (- +tag-object+) 8))
        ;; Offset into arrays.
        (incf to-storage (* (+ (* to-row to-width) to-col to-offset) stride))
        (incf from-storage (* (+ (* from-row from-width) from-col from-offset) stride))
        (%%bitblt to-storage from-storage bytes-per-col to-stride from-stride nrows)))))

;;; (value destination count)
(define-lap-function %fast-set-8 ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:mov64 :rdi :r9)
  (sys.lap-x86:mov64 :rcx :r10)
  (sys.lap-x86:sar64 :rax #.+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rdi #.+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rcx #.+n-fixnum-bits+)
  (sys.lap-x86:rep)
  (sys.lap-x86:stos8)
  (sys.lap-x86:ret))

(define-lap-function %fast-set-16 ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:mov64 :rdi :r9)
  (sys.lap-x86:mov64 :rcx :r10)
  (sys.lap-x86:sar64 :rax #.+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rdi #.+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rcx #.+n-fixnum-bits+)
  (sys.lap-x86:rep)
  (sys.lap-x86:stos16)
  (sys.lap-x86:ret))

(define-lap-function %fast-set-32 ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:mov64 :rdi :r9)
  (sys.lap-x86:mov64 :rcx :r10)
  (sys.lap-x86:sar64 :rax #.+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rdi #.+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rcx #.+n-fixnum-bits+)
  (sys.lap-x86:rep)
  (sys.lap-x86:stos32)
  (sys.lap-x86:ret))

(define-lap-function %fast-set-64 ()
  (sys.lap-x86:test64 :r8 #.+fixnum-tag-mask+)
  (sys.lap-x86:jnz load-bignum)
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax #.+n-fixnum-bits+)
  blah
  (sys.lap-x86:mov64 :rdi :r9)
  (sys.lap-x86:mov64 :rcx :r10)
  (sys.lap-x86:sar64 :rdi #.+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rcx #.+n-fixnum-bits+)
  (sys.lap-x86:rep)
  (sys.lap-x86:stos64)
  (sys.lap-x86:ret)
  load-bignum
  (sys.lap-x86:mov64 :rax (:r8 #.(+ (- +tag-object+) 8)))
  (sys.lap-x86:jmp blah))

(defun %bitset (nrows ncols val to-array to-row to-col)
  (mezzano.supervisor:with-pseudo-atomic
    (let ((to-displacement 0))
      (when (integerp (%complex-array-info to-array))
        ;; Undisplace array.
        (setf to-displacement (%complex-array-info to-array)
              to-array (%complex-array-storage to-array)))
      (let* ((to-storage (%complex-array-storage to-array))
             (to-width (array-dimension to-array 1))
             (to-offset (+ (* to-row to-width) to-col))
             (type (array-element-type to-array)))
        ;; Object to address.
        (setf to-storage (+ (lisp-object-address to-storage) (- +tag-object+) 8))
        ;; Offset into array.
        (incf to-storage (* to-displacement 4))
        (cond
          ((equal type '(unsigned-byte 8))
           (check-type val (unsigned-byte 8))
           (incf to-storage to-offset)
           (dotimes (i nrows)
             (%fast-set-8 val to-storage ncols)
             (incf to-storage to-width)))
          ((equal type '(unsigned-byte 16))
           (check-type val (unsigned-byte 16))
           (incf to-storage (* to-offset 2))
           (dotimes (i nrows)
             (%fast-set-16 val to-storage ncols)
             (incf to-storage (* to-width 2))))
          ((equal type '(unsigned-byte 32))
           (check-type val (unsigned-byte 32))
           (incf to-storage (* to-offset 4))
           (dotimes (i nrows)
             (%fast-set-32 val to-storage ncols)
             (incf to-storage (* to-width 4))))
          ((equal type '(unsigned-byte 64))
           (incf to-storage (* to-offset 8))
           (dotimes (i nrows)
             (%fast-set-64 val to-storage ncols)
             (incf to-storage (* to-width 8)))))))))
