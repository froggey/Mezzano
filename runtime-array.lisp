(in-package #:sys.int)

;;; (type tag size-in-bits 16-byte-aligned-p)
(defvar *array-info*
  '((bit 3 1 nil)
    ((unsigned-byte 2) 4 2 nil)
    ((unsigned-byte 4) 5 4 nil)
    ((unsigned-byte 8) 6 8 nil)
    ((unsigned-byte 16) 7 16 nil)
    ((unsigned-byte 32) 8 32 nil)
    ((unsigned-byte 64) 9 64 nil)
    ((signed-byte 1) 10 1 nil)
    ((signed-byte 2) 11 2 nil)
    ((signed-byte 4) 12 4 nil)
    ((signed-byte 8) 13 8 nil)
    ((signed-byte 16) 14 16 nil)
    ((signed-byte 32) 15 32 nil)
    ((signed-byte 64) 16 64 nil)
    (base-char 1 8 nil)
    (character 2 32 nil)
    (single-float 17 32 t)
    (double-float 18 64 t)
    (long-float 19 128 t)
    (xmm-vector 20 128 t)
    ((complex single-float) 21 64 t)
    ((complex double-float) 22 128 t)
    ((complex long-float) 23 256 t)
    (t 0 64 nil)))

(defun %allocate-and-clear-array (length real-element-type)
  (let* ((info (assoc real-element-type *array-info* :test 'equal))
         (total-size (+ (if (fourth info) 64 0) ; padding for alignment.
                        (* length (third info)))))
    ;; Align on a word boundary.
    (unless (zerop (rem total-size 64))
      (incf total-size (- 64 (rem total-size 64))))
    (%allocate-array-like (second info) (truncate total-size 64) length)))

(defun %allocate-and-fill-array (length real-element-type initial-element)
  (let ((array (%allocate-and-clear-array length real-element-type)))
    (dotimes (i length)
      (setf (aref array i) initial-element))
    array))

(defun make-simple-vector (length)
  "Allocate a SIMPLE-VECTOR with LENGTH elements.
Equivalent to (make-array length). Used by the compiler to
allocate environment frames."
  (%allocate-array-like 0 length length))

;;; FIXME: some parts must run with the GC off.
(defun %simple-array-aref (array index)
  (ecase (%simple-array-type array)
    (0 ;; simple-vector
     (svref array index))
    ((1 2) ;; simple-base-string or simple-string
     (schar array index))
    (6 ; ub8
     (memref-unsigned-byte-8 (+ (logand (lisp-object-address array) -16) 8)
                             index))
    (7 ; ub16
     (memref-unsigned-byte-16 (+ (logand (lisp-object-address array) -16) 8)
                              index))
    (8 ; ub32
     (memref-unsigned-byte-32 (+ (logand (lisp-object-address array) -16) 8)
                              index))
    (9 ; ub64
     (memref-unsigned-byte-64 (+ (logand (lisp-object-address array) -16) 8)
                              index))))

(defun (setf %simple-array-aref) (value array index)
  (ecase (%simple-array-type array)
    (0 ;; simple-vector
     (setf (svref array index) value))
    ((1 2) ;; simple-base-string or simple-string
     (setf (schar array index) value))
    (6 ; ub8
     (setf (memref-unsigned-byte-8 (+ (logand (lisp-object-address array) -16) 8)
                                   index)
           value))
    (7 ; ub16
     (setf (memref-unsigned-byte-16 (+ (logand (lisp-object-address array) -16) 8)
                                    index)
           value))
    (8 ; ub32
     (setf (memref-unsigned-byte-32 (+ (logand (lisp-object-address array) -16) 8)
                                    index)
           value))
    (0 ; ub64
     (setf (memref-unsigned-byte-64 (+ (logand (lisp-object-address array) -16) 8)
                                    index)
           value))))

(defun %memory-aref (type address index)
  (cond
    ((equal type '(unsigned-byte 8))
     (memref-unsigned-byte-8 address index))
    ((equal type '(unsigned-byte 16))
     (memref-unsigned-byte-16 address index))
    ((equal type '(unsigned-byte 32))
     (memref-unsigned-byte-32 address index))
    ((equal type '(unsigned-byte 64))
     (memref-unsigned-byte-64 address index))
    (t (error "TODO: %MEMORY-AREF ~S." type))))

(defun (setf %memory-aref) (value type address index)
  (cond
    ((equal type '(unsigned-byte 8))
     (setf (memref-unsigned-byte-8 address index) value))
    ((equal type '(unsigned-byte 16))
     (setf (memref-unsigned-byte-16 address index) value))
    ((equal type '(unsigned-byte 32))
     (setf (memref-unsigned-byte-32 address index) value))
    ((equal type '(unsigned-byte 64))
     (setf (memref-unsigned-byte-64 address index) value))
    (t (error "TODO: SETF %MEMORY-AREF ~S." type))))

(defparameter *array-types*
  #(t
    base-char
    character
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
    long-float
    xmm-vector
    (complex single-float)
    (complex double-float)
    (complex long-float)))

(defun %simple-array-element-type (array)
  (svref *array-types* (%simple-array-type array)))

(defun simple-string-p (object)
  (when (%simple-array-p object)
    (let ((tag (%simple-array-type object)))
      (or (eql tag 1) (eql tag 2)))))

;;; (destination source count)
(define-lap-function %fast-copy ()
  (sys.lap-x86:mov64 :rdi :r8)
  (sys.lap-x86:mov64 :rsi :r9)
  (sys.lap-x86:mov64 :rcx :r10)
  (sys.lap-x86:sar64 :rdi 3)
  (sys.lap-x86:sar64 :rsi 3)
  (sys.lap-x86:sar64 :rcx 3)
  (sys.lap-x86:rep)
  (sys.lap-x86:movs8)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun %bitblt (nrows ncols from-array from-row from-col to-array to-row to-col)
  (let* ((to-storage (%array-header-storage to-array))
	 (from-storage (%array-header-storage from-array))
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
    (unless (integerp to-storage)
      (setf to-storage (+ (lisp-object-address to-storage) 1)))
    (unless (integerp from-storage)
      (setf from-storage (+ (lisp-object-address from-storage) 1)))
    (incf to-storage (* (+ (* to-row to-width) to-col) stride))
    (incf from-storage (* (+ (* from-row from-width) from-col) stride))
    (dotimes (i nrows)
      (%fast-copy to-storage from-storage bytes-per-col)
      (incf to-storage to-stride)
      (incf from-storage from-stride))))

(defun %bitset (nrows ncols val to-array to-row to-col)
  (let* ((to-storage (%array-header-storage to-array))
	 (to-width (array-dimension to-array 1))
	 (to-offset (+ (* to-row to-width) to-col))
	 (type (array-element-type to-array)))
    (unless (fixnump to-storage)
      (setf to-storage (+ (lisp-object-address to-storage) 1)))
    (cond
      ((equal type '(unsigned-byte 8))
       (incf to-storage to-offset)
       (dotimes (i nrows)
	 (dotimes (j ncols)
           (setf (memref-unsigned-byte-8 to-storage j) val))
	 (incf to-storage to-width)))
      ((equal type '(unsigned-byte 16))
       (incf to-storage (* to-offset 2))
       (dotimes (i nrows)
	 (dotimes (j ncols)
           (setf (memref-unsigned-byte-16 to-storage j) val))
	 (incf to-storage (* to-width 2))))
      ((equal type '(unsigned-byte 32))
       (incf to-storage (* to-offset 4))
       (dotimes (i nrows)
	 (dotimes (j ncols)
           (setf (memref-unsigned-byte-32 to-storage j) val))
	 (incf to-storage (* to-width 4))))
      ((equal type '(unsigned-byte 64))
       (incf to-storage (* to-offset 8))
       (dotimes (i nrows)
	 (dotimes (j ncols)
           (setf (memref-unsigned-byte-64 to-storage j) val))
	 (incf to-storage (* to-width 8)))))))
