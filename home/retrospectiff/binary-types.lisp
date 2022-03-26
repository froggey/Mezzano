
(in-package :retrospectiff.binary-types)

;;; Perhaps the next few types should be moved to a
;;; binary-data-extensions file or some such?

(define-binary-type array (type size)
  (:reader (in)
           (let ((arr (make-array size :element-type type)))
             (dotimes (i size)
               (setf (elt arr i)
                     (read-value type in)))
             arr))
  (:writer (out value)
           (dotimes (i (length value))
             (write-value type out (elt value i)))))

(define-binary-type unsigned-integer* (bytes bits-per-byte)
  (:reader (in)
           (case *byte-order*
             (:big-endian
              (loop with value = 0
                 for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
                 (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
                 finally (return value)))
             (:little-endian
              (loop with value = 0
                 for low-bit to (* bits-per-byte (1- bytes)) by bits-per-byte do
                 (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
                 finally (return value)))))
  (:writer (out value)
           (case *byte-order*
             (:big-endian
              (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
                 do (write-byte (ldb (byte bits-per-byte low-bit) value) out)))
             (:little-endian
              (loop for low-bit to (* bits-per-byte (1- bytes)) by bits-per-byte
                 do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))))

(define-binary-type u2* () (unsigned-integer* :bytes 2 :bits-per-byte 8))
(define-binary-type u4* () (unsigned-integer* :bytes 4 :bits-per-byte 8))
(define-binary-type u8* () (unsigned-integer* :bytes 8 :bits-per-byte 8))

(defun convert-to-signed-integer (num bits)
  (let ((max (1- (ash 1 (1- bits)))))
    (if (> num max)
        (lognot (- (1- (ash 1 bits)) num))
        num)))

(defun convert-to-unsigned-integer (num bits)
  (if (minusp num)
      (+ (ash 1 bits) num)
      num))

(define-binary-type signed-integer* (bytes bits-per-byte)
  (:reader (in)
           (convert-to-signed-integer
            (case *byte-order*
              (:big-endian
               (loop with value = 0
                  for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
                    (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
                  finally (return value)))
              (:little-endian
               (loop with value = 0
                  for low-bit to (* bits-per-byte (1- bytes)) by bits-per-byte do
                    (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
                  finally (return value))))
            (* bytes bits-per-byte)))
  (:writer (out value)
           (let ((value (convert-to-unsigned-integer value (* bytes bits-per-byte))))
             (case *byte-order*
               (:big-endian
                (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
                   do (write-byte (ldb (byte bits-per-byte low-bit) value) out)))
               (:little-endian
                (loop for low-bit to  (* bits-per-byte (1- bytes)) by bits-per-byte
                   do (write-byte (ldb (byte bits-per-byte low-bit) value) out)))))))

(define-binary-type s1* () (signed-integer* :bytes 1 :bits-per-byte 8))
(define-binary-type s2* () (signed-integer* :bytes 2 :bits-per-byte 8))
(define-binary-type s4* () (signed-integer* :bytes 4 :bits-per-byte 8))

(define-binary-type f4* ()
  (:reader (in)
           (ieee-floats:decode-float32 (read-value 'u4* in)))
  (:writer (out value)
           (write-value 'u4* out (ieee-floats:encode-float32 value))))

(define-binary-type f8* ()
  (:reader (in)
           (ieee-floats:decode-float64 (read-value 'u8* in)))
  (:writer (out value)
           (write-value'u8* out (ieee-floats:encode-float64 value))))

;;; end binary-data-extensions section
;;;

(define-binary-type tiff-byte-order ()
  (:reader (in)
           (let ((val (read-value 'u2 in)))
             (case val
               (#x4949 (setf *byte-order* :little-endian))
               (#x4D4D (setf *byte-order* :big-endian))
               (t (error "unknown byte order")))))
  (:writer (out value)
           (case (or *byte-order* value)
             (:little-endian
              (write-value 'u2 out #x4949))
             (:big-endian
              (write-value 'u2 out #x4d4d)))))

(define-tagged-binary-class ifd-entry ()
  ((tag u2*)
   (field-type u2*)
   (value-count u4*))
  (:dispatch (case field-type
               (1 'byte-ifd-entry)
               (2 'ascii-ifd-entry)
               (3 'short-ifd-entry)
               (4 'long-ifd-entry)
               (5 'rational-ifd-entry)
               (6 'sbyte-ifd-entry)
               ;;
               ;; this next type (7) is really the UNKNOWN type, but
               ;; for now we read it as bytes
               (7 'byte-ifd-entry)
               (8 'sshort-ifd-entry)
               (9 'slong-ifd-entry)
               (10 'srational-ifd-entry)
               (11 'float-ifd-entry)
               (12 'double-ifd-entry)
               (t 'unknown-ifd-entry))))

(define-binary-class unknown-ifd-entry (ifd-entry)
  ((value-offset u4*)))

(defparameter *binary-type-sizes*
  `((iso-8859-1-char . 1)
    (u1 . 1)
    (u2 . 2)
    (u4 . 4)
    (u2* . 2)
    (u4* . 4)
    (s1* . 1)
    (s2* . 2)
    (s4* . 4)
    (rational . 8)
    (srational . 8)
    (f4* . 4)
    (f8* . 8)))

(define-binary-type ifd-array (type size element-type)
  (:reader (in)
           (let* ((bytes-per-element (cdr (assoc type *binary-type-sizes*))))
             (let ((pad (- (/ 4 bytes-per-element) size))
                   (v (apply #'make-array size
                             (when element-type `(:element-type ,element-type)))))
               (if (minusp pad)
                   (let ((position (read-value 'u4* in))
                         (cur (file-position in)))
                     (file-position in position)
                     (loop for i below size
                        do (setf (elt v i) (read-value type in)))
                     (file-position in cur))
                   (progn
                     (loop for i below size
                        do (setf (elt v i) (read-value type in)))
                     (loop for i below pad do (read-value type in))))
               v)))
  (:writer (out value)
           (let* ((bytes-per-element (cdr (assoc type *binary-type-sizes*))))
             (let ((pad (- (/ 4 bytes-per-element) size)))
               (if (minusp pad)
                   (let ((cur (file-position out))
                         (offset *tiff-file-offset*))
                     (progn (file-position out offset)
                            (loop for x across value
                               do (write-value type out x))
                            ;; need to make sure this is word aligned!
                            (setf *tiff-file-offset*
                                  (ash (ash (1+ (file-position out)) -1) 1))
                            (file-position out cur)
                            (write-value 'u4* out offset)))
                   (progn (loop for x across value
                             do (write-value type out x))
                          (loop for i below pad
                             do (write-value type out 0))))))))


(defgeneric bytes-per-entry (class))
(defgeneric entry-bytes (ifd-entry))

(defmethod entry-bytes ((entry ifd-entry))
  (* (value-count entry)
     (bytes-per-entry (class-of entry))))

;; 1 - byte
(define-binary-class byte-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'u1 :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'byte-ifd-entry)))) 1)

;; 2 - ascii
(define-binary-class ascii-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'iso-8859-1-char :size value-count :element-type 'character))))
(defmethod bytes-per-entry ((class (eql (find-class 'ascii-ifd-entry)))) 1)

;; 3 -- short
(define-binary-class short-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'u2* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'short-ifd-entry)))) 2)

;; 4 -- long
(define-binary-class long-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'u4* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'long-ifd-entry)))) 4)

;; 5 -- rational
(define-binary-class rational ()
  ((numerator u4*)
   (denominator u4*)))

(define-binary-class rational-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'rational :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'rational-ifd-entry)))) 8)

;; 6 -- signed byte
(define-binary-class sbyte-ifd-entry (ifd-entry)
  ((data (ifd-array :type 's1* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'sbyte-ifd-entry)))) 8)

;; 7 -- undefined (and unused, at least for now)
#+nil
(define-binary-class undefined-ifd-entry (ifd-entry)
  ((value-offset u4*)))
#+nil
(defmethod bytes-per-entry ((class (eql (find-class 'undefined-ifd-entry)))) 4)

;; 8 -- signed short
(define-binary-class sshort-ifd-entry (ifd-entry)
  ((data (ifd-array :type 's2* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'sshort-ifd-entry)))) 2)

;; 9 -- signed long
(define-binary-class slong-ifd-entry (ifd-entry)
  ((data (ifd-array :type 's4* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'slong-ifd-entry)))) 4)

;; 10 -- signed rational
(define-binary-class srational ()
  ((numerator s4*)
   (denominator s4*)))

(define-binary-class srational-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'srational :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'srational-ifd-entry)))) 8)

;; 11
(define-binary-class float-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'f4* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'float-ifd-entry)))) 4)

;; 12
(define-binary-class double-ifd-entry (ifd-entry)
  ((data (ifd-array :type 'f8* :size value-count))))
(defmethod bytes-per-entry ((class (eql (find-class 'double-ifd-entry)))) 8)


(define-binary-type tiff-ifd-offset ()
  (:reader (in)
           (let ((val (read-value 'u4* in)))
             (prog1 val
               (if (plusp val)
                   (file-position in val)))))
  (:writer (out value)
           (write-value 'u4* out value)
           (if (plusp value)
               (file-position out value))))

(define-binary-class ifd ()
  ((entry-count u2*)
   (entries (array :type 'ifd-entry :size entry-count))
   (next-ifd-offset tiff-ifd-offset)))

(define-binary-type ifd-list ()
  (:reader (in)
           (loop for ifd = (read-value 'ifd in)
              collect ifd
              while (plusp (next-ifd-offset ifd))))
  (:writer (out value)
           (loop for ifd in value
              do
                (setf (entries ifd)
                      (sort (entries ifd) #'< :key #'tag))
                (write-value 'ifd out ifd))))

(define-binary-class tiff-fields ()
  ((byte-order tiff-byte-order)
   (magic u2*)
   (ifd-offset tiff-ifd-offset)
   (ifd-list ifd-list)))

(defmethod read-value :around ((type (eql 'tiff)) stream &key)
  (let (*byte-order*)
    (call-next-method)))

(defun read-bytes (stream count)
  (let ((buf (make-array count :element-type '(unsigned-byte 8))))
    (read-sequence buf stream)
    buf))

