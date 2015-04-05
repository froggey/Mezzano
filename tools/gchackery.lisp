;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :iter))

(defun indent-log (file)
  (with-open-file (output (make-pathname :type "logout" :defaults file)
                          :direction :output)
    (iter (for c in-file file using #'read-char)
          (with indentation = 0)
          (write-char c output)
          (case c
            (#\> (incf indentation))
            (#\< (decf indentation))
            (#\Newline
             (format output "~D " indentation))))))

(defgeneric image-physical-word (image address)
  (:documentation "Read a word at a physical address from IMAGE"))

(defgeneric image-nil (image)
  (:documentation "Return the value of NIL in IMAGE."))

(defclass file-image ()
  ((path :initarg :path :reader file-image-path)
   (data :reader file-image-data)))

(defmethod initialize-instance :after ((instance file-image) &key &allow-other-keys)
  (with-open-file (s (file-image-path instance)
                     :element-type '(unsigned-byte 8))
    (let ((size (file-length s)))
      (setf (slot-value instance 'data) (make-array size :element-type '(unsigned-byte 8)))
      (read-sequence (file-image-data instance) s))))

(defmethod image-nil ((image file-image))
  (image-physical-word image 56))

(defmethod image-physical-word ((image file-image) address)
  (nibbles:ub64ref/le (file-image-data image) address))

(defun ptbl-lookup (image address)
  "Convert ADDRESS to a physical address."
  (let ((bml4e (ldb (byte 9 39) address))
        (bml3e (ldb (byte 9 30) address))
        (bml2e (ldb (byte 9 21) address))
        (bml1e (ldb (byte 9 12) address))
        (bml4 (image-physical-word image 96)))
    (let ((bml4ent (image-physical-word image (+ (* bml4 #x1000) (* bml4e 8)))))
      (when (zerop bml4ent)
        (error "Address ~X not present." address))
      (let* ((bml3 (ldb (byte 56 8) bml4ent))
             (bml3ent (image-physical-word image (+ (* bml3 #x1000) (* bml3e 8)))))
        (when (zerop bml3ent)
          (error "Address ~X not present." address))
        (let* ((bml2 (ldb (byte 56 8) bml3ent))
               (bml2ent (image-physical-word image (+ (* bml2 #x1000) (* bml2e 8)))))
          (when (zerop bml2ent)
            (error "Address ~X not present." address))
          (let* ((bml1 (ldb (byte 56 8) bml2ent))
                 (bml1ent (image-physical-word image (+ (* bml1 #x1000) (* bml1e 8)))))
            (when (zerop bml1ent)
              (error "Address ~X not present." address))
            (logior (* (ldb (byte 56 8) bml1ent) #x1000)
                    (logand address #xFFF))))))))

(defun image-word (image address)
  "Read a word at ADDRESS from IMAGE."
  (image-physical-word image (ptbl-lookup image address)))

(defun map-object (fn image seen-objects object)
  (unless (gethash object seen-objects)
    (setf (gethash object seen-objects) t)
    (funcall fn object)
    (let ((address (logand object (lognot #b1111))))
      (when (logtest object 1) ; ignore fixnums
        (ecase (ldb (byte 4 0) object)
          (#b0011 ; cons
           (map-object fn image seen-objects (image-word image address))
           (map-object fn image seen-objects (image-word image (+ address 8))))
          (#b0101) ; unbound-value
          ;; #b0111
          (#b1001 ; objects
           (let* ((header (image-word image address))
                  (type (ldb (byte sys.int::+object-type-size+
                                   sys.int::+object-type-shift+)
                             header))
                  (len (ldb (byte sys.int::+object-data-size+
                                  sys.int::+object-data-shift+)
                            header)))
             (if (<= sys.int::+first-function-object-tag+
                     type
                     sys.int::+last-function-object-tag+)
                 (let* ((header (image-word image address))
                        (mc-size (* (ldb (byte 16 16) header) 16))
                        (n-constants (ldb (byte 16 32) header)))
                   (dotimes (i n-constants)
                     (map-object fn image seen-objects (image-word image (+ address mc-size (* i 8))))))
                 (dotimes (i (if (<= 1 type sys.int::+last-simple-1d-array-object-tag+)
                                 0 ; ignore numeric arrays.
                                 (ecase type
                                   ((#.sys.int::+object-tag-array-t+
                                     #.sys.int::+object-tag-structure-object+)
                                    (1+ len))
                                   (#.sys.int::+object-tag-std-instance+ 3)
                                   (#.sys.int::+object-tag-thread+ (- 511 64 20))
                                   (#.sys.int::+object-tag-symbol+ 6)
                                   ((#.sys.int::+object-tag-simple-string+
                                     #.sys.int::+object-tag-string+
                                     #.sys.int::+object-tag-simple-array+
                                     #.sys.int::+object-tag-array+)
                                    (+ 4 len))
                                   (#.sys.int::+object-tag-function-reference+
                                    4)
                                   ((#.sys.int::+object-tag-bignum+
                                     #.sys.int::+object-tag-unbound-value+)
                                    0))))
                   (map-object fn image seen-objects (image-word image (+ address (* i 8))))))))
          (#b1011) ; character
          (#b1101) ; single-float
          (#b1111)))))) ; gc forwarding pointer

(defun map-objects (fn image)
  "Call FN for every object in IMAGE. Returns a hash-table whose keys are the seen objects.
Assumes that everything can be reached from NIL."
  (let ((objects (make-hash-table :test 'eq)))
    (map-object fn image objects (image-nil image))
    (make-array (hash-table-count objects)
                :element-type '(unsigned-byte 64)
                :initial-contents (alexandria:hash-table-keys objects))))

(defun extract-image-array (image address element-width)
  (let* ((size (ldb (byte 56 8) (image-word image address)))
         (array (make-array size))
         (elements-per-word (/ 64 element-width)))
    (dotimes (i size)
      (multiple-value-bind (word offset)
          (truncate i elements-per-word)
        (setf (aref array i) (ldb (byte element-width (* offset element-width))
                                  (image-word image (+ address 8 (* word 8)))))))
    array))

(defstruct image-symbol
  address
  image
  name
  package-name)


(defmethod print-object ((object image-symbol) stream)
  (if (or *print-escape* *print-readably*)
      (call-next-method)
      (cond ((equal (image-symbol-package-name object) "COMMON-LISP")
             (format stream "~A" (image-symbol-name object)))
            ((equal (image-symbol-package-name object) "KEYWORD")
             (format stream ":~A" (image-symbol-name object)))
            (t (format stream "~A::~A"
                       (image-symbol-package-name object)
                       (image-symbol-name object))))))

(defun extract-image-object (image value)
  (let ((address (logand value (lognot #b1111))))
    (ecase (ldb (byte 4 0) value)
      ((#.sys.int::+tag-fixnum-000+ #.sys.int::+tag-fixnum-001+
        #.sys.int::+tag-fixnum-010+ #.sys.int::+tag-fixnum-011+
        #.sys.int::+tag-fixnum-100+ #.sys.int::+tag-fixnum-101+
        #.sys.int::+tag-fixnum-110+ #.sys.int::+tag-fixnum-111+)
       ;; Make sure negative numbers are negative.
       (if (ldb-test (byte 1 63) value)
           (ash (logior (lognot (ldb (byte 64 0) -1)) value) (- sys.int::+n-fixnum-bits+))
           (ash value (- sys.int::+n-fixnum-bits+))))
      (#.sys.int::+tag-character+
       (code-char (ldb (byte 21 4) value)))
      (#.sys.int::+tag-cons+
       (cons (extract-image-object image (image-word image address))
             (extract-image-object image (image-word image (+ address 8)))))
      (#.sys.int::+tag-object+
       (ecase (ldb (byte sys.int::+object-type-size+
                         sys.int::+object-type-shift+)
                   (image-word image address))
         (#.sys.int::+object-tag-symbol+
          (when (eql value (image-nil image))
            (return-from extract-image-object nil))
          (let ((name (extract-image-object image (image-word image (+ address 8))))
                (package (image-word image (+ address 16))))
            (when (eql package (image-nil image))
              (error "Attemping to extract an uninterned symbol."))
            ;; package groveling...
            (let ((package-name (extract-image-object image
                                                      (image-word image
                                                                  (+ (logand package (lognot #b1111)) 16)))))
              (make-image-symbol :address address
                                 :image image
                                 :name name
                                 :package-name package-name))))
         ((#.sys.int::+object-tag-simple-string+
           #.sys.int::+object-tag-string+)
          (map 'simple-string #'code-char
               (extract-image-object image
                                     (image-word image (+ address 8)))))
         (#.sys.int::+object-tag-array-unsigned-byte-8+
          (extract-image-array image address 8))
         (#.sys.int::+object-tag-array-unsigned-byte-16+
          (extract-image-array image address 16))
         (#.sys.int::+object-tag-array-unsigned-byte-32+
          (extract-image-array image address 32))
         (#.sys.int::+object-tag-std-instance+
          (cons (extract-image-object image (image-word image (+ address 8)))
                (extract-image-object image (image-word image (+ address 16)))))
         (#.sys.int::+object-tag-function-reference+
          (format nil "#<FREF to ~S>" (extract-image-object image (image-word image (+ address 8)))))
         )))))

(defun identify-symbols (image)
  "Detect all symbols in IMAGE, returning a list of (symbol-name address)."
  (let ((symbols '()))
    (map-objects (lambda (value)
                   (when (and (eql (logand value #b1111) sys.int::+tag-object+)
                              (eql (ldb (byte sys.int::+object-type-size+
                                              sys.int::+object-type-shift+)
                                        (image-word image (logand value (lognot #b1111))))
                                   sys.int::+object-tag-symbol+))
                     (push (list (extract-image-object image (image-word image (+ (logand value (lognot #b1111)) 8)))
                                 value)
                           symbols)))
                 image)
    symbols))

(defun build-map-file (image)
  (map-objects (lambda (value)
                 (with-simple-restart (continue "Ignore value ~X" value)
                   (when (eql (logand value #b1111) sys.int::+tag-object+)
                     (let* ((address (logand value (lognot #b1111)))
                            (header (image-word image address))
                            (tag (ldb (byte 8 2) header))
                            (mc-size (* (ldb (byte 16 16) header) 16))
                            (n-constants (ldb (byte 16 32) header)))
                       (when (and (= tag sys.int::+object-tag-function+)
                                  (/= n-constants 0))
                         (format t "~8,'0X ~A~%"
                                 (+ (- value sys.int::+tag-object+) 16)
                                 (cl-ppcre:regex-replace (string #\Newline)
                                                         (format nil "~A" (extract-image-object image (image-word image (+ address mc-size))))
                                                         "#\\Newline")))))))
               image)
  (values))
