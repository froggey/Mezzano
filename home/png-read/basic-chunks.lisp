(in-package :png-read)

(defvar *png-file* nil)
(defvar *crc-fail-behaviour* :error)

(define-condition crc-failure (warning)
  ((file :initarg :file :reader file-of)
   (crc-is :initarg :crc-is :reader crc-is-of)
   (crc-read :initarg :crc-read :reader crc-read-of))
  (:report (lambda (c stream)
             (if (file-of c)
                 (format stream "Checksum failure in file ~a. Computed: #x~x, read: #x~x."
                         (file-of c) (crc-is-of c) (crc-read-of c))
                 (format stream "Checksum failure in datastream. Computed: #x~x, read: #x~x."
                         (crc-is-of c) (crc-read-of c))))))

(defun read-png-file (file)
  (let ((*png-file* file))
   (with-open-file (png-stream file :direction :input :element-type '(unsigned-byte 8))
     (read-png-datastream png-stream))))

(defvar *png-header* #(137 80 78 71 13 10 26 10))

(defun read-png-datastream (png-stream)
  (let ((header (make-array (length *png-header*) :element-type '(unsigned-byte 8))))
    (read-sequence header png-stream)
    (cond
      ((every #'eql *png-header* header)
       (read-png-chunks png-stream))
      (t (if *png-file*
             (error "File ~a is not a PNG file." *png-file*)
             (error "Not PNG datastream."))))))

(defun big-endian-vector-to-integer (byte-vector)
  (iter (for i from (1- (length byte-vector)) downto 0)
        (for j from 0)
        (summing (ash (aref byte-vector j) (* 8 i)))))

(define-compiler-macro big-endian-vector-to-integer (&whole form byte-vector-form)
  (if (and (listp byte-vector-form)
           (eql (car byte-vector-form) 'subseq))
      (destructuring-bind (subseq seq start &optional (end nil)) byte-vector-form
        (declare (ignore subseq))
        (let ((seq-gensym (gensym "SEQ-"))
              (start-gensym (gensym "START-"))
              (end-gensym (gensym "END-")))
         `(let ((,seq-gensym ,seq)
                (,start-gensym ,start))
            (let ((,end-gensym
                   ,(if end
                        end
                        `(length ,seq-gensym))))
              (iter (for i from (- ,end-gensym ,start-gensym 1) downto 0)
                    (for j from ,start-gensym)
                    (summing (ash (aref ,seq-gensym j) (* 8 i))))))))
      form))

(defun read-png-chunks (png-stream)
  (let ((length-field (make-array 4 :element-type '(unsigned-byte 8)))
        (type-field (make-array 4 :element-type '(unsigned-byte 8)))
        (crc-field (make-array 4 :element-type '(unsigned-byte 8)))
        (*png-state* (make-instance 'png-state)))
    (if *png-file* (setf (png-file *png-state*) *png-file*))
    (let ((crc-ok
           (iter
             (for read-status next (read-sequence length-field png-stream))
             (for type-status next (read-sequence type-field png-stream))
             (until (zerop read-status))
             (assert (eql read-status 4))
             (assert (eql type-status 4))
             (let ((chunk-length (big-endian-vector-to-integer length-field))
                   (type-string (map 'string #'code-char type-field)))
               (let ((chunk-data (make-array chunk-length :element-type '(unsigned-byte 8))))
                 (let ((data-status (read-sequence chunk-data png-stream)))
                   (assert (eql data-status chunk-length))
                   (let ((crc-status (read-sequence crc-field png-stream)))
                     (assert (eql crc-status 4))
                     (let ((read-crc (big-endian-vector-to-integer crc-field))
                           (computed-crc (finish-crc (updated-crc (start-crc type-field) chunk-data))))
                       (parse-chunk type-string chunk-data)
                       (unless (eql read-crc computed-crc)
                         (let ((condition (make-condition 'crc-failure
                                                          :file *png-file*
                                                          :crc-is computed-crc
                                                          :crc-read read-crc)))
                          (with-simple-restart (ignore-crc-failure "Ignore checksum failure.")
                           (ecase *crc-fail-behaviour*
                             (:error (error condition))
                             (:warn (warn condition ))
                             ((:no-action nil) nil)))))
                       (collect (eql read-crc computed-crc))))))))))
      (unless (finished *png-state*)
        (if (png-file *png-state*)
            (error "No IEND chunk in file ~a." (png-file *png-state*))
            (error "No IEND chunk in stream.")))
      (values *png-state* (every #'identity crc-ok)))))

(defun parse-chunk (chunk-type chunk-data)
  (let ((criticalp (char= (char chunk-type 0) (char (string-upcase chunk-type :end 1) 0))))
    (if criticalp
        (parse-critical-chunk (intern chunk-type (find-package :png-read)) chunk-data)
        (parse-ancillary-chunk (intern chunk-type (find-package :png-read)) chunk-data))))
