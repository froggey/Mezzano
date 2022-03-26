(in-package #:ql-minitar)

(defconstant +block-size+ 512)
(defconstant +space-code+ 32)
(defconstant +newline-code+ 10)
(defconstant +equals-code+ 61)

(defun make-block-buffer ()
  (make-array +block-size+ :element-type '(unsigned-byte 8) :initial-element 0))

(defun skip-n-blocks (n stream)
  (let ((block (make-block-buffer)))
    (dotimes (i n)
      (read-sequence block stream))))

(defun read-octet-vector (length stream)
  (let ((block (make-block-buffer))
        (vector (make-array length :element-type '(unsigned-byte 8)))
        (offset 0)
        (block-count (ceiling length +block-size+)))
    (dotimes (i block-count)
        (read-sequence block stream)
        (replace vector block :start1 offset)
        (incf offset +block-size+))
    vector))


(defun decode-pax-header-record (vector offset)
  "Decode VECTOR as pax extended header data. Returns the keyword and
value it specifies as multiple values."
  ;; Vector format is: "%d %s=%s\n", <length>, <keyword>, <value>
  ;; See http://pubs.opengroup.org/onlinepubs/009695399/utilities/pax.html
  (let* ((length-start offset)
         (length-end (position +space-code+ vector :start length-start))
         (length-string (ascii-subseq vector length-start length-end))
         (length (parse-integer length-string))
         (keyword-start (1+ length-end))
         (keyword-end (position +equals-code+ vector :start keyword-start))
         (keyword (ascii-subseq vector keyword-start keyword-end))
         (value-start (1+ keyword-end))
         (value-end (1- (+ offset length)))
         (value (ascii-subseq vector value-start value-end)))
    (values keyword value (+ offset length))))

(defun decode-pax-header (vector)
  "Decode VECTOR as a pax header and return it as an alist."
  (let ((header nil)
        (offset 0)
        (length (length vector)))
    (loop
      (when (<= length offset)
        (return header))
      (multiple-value-bind (keyword value new-offset)
          (decode-pax-header-record vector offset)
        (setf header (acons keyword value header))
        (setf offset new-offset)))))

(defun pax-header-path (vector)
  "Decode VECTOR as a pax header and return its 'path' value, if
  any."
  (let ((header-alist (decode-pax-header vector)))
    (cdr (assoc "path" header-alist :test 'equal))))

(defun ascii-subseq (vector start end)
  (let ((string (make-string (- end start))))
    (loop for i from 0
          for j from start below end
          do (setf (char string i) (code-char (aref vector j))))
    string))

(defun block-asciiz-string (block start length)
  (let* ((end (+ start length))
         (eos (or (position 0 block :start start :end end)
                            end)))
    (ascii-subseq block start eos)))

(defun prefix (header)
  (when (plusp (aref header 345))
    (block-asciiz-string header 345 155)))

(defun name (header)
  (block-asciiz-string header 0 100))

(defun payload-size (header)
  (values (parse-integer (block-asciiz-string header 124 12) :radix 8)))

(defun nth-block (n file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (let ((block (make-block-buffer)))
      (skip-n-blocks (1- n) stream)
      (read-sequence block stream)
      block)))

(defun payload-type (code)
  (case code
    (0 :file)
    (48 :file)
    (50 :symlink)
    (76 :long-name)
    (53 :directory)
    (103 :global-header)
    (120 :pax-extended-header)
    (t :unsupported)))

(defun full-path (header)
  (let ((prefix (prefix header))
        (name (name header)))
    (if prefix
        (format nil "~A/~A" prefix name)
        name)))

(defun save-file (file size stream)
  (multiple-value-bind (full-blocks partial)
      (truncate size +block-size+)
    (ensure-directories-exist file)
    (with-open-file (outstream file
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
      (let ((block (make-block-buffer)))
        (dotimes (i full-blocks)
          (read-sequence block stream)
          (write-sequence block outstream))
        (when (plusp partial)
          (read-sequence block stream)
          (write-sequence block outstream :end partial))))))

(defun gnu-long-name (size stream)
  ;; GNU long names are simply the filename (null terminated) packed into the
  ;; payload.
  (let ((payload (read-octet-vector size stream)))
    (ascii-subseq payload 0 (1- size))))

(defun unpack-tarball (tarfile &key (directory *default-pathname-defaults*))
  (let ((block (make-block-buffer))
        (extended-path nil))
    (with-open-file (stream tarfile :element-type '(unsigned-byte 8))
      (loop
       (let ((size (read-sequence block stream)))
         (when (zerop size)
           (return))
         (unless (= size +block-size+)
           (error "Bad size on tarfile"))
         (when (every #'zerop block)
           (return))
         (let* ((payload-code (aref block 156))
                (payload-type (payload-type payload-code))
                (tar-path (or (shiftf extended-path nil)
                              (full-path block)))
                (full-path (merge-pathnames (uiop:parse-unix-namestring
                                             tar-path
                                             :defaults directory)
                                            directory))
                (payload-size (payload-size block))
                (block-count (ceiling (payload-size block) +block-size+)))
         (case payload-type
           (:file
            (save-file full-path payload-size stream))
           (:directory
            (ensure-directories-exist full-path))
           ((:symlink :global-header)
            ;; These block types aren't required for Quicklisp archives
            (skip-n-blocks block-count stream))
           (:long-name
            (setf extended-path (gnu-long-name payload-size stream)))
           (:pax-extended-header
            (let* ((pax-header-data (read-octet-vector payload-size stream))
                   (path (pax-header-path pax-header-data)))
              (when path
                (setf extended-path path))))
           (t
            (warn "Unknown tar block payload code -- ~D" payload-code)
            (skip-n-blocks block-count stream)))))))))

(defun contents (tarfile)
  (let ((block (make-block-buffer))
        (result '()))
    (with-open-file (stream tarfile :element-type '(unsigned-byte 8))
      (loop
        (let ((size (read-sequence block stream)))
          (when (zerop size)
            (return (nreverse result)))
          (unless (= size +block-size+)
            (error "Bad size on tarfile"))
          (when (every #'zerop block)
            (return (nreverse result)))
          (let* ((payload-type (payload-type (aref block 156)))
                 (tar-path (full-path block))
                 (payload-size (payload-size block)))
            (skip-n-blocks (ceiling payload-size +block-size+) stream)
            (case payload-type
              (:file
               (push tar-path result))
              (:directory
               (push tar-path result)))))))))
