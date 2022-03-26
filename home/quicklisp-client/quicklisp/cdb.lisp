;;;; cdb.lisp

(in-package #:ql-cdb)

(defconstant +initial-hash-value+ 5381)

(defun cdb-hash (octets)
  "http://cr.yp.to/cdb/cdb.txt"
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
           (optimize speed))
  (let ((h +initial-hash-value+))
    (declare (type (unsigned-byte 32) h))
    (dotimes (i (length octets) h)
      (let ((c (aref octets i)))
        (setf h (logand #xFFFFFFFF (+ h (ash h 5))))
        (setf h (logxor h c))))))

(defun make-growable-vector (&key
                             (size 10) (element-type t))
  (make-array size :fill-pointer 0 :adjustable t :element-type element-type))

(defun make-octet-vector (size)
  (make-array size :element-type '(unsigned-byte 8)))

(defun encode-string (string)
  "Do a bare-bones ASCII encoding of STRING."
  (map-into (make-octet-vector (length string))
            'char-code
            string))

(defun decode-octets (octets)
  "Do a bare-bones ASCII decoding of OCTETS."
  (map-into (make-string (length octets))
            'code-char
            octets))

(defun read-cdb-u32 (stream)
  (logand #xFFFFFFFF
          (logior (ash (read-byte stream) 0)
                  (ash (read-byte stream) 8)
                  (ash (read-byte stream) 16)
                  (ash (read-byte stream) 24))))

(defun lookup-record-at (position key stream)
  (file-position stream position)
  (let ((key-size (read-cdb-u32 stream))
        (value-size (read-cdb-u32 stream)))
    (when (= key-size (length key))
      (let ((test-key (make-octet-vector key-size)))
        (when (/= key-size (read-sequence test-key stream))
          (error "Could not read record key of size ~D from cdb stream"
                 key-size))
        (unless (mismatch test-key key :test #'=)
          (let ((value (make-octet-vector value-size)))
            (if (= value-size (read-sequence value stream))
                value
                (error "Could not read record value of size ~D from cdb stream"
                       value-size))))))))

(defun table-slot-lookup (key hash table-position
                          initial-slot slot-count stream)
  (let ((slot initial-slot))
    (loop
      (file-position stream (+ table-position (* slot 8)))
      (let ((test-hash (read-cdb-u32 stream))
            (record-position (read-cdb-u32 stream)))
        (when (zerop record-position)
          (return))
        (when (= hash test-hash)
          (let ((value (lookup-record-at record-position key stream)))
            (when value
              (return value)))))
      (setf slot (mod (1+ slot) slot-count)))))

(defun stream-lookup (key stream)
  (let* ((hash (cdb-hash key))
         (pointer-index (logand #xFF hash)))
    (file-position stream (* pointer-index 8))
    (let ((table-position (read-cdb-u32 stream))
          (slot-count (read-cdb-u32 stream)))
      (when (plusp slot-count)
        (let ((initial-slot (mod (ash hash -8) slot-count)))
          (table-slot-lookup key hash
                             table-position initial-slot slot-count stream))))))

(defun %lookup (key cdb)
  "Return the value for KEY in CDB, or NIL if no matching key is
found. CDB should be a pathname or an open octet stream. The key
should be a vector of octets. The returned value will be a vector of
octets."
  (if (streamp cdb)
      (stream-lookup key cdb)
      (with-open-file (stream cdb :element-type '(unsigned-byte 8))
        (stream-lookup key stream))))

(defun lookup (key cdb)
  "Return the value for KEY in CDB, or NIL if no matching key is
found. CDB should be a pathname or an open octet stream. The key
should be an ASCII-encodable string. The returned value will be a
string."
  (let ((value (%lookup (encode-string key) cdb)))
    (when value
      (decode-octets value))))

(defun stream-map-cdb (function stream)
  (labels ((map-one-slot (i)
             (file-position stream (* i 8))
             (let ((table-position (read-cdb-u32 stream))
                   (slot-count (read-cdb-u32 stream)))
               (when (plusp slot-count)
                 (map-one-table table-position slot-count))))
           (map-one-table (position count)
             (dotimes (i count)
               (file-position stream (+ position (* i 8)))
               (let ((hash (read-cdb-u32 stream))
                     (position (read-cdb-u32 stream)))
                 (declare (ignore hash))
                 (when (plusp position)
                   (map-record position)))))
           (map-record (position)
             (file-position stream position)
             (let* ((key-size (read-cdb-u32 stream))
                    (value-size (read-cdb-u32 stream))
                    (key (make-octet-vector key-size))
                    (value (make-octet-vector value-size)))
               (read-sequence key stream)
               (read-sequence value stream)
               (funcall function key value))))
    (dotimes (i 256)
      (map-one-slot i))))

(defun %map-cdb (function cdb)
  "Call FUNCTION once with each key and value in CDB."
  (if (streamp cdb)
      (stream-map-cdb function cdb)
      (with-open-file (stream cdb :element-type '(unsigned-byte 8))
        (stream-map-cdb function stream))))

(defun map-cdb (function cdb)
  (%map-cdb (lambda (key value)
              (funcall function
                       (decode-octets key)
                       (decode-octets value)))
            cdb))


;;; Writing CDB files

(defun write-cdb-u32 (u32 stream)
  "Write an (unsigned-byte 32) value to STREAM in little-endian order."
  (write-byte (ldb (byte 8 0) u32) stream)
  (write-byte (ldb (byte 8 8) u32) stream)
  (write-byte (ldb (byte 8 16) u32) stream)
  (write-byte (ldb (byte 8 24) u32) stream))

(defclass record-pointer ()
  ((hash-value
    :initarg :hash-value
    :accessor hash-value
    :documentation "The hash value of the record key.")
   (record-position
    :initarg :record-position
    :accessor record-position
    :documentation "The file position at which the record is stored."))
  (:default-initargs
   :hash-value 0
   :record-position 0)
  (:documentation "Every key/value record written to a CDB has a
  corresponding record pointer, which tracks the key's hash value and
  the record's position in the data file. When all records have been
  written to the file, these record pointers are organized into hash
  tables at the end of the cdb file."))

(defmethod print-object ((record-pointer record-pointer) stream)
  (print-unreadable-object (record-pointer stream :type t)
    (format stream "~8,'0X@~:D"
            (hash-value record-pointer)
            (record-position record-pointer))))

(defvar *empty-record-pointer* (make-instance 'record-pointer))


(defclass hash-table-bucket ()
  ((table-position
    :initarg :table-position
    :accessor table-position
    :documentation "The file position at which this table
    is (eventually) slotted.")
   (entries
    :initarg :entries
    :accessor entries
    :documentation "A vector of record-pointers."))
  (:default-initargs
   :table-position 0
   :entries (make-growable-vector))
  (:documentation "During construction of the CDB, record pointers are
  accumulated into one of 256 hash table buckets, depending on the low
  8 bits of the hash value of the key. At the end of record writing,
  these buckets are used to write out hash table vectors at the end of
  the file, and write pointers to the hash table vectors at the start
  of the file."))

(defgeneric entry-count (object)
  (:method ((object hash-table-bucket))
    (length (entries object))))

(defgeneric slot-count (object)
  (:method ((object hash-table-bucket))
    (* (entry-count object) 2)))

(defun bucket-hash-vector (bucket)
  "Create a hash vector for a bucket. A hash vector has 2x the entries
of the bucket, and is initialized to an empty record pointer. The high
24 bits of the hash value of a record pointer, mod the size of the
vector, is used as a starting slot, and the vector is walked (wrapping
at the end) to find the first free slot for positioning each record
pointer entry."
  (let* ((size (slot-count bucket))
         (vector (make-array size :initial-element nil)))
    (flet ((slot (record)
             (let ((index (mod (ash (hash-value record) -8) size)))
               (loop
                 (unless (aref vector index)
                   (return (setf (aref vector index) record)))
                 (setf index (mod (1+ index) size))))))
      (map nil #'slot (entries bucket)))
    (nsubstitute *empty-record-pointer* nil vector)))

(defmethod print-object ((bucket hash-table-bucket) stream)
  (print-unreadable-object (bucket stream :type t)
    (format stream "~D entr~:@P" (entry-count bucket))))


(defclass cdb-writer ()
  ((buckets
    :initarg :buckets
    :accessor buckets)
   (end-of-records-position
    :initarg :end-of-records-position
    :accessor end-of-records-position)
   (output
    :initarg :output
    :accessor output))
  (:default-initargs
   :end-of-records-position 2048
   :buckets (map-into (make-array 256)
                      (lambda () (make-instance 'hash-table-bucket)))))


(defun add-record (key value cdb-writer)
  "Add KEY and VALUE to a cdb file. KEY and VALUE should both
be (unsigned-byte 8) vectors."
  (let* ((output (output cdb-writer))
         (hash-value (cdb-hash key))
         (bucket-index (logand #xFF hash-value))
         (bucket (aref (buckets cdb-writer) bucket-index))
         (record-position (file-position output))
         (record-pointer (make-instance 'record-pointer
                                        :record-position record-position
                                        :hash-value hash-value)))
    (vector-push-extend record-pointer (entries bucket))
    (write-cdb-u32 (length key) output)
    (write-cdb-u32 (length value) output)
    (write-sequence key output)
    (write-sequence value output)
    (force-output output)
    (incf (end-of-records-position cdb-writer)
          (+ 8 (length key) (length value)))))

(defun write-bucket-hash-table (bucket stream)
  "Write BUCKET's hash table vector to STREAM."
  (map nil
       (lambda (pointer)
         (write-cdb-u32 (hash-value pointer) stream)
         (write-cdb-u32 (record-position pointer) stream))
       (bucket-hash-vector bucket)))

(defun write-hash-tables (cdb-writer)
  "Write the traililng hash tables to the end of the cdb
file. Initializes the position of the buckets in the process."
  (let ((stream (output cdb-writer)))
    (map nil
         (lambda (bucket)
           (setf (table-position bucket) (file-position stream))
           (write-bucket-hash-table bucket stream))
         (buckets cdb-writer))))

(defun write-pointers (cdb-writer)
  "Write the leading hash table pointers to the beginning of the cdb
file. Must be called after WRITE-HASH-TABLES, or the positions won't
be available."
  (let ((stream (output cdb-writer)))
    (file-position stream :start)
    (map nil
         (lambda (bucket)
           (let ((position (table-position bucket))
                 (count (slot-count bucket)))
             (when (zerop position)
               (error "Table positions not initialized correctly"))
             (write-cdb-u32 position stream)
             (write-cdb-u32 count stream)))
         (buckets cdb-writer))))

(defun finish-cdb-writer (cdb-writer)
  "Write the trailing hash tables and leading table pointers to the
cdb file."
  (write-hash-tables cdb-writer)
  (write-pointers cdb-writer)
  (force-output (output cdb-writer)))


(defvar *pointer-padding* (make-array 2048 :element-type '( unsigned-byte 8)))

(defun call-with-output-to-cdb (cdb-pathname temp-pathname fun)
  "Call FUN with one argument, a CDB-WRITER instance to which records
can be added with ADD-RECORD."
  (with-open-file (stream temp-pathname
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (let ((cdb (make-instance 'cdb-writer :output stream)))
      (write-sequence *pointer-padding* stream)
      (funcall fun cdb)
      (finish-cdb-writer cdb)))
  (values (rename-file temp-pathname cdb-pathname)))

(defmacro with-output-to-cdb ((cdb file temp-file) &body body)
  "Evaluate BODY with CDB bound to a CDB-WRITER object. The CDB in
progress is written to TEMP-FILE, and then when the CDB is
successfully written, TEMP-FILE is renamed to FILE. For atomic
operation, FILE and TEMP-FILE must be on the same filesystem."
  `(call-with-output-to-cdb ,file ,temp-file
                            (lambda (,cdb)
                              ,@body)))


;;; Index file (systems.txt, releases.txt) conversion

(defun convert-index-file (index-file
                           &key (cdb-file (make-pathname :type "cdb"
                                                         :defaults index-file))
                             (index 0))
  (with-open-file (stream index-file)
    (let ((header (read-line stream)))
      (unless (and (plusp (length header))
                   (char= (char header 0) #\#))
        (error "Bad header line in ~A -- ~S"
               index-file header)))
    (with-output-to-cdb (cdb cdb-file (make-pathname :type "cdb-tmp"
                                                     :defaults cdb-file))
      (loop for line = (read-line stream nil)
            for words = (and line (ql-util:split-spaces line))
            while line do
            (add-record (encode-string (elt words index))
                        (encode-string line)
                        cdb)))))
