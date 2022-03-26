;;;; riff.lisp

(in-package #:riff)

(defun read-fourcc (stream)
  "Reads a four character tag (FOURCC) from stream and returns it as
an ASCII string. Returns NIL if end of file."
  (let ((byte (read-byte stream nil)))
    (when byte
      (coerce
       (list
	(code-char byte)
	(code-char (read-byte stream))
	(code-char (read-byte stream))
	(code-char (read-byte stream))) 'string))))

(defun read-u4 (stream)
  "Reads a 4 byte little-endian integer from stream."
  (let ((u4 0))
    (setf (ldb (byte 8 0) u4) (read-byte stream))
    (setf (ldb (byte 8 8) u4) (read-byte stream))
    (setf (ldb (byte 8 16) u4) (read-byte stream))
    (setf (ldb (byte 8 24) u4) (read-byte stream))
    u4))

(defun read-u2 (stream)
  "Reads a 2 byte little-endian integer from stream."
  (let ((u2 0))
    (setf (ldb (byte 8 0) u2) (read-byte stream))
    (setf (ldb (byte 8 8) u2) (read-byte stream))
    u2))

(defun default-chunk-data-reader (stream chunk-id chunk-data-size)
  "Reads chunk-data as an array of chunk-data-size bytes."
  (let ((chunk-data (make-array chunk-data-size
			       :element-type (stream-element-type stream))))
    (read-sequence chunk-data stream)
    chunk-data))

(defun read-riff-chunk (stream &key (chunk-data-reader #'default-chunk-data-reader))
  "Reads a riff file chunk from stream and returns it as a plist or
NIL if end of file."
  (let ((chunk-id (read-fourcc stream)))
    (when chunk-id
      (let
	  ((chunk-data-size (read-u4 stream)))
	(if (or (string= chunk-id "RIFF") (string= chunk-id "LIST"))
	    (list :chunk-id chunk-id
		  :chunk-data-size chunk-data-size
		  :file-type (read-fourcc stream))
	    (let
		((chunk-data (funcall chunk-data-reader stream chunk-id chunk-data-size)))
	      (when (oddp chunk-data-size)
		;; Discard pad character.
		(read-byte stream))
	      (list :chunk-id chunk-id
		    :chunk-data-size chunk-data-size
		    :chunk-data chunk-data)))))))

(defun read-riff-chunks (stream &key (chunk-data-reader #'default-chunk-data-reader))
  "Reads all the chunks from stream until end of file. Returns a list
of chunks."
  (loop for chunk = (read-riff-chunk stream :chunk-data-reader chunk-data-reader)
       while chunk
       collect chunk))

(defun find-riff-chunk (stream chunk-id &key (chunk-data-reader #'default-chunk-data-reader))
  "Reads chunks from stream until a chunk with chunk-id is found, or
NIL meaning not found."
  (loop for chunk = (read-riff-chunk stream :chunk-data-reader chunk-data-reader)
     while chunk
     until (string= (riff-chunk-id chunk) chunk-id)
     finally (return chunk)))

(defun read-riff-file (filespec &key (chunk-data-reader #'default-chunk-data-reader))
  "Reads a RIFF format file named by filespec, returning a list of chunks."
  (with-open-file (stream filespec :direction :input :element-type '(unsigned-byte 8))
    (read-riff-chunks stream :chunk-data-reader chunk-data-reader)))

;;; Define an interface for chunks:

(defgeneric riff-chunk-id (chunk) (:documentation "Returns the chunk-id of a riff chunk - a four character Ascii tag."))
(defgeneric riff-chunk-data-size (chunk) (:documentation "Returns the size of the riff chunk data."))
(defgeneric riff-chunk-data (chunk) (:documentation "Returns a byte array being the data in the riff chunk."))
(defgeneric riff-file-type (chunk) (:documentation "Returns a four character riff file type - e.g. \"WAVE\""))

;;; Define an implementation based on plists.

(defmethod riff-chunk-id ((chunk cons))
  (getf chunk :chunk-id))

(defmethod riff-chunk-data-size ((chunk cons))
  (getf chunk :chunk-data-size))

(defmethod riff-chunk-data ((chunk cons))
  (getf chunk :chunk-data))

(defmethod riff-chunk-data-start ((chunk cons))
  (getf chunk :chunk-data-start 0))

(defmethod riff-chunk-data-end (chunk)
  (+ (riff-chunk-data-start chunk) (riff-chunk-data-size chunk)))

(defmethod riff-file-type ((chunk cons))
  (getf chunk :file-type))

;;; END
