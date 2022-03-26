;;;; wav.lisp

(in-package #:wav)

;;; Compression codes

(define-constant +unknown+ 0)
(define-constant +pcmi-uncompressed+ 1)
(define-constant +microsoft-adpcm+ 2)
(define-constant +itu-g711-a-law+ 6)
(define-constant +itu-g711-mu-law+ 7)
(define-constant +ima-adpcm+ 17)
(define-constant +itu-g723-adpcm-yamaha+ 20)
(define-constant +gsm-610+ 49)
(define-constant +itu-g721+adpcm+ 64)
(define-constant +mpeg+ 80)
(define-constant +experimental+ 65536)

(defun format-chunk-data-reader (stream chunk-id chunk-data-size)
  "Reads and parses the chunk-data from a format chunk."
  (let ((compression-code (riff:read-u2 stream))
	(number-of-channels (riff:read-u2 stream))
	(sample-rate (riff:read-u4 stream))
	(average-bytes-per-second (riff:read-u4 stream))
	(block-align (riff:read-u2 stream))
	(significant-bits-per-sample (riff:read-u2 stream)))

    (if (eql compression-code 1)
	(list :compression-code compression-code
	      :number-of-channels number-of-channels
	      :sample-rate sample-rate
	      :average-bytes-per-second average-bytes-per-second
	      :block-align block-align
	      :significant-bits-per-sample significant-bits-per-sample)

	(let
	    ((extra-format-bytes (riff:read-u2 stream))
	     (buffer (make-array extra-format-bytes :element-type (stream-element-type stream)))
	     (extra-bytes (read-sequence buffer stream)))
	  (list :compression-code compression-code
		:number-of-channels number-of-channels
		:sample-rate sample-rate
		:average-bytes-per-second average-bytes-per-second
		:block-align block-align
		:significant-bits-per-sample significant-bits-per-sample
		:extra-format-bytes extra-format-bytes
		:bytes buffer)))))

(defparameter *format-chunk* nil)

(defun wrap-format-chunk-data-reader (&optional (chunk-data-reader #'riff:default-chunk-data-reader))
  "Creates a new chunk-data-reader function that wraps the supplied
chunk-data-reader with the ability to parse format chunks."
  (lambda (stream chunk-id chunk-data-size)
    (if (string= chunk-id "fmt ")
	(setf *format-chunk* 
	      (format-chunk-data-reader stream chunk-id chunk-data-size))
	(funcall chunk-data-reader stream chunk-id chunk-data-size))))

(defun read-u1-sample (stream)
  "Reads a 1 byte sample from stream, returning a corresponding float
between -1.0 and 1.0."
  (let ((byte (read-byte stream nil)))
    (when byte
      (float (/ (- byte 128) 128)))))

(defun u1-sample-data-chunk-reader (stream chunk-id chunk-data-size)
  "Returns an array of float samples corresponding to the 1 byte
samples read from data chunk."
  (let* ((n-samples chunk-data-size)
	 (samples (make-array n-samples :element-type 'float)))
    (loop for i from 0 below n-samples do
	 (setf (aref samples i) (read-u1-sample stream)))
    samples))

(defun read-s2 (stream)
  "Reads a 2 byte signed integer sample from stream."
  (let ((u2 (riff:read-u2 stream)))
    (if (> u2 32767)
	(- u2 65536)
	u2)))

(defun read-s2-sample (stream)
  "Reads a 2 byte sample from stream, returning a corresponding float
between -1.0 and 1.0."
  (let ((s2 (read-s2 stream)))
    (when s2
      (float (/ s2 32768)))))

(defun s2-sample-data-chunk-reader (stream chunk-id chunk-data-size)
  "Returns an array of float samples corresponding to the 2 byte
signed samples read from data chunk."
  (let* ((n-samples (/ chunk-data-size 2))
	 (samples (make-array n-samples :element-type 'float)))
    (loop for i from 0 below n-samples do
	 (setf (aref samples i) (read-s2-sample stream)))
    samples))

(defun data-chunk-data-samples-reader (stream chunk-id chunk-data-size)
  "Returns an array of float samples by reading and converting the
underlying data sample representation."
  (let* ((block-align (getf *format-chunk* :block-align))
	       (number-of-channels (getf *format-chunk* :number-of-channels))
	       (sample-size (/ block-align number-of-channels)))
	  (cond
	     ((eql sample-size 1) (u1-sample-data-chunk-reader stream chunk-id chunk-data-size))
	     ((eql sample-size 2) (s2-sample-data-chunk-reader stream chunk-id chunk-data-size))
	     (t (error "WAV data format not yet supported.")))))

(defun wrap-data-chunk-data-samples-reader (&optional (chunk-data-reader (wrap-format-chunk-data-reader)))
  "Creates a new chunk-data-reader function that wraps the supplied
chunk-data-reader with the ability to read data chunks as floating
point samples. Can be used as an alternative to the default
chunk-data-reader when calling read-wav-file."
  (lambda (stream chunk-id chunk-data-size)
    (if (string= chunk-id "data")
	(data-chunk-data-samples-reader stream chunk-id chunk-data-size)
	(funcall chunk-data-reader stream chunk-id chunk-data-size))))

(defun read-wav-file (filespec &key (chunk-data-reader (wrap-format-chunk-data-reader)))
  "Reads a wav file from filespec."
  (let (*format-chunk*)
    (riff:read-riff-file filespec :chunk-data-reader chunk-data-reader)))

