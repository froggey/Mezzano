;;;; dstate.lisp -- common bits for decompression state

(in-package :chipz)

;;; This structure is never meant to be instantiated.  It exists only to
;;; provide common framework for other decompressors.
(defstruct (decompression-state
             (:constructor)
             (:conc-name dstate-))
  (state nil :type (or null function))
  (done nil)

  (input (make-array 1 :element-type '(unsigned-byte 8))
         :type simple-octet-vector)
  (input-start 0 :type (and fixnum (integer 0 *)))
  (input-index 0 :type (and fixnum (integer 0 *)))
  (input-end 0 :type (and fixnum (integer 0 *)))

  (output (make-array 1 :element-type '(unsigned-byte 8))
          :type simple-octet-vector)
  (output-start 0 :type (and fixnum (integer 0 *)))
  (output-index 0 :type (and fixnum (integer 0 *)))
  (output-end 0 :type (and fixnum (integer 0 *)))

  ;; Checksums of various sorts.
  (checksum nil)
  (update-checksum nil :type (or null function))

  ;; Bit buffer.
  (bits 0 :type (unsigned-byte 32))
  (n-bits 0 :type (integer 0 32)))

(defun make-dstate (format)
  "Return a structure suitable for uncompressing data in DATA-FORMAT;
DATA-FORMAT should be:

  :BZIP2 or CHIPZ:BZIP2      For decompressing data in the `bzip2' format;
  :GZIP or CHIPZ:GZIP        For decompressing data in the `gzip' format;
  :ZLIB or CHIPZ:ZLIB        For decompressing data in the `zlib' format;
  :DEFLATE or CHIPZ:DEFLATE  For decompressing data in the `deflate' format.

The usual value of DATA-FORMAT will be one of CHIPZ:BZIP2 or CHIPZ:GZIP."
  (case format
    ((:deflate :zlib :gzip
       deflate zlib gzip)
     (make-inflate-state format))
    ((:bzip2 bzip2)
     (make-bzip2-state))
    (t
     (error 'invalid-format-error :format format))))

(defun finish-dstate (state)
  (unless (dstate-done state)
    (error 'premature-end-of-stream))
  t)
