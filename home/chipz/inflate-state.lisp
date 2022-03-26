;;; inflate-state.lisp -- definition of an inflate state

(in-package :chipz)

(deftype sliding-window () '(simple-array (unsigned-byte 8) (32768)))

(defstruct (inflate-state
             (:include decompression-state)
             (:constructor %make-inflate-state (data-format)))
  ;; whether the current block being processed is the last one
  (final-block-p nil :type (member t nil))
  ;; the number of bytes to copy for uncompressed blocks
  (length 0)
  ;; the code for length/distance codes
  (distance 0)
  (length-code 0 :type (integer 0 28))
  (distance-code 0 :type (integer 0 31))
  ;; values for dynamic blocks
  (n-length-codes 0)
  (n-distance-codes 0)
  (n-codes 0)
  (n-values-read 0)
  (code-lengths (make-array 288) :type (simple-vector 288))
  ;; sliding window
  (window (make-array 32768 :element-type '(unsigned-byte 8))
          :type sliding-window)
  ;; position in the sliding window
  (window-index 0 :type (mod 32768))
  ;; codes table for dynamically compressed blocks
  (codes-table nil)
  ;; literal/length table for compressed blocks
  (literal/length-table *fixed-literal/length-table*
                        :type huffman-decode-table)
  ;; distance table for compressed blocks
  (distance-table *fixed-distance-table* :type huffman-decode-table)
  ;; header for wrapped data, or NIL if raw deflate data
  (header nil)
  ;; format of the compressed data that we're reading
  (data-format 'deflate :type (member deflate zlib gzip)))

(defun make-inflate-state (format)
  "Return a INFLATE-STATE structure suitable for uncompressing data in
FORMAT; FORMAT should be:

  :GZIP or CHIPZ:GZIP        For decompressing data in the `gzip' format;
  :ZLIB or CHIPZ:ZLIB        For decompressing data in the `zlib' format;
  :DEFLATE or CHIPZ:DEFLATE  For decompressing data in the `deflate' format.

The usual value of FORMAT will be one of CHIPZ:GZIP or CHIPZ:ZLIB."
  (let* ((f (case format
              ((:gzip gzip) 'gzip)
              ((:zlib zlib) 'zlib)
              ((:deflate deflate) 'deflate)
              (t
               (error 'invalid-format-error :format format))))
          (state (%make-inflate-state f)))
    (case f
      (gzip
       (setf (dstate-checksum state) (make-crc32)
             (dstate-update-checksum state) #'update-crc32))
      (zlib
       (setf (dstate-checksum state) (make-adler32)
             (dstate-update-checksum state) #'update-adler32)))
    state))

(defun finish-inflate-state (state)
  (unless (inflate-state-done state)
    (error 'premature-end-of-stream))
  t)

(defmethod print-object ((object inflate-state) stream)
  (print-unreadable-object (object stream)
    (format stream "Inflate-State input ~D/~D; output ~D/~D"
            (- (inflate-state-input-index object)
               (inflate-state-input-start object))
            (- (inflate-state-input-end object)
               (inflate-state-input-index object))
            (- (inflate-state-output-index object)
               (inflate-state-output-start object))
            (- (inflate-state-output-end object)
               (inflate-state-output-index object)))))
