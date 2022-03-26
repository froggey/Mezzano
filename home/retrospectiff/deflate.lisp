
(in-package :retrospectiff.compression)

;; Uses Pierre Mai's DEFLATE package to decode the vector of DEFLATED
;; bytes.
(defun deflate-decode (compressed-vector)
  (let ((input-stream (flexi-streams:make-in-memory-input-stream
                       compressed-vector))
        (output-stream (flexi-streams:make-in-memory-output-stream)))
    ;; There's an extra byte for flags according to the TIFF spec. Not
    ;; sure what we should do with it.
    (let ((cmf (read-byte input-stream))
          (flags (read-byte input-stream)))
      (declare (ignore cmf flags))
      (deflate:inflate-stream input-stream output-stream)
      (flexi-streams:get-output-stream-sequence output-stream))))

