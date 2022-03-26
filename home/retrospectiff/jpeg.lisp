
(in-package :retrospectiff.compression)

(defun jpeg-encode (raw-vector)
  (let ((stream (flexi-streams:make-in-memory-input-stream raw-vector)))
    (declare (ignore stream))
    (error "JPEG encoding not supported yet!")))

(defun jpeg-decode (compressed-vector jpeg-image-info)
  ;;  NOTE: we currently read the jpeg-tables every time through. We
  ;;  should be able to cache this, but we don't as its state gets
  ;;  modified by the JPEG reading stuff and we can't just reuse it
  ;;  each time through. So..., for the moment at least, we make a new
  ;;  one each time through.
  (let* ((tables (jpeg-tables jpeg-image-info))
         (tables-stream
          ;; We need to trim off the last two bytes of the tables
          ;; vector as the jpeg-stream-reader code isn't expecting
          ;; the EOI marker.
          (flexi-streams:make-in-memory-input-stream (subseq tables 0 (- (length tables) 2))))
         (jpeg-data-stream
          ;; we also need to trim off the first two bytes from this
          ;; stream, but it's probably better to make the stream and
          ;; call read-byte twice than to copy the data vector.
          (flexi-streams:make-in-memory-input-stream compressed-vector)))
    ;; See comment above. Now skip the two bytes in the data stream.
    (read-byte jpeg-data-stream)
    (read-byte jpeg-data-stream)
    (let ((jpeg-stream
           (make-concatenated-stream tables-stream jpeg-data-stream)))
      (jpeg:decode-stream jpeg-stream))))

