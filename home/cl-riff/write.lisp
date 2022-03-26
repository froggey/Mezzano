;;;; write.lisp

(in-package #:riff)

(defun write-fourcc (fourcc stream)
  "Writes a four character tag (FOURCC) to stream."
  (assert (= (length fourcc) 4))
  (loop for ch across fourcc
     do (write-byte (char-code ch) stream))
  stream)

(defun write-u4 (u4 stream)
  "Writes a 4 byte little-endian integer to stream."
  (write-byte (ldb (byte 8 0) u4) stream)
  (write-byte (ldb (byte 8 8) u4) stream)
  (write-byte (ldb (byte 8 16) u4) stream)
  (write-byte (ldb (byte 8 24) u4) stream)
  stream)

(defun write-u2 (u2 stream)
  "Writes a 2 byte little-endian integer to stream."
  (write-byte (ldb (byte 8 0) u2) stream)
  (write-byte (ldb (byte 8 8) u2) stream)
  stream)

(defun default-chunk-data-writer (chunk-data stream
                                  &key (start 0) (end (length chunk-data)))
  "Writes chunk-data as an array of bytes."
  (write-sequence chunk-data stream :start start :end end)
  stream)

(defun write-riff-section (chunk-id section-type chunk-data-size stream)
  "Writes a riff subsection header to stream."
  (write-fourcc chunk-id stream)
  (write-u4 chunk-data-size stream)
  (write-fourcc section-type stream)
  stream)

(defun write-riff-chunk (chunk-id chunk-data stream
                         &key (chunk-data-writer #'default-chunk-data-writer)
                           start end)
  "Writes a riff file chunk to stream."
  (let* ((start (or start 0))
         (end (or end (length chunk-data)))
         (len (- end start)))
    (assert (<= 0 start end (length chunk-data)))

    (write-fourcc chunk-id stream)
    (write-u4 len stream)

    (funcall chunk-data-writer chunk-data stream :start start :end end)
    ;; Add pad character if needed
    (when (oddp len)
      (write-byte 0 stream)))

  stream)

(defun write-riff-chunks (chunks stream
                          &key (chunk-data-writer #'default-chunk-data-writer))
  "Writes all the chunks to stream."
  (flet ((write-section-or-chunk (chunk)
           (let ((chunk-id (riff-chunk-id chunk))
                 (file-type (riff-file-type chunk)))
             (if file-type
                 (write-riff-section chunk-id
                                     file-type
                                     (riff-chunk-data-size chunk)
                                     stream)
                 (write-riff-chunk chunk-id
                                   (riff-chunk-data chunk)
                                   stream
                                   :chunk-data-writer chunk-data-writer
                                   :start (riff-chunk-data-start chunk)
                                   :end (riff-chunk-data-end chunk))))))
    (loop for chunk in chunks
       do (write-section-or-chunk chunk)))
  stream)

(defun write-riff-file (chunks filespec
                        &key (chunk-data-writer #'default-chunk-data-writer))
  "Writes a RIFF format file named by filespec from the given chunks."
  (with-open-file (stream filespec
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-does-not-exist :create)
    (write-riff-chunks chunks stream :chunk-data-writer chunk-data-writer)))

;;; END
