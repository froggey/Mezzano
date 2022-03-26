(in-package :parsley)

(defun %uncompress-octets (octet-vector compression-scheme)
  (chipz:decompress nil compression-scheme octet-vector
                    :buffer-size (* (length octet-vector) 2)))

(defun %string-length (bytes null-terminated-p)
  (let* ((sequence (fast-io:input-buffer-vector (buffer-bytes)))
         (max-length (or bytes (length sequence)))
         (start (fast-io:buffer-position (buffer-bytes)))
         (end (min (length sequence) (+ start max-length)))
         (index (if null-terminated-p
                    (position 0 sequence :start start :end end)
                    end)))
    (- index start)))

(defun split-string (string delimiter)
  (let ((pos (position delimiter string)))
    (list (subseq string 0 pos)
          (subseq string (1+ pos)))))
