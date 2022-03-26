(in-package :parsley)

(defun octets= (octet-vector octet-list)
  (equalp octet-vector (fast-io:octets-from octet-list)))

(defun read-bits (count &key (processor #'identity))
  (funcall processor (bitio:read-bits (buffer-bits) count)))

(defun read-bytes (count &key (bits-per-byte 8) (processor #'identity))
  (let ((octet-vector (fast-io:make-octet-vector count)))
    (bitio:read-bytes (buffer-bits) octet-vector :bits-per-byte bits-per-byte)
    (funcall processor octet-vector)))

(defun read-uint-be (byte-count &key (bits-per-byte 8) (processor #'identity))
  (let ((value (bitio:read-integer
                (buffer-bits)
                :byte-endian :be
                :num-bytes byte-count
                :bits-per-byte bits-per-byte
                :unsignedp t)))
    (funcall processor value)))

(defun read-uint-le (byte-count &key (bits-per-byte 8) (processor #'identity))
  (let ((value (bitio:read-integer
                (buffer-bits)
                :byte-endian :le
                :num-bytes byte-count
                :bits-per-byte bits-per-byte
                :unsignedp t)))
    (funcall processor value)))

(defun read-int-be (byte-count &key (bits-per-byte 8) (processor #'identity))
  (let ((value (bitio:read-integer
                (buffer-bits)
                :byte-endian :be
                :num-bytes byte-count
                :bits-per-byte bits-per-byte
                :unsignedp nil)))
    (funcall processor value)))

(defun read-int-le (byte-count &key (bits-per-byte 8) (processor #'identity))
  (let ((value (bitio:read-integer
                (buffer-bits)
                :byte-endian :le
                :num-bytes byte-count
                :bits-per-byte bits-per-byte
                :unsignedp nil)))
    (funcall processor value)))

(defun read-string (&key bytes (encoding :ascii) (processor #'identity)
                      null-terminated-p)
  (let ((octet-vector (fast-io:make-octet-vector
                       (%string-length bytes null-terminated-p))))
    (fast-io:fast-read-sequence octet-vector (buffer-bytes))
    (when null-terminated-p
      (fast-io:fast-read-byte (buffer-bytes)))
    (babel:octets-to-string
     (funcall processor octet-vector)
     :encoding encoding)))
