(in-package :cl-user)

(defpackage #:parsley
  (:use #:cl)
  (:export #:*buffer*
           #:buffer-bytes
           #:buffer-bits
           #:buffer-sequence
           #:buffer-stream
           #:buffer-position
           #:with-buffer-read
           #:octets=
           #:read-bits
           #:read-bytes
           #:read-uint-be
           #:read-uint-le
           #:read-int-be
           #:read-int-le
           #:read-string
           #:uncompress-bzip2
           #:uncompress-gzip
           #:uncompress-zlib
           #:uncompress-deflate
           #:split-string))
