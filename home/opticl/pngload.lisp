;;; Copyright (c) 2017 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(defun read-png-stream (stream)
  (pngload:data (pngload:load-stream stream)))

(defun read-png-file (pathname)
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (read-png-stream stream)))

