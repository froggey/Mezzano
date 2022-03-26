;;;; zlib.lisp -- dealing with zlib-wrapped deflate data

(in-package :chipz)

(defclass zlib-header ()
  ((flags :initarg :flags :accessor flags)
   (cmf :initarg :cmf :accessor cmf)
   (fdict :initarg :fdict :accessor fdict)
   (adler32 :initarg :adler32 :accessor adler32)))

(defconstant +zlib-compression-method+ 8)

(defun zlib-compression-method (cmf-byte)
  (declare (type (unsigned-byte 8) cmf-byte))
  (ldb (byte 4 0) cmf-byte))

(defun zlib-compression-info (cmf-byte)
  (declare (type (unsigned-byte 8) cmf-byte))
  (ldb (byte 4 4) cmf-byte))

(defconstant +zlib-flag-fdict+ 5)

(defun zlib-flag-fcheck (flag-byte)
  (declare (type (unsigned-byte 8) flag-byte))
  (ldb (byte 4 0) flag-byte))

(defconstant +zlib-flevel-fastest+ 0)
(defconstant +zlib-flevel-fast+ 1)
(defconstant +zlib-flevel-default+ 2)
(defconstant +zlib-flevel-maximum+ 3)

(defun zlib-flag-flevel (flag-byte)
  (declare (type (unsigned-byte 8) flag-byte))
  (ldb (byte 2 6) flag-byte))
