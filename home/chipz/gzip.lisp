;;;; gzip.lisp -- dealing with gzip-wrapped deflate data

(in-package :chipz)

(defclass gzip-header ()
  ((flags :initarg :flags :accessor flags)
   (filename :initform nil :accessor filename)
   (write-date :initarg :write-date :accessor write-date)
   (mtime :initform 0 :accessor mtime)
   (comment :initform nil :accessor comment)
   (extra-flags :initarg :extra-flags :accessor extra-flags)
   (os :initarg :os :accessor os)
   (crc16 :initarg :crc16 :accessor crc16)
   (compression-method :initarg :compression-method :accessor compression-method)))

;;; individual bit meanings in the flag field
(defconstant +gzip-flag-text+ 0)
(defconstant +gzip-flag-crc+ 1)
(defconstant +gzip-flag-extra+ 2)
(defconstant +gzip-flag-name+ 3)
(defconstant +gzip-flag-comment+ 4)

;;; values of the compression method byte
(defconstant +gzip-deflate-method+ 8)

;;; values of the extra flag field
(defconstant +gzip-xfl-max-compression+ 2)
(defconstant +gzip-xfl-fast-compression+ 4)
