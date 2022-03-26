;;;
;;; Copyright (c) 2007 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package #:salza2)

(defvar *gzip-signature* (octet-vector #x1F #x8B)
  "These two octets precede all data in the gzip format.")

(defconstant +gzip-fast-compression+ 4
  "Code for gzip compression level. This is present only to create valid
gzip data; it has no meaning to the compressor and is only a hint to
the decompressor.")

;;; These are all used to create valid files, not to control or modify
;;; the compression process.

(defconstant +gzip-deflate-compression+ 8)
(defconstant +gzip-flags+ 0)
(defconstant +gzip-unix-os+ 3)
(defconstant +gzip-mtime+ 0)

(defun gzip-write-u32 (value compressor)
  ;; LSB
  (write-octet (ldb (byte 8 0) value) compressor)
  (write-octet (ldb (byte 8 8) value) compressor)
  (write-octet (ldb (byte 8 16) value) compressor)
  (write-octet (ldb (byte 8 24) value) compressor))

(defclass gzip-compressor (deflate-compressor)
  ((checksum
    :initarg :checksum
    :accessor checksum)
   (data-length
    :initarg :data-length
    :accessor data-length))
  (:default-initargs
   :checksum (make-instance 'crc32-checksum)
   :data-length 0))

(defmethod start-data-format :before ((compressor gzip-compressor))
  (write-octet-vector *gzip-signature* compressor)
  (write-octet +gzip-deflate-compression+ compressor)
  (write-octet +gzip-flags+ compressor)
  (gzip-write-u32 +gzip-mtime+ compressor)
  (write-octet +gzip-fast-compression+ compressor)
  (write-octet +gzip-unix-os+ compressor))

(defmethod process-input :after ((compressor gzip-compressor)
                                 input start count)
  (incf (data-length compressor) count)
  (update (checksum compressor) input start count))

(defmethod finish-data-format :after ((compressor gzip-compressor))
  (gzip-write-u32 (result (checksum compressor)) compressor)
  (gzip-write-u32 (data-length compressor) compressor))

(defmethod reset :after ((compressor gzip-compressor))
  (reset (checksum compressor))
  (setf (data-length compressor) 0))
