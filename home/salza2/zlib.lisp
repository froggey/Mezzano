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

(defclass zlib-compressor (deflate-compressor)
  ((adler32
    :initarg :adler32
    :accessor adler32))
  (:default-initargs
   :adler32 (make-instance 'adler32-checksum)))

(defmethod start-data-format :before ((compressor zlib-compressor))
  ;; FIXME: Replace these naked constants with symbolic constants.
  (write-octet #x78 compressor)
  (write-octet #x9C compressor))

(defmethod process-input :after ((compressor zlib-compressor) input start count)
  (let ((checksum (adler32 compressor)))
    (update checksum input start count)))

(defmethod finish-data-format :after ((compressor zlib-compressor))
  (dolist (octet (result-octets (adler32 compressor)))
    (write-octet octet compressor)))

(defmethod reset :after ((compressor zlib-compressor))
  (reset (adler32 compressor)))
