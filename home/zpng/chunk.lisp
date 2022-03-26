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

(in-package #:zpng)

;;; Chunks

(defclass chunk ()
  ((buffer :initarg :buffer :reader buffer)
   (pos :initform 4 :accessor pos)))

(defun chunk-write-byte (byte chunk)
  "Save one byte to CHUNK."
  (setf (aref (buffer chunk) (pos chunk)) byte)
  (incf (pos chunk)))

(defun chunk-write-uint32 (integer chunk)
  "Save INTEGER to CHUNK as four bytes."
  (let ((buffer (buffer chunk))
        (i (pos chunk)))
    (setf (aref buffer (+ i 0)) (ldb (byte 8 24)  integer)
          (aref buffer (+ i 1)) (ldb (byte 8 16)  integer)
          (aref buffer (+ i 2)) (ldb (byte 8  8)  integer)
          (aref buffer (+ i 3)) (ldb (byte 8  0)  integer)
          (pos chunk) (+ i 4))))

(defun make-chunk (a b c d size)
  "Make a chunk that uses A, B, C, and D as the signature bytes, with
data size SIZE."
  (let ((buffer (make-array (+ size 4) :element-type '(unsigned-byte 8))))
    (setf (aref buffer 0) a
          (aref buffer 1) b
          (aref buffer 2) c
          (aref buffer 3) d)
    (make-instance 'chunk
                   :buffer buffer)))

(defun write-chunk (chunk stream)
  (write-uint32 (- (pos chunk) 4) stream)
  (write-sequence (buffer chunk) stream :end (pos chunk))
  (write-uint32 (checksum (buffer chunk) (pos chunk)) stream))
