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

(defconstant +adler32-base+ 65521)

(defun adler32-update (adler-high adler-low buf start count)
  (declare (type array-index start count)
           (type (unsigned-byte 16) adler-high adler-low)
           (type octet-vector buf)
           (optimize speed))
  (cond ((zerop count)
         (values adler-high adler-low))
        (t
         (let ((length count)
               (i 0)
               (k 0)
               (s1 adler-low)
               (s2 adler-high))
           (declare (type (integer 0 16) k)
                    (type array-index i)
                    (type (unsigned-byte 16) length)
                    (type (unsigned-byte 32) s1 s2))
           (tagbody
            loop
              (setf k (min length 16))
              (decf length k)
            sum
              (setf s1 (+ (aref buf (logand #xFFFF (+ start i))) s1))
              (setf s2 (+ s1 s2))
              (decf k)
              (incf i)
              (unless (zerop k)
                (go sum))
              (setf s1 (mod s1 +adler32-base+))
              (setf s2 (mod s2 +adler32-base+))
              (unless (zerop length)
                (go loop)))
           (values s2 s1)))))

;;; Class interface

(defclass adler32-checksum (checksum)
  ((high
    :initarg :high
    :accessor high)
   (low
    :initarg :low
    :accessor low))
  (:default-initargs
   :high 0
   :low 1))

(defmethod result ((checksum adler32-checksum))
  (+ (ash (high checksum) 16)
     (low checksum)))

(defmethod result-octets ((checksum adler32-checksum))
  (ub32-octets (result checksum)))

(defmethod update ((checksum adler32-checksum) buffer start count)
  (setf (values (high checksum)
                (low checksum))
        (adler32-update (high checksum)
                        (low checksum)
                        buffer
                        start
                        count)))

(defmethod reset ((checksum adler32-checksum))
  (setf (high checksum) 0
        (low checksum) 1))
