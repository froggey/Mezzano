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

(deftype array-index ()
  `(mod ,array-dimension-limit))

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector ()
  '(simple-array (unsigned-byte 8) (*)))

(deftype input-index ()
  '(unsigned-byte 16))

(deftype input-buffer ()
  `(simple-array (unsigned-byte 8) (,+input-size+)))

(deftype chains-buffer ()
  `(simple-array (unsigned-byte 16) (,+input-size+)))

(deftype hashes-buffer ()
  `(simple-array (unsigned-byte 16) (,+hashes-size+)))

(deftype hash ()
  `(integer 0 ,+hashes-size+))

(deftype bitstream-buffer ()
  `(simple-array (unsigned-byte 8) (,+bitstream-buffer-size+)))

(deftype bitstream-buffer-bit-count ()
  `(integer 0 ,+bitstream-buffer-bits+))
