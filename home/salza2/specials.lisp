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

(defparameter +input-limit+ 32768)
(defparameter +input-limit-mask+ (1- +input-limit+))
(defparameter +buffer-size+ (* +input-limit+ 2))
(defparameter +buffer-size-mask+ (1- +buffer-size+))

(defparameter +input-size+ #x10000)
(defparameter +input-mask+ #x0FFFF)
(defparameter +hashes-size+ 8191)
(defparameter +radix+ 109)
(defparameter +rmax+ (* +radix+ +radix+))

(defparameter +bitstream-buffer-size+ 4096)
(defparameter +bitstream-buffer-mask+ (1- +bitstream-buffer-size+))
(defparameter +bitstream-buffer-bits+ (* +bitstream-buffer-size+ 8))
(defparameter +bitstream-buffer-bitmask+ (1- +bitstream-buffer-bits+))

(defconstant +final-block+ #b1)
(defconstant +fixed-tables+ #b01)
