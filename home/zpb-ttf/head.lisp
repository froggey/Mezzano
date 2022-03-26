;;; Copyright (c) 2006 Zachary Beane, All Rights Reserved
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
;;; Loading data from the "head" table.
;;;
;;;  http://www.microsoft.com/OpenType/OTSpec/head.htm
;;;  http://developer.apple.com/fonts/TTRefMan/RM06/Chap6head.html
;;;
;;; $Id: head.lisp,v 1.5 2006/02/18 23:13:43 xach Exp $

(in-package #:zpb-ttf)

(defgeneric load-head-info (font-loader))

(defmethod load-head-info ((font-loader font-loader))
  (seek-to-table "head" font-loader)
  (with-slots (input-stream units/em bounding-box loca-offset-format)
      font-loader
    (flet ((skip-bytes (count)
             (file-position input-stream (+ count
                                            (file-position input-stream)))))
      (let ((version (read-uint32 input-stream)))
        (check-version "\"head\" table" version #x00010000))
      ;; skip fontRevsion and checkSumAdjustment (both uint32)
      (skip-bytes 8)
      ;; check the magicNumber
      (let ((magic-number (read-uint32 input-stream)))
        (when (/= magic-number #x5F0F3CF5)
          (error 'bad-magic
                 :location "\"head\" table"
                 :expected-values (list #x5F0F3CF5)
                 :actual-value magic-number)))
      ;; skip flags
      (skip-bytes 2)
      (setf units/em (read-uint16 input-stream))
      ;; skip created and modified dates
      (skip-bytes 16)
      (setf bounding-box (vector (read-int16 input-stream)
                                 (read-int16 input-stream)
                                 (read-int16 input-stream)
                                 (read-int16 input-stream)))
      ;; skip macStyle, lowestRecPPEM, fontDirectionHint
      (skip-bytes 6)
      ;; set the loca-offset-format
      (if (zerop (read-int16 input-stream))
          (setf loca-offset-format :short)
          (setf loca-offset-format :long)))))
