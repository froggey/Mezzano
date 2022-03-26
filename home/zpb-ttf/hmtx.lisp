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
;;; Loading data from the "hmtx" table.
;;;
;;;  http://www.microsoft.com/OpenType/OTSpec/hmtx.htm
;;;  http://developer.apple.com/fonts/TTRefMan/RM06/Chap6hmtx.html
;;;
;;; $Id: hmtx.lisp,v 1.3 2006/02/18 23:13:43 xach Exp $

(in-package #:zpb-ttf)

(defgeneric load-hmtx-info (font-loader))

(defmethod load-hmtx-info ((font-loader font-loader))
  (let* ((horizontal-metrics-count (horizontal-metrics-count font-loader))
         (advance-widths (make-array horizontal-metrics-count))
         (left-side-bearings (make-array horizontal-metrics-count)))
    (seek-to-table "hmtx" font-loader)
    (with-slots (input-stream) font-loader
      (dotimes (i horizontal-metrics-count)
        (setf (svref advance-widths i) (read-uint16 input-stream))
        (setf (svref left-side-bearings i) (read-int16 input-stream))))
    (setf (advance-widths font-loader) advance-widths
          (left-side-bearings font-loader) left-side-bearings)))
