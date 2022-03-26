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
;;; Loading data from the "hhea" table.
;;;
;;;  http://www.microsoft.com/OpenType/OTSpec/hhea.htm
;;;  http://developer.apple.com/fonts/TTRefMan/RM06/Chap6hhea.html
;;;
;;; $Id: hhea.lisp,v 1.4 2006/02/18 23:13:43 xach Exp $

(in-package #:zpb-ttf)

(defgeneric load-hhea-info (font-loader))
(defgeneric horizontal-metrics-count (font-loader))

(defmethod load-hhea-info ((font-loader font-loader))
  (seek-to-table "hhea" font-loader)
  (with-slots (input-stream ascender descender line-gap)
      font-loader
    (let ((version (read-fixed input-stream)))
      (check-version "\"hhea\" table" version #x00010000))
    (setf ascender (read-fword input-stream)
          descender (read-fword input-stream)
          line-gap (read-fword input-stream))))

(defmethod horizontal-metrics-count ((font-loader font-loader))
  (seek-to-table "hhea" font-loader)
  (with-slots (input-stream) font-loader
    ;; Skip to the end, since all we care about is the last item
    (advance-file-position input-stream 34)
    (read-uint16 input-stream)))
