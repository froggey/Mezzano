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
;;; "kern" table functions
;;;
;;;   http://www.microsoft.com/OpenType/OTSpec/kern.htm
;;;   http://developer.apple.com/fonts/TTRefMan/RM06/Chap6kern.html
;;;
;;; $Id: kern.lisp,v 1.8 2006/03/28 14:38:37 xach Exp $

(in-package #:zpb-ttf)

(defgeneric load-kerning-subtable (font-loader format))
(defgeneric load-kern-info (font-loader))
(defgeneric all-kerning-pairs (font-loader))

(defun load-kerning-format-1 (table stream)
  "Return a hash table keyed on a UINT32 key that represents the glyph
index in the left and right halves with a value of the kerning
distance between the pair."
  (let ((pair-count (read-uint16 stream))
        (search-range (read-uint16 stream))
        (entry-selector (read-uint16 stream))
        (range-shift (read-uint16 stream)))
    (declare (ignore search-range entry-selector range-shift))
    (dotimes (i pair-count)
      (setf (gethash (read-uint32 stream) table)
            (read-int16 stream)))))

(defmethod load-kerning-subtable ((font-loader font-loader) format)
  (when (/= 1 format)
    (error 'unsupported-format
           :description "kerning subtable"
           :size 1
           :expected-values (list 1)
           :actual-value format))
  (load-kerning-format-1 (kerning-table font-loader)
                         (input-stream font-loader)))

(defmethod load-kern-info ((font-loader font-loader))
  (when (table-exists-p "kern" font-loader)
    (seek-to-table "kern" font-loader)
    (let* ((stream (input-stream font-loader))
           (maybe-version (read-uint16 stream))
           (maybe-table-count (read-uint16 stream))
           (version 0)
           (table-count 0))
      ;; These shenanegins are because Apple documents one style of
      ;; kern table and Microsoft documents another. This code
      ;; implements Microsoft's version.
      ;; See:
      ;;  http://developer.apple.com/fonts/TTRefMan/RM06/Chap6kern.html
      ;;  http://www.microsoft.com/OpenType/OTSpec/kern.htm
      (if (zerop version)
          (setf version maybe-version
                table-count maybe-table-count)
          (setf version (logand (ash maybe-version 16) maybe-table-count)
                table-count (read-uint32 stream)))
      (check-version "\"kern\" table" version 0)
      (dotimes (i table-count)
        (let ((version (read-uint16 stream))
              (length (read-uint16 stream))
              (coverage-flags (read-uint8 stream))
              (format (read-uint8 stream)))
          (declare (ignore version length coverage-flags))
          (load-kerning-subtable font-loader format))))))

(defmethod all-kerning-pairs ((font-loader font-loader))
  (let ((pairs nil))
    (maphash (lambda (k v)
               (let* ((left-index (ldb (byte 16 16) k))
                      (right-index (ldb (byte 16 0) k))
                      (left (index-glyph left-index font-loader))
                      (right (index-glyph right-index font-loader)))
                 (push (list left right v) pairs)))
             (kerning-table font-loader))
    pairs))
