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
;;; Loading data from the "cmap" table.
;;;
;;;  http://www.microsoft.com/OpenType/OTSpec/cmap.htm
;;;  http://developer.apple.com/fonts/TTRefMan/RM06/Chap6cmap.html
;;;
;;; $Id: cmap.lisp,v 1.15 2006/03/23 22:23:32 xach Exp $

(in-package #:zpb-ttf)

(deftype cmap-value-table ()
  `(array (unsigned-byte 16) (*)))

;;; FIXME: "unicode-cmap" is actually a format 4 character map that
;;; happens to currently be loaded from a Unicode-compatible
;;; subtable. However, other character maps (like Microsoft's Symbol
;;; encoding) also use format 4 and could be loaded with these
;;; "unicode" objects and functions.

(defclass unicode-cmap ()
  ((segment-count :initarg :segment-count :reader segment-count)
   (end-codes :initarg :end-codes :reader end-codes)
   (start-codes :initarg :start-codes :reader start-codes)
   (id-deltas :initarg :id-deltas :reader id-deltas)
   (id-range-offsets :initarg :id-range-offsets :reader id-range-offsets)
   (glyph-indexes :initarg :glyph-indexes :accessor glyph-indexes)))

(defun load-unicode-cmap (stream)
  "Load a Unicode character map of type 4 from STREAM starting at the
current offset."
  (let ((format (read-uint16 stream)))
    (when (/= format 4)
      (error 'unsupported-format
             :location "\"cmap\" subtable"
             :actual-value format
             :expected-values (list 4))))
  (let ((table-start (- (file-position stream) 2))
        (subtable-length (read-uint16 stream))
        (language-code (read-uint16 stream))
        (segment-count (/ (read-uint16 stream) 2))
        (search-range (read-uint16 stream))
        (entry-selector (read-uint16 stream))
        (range-shift (read-uint16 stream)))
    (declare (ignore language-code search-range entry-selector range-shift))
    (flet ((make-and-load-array (&optional (size segment-count))
             (loop with array = (make-array size
                                            :element-type '(unsigned-byte 16)
                                            :initial-element 0)
                   for i below size
                   do (setf (aref array i) (read-uint16 stream))
                   finally (return array)))
           (make-signed (i)
             (if (logbitp 15 i)
                 (1- (- (logandc2 #xFFFF i)))
                 i)))
      (let ((end-codes (make-and-load-array))
            (pad (read-uint16 stream))
            (start-codes (make-and-load-array))
            (id-deltas (make-and-load-array))
            (id-range-offsets (make-and-load-array))
            (glyph-index-array-size (/ (- subtable-length
                                          (- (file-position stream)
                                             table-start))
                                       2)))
        (declare (ignore pad))
        (make-instance 'unicode-cmap
                       :segment-count segment-count
                       :end-codes end-codes
                       :start-codes start-codes
                       ;; these are really signed, so sign them
                       :id-deltas (map 'vector #'make-signed id-deltas)
                       :id-range-offsets id-range-offsets
                       :glyph-indexes (make-and-load-array glyph-index-array-size))))))


(defgeneric invert-character-map (font-loader))

(defmethod invert-character-map (font-loader)
  "Return a vector mapping font indexes to code points."
  (with-slots (start-codes end-codes)
      (character-map font-loader)
    (declare (type cmap-value-table start-codes end-codes))
    (let ((points (make-array (glyph-count font-loader) :initial-element -1)))
      (dotimes (i (1- (length end-codes)) points)
        (loop for j from (aref start-codes i) to (aref end-codes i)
              for font-index = (code-point-font-index j font-loader)
              when (minusp (svref points font-index)) do
              (setf (svref points font-index) j))))))


(defgeneric code-point-font-index (code-point font-loader)
  (:documentation "Return the index of the Unicode CODE-POINT in
FONT-LOADER, if present, otherwise NIL.")
  (:method (code-point font-loader)
    (let ((cmap (character-map font-loader)))
      (with-slots (end-codes start-codes
                   id-deltas id-range-offsets
                   glyph-indexes)
          cmap
        (declare (type cmap-value-table
                       end-codes start-codes
                       id-range-offsets
                       glyph-indexes))
        (dotimes (i (segment-count cmap) 1)
          (when (<= code-point (aref end-codes i))
            (return
              (let ((start-code (aref start-codes i))
                    (id-range-offset (aref id-range-offsets i))
                    (id-delta (aref id-deltas i)))
                (cond ((< code-point start-code)
                       0)
                      ((zerop id-range-offset)
                       (logand #xFFFF (+ code-point id-delta)))
                      (t
                       (let* ((glyph-index-offset (- (+ i
                                                        (ash id-range-offset -1)
                                                        (- code-point start-code))
                                                     (segment-count cmap)))
                              (glyph-index (aref (glyph-indexes cmap)
                                                 glyph-index-offset)))
                         (logand #xFFFF
                                 (+ glyph-index id-delta)))))))))))))

(defgeneric font-index-code-point (glyph-index font-loader)
  (:documentation "Return the code-point for a given glyph index.")
  (:method (glyph-index font-loader)
    (let ((point (aref (inverse-character-map font-loader) glyph-index)))
      (if (plusp point)
          point
          0))))

(defgeneric load-cmap-info (font-loader))

(defmethod load-cmap-info ((font-loader font-loader))
  (seek-to-table "cmap" font-loader)
  (with-slots (input-stream)
      font-loader
    (let ((start-pos (file-position input-stream))
          (version-number (read-uint16 input-stream))
          (subtable-count (read-uint16 input-stream))
          (foundp nil))
      (declare (ignore version-number))
      (loop repeat subtable-count
            for platform-id = (read-uint16 input-stream)
            for platform-specific-id = (read-uint16 input-stream)
            for offset = (+ start-pos (read-uint32 input-stream))
            when (and (= platform-id
                         +microsoft-platform-id+)
                      (= platform-specific-id
                         +microsoft-unicode-bmp-encoding-id+))
            do
            (file-position input-stream offset)
            (setf (character-map font-loader) (load-unicode-cmap input-stream))
            (setf (inverse-character-map font-loader)
                  (invert-character-map font-loader)
                  foundp t)
            (return))
      (unless foundp
        (error "Could not find supported character map in font file")))))
                   
(defun available-character-maps (loader)
  (seek-to-table "cmap" loader)
  (let ((stream (input-stream loader)))
    (let ((start-pos (file-position stream))
          (version-number (read-uint16 stream))
          (subtable-count (read-uint16 stream)))
      (declare (ignore start-pos))
      (assert (zerop version-number))
      (dotimes (i subtable-count)
        (let ((platform-id (read-uint16 stream))
              (encoding-id (read-uint16 stream))
              (offset (read-uint32 stream)))
          (declare (ignore offset))
          (format t "~D (~A) - ~D (~A)~%"
                  platform-id (platform-id-name platform-id)
                  encoding-id (encoding-id-name platform-id encoding-id)))))))

