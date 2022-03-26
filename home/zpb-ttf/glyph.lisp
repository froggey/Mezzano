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
;;; An object for working with glyphs from the font. Some fields are
;;; lazily loaded from the input-stream of the font-loader when needed.
;;;
;;; $Id: glyph.lisp,v 1.28 2007/08/08 16:21:19 xach Exp $

(in-package #:zpb-ttf)

(defclass glyph ()
  ((font-loader
    :initarg :font-loader
    :reader font-loader
    :documentation "The font-loader from which this glyph originates.")
   (font-index
    :initarg :font-index
    :accessor font-index
    :documentation "The index of this glyph within the font file, used
to look up information in various structures in the truetype file.")
   (code-point
    :initarg :code-point
    :accessor code-point)
   (contours
    :initarg :contours
    :accessor contours)
   (bounding-box
    :initarg :bounding-box
    :accessor bounding-box)))

(defmethod initialize-instance :after ((glyph glyph)
                                       &key code-point font-index font-loader
                                       &allow-other-keys)
  (flet ((argument-error (name)
           (error "Missing required initarg ~S" name)))
    (unless font-loader
      (argument-error :font-loader))
    (cond ((and code-point font-index))  ;; do nothing
          (code-point
           (setf (font-index glyph)
                 (code-point-font-index code-point font-loader)))
          (font-index
           (let ((code-point (font-index-code-point font-index font-loader)))
             (when (zerop code-point)
               (setf code-point
                     (or (postscript-name-code-point (postscript-name glyph))
                         code-point)))
             (setf (code-point glyph) code-point)))
          (t
           (argument-error (list :font-index :code-point))))))

(defmethod print-object ((glyph glyph) stream)
  (print-unreadable-object (glyph stream :type t :identity nil)
    ;; FIXME: Is this really going to be Unicode?
    (format stream "~S U+~4,'0X"
            (postscript-name glyph)
            (code-point glyph))))


;;; Glyph-specific values determined from data in the font-loader

(defgeneric left-side-bearing (object)
  (:method ((glyph glyph))
    (bounded-aref (left-side-bearings (font-loader glyph))
                  (font-index glyph))))

(defgeneric (setf left-side-bearing) (new-value glyph))

(defmethod (setf left-side-bearing) (new-value glyph)
  (setf (bounded-aref (left-side-bearings (font-loader glyph))
                      (font-index glyph))
        new-value))

;;; Kerning

(defgeneric kerning-offset (left right loader))

(defmethod kerning-offset ((left-glyph glyph) (right-glyph glyph)
                           (font-loader font-loader))
  (let ((kerning-table-key (logior (ash (font-index left-glyph) 16)
                                   (font-index right-glyph))))
    (gethash kerning-table-key (kerning-table font-loader) 0)))

(defmethod kerning-offset ((left character) (right character)
                           (font-loader font-loader))
  (kerning-offset (find-glyph left font-loader)
                  (find-glyph right font-loader)
                  font-loader))

(defmethod kerning-offset ((left null) right font-loader)
  (declare (ignore left right font-loader))
  0)

(defmethod kerning-offset (left (right null) font-loader)
  (declare (ignore left right font-loader))
  0)

(defgeneric advance-width (object)
  (:method ((glyph glyph))
    (bounded-aref (advance-widths (font-loader glyph))
                  (font-index glyph))))

(defgeneric (setf advance-width) (new-value glyph))

(defmethod (setf advance-width) (new-value (glyph glyph))
  (setf (bounded-aref (advance-widths (font-loader glyph))
                      (font-index glyph))
        new-value))

(defgeneric kerned-advance-width (object next)
  (:method ((object glyph) next)
    (+ (advance-width object)
       (kerning-offset object next (font-loader object)))))

(defgeneric location (object)
  (:method ((glyph glyph))
    (with-slots (font-index font-loader)
        glyph
      (+ (table-position "glyf" font-loader)
         (glyph-location font-index font-loader)))))

(defgeneric data-size (object)
  (:method ((glyph glyph))
    (with-slots (font-index font-loader)
        glyph
      (- (glyph-location (1+ font-index) font-loader)
         (glyph-location font-index font-loader)))))


;;; Initializing delayed data

(defgeneric initialize-bounding-box (glyph))
(defmethod initialize-bounding-box ((glyph glyph))
  (if (zerop (data-size glyph))
      (setf (bounding-box glyph) (empty-bounding-box))
      (let ((stream (input-stream (font-loader glyph))))
        ;; skip contour-count
        (file-position stream (+ (location glyph) 2))
        (setf (bounding-box glyph)
              (vector (read-fword stream)
                      (read-fword stream)
                      (read-fword stream)
                      (read-fword stream))))))
  
(defgeneric initialize-contours (glyph))
(defmethod initialize-contours ((glyph glyph))
  (if (zerop (data-size glyph))
      (setf (contours glyph) (empty-contours))
      (let ((stream (input-stream (font-loader glyph))))
        (file-position stream (location glyph))
        (let ((contour-count (read-int16 stream)))
          ;; skip glyph bounding box, 4 FWords
          (advance-file-position stream 8)
          (if (= contour-count -1)
              (setf (contours glyph)
                    (read-compound-contours (font-loader glyph)))
              (setf (contours glyph)
                    (read-simple-contours contour-count stream)))))))

(defmethod bounding-box :before ((glyph glyph))
  (unless (slot-boundp glyph 'bounding-box)
    (initialize-bounding-box glyph)))

(defmethod contours :before ((glyph glyph))
  (unless (slot-boundp glyph 'contours)
    (initialize-contours glyph)))

(defgeneric contour-count (object)
  (:method (object)
    (length (contours object))))

(defgeneric contour (object idex)
  (:method (object index)
    (aref (contours object) index)))

(defmacro do-contours ((contour object &optional result) &body body)
  (let ((i (gensym))
        (obj (gensym)))
    `(let ((,obj ,object))
       (dotimes (,i (contour-count ,obj) ,result)
         (let ((,contour (contour ,obj ,i)))
           ,@body)))))

(defgeneric right-side-bearing (object)
  (:method ((glyph glyph))
    (- (advance-width glyph)
       (- (+ (left-side-bearing glyph) (xmax glyph))
          (xmin glyph)))))


;;; Producing a bounding box for a sequence of characters

(defgeneric string-bounding-box (string loader &key kerning))

(defmethod string-bounding-box (string (font-loader font-loader)
                                &key (kerning t))
  (cond ((zerop (length string))
         (empty-bounding-box))
        ((= 1 (length string))
         (copy-seq (bounding-box (find-glyph (char string 0) font-loader))))
        (t
         (let ((origin 0)
               (left (find-glyph (char string 0) font-loader))
               (xmin most-positive-fixnum) (ymin most-positive-fixnum)
               (xmax most-negative-fixnum) (ymax most-negative-fixnum))
           (flet ((update-bounds (glyph)
                    (setf xmin (min (+ (xmin glyph) origin) xmin)
                          xmax (max (+ (xmax glyph) origin) xmax)
                          ymin (min (ymin glyph) ymin)
                          ymax (max (ymax glyph) ymax))))
             (update-bounds left)
             (loop for i from 1 below (length string)
                   for glyph = (find-glyph (char string i) font-loader)
                   do
                   (incf origin (advance-width left))
                   (when kerning
                     (incf origin (kerning-offset left glyph font-loader)))
                   (setf left glyph)
                   (update-bounds glyph)))
           (vector xmin ymin xmax ymax)))))


;;; Producing glyphs from loaders

(defgeneric glyph-exists-p (character font-loader)
  (:method ((character glyph) font-loader)
    (let ((index (font-index character)))
      (not (zerop index))))
  (:method (character font-loader)
    (glyph-exists-p (find-glyph character font-loader) font-loader)))

(defgeneric find-glyph (character font-loader)
  (:documentation "Find the glyph object for CHARACTER in FONT-LOADER
and return it. If CHARACTER is an integer, treat it as a Unicode code
point. If CHARACTER is a Lisp character, treat its char-code as a
Unicode code point.")
  (:method ((character integer) (font-loader font-loader))
    (index-glyph (code-point-font-index character font-loader) font-loader))
  (:method ((character character) (font-loader font-loader))
    (find-glyph (char-code character) font-loader)))

(defgeneric index-glyph (index font-loader)
  (:documentation "Return the GLYPH object located at glyph index
INDEX in FONT-LOADER, or NIL if no glyph is defined for that
index. Despite the name, NOT the inverse of GLYPH-INDEX.")
  (:method (index font-loader)
    (let* ((cache (glyph-cache font-loader))
           (glyph (aref cache index)))
      (if glyph
          glyph
          (setf (aref cache index)
                (make-instance 'glyph
                               :font-index index
                               :font-loader font-loader))))))


;;; Misc

(defmethod postscript-name ((glyph glyph))
  (let* ((names (postscript-glyph-names (font-loader glyph)))
         (index (font-index glyph))
         (name (aref names index)))
    (cond (name)
          ((slot-boundp glyph 'code-point)
           (setf (aref names index)
                 (format nil "uni~4,'0X" (code-point glyph))))
          (t "unknown"))))
