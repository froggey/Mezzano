;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(declaim (special *unifont-bmp* *unifont-bmp-data*))

(defparameter *unifont-glyph-cache* (make-array 256 :initial-element nil))
(defparameter *unifont-2d-glyph-cache* (make-array 256 :initial-element nil))

(defun map-unifont (c)
  "Locate information for character C in Unifont.
Returns the character position in *UNIFONT-BMP-DATA* and the character pixel width (8 or 16) or NIL if the character is not present."
  (let ((code (char-code c)))
    ;; Unifont only covers plane 0, no fancy stuff either.
    (when (and (<= code #xFFFF)
               (zerop (char-bits c)))
      (let ((row (aref *unifont-bmp* (ldb (byte 8 8) code))))
        (when row
          (let ((data (aref row (ldb (byte 8 0) code))))
            (when (logtest data #x80000000)
              (values (ldb (byte 30 0) data)
                      (if (logtest data #x40000000) 16 8)))))))))

(defun unifont-glyph-width (character)
  (multiple-value-bind (offset width)
      (map-unifont character)
    (declare (ignore offset))
    (or width
        (* (length (char-name character)) 8))))

(defun map-unifont-2d (c)
  ;; TODO: Generate missing characters here.
  (let* ((code (char-code c))
         (row (ldb (byte 8 8) code))
         (cell (ldb (byte 8 0) code))
         (cache-row nil)
         (glyph nil))
    ;; Unifont only covers plane 0
    (when (and (<= code #xffff)
               (zerop (char-bits c)))
      (setf cache-row (svref *unifont-2d-glyph-cache* row))
      (unless cache-row
        (setf cache-row (make-array 256 :initial-element nil)
              (svref *unifont-2d-glyph-cache* row) cache-row))
      (setf glyph (svref cache-row cell))
      (unless glyph
        (multiple-value-bind (glyph-offset width)
            (map-unifont c)
          (when (not glyph-offset) (return-from map-unifont-2d nil))
          (setf glyph (make-array (list 16 width)
                                  :displaced-to *unifont-bmp-data*
                                  :displaced-index-offset glyph-offset)
                (svref cache-row cell) glyph)))
      glyph)))
