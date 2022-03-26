;;; -*- Mode: Lisp; Package: DREI-CORE -*-

;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Implementation of rectangle editing.

(in-package :drei-core)

(defvar *killed-rectangle* nil
  "The killed rectangle as a list of lines.")

(defun map-rectangle-lines (view function start end)
  "Map over lines in rectangle, calling `function' for each line.

The rectangle is defined by the marks `start' and `end'. For each
line, `function' will be called with arguments of a mark situated at
the beginning of the line, the starting column of the rectangle and
the ending column of the rectangle. This function returns a list of
the return values of `function'."
  (when (mark> start end)
    (rotatef start end))
  (let ((startcol (column-number start))
        (endcol (column-number end))
        (mark (make-buffer-mark (buffer view) (offset start))))
    (loop do (beginning-of-line mark)
       until (mark> mark end)
       collect (funcall function (clone-mark mark) startcol endcol)
       until (not (forward-line mark (syntax view) 1 nil)))))

(defmacro with-bounding-marks (((start-mark end-mark) mark startcol endcol
                                &key force-start force-end) &body body)
  "Evaluate `body' with `start-mark' and `end-mark' bound to marks
delimiting the rectangle area. The rectangle area is defined as the
part of the line that `mark' is situated in, that lies between the
columns `startcol' and `endcol'. If `force-start' or `force-end' is
non-NIL, the line will be padded with space characters in order to put
`start-mark' or `end-mark' at their specified columns respectively."
  (once-only (mark startcol endcol)
    `(progn
       (let ((,mark ,mark)
             (,startcol ,startcol)
             (,endcol ,endcol))
         (move-to-column ,mark ,startcol ,force-start)
         (let ((,start-mark (clone-mark ,mark)))
           (let ((,end-mark (clone-mark ,mark)))
             (move-to-column ,end-mark ,endcol ,force-end)
             ,@body))))))

(defun extract-and-delete-rectangle-line (mark startcol endcol)
  "For the line that `mark' is in, delete and return the string
between column `startcol' and `endcol'. If the string to be returned
is not as wide as the rectangle, it will be right-padded with space
characters."
  (with-bounding-marks ((start-mark end-mark) mark startcol endcol)
    (let ((str (concatenate 'string (buffer-substring (buffer mark)
                                                      (offset start-mark)
                                                      (offset end-mark))
                            (make-string (- (- endcol startcol)
                                            (- (column-number end-mark) (column-number start-mark)))
                                         :initial-element #\Space))))
      (delete-range start-mark (- (offset end-mark) (offset start-mark)))
      str)))

(defun delete-rectangle-line (mark startcol endcol)
  "For the line that `mark' is in, delete the string
between column `startcol' and `endcol'."
  (with-bounding-marks ((start-mark end-mark) mark startcol endcol)
    (delete-range start-mark (- (offset end-mark) (offset start-mark)))))

(defun open-rectangle-line (mark startcol endcol)
  "For the line that `mark' is in, move the string between column
`startcol' and `endcol' to the right, replacing the area previously
inhabited by it with space characters."
  (with-bounding-marks ((start-mark end-mark) mark startcol endcol)
    (unless (mark= start-mark end-mark)
      (insert-sequence start-mark (make-string (- endcol startcol) :initial-element #\Space)))))

(defun clear-rectangle-line (mark startcol endcol)
  "For the line that `mark' is in, replace the string between column
`startcol' and `endcol' with space characters."
  (with-bounding-marks ((start-mark end-mark) mark startcol endcol)
    (let ((size (- (offset end-mark) (offset start-mark))))
      (delete-range start-mark size)
      (insert-sequence start-mark (make-string size :initial-element #\Space)))))

(defun delete-rectangle-line-whitespace (mark startcol endcol)
  "For the line that `mark' is in, delete all whitespace characters
from `startcol' up to the first non-whitespace character."
  (with-bounding-marks ((start-mark end-mark) mark startcol endcol)
    (let ((target-mark (clone-mark start-mark)))
      (re-search-forward target-mark "[^ ]")
      (when (= (line-number start-mark) (line-number target-mark))
        (delete-range start-mark (- (offset target-mark) (offset start-mark) 1))))))

(defun replace-rectangle-line (mark startcol endcol string)
  "For the line that `mark' is in, replace the string between column
`startcol' and `endcol' with `string'."
  (with-bounding-marks ((start-mark end-mark) mark startcol endcol :force-start t)
    (delete-range start-mark (- (offset end-mark) (offset start-mark)))
    (insert-sequence start-mark string)))

(defun insert-in-rectangle-line (mark startcol endcol string)
    "For the line that `mark' is in, move the string between column
`startcol' and `endcol' to the right, replacing the area previously
inhabited by it with the contents of `string'."
  (with-bounding-marks ((start-mark end-mark) mark startcol endcol :force-start t)
    (insert-sequence start-mark string)))

(defun insert-rectangle-at-mark (view mark rectangle)
  "Yank the killed rectangle, positioning the upper left corner at
current point."
  (let ((insert-column (column-number mark)))
    (dolist (line rectangle) 
      (move-to-column mark insert-column t)
      (insert-sequence mark line)
      (unless (forward-line mark (syntax view) 1 nil)
        (open-line mark)
        (forward-object mark)))))
