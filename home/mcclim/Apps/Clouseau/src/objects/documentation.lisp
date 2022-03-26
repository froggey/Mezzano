;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clouseau)

(defun remove-common-leading-whitespace (string)
  (let ((length      (length string))
        (i           0)
        (start       0)
        (min-leading nil)
        (lines       '()))
    (flet ((note-leading (leading)
             (when (or (not min-leading) (< leading min-leading))
               (setf min-leading leading)))
           (get-character ()
             (when (< i length)
               (aref string i)))
           (next ()
             (incf i))
           (output ()
             (let ((lines (nreverse lines)))
               (return-from remove-common-leading-whitespace
                 (with-output-to-string (stream)
                   (write-string string stream :start (car (first lines)) :end (cdr (first lines)))
                   (loop :for (start . end) :in (rest lines)
                         :do (write-string string stream :start (+ start min-leading) :end end)))))))
      (tagbody
       :first
         (case (get-character)
           ((nil)
            (push (cons start i) lines)
            (output))
           (#\Newline
            (push (cons start (1+ i)) lines)
            (setf start (1+ i))
            (next) (go :whitespace))
           (t
            (next) (go :first)))
       :whitespace
         (case (get-character)
           ((nil)
            (note-leading (- start i))
            (push (cons start i) lines)
            (output))
           (#\Newline
            (note-leading (- start i))
            (push (cons start i) lines)
            (setf start i)
            (next) (go :whitespace))
           (#\Space
            (next) (go :whitespace))
           (t
            (note-leading (- i start))
            (next) (go :content)))
       :content
         (case (get-character)
           ((nil)
            (push (cons start i) lines)
            (output))
           (#\Newline
            (push (cons start (1+ i)) lines)
            (setf start (1+ i))
            (next) (go :whitespace))
           (t
            (next) (go :content)))))))

(defun clean-documentation (string)
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline) string))
         (lines   (remove-common-leading-whitespace trimmed)))
    lines))

(defun print-documentation (object stream &key (namespace t))
  (when-let ((documentation (handler-case (documentation object namespace)
                              (error ())
                              (warning ()))))
    (let ((clean (clean-documentation documentation)))
      (with-preserved-cursor-x (stream)
        (surrounding-output-with-border (stream :shape       :rectangle
                                                :padding     2
                                                :background  +beige+
                                                :outline-ink +light-goldenrod+
                                                :filled      t)
          (write-string clean stream))))))
