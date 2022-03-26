;;;; Copyright (C) 2019 Jan Moringen
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

;;; Object inspection methods

(defmethod inspect-object-using-state ((object character)
                                       (state  inspected-object)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (inspect-class-as-name (class-of object) stream)
  (write-char #\Space stream)
  (call-next-method))

(defmethod inspect-object-using-state :after ((object character)
                                              (state  inspected-object)
                                              (style  (eql :badges))
                                              (stream t))
  (macrolet ((attribute (predicate label)
               `(when (,predicate object)
                  (write-char #\Space stream)
                  (badge stream ,label))))
    (attribute graphic-char-p "graphic")
    (attribute digit-char-p   "digit")
    (attribute alpha-char-p   "alpha")
    (attribute alphanumericp  "alphanumeric")
    (attribute upper-case-p   "upper-case")
    (attribute lower-case-p   "lower-case")))

(defmethod inspect-object-using-state ((object character)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (formatting-table (stream)
    (format-place-row stream object 'reader-place 'char-name :label "Name")
    (format-place-row stream object 'reader-place 'char-code :label "Code")
    (when-let ((weight (digit-char-p object)))
      (format-place-row stream object 'pseudo-place weight :label "Weight"))))

;;; No circularity tracking for characters.
(defmethod note-object-occurrence ((object       character)
                                   (state        inspected-object)
                                   (presentation t)
                                   (stream       t)))
