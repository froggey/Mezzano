;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; external-format.lisp --- External format classes and functions.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:babel)

(defvar *default-eol-style*
  #+windows :crlf
  #-windows :lf
  "The end-of-line style used by external formats if none is
explicitly given.  Depends on the OS the code is compiled on.")

(deftype eol-style ()
  "Possible end-of-line styles."
  '(member :cr :lf :crlf))

(defclass external-format ()
  ((encoding :initarg :encoding :reader external-format-encoding
             :type character-encoding)
   (eol-style :initarg :eol-style :reader external-format-eol-style
              :type eol-style :initform *default-eol-style*))
  (:documentation
   "An EXTERNAL-FORMAT consists in a combination of a Babel
CHARACTER-ENCODING and an end-of-line style."))

(defmethod print-object ((ef external-format) stream)
  (print-unreadable-object (ef stream :type t :identity t)
    (format stream "~A ~A"
            (enc-name (external-format-encoding ef))
            (external-format-eol-style ef))))

;;; This interface is still somewhat sketchy.  The rest of Babel
;;; doesn't really understand external formats, for instance.
(defun make-external-format (encoding &key (eol-style *default-eol-style*))
  (check-type eol-style eol-style)
  (make-instance 'external-format
                 :encoding (get-character-encoding encoding)
                 :eol-style eol-style))

(defun ensure-external-format (thing)
  (etypecase thing
    (external-format thing)
    (character-encoding (make-instance 'external-format :encoding thing))
    (symbol (make-external-format thing))
    (list (apply #'make-external-format thing))))

(defun external-format-equal (ef1 ef2)
  (and (eq (external-format-encoding ef1) (external-format-encoding ef2))
       (eq (external-format-eol-style ef1) (external-format-eol-style ef2))))

(declaim (inline lookup-mapping))
(defun lookup-mapping (ht encoding)
  "HT should be an hashtable created by
INSTANTIATE-CONCRETE-MAPPINGS. ENCODING should be either an
external format, an encoding object or a keyword symbol
denoting a character encoding name or one of its aliases."
  (or (etypecase encoding
        (keyword
         (gethash encoding ht))
        (babel-encodings::concrete-mapping
         encoding)
        (character-encoding
         (gethash (enc-name encoding) ht))
        (external-format
         (gethash (enc-name (external-format-encoding encoding)) ht)))
      (error "~S is not a valid encoding designator" encoding)))
