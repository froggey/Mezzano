;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/output.lisp,v 1.65 2008/05/24 23:15:25 edi Exp $

;;; Copyright (c) 2005-2008, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

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

(in-package :flexi-streams)

(defgeneric write-byte* (byte stream)
  (declare #.*standard-optimize-settings*)
  (:documentation "Writes one byte \(octet) to the underlying stream
STREAM."))

#-:lispworks
(defmethod write-byte* (byte (flexi-output-stream flexi-output-stream))  
  (declare #.*standard-optimize-settings*)
  (with-accessors ((stream flexi-stream-stream))
      flexi-output-stream
    (write-byte byte stream)))

#+:lispworks
(defmethod write-byte* (byte (flexi-output-stream flexi-output-stream))
  (declare #.*standard-optimize-settings*)
  (with-accessors ((stream flexi-stream-stream))
      flexi-output-stream
    (write-byte byte stream)))

#+:lispworks
(defmethod write-byte* (byte (flexi-output-stream flexi-char-output-stream))
  "This method is only used for LispWorks bivalent streams which
aren't binary."
  (declare #.*standard-optimize-settings*)
  ;; we use WRITE-SEQUENCE because WRITE-BYTE doesn't work with all
  ;; bivalent streams in LispWorks (4.4.6)
  (with-accessors ((stream flexi-stream-stream))
      flexi-output-stream
    (write-sequence (make-array 1 :element-type 'octet
                                :initial-element byte)
                    stream)
    byte))

(defmethod stream-write-char ((stream flexi-output-stream) char)
  (declare #.*standard-optimize-settings*)
  (with-accessors ((external-format flexi-stream-external-format))
      stream
    (flet ((writer (octet)
             (write-byte* octet stream)))
      (declare (dynamic-extent (function writer)))
      (char-to-octets external-format char #'writer))))

(defmethod stream-write-char :after ((stream flexi-output-stream) char)
  (declare #.*standard-optimize-settings*)
  ;; update the column unless we're in the middle of the line and
  ;; the current value is NIL
  (with-accessors ((column flexi-stream-column))
      stream
    (cond ((char= char #\Newline) (setq column 0))
          (column (incf (the integer column))))))

(defmethod stream-clear-output ((flexi-output-stream flexi-output-stream))
  "Simply calls the corresponding method for the underlying
output stream."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((stream flexi-stream-stream))
      flexi-output-stream
    (clear-output stream)))

(defmethod stream-finish-output ((flexi-output-stream flexi-output-stream))
  "Simply calls the corresponding method for the underlying
output stream."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((stream flexi-stream-stream))
      flexi-output-stream
    (finish-output stream)))

(defmethod stream-force-output ((flexi-output-stream flexi-output-stream))
  "Simply calls the corresponding method for the underlying
output stream."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((stream flexi-stream-stream))
      flexi-output-stream
    (force-output stream)))

(defmethod stream-line-column ((flexi-output-stream flexi-output-stream))
  "Returns the column stored in the COLUMN slot of the
FLEXI-OUTPUT-STREAM object STREAM."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((column flexi-stream-column))
      flexi-output-stream
    column))

(defmethod stream-write-byte ((flexi-output-stream flexi-output-stream) byte)
  "Writes a byte \(octet) to the underlying stream."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((column flexi-stream-column))
      flexi-output-stream
    ;; set column to NIL because we don't know how to handle binary
    ;; output mixed with character output
    (setq column nil)
    (write-byte* byte flexi-output-stream)))

#+:allegro
(defmethod stream-terpri ((stream flexi-output-stream))
  "Writes a #\Newline character to the underlying stream."
  (declare #.*standard-optimize-settings*)
  ;; needed for AllegroCL - grrr...
  (stream-write-char stream #\Newline))

(defmethod stream-write-sequence ((flexi-output-stream flexi-output-stream) sequence start end &key)
  "An optimized version which uses a buffer underneath.  The function
can accepts characters as well as octets and it decides what to do
based on the element type of the sequence \(if possible) or on the
individual elements, i.e. you can mix characters and octets in
SEQUENCE if you want.  Whether that really works might also depend on
your Lisp, some of the implementations are more picky than others."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start end))
  (with-accessors ((column flexi-stream-column)
                   (external-format flexi-stream-external-format)
                   (stream flexi-stream-stream))
      flexi-output-stream
    (when (>= start end)
      (return-from stream-write-sequence sequence))
    (when (and (vectorp sequence)
               (subtypep (array-element-type sequence) 'integer))
      ;; if this is pure binary output, just send all the stuff to the
      ;; underlying stream directly and skip the rest
      (setq column nil)
      (return-from stream-write-sequence
        (write-sequence sequence stream :start start :end end)))
    ;; otherwise hand over to the external format to do the work
    (write-sequence* external-format flexi-output-stream sequence start end))
  sequence)

(defmethod stream-write-string ((stream flexi-output-stream) string
                                &optional (start 0) (end (length string)))
  "Simply hands over to the optimized method for STREAM-WRITE-SEQUENCE."
  (declare #.*standard-optimize-settings*)
  (stream-write-sequence stream string start (or end (length string))))
