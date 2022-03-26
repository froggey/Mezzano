;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/input.lisp,v 1.78 2008/05/25 19:25:44 edi Exp $

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

#-:lispworks
(defmethod read-byte* ((flexi-input-stream flexi-input-stream))
  "Reads one byte \(octet) from the underlying stream of
FLEXI-OUTPUT-STREAM \(or from the internal stack if it's not
empty)."
  (declare #.*standard-optimize-settings*)
  ;; we're using S instead of STREAM here because of an
  ;; issue with SBCL:
  ;; <http://article.gmane.org/gmane.lisp.steel-bank.general/1386>
  (with-accessors ((position flexi-stream-position)
                   (bound flexi-stream-bound)
                   (octet-stack flexi-stream-octet-stack)
                   (s flexi-stream-stream))
      flexi-input-stream
    (declare (integer position)
             (type (or null integer) bound))
    (when (and bound
               (>= position bound))
      (return-from read-byte* nil))
    (incf position)
    (or (pop octet-stack)
        (read-byte s nil nil)
        (progn (decf position) nil))))

#+:lispworks
(defmethod read-byte* ((flexi-input-stream flexi-input-stream))
  "Reads one byte \(octet) from the underlying \(binary) stream of
FLEXI-OUTPUT-STREAM \(or from the internal stack if it's not empty)."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((position flexi-stream-position)
                   (bound flexi-stream-bound)
                   (octet-stack flexi-stream-octet-stack)
                   (stream flexi-stream-stream))
      flexi-input-stream
    (declare (integer position)
             (type (or null integer) bound))
    (when (and bound
               (>= position bound))
      (return-from read-byte* nil))
    (incf position)
    (or (pop octet-stack)
        (read-byte stream nil nil)
        (progn (decf position) nil))))

#+:lispworks
(defmethod read-byte* ((flexi-input-stream flexi-char-input-stream))
  "Reads one byte \(octet) from the underlying stream of
FLEXI-OUTPUT-STREAM \(or from the internal stack if it's not empty).
Only used for LispWorks bivalent streams which aren't binary."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((position flexi-stream-position)
                   (bound flexi-stream-bound)
                   (octet-stack flexi-stream-octet-stack)
                   (stream flexi-stream-stream))
      flexi-input-stream
    (declare (integer position)
             (type (or null integer) bound))
    (when (and bound
               (>= position bound))
      (return-from read-byte* nil))
    (incf position)
    (or (pop octet-stack)
        ;; we use READ-SEQUENCE because READ-BYTE doesn't work with all
        ;; bivalent streams in LispWorks
        (let* ((buffer (make-array 1 :element-type 'octet))
               (new-position (read-sequence buffer stream)))
          (cond ((zerop new-position)
                 (decf position) nil)
                (t (aref buffer 0)))))))

(defmethod stream-clear-input ((flexi-input-stream flexi-input-stream))
  "Calls the corresponding method for the underlying input stream
and also clears the value of the OCTET-STACK slot."
  (declare #.*standard-optimize-settings*)
  ;; note that we don't reset the POSITION slot
  (with-accessors ((octet-stack flexi-stream-octet-stack)
                   (stream flexi-stream-stream))
      flexi-input-stream
    (setq octet-stack nil)
    (clear-input stream)))

(defmethod stream-listen ((flexi-input-stream flexi-input-stream))
  "Calls the corresponding method for the underlying input stream
but first checks if \(old) input is available in the OCTET-STACK
slot."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((position flexi-stream-position)
                   (bound flexi-stream-bound)
                   (octet-stack flexi-stream-octet-stack)
                   (stream flexi-stream-stream))
      flexi-input-stream
    (declare (integer position)
             (type (or null integer) bound))
    (when (and bound
               (>= position bound))
      (return-from stream-listen nil))
    (or octet-stack (listen stream))))

(defmethod stream-read-byte ((stream flexi-input-stream))
  "Reads one byte \(octet) from the underlying stream."
  (declare #.*standard-optimize-settings*)
  ;; set LAST-CHAR-CODE slot to NIL because we can't UNREAD-CHAR after
  ;; this operation
  (with-accessors ((last-char-code flexi-stream-last-char-code)
                   (last-octet flexi-stream-last-octet))
      stream
    (setq last-char-code nil)
    (let ((octet (read-byte* stream)))
      (setq last-octet octet)
      (or octet :eof))))

(defun unread-char% (char flexi-input-stream)
  "Used internally to put a character CHAR which was already read back
on the stream.  Uses the OCTET-STACK slot and decrements the POSITION
slot accordingly."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((position flexi-stream-position)
                   (octet-stack flexi-stream-octet-stack)
                   (external-format flexi-stream-external-format))
      flexi-input-stream
    (let ((counter 0) octets-reversed)
      (declare (fixnum counter))
      (flet ((writer (octet)
               (incf counter)
               (push octet octets-reversed)))
        (declare (dynamic-extent (function writer)))
        (char-to-octets external-format char #'writer)
        (decf position counter)
        (setq octet-stack (nreconc octets-reversed octet-stack))))))

(defmethod stream-read-char ((stream flexi-input-stream))
  (declare #.*standard-optimize-settings*)
  ;; note that we do nothing for the :LF EOL style because we assume
  ;; that #\Newline is the same as #\Linefeed in all Lisps which will
  ;; use this library
  (with-accessors ((external-format flexi-stream-external-format)
                   (last-octet flexi-stream-last-octet)
                   (last-char-code flexi-stream-last-char-code))
      stream
    ;; set LAST-OCTET slot to NIL because we can't UNREAD-BYTE after
    ;; this operation
    (setq last-octet nil)
    (flet ((reader ()
             (read-byte* stream))
           (unreader (char)
             (unread-char% char stream)))
      (declare (dynamic-extent (function reader) (function unreader)))
      (let* ((*current-unreader* #'unreader)
             (char-code (or (octets-to-char-code external-format #'reader)
                            (return-from stream-read-char :eof))))
        ;; remember this character and its char code for UNREAD-CHAR
        (setq last-char-code char-code)
        (or (code-char char-code) char-code)))))

(defmethod stream-read-char-no-hang ((stream flexi-input-stream))
  "Reads one character if the underlying stream has at least one
octet available."
  (declare #.*standard-optimize-settings*)
  ;; note that this may block for non-8-bit encodings - I think
  ;; there's no easy way to handle this correctly
  (and (stream-listen stream)
       (stream-read-char stream)))

(defmethod stream-read-sequence ((flexi-input-stream flexi-input-stream) sequence start end &key)
  "An optimized version which uses a buffer underneath.  The function
can deliver characters as well as octets and it decides what to do
based on the element type of the sequence \(which takes precedence)
and the element type of the stream.  What you'll really get might also
depend on your Lisp.  Some of the implementations are more picky than
others - see for example FLEXI-STREAMS-TEST::SEQUENCE-TEST."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start end))
  (with-accessors ((octet-stack flexi-stream-octet-stack)
                   (external-format flexi-stream-external-format)
                   (last-octet flexi-stream-last-octet)
                   (last-char-code flexi-stream-last-char-code)
                   (element-type flexi-stream-element-type)
                   (stream flexi-stream-stream))
      flexi-input-stream
    (when (>= start end)
      (return-from stream-read-sequence start))
    (when (or (subtypep (etypecase sequence
                          (vector (array-element-type sequence))
                          (list t))
                        'integer)
              (and (not (stringp sequence))
                   (type-equal element-type 'octet)))
      ;; if binary data is requested, just read from the underlying
      ;; stream directly and skip the rest (but flush octet stack
      ;; first)
      (let ((index start))
        (declare (fixnum index))
        (when octet-stack
          (replace sequence octet-stack :start1 start :end1 end)
          (let ((octets-flushed (min (length octet-stack) (- end start))))
            (incf index octets-flushed)
            (setq octet-stack (nthcdr octets-flushed octet-stack))))
        (setq index (read-sequence sequence stream :start index :end end))
        (when (> index start)
          (setq last-char-code nil
                last-octet (elt sequence (1- index))))
        (return-from stream-read-sequence index)))
    ;; otherwise hand over to the external format to do the work
    (read-sequence* external-format flexi-input-stream sequence start end)))

(defmethod stream-unread-char ((stream flexi-input-stream) char)
  "Implements UNREAD-CHAR for streams of type FLEXI-INPUT-STREAM.
Makes sure CHAR will only be unread if it was the last character
read and if it was read with the same encoding that's currently
being used by the stream."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((last-char-code flexi-stream-last-char-code))
      stream
    (unless last-char-code
      (error 'flexi-stream-error
             :format-control "No character to unread from this stream \(or external format has changed or last reading operation was binary)."))
    (unless (= (char-code char) last-char-code)
      (error 'flexi-stream-error
             :format-control "Last character read (~S) was different from ~S."
             :format-arguments (list (code-char last-char-code) char)))
    (unread-char% char stream)
    (setq last-char-code nil)
    nil))

(defmethod unread-byte (byte (flexi-input-stream flexi-input-stream))
  "Similar to UNREAD-CHAR in that it `unreads' the last octet from
STREAM.  Note that you can only call UNREAD-BYTE after a corresponding
READ-BYTE."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((last-octet flexi-stream-last-octet)
                   (octet-stack flexi-stream-octet-stack)
                   (position flexi-stream-position))
      flexi-input-stream
    (unless last-octet
      (error 'flexi-stream-error
             :format-control "No byte to unread from this stream \(or last reading operation read a character)."))
    (unless (= byte last-octet)
      (error 'flexi-stream-error
             :format-control "Last byte read was different from #x~X."
             :format-arguments (list byte)))
    (setq last-octet nil)
    (decf (the integer position))
    (push byte octet-stack)
    nil))
    
(defmethod peek-byte ((flexi-input-stream flexi-input-stream)
                      &optional peek-type (eof-error-p t) eof-value)
  "PEEK-BYTE is like PEEK-CHAR, i.e. it returns an octet from
FLEXI-INPUT-STREAM without actually removing it.  If PEEK-TYPE is NIL
the next octet is returned, if PEEK-TYPE is T, the next octet which is
not 0 is returned, if PEEK-TYPE is an octet, the next octet which
equals PEEK-TYPE is returned.  EOF-ERROR-P and EOF-VALUE are
interpreted as usual."
  (declare #.*standard-optimize-settings*)
  (loop for octet = (read-byte flexi-input-stream eof-error-p eof-value)
        until (cond ((null peek-type))
                    ((eql octet eof-value))
                    ((eq peek-type t)
                     (plusp octet))
                    (t (= octet peek-type)))
        finally (unless (eql octet eof-value)
                  (unread-byte octet flexi-input-stream))
                (return octet)))