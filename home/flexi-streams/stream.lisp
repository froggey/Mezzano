;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/stream.lisp,v 1.61 2008/05/19 22:32:56 edi Exp $

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

(defclass flexi-stream (trivial-gray-stream-mixin)
  ((stream :initarg :stream
           :reader flexi-stream-stream
           :documentation "The actual stream that's used for
input and/or output.  It must be capable of reading/writing
octets with READ-SEQUENCE and/or WRITE-SEQUENCE.")
   (external-format :initform (make-external-format :iso-8859-1)
                    :initarg :flexi-stream-external-format
                    :accessor flexi-stream-external-format
                    :documentation "The encoding currently used
by this stream.  Can be changed on the fly.")
   (element-type :initform 'char*
                 :initarg :element-type
                 :accessor flexi-stream-element-type
                 :documentation "The element type of this stream."))
  (:documentation "A FLEXI-STREAM object is a stream that's
`layered' atop an existing binary/bivalent stream in order to
allow for multi-octet external formats.  FLEXI-STREAM itself is a
mixin and should not be instantiated."))

(defmethod initialize-instance :after ((flexi-stream flexi-stream) &rest initargs)
  "Makes sure the EXTERNAL-FORMAT and ELEMENT-TYPE slots contain
reasonable values."
  (declare #.*standard-optimize-settings*)
  (declare (ignore initargs))
  (with-accessors ((external-format flexi-stream-external-format)
                   (element-type flexi-stream-element-type))
      flexi-stream
    (unless (or (subtypep element-type 'character)
                (subtypep element-type 'octet))
      (error 'flexi-stream-element-type-error
             :element-type element-type
             :stream flexi-stream))
    (setq external-format (maybe-convert-external-format external-format))))

(defmethod (setf flexi-stream-external-format) :around (new-value (flexi-stream flexi-stream))
  "Converts the new value to an EXTERNAL-FORMAT object if
necessary."
  (declare #.*standard-optimize-settings*)
  (call-next-method (maybe-convert-external-format new-value) flexi-stream))

(defmethod (setf flexi-stream-element-type) :before (new-value (flexi-stream flexi-stream))
  "Checks whether the new value makes sense before it is set."
  (declare #.*standard-optimize-settings*)
  (unless (or (subtypep new-value 'character)
              (type-equal new-value 'octet))
    (error 'flexi-stream-element-type-error
           :element-type new-value
           :stream flexi-stream)))

(defmethod stream-element-type ((stream flexi-stream))
  "Returns the element type that was provided by the creator of
the stream."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((element-type flexi-stream-element-type))
      stream
    element-type))

(defmethod close ((stream flexi-stream) &key abort)
  "Closes the flexi stream by closing the underlying `real'
stream."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((stream flexi-stream-stream))
      stream
    (cond ((open-stream-p stream)
           (close stream :abort abort))
          (t nil))))

(defmethod open-stream-p ((stream flexi-stream))
  "A flexi stream is open if its underlying stream is open."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((stream flexi-stream-stream))
      stream
    (open-stream-p stream)))

(defmethod stream-file-position ((stream flexi-stream))
  "Dispatch to method for underlying stream."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((stream flexi-stream-stream))
      stream
    (file-position stream)))

(defmethod (setf stream-file-position) (position-spec (stream flexi-stream))
  "Dispatch to method for underlying stream."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((underlying-stream flexi-stream-stream))
      stream
    (if (file-position underlying-stream position-spec)
        (setf (flexi-stream-position stream) (file-position underlying-stream))
          nil)))

(defclass flexi-output-stream (flexi-stream fundamental-binary-output-stream
                                            fundamental-character-output-stream)
  ((column :initform 0
           :accessor flexi-stream-column
           :documentation "The current output column.  A
non-negative integer or NIL."))
  (:documentation "A FLEXI-OUTPUT-STREAM is a FLEXI-STREAM that
can actually be instatiated and used for output.  Don't use
MAKE-INSTANCE to create a new FLEXI-OUTPUT-STREAM but use
MAKE-FLEXI-STREAM instead."))

#+:cmu
(defmethod input-stream-p ((stream flexi-output-stream))
  "Explicitly states whether this is an input stream."
  (declare #.*standard-optimize-settings*)
  nil)

(defclass flexi-input-stream (flexi-stream fundamental-binary-input-stream
                                           fundamental-character-input-stream)
  ((last-char-code :initform nil
                   :accessor flexi-stream-last-char-code
                   :documentation "This slot either holds NIL or the
last character \(code) read successfully.  This is mainly used for
UNREAD-CHAR sanity checks.")
   (last-octet :initform nil
               :accessor flexi-stream-last-octet
               :documentation "This slot either holds NIL or the last
octet read successfully from the stream using a `binary' operation
such as READ-BYTE.  This is mainly used for UNREAD-BYTE sanity
checks.")
   (octet-stack :initform nil
                :accessor flexi-stream-octet-stack
                :documentation "A small buffer which holds octets
that were already read from the underlying stream but not yet
used to produce characters.  This is mainly used if we have to
look ahead for a CR/LF line ending.")
   (position :initform 0
             :initarg :position
             :type integer
             :accessor flexi-stream-position
             :documentation "The position within the stream where each
octet read counts as one.")
   (bound :initform nil
          :initarg :bound
          :type (or null integer)
          :accessor flexi-stream-bound
          :documentation "When this is not NIL, it must be an integer
and the stream will behave as if no more data is available as soon as
POSITION is greater or equal than this value."))
  (:documentation "A FLEXI-INPUT-STREAM is a FLEXI-STREAM that
can actually be instatiated and used for input.  Don't use
MAKE-INSTANCE to create a new FLEXI-INPUT-STREAM but use
MAKE-FLEXI-STREAM instead."))

#+:cmu
(defmethod output-stream-p ((stream flexi-input-stream))
  "Explicitly states whether this is an output stream."
  (declare #.*standard-optimize-settings*)
  nil)

(defclass flexi-io-stream (flexi-input-stream flexi-output-stream)
  ()
  (:documentation "A FLEXI-IO-STREAM is a FLEXI-STREAM that can
actually be instatiated and used for input and output.  Don't use
MAKE-INSTANCE to create a new FLEXI-IO-STREAM but use
MAKE-FLEXI-STREAM instead."))

#+:cmu
(defmethod input-stream-p ((stream flexi-io-stream))
  "Explicitly states whether this is an input stream."
  (declare #.*standard-optimize-settings*)
  t)

#+:cmu
(defmethod output-stream-p ((stream flexi-io-stream))
  "Explicitly states whether this is an output stream."
  (declare #.*standard-optimize-settings*)
  t)

(defun make-flexi-stream (stream &rest args
                                 &key (external-format (make-external-format :iso-8859-1))
                                      element-type column position bound)
  "Creates and returns a new flexi stream.  STREAM must be an open
binary or `bivalent' stream, i.e. it must be capable of
reading/writing octets with READ-SEQUENCE and/or WRITE-SEQUENCE.  The
resulting flexi stream is an input stream if and only if STREAM is an
input stream.  Likewise, it's an output stream if and only if STREAM
is an output stream.  The default for ELEMENT-TYPE is LW:SIMPLE-CHAR
on LispWorks and CHARACTER on other Lisps.  EXTERNAL-FORMAT must be an
EXTERNAL-FORMAT object or a symbol or a list denoting such an object.
COLUMN is the initial column of the stream which is either a
non-negative integer or NIL.  The COLUMN argument must only be used
for output streams.  POSITION \(only used for input streams) should be
an integer and it denotes the position the stream is in - it will be
increased by one for each octet read.  BOUND \(only used for input
streams) should be NIL or an integer.  If BOUND is not NIL and
POSITION has gone beyond BOUND, then the stream will behave as if no
more input is available."
  (declare #.*standard-optimize-settings*)
  ;; these arguments are ignored - they are only there to provide a
  ;; meaningful parameter list for IDEs
  (declare (ignore element-type column position bound))
  (unless (and (streamp stream)
               (open-stream-p stream))
    (error "~S should have been an open stream." stream))
  (apply #'make-instance
         ;; actual type depends on STREAM
         (cond ((and (input-stream-p stream)
                     (output-stream-p stream))
                'flexi-io-stream)
               ((input-stream-p stream)
                'flexi-input-stream)
               ((output-stream-p stream)
                'flexi-output-stream)
               (t
                (error "~S is neither an input nor an output stream." stream)))
         :stream stream
         :flexi-stream-external-format external-format
         (sans args :external-format)))
