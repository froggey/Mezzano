;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/lw-char-stream.lisp,v 1.1 2008/05/23 14:43:09 edi Exp $

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

(defclass flexi-char-output-stream (flexi-output-stream)
  ()
  (:documentation "This class is for output streams where the
underlying stream is bivalent but not binary.  It exists solely for
the purpose of optimizing output to binary streams on LispWorks.  See
WRITE-BYTE*."))

(defclass flexi-char-input-stream (flexi-input-stream)
  ()
  (:documentation "This class is for input streams where the
underlying stream is bivalent but not binary.  It exists solely for
the purpose of optimizing input to binary streams on LispWorks.  See
READ-BYTE*."))

(defclass flexi-char-io-stream (flexi-char-input-stream flexi-char-output-stream flexi-io-stream)
  ()
  (:documentation "This class is for bidirectional streams where the
underlying stream is bivalent but not binary.  It exists solely for
the purpose of optimizing input and output from/to binary streams on
LispWorks.  See READ-BYTE* and WRITE-BYTE*."))

(defmethod initialize-instance :after ((flexi-stream flexi-output-stream) &rest initargs)
  "Might change the class of FLEXI-STREAM for optimization purposes.
Only needed for LispWorks."
  (declare #.*standard-optimize-settings*)
  (declare (ignore initargs))
  (with-accessors ((stream flexi-stream-stream))
      flexi-stream
    (unless (subtypep (stream-element-type stream) 'octet)
      (change-class flexi-stream
                    (typecase flexi-stream
                      (flexi-io-stream 'flexi-char-io-stream)
                      (otherwise 'flexi-char-output-stream))))))

(defmethod initialize-instance :after ((flexi-stream flexi-input-stream) &rest initargs)
  "Might change the class of FLEXI-STREAM for optimization purposes.
Only needed for LispWorks."
  (declare #.*standard-optimize-settings*)
  (declare (ignore initargs))
  (with-accessors ((stream flexi-stream-stream))
      flexi-stream
    (unless (subtypep (stream-element-type stream) 'octet)
      (change-class flexi-stream
                    (typecase flexi-stream
                      (flexi-io-stream 'flexi-char-io-stream)
                      (otherwise 'flexi-char-input-stream))))))
