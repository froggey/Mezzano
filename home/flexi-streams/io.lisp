;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/io.lisp,v 1.2 2008/05/20 23:44:45 edi Exp $

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

(defmethod reset-input-state ((flexi-io-stream flexi-io-stream))
  "This method is used to clear any state associated with previous
input before output is attempted on the stream.  It can fail if the
octet stack is not empty and the stream can't be `rewound'."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((last-char-code flexi-stream-last-char-code)
                   (last-octet flexi-stream-last-octet)
                   (octet-stack flexi-stream-octet-stack)
                   (stream flexi-stream-stream))
      flexi-io-stream
    (when octet-stack
      (unless (maybe-rewind stream (length octet-stack))
        (error 'flexi-stream-out-of-sync-error
               :stream flexi-io-stream))
      (setq octet-stack nil))
    (setq last-octet nil
          last-char-code nil)))

(defmethod stream-write-byte :before ((stream flexi-io-stream) byte)
  (declare #.*standard-optimize-settings*)
  (declare (ignore byte))
  (reset-input-state stream))
  
(defmethod stream-write-char :before ((stream flexi-io-stream) char)
  (declare #.*standard-optimize-settings*)
  (declare (ignore char))
  (reset-input-state stream))
  
(defmethod stream-write-sequence :before ((stream flexi-io-stream) sequence start end &key)
  (declare #.*standard-optimize-settings*)
  (declare (ignore sequence start end))
  (reset-input-state stream))
  
(defmethod stream-clear-output :before ((stream flexi-io-stream))
  (declare #.*standard-optimize-settings*)
  (reset-input-state stream))

(defmethod reset-output-state ((flexi-io-stream flexi-io-stream))
  "This method is used to clear any state associated with previous
output before the stream is used for input."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((column flexi-stream-column))
      flexi-io-stream
    (setq column nil)))
  
(defmethod stream-read-byte :before ((stream flexi-io-stream))
  (declare #.*standard-optimize-settings*)
  (reset-output-state stream))
  
(defmethod stream-read-char :before ((stream flexi-io-stream))
  (declare #.*standard-optimize-settings*)
  (reset-output-state stream))

(defmethod stream-read-sequence :before ((stream flexi-io-stream) sequence start end &key)
  (declare #.*standard-optimize-settings*)
  (declare (ignore sequence start end))
  (reset-output-state stream))

(defmethod stream-unread-char :before ((stream flexi-io-stream) char)
  (declare #.*standard-optimize-settings*)
  (declare (ignore char))
  (reset-output-state stream))
  
(defmethod unread-byte :before (byte (stream flexi-io-stream))
  (declare #.*standard-optimize-settings*)
  (declare (ignore byte))
  (reset-output-state stream))
  
(defmethod stream-clear-input :before ((stream flexi-io-stream))
  (declare #.*standard-optimize-settings*)
  (reset-output-state stream))

(defmethod write-byte* :after (byte (stream flexi-io-stream))
  "Keep POSITION slot up to date even when performing output."
  (declare #.*standard-optimize-settings*)
  (declare (ignore byte))
  (with-accessors ((position flexi-stream-position))
      stream
    (incf position)))