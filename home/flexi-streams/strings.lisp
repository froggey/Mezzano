;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/strings.lisp,v 1.34 2008/05/26 10:55:08 edi Exp $

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

(defun string-to-octets (string &key
                                (external-format :latin1)
                                (start 0) (end (length string)))
  "Converts the Lisp string STRING from START to END to an array of
octets corresponding to the external format designated by
EXTERNAL-FORMAT.

In spite of the name, STRING can be any sequence of characters, but
the function is optimized for strings."
  (declare #.*standard-optimize-settings*)
  (setq external-format (maybe-convert-external-format external-format))
  ;; the external format knows how to do it...
  (string-to-octets* external-format string start end))

(defun octets-to-string (sequence &key
                                  (external-format :latin1)
                                  (start 0) (end (length sequence)))
  "Converts the Lisp sequence SEQUENCE of octets from START to END to
a string using the external format designated by EXTERNAL-FORMAT.

This function is optimized for the case of SEQUENCE being a vector.
Don't use lists if you're in a hurry."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start end))
  (setq external-format (maybe-convert-external-format external-format))
  ;; the external format knows how to do it...
  (octets-to-string* external-format sequence start end))

(defun octet-length (string &key (external-format :latin1) (start 0) (end (length string)))
  "Returns the length of the substring of STRING from START to END in
octets if encoded using the external format EXTERNAL-FORMAT.

In spite of the name, STRING can be any sequence of characters, but
the function is optimized for strings."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start end))
  (setq external-format (maybe-convert-external-format external-format))
  (compute-number-of-octets external-format string start end))

(defun char-length (sequence &key (external-format :latin1) (start 0) (end (length sequence)))
  "Kind of the inverse of OCTET-LENGTH.  Returns the length of the
subsequence \(of octets) of SEQUENCE from START to END in characters
if decoded using the external format EXTERNAL-FORMAT.  Note that this
function doesn't check for the validity of the data in SEQUENCE.

This function is optimized for the case of SEQUENCE being a vector.
Don't use lists if you're in a hurry."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start end))
  (setq external-format (maybe-convert-external-format external-format))
  (compute-number-of-chars external-format sequence start end))
