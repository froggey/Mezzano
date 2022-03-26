;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/external-format.lisp,v 1.24 2008/05/26 10:55:08 edi Exp $

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

(defclass external-format ()
  ((name :initarg :name
         :reader external-format-name
         :documentation "The name of the external format - a
keyword.")
   (id :initarg :id
       :initform nil
       :reader external-format-id
       :documentation "If the external format denotes a Windows
code page this ID specifies which one to use.  Otherwise the
value is ignored \(and usually NIL).")
   (little-endian :initarg :little-endian
                  :initform *default-little-endian*
                  :reader external-format-little-endian
                  :documentation "Whether multi-octet values are
read and written with the least significant octet first.  For
8-bit encodings like :ISO-8859-1 this value is ignored.")
   (eol-style :initarg :eol-style
              :reader external-format-eol-style
              :documentation "The character\(s) to or from which
a #\Newline will be translated - one of the keywords :CR, :LF,
or :CRLF."))
  (:documentation "EXTERNAL-FORMAT objects are used to denote
encodings for flexi streams or for the string functions defined in
strings.lisp."))

(defmethod make-load-form ((thing external-format) &optional environment)
  "Defines a way to reconstruct external formats.  Needed for OpenMCL."
  (make-load-form-saving-slots thing :environment environment))

(defclass flexi-cr-mixin ()
  ()
  (:documentation "A mixin for external-formats where the end-of-line
designator is #\Return."))

(defclass flexi-crlf-mixin ()
  ()
  (:documentation "A mixin for external-formats where the end-of-line
designator is the sequence #\Return #\Linefeed."))

(defclass flexi-8-bit-format (external-format)
  ((encoding-hash :accessor external-format-encoding-hash)
   (decoding-table :accessor external-format-decoding-table))
  (:documentation "The class for all flexi streams which use an 8-bit
encoding and thus need additional slots for the encoding/decoding
tables."))

(defclass flexi-cr-8-bit-format (flexi-cr-mixin flexi-8-bit-format)
  ()
  (:documentation "Special class for external formats which use an
8-bit encoding /and/ have #\Return as the line-end character."))

(defclass flexi-crlf-8-bit-format (flexi-crlf-mixin flexi-8-bit-format)
  ()
  (:documentation "Special class for external formats which use an
8-bit encoding /and/ have the sequence #\Return #\Linefeed as the
line-end character."))

(defclass flexi-ascii-format (flexi-8-bit-format)
  ()
  (:documentation "Special class for external formats which use the
US-ASCII encoding."))

(defclass flexi-cr-ascii-format (flexi-cr-mixin flexi-ascii-format)
  ()
  (:documentation "Special class for external formats which use the
US-ASCII encoding /and/ have #\Return as the line-end character."))

(defclass flexi-crlf-ascii-format (flexi-crlf-mixin flexi-ascii-format)
  ()
  (:documentation "Special class for external formats which use the
US-ASCII encoding /and/ have the sequence #\Return #\Linefeed as the
line-end character."))

(defclass flexi-latin-1-format (flexi-8-bit-format)
  ()
  (:documentation "Special class for external formats which use the
ISO-8859-1 encoding."))

(defclass flexi-cr-latin-1-format (flexi-cr-mixin flexi-latin-1-format)
  ()
  (:documentation "Special class for external formats which use the
ISO-8859-1 encoding /and/ have #\Return as the line-end character."))

(defclass flexi-crlf-latin-1-format (flexi-crlf-mixin flexi-latin-1-format)
  ()
  (:documentation "Special class for external formats which use the
ISO-8859-1 encoding /and/ have the sequence #\Return #\Linefeed as the
line-end character."))

(defclass flexi-utf-32-format (external-format)
  ()
  (:documentation "Abstract class for external formats which use the
UTF-32 encoding."))

(defclass flexi-utf-32-le-format (flexi-utf-32-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-32 encoding with little-endian byte ordering."))

(defclass flexi-cr-utf-32-le-format (flexi-cr-mixin flexi-utf-32-le-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-32 encoding with little-endian byte ordering /and/ have #\Return
as the line-end character."))

(defclass flexi-crlf-utf-32-le-format (flexi-crlf-mixin flexi-utf-32-le-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-32 encoding with little-endian byte ordering /and/ have the
sequence #\Return #\Linefeed as the line-end character."))

(defclass flexi-utf-32-be-format (flexi-utf-32-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-32 encoding with big-endian byte ordering."))

(defclass flexi-cr-utf-32-be-format (flexi-cr-mixin flexi-utf-32-be-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-32 encoding with big-endian byte ordering /and/ have #\Return as
the line-end character."))

(defclass flexi-crlf-utf-32-be-format (flexi-crlf-mixin flexi-utf-32-be-format)
  ()
  (:documentation "Special class for external formats which use the
the UTF-32 encoding with big-endian byte ordering /and/ have the
sequence #\Return #\Linefeed as the line-end character."))

(defclass flexi-utf-16-format (external-format)
  ()
  (:documentation "Abstract class for external formats which use the
UTF-16 encoding."))

(defclass flexi-utf-16-le-format (flexi-utf-16-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-16 encoding with little-endian byte ordering."))

(defclass flexi-cr-utf-16-le-format (flexi-cr-mixin flexi-utf-16-le-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-16 encoding with little-endian byte ordering /and/ have #\Return
as the line-end character."))

(defclass flexi-crlf-utf-16-le-format (flexi-crlf-mixin flexi-utf-16-le-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-16 encoding with little-endian byte ordering /and/ have the
sequence #\Return #\Linefeed as the line-end character."))

(defclass flexi-utf-16-be-format (flexi-utf-16-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-16 encoding with big-endian byte ordering."))

(defclass flexi-cr-utf-16-be-format (flexi-cr-mixin flexi-utf-16-be-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-16 encoding with big-endian byte ordering /and/ have #\Return as
the line-end character."))

(defclass flexi-crlf-utf-16-be-format (flexi-crlf-mixin flexi-utf-16-be-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-16 encoding with big-endian byte ordering /and/ have the sequence
#\Return #\Linefeed as the line-end character."))

(defclass flexi-utf-8-format (external-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-8 encoding."))

(defclass flexi-cr-utf-8-format (flexi-cr-mixin flexi-utf-8-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-8 encoding /and/ have #\Return as the line-end character."))

(defclass flexi-crlf-utf-8-format (flexi-crlf-mixin flexi-utf-8-format)
  ()
  (:documentation "Special class for external formats which use the
UTF-8 encoding /and/ have the sequence #\Return #\Linefeed as the
line-end character."))

(defmethod initialize-instance :after ((external-format flexi-8-bit-format) &rest initargs)
  "Sets the fixed encoding/decoding tables for this particular
external format."
  (declare #.*standard-optimize-settings*)
  (declare (ignore initargs))
  (with-accessors ((encoding-hash external-format-encoding-hash)
                   (decoding-table external-format-decoding-table)
                   (name external-format-name)
                   (id external-format-id))
      external-format
    (multiple-value-setq (encoding-hash decoding-table)
        (cond ((ascii-name-p name)
               (values +ascii-hash+ +ascii-table+))
              ((koi8-r-name-p name)
               (values +koi8-r-hash+ +koi8-r-table+))
              ((iso-8859-name-p name)
               (values (cdr (assoc name +iso-8859-hashes+ :test #'eq))                       
                       (cdr (assoc name +iso-8859-tables+ :test #'eq))))
              ((code-page-name-p name)
               (values (cdr (assoc id +code-page-hashes+))                       
                       (cdr (assoc id +code-page-tables+))))))))

(defun external-format-class-name (real-name &key eol-style little-endian id)
  "Given the initargs for a general external format returns the name
\(a symbol) of the most specific subclass matching these arguments."
  (declare #.*standard-optimize-settings*)
  (declare (ignore id))
  (cond ((ascii-name-p real-name)
         (ecase eol-style
           (:lf 'flexi-ascii-format)
           (:cr 'flexi-cr-ascii-format)
           (:crlf 'flexi-crlf-ascii-format)))
        ((eq real-name :iso-8859-1)
         (ecase eol-style
           (:lf 'flexi-latin-1-format)
           (:cr 'flexi-cr-latin-1-format)
           (:crlf 'flexi-crlf-latin-1-format)))
        ((or (koi8-r-name-p real-name)
             (iso-8859-name-p real-name)
             (code-page-name-p real-name))
         (ecase eol-style
           (:lf 'flexi-8-bit-format)
           (:cr 'flexi-cr-8-bit-format)
           (:crlf 'flexi-crlf-8-bit-format)))
        (t (ecase real-name
             (:utf-8 (ecase eol-style
                       (:lf 'flexi-utf-8-format)
                       (:cr 'flexi-cr-utf-8-format)
                       (:crlf 'flexi-crlf-utf-8-format)))
             (:utf-16 (ecase eol-style
                        (:lf (if little-endian
                               'flexi-utf-16-le-format
                               'flexi-utf-16-be-format))
                        (:cr (if little-endian
                               'flexi-cr-utf-16-le-format
                               'flexi-cr-utf-16-be-format))
                        (:crlf (if little-endian
                                 'flexi-crlf-utf-16-le-format
                                 'flexi-crlf-utf-16-be-format))))
             (:utf-32 (ecase eol-style
                        (:lf (if little-endian
                               'flexi-utf-32-le-format
                               'flexi-utf-32-be-format))
                        (:cr (if little-endian
                               'flexi-cr-utf-32-le-format
                               'flexi-cr-utf-32-be-format))
                        (:crlf (if little-endian
                                 'flexi-crlf-utf-32-le-format
                                 'flexi-crlf-utf-32-be-format))))))))
                         
(defun make-external-format% (name &key (little-endian *default-little-endian*)
                                   id eol-style)
  "Used internally by MAKE-EXTERNAL-FORMAT to default some of the
keywords arguments and to determine the right subclass of
EXTERNAL-FORMAT."
  (declare #.*standard-optimize-settings*)
  (let* ((real-name (normalize-external-format-name name))
         (initargs
          (cond ((or (iso-8859-name-p real-name)
		     (koi8-r-name-p real-name)
                     (ascii-name-p real-name))
                 (list :eol-style (or eol-style *default-eol-style*)))
                ((code-page-name-p real-name)
                 (list :id (or (known-code-page-id-p id)
                               (error 'external-format-error
                                      :format-control "Unknown code page ID ~S"
                                      :format-arguments (list id)))
                       ;; default EOL style for Windows code pages is :CRLF
                       :eol-style (or eol-style :crlf)))
                (t (list :eol-style (or eol-style *default-eol-style*)
                         :little-endian little-endian)))))
    (apply #'make-instance (apply #'external-format-class-name real-name initargs)
           :name real-name
           initargs)))

(defun make-external-format (name &rest args
                                  &key (little-endian *default-little-endian*)
                                       id eol-style)
  "Creates and returns an external format object as specified.
NAME is a keyword like :LATIN1 or :UTF-8, LITTLE-ENDIAN specifies
the `endianess' of the external format and is ignored for 8-bit
encodings, EOL-STYLE is one of the keywords :CR, :LF, or :CRLF
which denote the end-of-line character \(sequence), ID is the ID
of a Windows code page \(and ignored for other encodings)."
  (declare #.*standard-optimize-settings*)
  ;; the keyword arguments are only there for arglist display in the IDE
  (declare (ignore id little-endian))
  (let ((shortcut-args (cdr (assoc name +shortcut-map+ :test #'string-equal))))
    (cond (shortcut-args
           (apply #'make-external-format%
                  (append shortcut-args
                          `(:eol-style ,eol-style))))
          (t (apply #'make-external-format% name args)))))

(defun maybe-convert-external-format (external-format)
  "Given an external format designator \(a keyword, a list, or an
EXTERNAL-FORMAT object) returns the corresponding EXTERNAL-FORMAT
object."
  (declare #.*standard-optimize-settings*)
  (typecase external-format
    (symbol (make-external-format external-format))
    (list (apply #'make-external-format external-format))
    (otherwise external-format)))
  
(defun external-format-equal (ef1 ef2)
  "Checks whether two EXTERNAL-FORMAT objects denote the same encoding."
  (declare #.*standard-optimize-settings*)
  (let* ((name1 (external-format-name ef1))
         (code-page-name-p (code-page-name-p name1)))
    ;; they must habe the same canonical name
    (and (eq name1
             (external-format-name ef2))
         ;; if both are code pages the IDs must be the same
         (or (not code-page-name-p)
             (eql (external-format-id ef1)
                  (external-format-id ef2)))
         ;; for non-8-bit encodings the endianess must be the same
         (or code-page-name-p
             (ascii-name-p name1)
	     (koi8-r-name-p name1)
             (iso-8859-name-p name1)
             (eq name1 :utf-8)
             (eq (not (external-format-little-endian ef1))
                 (not (external-format-little-endian ef2))))
         ;; the EOL style must also be the same
         (eq (external-format-eol-style ef1)
             (external-format-eol-style ef2)))))

(defun normalize-external-format (external-format)
  "Returns a list which is a `normalized' representation of the
external format EXTERNAL-FORMAT.  Used internally by PRINT-OBJECT, for
example.  Basically, the result is an argument list that can be fed
back to MAKE-EXTERNAL-FORMAT to create an equivalent object."
  (declare #.*standard-optimize-settings*)
  (let ((name (external-format-name external-format))
        (eol-style (external-format-eol-style external-format)))
    (cond ((or (ascii-name-p name)
               (koi8-r-name-p name)
               (iso-8859-name-p name)
               (eq name :utf-8))
           (list name :eol-style eol-style))
          ((code-page-name-p name)
           (list name
                 :id (external-format-id external-format)
                 :eol-style eol-style))
          (t (list name
                   :eol-style eol-style
                   :little-endian (external-format-little-endian external-format))))))

(defmethod print-object ((object external-format) stream)
  "How an EXTERNAL-FORMAT object is rendered.  Uses
NORMALIZE-EXTERNAL-FORMAT."
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (normalize-external-format object) stream)))
