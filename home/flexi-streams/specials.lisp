;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/specials.lisp,v 1.33 2008/05/25 01:40:54 edi Exp $

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

(defvar *standard-optimize-settings*
  '(optimize
    speed
    (space 0)
    (debug 1)
    (compilation-speed 0))
  "The standard optimize settings used by most declaration expressions.")

(defvar *fixnum-optimize-settings*
  '(optimize
    speed
    (space 0)
    (debug 1)
    (compilation-speed 0)
    #+:lispworks (hcl:fixnum-safety 0))
  "Like *STANDARD-OPTIMIZE-SETTINGS*, but \(on LispWorks) with all
arithmetic being fixnum arithmetic.")

(defconstant +lf+ (char-code #\Linefeed))

(defconstant +cr+ (char-code #\Return))

(defvar *current-unreader* nil
  "A unary function which might be called to `unread' a character
\(i.e. the sequence of octets it represents).

Used by the function OCTETS-TO-CHAR-CODE and must always be bound to a
suitable functional object when this function is called.")

(defvar +name-map+
  '((:utf8 . :utf-8)
    (:utf16 . :utf-16)
    (:ucs2 . :utf-16)
    (:ucs-2 . :utf-16)
    (:unicode . :utf-16)
    (:utf32 . :utf-32)
    (:ucs4 . :utf-32)
    (:ucs-4 . :utf-32)
    (:ascii . :us-ascii)
    (:koi8r . :koi8-r)
    (:latin-1 . :iso-8859-1)
    (:latin1 . :iso-8859-1)
    (:latin-2 . :iso-8859-2)
    (:latin2 . :iso-8859-2)
    (:latin-3 . :iso-8859-3)
    (:latin3 . :iso-8859-3)
    (:latin-4 . :iso-8859-4)
    (:latin4 . :iso-8859-4)
    (:cyrillic . :iso-8859-5)
    (:arabic . :iso-8859-6)
    (:greek . :iso-8859-7)
    (:hebrew . :iso-8859-8)
    (:latin-5 . :iso-8859-9)
    (:latin5 . :iso-8859-9)
    (:latin-6 . :iso-8859-10)
    (:latin6 . :iso-8859-10)
    (:thai . :iso-8859-11)
    (:latin-7 . :iso-8859-13)
    (:latin7 . :iso-8859-13)
    (:latin-8 . :iso-8859-14)
    (:latin8 . :iso-8859-14)
    (:latin-9 . :iso-8859-15)
    (:latin9 . :iso-8859-15)
    (:latin-0 . :iso-8859-15)
    (:latin0 . :iso-8859-15)
    (:latin-10 . :iso-8859-16)
    (:latin10 . :iso-8859-16)
    (:codepage . :code-page)
    #+(and :lispworks :win32)
    (win32:code-page . :code-page))
  "An alist which mapes alternative names for external formats to
their canonical counterparts.")

(defvar +shortcut-map+
  '((:ucs-2le . (:ucs-2 :little-endian t))
    (:ucs-2be . (:ucs-2 :little-endian nil))
    (:ucs-4le . (:ucs-4 :little-endian t))
    (:ucs-4be . (:ucs-4 :little-endian nil))
    (:utf-16le . (:utf-16 :little-endian t))
    (:utf-16be . (:utf-16 :little-endian nil))
    (:utf-32le . (:utf-32 :little-endian t))
    (:utf-32be . (:utf-32 :little-endian nil))
    (:ibm437 . (:code-page :id 437))
    (:ibm850 . (:code-page :id 850))
    (:ibm852 . (:code-page :id 852))
    (:ibm855 . (:code-page :id 855))
    (:ibm857 . (:code-page :id 857))
    (:ibm860 . (:code-page :id 860))
    (:ibm861 . (:code-page :id 861))
    (:ibm862 . (:code-page :id 862))
    (:ibm863 . (:code-page :id 863))
    (:ibm864 . (:code-page :id 864))
    (:ibm865 . (:code-page :id 865))
    (:ibm866 . (:code-page :id 866))
    (:ibm869 . (:code-page :id 869))
    (:windows-1250 . (:code-page :id 1250))
    (:windows-1251 . (:code-page :id 1251))
    (:windows-1252 . (:code-page :id 1252))
    (:windows-1253 . (:code-page :id 1253))
    (:windows-1254 . (:code-page :id 1254))
    (:windows-1255 . (:code-page :id 1255))
    (:windows-1256 . (:code-page :id 1256))
    (:windows-1257 . (:code-page :id 1257))
    (:windows-1258 . (:code-page :id 1258)))
  "An alist which maps shortcuts for external formats to their
long forms.")
    
(defvar *default-eol-style*
  #+:win32 :crlf
  #-:win32 :lf
  "The end-of-line style used by external formats if none is
explicitly given.  Depends on the OS the code is compiled on.")

(defvar *default-little-endian*
  #+:little-endian t
  #-:little-endian nil
  "Whether external formats are little-endian by default
\(i.e. unless explicitly specified).  Depends on the platform
the code is compiled on.")

(defvar *substitution-char* nil
  "If this value is not NIL, it should be a character which is used
\(as if by a USE-VALUE restart) whenever during reading an error of
type FLEXI-STREAM-ENCODING-ERROR would have been signalled otherwise.")

(defconstant +iso-8859-hashes+
  (loop for (name . table) in +iso-8859-tables+
        collect (cons name (invert-table table)))
  "An alist which maps names for ISO-8859 encodings to hash
tables which map character codes to the corresponding octets.")

(defconstant +code-page-hashes+
  (loop for (id . table) in +code-page-tables+
        collect (cons id (invert-table table)))
  "An alist which maps IDs of Windows code pages to hash tables
which map character codes to the corresponding octets.")

(defconstant +ascii-hash+ (invert-table +ascii-table+)
  "A hash table which maps US-ASCII character codes to the
corresponding octets.")

(defconstant +koi8-r-hash+ (invert-table +koi8-r-table+)
  "A hash table which maps KOI8-R character codes to the
corresponding octets.")

(defconstant +buffer-size+ 8192
  "Default size for buffers used for internal purposes.")

(pushnew :flexi-streams *features*)

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/flexi-streams/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :flexi-streams
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
