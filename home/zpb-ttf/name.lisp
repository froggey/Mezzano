;;; Copyright (c) 2006 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
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
;;;
;;; Loading data from the TrueType "name" table.
;;;
;;;   http://www.microsoft.com/OpenType/OTSpec/name.htm
;;;   http://developer.apple.com/fonts/TTRefMan/RM06/Chap6name.html
;;;
;;; $Id: name.lisp,v 1.8 2006/02/18 23:13:43 xach Exp $

(in-package #:zpb-ttf)

(defvar *name-identifiers*
  #(:copyright-notice
    :font-family
    :font-subfamily
    :unique-subfamily
    :full-name
    :name-table-version
    :postscript-name
    :trademark-notice
    :manufacturer-name
    :designer
    :description
    :vendor-url
    :designer-url
    :license-description
    :licence-info-url
    :reserved
    :preferred-family
    :preferred-subfamily
    :compatible-full
    :sample-text))

(defvar *platform-identifiers*
  #(:unicode
    :macintosh
    :iso
    :microsoft
    :custom))

(defvar *unicode-encoding-ids*
  #(:unicode-1.0
    :unicode-1.1
    :iso-10646\:1993
    :unicode>=2.0-bmp-only
    :unicode>=2.0-full-repertoire))

(defvar *microsoft-encoding-ids*
  #(:symbol
    :unicode
    :shiftjis
    :prc
    :big5
    :wansung
    :johab
    :reserved
    :reserved
    :reserved
    :ucs-4))

(defvar *macintosh-encoding-ids*
  #(:roman
    :japanese
    :chinese-traditional
    :korean
    :arabic
    :hebrew
    :greek
    :russian
    :RSymbol
    :devanagari
    :gurmukhi
    :gujarati
    :oriya
    :bengali
    :tamil
    :telugu
    :kennada
    :malayam
    :sinhalese
    :burmese
    :khmer
    :thai
    :laotian
    :georgian
    :armenian
    :chinese-simplified
    :tibetan
    :mongolian
    :geez
    :slavic
    :vietnamese
    :sindhi
    :uninterpreted))

(defparameter *encoding-tables*
  (vector *unicode-encoding-ids*
          *macintosh-encoding-ids*
          nil
          *microsoft-encoding-ids*
          nil))

(defun encoding-id-name (platform-id encoding-id)
  (aref (aref *encoding-tables* platform-id) encoding-id))

(defun platform-id-name (platform-id)
  (aref *platform-identifiers* platform-id))

(defparameter *macroman-translation-table*
  #(#x00 #x00
    #x01 #x01
    #x02 #x02
    #x03 #x03
    #x04 #x04
    #x05 #x05
    #x06 #x06
    #x07 #x07
    #x08 #x08
    #x09 #x09
    #x0A #x0A
    #x0B #x0B
    #x0C #x0C
    #x0D #x0D
    #x0E #x0E
    #x0F #x0F
    #x10 #x10
    #x11 #x11
    #x12 #x12
    #x13 #x13
    #x14 #x14
    #x15 #x15
    #x16 #x16
    #x17 #x17
    #x18 #x18
    #x19 #x19
    #x1A #x1A
    #x1B #x1B
    #x1C #x1C
    #x1D #x1D
    #x1E #x1E
    #x1F #x1F
    #x20 #x20
    #x21 #x21
    #x22 #x22
    #x23 #x23
    #x24 #x24
    #x25 #x25
    #x26 #x26
    #x27 #x27
    #x28 #x28
    #x29 #x29
    #x2A #x2A
    #x2B #x2B
    #x2C #x2C
    #x2D #x2D
    #x2E #x2E
    #x2F #x2F
    #x30 #x30
    #x31 #x31
    #x32 #x32
    #x33 #x33
    #x34 #x34
    #x35 #x35
    #x36 #x36
    #x37 #x37
    #x38 #x38
    #x39 #x39
    #x3A #x3A
    #x3B #x3B
    #x3C #x3C
    #x3D #x3D
    #x3E #x3E
    #x3F #x3F
    #x40 #x40
    #x41 #x41
    #x42 #x42
    #x43 #x43
    #x44 #x44
    #x45 #x45
    #x46 #x46
    #x47 #x47
    #x48 #x48
    #x49 #x49
    #x4A #x4A
    #x4B #x4B
    #x4C #x4C
    #x4D #x4D
    #x4E #x4E
    #x4F #x4F
    #x50 #x50
    #x51 #x51
    #x52 #x52
    #x53 #x53
    #x54 #x54
    #x55 #x55
    #x56 #x56
    #x57 #x57
    #x58 #x58
    #x59 #x59
    #x5A #x5A
    #x5B #x5B
    #x5C #x5C
    #x5D #x5D
    #x5E #x5E
    #x5F #x5F
    #x60 #x60
    #x61 #x61
    #x62 #x62
    #x63 #x63
    #x64 #x64
    #x65 #x65
    #x66 #x66
    #x67 #x67
    #x68 #x68
    #x69 #x69
    #x6A #x6A
    #x6B #x6B
    #x6C #x6C
    #x6D #x6D
    #x6E #x6E
    #x6F #x6F
    #x70 #x70
    #x71 #x71
    #x72 #x72
    #x73 #x73
    #x74 #x74
    #x75 #x75
    #x76 #x76
    #x77 #x77
    #x78 #x78
    #x79 #x79
    #x7A #x7A
    #x7B #x7B
    #x7C #x7C
    #x7D #x7D
    #x7E #x7E
    #x7F #x7F
    #x80 #x00C4
    #x81 #x00C5
    #x82 #x00C7
    #x83 #x00C9
    #x84 #x00D1
    #x85 #x00D6
    #x86 #x00DC
    #x87 #x00E1
    #x88 #x00E0
    #x89 #x00E2
    #x8A #x00E4
    #x8B #x00E3
    #x8C #x00E5
    #x8D #x00E7
    #x8E #x00E9
    #x8F #x00E8
    #x90 #x00EA
    #x91 #x00EB
    #x92 #x00ED
    #x93 #x00EC
    #x94 #x00EE
    #x95 #x00EF
    #x96 #x00F1
    #x97 #x00F3
    #x98 #x00F2
    #x99 #x00F4
    #x9A #x00F6
    #x9B #x00F5
    #x9C #x00FA
    #x9D #x00F9
    #x9E #x00FB
    #x9F #x00FC
    #xA0 #x2020
    #xA1 #x00B0
    #xA2 #x00A2
    #xA3 #x00A3
    #xA4 #x00A7
    #xA5 #x2022
    #xA6 #x00B6
    #xA7 #x00DF
    #xA8 #x00AE
    #xA9 #x00A9
    #xAA #x2122
    #xAB #x00B4
    #xAC #x00A8
    #xAD #x2260
    #xAE #x00C6
    #xAF #x00D8
    #xB0 #x221E
    #xB1 #x00B1
    #xB2 #x2264
    #xB3 #x2265
    #xB4 #x00A5
    #xB5 #x00B5
    #xB6 #x2202
    #xB7 #x2211
    #xB8 #x220F
    #xB9 #x03C0
    #xBA #x222B
    #xBB #x00AA
    #xBC #x00BA
    #xBD #x03A9
    #xBE #x00E6
    #xBF #x00F8
    #xC0 #x00BF
    #xC1 #x00A1
    #xC2 #x00AC
    #xC3 #x221A
    #xC4 #x0192
    #xC5 #x2248
    #xC6 #x2206
    #xC7 #x00AB
    #xC8 #x00BB
    #xC9 #x2026
    #xCA #x00A0
    #xCB #x00C0
    #xCC #x00C3
    #xCD #x00D5
    #xCE #x0152
    #xCF #x0153
    #xD0 #x2103
    #xD1 #x2014
    #xD2 #x201C
    #xD3 #x201D
    #xD4 #x2018
    #xD5 #x2019
    #xD6 #x00F7
    #xD7 #x25CA
    #xD8 #x00FF
    #xD9 #x0178
    #xDA #x2044
    #xDB #x20AC
    #xDC #x2039
    #xDD #x203A
    #xDE #xFB01
    #xDF #xFB02
    #xE0 #x2021
    #xE1 #x00B7
    #xE2 #x201A
    #xE3 #x201E
    #xE4 #x2030
    #xE5 #x00C2
    #xE6 #x00CA
    #xE7 #x00C1
    #xE8 #x00CB
    #xE9 #x00C8
    #xEA #x00CD
    #xEB #x00CE
    #xEC #x00CF
    #xED #x00CC
    #xEE #x00D3
    #xEF #x00D4
    #xF0 #xF8FF
    #xF1 #x00D2
    #xF2 #x00DA
    #xF3 #x00DB
    #xF4 #x00D9
    #xF5 #x0131
    #xF6 #x02C6
    #xF7 #x02DC
    #xF8 #x00AF
    #xF9 #x02D8
    #xFA #x02D9
    #xFB #x02DA
    #xFC #x00B8
    #xFD #x02DD
    #xFE #x02DB
    #xFF #x02C7))

(defconstant +unicode-platform-id+   0)
(defconstant +macintosh-platform-id+ 1)
(defconstant +iso-platform-id+       2)
(defconstant +microsoft-platform-id+ 3)
(defconstant +custom-platform-id+    4)

(defconstant +unicode-2.0-encoding-id+           3)
(defconstant +microsoft-unicode-bmp-encoding-id+ 1)
(defconstant +microsoft-symbol-encoding-id+      0)
(defconstant +macintosh-roman-encoding-id+       1)

;; Full list of microsoft language IDs is here:
;;  http://www.microsoft.com/globaldev/reference/lcid-all.mspx

(defconstant +microsoft-us-english-language-id+ #x0409)
(defconstant +macintosh-english-language-id+    1)
(defconstant +unicode-language-id+              0)


(defclass name-entry ()
  ((font-loader
    :initarg :font-loader
    :accessor font-loader)
   (platform-id
    :initarg :platform-id
    :accessor platform-id)
   (encoding-id
    :initarg :encoding-id
    :accessor encoding-id)
   (language-id
    :initarg :language-id
    :accessor language-id)
   (name-id
    :initarg :name-id
    :accessor name-id)
   (offset
    :initarg :offset
    :accessor offset
    :documentation "The octet offset within the TrueType file stream
of the entry's data. *Not* the same as the offset in the NameRecord
structure, which is relative to the start of the string data for the
table.")
   (entry-length
    :initarg :entry-length
    :accessor entry-length)
   (value
    :reader %value
    :writer (setf value))
   (octets
    :reader %octets
    :writer (setf octets))))

(defmethod print-object ((name-entry name-entry) stream)
  (print-unreadable-object (name-entry stream :type t)
    (format stream "~A (~A/~A/~D)"
            (aref *name-identifiers* (name-id name-entry))
            (platform-id-name (platform-id name-entry))
            (encoding-id-name (platform-id name-entry)
                              (encoding-id name-entry))
            (language-id name-entry))))

(defun unicode-octets-to-string (octets)
  (let ((string (make-string (/ (length octets) 2))))
    (flet ((ref16 (i)
             (+ (ash (aref octets i) 8)
                (aref octets (1+ i)))))
      (loop for i from 0 below (length octets) by 2
            for j from 0
            do (setf (char string j) (code-char (ref16 i))))
      string)))

(defun macintosh-octets-to-string (octets)
  (flet ((macroman->unicode (point)
           (code-char (aref *macroman-translation-table* (1+ (ash point 1))))))
    (let ((string (make-string (length octets))))
      (dotimes (i (length octets) string)
        (setf (schar string i) (macroman->unicode (aref octets i)))))))

(defgeneric initialize-name-entry (name-entry)
  (:method (name-entry)
    (let ((stream (input-stream (font-loader name-entry)))
          (octets (make-array (entry-length name-entry)
                              :element-type '(unsigned-byte 8)))
          (value nil)
          (platform-id (platform-id name-entry)))
      (file-position stream (offset name-entry))
      (read-sequence octets stream)
      (cond ((or (= platform-id +unicode-platform-id+)
                 (= platform-id +microsoft-platform-id+))
             (setf value (unicode-octets-to-string octets)))
            ((= platform-id +macintosh-platform-id+)
             (setf value (macintosh-octets-to-string octets)))
            (t
             (error 'unsupported-value
                    :location "\"name\" table platform ID"
                    :actual-value platform-id
                    :expected-values (list +unicode-platform-id+
                                           +microsoft-platform-id+
                                           +macintosh-platform-id+))))
      (setf (value name-entry) value
            (octets name-entry) octets))))
                    
(defgeneric value (name-entry)
  (:method (name-entry)
    (unless (slot-boundp name-entry 'value)
      (initialize-name-entry name-entry))
    (%value name-entry)))

(defgeneric octets (name-entry)
  (:method (name-entry)
    (unless (slot-boundp name-entry 'octets)
      (initialize-name-entry name-entry))
    (%octets name-entry)))

(defun load-name-info (loader)
  (seek-to-table "name" loader)
  (let* ((stream (input-stream loader))
         (table-offset (file-position stream))
         (format (read-uint16 stream)))
    (unless (= format 0)
      (error 'unsupported-format
             :location "\"name\" table"
             :actual-value format
             :expected-values (list 0)))
    (let* ((count (read-uint16 stream))
           (values-offset (read-uint16 stream))
           (entries (make-array count)))
      (setf (name-entries loader) entries)
      (dotimes (i count)
        (let ((platform-id (read-uint16 stream))
              (encoding-id (read-uint16 stream))
              (language-id (read-uint16 stream))
              (name-id (read-uint16 stream))
              (length (read-uint16 stream))
              (offset (read-uint16 stream)))
          (setf (aref entries i)
                (make-instance 'name-entry
                               :font-loader loader
                               :platform-id platform-id
                               :encoding-id encoding-id
                               :language-id language-id
                               :name-id name-id
                               :entry-length length
                               :offset (+ table-offset values-offset offset))))))))

;;;
;;; Fetching info out of the name-entry vector
;;;

(defun name-identifier-id (symbol)
  (let ((id (position symbol *name-identifiers*)))
    (if id
        id
        (error "Unknown NAME identifier: ~S" symbol))))
  
(defgeneric find-name-entry (platform-id encoding-id language-id name-id
                             font-loader))
(defgeneric name-entry-value (name-designator font-loader))
(defgeneric postscript-name (font-loader))
(defgeneric family-name (font-loader))
(defgeneric subfamily-name (font-loader))
(defgeneric full-name (font-loader))

(defmethod find-name-entry (platform-id encoding-id language-id name-id
                            (font-loader font-loader))
  ;; FIXME: this vector is sorted by platform ID, encoding ID,
  ;; language ID, and name ID, in that order. Could bisect if it
  ;; mattered.
  (loop for name-entry across (name-entries font-loader)
        when (and (or (null platform-id)
                      (= (platform-id name-entry) platform-id))
                  (or (null encoding-id)
                      (= (encoding-id name-entry) encoding-id))
                  (or (null language-id)
                      (= (language-id name-entry) language-id))
                  (or (null name-id)
                      (= (name-id name-entry) name-id)))
        return name-entry))

(defmethod name-entry-value (name-designator (font-loader font-loader))
  (let* ((name-id (etypecase name-designator
                    (keyword (name-identifier-id name-designator))
                    (integer name-designator)))
         (entry (or (find-name-entry +unicode-platform-id+
                                     +unicode-2.0-encoding-id+
                                     +unicode-language-id+
                                     name-id
                                     font-loader)
                    (find-name-entry +microsoft-platform-id+
                                     nil
                                     +microsoft-us-english-language-id+
                                     name-id
                                     font-loader)
                    (find-name-entry +macintosh-platform-id+
                                     +macintosh-roman-encoding-id+
                                     +macintosh-english-language-id+
                                     name-id
                                     font-loader))))
    (when entry
      (value entry))))


(defmethod postscript-name ((font-loader font-loader))
  (name-entry-value :postscript-name font-loader))

(defmethod family-name ((font-loader font-loader))
  (name-entry-value :font-family font-loader))

(defmethod subfamily-name ((font-loader font-loader))
  (name-entry-value :font-subfamily font-loader))

(defmethod full-name ((font-loader font-loader))
  (name-entry-value :full-name font-loader))
