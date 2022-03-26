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
;;; "post" table functions
;;;
;;;   http://www.microsoft.com/OpenType/OTSpec/post.htm
;;;   http://developer.apple.com/fonts/TTRefMan/RM06/Chap6post.html
;;;
;;; $Id: post.lisp,v 1.7 2006/11/09 15:06:16 xach Exp $

(in-package #:zpb-ttf)

(defvar *standard-mac-glyph-names*
  #(".notdef"
    ".null"
    "nonmarkingreturn"
    "space"
    "exclam"
    "quotedbl"
    "numbersign"
    "dollar"
    "percent"
    "ampersand"
    "quotesingle"
    "parenleft"
    "parenright"
    "asterisk"
    "plus"
    "comma"
    "hyphen"
    "period"
    "slash"
    "zero" "one" "two" "three" "four"
    "five" "six" "seven" "eight" "nine"
    "colon"
    "semicolon"
    "less"
    "equal"
    "greater"
    "question"
    "at"
    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
    "bracketleft"
    "backslash"
    "bracketright"
    "asciicircum"
    "underscore"
    "grave"
    "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
    "braceleft"
    "bar"
    "braceright"
    "asciitilde"
    "Adieresis"
    "Aring"
    "Ccedilla"
    "Eacute"
    "Ntilde"
    "Odieresis"
    "Udieresis"
    "aacute"
    "agrave"
    "acircumflex"
    "adieresis"
    "atilde"
    "aring"
    "ccedilla"
    "eacute"
    "egrave"
    "ecircumflex"
    "edieresis"
    "iacute"
    "igrave"
    "icircumflex"
    "idieresis"
    "ntilde"
    "oacute"
    "ograve"
    "ocircumflex"
    "odieresis"
    "otilde"
    "uacute"
    "ugrave"
    "ucircumflex"
    "udieresis"
    "dagger"
    "degree"
    "cent"
    "sterling"
    "section"
    "bullet"
    "paragraph"
    "germandbls"
    "registered"
    "copyright"
    "trademark"
    "acute"
    "dieresis"
    "notequal"
    "AE"
    "Oslash"
    "infinity"
    "plusminus"
    "lessequal"
    "greaterequal"
    "yen"
    "mu"
    "partialdiff"
    "summation"
    "product"
    "pi"
    "integral"
    "ordfeminine"
    "ordmasculine"
    "Omega"
    "ae"
    "oslash"
    "questiondown"
    "exclamdown"
    "logicalnot"
    "radical"
    "florin"
    "approxequal"
    "Delta"
    "guillemotleft"
    "guillemotright"
    "ellipsis"
    "nonbreakingspace"
    "Agrave"
    "Atilde"
    "Otilde"
    "OE"
    "oe"
    "endash"
    "emdash"
    "quotedblleft"
    "quotedblright"
    "quoteleft"
    "quoteright"
    "divide"
    "lozenge"
    "ydieresis"
    "Ydieresis"
    "fraction"
    "currency"
    "guilsinglleft"
    "guilsinglright"
    "fi"
    "fl"
    "daggerdbl"
    "periodcentered"
    "quotesinglbase"
    "quotedblbase"
    "perthousand"
    "Acircumflex"
    "Ecircumflex"
    "Aacute"
    "Edieresis"
    "Egrave"
    "Iacute"
    "Icircumflex"
    "Idieresis"
    "Igrave"
    "Oacute"
    "Ocircumflex"
    "apple"
    "Ograve"
    "Uacute"
    "Ucircumflex"
    "Ugrave"
    "dotlessi"
    "circumflex"
    "tilde"
    "macron"
    "breve"
    "dotaccent"
    "ring"
    "cedilla"
    "hungarumlaut"
    "ogonek"
    "caron"
    "Lslash"
    "lslash"
    "Scaron"
    "scaron"
    "Zcaron"
    "zcaron"
    "brokenbar"
    "Eth"
    "eth"
    "Yacute"
    "yacute"
    "Thorn"
    "thorn"
    "minus"
    "multiply"
    "onesuperior"
    "twosuperior"
    "threesuperior"
    "onehalf"
    "onequarter"
    "threequarters"
    "franc"
    "Gbreve"
    "gbreve"
    "Idotaccent"
    "Scedilla"
    "scedilla"
    "Cacute"
    "cacute"
    "Ccaron"
    "ccaron"
    "dcroat"))

(defun load-post-format-2 (names stream)
  (let* ((glyph-count (read-uint16 stream))
         (new-count glyph-count))
    (when (/= glyph-count (length names))
      (warn "Glyph count in \"post\" table (~D) ~
             does not match glyph count in \"maxp\" table (~D). ~
             This font may be broken."
            glyph-count (length names))
      (setf glyph-count (length names)
            new-count (length names)))
    ;; This is done in a couple passes. First, initialize the names
    ;; tables with indexes into either the standard table or the
    ;; pstring table. Next, read the pstring table into a vector.
    ;; Finally, replace the indexes with names.
    (dotimes (i glyph-count)
      (let ((name-index (read-uint16 stream)))
        (when (< name-index 258)
          (decf new-count))
        (setf (aref names i) name-index)))
    (let ((pstrings (make-array new-count)))
      (dotimes (i new-count)
        (setf (aref pstrings i) (read-pstring stream)))
      (loop for i below glyph-count
            for j across names
            do
            (cond ((< j 258)
                   (setf (aref names i)
                         (aref *standard-mac-glyph-names* j)))
                  (t
                   (setf (aref names i)
                         (aref pstrings (- j 258)))))))))

(defun load-post-format-3 (names stream)
  (declare (ignore stream))
  (fill names nil))

(defgeneric load-post-info (font-loader))

(defmethod load-post-info ((font-loader font-loader))
  (let ((names (make-array (glyph-count font-loader)
                           :initial-element 0))
        (stream (input-stream font-loader)))
    (seek-to-table "post" font-loader)
    (let ((format (read-uint32 stream)))
      (when (/= format #x00020000 #x00030000)
        (error 'unsupported-format
               :location "\"post\" table"
               :expected-values (list #x00020000 #x00030000)
               :actual-value format))
      (setf (italic-angle font-loader) (read-fixed stream)
            (underline-position font-loader) (read-fword stream)
            (underline-thickness font-loader) (read-fword stream)
            (fixed-pitch-p font-loader) (plusp (read-uint32 stream))
            (postscript-glyph-names font-loader) names)
      ;; skip minMemType* fields
      (advance-file-position stream 16)
      (case format
        (#x00020000 (load-post-format-2 names stream))
        (#x00030000 (load-post-format-3 names stream))))))
        
(defun postscript-uni-name-p (name)
  (let ((end (or (position #\. name) (length name))))
    (and (= end 7)
         (= (mismatch "uni" name) 3)
         (loop for i from 3 below end
               always (digit-char-p (char name i) 16)))))

(defun postscript-name-code-point (name)
  "Returns, if available, the interpretation of the PostScript name NAME as a Unicode code point specifier.
Ref: http://partners.adobe.com/public/developer/opentype/index_glyph.html"
  (when (postscript-uni-name-p name)
    (parse-integer name :start 3 :end 7 :radix 16)))
