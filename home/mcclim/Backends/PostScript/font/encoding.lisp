;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-POSTSCRIPT; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Postscript Font Encodings
;;;   Created: 2004-12-03
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2004 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.
 
 
(in-package :clim-postscript-font)

(defvar *iso-latin-1-symbolic-names*
    '#(NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       
       "space"         "exclam"        "quotedbl"      "numbersign"
       "dollar"        "percent"       "ampersand"     "quoteright"
       "parenleft"     "parenright"    "asterisk"      "plus"
       "comma"         "hyphen"        "period"        "slash"
       "zero"          "one"           "two"           "three"
       "four"          "five"          "six"           "seven"
       "eight"         "nine"          "colon"         "semicolon"
       "less"          "equal"         "greater"       "question"
       
       "at"            "A"             "B"             "C"
       "D"             "E"             "F"             "G"
       "H"             "I"             "J"             "K"
       "L"             "M"             "N"             "O"
       "P"             "Q"             "R"             "S"
       "T"             "U"             "V"             "W"
       "X"             "Y"             "Z"             "bracketleft"
       "backslash"     "bracketright"  "asciicircum"   "underscore"
       
       "quoteleft"     "a"             "b"             "c"
       "d"             "e"             "f"             "g"
       "h"             "i"             "j"             "k"
       "l"             "m"             "n"             "o"
       "p"             "q"             "r"             "s"
       "t"             "u"             "v"             "w"
       "x"             "y"             "z"             "braceleft"
       "bar"           "braceright"    "asciitilde"    NIL

       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       NIL             NIL             NIL             NIL
       
       "nbspace"       "exclamdown"    "cent"          "sterling"
       "currency"      "yen"           "brokenbar"     "section"
       NIL             "copyright"     "ordfeminine"   "guillemotleft"
       "logicalnot"    NIL             "registered"    NIL
       "degree"        "plusminus"     "twosuperior"   "threesuperior"
       "acute"         "mu"            "paragraph"     "periodcentered"
       "cedilla"       "onesuperior"   "ordmasculine"  "guillemotright"
       "onequarter"   "onehalf"       "threequarters" "questiondown"

       "Agrave"        "Aacute"        "Acircumflex"   "Atilde"
       "Adieresis"     "Aring"         "AE"            "Ccedilla"
       "Egrave"        "Eacute"        "Ecircumflex"   "Edieresis"
       "Igrave"        "Iacute"        "Icircumflex"   "Idieresis"
       "Eth"           "Ntilde"        "Ograve"        "Oacute"
       "Ocircumflex"   "Otilde"        "Odieresis"     "multiply"
       "Oslash"        "Ugrave"        "Uacute"        "Ucircumflex"
       "Udieresis"     "Yacute"        "Thorn"         "germandbls"

       "agrave"        "aacute"        "acircumflex"   "atilde"
       "adieresis"     "aring"         "ae"            "ccedilla"
       "egrave"        "eacute"        "ecircumflex"   "edieresis"
       "igrave"        "iacute"        "icirc"         "idieresis"
       "eth"           "ntilde"        "ograve"        "oacute"
       "ocircumflex"   "otilde"        "odieresis"     "divide"
       "oslash"        "ugrave"        "uacute"        "ucircumflex"
       "udieresis"     "yacute"        "thorn"         "ydieresis")
  "A mapping of iso-8859-1 code points to adobe glyph names.")


