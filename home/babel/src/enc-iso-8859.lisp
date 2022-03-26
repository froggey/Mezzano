;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-iso-8859.lisp --- ISO-8859-* encodings.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;; This implementation is largely based on OpenMCL's l1-unicode.lisp
;;;   Copyright (C) 2006 Clozure Associates and contributors.

(in-package #:babel-encodings)

;;; Typically, ISO-8859-* codes in the range #x00-#x9f map straight
;;; through, while codes #xa0-#xff select arbitrary Unicode characters
;;; that are commonly used in some locale.  (Sometimes the break is at
;;; #x80 instead of #xa0).
;;;
;;; (comment from OpenMCL's ccl/level-1/l1-unicode.lisp)

(define-character-encoding :iso-8859-1
    "An 8-bit, fixed-width character encoding in which all
character codes map to their Unicode equivalents.  Intended to
support most characters used in most Western European languages."
  :aliases '(:latin-1 :latin1)
  :literal-char-code-limit 256)

(define-unibyte-encoder :iso-8859-1 (code)
  (if (>= code 256)
      (handle-error)
      code))

(define-unibyte-decoder :iso-8859-1 (octet)
  octet)

(define-character-encoding :iso-8859-2
    "An 8-bit, fixed-width character encoding in which codes
#x00-#x9f map to their Unicode equivalents and other codes map to
other Unicode character values.  Intended to provide most
characters found in most languages used in Central/Eastern
Europe."
  :aliases '(:latin-2 :latin2)
  :literal-char-code-limit #xa0)

(define-constant +unicode-00a0-0180-to-iso-8859-2+
    #(#xa0 nil nil nil #xa4 nil nil #xa7 ; #xa0-#xa7
      #xa8 nil nil nil nil #xad nil nil  ; #xa8-#xaf
      #xb0 nil nil nil #xb4 nil nil nil  ; #xb0-#xb7
      #xb8 nil nil nil nil nil nil nil   ; #xb8-#xbf
      nil #xc1 #xc2 nil #xc4 nil nil #xc7 ; #xc0-#xc7
      nil #xc9 nil #xcb nil #xcd #xce nil ; #xc8-#xcf
      nil nil nil #xd3 #xd4 nil #xd6 #xd7 ; #xd0-#xd7
      nil nil #xda nil #xdc #xdd nil #xdf ; #xd8-#xdf
      nil #xe1 #xe2 nil #xe4 nil nil #xe7 ; #xe0-#xe7
      nil #xe9 nil #xeb nil #xed #xee nil ; #xe8-#xef
      nil nil nil #xf3 #xf4 nil #xf6 #xf7 ; #xf0-#xf7
      nil nil #xfa nil #xfc #xfd nil nil  ; #xf8-#xff
      ;; #x0100
      nil nil #xc3 #xe3 #xa1 #xb1 #xc6 #xe6 ; #x100-#x107
      nil nil nil nil #xc8 #xe8 #xcf #xef   ; #x108-#x10f
      #xd0 #xf0 nil nil nil nil nil nil     ; #x110-#x117
      #xca #xea #xcc #xec nil nil nil nil   ; #x118-#x11f
      nil nil nil nil nil nil nil nil       ; #x120-#x127
      nil nil nil nil nil nil nil nil       ; #x128-#x12f
      nil nil nil nil nil nil nil nil       ; #x130-#x137
      nil #xc5 #xe5 nil nil #xa5 #xb5 nil   ; #x138-#x13f
      nil #xa3 #xb3 #xd1 #xf1 nil nil #xd2  ; #x140-#x147
      #xf2 nil nil nil nil nil nil nil      ; #x148-#x14f
      #xd5 #xf5 nil nil #xc0 #xe0 nil nil   ; #x150-#x157
      #xd8 #xf8 #xa6 #xb6 nil nil #xaa #xba ; #x158-#x15f
      #xa9 #xb9 #xde #xfe #xab #xbb nil nil ; #x160-#x167
      nil nil nil nil nil nil #xd9 #xf9     ; #x168-#x16f
      #xdb #xfb nil nil nil nil nil nil     ; #x170-#x177
      nil #xac #xbc #xaf #xbf #xae #xbe nil) ; #x178-#x17f
  :test #'equalp)

(define-constant +unicode-02c0-02e0-to-iso-8859-2+
    #(nil nil nil nil nil nil nil #xb7  ; #x2c0-#x2c7
      nil nil nil nil nil nil nil nil   ; #x2c8-#x2cf
      nil nil nil nil nil nil nil nil   ; #x2d0-#x2d7
      #xa2 #xff nil #xb2 nil #xbd nil nil) ; #x2d8-#x2df
  :test #'equalp)

(define-unibyte-encoder :iso-8859-2 (code)
  (or (cond ((< code #xa0) code)
            ((< code #x180)
             (svref +unicode-00a0-0180-to-iso-8859-2+
                    (the ub8 (- code #xa0))))
            ((<= #x2c0 code #x2df)
             (svref +unicode-02c0-02e0-to-iso-8859-2+
                    (the ub8 (- code #x2c0)))))
      (handle-error)))

(define-constant +iso-8859-2-to-unicode+
    #(;; #xa0
      #x00a0 #x0104 #x02d8 #x0141 #x00a4 #x013d #x015a #x00a7
      #x00a8 #x0160 #x015e #x0164 #x0179 #x00ad #x017d #x017b
      ;; #xb0
      #x00b0 #x0105 #x02db #x0142 #x00b4 #x013e #x015b #x02c7
      #x00b8 #x0161 #x015f #x0165 #x017a #x02dd #x017e #x017c
      ;; #xc0
      #x0154 #x00c1 #x00c2 #x0102 #x00c4 #x0139 #x0106 #x00c7
      #x010c #x00c9 #x0118 #x00cb #x011a #x00cd #x00ce #x010e
      ;; #xd0
      #x0110 #x0143 #x0147 #x00d3 #x00d4 #x0150 #x00d6 #x00d7
      #x0158 #x016e #x00da #x0170 #x00dc #x00dd #x0162 #x00df
      ;; #xe0
      #x0155 #x00e1 #x00e2 #x0103 #x00e4 #x013a #x0107 #x00e7
      #x010d #x00e9 #x0119 #x00eb #x011b #x00ed #x00ee #x010f
      ;; #xf0
      #x0111 #x0144 #x0148 #x00f3 #x00f4 #x0151 #x00f6 #x00f7
      #x0159 #x016f #x00fa #x0171 #x00fc #x00fd #x0163 #x02d9)
  :test #'equalp)

(define-unibyte-decoder :iso-8859-2 (octet)
  (if (< octet #xa0)
      octet
      (svref +iso-8859-2-to-unicode+ (the ub8 (- octet #xa0)))))

(define-character-encoding :iso-8859-3
    "An 8-bit, fixed-width character encoding in which codes
#x00-#x9f map to their Unicode equivalents and other codes map to
other Unicode character values.  Intended to provide most
characters found in most languages used in Southern Europe."
  :aliases '(:latin-3 :latin3)
  :literal-char-code-limit #xa0)

(define-constant +unicode-a0-100-to-iso-8859-3+
    #(#xa0 nil nil #xa3 #xa4 nil nil #xa7 ; #xa0-#xa7
      #xa8 nil nil nil nil #xad nil nil   ; #xa8-#xaf
      #xb0 nil #xb2 #xb3 #xb4 #xb5 nil #xb7 ; #xb0-#xb7
      #xb8 nil nil nil nil #xbd nil nil     ; #xb8-#xbf
      #xc0 #xc1 #xc2 nil #xc4 nil nil #xc7  ; #xc0-#xc7
      #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #xc8-#xcf
      nil #xd1 #xd2 #xd3 #xd4 nil #xd6 #xd7   ; #xd0-#xd7
      nil #xd9 #xda #xdb #xdc nil nil #xdf    ; #xd8-#xdf
      #xe0 #xe1 #xe2 nil #xe4 nil nil #xe7    ; #xe0-#xe7
      #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #xe8-#xef
      nil #xf1 #xf2 #xf3 #xf4 nil #xf6 #xf7   ; #xf0-#xf7
      nil #xf9 #xfa #xfb #xfc nil nil nil)    ; #xf8-#xff
  :test #'equalp)

(define-constant +unicode-108-180-to-iso-8859-3+
    #(#xc6 #xe6 #xc5 #xe5 #x00 #x00 #x00 #x00 ; #x108-#x10f
      nil nil nil nil nil nil nil nil         ; #x110-#x117
      nil nil nil nil #xd8 #xf8 #xab #xbb     ; #x118-#x11f
      #xd5 #xf5 nil nil #xa6 #xb6 #xa1 #xb1   ; #x120-#x127
      nil nil nil nil nil nil nil nil         ; #x128-#x12f
      #xa9 #xb9 nil nil #xac #xbc nil nil     ; #x130-#x137
      nil nil nil nil nil nil nil nil         ; #x138-#x13f
      nil nil nil nil nil nil nil nil         ; #x140-#x147
      nil nil nil nil nil nil nil nil         ; #x148-#x14f
      nil nil nil nil nil nil nil nil         ; #x150-#x157
      nil nil nil nil #xde #xfe #xaa #xba     ; #x158-#x15f
      nil nil nil nil nil nil nil nil         ; #x160-#x167
      nil nil nil nil #xdd #xfd nil nil       ; #x168-#x16f
      nil nil nil nil nil nil nil nil         ; #x170-#x177
      nil nil nil #xaf #xbf nil nil nil)      ; #x178-#x17f
  :test #'equalp)

(define-constant +unicode-2d8-2e0-to-iso-8859-3+
    #(#xa2 #xff nil nil nil nil nil nil) ; #x2d8-#x2df
  :test #'equalp)

(define-unibyte-encoder :iso-8859-3 (code)
  (or (cond ((< code #xa0) code)
            ((< code #x100)
             (svref +unicode-a0-100-to-iso-8859-3+
                    (the ub8 (- code #xa0))))
            ((<= #x108 code #x17f)
             (svref +unicode-108-180-to-iso-8859-3+
                    (the ub8 (- code #x108))))
            ((<= #x2d8 code #x2df)
             (svref +unicode-2d8-2e0-to-iso-8859-3+
                    (the ub8 (- code #x2d8)))))
      (handle-error)))

(define-constant +iso-8859-3-to-unicode+
    #(;; #xa0
      #x00a0 #x0126 #x02d8 #x00a3 #x00a4 #xfffd #x0124 #x00a7
      #x00a8 #x0130 #x015e #x011e #x0134 #x00ad #xfffd #x017b
      ;; #xb0
      #x00b0 #x0127 #x00b2 #x00b3 #x00b4 #x00b5 #x0125 #x00b7
      #x00b8 #x0131 #x015f #x011f #x0135 #x00bd #xfffd #x017c
      ;; #xc0
      #x00c0 #x00c1 #x00c2 #xfffd #x00c4 #x010a #x0108 #x00c7
      #x00c8 #x00c9 #x00ca #x00cb #x00cc #x00cd #x00ce #x00cf
      ;; #xd0
      #xfffd #x00d1 #x00d2 #x00d3 #x00d4 #x0120 #x00d6 #x00d7
      #x011c #x00d9 #x00da #x00db #x00dc #x016c #x015c #x00df
      ;; #xe0
      #x00e0 #x00e1 #x00e2 #xfffd #x00e4 #x010b #x0109 #x00e7
      #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
      ;; #xf0
      #xfffd #x00f1 #x00f2 #x00f3 #x00f4 #x0121 #x00f6 #x00f7
      #x011d #x00f9 #x00fa #x00fb #x00fc #x016d #x015d #x02d9)
  :test #'equalp)

(define-unibyte-decoder :iso-8859-3 (octet)
  (if (< octet #xa0)
      octet
      (svref +iso-8859-3-to-unicode+ (the ub8 (- octet #xa0)))))

(define-character-encoding :iso-8859-4
    "An 8-bit, fixed-width character encoding in which codes
#x00-#x9f map to their Unicode equivalents and other codes map to
other Unicode character values.  Intended to provide most
characters found in most languages used in Northern Europe."
  :aliases '(:latin-4 :latin4)
  :literal-char-code-limit #xa0)

(define-constant +unicode-a0-180-to-iso-8859-4+
    #(#xa0 nil nil nil #xa4 nil nil #xa7 ; #xa0-#xa7
      #xa8 nil nil nil nil #xad nil #xaf ; #xa8-#xaf
      #xb0 nil nil nil #xb4 nil nil nil  ; #xb0-#xb7
      #xb8 nil nil nil nil nil nil nil   ; #xb8-#xbf
      nil #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 nil ; #xc0-#xc7
      nil #xc9 nil #xcb nil #xcd #xce nil   ; #xc8-#xcf
      nil nil nil nil #xd4 #xd5 #xd6 #xd7   ; #xd0-#xd7
      #xd8 nil #xda #xdb #xdc nil nil #xdf  ; #xd8-#xdf
      nil #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 nil ; #xe0-#xe7
      nil #xe9 nil #xeb nil #xed #xee nil   ; #xe8-#xef
      nil nil nil nil #xf4 #xf5 #xf6 #xf7   ; #xf0-#xf7
      #xf8 nil #xfa #xfb #xfc nil nil nil   ; #xf8-#xff
      #xc0 #xe0 nil nil #xa1 #xb1 nil nil   ; #x100-#x107
      nil nil nil nil #xc8 #xe8 nil nil     ; #x108-#x10f
      #xd0 #xf0 #xaa #xba nil nil #xcc #xec ; #x110-#x117
      #xca #xea nil nil nil nil nil nil     ; #x118-#x11f
      nil nil #xab #xbb nil nil nil nil     ; #x120-#x127
      #xa5 #xb5 #xcf #xef nil nil #xc7 #xe7 ; #x128-#x12f
      nil nil nil nil nil nil #xd3 #xf3     ; #x130-#x137
      #xa2 nil nil #xa6 #xb6 nil nil nil    ; #x138-#x13f
      nil nil nil nil nil #xd1 #xf1 nil     ; #x140-#x147
      nil nil #xbd #xbf #xd2 #xf2 nil nil   ; #x148-#x14f
      nil nil nil nil nil nil #xa3 #xb3     ; #x150-#x157
      nil nil nil nil nil nil nil nil       ; #x158-#x15f
      #xa9 #xb9 nil nil nil nil #xac #xbc   ; #x160-#x167
      #xdd #xfd #xde #xfe nil nil nil nil   ; #x168-#x16f
      nil nil #xd9 #xf9 nil nil nil nil     ; #x170-#x177
      nil nil nil nil nil #xae #xbe nil)    ; #x178-#x17f
  :test #'equalp)

(define-constant +unicode-2c0-2e0-to-iso-8859-4+
    #(nil nil nil nil nil nil nil #xb7  ; #x2c0-#x2c7
      nil nil nil nil nil nil nil nil   ; #x2c8-#x2cf
      nil nil nil nil nil nil nil nil   ; #x2d0-#x2d7
      nil #xff nil #xb2 nil nil nil nil) ; #x2d8-#x2df
  :test #'equalp)

(define-unibyte-encoder :iso-8859-4 (code)
  (or (cond ((< code #xa0) code)
            ((< code #x180)
             (svref +unicode-a0-180-to-iso-8859-4+
                    (the ub8 (- code #xa0))))
            ((<= #x2c0 code #x2df)
             (svref +unicode-2c0-2e0-to-iso-8859-4+
                    (the ub8 (- code #x2c0)))))
      (handle-error)))

(define-constant +iso-8859-4-to-unicode+
    #(;; #xa0
      #x00a0 #x0104 #x0138 #x0156 #x00a4 #x0128 #x013b #x00a7
      #x00a8 #x0160 #x0112 #x0122 #x0166 #x00ad #x017d #x00af
      ;; #xb0
      #x00b0 #x0105 #x02db #x0157 #x00b4 #x0129 #x013c #x02c7
      #x00b8 #x0161 #x0113 #x0123 #x0167 #x014a #x017e #x014b
      ;; #xc0
      #x0100 #x00c1 #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x012e
      #x010c #x00c9 #x0118 #x00cb #x0116 #x00cd #x00ce #x012a
      ;; #xd0
      #x0110 #x0145 #x014c #x0136 #x00d4 #x00d5 #x00d6 #x00d7
      #x00d8 #x0172 #x00da #x00db #x00dc #x0168 #x016a #x00df
      ;; #xe0
      #x0101 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x012f
      #x010d #x00e9 #x0119 #x00eb #x0117 #x00ed #x00ee #x012b
      ;; #xf0
      #x0111 #x0146 #x014d #x0137 #x00f4 #x00f5 #x00f6 #x00f7
      #x00f8 #x0173 #x00fa #x00fb #x00fc #x0169 #x016b #x02d9)
  :test #'equalp)

(define-unibyte-decoder :iso-8859-4 (octet)
  (if (< octet #xa0)
      octet
      (svref +iso-8859-4-to-unicode+ (the ub8 (- octet #xa0)))))

(define-character-encoding :iso-8859-5
    "An 8-bit, fixed-width character encoding in which codes
#x00-#x9f map to their Unicode equivalents and other codes map to
other Unicode character values.  Intended to provide most
characters found in the Cyrillic alphabet."
  :aliases '(:cyrillic)
  :literal-char-code-limit #xa0)

(define-constant +unicode-a0-b0-to-iso-8859-5+
    #(#xa0 nil nil nil nil nil nil #xfd ; #xa0-#xa7
      nil nil nil nil nil #xad nil nil) ; #xa8-#xaf
  :test #'equalp)

(define-constant +unicode-400-460-to-iso-8859-5+
    #(nil #xa1 #xa2 #xa3 #xa4 #xa5 #xa6 #xa7 ; #x400-#x407
      #xa8 #xa9 #xaa #xab #xac nil #xae #xaf ; #x408-#x40f
      #xb0 #xb1 #xb2 #xb3 #xb4 #xb5 #xb6 #xb7 ; #x410-#x417
      #xb8 #xb9 #xba #xbb #xbc #xbd #xbe #xbf ; #x418-#x41f
      #xc0 #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 #xc7 ; #x420-#x427
      #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #x428-#x42f
      #xd0 #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7 ; #x430-#x437
      #xd8 #xd9 #xda #xdb #xdc #xdd #xde #xdf ; #x438-#x43f
      #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #x440-#x447
      #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #x448-#x44f
      nil #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7  ; #x450-#x457
      #xf8 #xf9 #xfa #xfb #xfc nil #xfe #xff) ; #x458-#x45f
  :test #'equalp)

(define-unibyte-encoder :iso-8859-5 (code)
  (or (cond ((< code #xa0) code)
            ((< code #xb0)
             (svref +unicode-a0-b0-to-iso-8859-5+
                    (the ub8 (- code #xa0))))
            ((<= #x400 code #x45f)
             (svref +unicode-400-460-to-iso-8859-5+
                    (the ub8 (- code #x400))))
            ;; the Numero sign
            ((= code #x2116) #xf0))
      (handle-error)))

(define-constant +iso-8859-5-to-unicode+
    #(;; #xa0
      #x00a0 #x0401 #x0402 #x0403 #x0404 #x0405 #x0406 #x0407
      #x0408 #x0409 #x040a #x040b #x040c #x00ad #x040e #x040f
      ;; #xb0
      #x0410 #x0411 #x0412 #x0413 #x0414 #x0415 #x0416 #x0417
      #x0418 #x0419 #x041a #x041b #x041c #x041d #x041e #x041f
      ;; #xc0
      #x0420 #x0421 #x0422 #x0423 #x0424 #x0425 #x0426 #x0427
      #x0428 #x0429 #x042a #x042b #x042c #x042d #x042e #x042f
      ;; #xd0
      #x0430 #x0431 #x0432 #x0433 #x0434 #x0435 #x0436 #x0437
      #x0438 #x0439 #x043a #x043b #x043c #x043d #x043e #x043f
      ;; #xe0
      #x0440 #x0441 #x0442 #x0443 #x0444 #x0445 #x0446 #x0447
      #x0448 #x0449 #x044a #x044b #x044c #x044d #x044e #x044f
      ;; #xf0
      #x2116 #x0451 #x0452 #x0453 #x0454 #x0455 #x0456 #x0457
      #x0458 #x0459 #x045a #x045b #x045c #x00a7 #x045e #x045f)
  :test #'equalp)

(define-unibyte-decoder :iso-8859-5 (octet)
  (if (< octet #xa0)
      octet
      (svref +iso-8859-5-to-unicode+ (the ub8 (- octet #xa0)))))

(define-character-encoding :iso-8859-6
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in the
Arabic alphabet."
  :aliases '(:arabic)
  :literal-char-code-limit #xa0)

(define-constant +unicode-a0-b0-to-iso-8859-6+
    #(#xa0 nil nil nil #xa4 nil nil nil ; #xa0-#xa7
      nil nil nil nil nil #xad nil nil) ; #xa8-#xaf
  :test #'equalp)

(define-constant +unicode-608-658-to-iso-8859-6+
    #(nil nil nil nil #xac nil nil nil  ; #x608-#x60f
      nil nil nil nil nil nil nil nil   ; #x610-#x617
      nil nil nil #xbb nil nil nil #xbf ; #x618-#x61f
      nil #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 #xc7  ; #x620-#x627
      #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #x628-#x62f
      #xd0 #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7 ; #x630-#x637
      #xd8 #xd9 #xda nil nil nil nil nil      ; #x638-#x63f
      #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #x640-#x647
      #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #x648-#x64f
      #xf0 #xf1 #xf2 nil nil nil nil nil)     ; #x650-#x657
  :test #'equalp)

(define-unibyte-encoder :iso-8859-6 (code)
  (or (cond ((< code #xa0) code)
            ((< code #xb0)
             (svref +unicode-a0-b0-to-iso-8859-6+
                    (the ub8 (- code #xa0))))
            ((<= #x608 code #x657)
             (svref +unicode-608-658-to-iso-8859-6+
                    (the ub8 (- code #x608)))))
      (handle-error)))

(define-constant +iso-8859-6-to-unicode+
    #(;; #xa0
      #x00a0 #xfffd #xfffd #xfffd #x00a4 #xfffd #xfffd #xfffd
      #xfffd #xfffd #xfffd #xfffd #x060c #x00ad #xfffd #xfffd
      ;; #xb0
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      #xfffd #xfffd #xfffd #x061b #xfffd #xfffd #xfffd #x061f
      ;; #xc0
      #xfffd #x0621 #x0622 #x0623 #x0624 #x0625 #x0626 #x0627
      #x0628 #x0629 #x062a #x062b #x062c #x062d #x062e #x062f
      ;; #xd0
      #x0630 #x0631 #x0632 #x0633 #x0634 #x0635 #x0636 #x0637
      #x0638 #x0639 #x063a #xfffd #xfffd #xfffd #xfffd #xfffd
      ;; #xe0
      #x0640 #x0641 #x0642 #x0643 #x0644 #x0645 #x0646 #x0647
      #x0648 #x0649 #x064a #x064b #x064c #x064d #x064e #x064f
      ;; #xf0
      #x0650 #x0651 #x0652 #xfffd #xfffd #xfffd #xfffd #xfffd
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd)
  :test #'equalp)

(define-unibyte-decoder :iso-8859-6 (octet)
  (if (< octet #xa0)
      octet
      (svref +iso-8859-6-to-unicode+ (the ub8 (- octet #xa0)))))

(define-character-encoding :iso-8859-7
    "An 8-bit, fixed-width character encoding in which codes
#x00-#x9f map to their Unicode equivalents and other codes map to
other Unicode character values.  Intended to provide most
characters found in the Greek alphabet."
  :aliases '(:greek)
  :literal-char-code-limit #xa0)

(define-constant +unicode-a0-c0-to-iso-8859-7+
    #(#xa0 nil nil #xa3 nil nil #xa6 #xa7  ; #xa0-#xa7
      #xa8 #xa9 nil #xab #xac #xad nil nil ; #xa8-#xaf
      #xb0 #xb1 #xb2 #xb3 nil nil nil #xb7 ; #xb0-#xb7
      nil nil nil #xbb nil #xbd nil nil)   ; #xb8-#xbf
  :test #'equalp)

(define-constant +unicode-378-3d0-to-iso-8859-7+
    #(nil nil #xaa nil nil nil nil nil    ; #x378-#x37f
      nil nil nil nil #xb4 #xb5 #xb6 nil  ; #x380-#x387
      #xb8 #xb9 #xba nil #xbc nil #xbe #xbf   ; #x388-#x38f
      #xc0 #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 #xc7 ; #x390-#x397
      #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #x398-#x39f
      #xd0 #xd1 nil #xd3 #xd4 #xd5 #xd6 #xd7  ; #x3a0-#x3a7
      #xd8 #xd9 #xda #xdb #xdc #xdd #xde #xdf ; #x3a8-#x3af
      #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #x3b0-#x3b7
      #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #x3b8-#x3bf
      #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 ; #x3c0-#x3c7
      #xf8 #xf9 #xfa #xfb #xfc #xfd #xfe nil) ; #x3c8-#x3cf
  :test #'equalp)

(define-constant +unicode-2010-2020-to-iso-8859-7+
    #(nil nil nil nil nil #xaf nil nil   ; #x2010-#x2017
      #xa1 #xa2 nil nil nil nil nil nil) ; #x2018-#x201f
  :test #'equalp)

(define-constant +unicode-20ac-20b0-to-iso-8859-7+
    #(#xa4 nil nil #xa5)
  :test #'equalp)

(define-unibyte-encoder :iso-8859-7 (code)
  (or (cond ((< code #xa0) code)
            ((< code #xc0)
             (svref +unicode-a0-c0-to-iso-8859-7+
                    (the ub8 (- code #xa0))))
            ((<= #x378 code #x3cf)
             (svref +unicode-378-3d0-to-iso-8859-7+
                    (the ub8 (- code #x378))))
            ((<= #x2010 code #x201f)
             (svref +unicode-2010-2020-to-iso-8859-7+
                    (the ub8 (- code #x2010))))
            ((<= #x201c code #x20af)
             (svref +unicode-20ac-20b0-to-iso-8859-7+
                    (the ub8 (- code #x20ac)))))
      (handle-error)))

(define-constant +iso-8859-7-to-unicode+
    #(;; #xa0
      #x00a0 #x2018 #x2019 #x00a3 #x20ac #x20af #x00a6 #x00a7
      #x00a8 #x00a9 #x037a #x00ab #x00ac #x00ad #xfffd #x2015
      ;; #xb0
      #x00b0 #x00b1 #x00b2 #x00b3 #x0384 #x0385 #x0386 #x00b7
      #x0388 #x0389 #x038a #x00bb #x038c #x00bd #x038e #x038f
      ;; #xc0
      #x0390 #x0391 #x0392 #x0393 #x0394 #x0395 #x0396 #x0397
      #x0398 #x0399 #x039a #x039b #x039c #x039d #x039e #x039f
      ;; #xd0
      #x03a0 #x03a1 #xfffd #x03a3 #x03a4 #x03a5 #x03a6 #x03a7
      #x03a8 #x03a9 #x03aa #x03ab #x03ac #x03ad #x03ae #x03af
      ;; #xe0
      #x03b0 #x03b1 #x03b2 #x03b3 #x03b4 #x03b5 #x03b6 #x03b7
      #x03b8 #x03b9 #x03ba #x03bb #x03bc #x03bd #x03be #x03bf
      ;; #xf0
      #x03c0 #x03c1 #x03c2 #x03c3 #x03c4 #x03c5 #x03c6 #x03c7
      #x03c8 #x03c9 #x03ca #x03cb #x03cc #x03cd #x03ce #xfffd)
  :test #'equalp)

(define-unibyte-decoder :iso-8859-7 (octet)
  (if (< octet #xa0)
      octet
      (svref +iso-8859-7-to-unicode+ (the ub8 (- octet #xa0)))))

(define-character-encoding :iso-8859-8
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in the
Hebrew alphabet."
  :aliases '(:hebrew)
  :literal-char-code-limit #xa0)

(define-constant +unicode-a0-f8-to-iso-8859-8+
    #(#xa0 nil #xa2 #xa3 #xa4 #xa5 #xa6 #xa7 ; #xa0-#xa7
      #xa8 #xa9 nil #xab #xac #xad #xae #xaf ; #xa8-#xaf
      #xb0 #xb1 #xb2 #xb3 #xb4 #xb5 #xb6 #xb7 ; #xb0-#xb7
      #xb8 #xb9 nil #xbb #xbc #xbd #xbe nil   ; #xb8-#xbf
      nil nil nil nil nil nil nil nil         ; #xc0-#xc7
      nil nil nil nil nil nil nil nil         ; #xc8-#xcf
      nil nil nil nil nil nil nil #xaa        ; #xd0-#xd7
      nil nil nil nil nil nil nil nil         ; #xd8-#xdf
      nil nil nil nil nil nil nil nil         ; #xe0-#xe7
      nil nil nil nil nil nil nil nil         ; #xe8-#xef
      nil nil nil nil nil nil nil #xba)       ; #xf0-#xf7
  :test #'equalp)

(define-constant +unicode-5d0-5f0-to-iso-8859-8+
    #(#xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #x5d0-#x5d7
      #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #x5d8-#x5df
      #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 ; #x5e0-#x5e7
      #xf8 #xf9 #xfa nil nil nil nil nil)     ; #x5e8-#x5ef
  :test #'equalp)

(define-constant +unicode-2008-2018-to-iso-8859-8+
    #(nil nil nil nil nil nil #xfd #xfe   ; #x2008-#x200f
      nil nil nil nil nil nil nil #xdf)   ; #x2010-#x2017
  :test #'equalp)

(define-unibyte-encoder :iso-8859-8 (code)
  (or (cond ((< code #xa0) code)
            ((< code #xf8)
             (svref +unicode-a0-f8-to-iso-8859-8+
                    (the ub8 (- code #xa0))))
            ((<= #x5d0 code #x5ef)
             (svref +unicode-5d0-5f0-to-iso-8859-8+
                    (the ub8 (- code #x5d0))))
            ((<= #x2008 code #x201f)
             (svref +unicode-2008-2018-to-iso-8859-8+
                    (the ub8 (- code #x2008)))))
      (handle-error)))

(define-constant +iso-8859-8-to-unicode+
    #(;; #xa0
      #x00a0 #xfffd #x00a2 #x00a3 #x00a4 #x00a5 #x00a6 #x00a7
      #x00a8 #x00a9 #x00d7 #x00ab #x00ac #x00ad #x00ae #x00af
      ;; #xb0
      #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
      #x00b8 #x00b9 #x00f7 #x00bb #x00bc #x00bd #x00be #xfffd
      ;; #xc0
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      ;; #xd0
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #x2017
      ;; #xe0
      #x05d0 #x05d1 #x05d2 #x05d3 #x05d4 #x05d5 #x05d6 #x05d7
      #x05d8 #x05d9 #x05da #x05db #x05dc #x05dd #x05de #x05df
      ;; #xf0
      #x05e0 #x05e1 #x05e2 #x05e3 #x05e4 #x05e5 #x05e6 #x05e7
      #x05e8 #x05e9 #x05ea #xfffd #xfffd #x200e #x200f #xfffd)
  :test #'equalp)

(define-unibyte-decoder :iso-8859-8 (octet)
  (if (< octet #xa0)
      octet
      (svref +iso-8859-8-to-unicode+ (the ub8 (- octet #xa0)))))

(define-character-encoding :iso-8859-9
    "An 8-bit, fixed-width character encoding in which codes
#x00-#xcf map to their Unicode equivalents and other codes map to
other Unicode character values.  Intended to provide most
characters found in the Turkish alphabet."
  :aliases '(:latin-5 :latin5)
  :decode-literal-code-unit-limit #xd0
  :encode-literal-code-unit-limit #xa0)

(define-constant +unicode-d0-100-to-iso-8859-9+
    #(nil #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7 ; #xd0-#xd7
      #xd8 #xd9 #xda #xdb #xdc nil nil #xdf  ; #xd8-#xdf
      #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #xe0-#xe7
      #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #xe8-#xef
      nil #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7  ; #xf0-#xf7
      #xf8 #xf9 #xfa #xfb #xfc nil nil #xff)  ; #xf8-#xff
  :test #'equalp)

(define-constant +unicode-118-160-to-iso-8859-9+
    #(nil nil nil nil nil nil #xd0 #xf0 ; #x118-#x11f
      nil nil nil nil nil nil nil nil   ; #x120-#x127
      nil nil nil nil nil nil nil nil   ; #x128-#x12f
      #xdd #xfd nil nil nil nil nil nil ; #x130-#x137
      nil nil nil nil nil nil nil nil   ; #x138-#x13f
      nil nil nil nil nil nil nil nil   ; #x140-#x147
      nil nil nil nil nil nil nil nil   ; #x148-#x14f
      nil nil nil nil nil nil nil nil   ; #x150-#x157
      nil nil nil nil nil nil #xde #xfe) ; #x158-#x15f
  :test #'equalp)

(define-unibyte-encoder :iso-8859-9 (code)
  (or (cond ((< code #xd0) code)
            ((< code #x100)
             (svref +unicode-d0-100-to-iso-8859-9+
                    (the ub8 (- code #xd0))))
            ((<= #x118 code #x15f)
             (svref +unicode-118-160-to-iso-8859-9+
                    (the ub8 (- code #x118)))))
      (handle-error)))

(define-constant +iso-8859-9-to-unicode+
    #(;; #xd0
      #x011e #x00d1 #x00d2 #x00d3 #x00d4 #x00d5 #x00d6 #x00d7
      #x00d8 #x00d9 #x00da #x00db #x00dc #x0130 #x015e #x00df
      ;; #xe0
      #x00e0 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x00e7
      #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
      ;; #xf0
      #x011f #x00f1 #x00f2 #x00f3 #x00f4 #x00f5 #x00f6 #x00f7
      #x00f8 #x00f9 #x00fa #x00fb #x00fc #x0131 #x015f #x00ff)
  :test #'equalp)

(define-unibyte-decoder :iso-8859-9 (octet)
  (if (< octet #xd0)
      octet
      (svref +iso-8859-9-to-unicode+ (the ub8 (- octet #xd0)))))

(define-character-encoding :iso-8859-10
    "An 8-bit, fixed-width character encoding in which codes
#x00-#x9f map to their Unicode equivalents and other codes map to
other Unicode character values.  Intended to provide most
characters found in Nordic alphabets."
  :aliases '(:latin-6 :latin6)
  :literal-char-code-limit #xa0)

(define-constant +unicode-a0-180-to-iso-8859-10+
    #(#xa0 nil nil nil nil nil nil #xa7  ; #xa0-#xa7
      nil nil nil nil nil #xad nil nil   ; #xa8-#xaf
      #xb0 nil nil nil nil nil nil #xb7  ; #xb0-#xb7
      nil nil nil nil nil #x2015 nil nil ; #xb8-#xbf
      nil #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 nil ; #xc0-#xc7
      nil #xc9 nil #xcb nil #xcd #xce #xcf  ; #xc8-#xcf
      #xd0 nil nil #xd3 #xd4 #xd5 #xd6 nil  ; #xd0-#xd7
      #xd8 nil #xda #xdb #xdc #xdd #xde #xdf ; #xd8-#xdf
      nil #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 nil  ; #xe0-#xe7
      nil #xe9 nil #xeb nil #xed #xee #xef   ; #xe8-#xef
      #xf0 nil nil #xf3 #xf4 #xf5 #xf6 nil   ; #xf0-#xf7
      #xf8 nil #xfa #xfb #xfc #xfd #xfe nil  ; #xf8-#xff
      #xc0 #xe0 nil nil #xa1 #xb1 nil nil    ; #x100-#x107
      nil nil nil nil #xc8 #xe8 nil nil      ; #x108-#x10f
      #xa9 #xb9 #xa2 #xb2 nil nil #xcc #xec  ; #x110-#x117
      #xca #xea nil nil nil nil nil nil      ; #x118-#x11f
      nil nil #xa3 #xb3 nil nil nil nil      ; #x120-#x127
      #xa5 #xb5 #xa4 #xb4 nil nil #xc7 #xe7  ; #x128-#x12f
      nil nil nil nil nil nil #xa6 #xb6      ; #x130-#x137
      #xff nil nil #xa8 #xb8 nil nil nil     ; #x138-#x13f
      nil nil nil nil nil #xd1 #xf1 nil      ; #x140-#x147
      nil nil #xaf #xbf #xd2 #xf2 nil nil    ; #x148-#x14f
      nil nil nil nil nil nil nil nil        ; #x150-#x157
      nil nil nil nil nil nil nil nil        ; #x158-#x15f
      #xaa #xba nil nil nil nil #xab #xbb    ; #x160-#x167
      #xd7 #xf7 #xae #xbe nil nil nil nil    ; #x168-#x16f
      nil nil #xd9 #xf9 nil nil nil nil      ; #x170-#x177
      nil nil nil nil nil #xac #xbc nil)     ; #x178-#x17f
  :test #'equalp)

(define-unibyte-encoder :iso-8859-10 (code)
  (or (cond ((< code #xa0) code)
            ((< code #x180)
             (svref +unicode-a0-180-to-iso-8859-10+
                    (the ub8 (- code #xa0))))
            ;; Horizontal bar
            ((= code #x2015) #xbd))
      (handle-error)))

(define-constant +iso-8859-10-to-unicode+
    #(;; #xa0
      #x00a0 #x0104 #x0112 #x0122 #x012a #x0128 #x0136 #x00a7
      #x013b #x0110 #x0160 #x0166 #x017d #x00ad #x016a #x014a
      ;; #xb0
      #x00b0 #x0105 #x0113 #x0123 #x012b #x0129 #x0137 #x00b7
      #x013c #x0111 #x0161 #x0167 #x017e #x2015 #x016b #x014b
      ;; #xc0
      #x0100 #x00c1 #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x012e
      #x010c #x00c9 #x0118 #x00cb #x0116 #x00cd #x00ce #x00cf
      ;; #xd0
      #x00d0 #x0145 #x014c #x00d3 #x00d4 #x00d5 #x00d6 #x0168
      #x00d8 #x0172 #x00da #x00db #x00dc #x00dd #x00de #x00df
      ;; #xe0
      #x0101 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x012f
      #x010d #x00e9 #x0119 #x00eb #x0117 #x00ed #x00ee #x00ef
      ;; #xf0
      #x00f0 #x0146 #x014d #x00f3 #x00f4 #x00f5 #x00f6 #x0169
      #x00f8 #x0173 #x00fa #x00fb #x00fc #x00fd #x00fe #x0138)
  :test #'equalp)

(define-unibyte-decoder :iso-8859-10 (octet)
  (if (< octet #xa0)
      octet
      (svref +iso-8859-10-to-unicode+ (the ub8 (- octet #xa0)))))

(define-character-encoding :iso-8859-11
    "An 8-bit, fixed-width character encoding in which codes
#x00-#x9f map to their Unicode equivalents and other codes map to
other Unicode character values.  Intended to provide most
characters found the Thai alphabet."
  :aliases '()
  :literal-char-code-limit #xa0)

(define-unibyte-encoder :iso-8859-11 (code)
  (cond ((< code #xa1) code)
        ((and (<= #xe01 code #xe5b)
              (not (<= #xe3b code #xe3e))
              (not (<= #xe5c code #xe5f)))
         (- code #xd60))
        (t (handle-error))))

(define-unibyte-decoder :iso-8859-11 (octet)
  (cond ((<= octet #xa0) octet)
        ((or (<= #xdb octet #xde)
             (<= #xfc octet #xff))
         #xfffd)
        ((<= octet #xfb)
         (+ octet #x0d60))
        (t (handle-error))))

;;; There is no iso-8859-12 encoding.

(define-character-encoding :iso-8859-13
    "An 8-bit, fixed-width character encoding in which codes
#x00-#x9f map to their Unicode equivalents and other codes map to
other Unicode character values.  Intended to provide most
characters found in Baltic alphabets."
  :aliases '()
  :literal-char-code-limit #xa0)

(define-constant +unicode-a0-180-to-iso-8859-13+
    #(#xa0 nil #xa2 #xa3 #xa4 nil #xa6 #xa7 ; #xa0-#xa7
      nil #xa9 nil #xab #xac #xad #xae nil  ; #xa8-#xaf
      #xb0 #xb1 #xb2 #xb3 nil #xb5 #xb6 #xb7 ; #xb0-#xb7
      nil #xb9 nil #xbb #xbc #xbd #xbe nil   ; #xb8-#xbf
      nil nil nil nil #xc4 #xc5 #xaf nil     ; #xc0-#xc7
      nil #xc9 nil nil nil nil nil nil       ; #xc8-#xcf
      nil nil nil #xd3 nil #xd5 #xd6 #xd7    ; #xd0-#xd7
      #xa8 nil nil nil #xdc nil nil #xdf     ; #xd8-#xdf
      nil nil nil nil #xe4 #xe5 #xbf nil     ; #xe0-#xe7
      nil #xe9 nil nil nil nil nil nil       ; #xe8-#xef
      nil nil nil #xf3 nil #xf5 #xf6 #xf7    ; #xf0-#xf7
      #xb8 nil nil nil #xfc nil nil nil      ; #xf8-#xff
      #xc2 #xe2 nil nil #xc0 #xe0 #xc3 #xe3  ; #x100-#x107
      nil nil nil nil #xc8 #xe8 nil nil      ; #x108-#x10f
      nil nil #xc7 #xe7 nil nil #xcb #xeb    ; #x110-#x117
      #xc6 #xe6 nil nil nil nil nil nil      ; #x118-#x11f
      nil nil #xcc #xec nil nil nil nil      ; #x120-#x127
      nil nil #xce #xee nil nil #xc1 #xe1    ; #x128-#x12f
      nil nil nil nil nil nil #xcd #xed      ; #x130-#x137
      nil nil nil #xcf #xef nil nil nil      ; #x138-#x13f
      nil #xd9 #xf9 #xd1 #xf1 #xd2 #xf2 nil  ; #x140-#x147
      nil nil nil nil #xd4 #xf4 nil nil      ; #x148-#x14f
      nil nil nil nil nil nil #xaa #xba      ; #x150-#x157
      nil nil #xda #xfa nil nil nil nil      ; #x158-#x15f
      #xd0 #xf0 nil nil nil nil nil nil      ; #x160-#x167
      nil nil #xdb #xfb nil nil nil nil      ; #x168-#x16f
      nil nil #xd8 #xf8 nil nil nil nil      ; #x170-#x177
      nil #xca #xea #xdd #xfd #xde #xfe nil) ; #x178-#x17f
  :test #'equalp)

(define-constant +unicode-2018-2020-to-iso-8859-13+
    #(nil #xff nil nil #xb4 #xa1 #xa5 nil) ; #x2018-#x201f
  :test #'equalp)

(define-unibyte-encoder :iso-8859-13 (code)
  (or (cond ((< code #xa0) code)
            ((< code #x180)
             (svref +unicode-a0-180-to-iso-8859-13+
                    (the ub8 (- code #xa0))))
            ((<= #x2018 code #x201f)
             (svref +unicode-2018-2020-to-iso-8859-13+
                    (the ub8 (- code #x2018)))))
      (handle-error)))

(define-constant +iso-8859-13-to-unicode+
    #(;; #xa0
      #x00a0 #x201d #x00a2 #x00a3 #x00a4 #x201e #x00a6 #x00a7
      #x00d8 #x00a9 #x0156 #x00ab #x00ac #x00ad #x00ae #x00c6
      ;; #xb0
      #x00b0 #x00b1 #x00b2 #x00b3 #x201c #x00b5 #x00b6 #x00b7
      #x00f8 #x00b9 #x0157 #x00bb #x00bc #x00bd #x00be #x00e6
      ;; #xc0
      #x0104 #x012e #x0100 #x0106 #x00c4 #x00c5 #x0118 #x0112
      #x010c #x00c9 #x0179 #x0116 #x0122 #x0136 #x012a #x013b
      ;; #xd0
      #x0160 #x0143 #x0145 #x00d3 #x014c #x00d5 #x00d6 #x00d7
      #x0172 #x0141 #x015a #x016a #x00dc #x017b #x017d #x00df
      ;; #xe0
      #x0105 #x012f #x0101 #x0107 #x00e4 #x00e5 #x0119 #x0113
      #x010d #x00e9 #x017a #x0117 #x0123 #x0137 #x012b #x013c
      ;; #xf0
      #x0161 #x0144 #x0146 #x00f3 #x014d #x00f5 #x00f6 #x00f7
      #x0173 #x0142 #x015b #x016b #x00fc #x017c #x017e #x2019)
  :test #'equalp)

(define-unibyte-decoder :iso-8859-13 (octet)
  (if (< octet #xa0)
      octet
      (svref +iso-8859-13-to-unicode+ (the ub8 (- octet #xa0)))))

(define-character-encoding :iso-8859-14
    "An 8-bit, fixed-width character encoding in which codes
#x00-#x9f map to their Unicode equivalents and other codes map to
other Unicode character values.  Intended to provide most
characters found in Celtic languages."
  :aliases '(:latin-8 :latin8)
  :literal-char-code-limit #xa0)

(define-constant +unicode-a0-100-to-iso-8859-14+
    #(#xa0 nil nil #xa3 nil nil nil #xa7 ; #xa0-#xa7
      nil #xa9 nil nil nil #xad #xae nil ; #xa8-#xaf
      nil nil nil nil nil nil #xb6 nil   ; #xb0-#xb7
      nil nil nil nil nil nil nil nil    ; #xb8-#xbf
      #xc0 #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 #xc7 ; #xc0-#xc7
      #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #xc8-#xcf
      nil #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 nil   ; #xd0-#xd7
      #xd8 #xd9 #xda #xdb #xdc #xdd nil #xdf  ; #xd8-#xdf
      #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #xe0-#xe7
      #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #xe8-#xef
      nil #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 nil   ; #xf0-#xf7
      #xf8 #xf9 #xfa #xfb #xfc #xfd nil #xff) ; #xf8-#xff
  :test #'equalp)

(define-constant +unicode-108-128-to-iso-8859-14+
    #(nil nil #xa4 #xa5 nil nil nil nil ; #x108-#x10f
      nil nil nil nil nil nil nil nil   ; #x110-#x117
      nil nil nil nil nil nil nil nil   ; #x118-#x11f
      #xb2 #xb3 nil nil nil nil nil nil) ; #x120-#x127
  :test #'equalp)

(define-constant +unicode-170-180-to-iso-8859-14+
    #(nil nil nil nil #xd0 #xf0 #xde #xfe  ; #x170-#x177
      #xaf nil nil nil nil nil nil nil)    ; #x178-#x17f
  :test #'equalp)

(define-constant +unicode-1e00-1e88-to-iso-8859-14+
    #(nil nil #xa1 #xa2 nil nil nil nil ; #x1e00-#x1e07
      nil nil #xa6 #xab nil nil nil nil ; #x1e08-#x1e0f
      nil nil nil nil nil nil nil nil   ; #x1e10-#x1e17
      nil nil nil nil nil nil #xb0 #xb1 ; #x1e18-#x1e1f
      nil nil nil nil nil nil nil nil   ; #x1e20-#x1e27
      nil nil nil nil nil nil nil nil   ; #x1e28-#x1e2f
      nil nil nil nil nil nil nil nil   ; #x1e30-#x1e37
      nil nil nil nil nil nil nil nil   ; #x1e38-#x1e3f
      #xb4 #xb5 nil nil nil nil nil nil ; #x1e40-#x1e47
      nil nil nil nil nil nil nil nil   ; #x1e48-#x1e4f
      nil nil nil nil nil nil #xb7 #xb9 ; #x1e50-#x1e57
      nil nil nil nil nil nil nil nil   ; #x1e58-#x1e5f
      #xbb #xbf nil nil nil nil nil nil ; #x1e60-#x1e67
      nil nil #xd7 #xf7 nil nil nil nil ; #x1e68-#x1e6f
      nil nil nil nil nil nil nil nil   ; #x1e70-#x1e77
      nil nil nil nil nil nil nil nil   ; #x1e78-#x1e7f
      #xa8 #xb8 #xaa #xba #xbd #xbe nil nil) ; #x1e80-#x1e87
  :test #'equalp)

(define-constant +unicode-1ef0-1ef8-to-iso-8859-14+
    #(nil nil #xac #xbc nil nil nil nil) ; #x1ef0-#x1ef7
  :test #'equalp)

(define-unibyte-encoder :iso-8859-14 (code)
  (or (cond ((< code #xa0) code)
            ((< code #x100)
             (svref +unicode-a0-100-to-iso-8859-14+
                    (the ub8 (- code #xa0))))
            ((<= #x108 code #x127)
             (svref +unicode-108-128-to-iso-8859-14+
                    (the ub8 (- code #x108))))
            ((<= #x170 code #x17f)
             (svref +unicode-170-180-to-iso-8859-14+
                    (the ub8 (- code #x170))))
            ((<= #x1e00 code #x1e87)
             (svref +unicode-1e00-1e88-to-iso-8859-14+
                    (the ub8 (- code #x1e00))))
            ((<= #x1ef0 code #x1ef7)
             (svref +unicode-1ef0-1ef8-to-iso-8859-14+
                    (the ub8 (- code #x1ef0)))))
      (handle-error)))

(define-constant +iso-8859-14-to-unicode+
    #(;; #xa0
      #x00a0 #x1e02 #x1e03 #x00a3 #x010a #x010b #x1e0a #x00a7
      #x1e80 #x00a9 #x1e82 #x1e0b #x1ef2 #x00ad #x00ae #x0178
      ;; #xb0
      #x1e1e #x1e1f #x0120 #x0121 #x1e40 #x1e41 #x00b6 #x1e56
      #x1e81 #x1e57 #x1e83 #x1e60 #x1ef3 #x1e84 #x1e85 #x1e61
      ;; #xc0
      #x00c0 #x00c1 #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x00c7
      #x00c8 #x00c9 #x00ca #x00cb #x00cc #x00cd #x00ce #x00cf
      ;; #xd0
      #x0174 #x00d1 #x00d2 #x00d3 #x00d4 #x00d5 #x00d6 #x1e6a
      #x00d8 #x00d9 #x00da #x00db #x00dc #x00dd #x0176 #x00df
      ;; #xe0
      #x00e0 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x00e7
      #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
      ;; #xf0
      #x0175 #x00f1 #x00f2 #x00f3 #x00f4 #x00f5 #x00f6 #x1e6b
      #x00f8 #x00f9 #x00fa #x00fb #x00fc #x00fd #x0177 #x00ff)
  :test #'equalp)

(define-unibyte-decoder :iso-8859-14 (octet)
  (if (< octet #xa0)
      octet
      (svref +iso-8859-14-to-unicode+ (the ub8 (- octet #xa0)))))

(define-character-encoding :iso-8859-15
    "An 8-bit, fixed-width character encoding in which codes
#x00-#x9f map to their Unicode equivalents and other codes map to
other Unicode character values.  Intended to provide most
characters found in Western European languages (including the
Euro sign and some other characters missing from ISO-8859-1."
  :aliases '(:latin-9 :latin9)
  :literal-char-code-limit #xa0)

(define-constant +unicode-a0-100-to-iso-8859-15+
    #(#xa0 #xa1 #xa2 #xa3 nil #xa5 nil #xa7  ; #xa0-#xa7
      nil #xa9 #xaa #xab #xac #xad #xae #xaf ; #xa8-#xaf
      #xb0 #xb1 #xb2 #xb3 nil #xb5 #xb6 #xb7 ; #xb0-#xb7
      nil #xb9 #xba #xbb nil nil nil #xbf    ; #xb8-0xbf
      #xc0 #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 #xc7 ; #xc0-#xc7
      #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #xc8-#xcf
      #xd0 #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7 ; #xd0-#xd7
      #xd8 #xd9 #xda #xdb #xdc #xdd #xde #xdf ; #xd8-#xdf
      #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #xe0-#xe7
      #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #xe8-#xef
      #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 ; #xf0-#xf7
      #xf8 #xf9 #xfa #xfb #xfc #xfd #xfe #xff) ; #xf8-#xff
  :test #'equalp)

(define-constant +unicode-150-180-to-iso-8859-15+
    #(nil nil #xbc #xbd nil nil nil nil ; #x150-#x157
      nil nil nil nil nil nil nil nil   ; #x158-#x15f
      #xa6 #xa8 nil nil nil nil nil nil ; #x160-#x167
      nil nil nil nil nil nil nil nil   ; #x168-#x16f
      nil nil nil nil nil nil nil nil   ; #x170-#x177
      #xbe nil nil nil nil #xb4 #xb8 nil) ; #x178-#x17f
  :test #'equalp)

(define-unibyte-encoder :iso-8859-15 (code)
  (or (cond ((< code #xa0) code)
            ((< code #x100)
             (svref +unicode-a0-100-to-iso-8859-15+
                    (the ub8 (- code #xa0))))
            ((<= #x150 code #x1f7)
             (svref +unicode-150-180-to-iso-8859-15+
                    (the ub8 (- code #x150))))
            ;; Euro sign
            ((= code #x20ac) #xa4))
      (handle-error)))

(define-constant +iso-8859-15-to-unicode+
    #(;; #xa0
      #x00a0 #x00a1 #x00a2 #x00a3 #x20ac #x00a5 #x0160 #x00a7
      #x0161 #x00a9 #x00aa #x00ab #x00ac #x00ad #x00ae #x00af
      ;; #xb0
      #x00b0 #x00b1 #x00b2 #x00b3 #x017d #x00b5 #x00b6 #x00b7
      #x017e #x00b9 #x00ba #x00bb #x0152 #x0153 #x0178 #x00bf
      ;; #xc0
      #x00c0 #x00c1 #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x00c7
      ;; #xc8
      #x00c8 #x00c9 #x00ca #x00cb #x00cc #x00cd #x00ce #x00cf
      ;; #xd0
      #x00d0 #x00d1 #x00d2 #x00d3 #x00d4 #x00d5 #x00d6 #x00d7
      ;; #xd8
      #x00d8 #x00d9 #x00da #x00db #x00dc #x00dd #x00de #x00df
      ;; #xe0
      #x00e0 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x00e7
      ;; #xe8
      #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
      ;; #xf0
      #x00f0 #x00f1 #x00f2 #x00f3 #x00f4 #x00f5 #x00f6 #x00f7
      ;; #xf8
      #x00f8 #x00f9 #x00fa #x00fb #x00fc #x00fd #x00fe #x00ff)
  :test #'equalp)

(define-unibyte-decoder :iso-8859-15 (octet)
  (if (< octet #xa0)
      octet
      (svref +iso-8859-15-to-unicode+ (the ub8 (- octet #xa0)))))

(define-character-encoding :iso-8859-16
    "An 8-bit, fixed-width character encoding in which codes
#x00-#x9f map to their Unicode equivalents and other codes map to
other Unicode character values.  Intended to provide most
characters found in Southeast European languages."
  :aliases '(:latin-10 :latin10)
  :literal-char-code-limit #xa0)

(define-constant +unicode-a0-180-to-iso-8859-16+
    #(#xa0 nil nil nil nil nil nil #xa7  ; #xa0-#xa7
      nil #xa9 nil #xab nil #xad nil nil ; #xa8-#xaf
      #xb0 #xb1 nil nil nil nil #xb6 #xb7 ; #xb0-#xb7
      nil nil nil #xbb nil nil nil nil    ; #xb8-#xbf
      #xc0 #xc1 #xc2 nil #xc4 nil #xc6 #xc7   ; #xc0-#xc7
      #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #xc8-#xcf
      nil nil #xd2 #xd3 #xd4 nil #xd6 nil     ; #xd0-#xd7
      nil #xd9 #xda #xdb #xdc nil nil #xdf    ; #xd8-#xdf
      #xe0 #xe1 #xe2 nil #xe4 nil #xe6 #xe7   ; #xe0-#xe7
      #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #xe8-#xef
      nil nil #xf2 #xf3 #xf4 nil #xf6 nil     ; #xf0-#xf7
      nil #xf9 #xfa #xfb #xfc nil nil #xff    ; #xf8-#xff
      nil nil #xc3 #xe3 #xa1 #xa2 #xc5 #xe5   ; #x100-#x107
      nil nil nil nil #xb2 #xb9 nil nil       ; #x108-#x10f
      #xd0 #xf0 nil nil nil nil nil nil       ; #x110-#x117
      #xdd #xfd nil nil nil nil nil nil       ; #x118-#x11f
      nil nil nil nil nil nil nil nil         ; #x120-#x127
      nil nil nil nil nil nil nil nil         ; #x128-#x12f
      nil nil nil nil nil nil nil nil         ; #x130-#x137
      nil nil nil nil nil nil nil nil         ; #x138-#x13f
      nil #xa3 #xb3 #xd1 #xf1 nil nil nil     ; #x140-#x147
      nil nil nil nil nil nil nil nil         ; #x148-#x14f
      #xd5 #xf5 #xbc #xbd nil nil nil nil     ; #x150-#x157
      nil nil #xd7 #xf7 nil nil nil nil       ; #x158-#x15f
      #xa6 #xa8 nil nil nil nil nil nil       ; #x160-#x167
      nil nil nil nil nil nil nil nil         ; #x168-#x16f
      #xd8 #xf8 nil nil nil nil nil nil       ; #x170-#x177
      #xbe #xac #xae #xaf #xbf #xb4 #xb8 nil) ; #x178-#x17f
  :test #'equalp)

(define-constant +unicode-218-220-to-iso-8859-16+
    #(#xaa #xba #xde #xfe nil nil nil nil) ; #x218-#x21f
  :test #'equalp)

(define-constant +unicode-2018-2020-to-iso-8859-16+
    #(nil nil nil nil nil #xb5 #xa5 nil) ; #x2018-#x201f
  :test #'equalp)

(define-unibyte-encoder :iso-8859-16 (code)
  (or (cond ((< code #xa0) code)
            ((< code #x180)
             (svref +unicode-a0-180-to-iso-8859-16+
                    (the ub8 (- code #xa0))))
            ((<= #x218 code #x21f)
             (svref +unicode-218-220-to-iso-8859-16+
                    (the ub8 (- code #x218))))
            ((< #x2018 code #x201f)
             (svref +unicode-2018-2020-to-iso-8859-16+
                    (the ub8 (- code #x2018))))
            ;; Euro sign
            ((= code #x20ac) #xa4))
      (handle-error)))

(define-constant +iso-8859-16-to-unicode+
    #(;; #xa0
      #x00a0 #x0104 #x0105 #x0141 #x20ac #x201e #x0160 #x00a7
      #x0161 #x00a9 #x0218 #x00ab #x0179 #x00ad #x017a #x017b
      ;; #xb0
      #x00b0 #x00b1 #x010c #x0142 #x017d #x201d #x00b6 #x00b7
      #x017e #x010d #x0219 #x00bb #x0152 #x0153 #x0178 #x017c
      ;; #xc0
      #x00c0 #x00c1 #x00c2 #x0102 #x00c4 #x0106 #x00c6 #x00c7
      #x00c8 #x00c9 #x00ca #x00cb #x00cc #x00cd #x00ce #x00cf
      ;; #xd0
      #x0110 #x0143 #x00d2 #x00d3 #x00d4 #x0150 #x00d6 #x015a
      #x0170 #x00d9 #x00da #x00db #x00dc #x0118 #x021a #x00df
      ;; #xe0
      #x00e0 #x00e1 #x00e2 #x0103 #x00e4 #x0107 #x00e6 #x00e7
      #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
      ;; #xf0
      #x0111 #x0144 #x00f2 #x00f3 #x00f4 #x0151 #x00f6 #x015b
      #x0171 #x00f9 #x00fa #x00fb #x00fc #x0119 #x021b #x00ff)
  :test #'equalp)

(define-unibyte-decoder :iso-8859-16 (octet)
  (if (< octet #xa0)
      octet
      (svref +iso-8859-16-to-unicode+ (the ub8 (- octet #xa0)))))
