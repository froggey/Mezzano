;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp1252.lisp --- Implementation of the CP1252 character encoding.
;;;
;;; Copyright (C) 2011, Nicolas Martyanoff
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

(in-package #:babel-encodings)

(define-character-encoding :cp1252
    "A 8-bit, fixed-width character encoding used by Windows for Western
    European languages."
  :aliases '(:windows-1252)
  :literal-char-code-limit 256)

(define-constant +cp1252-to-unicode+
    #(#x20ac    nil #x201a #x0192 #x201e #x2026 #x2020 #x2021
      #x02c6 #x2030 #x0160 #x2039 #x0152    nil #x017d    nil
         nil #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
      #x02dc #x2122 #x0161 #x203a #x0153    nil #x017e #x0178)
  :test #'equalp)

(define-unibyte-decoder :cp1252 (octet)
  (if (and (>= octet #x80) (<= octet #x9f))
      (svref +cp1252-to-unicode+
             (the ub8 (- octet #x80)))
      octet))

(define-constant +unicode-0152-017e-cp1252+
    #(#x8c #x9c #x00 #x00 #x00 #x00 #x00 #x00
      #x00 #x00 #x00 #x00 #x00 #x00 #x8a #x9a
      #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
      #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
      #x00 #x00 #x00 #x00 #x00 #x00 #x9f #x00
      #x00 #x00 #x00 #x8e #x9e)
  :test #'equalp)

(define-constant +unicode-2013-203a-cp1252+
    #(#x96 #x97 #x00 #x00 #x00 #x91 #x92 #x82
      #x00 #x93 #x94 #x84 #x00 #x86 #x87 #x95
      #x00 #x00 #x00 #x85 #x00 #x00 #x00 #x00
      #x00 #x00 #x00 #x00 #x00 #x89 #x00 #x00
      #x00 #x00 #x00 #x00 #x00 #x00 #x8b #x9b)
  :test #'equalp)

(define-unibyte-encoder :cp1252 (code)
  (cond
    ((or (< code #x80)
         (and (> code #xa0) (<= code #xff)))
     code)
    ((and (>= code #x0152) (<= code #x017e))
     (svref +unicode-0152-017e-cp1252+
            (the ub8 (- code #x0152))))
    ((= code #x0192) #x83)
    ((= code #x02c6) #x88)
    ((= code #x02dc) #x89)
    ((and (>= code #x2013) (<= code #x203a))
     (svref +unicode-2013-203a-cp1252+
            (the ub8 (- code #x2013))))
    ((= code #x20ac) #x80)
    ((= code #x2122) #x99)
    (t (handle-error))))
