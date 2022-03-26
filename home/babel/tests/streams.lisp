;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; streams.lisp --- Unit and regression tests for Babel streams.
;;;
;;; Copyright (C) 2007, Attila Lendva <attila.lendvai@gmail.com>
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

(in-package #:babel-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :babel-streams))

(deftest in-memory-vector-stream
  (with-output-to-sequence (output)
    ;; TODO use a better test lib and inject asserts for the file position
    ;(print (file-position output))
    (write-sequence "éáőűú" output)
    ;(print (file-position output))
    (write-char #\ű output)
    ;(print (file-position output))
    (write-byte 12 output)
    (write-sequence (string-to-octets "körte") output)
    (write-string "körte" output)
    ;(print (file-position output))
    )
  #(195 169 195 161 197 145 197 177 195 186 197 177 12 107 195 182 114 116 101))
