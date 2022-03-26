;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; benchmarks.lisp --- Benchmarks, Babel vs. other implementations.
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

(defpackage #:babel-benchmarks
  (:use #:cl #:babel))
(in-package #:babel-benchmarks)

(defun benchmark (enc file-name file-type &optional (n 100))
  (let* ((octets (read-test-file file-name file-type))
         (string (octets-to-string octets :encoding enc)))
    (write-line ";; testing SB-EXT:STRING-TO-OCTETS")
    (time (loop repeat n do
                (sb-ext:string-to-octets string :external-format enc)))
    (write-line ";; testing BABEL:STRING-TO-OCTETS")
    (time (loop repeat n do (string-to-octets string :encoding enc)))
    (write-line ";; testing SB-EXT:OCTETS-TO-STRING")
    (time (loop repeat n do
                (sb-ext:octets-to-string octets :external-format enc)))
    (write-line ";; testing BABEL:OCTETS-TO-STRING")
    (time (loop repeat n do (octets-to-string octets :encoding enc)))))
