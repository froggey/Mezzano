;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; tf-clisp.lisp --- CLISP trivial-features implementation.
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

(in-package :cl-user)

;;;; Endianness

(pushnew (intern (symbol-name (if sys::*big-endian*
                                  '#:big-endian
                                  '#:little-endian))
                 '#:keyword)
         *features*)

;;;; OS

;;; CLISP already exports :UNIX.

#+win32 (pushnew :windows *features*)

#-win32
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (with-standard-io-syntax
             (read-from-string
              (format nil ":~(~A~)" (posix:uname-sysname (posix:uname)))))
           *features*))

#+(or darwin freebsd netbsd openbsd)
(pushnew :bsd *features*)

;;;; CPU

;;; FIXME: not complete
(let ((cpu (cond
             ((or (member :pc386 *features*)
                  (member (machine-type) '("x86" "x86_64")
                          :test #'string-equal))
              (if (member :word-size=64 *features*)
                  '#:x86-64
                  '#:x86))
             ((string= (machine-type) "POWER MACINTOSH")
              '#:ppc))))
  (when cpu
    (pushnew (intern (symbol-name cpu) '#:keyword)
             *features*)))
