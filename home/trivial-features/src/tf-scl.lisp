;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; tf-scl.lisp --- SCL implementation of trivial-features.
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

(pushnew (alien:with-alien ((ptr (array (alien:unsigned 8) 2)))
           (setf (sys:sap-ref-16 (alien:alien-sap ptr) 0) #xfeff)
           (ecase (sys:sap-ref-8 (alien:alien-sap ptr) 0)
             (#xfe (intern (symbol-name '#:big-endian) '#:keyword))
             (#xff (intern (symbol-name '#:little-endian) '#:keyword))))
         *features*)

;;;; OS

;;; SCL already pushes :unix, :bsd, :linux, :hpux, and :solaris

;;;; CPU

;;; SCL already pushes :amd64, :x86, :sparc, :sparc64, :hppa and :hppa64.
;;; For 64 bit CPUs the SCL pushes: :64bit

#+amd64 (pushnew :x86-64 *features*)
