;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; tf-sbcl.lisp --- SBCL trivial-features implementation.
;;;
;;; Copyright (C) 2007-2009, Luis Oliveira  <loliveira@common-lisp.net>
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

(pushnew (sb-alien:with-alien ((ptr (array (sb-alien:unsigned 8) 2)))
           (setf (sb-sys:sap-ref-16 (sb-alien:alien-sap ptr) 0) #xfeff)
           (ecase (sb-sys:sap-ref-8 (sb-alien:alien-sap ptr) 0)
             (#xfe (intern "BIG-ENDIAN" :keyword))
             (#xff (intern "LITTLE-ENDIAN" :keyword))))
         *features*)

;;;; OS

;;; SBCL already pushes :DARWIN, :LINUX, :BSD and :UNIX.

#+win32
(progn
  ;; note: as of 2008 or so, SBCL doesn't push :UNIX and :WIN32
  ;; simultaneously anymore.
  (setq *features* (remove :unix *features*))
  (pushnew :windows *features*))

;;;; CPU

;;; SBCL already pushes: :X86, :X86-64, and :PPC
