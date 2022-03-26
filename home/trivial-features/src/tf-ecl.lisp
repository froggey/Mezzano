;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; tf-ecl.lisp --- ECL implementation of trivial-features.
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

(pushnew (let ((ptr (ffi:allocate-foreign-object :unsigned-short)))
           (unwind-protect
                (progn
                  (setf (ffi:deref-pointer ptr :unsigned-short) #xfeff)
                  (ecase (ffi:deref-pointer ptr :unsigned-byte)
                    (#xfe (intern "BIG-ENDIAN" "KEYWORD"))
                    (#xff (intern "LITTLE-ENDIAN" "KEYWORD"))))
             (ffi:free-foreign-object ptr)))
         *features*)

;;;; OS

;;; ECL already pushes :DARWIN, :LINUX, :UNIX (except on Darwin) and :BSD.

#+darwin (pushnew :unix *features*)
#+win32 (pushnew :windows *features*)

;;;; CPU

;;; FIXME: add more
#+powerpc7450 (pushnew :ppc *features*)
#+x86_64 (pushnew :x86-64 *features*)
#+(or i386 i486 i586 i686) (pushnew :x86 *features*)
#+(or armv5l armv6l armv7l) (pushnew :arm *features*)
