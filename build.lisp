;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               build.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This scripts compiles and generate the mezzano image.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;
;;;;    MIT
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
;;;;
;;;;    Permission is hereby granted, free of charge, to any person
;;;;    obtaining a copy of this software and associated documentation
;;;;    files (the "Software"), to deal in the Software without
;;;;    restriction, including without limitation the rights to use,
;;;;    copy, modify, merge, publish, distribute, sublicense, and/or
;;;;    sell copies of the Software, and to permit persons to whom the
;;;;    Software is furnished to do so, subject to the following
;;;;    conditions:
;;;;
;;;;    The above copyright notice and this permission notice shall be
;;;;    included in all copies or substantial portions of the
;;;;    Software.
;;;;
;;;;    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
;;;;    KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
;;;;    WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;;;;    PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;;;    COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;;    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;;;;    OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;;    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;;**************************************************************************
(in-package :cl-user)
(load "configuration.lisp")


;; Load the cross build environment.
(ql:quickload :lispos)

;; Initialize the empty cross environment.
(with-compilation-unit ()
  (sys.c::set-up-cross-compiler)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*supervisor-source-files*)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*source-files*)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*warm-source-files*))

;; Build a cold image.
(cold-generator::make-image *image-name* :header-path "tools/disk_header")
;; This will produce a raw disk image called mezzano.image in the current directory.
