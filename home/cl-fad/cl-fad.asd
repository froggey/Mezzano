;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-fad/cl-fad.asd,v 1.21 2009/09/30 14:23:09 edi Exp $

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#+:allegro (cl:require :osi)

(asdf:defsystem #:cl-fad
  :version "0.7.5"
  :description "Portable pathname library"
  :serial t
  :license "BSD-2-Clause"
  :components ((:file "packages")
               #+:cormanlisp (:file "corman")
               #+:openmcl (:file "openmcl")
               (:file "fad")
               (:file "path" :depends-on ("fad"))
               (:file "temporary-files" :depends-on ("fad")))
  :depends-on (#+sbcl :sb-posix :bordeaux-threads :alexandria))

(asdf:defsystem #:cl-fad-test
  :serial t
  :components ((:file "packages.test")
               (:file "fad.test" :depends-on ("packages.test"))
               (:file "temporary-files.test" :depends-on ("packages.test")))
  :depends-on (:cl-fad :unit-test :cl-ppcre))
