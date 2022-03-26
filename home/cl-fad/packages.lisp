;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-fad/packages.lisp,v 1.12 2009/09/30 14:23:10 edi Exp $

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

(in-package #:cl-user)

(defpackage :cl-fad
  (:nicknames :fad)
  (:use :cl)
  #+:allegro
  (:shadow :copy-file
           :delete-directory-and-files)
  #+:abcl
  (:shadow :list-directory)
  (:export :copy-file
           :copy-stream
           :delete-directory-and-files
           :directory-exists-p
           :directory-pathname-p
           :file-exists-p
           :list-directory
           :pathname-as-directory
           :pathname-as-file
           :pathname-directory-pathname
           :pathname-equal
           :pathname-parent-directory
           :pathname-absolute-p
           :pathname-relative-p
           :pathname-root-p
           
           :canonical-pathname
           :merge-pathnames-as-directory
           :merge-pathnames-as-file           
           
           :walk-directory

           :open-temporary
           :with-output-to-temporary-file
           :with-open-temporary-file
           :*default-template*
           :invalid-temporary-pathname-template
           :cannot-create-temporary-file
           #+win32 #:missing-temp-environment-variable))

(defpackage :path
  (:use)
  (:documentation "Rexporting certain functions from the cl-fad package with shorter names.

This package provides no functionality, it serves only to make file
system intensive code easier to read (for unix people at least).")
  (:export #:dirname
           #:basename
           #:-e
           #:-d
           #:catfile
           #:catdir
           #:rm-r
           #:=

           #:absolute-p
           #:relative-p
           #:root-p))
