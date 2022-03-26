;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/cl-ppcre.asd,v 1.49 2009/10/28 07:36:15 edi Exp $

;;; This ASDF system definition was kindly provided by Marco Baringer.

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :cl-user)

(defpackage :cl-ppcre-asd
  (:use :cl :asdf))

(in-package :cl-ppcre-asd)

(defsystem :cl-ppcre
  :version "2.1.1"
  :description "Perl-compatible regular expression library"
  :author "Dr. Edi Weitz"
  :license "BSD"
  :serial t
  :components ((:file "packages")
               (:file "specials")
               (:file "util")
               (:file "errors")
               (:file "charset")
               (:file "charmap")
               (:file "chartest")
               #-:use-acl-regexp2-engine
               (:file "lexer")
               #-:use-acl-regexp2-engine
               (:file "parser")
               #-:use-acl-regexp2-engine
               (:file "regex-class")
               #-:use-acl-regexp2-engine
               (:file "regex-class-util")
               #-:use-acl-regexp2-engine
               (:file "convert")
               #-:use-acl-regexp2-engine
               (:file "optimize")
               #-:use-acl-regexp2-engine
               (:file "closures")
               #-:use-acl-regexp2-engine
               (:file "repetition-closures")
               #-:use-acl-regexp2-engine
               (:file "scanner")
               (:file "api")))

(defsystem :cl-ppcre-test
  :description "Perl-compatible regular expression library tests"
  :author "Dr. Edi Weitz"
  :license "BSD"
  :depends-on (:cl-ppcre :flexi-streams)
  :components ((:module "test"
                        :serial t
                        :components ((:file "packages")
                                     (:file "tests")
                                     (:file "perl-tests")))))

(defmethod perform ((o test-op) (c (eql (find-system :cl-ppcre))))
  (operate 'load-op :cl-ppcre-test)
  (funcall (intern (symbol-name :run-all-tests) (find-package :cl-ppcre-test))))
