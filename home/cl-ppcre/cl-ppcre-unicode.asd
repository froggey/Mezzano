;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/cl-ppcre-unicode.asd,v 1.15 2009/09/17 19:17:30 edi Exp $

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

(defpackage :cl-ppcre-unicode-asd
  (:use :cl :asdf))

(in-package :cl-ppcre-unicode-asd)

(defsystem :cl-ppcre-unicode
  :description "Perl-compatible regular expression library (Unicode)"
  :author "Dr. Edi Weitz"
  :license "BSD"
  :components ((:module "cl-ppcre-unicode"
                        :serial t
                        :components ((:file "packages")
                                     (:file "resolver"))))
  :depends-on (:cl-ppcre :cl-unicode))

(defsystem :cl-ppcre-unicode-test
  :description "Perl-compatible regular expression library tests (Unicode)"
  :author "Dr. Edi Weitz"
  :license "BSD"
  :depends-on (:cl-ppcre-unicode :cl-ppcre-test)
  :components ((:module "test"
                        :serial t
                        :components ((:file "unicode-tests")))))

(defmethod perform ((o test-op) (c (eql (find-system :cl-ppcre-unicode))))
  ;; we must load CL-PPCRE explicitly so that the CL-PPCRE-TEST system
  ;; will be found
  (operate 'load-op :cl-ppcre)
  (operate 'load-op :cl-ppcre-unicode-test)
  (funcall (intern (symbol-name :run-all-tests) (find-package :cl-ppcre-test))
           :more-tests (intern (symbol-name :unicode-test) (find-package :cl-ppcre-test))))
