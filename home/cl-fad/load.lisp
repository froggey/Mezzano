;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-fad/load.lisp,v 1.9 2009/09/30 14:23:10 edi Exp $

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

(in-package :cl-user)

(defparameter *cl-fad-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

#+:allegro (require :osi)
#+:sbcl (require :sb-executable)
#+:sbcl (require :sb-posix)

(let ((cl-fad-base-directory
        (make-pathname :name nil :type nil :version nil
                       :defaults (parse-namestring *load-truename*))))
  (let (must-compile)
    #+:cormanlisp (declare (ignore must-compile))
    (dolist (file '("packages"
		    #+:cormanlisp "corman"
		    #+:openmcl "openmcl"
		    "fad"))
      (let ((pathname (make-pathname :name file :type "lisp" :version nil
                                     :defaults cl-fad-base-directory)))
        ;; don't use COMPILE-FILE in Corman Lisp, it's broken - LOAD
        ;; will yield compiled functions anyway
        #-:cormanlisp
        (let ((compiled-pathname (compile-file-pathname pathname)))
          (unless (and (not must-compile)
                       (probe-file compiled-pathname)
                       (< (file-write-date pathname)
                          (file-write-date compiled-pathname)))
            (setq must-compile t)
            (compile-file pathname))
          (setq pathname compiled-pathname))
        (load pathname)))))
