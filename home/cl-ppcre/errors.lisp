;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/errors.lisp,v 1.22 2009/09/17 19:17:31 edi Exp $

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

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

(in-package :cl-ppcre)

(defvar *syntax-error-string* nil
  "The string which caused the syntax error.")

(define-condition ppcre-error (simple-error)
  ()
  (:documentation "All errors signaled by CL-PPCRE are of
this type."))

(define-condition ppcre-syntax-error (ppcre-error)
  ((string :initarg :string
           :reader ppcre-syntax-error-string)
   (pos :initarg :pos
        :reader ppcre-syntax-error-pos))
  (:default-initargs
      :pos nil
      :string *syntax-error-string*)
  (:report (lambda (condition stream)
             (format stream "~?~@[ at position ~A~]~@[ in string ~S~]"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (ppcre-syntax-error-pos condition)
                     (ppcre-syntax-error-string condition))))
  (:documentation "Signaled if CL-PPCRE's parser encounters an error
when trying to parse a regex string or to convert a parse tree into
its internal representation."))

(setf (documentation 'ppcre-syntax-error-string 'function)
      "Returns the string the parser was parsing when the error was
encountered \(or NIL if the error happened while trying to convert a
parse tree).")

(setf (documentation 'ppcre-syntax-error-pos 'function)
      "Returns the position within the string where the error occurred
\(or NIL if the error happened while trying to convert a parse tree")

(define-condition ppcre-invocation-error (ppcre-error)
  ()
  (:documentation "Signaled when CL-PPCRE functions are
invoked with wrong arguments."))

(defmacro signal-syntax-error* (pos format-control &rest format-arguments)
  `(error 'ppcre-syntax-error
          :pos ,pos
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))

(defmacro signal-syntax-error (format-control &rest format-arguments)
  `(signal-syntax-error* nil ,format-control ,@format-arguments))

(defmacro signal-invocation-error (format-control &rest format-arguments)
  `(error 'ppcre-invocation-error
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))
