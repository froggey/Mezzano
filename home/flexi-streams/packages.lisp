;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/packages.lisp,v 1.39 2008/05/30 07:50:31 edi Exp $

;;; Copyright (c) 2005-2008, Dr. Edmund Weitz.  All rights reserved.

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

(unless (find-symbol (symbol-name :stream-file-position) :trivial-gray-streams)
  (error "You need a newer version of TRIVIAL-GRAY-STREAMS."))

(defpackage :flexi-streams
  (:use :cl :trivial-gray-streams)
  (:nicknames :flex)  
  (:shadow #+:lispworks :with-accessors
           :defconstant)
  (:export :*default-eol-style*
           :*default-little-endian*
           :*substitution-char*
           :accept-overlong-sequence
           :char-length
           :external-format-condition
           :external-format-condition-external-format
           :external-format-eol-style
           :external-format-error
           :external-format-encoding-error
           :external-format-equal
           :external-format-id
           :external-format-little-endian
           :external-format-name
           :flexi-input-stream
           :flexi-output-stream
           :flexi-io-stream
           :flexi-stream
           :flexi-stream-bound
           :flexi-stream-column
           :flexi-stream-external-format
           :flexi-stream-element-type
           :flexi-stream-element-type-error
           :flexi-stream-element-type-error-element-type
           :flexi-stream-error
           :flexi-stream-out-of-sync-error
           :flexi-stream-position
           :flexi-stream-stream
           :get-output-stream-sequence
           :in-memory-stream
           :in-memory-stream-closed-error
           :in-memory-stream-error
           :in-memory-stream-position-spec-error
           :in-memory-stream-position-spec-error-position-spec
           :in-memory-input-stream
           :in-memory-output-stream
           :list-stream
           :make-external-format
           :make-in-memory-input-stream
           :make-in-memory-output-stream
           :make-flexi-stream
           :octet
           :octet-length
           :octets-to-string
           :output-stream-sequence-length
           :peek-byte
           :string-to-octets
           :unread-byte
           :vector-stream
           :with-input-from-sequence
           :with-output-to-sequence))
