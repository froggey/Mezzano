;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/conditions.lisp,v 1.9 2008/05/25 22:23:58 edi Exp $

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

(in-package :flexi-streams)

(define-condition flexi-stream-error (stream-error)
  ()
  (:documentation "Superclass for all errors related to flexi
streams."))

(define-condition flexi-stream-simple-error (flexi-stream-error simple-condition)
  ()
  (:documentation "Like FLEXI-STREAM-ERROR but with formatting
capabilities."))

(define-condition flexi-stream-element-type-error (flexi-stream-error)
  ((element-type :initarg :element-type
                 :reader flexi-stream-element-type-error-element-type))
  (:report (lambda (condition stream)
             (format stream "Element type ~S not allowed."
                     (flexi-stream-element-type-error-element-type condition))))
  (:documentation "Errors of this type are signalled if the flexi
stream has a wrong element type."))

(define-condition flexi-stream-out-of-sync-error (flexi-stream-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Stream out of sync from previous
lookahead, couldn't rewind.")))
  (:documentation "This can happen if you're trying to write to an IO
stream which had prior to that `looked ahead' while reading and now
can't `rewind' to the octet where you /should/ be."))

(define-condition in-memory-stream-error (stream-error)
  ()
  (:documentation "Superclass for all errors related to
IN-MEMORY streams."))

(define-condition in-memory-stream-simple-error (in-memory-stream-error simple-condition)
  ()
  (:documentation "Like IN-MEMORY-STREAM-ERROR but with formatting
capabilities."))

(define-condition in-memory-stream-closed-error (in-memory-stream-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~S is closed."
                     (stream-error-stream condition))))
  (:documentation "An error that is signalled when someone is trying
to read from or write to a closed IN-MEMORY stream."))

(define-condition in-memory-stream-position-spec-error (in-memory-stream-simple-error)
  ((position-spec :initarg :position-spec
                  :reader in-memory-stream-position-spec-error-position-spec))
  (:documentation "Errors of this type are signalled if an erroneous
position spec is used in conjunction with FILE-POSITION."))

(define-condition external-format-condition (simple-condition)
  ((external-format :initarg :external-format
                    :initform nil
                    :reader external-format-condition-external-format))
  (:documentation "Superclass for all conditions related to external
formats."))

(define-condition external-format-error (external-format-condition error)
  ()
  (:documentation "Superclass for all errors related to external
formats."))
  
(define-condition external-format-encoding-error (external-format-error)
  ()
  (:documentation "Errors of this type are signalled if there is an
encoding problem."))

(defun signal-encoding-error (external-format format-control &rest format-args)
  "Convenience function similar to ERROR to signal conditions of type
EXTERNAL-FORMAT-ENCODING-ERROR."
  (error 'external-format-encoding-error
         :format-control format-control
         :format-arguments format-args
         :external-format external-format))
