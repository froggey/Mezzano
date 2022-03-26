;;; Copyright (c) 2006 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
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
;;;
;;;; $Id: conditions.lisp,v 1.2 2007/01/04 01:30:40 xach Exp $

(in-package #:skippy)

(define-condition skippy-warning (simple-warning) ())

(define-condition skippy-error (error) ())

(defun skippy-warn (control &rest args)
  (warn 'skippy-warning
        :format-control control
        :format-arguments args))

(define-condition lzw-error (skippy-error)
  ((description
    :initarg :description
    :reader lzw-error-description))
  (:report
   (lambda (condition stream)
     (write-string (lzw-error-description condition) stream))))

(define-condition unexpected-value (skippy-error)
  ((description
    :initarg :description
    :reader unexpected-value-description
    :initform nil)
   (actual-value
    :initarg :actual-value
    :reader unexpected-value-actual-value)
   (expected-value
    :initarg :expected-value
    :initform nil
    :reader unexpected-value-expected-value)
   (source
    :initarg :source
    :initform nil
    :reader unexpected-value-source)
   (source-position
    :initarg :source-position
    :initform nil
    :reader unexpected-value-source-position))
  (:report
   (lambda (condition stream)
     (format stream "Unexpected~@[ ~A~] value ~A~@[ at position ~D~]~
                     ~@[ in ~A~]~@[ (expected ~A) ~]"
             (unexpected-value-description condition)
             (unexpected-value-actual-value condition)
             (unexpected-value-source-position condition)
             (unexpected-value-source condition)
             (unexpected-value-expected-value condition)))))

(define-condition missing-color-table (skippy-error)
  ((image
    :initarg :image
    :reader missing-color-table-image))
  (:report
   (lambda (condition stream)
     (format stream "No local or global color table available for ~A"
             (missing-color-table-image condition)))))

(define-condition signature-error (skippy-error)
  ((source
    :initarg :source
    :reader signature-error-source)
   (position
    :initarg :position
    :initform nil
    :reader signature-error-position)))

(define-condition short-signature (signature-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Missing signature~@[ at position ~D~] in ~A"
             (signature-error-position condition)
             (signature-error-source condition)))))

(define-condition signature-mismatch (signature-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Signature mismatch~@[ at position ~D~] in ~A"
             (signature-error-position condition)
             (signature-error-source condition)))))

(define-condition color-table-full (skippy-error)
  ((color-table
    :initarg :color-table
    :reader color-table-full-color-table))
  (:report
   (lambda (condition stream)
     (format stream "Color table ~A is full (256 entries)"
             (color-table-full-color-table condition)))))

(define-condition invalid-image-dimensions (skippy-error)
  ((width
    :initarg :width
    :reader invalid-image-dimension-width)
   (height
    :initarg :height
    :reader invalid-image-dimension-height))
  (:report
   (lambda (condition stream)
     (format stream "Invalid image dimensions ~Ax~A - each dimensions must ~
                     be (< 0 dimension 65536)"
             (invalid-image-dimension-width condition)
             (invalid-image-dimension-height condition)))))

