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
;;; Conditions
;;;
;;; $Id: conditions.lisp,v 1.3 2006/02/18 23:13:43 xach Exp $

(in-package #:zpb-ttf)

(define-condition regrettable-value ()
  ((actual-value
    :initarg :actual-value
    :accessor actual-value)
   (expected-values
    :initarg :expected-values
    :accessor expected-values)
   (description
    :initarg :description
    :initform nil
    :accessor description)
   (location
    :initarg :location
    :initform nil
    :accessor location))
  (:report
   (lambda (c s)
     (format s "~:[Regrettable~;~:*~A~] value~:[~;~:* in ~A~]: ~
                ~A (expected ~{~A~^ or ~})"
             (description c)
             (location c)
             (actual-value c)
             (expected-values c)))))

(define-condition regrettable-hex-value (regrettable-value)
  ((size
    :initarg :size
    :initform 8
    :accessor size)
   (actual-value
    :reader %actual-value)
   (expected-values
    :reader %expected-values)))
  
(defmethod actual-value ((c regrettable-hex-value))
  (format nil "#x~v,'0X" (size c) (%actual-value c)))

(defmethod expected-values ((c regrettable-hex-value))
  (mapcar (lambda (v)
            (format nil "#x~v,'0X" (size c) v))
          (%expected-values c)))

(define-condition bad-magic (regrettable-hex-value)
  ((description :initform "Bad magic")))

(define-condition unsupported-version (regrettable-hex-value)
  ((description :initform "Unsupported version")))

(define-condition unsupported-format (regrettable-hex-value)
  ((description :initform "Unsupported format")))

(define-condition unsupported-value (regrettable-value)
  ((description :initform "Unsupported")))

(defun check-version (location actual &rest expected)
  (or (member actual expected :test #'=)
      (error 'unsupported-version
             :location location
             :actual-value actual
             :expected-values expected)))
