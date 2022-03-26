;;;
;;; Copyright (c) 2008 Zachary Beane, All Rights Reserved
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
;;; conditions.lisp

(in-package #:zpng)

(define-condition zpng-error (error) ())

(define-condition invalid-size (zpng-error)
  ((width
    :initarg :width
    :reader invalid-size-width)
   (height
    :initarg :height
    :reader invalid-size-height))
  (:report (lambda (condition stream)
             (format stream "Invalid PNG size ~Ax~A; ~
                             both width and height must be positive"
                     (invalid-size-width condition)
                     (invalid-size-height condition)))))

(define-condition invalid-row-length (zpng-error)
  ((expected-length
    :initarg :expected-length
    :accessor invalid-row-length-expected-length)
   (actual-length
    :initarg :actual-length
    :accessor invalid-row-length-actual-length))
  (:report (lambda (condition stream)
             (format stream "Invalid row length ~A (expected ~A)"
                     (invalid-row-length-actual-length condition)
                     (invalid-row-length-expected-length condition)))))

(define-condition insufficient-rows (zpng-error)
  ((written
    :initarg :written
    :accessor insufficient-rows-written)
   (needed
    :initarg :needed
    :accessor insufficient-rows-needed))
  (:report (lambda (condition stream)
             (format stream "Insufficient rows written; need ~A, but only ~A ~
                             written"
                     (insufficient-rows-needed condition)
                     (insufficient-rows-written condition)))))

(define-condition incomplete-row (zpng-error)
  ((written
    :initarg :written
    :accessor incomplete-row-written)
   (needed
    :initarg :needed
    :accessor incomplete-row-needed))
  (:report (lambda (condition stream)
             (format stream "Incomplete row started; need ~A, but only ~A ~
                             written"
                     (incomplete-row-needed condition)
                     (incomplete-row-written condition)))))

(define-condition too-many-rows (zpng-error)
  ((count
    :initarg :count
    :accessor too-many-rows-count))
  (:report (lambda (condition stream)
             (format stream "Too many rows written for PNG; maximum row count ~
                             is ~A"
                     (too-many-rows-count condition)))))

(define-condition color-type-mismatch (zpng-error)
  ((given
    :initarg :given
    :accessor color-type-mismatch-given)
   (expected
    :initarg :expected
    :accessor color-type-mismatch-expected))
  (:report (lambda (condition stream)
	     (format stream "Wrong number of samples for PNG pixel; need ~A, ~
                             but only ~A written"
		     (color-type-mismatch-expected condition)
		     (color-type-mismatch-given condition)))))
