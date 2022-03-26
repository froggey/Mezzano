;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition
;;;

(in-package :common-lisp-user)

(defpackage :static-vectors
  (:use #:common-lisp :alexandria #-mezzano :cffi)
  (:shadow #:constantp)
  (:export
   ;; Constructors and destructors
   #:make-static-vector
   #:free-static-vector
   #:with-static-vector
   #:with-static-vectors

   ;; Accessors
   #-mezzano #:static-vector-pointer

   ;; CFFI wrapper type
   #:static-vector

   ;; Foreign memory operations
   #:replace-foreign-memory
   #:fill-foreign-memory
   ))
