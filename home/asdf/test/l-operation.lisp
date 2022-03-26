(cl:defpackage :l-operation
  (:use :asdf :uiop :cl) ;; asdf/package-inferred-system dependencies
  (:export #:op #:*x*))

(cl:in-package :l-operation)

(defparameter *x* 0)

(defclass op (load-op) ())

(defmethod perform :after ((o op) (c t))
  (incf *x*))
