(defpackage :l-file
  (:use :asdf :uiop :cl) ;; asdf/package-inferred-system dependencies
  (:export #:cl-source-file.l))

(in-package :l-file)

(defclass cl-source-file.l (cl-source-file)
  ((type :initform "l")))

(defclass asdf::cl-source-file.l (cl-source-file.cl) ())
