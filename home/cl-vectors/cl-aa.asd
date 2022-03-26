;;;; -*- Lisp -*- mode

;;;; cl-vectors -- Rasterizer and paths manipulation library
;;;; Copyright (C) 2007-2013  Frédéric Jolliton <frederic@jolliton.com>
;;;; This code is licensed under the MIT license.

(defpackage #:cl-aa-system
  (:use #:cl #:asdf))

(in-package #:cl-aa-system)

(defsystem #:cl-aa
  :description "cl-aa: polygon rasterizer"
  ;; :VERSION "$VERSION$"
  :author "Frederic Jolliton <frederic@jolliton.com>"
  :licence "MIT"
  :components ((:file "aa")
               (:file "aa-bin")))
