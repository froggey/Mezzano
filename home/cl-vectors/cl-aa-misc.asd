;;;; -*- Lisp -*- mode

;;;; cl-vectors -- Rasterizer and paths manipulation library
;;;; Copyright (C) 2007-2013  Frédéric Jolliton <frederic@jolliton.com>
;;;; This code is licensed under the MIT license.

(defpackage #:cl-aa-misc-system
  (:use #:cl #:asdf))

(in-package #:cl-aa-misc-system)

(defsystem #:cl-aa-misc
  :description "cl-aa-misc: some tools related to cl-aa"
  ;; :version "$VERSION$"
  :author "Frederic Jolliton <frederic@jolliton.com>"
  :licence "MIT"
  :components ((:file "aa-misc")))
