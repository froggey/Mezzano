;;;; -*- Lisp -*- mode

;;;; cl-vectors -- Rasterizer and paths manipulation library
;;;; Copyright (C) 2007-2013  Frédéric Jolliton <frederic@jolliton.com>
;;;; This code is licensed under the MIT license.

(defpackage #:cl-paths-ttf-system
  (:use #:cl #:asdf))

(in-package #:cl-paths-ttf-system)

(defsystem #:cl-paths-ttf
  :description "cl-paths-ttf: vectorial paths manipulation"
  ;; :version "$VERSION$"
  :author "Frederic Jolliton <frederic@jolliton.com>"
  :licence "MIT"
  :depends-on ("cl-paths" "zpb-ttf")
  :components ((:file "paths-ttf")))
