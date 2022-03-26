;;;; -*- Lisp -*- mode

;;;; cl-vectors -- Rasterizer and paths manipulation library
;;;; Copyright (C) 2007-2013  Frédéric Jolliton <frederic@jolliton.com>
;;;; This code is licensed under the MIT license.

(defpackage #:cl-paths-system
  (:use #:cl #:asdf))

(in-package #:cl-paths-system)

(defsystem #:cl-paths
  :description "cl-paths: vectorial paths manipulation"
  ;; :version "$VERSION$"
  :author "Frederic Jolliton <frederic@jolliton.com>"
  :licence "MIT"
  :components ((:file "paths-package")
               (:file "paths" :depends-on ("paths-package"))
               (:file "paths-annotation" :depends-on ("paths-package" "paths"))))
