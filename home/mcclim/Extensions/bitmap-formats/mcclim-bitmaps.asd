;;;
;;; Copyright (c) 2008, Troels Henriksen (athas@sigkill.dk)
;;; Copyright (c) 2009, Samium Gromoff (_deepfire@feelingofgreen.ru)
;;; Copyright (c) 2009, Cyrus Harmon (ch-lisp@bobobeach.com)
;;; Copyright (c) 2016, Daniel Kochmański (daniel@turtleware.eu)
;;;
;;; See file 'LICENSE' for the copyright details
;;;

(asdf:defsystem #:mcclim-bitmaps
  :description "Support for various image formats in McCLIM."
  :long-description "Support for various image formats in McCLIM
bitmap reading functions.

Currently supported formats are the formats covered by opticl
library and XPM format."
  :depends-on (#:clim-basic #:opticl)
  :components ((:file "xpm")
               (:file "bitmaps" :depends-on ("xpm"))))
