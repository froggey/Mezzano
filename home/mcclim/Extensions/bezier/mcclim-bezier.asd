;;;
;;; Copyright (c) 2017, Cyrus Harmon (ch-lisp@bobobeach.com)
;;;
;;; This is the ASDF defsystem file for Robert Strandh's bezier curve
;;; work in mcclim.
;;;
;;; In May 2017, the relevant code was moved into the Extensions
;;; directory.
;;;
;;; See file 'LICENSE' for the copyright details
;;;
(in-package #:asdf-user)

(defsystem #:mcclim-bezier
  :description "Support for various bezier curves in McCLIM."
  :depends-on (#:mcclim-bezier/core
               #-mezzano #:mcclim/bezier-clx))

(defsystem #:mcclim-bezier/core
  :description "core bezier routines"
  :depends-on (#:clim #:mcclim-null #:mcclim-render #:clim-postscript #:clim-pdf)
  :components ((:file "package")
               (:file "bezier" :depends-on ("package"))))

(defsystem #:mcclim-bezier/clx
  :description "CLX bezier drawing routines"
  :depends-on (#:clim #:mcclim-clx #:mcclim-bezier/core)
  :components ((:file "bezier-clx")))
