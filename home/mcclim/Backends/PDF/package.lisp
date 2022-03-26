;;; -*- Mode: Lisp; Package: CLIM-PDF -*-

;;;  (c) copyright 2017 by
;;;           Cyrus Harmon (cyrus@bobobeach.com)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :cl-user)

(defpackage #:clim-pdf
  (:use #:clim #:clim-extensions #:clim-lisp #:clim-backend)
  (:export #:with-output-to-pdf-stream)
  (:import-from #:clim-internals
                #:map-repeated-sequence
                #:with-transformed-position
                #:get-environment-variable
                #:port-text-style-mappings
                ;; ellipses
                #:reparameterize-ellipse
                #:ellipse-cubic-bezier-points
                #:transform-angle
                #:native-transformation
                #:device-transformation
                #:native-region
                #:device-region
                #:port-grafts))
