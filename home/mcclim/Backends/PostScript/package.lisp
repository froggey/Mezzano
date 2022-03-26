;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)

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

(cl:defpackage #:clim-postscript
  (:use #:clim #:clim-extensions #:clim-lisp #:clim-postscript-font)
  (:import-from #:clim-internals
                #:get-environment-variable
                #:map-repeated-sequence
                #:atan*

                #:ellipse-normal-radii*

                #:get-transformation
                #:untransform-angle
                #:with-transformed-position

                #:maxf

                #:port-text-style-mappings
                #:native-transformation
                #:device-transformation
                #:native-region
                #:device-region
                #:port-grafts)
  (:export #:load-afm-file
           #:with-output-to-postscript-stream))
