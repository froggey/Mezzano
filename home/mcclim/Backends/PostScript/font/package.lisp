;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

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


(defpackage #:clim-postscript-font
  (:use #:clim #:clim-extensions #:clim-lisp)
  (:export #:postscript-font-medium
           #:postscript-font-port
           #:device-fonts
           #:postscript-font-name
           #:postscript-device-font-name
           #:postscript-device-font-name-font-file
           #:text-size-in-font
           #:get-font-info
           #:font-info-name
           #:font-name-name
           #:font-name-size
           #:font-name-metrics-key
           #:*iso-latin-1-symbolic-names*)
  (:import-from #:clim-internals
                #:maxf
                #:port-text-style-mappings))

