;;; -*- Mode: Lisp; Package: CLIM-CLX -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)

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

(in-package :clim-clx)

;;; CLX-GRAFT class

(defclass clx-graft (graft) ())

(defmethod graft-width ((graft clx-graft) &key (units :device))
  (let ((screen (clx-port-screen (port graft))))
    (ecase units
      (:device (xlib:screen-width screen))
      (:inches (/ (xlib:screen-width-in-millimeters screen) 25.4f0))
      (:millimeters (xlib:screen-width-in-millimeters screen))
      (:screen-sized 1))))

(defmethod graft-height ((graft clx-graft) &key (units :device))
  (let ((screen (clx-port-screen (port graft))))
    (ecase units
      (:device (xlib:screen-height screen))
      (:inches (/ (xlib:screen-height-in-millimeters screen) 25.4f0))
      (:millimeters (xlib:screen-height-in-millimeters screen))
      (:screen-sized 1))))
