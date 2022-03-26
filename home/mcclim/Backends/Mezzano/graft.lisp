;;;  (c) copyright 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

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

(in-package #:clim-mezzano)

(defconstant +dots-per-inch+ 96)

(defclass mezzano-graft (graft)
  ((%width :initarg :width
           :type (real (0))
           :accessor %graft-width)
   (%height :initarg :height
            :type (real (0))
            :accessor %graft-height)))

(defmethod sheet-direct-mirror ((sheet mezzano-graft))
  (port-lookup-mirror (port sheet) sheet))

(defmethod (setf sheet-direct-mirror) (mirror (sheet mezzano-graft))
  (port-register-mirror (port sheet) sheet mirror))

(defmethod initialize-instance :after ((graft mezzano-graft) &rest args)
  (declare (ignore args))
  (let ((framebuffer (mos:current-framebuffer)))
    (setf (%graft-width graft) (mos:framebuffer-width framebuffer)
          (%graft-height graft) (mos:framebuffer-height framebuffer))))

(flet ((convert (value units)
         (ecase units
           (:device value)
           (:inches (/ value +dots-per-inch+))
           (:millimeters (/ value +dots-per-inch+ 2.54))
           (:screen-sized 1))))

  (defmethod graft-width ((graft mezzano-graft) &key (units :device))
    (convert (%graft-width graft) units))

  (defmethod graft-height ((graft mezzano-graft) &key (units :device))
    (convert (%graft-height graft) units)))
