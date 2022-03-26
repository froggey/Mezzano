;;;;  (c) copyright 2019 Jan Moringen

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:mcclim-raster-image)

(defclass raster-image-destination (file-destination)
  ((%file-format :initarg :file-format
                 :reader file-format)
   (%width :initarg :width
           :reader width
           :initform :compute)
   (%height :initarg :height
            :reader height
            :initform :compute)
   (%border-width :initarg :border-width
                  :reader border-width
                  :initform 10)))

(defmethod shared-initialize :after ((instance raster-image-destination)
                                     (slot-names t)
                                     &key
                                     (file nil file-p)
                                     (file-format nil file-format-p))
  (declare (ignore file-format))
  (when (and file-p (not file-format-p))
    (setf (slot-value instance '%file-format) (extract-format file))))

(defmethod destination-element-type ((destination raster-image-destination))
  '(unsigned-byte 8))

(defmethod invoke-with-standard-output
    (continuation (destination raster-image-destination))
  (call-next-method (lambda ()
                      (with-output-to-raster-image-stream
                          (*standard-output* *standard-output*
                                             (file-format destination)
                                             :width (width destination)
                                             :height (height destination)
                                             :border-width (border-width destination)
                                             :recording-p nil)
                        (funcall continuation)))
                    destination))

(register-output-destination-type
 "Raster Image File" 'raster-image-destination)
