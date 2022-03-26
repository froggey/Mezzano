;;;  (c) copyright 2019 Jan Moringen

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

(cl:in-package #:mcclim-raster-image.test)

(in-suite :mcclim-raster-image)

(test smoke
  "Smoke test for the raster image backend."

  (loop for i from 1
        for filename = (format nil "raster-image-test-~D.png" i)
        for page in clim-test-util:*all-test-pages*
        do (finishes
             (mcclim-raster-image:with-output-to-raster-image-file
                 (stream filename)
               (funcall page stream)))))
