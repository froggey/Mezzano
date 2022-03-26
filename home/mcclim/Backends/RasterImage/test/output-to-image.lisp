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

(test with-output-to-raster-image.smoke
  "Smoke test for the WITH-OUTPUT-TO-RASTER-IMAGE macro."

  (flet ((do-it (width height border-width)
           (let* ((name (format nil "with-output-to-raster-image-~A-~A-~A"
                                width height border-width))
                  (pathname (make-pathname :name name :type "png")))
             (mcclim-raster-image:with-output-to-raster-image-file
                 (stream pathname :width width
                                  :height height
                                  :border-width border-width)
               (let ((*standard-output* stream))
                 (room))))))
    (map-product (lambda (&rest args)
                   (finishes (apply #'do-it args)))
                 '(60 :compute) '(60 :compute) '(0 20))))
