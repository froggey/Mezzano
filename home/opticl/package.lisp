;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(cl:defpackage :opticl
  (:use #:cl #:opticl-core)
  (:export

   #:image
   #:pixel
   #:pixel*

   #:do-pixels
   #:set-pixels
   
   #:do-region-pixels
   #:set-region-pixels

   #:gray-image
   #:1-bit-gray-image #:make-1-bit-gray-image
   #:2-bit-gray-image #:make-2-bit-gray-image
   #:4-bit-gray-image #:make-4-bit-gray-image
   #:8-bit-gray-image #:make-8-bit-gray-image
   #:16-bit-gray-image #:make-16-bit-gray-image
   #:32-bit-gray-image #:make-32-bit-gray-image
   #:fixnum-gray-image #:make-fixnum-gray-image
   #:single-float-gray-image #:make-single-float-gray-image
   #:double-float-gray-image #:make-double-float-gray-image

   #:gray-alpha-image
   #:1-bit-gray-alpha-image #:make-1-bit-gray-alpha-image
   #:2-bit-gray-alpha-image #:make-2-bit-gray-alpha-image
   #:4-bit-gray-alpha-image #:make-4-bit-gray-alpha-image
   #:8-bit-gray-alpha-image #:make-8-bit-gray-alpha-image
   #:16-bit-gray-alpha-image #:make-16-bit-gray-alpha-image
   #:32-bit-gray-alpha-image #:make-32-bit-gray-image

   #:rgb-image
   #:4-bit-rgb-image #:make-4-bit-rgb-image
   #:8-bit-rgb-image #:make-8-bit-rgb-image
   #:16-bit-rgb-image #:make-16-bit-rgb-image
   #:32-bit-rgb-image #:make-32-bit-rgb-image
   #:fixnum-rgb-image #:make-fixnum-rgb-image
   #:single-float-rgb-image #:make-single-float-rgb-image
   #:double-float-rgb-image #:make-double-float-rgb-image

   #:rgba-image
   #:4-bit-rgba-image #:make-4-bit-rgba-image
   #:8-bit-rgba-image #:make-8-bit-rgba-image
   #:16-bit-rgba-image #:make-16-bit-rgba-image
   #:32-bit-rgba-image #:make-32-bit-rgba-image
   #:fixnum-rgba-image #:make-fixnum-rgba-image
   #:single-float-rgba-image #:make-single-float-rgba-image
   #:double-float-rgba-image #:make-double-float-rgba-image

   ;;
   #:constrain
   #:with-image-bounds
   #:pixel-in-bounds

   #:transpose-image
   #:horizontal-flip-image
   #:vertical-flip-image

   #:copy-image
   #:crop-image
   #:trim-image
   
   #:map-array

   #:resize-image
   #:fit-image-into
   #:transform-image
   #:make-affine-transformation

   ;; convolution and friends
   #:discrete-convolve
   #:sharpen-image
   #:blur-image
   #:edge-detect-image

   ;; morphological operations
   #:morphological-op
   #:dilate
   #:erode
   
   #:clear-image

   ;; Drawing primitives
   #:fill-image
   #:fill-image*
   #:draw-circle
   #:draw-circle*
   #:fill-circle
   #:fill-circle*
   #:draw-rectangle
   #:draw-rectangle*
   #:fill-rectangle
   #:fill-rectangle*
   #:horizontal-line
   #:horizontal-line*
   #:vertical-line
   #:vertical-line*
   #:draw-line
   #:draw-line*
   #:draw-triangle
   #:draw-triangle*

   ;; Various Image Operations
   #:make-gamma-curve-lookup-table
   #:apply-gamma-curve-lookup-table
   #:apply-gamma

   ;; I/O
   #:read-jpeg-file
   #:read-jpeg-stream
   #:write-jpeg-file
   #:write-jpeg-stream

   #:read-png-file
   #:read-png-stream
   #:write-png-file
   #:write-png-stream

   #:read-tiff-file
   #:read-tiff-stream
   #:write-tiff-file
   #:write-tiff-stream
   
   #:read-pnm-stream
   #:read-pnm-file

   #:read-pbm-stream
   #:read-pbm-file
   #:write-pbm-stream
   #:write-pbm-file

   #:read-pgm-stream
   #:read-pgm-file
   #:write-pgm-stream
   #:write-pgm-file
   
   #:read-ppm-stream
   #:read-ppm-file
   #:write-ppm-stream
   #:write-ppm-file

   #:read-gif-stream
   #:read-gif-file
   #:write-gif-stream
   #:write-gif-file

   #:read-image-file
   #:read-image-stream
   #:write-image-file
   #:write-image-stream

   ;; conversion
   #:coerce-image

   ;; k-means clustering
   #:k-means-cluster-image-pixels

   ;; thresholding
   #:threshold-image
   #:min-error-threshold-image

   ;;
   ;; deprectated!!! do not use!!!
   #:convert-image-to-grayscale
   #:convert-image-to-grayscale-luminance
   #:convert-image-to-rgb
   #:convert-image-to-rgba
   #:convert-image-to-8-bit-grayscale
   ))

(in-package :opticl)

(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))

(handler-bind ((warning #'ignore-warning))
  (defpackage :opticl-color
    (:use :cl)))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-cltl2))

(defpackage :opticl-cltl2
  #+sbcl (:import-from :sb-cltl2 :variable-information)
  #+ccl (:import-from :ccl :variable-information)
  #+(or sbcl ccl allegro) (:export :variable-information))

#+allegro
(defun opticl-cltl2:variable-information (symbol &optional env)
  "A CLTL2-signature-compatible version of VARIABLE-INFORMATION on Allegro Common Lisp."
  (multiple-value-bind (binding-type locative decl localp)
      (sys:variable-information symbol env)
    (declare (ignore locative))
    (values binding-type localp decl)))
