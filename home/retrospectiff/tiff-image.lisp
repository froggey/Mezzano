
(in-package #:retrospectiff.image)

(defclass tiff-image ()
  ((length :accessor tiff-image-length :initarg :length)
   (width :accessor tiff-image-width :initarg :width)
   (bits-per-sample :accessor tiff-image-bits-per-sample :initarg :bits-per-sample)
   (samples-per-pixel :accessor tiff-image-samples-per-pixel
                      :initarg :samples-per-pixel
                      :initform nil)
   (data :accessor tiff-image-data :initarg :data)
   (byte-order :accessor tiff-image-byte-order :initarg :byte-order)
   (color-map :accessor tiff-image-color-map :initarg :color-map :initform nil)
   (min-is-white :accessor tiff-image-min-is-white :initarg :min-is-white
                 :initform nil)))


