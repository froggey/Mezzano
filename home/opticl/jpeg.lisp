;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(defconstant +ncomp-gray+ 1)
(defconstant +ncomp-rgb+ 3)

(defparameter *rgb-sampling* '((1 1)(1 1)(1 1)))
(defparameter *rgb-q-tabs* (vector jpeg::+q-luminance-hi+
                                   jpeg::+q-chrominance-hi+))
(defparameter *gray-q-tabs* (vector jpeg::+q-luminance+))

;;;
;;; Reading JPEG files
(defun read-jpeg-stream (stream  &key (colorspace-conversion t))
  (multiple-value-bind (buffer height width ncomp)
      (jpeg:decode-stream stream :colorspace-conversion colorspace-conversion)
    (cond
      ((= ncomp +ncomp-rgb+)
       (let ((image (make-8-bit-rgb-image height width)))
         (declare (type 8-bit-rgb-image image))
         (loop for i below height
            do 
              (loop for j below width
                 do 
                   (let ((pixoff (* +ncomp-rgb+ (+ (* i width) j))))
                     (setf (pixel image i j)
                           (values (aref buffer (+ 2 pixoff))
                                   (aref buffer (+ 1 pixoff))
                                   (aref buffer  pixoff))))))
         image))
      ((= ncomp 1)
       (let ((image (make-8-bit-gray-image height width))
             (pixoff 0))
         (declare (type 8-bit-gray-image image))
         (loop for i below height
            do 
              (loop for j below width
                 do 
                   (setf (pixel image i j)
                         (aref buffer pixoff))
                   (incf pixoff)))
         image)))))

(defun read-jpeg-file (pathname &key (colorspace-conversion t))
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-jpeg-stream stream :colorspace-conversion colorspace-conversion)))

(defun write-jpeg-stream (stream image)
  (typecase image
    (8-bit-gray-image
     (locally
         (declare (type 8-bit-gray-image image))
       (destructuring-bind (height width)
           (array-dimensions image)
         (let ((jpeg-array (make-array (* height width) :element-type '(unsigned-byte 8)))
               (pixoff 0))
           (loop for i below height
              do
                (loop for j below width
                   do
                     (setf (aref jpeg-array pixoff)
                           (pixel image i j))
                     (incf pixoff)))
           (jpeg::encode-image-stream stream jpeg-array +ncomp-gray+ height width
                                      :q-tabs *gray-q-tabs*)))))

    (8-bit-rgb-image
     (locally
         (declare (type 8-bit-rgb-image image))
       (destructuring-bind (height width channels)
           (array-dimensions image)
         (declare (ignore channels))
         (let ((jpeg-array (make-array (* height width +ncomp-rgb+) :element-type '(unsigned-byte 8))))
           (loop for i below height
              do
                (loop for j below width
                   do
                     (let ((pixoff (* +ncomp-rgb+ (+ (* i width) j))))
                       (multiple-value-bind
                             (r g b)
                           (pixel image i j)
                         (setf (aref jpeg-array pixoff) b
                               (aref jpeg-array (incf pixoff)) g
                               (aref jpeg-array (incf pixoff)) r)))))
           (jpeg::encode-image-stream stream jpeg-array +ncomp-rgb+ height width
                                      :sampling *rgb-sampling*
                                      :q-tabs *rgb-q-tabs*)))))

    ;; NB: The JPEG format doesn't, AAICT, have a well-specified way
    ;; of writing an RGBA image. So, for now at least, we'll punt and
    ;; write it as an RGB image.
    (8-bit-rgba-image
     (locally
         (declare (type 8-bit-rgba-image image))
       (destructuring-bind (height width channels)
           (array-dimensions image)
         (declare (ignore channels))
         (let ((jpeg-array (make-array (* height width +ncomp-rgb+) :element-type '(unsigned-byte 8))))
           (loop for i below height
              do
                (loop for j below width
                   do
                     (let ((pixoff (* +ncomp-rgb+ (+ (* i width) j))))
                       (multiple-value-bind
                             (r g b a)
                           (pixel image i j)
                         (declare (ignore a))
                         (setf (aref jpeg-array pixoff) b
                               (aref jpeg-array (incf pixoff)) g
                               (aref jpeg-array (incf pixoff)) r)))))
           (jpeg::encode-image-stream stream jpeg-array +ncomp-rgb+ height width
                                      :sampling *rgb-sampling*
                                      :q-tabs *rgb-q-tabs*)))))

    (t (error "Cannot write a JPEG image from ~A" (type-of image)))))

(defun write-jpeg-file (pathname image)
  (with-open-file (stream pathname
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-jpeg-stream stream image)
    pathname))


