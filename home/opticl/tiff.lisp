;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

;;;
;;; Reading TIFF files
(defun read-tiff-stream (stream)
  "reads a TIFF image from a stream and returns either a 32-bit ARGB
image or an 8-bit grayscale image"
  (let ((tiff-image (tiff2:read-tiff-stream stream)))
    (with-accessors ((image-length tiff:tiff-image-length)
                     (image-width tiff:tiff-image-width)
                     (samples-per-pixel tiff:tiff-image-samples-per-pixel) 
                     (bits-per-sample tiff:tiff-image-bits-per-sample) 
                     (image-data tiff:tiff-image-data)
                     (color-map tiff:tiff-image-color-map)
                     (min-is-white tiff:tiff-image-min-is-white))
        tiff-image
      (cond

        (color-map  ;; indexed RGB
         ;; FIXME! This should probably be moved to retrospectiff
         (let ((image (make-8-bit-rgb-image image-length image-width)))
           (declare (type 8-bit-rgb-image image))
           (loop for i below image-length
              do 
                (loop for j below image-width
                   do 
                     (setf (pixel* image i j)
                           (mapcar (lambda (x) (ash x -8))
                                   (aref color-map
                                         (pixel image-data i j))))))
           image))

        ((and (= samples-per-pixel 1)
              (equalp bits-per-sample 1)) ;; black and white
         image-data)

        ((and (= samples-per-pixel 1)
              (equalp bits-per-sample 4)) ;; 4-bit Grayscale
         image-data)

        ((and (= samples-per-pixel 1)
              (equalp bits-per-sample 8)) ;; 8-bit Grayscale
         image-data)

        ((and (= samples-per-pixel 3)
              (equalp bits-per-sample #(8 8 8))) ;; 8-bit RGB
         image-data)

        ((and (= samples-per-pixel 4)
              (equalp bits-per-sample #(8 8 8 8))) ;; 8-bit RGBA
         ;; FIXME! We're just faking the alpha channel here
         (coerce-image image-data '8-bit-rgba-image))


        ((and (= samples-per-pixel 3)
              (equalp bits-per-sample #(16 16 16))) ;; 16-bit RGB
         image-data)
            
        ((and (= samples-per-pixel 4)
              (equalp bits-per-sample #(16 16 16 16))) ;; 16-bit RGBA
         ;; FIXME! Test this, I'm not sure it really works.
         image-data)
        (t 
         (error "TIFF decoding error"))))))

(defun read-tiff-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-tiff-stream stream)))

(defun write-tiff-stream (stream image &key byte-order)
  (apply #'tiff2:write-tiff-stream stream image
         (when byte-order `(:byte-order ,byte-order))))

(defun write-tiff-file (pathname image &key byte-order)
  (apply #'tiff2:write-tiff-file pathname image
         :if-exists :supersede
         (when byte-order `(:byte-order ,byte-order))))

