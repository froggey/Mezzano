;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(defun write-png-stream (stream image)
  (typecase image
    (8-bit-rgb-image
     (with-image-bounds (ymax xmax)
         image
       (let ((displaced
              (make-array (* ymax xmax 3)
                          :element-type '(unsigned-byte 8)
                          :initial-contents
                          (make-array (* ymax xmax 3)
                                      :element-type '(unsigned-byte 8)
                                      :displaced-to image))))
         (zpng:write-png-stream 
          (make-instance 'zpng:png
                         :color-type :truecolor
                         :height ymax
                         :width xmax
                         :bpp 8
                         :image-data displaced)
          stream))))
    (8-bit-rgba-image
     (with-image-bounds (ymax xmax)
         image
       (let ((displaced
              (make-array (* ymax xmax 4)
                          :element-type '(unsigned-byte 8)
                          :initial-contents
                          (make-array (* ymax xmax 4)
                                      :element-type '(unsigned-byte 8)
                                      :displaced-to image))))
         (zpng:write-png-stream 
          (make-instance 'zpng:png
                         :color-type :truecolor-alpha
                         :height ymax
                         :width xmax
                         :bpp 8
                         :image-data displaced)
          stream))))
    (8-bit-gray-image
     (with-image-bounds (ymax xmax)
         image
       (let ((displaced
              (make-array (* ymax xmax)
                          :element-type '(unsigned-byte 8)
                          :initial-contents
                          (make-array (* ymax xmax)
                                      :element-type '(unsigned-byte 8)
                                      :displaced-to image))))
         (zpng:write-png-stream 
          (make-instance 'zpng:png
                         :color-type :grayscale
                         :height ymax
                         :width xmax
                         :bpp 8
                         :image-data displaced)
          stream))))
    (t (error "No PNG writing support for this image type."))))

(defun write-png-file (pathname image)
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-png-stream stream image)
    (truename pathname)))
