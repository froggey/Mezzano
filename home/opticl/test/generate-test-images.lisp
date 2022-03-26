
(cl:in-package #:opticl-test)

(in-suite :opticl)

;;
;; TIFF grayscale images
(test tiff-write-bitmap-circle
  (let ((img (make-1-bit-gray-image 32 32)))
    (fill-image img 1)
    (draw-circle img 10 10 8 0)
    (let ((out (output-image "bitmap-circle.tiff")))
      (is (equal out
                 (write-tiff-file out img))))))

(test tiff-write-4-bit-gray-circle
  (let ((img (make-4-bit-gray-image 32 32)))
    (fill-image img 13)
    (draw-circle img 10 10 8 3)
    (let ((out (output-image "4-bit-gray-circle.tiff")))
      (is (equal out
                 (write-tiff-file out img))))))

(test tiff-write-8-bit-gray-circle
  (let ((img (make-8-bit-gray-image 32 32)))
    (fill-image img 127)
    (draw-circle img 10 10 8 16)
    (let ((out (output-image "8-bit-gray-circle.tiff")))
      (is (equal out
                 (write-tiff-file out img))))))

;;
;; JPEG grayscale images

;; no support for 1-bit JPEGs, AFAICT
#+nil
(test jpeg-write-bitmap-circle
  (let ((img (make-1-bit-gray-image 32 32)))
    (fill-image img 1)
    (draw-circle img 10 10 8 0)
    (let ((out (output-image "bitmap-circle.jpeg")))
      (is (equal out
                 (write-jpeg-file out img))))))

;; no support for 4-bit JPEGs, AFAICT
#+nil
(test jpeg-write-4-bit-gray-circle
  (let ((img (make-4-bit-gray-image 32 32)))
    (fill-image img 13)
    (draw-circle img 10 10 8 3)
    (let ((out (output-image "4-bit-gray-circle.jpeg")))
      (is (equal out
                 (write-jpeg-file out img))))))

(test jpeg-write-8-bit-gray-circle
  (let ((img (make-8-bit-gray-image 32 32)))
    (fill-image img 127)
    (draw-circle img 10 10 8 16)
    (let ((out (output-image "8-bit-gray-circle.jpeg")))
      (is (equal out
                 (write-jpeg-file out img))))))
