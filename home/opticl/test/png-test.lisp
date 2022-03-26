
(cl:in-package #:opticl-test)

(in-suite :opticl)

;;
;; Grayscale PNG Images

;; zpng doesn't support writing 1-bit grayscale (bitmap) images
#+nil
(test png-bitmap-write-and-read
  (let ((img (make-1-bit-gray-image 32 32)))
    (fill-image img 1)
    (draw-circle img 10 10 8 0)
    (let ((out (output-image "bitmap-circle.png")))
      (write-png-file out img)
      (let ((input-img (read-png-file out)))
        (is (equalp img input-img))))))

;; zpng doesn't support writing 4-bit grayscale images
#+nil
(test png-4-bit-gray-write-and-read
  (let ((img (make-4-bit-gray-image 32 32)))
    (fill-image img 5)
    (draw-circle img 10 10 8 1)
    (let ((out (output-image "4-bit-gray-circle.png")))
      (write-png-file out img)
      (let ((input-img (read-png-file out)))
        (is (equalp img input-img))))))

(test png-8-bit-gray-write-and-read
  (let ((img (make-8-bit-gray-image 32 32)))
    (fill-image img 255)
    (draw-circle img 10 10 8 127)
    (let ((out (output-image "8-bit-gray-circle.png")))
      (write-png-file out img)
      (let ((input-img (read-png-file out)))
        (is (equalp img input-img))))))
