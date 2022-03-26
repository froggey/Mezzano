
(cl:in-package #:opticl-test)

(in-suite :opticl)

(test jpeg-read-8-bit-gray
  (let* ((file (test-image "truck-gray.jpeg"))
         (img (read-jpeg-file file)))
    img))

;;
;; Grayscale JPEG Images

;; no support for 1-bit JPEGs, AFAICT
#+nil
(test jpeg-bitmap-write-and-read
  (let ((img (make-1-bit-gray-image 32 32)))
    (fill-image img 1)
    (draw-circle img 10 10 8 0)
    (let ((out (output-image "bitmap-circle.jpeg")))
      (write-jpeg-file out img)
      (let ((input-img (read-jpeg-file out)))
        (is (equalp img input-img))))))

;; no support for 4-bit JPEGs, AFAICT
#+nil
(test jpeg-4-bit-gray-write-and-read
  (let ((img (make-4-bit-gray-image 32 32)))
    (fill-image img 5)
    (draw-circle img 10 10 8 1)
    (let ((out (output-image "4-bit-gray-circle.jpeg")))
      (write-jpeg-file out img)
      (let ((input-img (read-jpeg-file out)))
        (is (equalp img input-img))))))

(test jpeg-8-bit-gray-write-and-read
  (let ((height 32)
        (width 32))
    (let ((img (make-8-bit-gray-image height width)))
      (fill-image img 192)
      (draw-circle img 10 10 8 63)
      (let ((out (output-image "8-bit-gray-circle.jpeg")))
        (write-jpeg-file out img)
        (let ((input-img (read-jpeg-file out)))
          ;; the JPEG images won't be identical, so let's see if we
          ;; take the difference between the two images it ends up
          ;; being less than some arbitrary threshold
          (let ((img-diff (abs (sum-of-element-wise-differences img input-img)))
                (difference-threshold (* height width 4)))
            (is (< img-diff difference-threshold))))))))

