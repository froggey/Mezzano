
(cl:in-package #:opticl-test)

(in-suite :opticl)

;;
;; Bitmap TIFF Images
(test tiff-bitmap-write-and-read
  (let ((img (make-1-bit-gray-image 32 32)))
    (fill-image img 1)
    (draw-circle img 10 10 8 0)
    (let ((out (output-image "bitmap-circle.tiff")))
      (write-tiff-file out img)
      (let ((input-img (read-tiff-file out)))
        (is (equalp img input-img))))))

;;
;; TIFF 4-bit Grayscale Round Trip Read and Write
(test tiff-4-bit-gray-write-and-read
  (let ((img (make-4-bit-gray-image 32 32)))
    (fill-image img 5)
    (draw-circle img 10 10 8 1)
    (let ((out (output-image "4-bit-gray-circle.tiff")))
      (write-tiff-file out img)
      (let ((input-img (read-tiff-file out)))
        (is (equalp img input-img))))))

;;
;; TIFF 8-bit Write and Read
(test tiff-8-bit-gray-write-and-read
  (let ((img (make-8-bit-gray-image 32 32)))
    (fill-image img 255)
    (draw-circle img 10 10 8 127)
    (let ((out (output-image "8-bit-gray-circle.tiff")))
      (write-tiff-file out img)
      (let ((input-img (read-tiff-file out)))
        (is (equalp img input-img))))))


;;
;; TIFF 8-bit Grayscale Round Trip Read and Write
(test tiff-read-8-bit-gray-no-compression
  (let* ((file (test-image "truck-gray-none.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray-from-none.tiff")))
      (is (equal out (write-tiff-file out img)))
      (let ((input-img (read-image-file out)))
        (is (equalp img input-img))))))

(test tiff-read-8-bit-gray-lzw-compression
  (let* ((file (test-image "truck-gray-lzw.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray-from-lzw.tiff")))
      (is (equal out (write-tiff-file out img)))
      (let ((input-img (read-image-file out)))
        (is (equalp img input-img))))))

(test tiff-read-8-bit-gray-packbits-compression
  (let* ((file (test-image "truck-gray-packbits.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray-from-packbits.tiff")))
      (is (equal out (write-tiff-file out img)))
      (let ((input-img (read-image-file out)))
        (is (equalp img input-img))))))

(test tiff-read-8-bit-gray-deflate-compression
  (let* ((file (test-image "truck-gray-deflate.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray-from-deflate.tiff")))
      (is (equal out (write-tiff-file out img)))
      (let ((input-img (read-image-file out)))
        (is (equalp img input-img))))))

(test tiff-read-8-bit-gray-jpeg-compression
  (let* ((file (test-image "truck-gray-jpeg.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray-from-jpeg.tiff")))
      (is (equal out (write-tiff-file out img)))
      (let ((input-img (read-image-file out)))
        (is (equalp img input-img))))))

;;
;;; TIFF RGB Image Round Trip Read and Write
(test tiff-read-8-bit-rgb-no-compression
  (let* ((file (test-image "truck-rgb-none.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-rgb-from-none.tiff")))
      (is (equal out (write-tiff-file out img)))
      (let ((input-img (read-image-file out)))
        (is (equalp img input-img))))))

(test tiff-read-8-bit-rgb-lzw-compression
  (let* ((file (test-image "truck-rgb-lzw.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-rgb-from-lzw.tiff")))
      (is (equal out (write-tiff-file out img)))
      (let ((input-img (read-image-file out)))
        (is (equalp img input-img))))))

(test tiff-read-8-bit-rgb-packbits-compression
  (let* ((file (test-image "truck-rgb-packbits.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-rgb-from-packbits.tiff")))
      (is (equal out (write-tiff-file out img)))
      (let ((input-img (read-image-file out)))
        (is (equalp img input-img))))))

(test tiff-read-8-bit-rgb-jpeg-compression
  (let* ((file (test-image "truck-rgb-jpeg.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-rgb-from-jpeg.tiff")))
      (is (equal out (write-tiff-file out img)))
      (let ((input-img (read-image-file out)))
        (is (equalp img input-img))))))

(test tiff-read-8-bit-rgb-deflate-compression
  (let* ((file (test-image "truck-rgb-deflate.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-rgb-from-deflate.tiff")))
      (is (equal out (write-tiff-file out img)))
      (let ((input-img (read-image-file out)))
        (is (equalp img input-img))))))

;; 16-bit RGB

(test tiff-read-16-bit-rgb
  (let* ((file (test-image "horse-16-bit.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "horse-16-bit.tiff")))
      (is (equal out (write-tiff-file out img)))
      (let ((input-img (read-image-file out)))
        (is (equalp img input-img))))))

;; Indexed RGB TIFF image

(test tiff-read-indexed-rgb
  (let* ((file (test-image "camel-indexed.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "camel-indexed.tiff")))
      (is (equal out (write-tiff-file out img)))
      (let ((input-img (read-image-file out)))
        (is (equalp img input-img))))))
