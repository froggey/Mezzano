
(cl:in-package #:retrospectiff2-test)

(in-suite :retrospectiff2)

;; retrospectiff2 tests

(defun test-image (filename)
  (reduce #'merge-pathnames (list filename "test/images/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "retrospectiff"))))

(defun output-image (filename)
  (reduce #'merge-pathnames (list filename "test/output2/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "retrospectiff"))))

(ensure-directories-exist (output-image ""))


(test tiff2-read-and-write-simple-tiff-rgb-file
  (let* ((img (read-tiff-file (test-image "blocks.tiff"))))
    (let ((out (output-image "blocks.tiff")))
      (is (equal out (write-tiff-file out img :if-exists :supersede)))
      (let ((input-img (read-tiff-file out)))
        (is (equalp (tiff-image-data img)
                    (tiff-image-data input-img)))))))

(test tiff2-read-and-write-simple-tiff-gray-file
  (let* ((img (read-tiff-file (test-image "blocks-gray.tiff"))))
    (let ((out (output-image "blocks-gray.tiff")))
      (is (equal out (write-tiff-file out img :if-exists :supersede)))
      (let ((input-img (read-tiff-file out)))
        (is (equalp (tiff-image-data img)
                    (tiff-image-data input-img)))))))

(defmacro read-and-write-image-test (test-name input-filename output-filename)
  `(test ,test-name
    (let ((img (read-tiff-file (test-image ,input-filename))))
      (let ((out (output-image ,output-filename)))
        (is (equal out (write-tiff-file out img :if-exists :supersede)))
        (let ((input-img (read-tiff-file out)))
          (is (equalp (tiff-image-data img)
                      (tiff-image-data input-img))))))))

(read-and-write-image-test tiff2-read-and-write-planar-tiff-rgb-file
                           "ortex.tiff" "ortex.tiff")

(read-and-write-image-test tiff2-read-and-write-file-8-bit-grayscale-no-compression
                           "window-8bit-none.tiff" "window-8bit-none.tiff")

(read-and-write-image-test tiff2-read-and-write-file-8-bit-grayscale-lzw-compression
                           "window-8bit-lzw.tiff" "window-8bit-lzw.tiff")

(read-and-write-image-test tiff2-read-and-write-file-8-bit-grayscale-deflate-with-predictor-compression
                           "window-8bit-deflate-with-predictor.tiff"
                           "window-8bit-none-from-deflate-with-predictor.tiff")

(read-and-write-image-test tiff2-read-and-write-indexed-rgb-image
                           "camel-indexed.tiff" "camel-indexed.tiff")

(read-and-write-image-test tiff2-read-and-write-bitmap-file
                           "goat-bitmap.tiff" "goat-bitmap.tiff")

(read-and-write-image-test tiff2-read-and-write-4-bit-grayscale-image
                           "4-bit-gray-circle.tiff" "4-bit-gray-circle.tiff")

(read-and-write-image-test tiff2-read-and-write-8-bit-grayscale-image
                           "truck-gray.tiff" "truck-gray.tiff")

(read-and-write-image-test tiff2-read-and-write-16-bit-grayscale-image
                           "window.tiff" "window.tiff")

(read-and-write-image-test tiff2-read-and-write-16-bit-rgb-image
                           "horse-16-bit.tiff" "horse-16-bit.tiff")


