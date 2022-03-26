
(cl:in-package #:retrospectiff-test)

(in-suite :retrospectiff)

;;; TIFF reading

(defun test-image (filename)
  (reduce #'merge-pathnames (list filename "test/images/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "retrospectiff"))))

(defun output-image (filename)
  (reduce #'merge-pathnames (list filename "test/output/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "retrospectiff"))))

(ensure-directories-exist (output-image ""))

(test tiff-read-and-write-simple-tiff-rgb-file
  (let* ((img (read-tiff-file (test-image "blocks.tiff"))))
    (let ((out (output-image "blocks.tiff")))
      (is (equal out (write-tiff-file out img :if-exists :supersede)))
      (let ((input-img (read-tiff-file out)))
        (is (equalp (tiff-image-data img)
                    (tiff-image-data input-img)))))))

(test tiff-read-and-write-simple-tiff-gray-file
  (let* ((img (read-tiff-file (test-image "blocks-gray.tiff"))))
    (let ((out (output-image "blocks-gray.tiff")))
      (is (equal out (write-tiff-file out img :if-exists :supersede)))
      (let ((input-img (read-tiff-file out)))
        (is (equalp (tiff-image-data img)
                    (tiff-image-data input-img)))))))

#+nil
(test tiff-read-and-write-simple-tiff-gray-file
  (let* ((img (read-tiff-file (test-image "truck-gray-none.tiff"))))
    (let ((out (output-image "truck-gray-from-none.tiff")))
      (is (equal out (write-tiff-file out img :if-exists :supersede)))
      (let ((input-img (read-tiff-file out)))
        (is (equalp (tiff-image-data img)
                    (tiff-image-data input-img)))))))

#+nil
(defparameter *snow-image* (read-tiff-file (test-image "snow.tiff")))

#+nil
(defparameter *snow-lzw-image* (read-tiff-file (test-image "snow-lzw.tiff")))

#+nil
(assert (equalp (tiff-image-data *snow-image*)
                (tiff-image-data *snow-lzw-image*)))

#+nil (write-tiff-file "foo.tiff" *snow-image* :if-exists :supersede)

#+nil
(with-open-file (stream "test/images/snow.tiff" :element-type '(unsigned-byte 8))
  (let ((length (file-length stream)))
    (let ((vector (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence vector stream)
      (with-open-file (outstream "quux.tiff"
                                 :direction :output
                                 :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
        (write-sequence vector outstream)))))

(test tiff-read-and-write-planar-tiff-rgb-file
  (let* ((img (read-tiff-file (test-image "ortex.tiff"))))
    (let ((out (output-image "ortex.tiff")))
      (is (equal out (write-tiff-file out img :if-exists :supersede)))
      (let ((input-img (read-tiff-file out)))
        (is (equalp (tiff-image-data img)
                    (tiff-image-data input-img)))))))

(test tiff-read-and-write-file-8-bit-grayscale-no-compression
  (let* ((img (read-tiff-file (test-image "window-8bit-none.tiff"))))
    (let ((out (output-image "window-8bit-none.tiff")))
      (is (equal out (write-tiff-file out img :if-exists :supersede)))
      (let ((input-img (read-tiff-file out)))
        (is (equalp (tiff-image-data img)
                    (tiff-image-data input-img)))))))

(test tiff-read-and-write-file-8-bit-grayscale-lzw-compression
  (let* ((img (read-tiff-file (test-image "window-8bit-lzw.tiff"))))
    (let ((out (output-image "window-8bit-none-from-lzw.tiff")))
      (is (equal out (write-tiff-file out img :if-exists :supersede)))
      (let ((input-img (read-tiff-file out)))
        (is (equalp (tiff-image-data img)
                    (tiff-image-data input-img)))))))

(test tiff-read-and-write-file-8-bit-grayscale-deflate-with-predictor-compression
  (let* ((img (read-tiff-file (test-image "window-8bit-deflate-with-predictor.tiff"))))
    (let ((out (output-image "window-8bit-none-from-deflate-with-predictor.tiff")))
      (is (equal out (write-tiff-file out img :if-exists :supersede)))
      (let ((input-img (read-tiff-file out)))
        (is (equalp (tiff-image-data img)
                    (tiff-image-data input-img)))))))

(test tiff-read-and-write-indexed-rgb-image
  (let* ((img (read-tiff-file (test-image "camel-indexed.tiff"))))
    (let ((out (output-image "camel-indexed.tiff")))
      (is (equal out (write-tiff-file out img :if-exists :supersede)))
      (let ((input-img (read-tiff-file out)))
        (is (equalp (tiff-image-data img)
                    (tiff-image-data input-img)))))))

