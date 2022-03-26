
(in-package :retrospectiff)

;;; LZW tests

(defparameter *img* (vector-contents-of-file "images/snow.tiff"))
(defparameter *img-lzw* (lzw-encode *img*))
(defparameter *img-codes* (lzw-decode-codes *img-lzw*))
(defparameter *img-decoded* (lzw-decode *img-lzw*))

(progn
  (defparameter *img* (vector-contents-of-file "images/snow.tiff"))
  (defparameter *img-lzw* (lzw-encode *img*))
  (defparameter *img-decoded* (lzw-decode *img-lzw*))
  (equalp *img* *img-decoded*))

(let ((string "AAA"))
  (equal string (map 'string #'code-char (lzw-decode (lzw-encode string)))))

(defparameter *sbcl-core*
  (vector-contents-of-file
   #p"/Users/sly/projects/sbcl.boinkor.net/sbcl.git.darwin-x86+sb-thread/output/sbcl.core"))
(time (defparameter *sbcl-core-lzw* (lzw-encode *sbcl-core*)))
(defparameter *sbcl-core-decoded* (lzw-decode *sbcl-core-lzw*))


;;; TIFF reading

(defparameter *snow-image* (retrospectiff:read-tiff-file "images/snow.tiff"))
(defparameter *snow-lzw-image* (read-tiff-file "images/snow-lzw.tiff"))

(equalp (tiff-image-data *snow-image*)
        (tiff-image-data *snow-lzw-image*))


(write-tiff-file "foo.tiff" *snow-image*  :if-exists :supersede)

(with-open-file (stream "images/snow.tiff" :element-type '(unsigned-byte 8))
  (let ((length (file-length stream)))
    (let ((vector (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence vector stream)
      (with-open-file (outstream "quux.tiff"
                                 :direction :output
                                 :element-type '(unsigned-byte 8))
        (write-sequence vector outstream)))))

