(cl:defpackage :png2lisp
  (:use :cl :iterate)
  (:export :convert))

(in-package :png2lisp)

(require :png-read)

(defun png-colour (png y x)
  (logior (ash (aref png x y 0) 16)
          (ash (aref png x y 1) 8)
          (aref png x y 2)
          (ash #xFF #+nil(aref png x y 3) 24)))

(defun convert (path name &optional
                       (output-path (make-pathname :type "lisp" :defaults path)))
  (let* ((png (png-read:read-png-file path))
         (data (png-read:image-data png))
         (width (png-read:width png))
         (height (png-read:height png)))
    (with-open-file (output output-path :direction :output :if-exists :supersede)
      (format output "(cl:defvar ~A (cl:make-array '(~D ~D) :element-type '(cl:unsigned-byte 32)))
 (setf (subseq (make-array ~D :displaced-to ~A) 0) #(~%"
              name
              height width
              (* height width)
              name)
      (dotimes (y height)
        (dotimes (x width)
          (format output " #x~X" (png-colour data y x)))
        (format output "~%"))
      (format output "))~%"))))
