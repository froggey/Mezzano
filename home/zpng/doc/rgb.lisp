(defpackage #:rgb
  (:use #:cl #:zpng))

(in-package #:rgb)

(defun draw-rgb (file)
  (let ((png (make-instance 'pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width 200
                             :height 200)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (start-png png stream)
      (loop for a from 38 to 255 by 31
	do (loop for b from 10 to 255 by 10
	     do (loop for g from 38 to 255 by 31
		  do (loop for r from 10 to 255 by 10
			do (write-pixel (list r g b a) png)))))
      (finish-png png))))
