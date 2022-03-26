(defpackage #:mandelbrot
  (:use #:cl #:zpng))

(in-package #:mandelbrot)

(defun draw-mandelbrot (file)
  (let* ((png (make-instance 'png
                             :color-type :grayscale-alpha
                             :width 200
                             :height 200))
         (image (data-array png))
         (max 255))
    (dotimes (y 200 (write-png png file))
      (dotimes (x 200)
        (let ((c (complex (- (/ x 100.0) 1.5) (- (/ y 100.0) 1.0)))
              (z (complex 0.0 0.0))
              (iteration 0))
          (loop
           (setf z (+ (* z z) c))
           (incf iteration)
           (cond ((< 4 (abs z))
                  (setf (aref image y x 1) iteration)
                  (return))
                 ((= iteration max)
                  (setf (aref image y x 1) 255)
                  (return))))))))))
