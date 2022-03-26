
(in-package :opticl)

(defun read-tga-file (file)
  (let ((tga (tga:read-tga file)))
    (with-accessors ((width tga::image-width)
                     (height tga::image-height)
                     (bits tga::image-bpp)
                     (channels tga::image-channels)
                     (data tga::image-data))
        tga
      ;; (list width height bits channels)
      (cond ((and (= bits 24)
                  (= channels 3))
             (let ((image (make-8-bit-rgb-image height width)))
               (declare (type 8-bit-rgb-image image))
               (loop for i below height
                  do
                    (loop for j below width
                       do
                         (let ((pixoff (* 3 (+ (* i width) j))))
                           (setf (pixel image (- height i 1) j)
                                 (values (aref data (+ 2 pixoff))
                                         (aref data (+ 1 pixoff))
                                         (aref data pixoff))))))
               image))
            ((and (= bits 32)
                  (= channels 4))
             (let ((image (make-8-bit-rgba-image height width)))
               (declare (type 8-bit-rgb-image image))
               (loop for i below height
                  do
                    (loop for j below width
                       do
                         (let ((pixoff (* 4 (+ (* i width) j))))
                           (setf (pixel image (- height i 1) j)
                                 (values (aref data (+ 2 pixoff))
                                         (aref data (+ 1 pixoff))
                                         (aref data pixoff)
                                         (aref data (+ 3 pixoff)))))))
               image))))))
