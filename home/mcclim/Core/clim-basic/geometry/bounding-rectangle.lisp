(in-package #:climi)

;;; 4.1 Bounding rectangles

(defclass standard-bounding-rectangle (standard-rectangle) ())

(defun make-bounding-rectangle (x1 y1 x2 y2)
  (let ((x1 (coordinate x1))
        (y1 (coordinate y1))
        (x2 (coordinate x2))
        (y2 (coordinate y2)))
    (multiple-value-bind (x1 x2)
        (if (<= x1 x2)
            (values x1 x2)
            (values x2 x1))
      (multiple-value-bind (y1 y2)
          (if (<= y1 y2)
              (values y1 y2)
              (values y2 y1))
        (make-instance 'standard-bounding-rectangle :x1 x1 :y1 y1 :x2 x2 :y2 y2)))))

;;; 4.1.1 The Bounding Rectangle Protocol

(defmethod bounding-rectangle ((region rectangle))
  region)

(defmethod bounding-rectangle ((region region))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
    (make-bounding-rectangle x1 y1 x2 y2)))

;;; 4.1.2 Bounding Rectangle Convenience Functions

(defmacro with-bounding-rectangle* ((min-x min-y max-x max-y) region &body body)
  `(multiple-value-bind (,min-x ,min-y ,max-x ,max-y) (bounding-rectangle* ,region)
     ,@body))

(defmethod bounding-rectangle-position (bounding-rectangle)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* bounding-rectangle)
    (declare (ignore x2 y2))
    (values x1 y1)))

(defmethod bounding-rectangle-min-x (bounding-rectangle)
  (nth-value 0 (bounding-rectangle* bounding-rectangle)))

(defmethod bounding-rectangle-min-y (bounding-rectangle)
  (nth-value 1 (bounding-rectangle* bounding-rectangle)))

(defmethod bounding-rectangle-max-x (bounding-rectangle)
  (nth-value 2 (bounding-rectangle* bounding-rectangle)))

(defmethod bounding-rectangle-max-y (bounding-rectangle)
  (nth-value 3 (bounding-rectangle* bounding-rectangle)))

(defmethod bounding-rectangle-width (bounding-rectangle)
  (nth-value 0 (bounding-rectangle-size bounding-rectangle)))

(defmethod bounding-rectangle-height (bounding-rectangle)
  (nth-value 1 (bounding-rectangle-size bounding-rectangle)))

(defmethod bounding-rectangle-size (bounding-rectangle)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* bounding-rectangle)
    (values (- x2 x1) (- y2 y1))))
