
(in-package #:opticl)

(defun fill-image (img &rest vals)
  (with-image-bounds (height width)
      img
    (loop for i below height
       do (loop for j below width 
             do 
               (setf (pixel img i j) (values-list vals))))))

(defun fill-image* (img list)
  (multiple-value-call #'fill-image img (values-list list)))

(defun horizontal-line (img y x0 x1 &rest vals)
  (declare (type fixnum y x0 x1))
  (with-image-bounds (ymax xmax)
      img
    (let ((y (constrain y 0 (1- ymax)))
          (x0 (constrain x0 0 (1- xmax)))
          (x1 (constrain x1 0 (1- xmax))))
      (loop for x fixnum from x0 to x1
         do (setf (pixel img y x) (values-list vals))))))

(defun horizontal-line* (img y x0 x1 list)
  (multiple-value-call #'horizontal-line img y x0 x1 (values-list list)))

(defun vertical-line (img y0 y1 x &rest vals)
  (declare (type fixnum y0 y1 x))
  (with-image-bounds (ymax xmax)
      img
    (let ((y0 (constrain y0 0 (1- ymax)))
          (y1 (constrain y1 0 (1- xmax)))
          (x (constrain x 0 (1- xmax))))
      (loop for y fixnum from y0 to y1
         do (when (pixel-in-bounds img y x)
              (setf (pixel img y x) (values-list vals)))))))

(defun vertical-line* (img y0 y1 x list)
  (multiple-value-call #'vertical-line img y0 y1 x (values-list list)))

(defun draw-line (img y0 x0 y1 x1 &rest vals)
  (declare (type fixnum y0 x0 y1 x1))
  (let ((dx (- x1 x0))
        (dy (- y1 y0)))
    (declare (type fixnum dx dy))
    (let ((absdx (abs dx))
          (absdy (abs dy)))
      (declare (type fixnum absdx absdy))
      (let ((xstep (if (minusp dx) -1 1))
            (ystep (if (minusp dy) -1 1)))
        (if (>= absdx absdy)
            (let ((d (- (* 2 absdy) absdx))
                  (incr-e (* 2 absdy))
                  (incr-ne (* 2 (- absdy absdx)))
                  (x x0)
                  (y y0))
              (declare (type fixnum d incr-e incr-ne x y))
              (when (pixel-in-bounds img y x)
                (setf (pixel img y x) (values-list vals)))
              (dotimes (i absdx)
                (cond
                  ((<= d 0)
                   (incf d incr-e)
                   (incf x xstep))
                  (t
                   (incf d incr-ne)
                   (incf x xstep)
                   (incf y ystep)))
                (when (pixel-in-bounds img y x)
                  (setf (pixel img y x) (values-list vals)))))
            (let ((d (- (* 2 absdy) absdx))
                  (incr-n (* 2 absdx))
                  (incr-ne (* 2 (- absdx absdy)))
                  (x x0)
                  (y y0))
              (declare (type fixnum d incr-n incr-ne x y))
                (when (pixel-in-bounds img y x)
                  (setf (pixel img y x) (values-list vals)))
              (dotimes (i absdy)
                (cond
                  ((<= d 0)
                   (incf d incr-n)
                   (incf y ystep))
                  (t
                   (incf d incr-ne)
                   (incf y ystep)
                   (incf x xstep)))
                (when (pixel-in-bounds img y x)
                  (setf (pixel img y x) (values-list vals))))))))))

(defun draw-line* (img y0 x0 y1 x1 list)
  (multiple-value-call #'draw-line img y0 x0 y1 x1 (values-list list)))

(defun draw-circle (img center-y center-x radius &rest vals)
  "draws a circle centered at (x, y) with radius r on a image."
  (declare (type fixnum center-y center-x radius))
  (flet ((pixel-if (y x vals)
           (when (pixel-in-bounds img y x)
             (setf (pixel img y x) (values-list vals)))))
    (flet ((circle-points (y x)
             (pixel-if (+ center-y y) (+ center-x x) vals)
             (pixel-if (+ center-y x) (+ center-x y) vals)
             (pixel-if (- center-y x) (+ center-x y) vals)
             (pixel-if (- center-y y) (+ center-x x) vals)
             (pixel-if (- center-y y) (- center-x x) vals)
             (pixel-if (- center-y x) (- center-x y) vals)
             (pixel-if (+ center-y x) (- center-x y) vals)
             (pixel-if (+ center-y y) (- center-x x) vals)))
      (let ((x 0)
            (y radius)
            (d (- 1 radius))
            (delta-e 3)
            (delta-se (+ (* -2 radius) 5)))
        (declare (type fixnum x y d delta-e delta-se))
        (circle-points y x)
        (do () ((>= x y))
          (if (< d 0)
              (progn
                (incf d delta-e)
                (incf delta-e 2)
                (incf delta-se 2))
              (progn
                (incf d delta-se)
                (incf delta-e 2)
                (incf delta-se 4)
                (decf y)))
          (incf x)
          (circle-points y x))))))

(defun draw-circle* (img center-y center-x radius list)
  (multiple-value-call #'draw-circle img center-y center-x radius (values-list list)))

(defun fill-circle (img center-y center-x radius &rest vals)
  "draws a filled circle centered at (x, y) with radius r on a image."
  (declare (type fixnum center-y center-x radius))
  (flet ((circle-lines (y x)
           (apply #'horizontal-line img (- center-y y) (- center-x x) (+ center-x x) vals)
           (apply #'horizontal-line img (- center-y x) (- center-x y) (+ center-x y) vals)
           (apply #'horizontal-line img (+ center-y y) (- center-x x) (+ center-x x) vals)
           (apply #'horizontal-line img (+ center-y x) (- center-x y) (+ center-x y) vals)))
    (let ((x 0)
          (y radius)
          (d (- 1 radius))
          (delta-e 3)
          (delta-se (+ (* -2 radius) 5)))
      (declare (type fixnum x y d delta-e delta-se))
      (circle-lines y x)
      (do () ((>= x y))
        (if (< d 0)
            (progn
              (incf d delta-e)
              (incf delta-e 2)
              (incf delta-se 2))
            (progn
              (incf d delta-se)
              (incf delta-e 2)
              (incf delta-se 4)
              (decf y)))
        (incf x)
        (circle-lines y x)))))

(defun fill-circle* (img center-y center-x radius list)
  (multiple-value-call #'fill-circle img center-y center-x radius (values-list list)))

(defun draw-rectangle (img y0 x0 y1 x1 &rest vals)
  (apply #'horizontal-line img y0 x0 x1 vals)
  (apply #'vertical-line img y0 y1 x0 vals)
  (apply #'vertical-line img y0 y1 x1 vals)
  (apply #'horizontal-line img y1 x0 x1 vals))

(defun draw-rectangle* (img y0 x0 y1 x1 list)
  (multiple-value-call #'draw-rectangle img y0 x0 y1 x1 (values-list list)))

(defun fill-rectangle (img y0 x0 y1 x1 &rest vals)
  (loop for x from x0 to x1
     do 
       (apply #'vertical-line img y0 y1 x vals)))

(defun fill-rectangle* (img y0 x0 y1 x1 list)
  (multiple-value-call #'fill-rectangle img y0 x0 y1 x1 (values-list list)))

(defun draw-triangle (img y0 x0 y1 x1 y2 x2 &rest vals)
  (apply #'draw-line img y0 x0 y1 x1 vals)
  (apply #'draw-line img y1 x1 y2 x2 vals)
  (apply #'draw-line img y2 x2 y0 x0 vals))

(defun draw-triangle* (img y0 x0 y1 x1 y2 x2 list)
  (multiple-value-call #'draw-triangle img y0 x0 y1 x1 y2 x2 (values-list list)))

(defun draw-polygon (img points &rest vals)
  (loop for p across points
     do (let ((p1 (elt p 0))
              (p2 (elt p 1)))
          (when (and (consp p1) (consp p2))
            (apply #'draw-line img (car p1) (cdr p1) (car p2) (cdr p2) vals)))))

(defun draw-polygon* (img points list)
  (multiple-value-call #'draw-polygon img points (values-list list)))
