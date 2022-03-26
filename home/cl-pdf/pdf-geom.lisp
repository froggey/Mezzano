;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for details.
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)

;;; Geometry functions contributed by Eduardo Munoz

;;; Exported functions

(defconstant +2pi+ (* 2 pi))
(defconstant +pi/2+ (/ pi 2))

(defun arc (center-x center-y radius start extent)
  (move-to (+ center-x (* radius (cos start)))
           (+ center-y (* radius (sin start))))
  (arc-to center-x center-y radius start extent))

(defun pie (center-x center-y radius start extent)
  (move-to center-x center-y)
  (line-to (+ center-x (* radius (cos start)))
           (+ center-y (* radius (sin start))))
  (arc-to center-x center-y radius start extent)
  (line-to center-x center-y))

(defun circle (center-x center-y radius)
  (move-to (+ center-x radius) center-y)
  (arc-to center-x center-y radius 0 +2pi+))


(defun ellipse (center-x center-y radius-a radius-b)
  (move-to (+ center-x radius-a) center-y)
  (let ((kappa (* 4 (/ (- (sqrt 2) 1) 3.0))))
    (bezier-to (+ center-x radius-a) (+ center-y (* kappa radius-b))
               (+ center-x (* kappa radius-a)) (+ center-y radius-b)
               center-x (+ center-y radius-b))
    (bezier-to (- center-x (* kappa radius-a)) (+ center-y radius-b)
               (- center-x radius-a) (+ center-y (* kappa radius-b))
               (- center-x radius-a) center-y)
    (bezier-to (- center-x radius-a) (- center-y (* kappa radius-b))
               (- center-x (* kappa radius-a)) (- center-y radius-b)
               center-x (- center-y radius-b))
    (bezier-to (+ center-x (* kappa radius-a)) (- center-y radius-b)
               (+ center-x radius-a) (- center-y (* kappa radius-b))
               (+ center-x radius-a) center-y)))

(defun rectangle (x y dx dy &key (radius 0))
  (assert (or (and (listp radius) (= 4 (length radius))) (numberp radius)))
  (if (and (numberp radius) (zerop radius))
      (basic-rect x y dx dy)
      (progn
	(move-to (+ x dx) (- (+ y dy) (if (listp radius) (car radius) radius)))
	(polyline (list (list x y) (list (+ x dx) y)
			(list (+ x dx) (+ y dy)) (list x (+ y dy)))
		  :radius radius :closed t))))

(defun polyline (points &key (radius 0) (closed nil))
  (assert (or (and (listp radius) (= (length points) (length radius))) (numberp radius)))
  (cond ((listp radius)
	 (naive-polyline points :radius radius :closed closed))
	((zerop radius)
	 (destructuring-bind ((x1 y1) . other-points) points
	   (move-to x1 y1)
	   (loop for (x y) in other-points
		 do (line-to x y)
		 finally (when closed (line-to x1 y1)))))
	((numberp radius)
	 (naive-polyline points :radius (make-list (length points) :initial-element radius) :closed closed))))

(defun naive-polyline (points &key radius (closed nil))
  "Takes a list of points and radii and returns a curved list."
  (assert (and (listp radius) (= (length radius) (length points))))
  (let* ((break-point (midpoint (first points) (first (last points)) 0.5))
	 (pts (if closed `(,break-point ,@points ,break-point) points)))
    (destructuring-bind ((start-x start-y) ((end-x end-y))) (list (first pts) (last pts))
      (move-to start-x start-y)
      (loop for (p1 p2 p3) on pts
	    until (null p3)
	    for r in radius
	    do (fillet p2 p1 p3 r))
      (line-to end-x end-y))))

(defun regular-polygon (center-x center-y radius sides &key (fillet-radius 0))
  (polyline (loop with step-angle = (/ +2pi+ sides)
		  repeat sides
		  for current-angle from +pi/2+ by step-angle
		  collect (list (+ center-x (* radius (cos current-angle)))
				(+ center-y (* radius (sin current-angle)))))
	    :radius fillet-radius :closed t))

(defun star (center-x center-y ext-radius int-radius sides
             &key (fillet-radius 0))
  (let* ((current-angle +pi/2+)
         (step-angle (/ +2pi+ sides))
         (half-step (/ step-angle 2.0))
         (points '()))
    (dotimes (i sides)
      (push (list (+ center-x (* ext-radius (cos current-angle)))
                  (+ center-y (* ext-radius (sin current-angle))))
            points)
      (push (list (+ center-x (* int-radius (cos (+ current-angle half-step))))
                  (+ center-y (* int-radius (sin (+ current-angle half-step)))))
                  points)
      (setf current-angle (+ current-angle step-angle)))
    (polyline points :radius fillet-radius :closed t)))



;;; Non exported functions

(defun arc-to (center-x center-y radius start extent)
  ;; An arc of extent zero will generate an error at bezarc (divide by zero).
  ;; This case may be given by two aligned points in a polyline.
  ;; Better do nothing.
  (unless (zerop extent)
    (if (<= (abs extent) (/ pi 2.0))
        (multiple-value-bind (x1 y1 x2 y2 x3 y3)
            (bezarc center-x center-y radius start extent)
          (bezier-to x1 y1 x2 y2 x3 y3))
        (let ((half-extent (/ extent 2.0)))
          (arc-to center-x center-y radius start half-extent)
          (arc-to center-x center-y radius (+ start half-extent) half-extent)))))

(defun bezarc (center-x center-y radius start extent)
  ;; start and extent should be in radians.
  ;; Returns first-control-point-x first-control-point-y
  ;;         second-control-point-x second-control-point-y
  ;;         end-point-x end-point-y
  (let* ((end (+ start extent))
         (s-start (sin start)) (c-start (cos start))
         (s-end (sin end)) (c-end (cos end))
         (ang/2 (/ extent 2.0))
         (kappa (* (/ 4.0 3.0)
                   (/ (- 1 (cos ang/2))
                      (sin ang/2))))
	 (x1 (- c-start (* kappa s-start)))
	 (y1 (+ s-start (* kappa c-start)))
	 (x2 (+ c-end   (* kappa s-end)))
	 (y2 (- s-end   (* kappa c-end))))
    (values (+ (* x1 radius) center-x)(+ (* y1 radius) center-y)
	    (+ (* x2 radius) center-x)(+ (* y2 radius) center-y)
	    (+ (* c-end radius) center-x)(+ (* s-end radius) center-y))))


(defun distance (p1 p2)
  (sqrt (+ (expt (- (first p2)  (first p1))  2)
           (expt (- (second p2) (second p1)) 2))))

(defun angle (p1 p2)
  (if (zerop (distance p1 p2))
      0.0
      (atan (- (second p2) (second p1)) (- (first p2) (first p1)))))


;;;============================================================================;
;;;
;;; (angle-3points <point> <point> <point>)
;;;
;;; Devuelve el angulo en radianes entre tres puntos.  Se considera el punto
;;; 'pt1' como vertice del angulo.  El rango del angulo de salida es [+Pi -Pi)
;;;

(defun angle-3points (pt1 pt2 pt3)
  (let ((ang (- (angle pt1 pt3) (angle pt1 pt2))))
    (if	(or (> ang pi) (<= ang (- pi)))
        (- ang (* (signum ang) +2pi+))
        ang)))


;;;============================================================================;
;;;
;;; (midpoint <point> <point> <real>)
;;;
;;; Devuelve un punto situado entre los dos que se dan como argumento. El
;;; factor de posicion indica la relacion de las distancias entre los puntos
;;; de entrada y el de salida.
;;;

(defun midpoint (pt1 pt2 ratio)
  (let ((x1 (first pt1))(y1 (second pt1))
	(x2 (first pt2))(y2 (second pt2)))
    (list (+ x1 (* ratio (- x2 x1)))
          (+ y1 (* ratio (- y2 y1))))))


;; This function is the support to create rounded polylines
;;
;; p1 = corner
;; p2 = start
;; p3 = end
;; -> no usefull return value
(defun fillet (p1 p2 p3 radius)
  (let* ((gamma (/ (abs (angle-3points p1 p2 p3)) 2))
         (dist-p1-t (/ radius (tan gamma)))
         (dist-p1-s (/ (sqrt (+ (expt radius 2) (expt dist-p1-t 2)))
                       (cos gamma)))
         (dist-p1-p2 (distance p1 p2))
         (dist-p1-p3 (distance p1 p3)))
    (if (or (< dist-p1-p2 dist-p1-t)
            (< dist-p1-p3 dist-p1-t))
        ;; Radius is too large, so we aren't going to draw the arc.
        (line-to (first p1) (second p1))
        ;; Else, draw the arc.
        (let ((t2 (midpoint p1 p2 (/ dist-p1-t dist-p1-p2)))
              (t3 (midpoint p1 p3 (/ dist-p1-t dist-p1-p3)))
              (center (midpoint (midpoint p1 p2 (/ dist-p1-s dist-p1-p2))
                                (midpoint p1 p3 (/ dist-p1-s dist-p1-p3))
                                0.5)))
          (line-to (first t2) (second t2))
          (arc-to (first center) (second center) radius
                  (angle center t2) (angle-3points center t2 t3))))))
