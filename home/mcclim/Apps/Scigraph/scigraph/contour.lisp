;;; -*- Syntax: Common-lisp; Package: GRAPH -*-
#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(in-package :graph)

;;;
;;; CONTOUR
;;;

#||
;;; Examples:

(defun surface (x y)
  (* (sqrt (abs (* (- x 15) (abs (- y 35)) (- x 25) (- y 20)))) 1e-1))

(defun try-contour (&optional (stream *standard-output*))
  (flet
    ((plotter
       (x1 y1 x2 y2 level stream)
       (let ((scale 10))			; Pixels per cell
	 (draw-line (* x1 scale) (* y1 scale) (* x2 scale) (* y2 scale)
		    :stream stream
		    :thickness (if (zerop (mod level 10)) 2 0)
		    )))

     (map-levels
       (min max function)
       (let ((dlevel 2))
	 (do ((level (* dlevel (values (round min dlevel))) (+ level dlevel)))
	     ((> level max))
	   (funcall function level)))))
    (with-output-recording-disabled (stream)
      #+genera
      (graphics:with-room-for-graphics (stream 500)
	(basic-contour #'surface #'plotter #'map-levels stream :x-max 50 :y-max 50
		       :dx 1.0 :dy 1.0))
      #-genera
      (basic-contour #'surface #'plotter #'map-levels stream :x-max 50 :y-max 50
		     :dx 1.0 :dy 1.0))))

(defun make-contour-demo-frame ()
  (let* ((cd (make-instance 'contour-data :contour-surface #'surface))
	 (cg (make-instance 'contour-graph :datasets (list cd)
			    :auto-scale nil)))
    (add-dataset cg cd)
    (set-xy-inside cg nil 0.0 50.0 0.0 50.0)
    (view-graphs (list cg) :title "Contour" :left 0 :bottom 0
			 :width 600 :height 400 :create t
			 :wait-until-done nil)))

||#



(defun CONTOUR-RANGE (surface x-min dx x-max y-min dy y-max)
  ;; Returns the minimum and maximum values of (SURFACE x y),
  ;; x-min <= x <= xmax, y-min <= y, y-min.
  (do* ((min (funcall surface x-min y-min))
	(max min)
	(x x-min (+ x dx)))
       ((> x x-max) (values min max))
    (do ((y y-min (+ y dy)))
	((> y y-max))
      (let ((f (funcall surface x y)))
	(when f
	  (if (< f min) (setq min f)
	      (if (> f max) (setq max f))))))))

(defun MAJOR-MINOR-INTERVALS (min max)
  ;; Choose a nice major and minor contour interval
  (let* ((major-interval (auto-tick min max))
	 (minor-interval (auto-tick 0.0 major-interval)))
    (values major-interval minor-interval)))

(defun BASIC-CONTOUR (surface plotter map-levels stream
			      &key (x-min 0) x-max (y-min 0) y-max
			           (dx 1.0) (dy 1.0))
  ;; Contour display of a surface.
  ;; SURFACE is a function of (X Y) that returns the surface value at (X Y).
  ;; PLOTTER is a function of (X1 Y1 X2 Y2 LEVEL STREAM) that should draw a line.
  ;; MAP-LEVELS is a function of (MIN MAX FUNCTION) that should apply FUNCTION to all
  ;; contour levels between MIN and MAX.

  ;; See Example TRY-CONTOUR.
  (let ((x x-min)
	(y y-min)
	x2 y2
	ll lr ul ur)
    (flet
      ((contour-one-cell 
	  (level)
	 ;; Plot the contour segments for a cell at x y.
	 ;; UL---UR  Consider all four bits, (level < ul,ur,lr,lr) = num
	 ;; |    |   Then num and (not num) have same contours. Therefore, 
	 ;; LL---LR  colapse num and (not num) into the 1<= 7.
	 (macrolet
	   ((plot (dx1 dy1 dx2 dy2)
	      `(funcall plotter
			(+ x (* dx ,dx1)) (+ y (* dy ,dy1))
			(+ x (* dx ,dx2)) (+ y (* dy ,dy2)) level stream)))
	   (let ((num (logior (if (< ul level) 8 0) (if (< ur level) 4 0)
			      (if (< lr level) 2 0) (if (< ll level) 1 0)))
		 xb yb xr yr xt yt xl yl)
	     (if (> num 7) (setq num (- 15 num)))
	     (when (not (= num 0))
	       (case
		 num
		 (1 (setq xb (/ (- level ll) (- lr ll)) yb 0.0)
		    (setq xl 0.0 yl (/ (- level ll) (- ul ll)))
		    (plot xb yb xl yl))
		 (2 (setq xb (/ (- level ll) (- lr ll)) yb 0.0)
		    (setq xr 1.0 yr (/ (- level lr) (- ur lr)))
		    (plot xb yb xr yr))
		 (3 (setq xr 1.0 yr (/ (- level lr) (- ur lr)))
		    (setq xl 0.0 yl (/ (- level ll) (- ul ll)))
		    (plot xl yl xr yr))
		 (4 (setq xr 1.0 yr (/ (- level lr) (- ur lr)))
		    (setq xt (/ (- level ul) (- ur ul)) yt 1.0)
		    (plot xr yr xt yt))
		 (5 (setq xb (/ (- level ll) (- lr ll)) yb 0.0)
		    (setq xr 1.0 yr (/ (- level lr) (- ur lr)))
		    (setq xt (/ (- level ul) (- ur ul)) yt 1.0)
		    (setq xl 0.0 yl (/ (- level ll) (- ul ll)))
		    (plot xb yb xt yt)
		    (plot xl yl xr yr))
		 (6 (setq xb (/ (- level ll) (- lr ll)) yb 0.0)
		    (setq xt (/ (- level ul) (- ur ul)) yt 1.0)
		    (plot xb yb xt yt))
		 (7 (setq xt (/ (- level ul) (- ur ul)) yt 1.0)
		    (setq xl 0.0 yl (/ (- level ll) (- ul ll)))
		    (plot xl yl xt yt))))))
	 ))
      (do ()
	  ((> y y-max))
	(setq x x-min
	      y2 (+ y dy)
	      ll (funcall surface x y)
	      ul (funcall surface x y2))
	(do ()
	    ((> x x-max))
	  (setq x2 (+ x dx)
		lr (funcall surface x2 y)
		ur (funcall surface x2 y2))
	  (when (and ll ul lr ur)
	    (funcall map-levels (min ul ur ll lr) (max ul ur ll lr)
		     #'contour-one-cell))
	  (setq x x2 ll lr ul ur))
	(setq y y2)))))
		      


(defclass CONTOUR-DATA (graph-data)
  (;; Surface value function.
   (contour-surface :initform nil :initarg :contour-surface :accessor contour-surface)
   ;; Spacing of grid points... default is 20 pixels apart.
   (contour-dx      :initform nil :initarg :contour-dx      :accessor contour-dx)
   (contour-dy      :initform nil :initarg :contour-dy      :accessor contour-dy))
  ;; Contour-data does not provide scale limits by default, because it gets its limits
  ;; from the graph.  We don't have a good legend yet.
  (:default-initargs :auto-scale? nil :show-legend nil :present-self-p nil)
  (:documentation "Contour map of a 2-D surface."))
  
(defmethod surface-value ((self contour-data) x y)
  (with-slots (contour-surface) self
    (funcall contour-surface x y)))

(defmethod contour-setup ((self contour-data) graph)
  (with-slots (contour-surface contour-dx contour-dy) self
    (multiple-value-bind (left right bottom top)
	(xy-inside graph)
      (let ((x-u-scale (x-u-scale graph))
	    (y-v-scale (y-v-scale graph)))
	(values contour-surface
		left
		(or contour-dx (/ 20 x-u-scale))
		right
		bottom
		(or contour-dy (/ 20 y-v-scale))
		top)))))

(defmethod draw-contours ((self contour-data) stream graph)
  (multiple-value-bind (surface x-min dx x-max y-min dy y-max)
      (contour-setup self graph)
    (setq x-min (float x-min 0.0)
	  dx (float dx 0.0)
	  x-max (float x-max 0.0)
	  y-min (float y-min 0.0)
	  dy (float dy 0.0)
	  y-max (float y-max 0.0))
    (multiple-value-bind (level-min level-max)
	(contour-range surface x-min dx x-max y-min dy y-max)
      (multiple-value-bind (major-interval minor-interval)
	  (major-minor-intervals level-min level-max)
	(with-slots (alu) self
	  (flet ((plotter (x1 y1 x2 y2 level the-graph)
		   (xy-draw-line the-graph stream x1 y1 x2 y2
				 :alu alu
				 :thickness (if (< (abs (mod level major-interval))
						   (* .01 major-interval))
						3 0)))
		 (map-levels (min max function)
		   (let ((dlevel minor-interval))
		     (do ((level (* dlevel (values (round min dlevel))) (+ level dlevel)))
			 ((> level max))
		       (funcall function level)))))
	    (basic-contour surface #'plotter #'map-levels graph
			   :x-min x-min :dx dx :x-max x-max
			   :y-min y-min :dy dy :y-max y-max)))))))

(defmethod DISPLAY-DATA ((self CONTOUR-DATA) stream graph)
  (draw-contours self stream graph))


(defclass CONTOUR-GRAPH-MIXIN (basic-graph)
  ()
  (:documentation
   "Let the slider of a graph with contour data on it, display the Z value."))

(defmethod surface-value ((self contour-graph-mixin) x y)
  (surface-value (first (datasets self)) x y))

(defmethod slider-z-label-string ((self contour-graph-mixin) x-value y-value)
  "Assume the first dataset is the contour-data."
  (float-to-string (surface-value self x-value y-value)))

(defmethod draw-slider-x-label :after ((self crosshairs)
				       (graph CONTOUR-GRAPH-MIXIN)
				       stream alu value text)
  (declare (ignore text))
  (let ((string (slider-z-label-string graph (slider-x self) (slider-y self))))
    (multiple-value-bind (u v) (xy-to-uv graph value (yll graph))
      (multiple-value-setq (u v) (uv-to-screen stream u v))
      (incf v (* 3 (stream-line-height stream)))
      (draw-string string u v :stream stream :alu alu))))

(defclass contour-graph (contour-graph-mixin graph)
  ())


