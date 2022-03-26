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

;;; MOVING-OBJECT PROTOCOL:
;;;
;;; Provides an object with the ability to be dragged by the mouse.
;;;
;;; DRAG-OBJECT
;;;   Object follows the mouse until a click is seen.  This should work whether or
;;;   not an object keeps track of its position on state variables, though if it does
;;;   not, you must provide its initial position explicitly.
;;; DRAW-AT
;;;   Draw the object at given stream coordinates.  By default, this method calls
;;;   SET-STREAM-POSITION to update the state variables of the object so that it
;;;   knows its new position; then it calls DISPLAY, which takes only the object and
;;;   the stream as arguments and therefore must rely on state variables for
;;;   position.  If this is very expensive, you may want to override DRAW-AT with
;;;   code that makes some abbreviated drawing for the duration of mouse tracking. 
;;; ERASE-AT
;;;   Erase the object at given stream coordinates.

(defclass moving-object (essential-display-mixin) ()
  (:documentation
    "An abstract class that provides a displayable object with the ability
     to be dragged with the mouse."))

(defmethod stream-position ((self moving-object) stream)
  "Stream coordinates of object"
  (declare (ignore stream))
  (values 0 0))

(defmethod set-stream-position ((self moving-object) stream newx newy)
  "Set stream coordinates of object."
  (declare (ignore stream newx newy))
  nil)

(defmethod draw-at ((self moving-object) stream x y)
  (set-stream-position self stream x y)
  (display self stream))

(defmethod erase-at ((self moving-object) stream x y)
  (declare (ignore x y))
  ;; No need to set position here, it should be the same as the last draw.
  (erase self stream))

(defmethod drag-object ((self moving-object) STREAM &optional mouse-line initial-x initial-y)
  "Drag the object around by tracking the mouse."
  ;;(declare (values stream-x stream-y button))
  ;;
  ;; we never call this method with initial-x and initial-y, so
  ;; comment this out as it doesn't seem like
  ;; stream-set-pointer-position* is supported in current McCLIM
  ;;
  ;; (when (and initial-x initial-y)
  ;;   (stream-set-pointer-position* stream initial-x initial-y))
  (multiple-value-setq (initial-x initial-y)
    ;; Do this even if stream-set-pointer-position* was just called,
    ;; because that doesn't always seem to work (clim 1.0).  jpm.
    (stream-pointer-position* stream))
  (let ((stream-x initial-x)
	(stream-y initial-y))
    ;; Give object a chance to notice its initial position.
    (set-stream-position self stream stream-x stream-y)
    ;; Start tracking.
    (multiple-value-bind (button)
	(drag-icon stream
		   #'(lambda (str)
		       (draw-at self str stream-x stream-y))
		   #'(lambda (str)
		       (erase-at self str stream-x stream-y))
		   #'(lambda (dx dy)
		       (incf stream-x dx)
		       (incf stream-y dy))
		   mouse-line)
      (values stream-x stream-y button))))

;;; Need some stubs to put constraint checking.
(defmethod before-motion ((object t) (stream t)) nil)
(defmethod after-motion ((object t) (stream t)) nil)

(defmethod move ((self moving-object) STREAM)
  "Interactively move the object."
  (unwind-protect
      (progn
	(erase self stream)
	(before-motion self stream)
	(drag-object self stream)
	(after-motion self stream))
    (display self STREAM)))

(define-graph-command com-move-object ((object 'invisible-object) (window 'sheet))
  "Reposition an object, such as an annotation."
  (move object WINDOW))


(defclass moving-point (moving-object)
    ((x :initform 0 :initarg :x)
     (y :initform 0 :initarg :y))
  (:documentation
    "A point that can be dragged with the mouse.  A part of point annotations."))

(defmethod graph ((object moving-point))
  (error "A missing method GRAPH was detected."))

(defmethod xy-position ((object moving-point))
  (with-slots (x y) object (values x y)))

(defmethod set-xy-position ((object moving-point) newx newy &optional constrain-p)
  (declare (ignore constrain-p))
  (with-slots (x y) object (setq x newx y newy)))

(defmethod uv-position ((object moving-point))
  (multiple-value-bind (x y) (xy-position object)
    (xy-to-uv (graph object) x y)))

(defmethod set-uv-position ((self moving-point) x y &optional constrain-p)
  (declare (ignore constrain-p))
  (multiple-value-setq (x y) (uv-to-xy (graph self) x y))
  (set-xy-position self x y))

(defmethod set-stream-position ((self moving-point) stream x y)
  (multiple-value-setq (x y) (screen-to-uv stream x y))
  (multiple-value-setq (x y) (uv-to-xy (graph self) x y))
  (set-xy-position self x y))

(defmethod stream-position ((self moving-point) stream)
  (multiple-value-bind (x y) (xy-position self)
    (multiple-value-setq (x y) (xy-to-uv (graph self) x y))
    (uv-to-screen stream x y)))

(defclass moving-polygon (moving-object)
    ((corners :initform nil :initarg :corners :accessor corners)
     (motion-corner :initform nil :accessor motion-corner))
  (:documentation
    "A polygon whose corners can be moved around with the mouse.
     The instance variable MOTION-CORNER caches the chosen corner.
     A part of region annotations."))

(defmethod moveable-polygon-p ((object t)) nil)
(defmethod moveable-polygon-p ((object moving-polygon)) t)

(defmethod graph ((object moving-polygon))
  (error "A missing method GRAPH was detected."))

(defmethod draw-xy-polygon ((object moving-polygon) stream &optional (alu %draw))
  (let ((graph (graph object)))
    (with-clipping-to-graph (graph stream t)
      (map-polygon-edges
	#'(lambda (x1 y1 x2 y2)
	    (multiple-value-setq (x1 y1) (xy-to-uv graph x1 y1))
	    (multiple-value-setq (x2 y2) (xy-to-uv graph x2 y2))
	    (device-draw-line stream x1 y1 x2 y2 :alu alu))
	(corners object)))))

(defmethod set-stream-position ((self moving-polygon) stream x y)
  (multiple-value-setq (x y) (screen-to-uv stream x y))
  (multiple-value-setq (x y) (uv-to-xy (graph self) x y))
  (let ((corner (motion-corner self)))
    (setf (first corner) x (second corner) y)))

(defmethod stream-position ((self moving-polygon) stream)
  (let ((corner (motion-corner self)))
    (multiple-value-bind (u v)
	(xy-to-uv (graph self) (first corner) (second corner))
      (uv-to-screen stream u v))))

(defun closest-point (u v mapper points)
  "Map over a set of points to find the nearest one."
  ;; Don't care about coordinate systems here, that is the caller's problem.
  ;;(declare (values closest-point closest-u closest-v))
  (macrolet ((distance (a0 b0 a1 b1)
	       ;; Omit the sqrt as an efficiency hack.
	       `(let ((a (- ,a0 ,a1))
		      (b (- ,b0 ,b1)))
		  (+ (* a a) (* b b)))))
    (let (d0 u0 v0 datum0)
      (funcall mapper
	       #'(lambda (u1 v1 datum1)
		   (let ((d (distance u1 v1 u v)))
		     (when (or (not d0) (< d d0))
		       (setq d0 d u0 u1 v0 v1 datum0 datum1))))
	       points)
      (values datum0 u0 v0))))

(defmethod cache-moving-corner ((self moving-polygon) stream stream-x stream-y)
  (when (and stream-x stream-y)
    (setq stream-x (truncate stream-x))
    (setq stream-y (truncate stream-y))
    (let ((graph (graph self)))
      (multiple-value-bind (x y) (screen-to-uv stream stream-x stream-y)
	(setf (motion-corner self)
	      (closest-point
		x y
		#'(lambda (function points)
		    (dolist (point points)
		      (let ((x0 (car point)) (y0 (cadr point)))
			(multiple-value-setq (x0 y0) (xy-to-uv graph x0 y0))
			(funcall function x0 y0 point))))
		(corners self)))))))


(defclass basic-slider (moving-object)
    ((bounds :initform nil :initarg :bounds)	;limit range by graph values (!NIL)
     (graphs :initform nil :accessor slider-graphs))
  (:documentation "Base class for crosshairs and rectangle-sliders"))

(defmethod BOUNDS ((self basic-slider))
  (with-slots (bounds) self
    (multiple-value-bind (x-min y-min x-max y-max)
	(values-list bounds)
      (let ((graph (car (slider-graphs self))))
	(when graph
	  (multiple-value-bind (left right bottom top)
	      (xy-inside graph)
	    (setq x-min (or x-min left)
		  y-min (or y-min bottom)
		  x-max (or x-max right)
		  y-max (or y-max top))))
	(values x-min y-min x-max y-max)))))

(defmethod SET-BOUNDS ((self basic-slider) &optional x-min y-min x-max y-max)
  ;; Values are in graph coordinates -- slider will be limited to this range,
  ;; in spite of the actual limits in the graph itself.
  (setf (slot-value self 'bounds)
	(when (or x-min x-max y-min y-max)
	  `(,x-min ,y-min ,x-max ,y-max))))

(defmethod drag-object :before ((self basic-slider) stream &optional mouse-line x y)
  (declare (ignore stream mouse-line x y))
  ;; recache boundaries.
  (set-bounds self)
  (multiple-value-bind (a b c d) (bounds self)
    (set-bounds self a b c d)))

(defmethod redraw-overlapping-sliders ((self basic-slider) STREAM)
  ;; Not smart enough to really find overlapping ones;
  ;; instead returns all sliders which appear on same graph with this one.
  (dolist (slider (delete self (reduce #'union
				       (loop for graph in (slider-graphs self)
					     collect (sliders graph)))))
    (erase slider STREAM)
    (draw-without-labels slider STREAM)))

(defmethod SLIDER-REMOVE-INTERNAL ((self basic-slider) STREAM)
  (dolist (g (slider-graphs self)) (remove-slider g STREAM self)))

(defmethod KILL ((self basic-slider) STREAM)
  (erase self STREAM)
  (redraw-overlapping-sliders self STREAM)
  (slider-remove-internal self STREAM))

(defmethod set-stream-position ((self basic-slider) stream newx newy)
  "Set stream coordinates of object."
  (multiple-value-setq (newx newy) (screen-to-uv stream newx newy))
  (multiple-value-setq (newx newy) (uv-to-xy (car (slider-graphs self)) newx newy))
  (setf (slider-x self) newx (slider-y self) newy)
  (multiple-value-bind (left bottom right top) (bounds self)
    (constrain-position self left bottom right top))
  t)

(defmethod CONSTRAIN-POSITION ((self basic-slider) left bottom right top)
  "Try to constrain X,Y to be within the bounds."
  (multiple-value-bind (x1 y1)
      (constrain-point-in-rectangle (slider-x self) (slider-y self)
				    left bottom right top)
    (setf (slider-x self) x1 (slider-y self) y1)
    (values x1 y1)))

(defmethod TRACK ((self basic-slider) WINDOW &optional mouse-line)
  (multiple-value-bind (x y button)
      (drag-object self WINDOW mouse-line)
    (multiple-value-setq (x y) (screen-to-uv window x y))
    (multiple-value-setq (x y) (uv-to-xy (car (slider-graphs self)) x y))
    (values x y button)))


(defclass crosshairs (basic-slider)
    ((x :initform 0.0 :accessor slider-x)	;current coordinates in first graph
     (y :initform 0.0 :accessor slider-y)
     (x-label :initform nil :accessor x-label)
     (y-label :initform nil :accessor y-label)
     (x? :initform t :accessor slider-x?)
     (y? :initform t :accessor slider-y?)))

(defmethod stream-position ((self crosshairs) stream)
  (let ((x (slider-x self))
	(y (slider-y self)))
    (multiple-value-setq (x y) (xy-to-uv (car (slider-graphs self)) x y))
    (uv-to-screen stream x y)))

(defmethod compute-labels ((self crosshairs))
  (setf (x-label self) (float-to-string (slider-x self) *max-digits* (x-label self)))
  (setf (y-label self) (float-to-string (slider-y self) *max-digits* (y-label self))))

(defmethod set-stream-position :after ((self crosshairs) stream newx newy)
  (declare (ignore stream newx newy))
  (compute-labels self))

(defmethod DRAW-CROSSHAIRS ((self crosshairs) stream graph x y alu)
  "Draw the horizontal and vertical lines but no labels"
  (multiple-value-bind (ull uur vll vur) (uv-inside graph)
    (multiple-value-setq (x y) (xy-to-uv graph x y))
    (multiple-value-setq (ull vll) (uv-to-screen stream ull vll))
    (multiple-value-setq (uur vur) (uv-to-screen stream uur vur))
    (multiple-value-setq (x y) (uv-to-screen stream x y))
    (when (slider-y? self) (draw-line x vll x vur :stream stream :alu alu))
    (when (slider-x? self) (draw-line ull y uur y :stream stream :alu alu))))

(defmethod DRAW-SLIDER-X-LABEL ((self crosshairs) graph STREAM alu value text)
  (or text (setq text (float-to-string value (x-digits graph))))
  (multiple-value-bind (u v) (xy-to-uv graph value (yll graph))
    (multiple-value-setq (u v) (uv-to-screen stream u v))
    (incf v (* 2 (stream-line-height stream)))
    (draw-string text u v :stream stream :alu alu)))

(defmethod DRAW-SLIDER-Y-LABEL ((self crosshairs) graph STREAM alu value text)
  (or text (setq text (float-to-string value (y-digits graph))))
  (multiple-value-bind (u v) (xy-to-uv graph (xll graph) value)
    (multiple-value-setq (u v) (uv-to-screen stream u v))
    (setq u (max 0 (- u (string-size stream nil text))))
    (draw-string text u v :stream stream :alu alu)))

(defmethod DRAW-INTERNAL ((self crosshairs) STREAM x y &optional (labels? t))
  ;; Only draw labels on the "head" graph -- seemed to take to long otherwise (RBR)
  (with-slots (graphs) self
    (let ((alu %flip))
      (dolist (g graphs)
	(draw-crosshairs self stream g x y alu))
      (when labels?
	(let ((graph (car graphs)))
	  (when (slider-x? self)
	    (draw-slider-x-label self graph STREAM alu x (x-label self)))
	  (when (slider-y? self)
	    (draw-slider-y-label self graph STREAM alu y (y-label self))))))))

(defmethod DISPLAY ((self crosshairs) STREAM)
  (draw-internal self STREAM (slider-x self) (slider-y self) t))

(defmethod DRAW-WITHOUT-LABELS ((self crosshairs) STREAM)
  (draw-internal self STREAM (slider-x self) (slider-y self) nil))

(defmethod ERASE ((self crosshairs) STREAM)
  (draw-internal self STREAM (slider-x self) (slider-y self) t))


(defclass rectangle-slider (basic-slider)
    ((dx :initform 0 :initarg :dx :accessor rectangle-slider-dx)
     (dy :initform 0 :initarg :dx :accessor rectangle-slider-dy)
     (du :initform 0 :initarg :du :accessor rectangle-slider-du)
     (dv :initform 0 :initarg :dv :accessor rectangle-slider-dv)
     (slider1 :initform (make-instance 'crosshairs) :accessor rectangle-slider-1)
     (slider2 :initform (make-instance 'crosshairs) :accessor rectangle-slider-2))
  (:documentation
    "Two cross hairs that can move as a unit."))

(defmethod display ((self rectangle-slider) stream)
  (display (rectangle-slider-1 self) stream)
  (display (rectangle-slider-2 self) stream))

(defmethod erase ((self rectangle-slider) stream)
  (erase (rectangle-slider-1 self) stream)
  (erase (rectangle-slider-2 self) stream))

(defmethod stream-position ((self rectangle-slider) stream)
  (stream-position (rectangle-slider-1 self) stream))

(defmethod set-stream-position ((self rectangle-slider) stream newx newy)
  (set-stream-position (rectangle-slider-1 self) stream newx newy)
  (set-stream-position (rectangle-slider-2 self) stream
		       (+ newx (rectangle-slider-du self))
		       (+ newy (rectangle-slider-dv self)))
  (multiple-value-bind (left bottom right top) (bounds self)
    (constrain-position self left bottom right top)))

(defmethod CONSTRAIN-POSITION ((self rectangle-slider) left bottom right top)
  ;; Try to constrain X,Y, DX, and DY to be within the bounds.
  (with-slots (slider1 slider2 dx dy) self
    (cond ((and (minusp dx) (minusp dy))	
	   (constrain-position slider1 (- left dx) (- bottom dy) right top)
	   (constrain-position slider2 left bottom (+ right dx) (+ top dy)))
	  ((and (plusp dx) (plusp dy))
	   (constrain-position slider1 left bottom (- right dx) (- top dy))
	   (constrain-position slider2 (+ left dx) (+ bottom dy) right top))
	  ((and (plusp dx) (minusp dy))
	   (constrain-position slider1 left (- bottom dy) (- right dx) top)
	   (constrain-position slider2 (+ left dx) bottom right (+ top dy)))
	  (t
	   (constrain-position slider1 (- left dx) bottom right (- top dy))
	   (constrain-position slider2 left (+ bottom dy) (+ right dx) top)))))

(defmethod set-rectangle-size ((self rectangle-slider) stream)
  (with-slots (dx dy du dv slider1 slider2) self
    (let ((x1 (slider-x slider1))
	  (y1 (slider-y slider1))
	  (x2 (slider-x slider2))
	  (y2 (slider-y slider2)))
      (setq dx (- x2 x1) dy (- y2 y1))
      (multiple-value-setq (x1 y1) (xy-to-uv (car (slider-graphs self)) x1 y1))
      (multiple-value-setq (x2 y2) (xy-to-uv (car (slider-graphs self)) x2 y2))
      (multiple-value-setq (x1 y1) (uv-to-screen stream x1 y1))
      (multiple-value-setq (x2 y2) (uv-to-screen stream x2 y2))
      (setq du (- x2 x1) dv (- y2 y1)))))

(defmethod slider-graphs ((self rectangle-slider))
  (slider-graphs (rectangle-slider-1 self)))

(defmethod (setf slider-graphs) (new (self rectangle-slider))
  (setf (slider-graphs (rectangle-slider-1 self)) new)
  (setf (slider-graphs (rectangle-slider-2 self)) new))

(defmethod TRACK :around ((self rectangle-slider) WINDOW &optional mouse-line)
  window mouse-line
  (multiple-value-bind (a b c) (call-next-method)
    (values a b c
	    (slider-x (rectangle-slider-2 self))
	    (slider-y (rectangle-slider-2 self)))))


(defvar *slider* (make-instance 'crosshairs))
(defvar *rectangle-slider* (make-instance 'rectangle-slider))

(defun define-point (graph stream &key (x 0) (y 0) (slider *slider*))
  "Use the slider to choose a point off the graph."
  ;;(declare (values x y))
  (setf (slider-graphs slider) (graphs-for-slider graph))
  (setf (slider-x slider) x (slider-y slider) y)
  (multiple-value-bind (x1 y1 button)
      (track slider STREAM "L,R:Fix point, M:Abort.")
    (button-case button
		 :left (values x1 y1)
		 :right (values x1 y1)
		 :middle nil)))

(defun define-region (graph stream &key
		      (x1 0) (y1 0)
		      (x2 0) (y2 0)
		      (slider *rectangle-slider*))
  "Use the slider to choose two points defining the edges of a box."
  ;;(declare (values (x1 y1 x2 y2)))
  (let ((slider1 (rectangle-slider-1 slider))
	(slider2 (rectangle-slider-2 slider)))
    (setf (slider-graphs slider) (graphs-for-slider graph))
    (setf (slider-x slider1) x1 (slider-y slider1) y1)
    (setf (slider-x slider2) x2 (slider-y slider2) y2)
    (let ((button nil)
	  (string1 "L,R:Fix and continue, M:Abort")
	  (string2 "L:Fix other edge and finish, M:Abort, R:Fix other edge and continue.")
	  (string3 "L,R:Fix and finish, M:Abort."))
      (when
	(catch 'abort
	  (labels ((DRAG-THE-BOX ()
		     (set-rectangle-size slider stream)
		     (multiple-value-setq (x1 y1 button x2 y2)
		       (track slider STREAM string3))
		     (button-case button :middle (throw 'abort nil)))
		   (CHOOSE-THE-SECOND-POINT ()
		     (multiple-value-setq (x2 y2 button)
		       (with-output-recording-disabled (stream)
			 (unwind-protect
			     (progn (display slider1 stream)
				    (track slider2 stream string2))
			   (erase slider1 stream))))
		     (button-case
		       button
		       :middle (throw 'abort nil)
		       :left t
		       :right (drag-the-box)))
		   (CHOOSE-THE-FIRST-POINT ()
		     (multiple-value-setq (x1 y1 button)
		       (track slider1 STREAM string1))
		     (button-case
		       button
		       :right (choose-the-second-point)
		       :middle (throw 'abort nil)
		       :left (choose-the-second-point))))
	    (choose-the-first-point)
	    t))
	(values x1 y1 x2 y2)))))



(defclass graph-slider-mixin
	  (graph-mouse-resolution-mixin basic-graph)
  ((sliders :initform nil :initarg :sliders
	    :accessor sliders) ;List of sliders on this graph.

   ;; X and y labeling resolution; Uses dx/dy-mouse if !given.
   (dx-slider :initform nil :initarg :dx-slider) 
   (dy-slider :initform nil :initarg :dy-slider))
  )

(defmethod after-fasd ((self graph-slider-mixin))
  `(set-sliders ',self ',(sliders self)))

(defmethod add-slider ((self graph-slider-mixin) (STREAM t) slider)
  (pushnew slider (sliders self)))

(defmethod REMOVE-SLIDER ((self graph-slider-mixin) (STREAM t) slider)
  (setf (sliders self) (delete slider (sliders self))))

(defmethod REMOVE-ALL-SLIDERS ((self graph-slider-mixin) STREAM)
  (dolist (s (sliders self)) (remove-slider self STREAM s)))

(defmethod KILL-ALL-SLIDERS ((self graph-slider-mixin) STREAM)
  (dolist (s (sliders self)) 
    (kill s STREAM)
    (remove-slider self STREAM s)))

;;; Erase and draw sliders along with the graph itself.

(defmethod ERASE :before ((self graph-slider-mixin) STREAM)
  (dolist (s (sliders self)) (erase s STREAM)))

;;; !KRA:  This has a bug because slider may not be drawn correctly if
;;; on multiple windows after a :REFRESH, say.
(defmethod display :after ((self graph-slider-mixin) STREAM)
  (dolist (s (sliders self)) (display s STREAM)))

(defclass GRAPH-SLIDER-INTERACTION-MIXIN
	  (graph-slider-mixin graph-mouse-resolution-mixin basic-graph)
    ()
  (:documentation
   "Provides graphs with a protocol for interacting with a slider
    through the SLIDER-INTERACT and SLIDER-SETUP methods."))

(defmethod graphs-for-slider ((self graph-slider-interaction-mixin)) (list self))

(defmethod slider-setup ((self graph-slider-interaction-mixin) slider)
  ;; Set up a slider any way you like.
  (setf (slider-graphs slider) (graphs-for-slider self)))

(defmethod slider-interact ((self graph-slider-interaction-mixin) STREAM
			    x y &optional (point-p nil))
  ;; Interact with a slider, initially at (x y) to define a point if
  ;; POINT-P or a  region if (NOT POINT-P). 
  ;; Returns values X1 Y1 X2 Y2.
  (if point-p
      (define-point self STREAM :x x :y y)
      (define-region self STREAM :x1 x :y1 y :x2 x :y2 y)))

(defmethod describe-point ((self graph-slider-interaction-mixin) x y)
  ;; No op.  You may want to redefine this.
  (declare (ignore x y))
  nil)


(defclass GRAPH-ZOOM-MIXIN
	  (graph-slider-interaction-mixin basic-graph)
    ((zoom-stack :initform ()
		 :accessor zoom-stack :initarg :zoom-stack))
  (:documentation "Lets you ZOOM in on an area of the graph."))

(defmethod do-auto-scale :around ((self graph-zoom-mixin))
  ;; Don't auto-scale when zoomed in.
  (unless (zoom-stack self) (call-next-method)))

(defmethod choose-xy-rectangle (graph stream)
  (multiple-value-bind (left top right bottom button)
      (device-specify-rectangle stream)
    (when left
      (multiple-value-setq (left top) (uv-to-xy graph left top))
      (multiple-value-setq (right bottom) (uv-to-xy graph right bottom))
      (values (min left right) (max top bottom)
	      (max left right) (min top bottom)
	      button))))

(defmethod zoom-in ((self graph-zoom-mixin) STREAM)
  (with-slots (xll xur yll yur zoom-stack) self
    (multiple-value-bind (x1 y1 x2 y2) (choose-xy-rectangle self stream)
      (when (and x1 y1 x2 y2
		 (not (= x1 x2)) (not (= y1 y2)))
	(push (list xll xur yll yur) zoom-stack)
	(if (< x2 x1) (psetq x1 x2 x2 x1))
	(if (< y2 y1) (psetq y1 y2 y2 y1))
	(set-xy-inside self STREAM x1 x2 y1 y2)
	(refresh self STREAM)))))

(defmethod zoom-out ((self graph-zoom-mixin) STREAM)
  (with-slots (zoom-stack) self
    (when zoom-stack
      (apply #'set-xy-inside self STREAM (pop zoom-stack)))
    (refresh self STREAM)))
