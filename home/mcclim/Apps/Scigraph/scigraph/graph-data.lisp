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

#|
Graphs apply the following functions to datasets:

DISPLAY-DATA
  MAP-DATA
  DATUM-POSITION
  DATUM-DISPLAYER
    DATUM-STYLE-DISPLAYER
AUTO-SCALE-LIMITS
  MAP-DATA-XY
    DATUM-POSITION
SHOW-LEGEND
DISPLAY-LEGEND-DATASET
  this has a subprotocol, see legend.lisp.
TITLE
X-LABEL
Y-LABEL

|#

;;; MT - put the DATA into a separate mixin, for ease of recombining the classes
(defclass RAW-GRAPH-DATA ()
  ((data :initform () :initarg :data :accessor data))) ; data to display

(defclass BASIC-GRAPH-DATA (named-mixin)
  (;; The alu is basically a (relatively) internal representation of a color so the
   ;; value here is implementation specific.
   ;; On LISPM it is either a number like tv:alu-xor, a keyword like :DRAW
   ;; In CLIM, it is an ink.
   (alu :initform %alu :initarg :alu :accessor alu)))

(declare-required-method display-data (SELF STREAM graph)) ; For drawing
(declare-required-method name (SELF))	; For presenting

;;; Define these guys just to simplify the protocol requirements.
(defmethod data ((thing t)) nil)
(defmethod map-data-xy ((dataset t) (function t) (data t)) nil)
(defmethod x-label ((thing t)) nil)
(defmethod y-label ((thing t)) nil)
(defmethod title ((thing t)) nil)
(defmethod rescale ((thing t)) nil)


#|
The following classes provide a protocol for manipulating data in a
generic way that are used by other classes, for autoscaling etc.  When
defining new types of data classes, either use the generic interface,
or override the methods that use them. 

PROTOCOL:
  (MAP-DATA function) - Map function over each datum.
  (MAP-DATA-XY function) - Map function over each xy pair.

|#

(defclass ESSENTIAL-GRAPH-DATA-MAP-MIXIN ()
  ()
  (:documentation
    "Generic protocol for mapping over data elements.
     See BASIC-GRAPH-DATUM-SYMBOLOGY-MIXIN"))

(defmethod map-data ((dataset t) function (data sequence))
  "Map FUNCTION over each datum."
  (map nil function data))

(defmethod map-data-xy ((dataset ESSENTIAL-GRAPH-DATA-MAP-MIXIN) function data)
  "Map function over each x y pair."
  (map-data dataset
	    #'(lambda (datum)
		(multiple-value-bind (x y)
		    (datum-position dataset datum)
		  (funcall function x y)))
	    data))

(defmacro with-alu ((stream alu) &body body)
  `(with-drawing-options (,stream :ink ,alu) ,@body))

(defmethod display-data ((self essential-graph-data-map-mixin) STREAM graph)
  "Display the data on graph GRAPH using DATUM-DISPLAYER."
  (with-alu (stream (alu self))
    ;; Fixup the alu just once, since its the same for every datum.
    (let ((displayer (datum-displayer self graph))
	  (H (stream-height stream))
	  (Trans (xy-to-uv-transform graph)))
      (declare (compiled-function displayer Trans)
	       (fixnum H))
      (map-data self #'(lambda (datum)
			 (multiple-value-bind (x y) (datum-position self datum)
			   (multiple-value-setq (x y) (funcall trans x y))
			   (setq y (- H (the fixnum y)))
			   (funcall displayer stream x y datum)
			   ;; Forcing the x buffer is nice here, but it is
			   ;; extremely expensive.  It slows drawing by 4x.
			   ;;  (force-output stream)
                           ))
		(data self)))))

(defmethod datum-displayer ((self essential-graph-data-map-mixin) graph)
  "Returns a function that expects a stream, UV coordinates of next datum,
   and the datum itself."
  (let ((displayers nil))
    (dolist (symbology (symbologies self))
      (let ((d (datum-style-displayer self graph symbology)))
	(when d (push d displayers))))
    (if (cdr displayers)
	#'(lambda (stream u v datum)
	    (dolist (d displayers) (funcall (the compiled-function d) stream u v datum)))
	(car displayers))))

(defmethod datum-position ((dataset t) (datum list))
  "Extract the x,y coordinates of this datum."
  (values (first datum) (second datum)))


(defclass ACCESSOR-DATUM-MIXIN () 
  ((symbol-height)
   (x-accessor :accessor x-accessor :initform #'first :initarg :x-accessor)
   (y-accessor :accessor y-accessor :initform #'second :initarg :y-accessor))
  (:documentation
    "Provides DATUM-POSITION based on accessor functions."))

(defmethod datum-position ((self accessor-datum-mixin) datum)
  "Returns the actual X Y to plot.  It should:
   1. decode the datum to determine its X and Y, and 
   2. use :XY-TO-PLOT to determine the actual values to be plotted."
  (values (funcall (the compiled-function (x-accessor self)) datum)
	  (funcall (the compiled-function (y-accessor self)) datum)))


(defclass GRAPH-DATA-X-OFFSET-MIXIN ()
  ((x-offset :initform nil :initarg :x-offset :accessor x-offset))
  (:documentation "Modifies DATUM-POSITION method to plot data with a
    constant x offset."))

(defmethod datum-position :around ((self graph-data-x-offset-mixin) datum)
  (let ((x-offset (x-offset self)))
    (multiple-value-bind (x y) (call-next-method self datum)
      (when x-offset (incf x x-offset))
      (values x y))))

(defclass GRAPH-DATA-Y-OFFSET-MIXIN ()
  ((y-offset :initform nil :initarg :y-offset :accessor y-offset))
  (:documentation "Modifies DATUM-POSITION to plot data with a constant y offset."))

(defmethod datum-position :around ((self graph-data-y-offset-mixin) datum)
  (let ((y-offset (y-offset self))) 
    (multiple-value-bind (x y) (call-next-method self datum)
      (when y-offset (incf y y-offset))
      (values x y))))

;;; GRAPH-DATA-XY-OFFSET-MIXIN is written as a separate mixin rather
;;; than a mixture of the 2 above to avoid adding 2 whoppers instead
;;; of 1 in the inner plot loop. 
(defclass GRAPH-DATA-XY-OFFSET-MIXIN ()
  ((x-offset :initform nil :initarg :x-offset :accessor x-offset)
   (y-offset :initform nil :initarg :y-offset :accessor y-offset))
  (:documentation
    "Modifies DATUM-POSITION to plot data with a constant x or y offset."))

(defmethod datum-position :around ((self graph-data-xy-offset-mixin) datum)
  (let ((x-offset (x-offset self))
	(y-offset (y-offset self)))
    (multiple-value-bind (x y) (call-next-method self datum)
      (when x-offset (incf x x-offset))
      (when y-offset (incf y y-offset))
      (values x y))))


(defclass GRAPH-DATA-DITHER-MIXIN ()
  ((x-dither :initform 0 :initarg :x-dither :accessor x-dither)
   (y-dither :initform 0 :initarg :y-dither :accessor y-dither)
   (dither-seed :initform (random-seed) :initarg :dither-seed :accessor dither-seed))
  (:documentation
   "Used for data whose values are quantized, so that many datums will not map
    to exactly the same screen location.  Modifies displayer methods but not
    map methods, because we still want to know exactly where datums are.
    The random seed is kept in hopes of generating a reproducible dithering."))

(defmethod xy-to-uv-distance (graph x-distance y-distance)
  ;; there must be a better equation
  (multiple-value-bind (u0 v0) (xy-to-uv graph 0 0)
    (declare (fixnum u0 v0))
    (multiple-value-bind (u1 v1) (xy-to-uv graph x-distance y-distance)
      (declare (fixnum u1 v1))
      (values (- u1 u0) (- v1 v0)))))

(defmethod uv-to-xy-distance (graph x-distance y-distance)
  ;; there must be a better equation
  (multiple-value-bind (u0 v0) (uv-to-xy graph 0 0)
    (multiple-value-bind (u1 v1) (uv-to-xy graph x-distance y-distance)
      (values (- u1 u0) (- v1 v0)))))

(defmethod datum-displayer :around ((self GRAPH-DATA-DITHER-MIXIN) graph)
  (with-slots (x-dither y-dither dither-seed) self
    (let ((f (call-next-method self graph)))
      (declare (compiled-function f))
      (if (and x-dither x-dither (zerop x-dither) (zerop y-dither))
	  f
	  (multiple-value-bind (u-dither v-dither)
	      (xy-to-uv-distance graph
				 (or x-dither 0) (or y-dither 0))
	    (declare (fixnum u-dither v-dither))
	    ;; Do dithering in uv coordinates so we can avoid floating point arithmetic
	    ;; (big win for Lucid).
	    (setq u-dither (abs u-dither) v-dither (abs v-dither))
	    #'(lambda (stream u v datum)
		(declare (fixnum u v))
		(funcall f stream
			 (+ u (the fixnum
				   (statistics:uniform-between
				    (- u-dither) u-dither)))
			 (+ v (the fixnum
				   (statistics:uniform-between
				    (- v-dither) v-dither)))
			 datum)))))))

(defmethod display-data :around ((self graph-data-dither-mixin) stream graph)
  ;; Always use the same seed to get the same dithering.
  (with-seed (dither-seed self)
    (call-next-method self stream graph)))


;;; Actually each symbology should provide this, but this is more
;;; consistant with the way graphs used to do it.  In fact,
;;; symbologies should be objects that you can ask for a display datum
;;; function that you map to display your data. 
(defclass graphics-style-mixin ()
  ((pattern :initform nil :initarg :pattern :accessor pattern) ; in clim, NIL == FILLED
   (thickness :initform 0 :initarg :thickness :accessor thickness)			
   (line-end-shape :initform :round :initarg :line-end-shape :accessor line-end-shape))
  (:documentation "Fancy graphics style."))

(defvar *SCI-GRAPH-AVAILABLE-STIPPLES*
	(nconc '(("None" :value nil) ("Filled" :value t))))


;;; SYMBOLOGIES
;;; :scatter - plot datum as individual centered symbols.
;;; :line    - draw continuous line through data.
;;; :step    - draw stair step line between data points.
;;; :bar     - draw a bar graph.

;;; Symbologies are organized into classes that share attributes in common.
(defmacro symbology-classes (symbology)
  `(get ,symbology 'symbology-classes))

(setf (symbology-classes :scatter) '(:scatter))
(setf (symbology-classes :line)    '(:line))
(setf (symbology-classes :bar)     '(:bar :line))
(setf (symbology-classes :step)    '(:step :line))
(setf (symbology-classes :line-symbol) '(:line-symbol :line :scatter))

(defun symbology-class-p (symbology class)
  (member class (symbology-classes symbology) :test #'eq))

(defun contains-symbology-class (symbologies class)
  (loop for symbology in symbologies
	thereis (symbology-class-p symbology class)))

(defclass BASIC-GRAPH-DATUM-SYMBOLOGY-MIXIN (graphics-style-mixin)
    ;; Each datum is plotted in each symbology.
    ((symbologies :initform () :initarg :symbologies :accessor symbologies))
  (:documentation
    "Any mixin providing a DATUM-STYLE-DISPLAYER should
    use 1 or more DATUM to extract the information it needs from the
    datum.  See ESSENTIAL-DATUM-MIXIN.")) 

(defgeneric symbology-choices (self)
  (:method-combination append)
  (:documentation
    "Returns an alist acceptable for use with the ALIST-MEMBER presentation type"))


(defclass GRAPH-DATUM-LINE-SYMBOLOGY-MIXIN
	  (basic-graph-datum-symbology-mixin
	   basic-graph-data)
  ((line-style :initform 0 :initarg :line-style :accessor line-style)
   (min-symbol-spacing :initform 20	; pixels
		       :initarg :min-symbol-spacing
		       :accessor min-symbol-spacing))
  (:documentation "Draw a line in dotted line style LINE-STYLE."))

(defmethod symbology-choices append ((self graph-datum-line-symbology-mixin))
  ;; Lucid seems to do nconc instead of append, and makes circular lists,
  ;; hence the consing here.
  (list '("Line" :value :line) '("Line-Symbol" :value :line-symbol)))

(defmethod transformation-offsets ((any t)) (values 0 0))

(defmethod dont-record-output-history ((dataset t))
  "If true, then we have permission to optimize the hell out of the line displayer."
  (let ((line-style (line-style dataset))
	(thickness (thickness dataset))
	(data (data dataset)))
    ;; Returns T for a vanilla line style and a relatively large data set.
    (and (zerop line-style)
	 (< thickness 2)
	 (not (present-self-p dataset))
	 (typep data 'sequence)
	 (> (length data) 2500))))

(defmethod compute-line-displayer ((dataset t))
  ;; Returns a function of (stream x1 y1 x2 y2) that draws a line without clipping.
  (make-optimized-line-displayer
   (alu dataset)
   (thickness dataset)
   (not (dont-record-output-history dataset))))

(defmethod datum-style-displayer
	   ((self graph-datum-line-symbology-mixin)
	    graph
	    (type (eql :LINE)))
  "Draws a line between points."
  (declare (ignore graph))
  (let* ((line-style (line-style self))
	 (thickness (thickness self))
	 (alu (alu self))
	 (pattern (pattern self))
	 (line-end-shape (line-end-shape self))
	 (clip-rectangle *clip-rectangle*)
	 (left (pop clip-rectangle))
	 (right (pop clip-rectangle))
	 (bottom (pop clip-rectangle))
	 (top (pop clip-rectangle))
	 (dash-ds 0.0)
	 (last-in nil)
	 (last-u NIL)
	 (last-v NIL))
    (declare (fixnum bottom top left right line-style thickness))
    (if (< bottom top) (psetq top bottom bottom top))
    (if (zerop line-style)
	(let ((displayer (compute-line-displayer self)))
	  #'(lambda (stream u v datum)
	      (declare (fixnum u v)
		       (ignore datum))
	      ;; This is the most common case.
	      (let ((this-in (and u (< left u right) (< top v bottom))))
		(cond ((null last-u))
		      ((null u))
		      ((and (= u last-u) (= v last-v)))
		      ((and last-in this-in)
		       ;; dont bother to clip.
		       (funcall displayer stream last-u last-v u v))
		      (t
		       ;; do it the slow way (rarely)
		       (device-draw-line stream last-u last-v u v
					 :thickness thickness
					 :transform nil :alu alu)))
		(setq last-in this-in)
		(setq last-u u last-v v))))
	#'(lambda (stream u v datum)
	    (declare (fixnum u v)
		     (ignore datum))
	    (when (and u last-u)
	      (setq dash-ds
		    (device-draw-line stream last-u last-v u v
				      :alu alu
				      :dash-pattern line-style 
				      :pattern pattern
				      :thickness thickness
				      :line-end-shape line-end-shape
				      :dash-ds dash-ds
				      :transform nil)))
	    (setq last-u u last-v v)))))

(defmethod datum-style-displayer
    ((self graph-datum-line-symbology-mixin)
     graph
     (type (eql :LINE-SYMBOL)))
  "Draws a line, with a symbol every min-symbol-spacing pixels."
  (let* ((line-style (line-style self))
	 (alu (alu self))
	 (pattern (pattern self))
	 (thickness (thickness self))
	 (line-end-shape (line-end-shape self))
	 (min-symbol-spacing (min-symbol-spacing self))
	 (dash-ds 0.0)
	 (last-distance -1)
	 (last-u NIL)
	 (last-v NIL)
	 (symbol-displayer
	  (datum-style-displayer self graph :scatter)))
    (declare (fixnum last-distance min-symbol-spacing thickness)
	     (compiled-function symbol-displayer))
    (flet ((distance (x1 y1 x2 y2)
	     (declare (fixnum x1 y1 x2 y2))
	     ;;; This needs to return an integer (actually it is a fixnum, assuming x&y are.)
	     (values (ROUND (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))))
      #'(lambda (stream u v datum)
	  (declare (fixnum u v))
	  (when (not (minusp last-distance))
	    (incf last-distance (the fixnum (distance u v last-u last-v))))
	  (when (or (minusp last-distance) (> last-distance min-symbol-spacing))
	    (funcall symbol-displayer stream u v datum)
	    (setq last-distance 0))
	  (cond ((not last-u))
		((not u))
		(t
		 (setq dash-ds
		   (device-draw-line stream last-u last-v u v
				     :alu alu
				     :dash-pattern line-style 
				     :pattern  pattern
				     :thickness thickness
				     :line-end-shape line-end-shape
				     :dash-ds dash-ds
				     :transform nil))))
	  (setq last-u u last-v v)))))
			      

(defclass GRAPH-DATUM-BAR-SYMBOLOGY-MIXIN
	  (graph-datum-line-symbology-mixin
	    basic-graph-datum-symbology-mixin
	    basic-graph-data)
					; Default bar width (Xu)
  ((bar-width :initform nil :initarg :bar-width :accessor bar-width))
  (:documentation "Provides a vertical bar symbology."))

(defmethod symbology-choices append ((self GRAPH-DATUM-BAR-SYMBOLOGY-MIXIN))
  (list '("Bar" :value :bar)))

(defmethod make-legend-datum ((self graph-datum-bar-symbology-mixin) x y
			      &rest args)
  (declare (ignore args))
  (list x y)
  ;(list x y 0.75)			; Provide a width to look nicer.
  )

;;;                                                      ----             ----
;;;                                                      |  |                |
;;; MThome has pointed out that when bars are drawn like |  | rather than    |
;;; the dotted patterns overlap and look ugly.
;;;

(defmethod datum-style-displayer
	   ((self GRAPH-DATUM-BAR-SYMBOLOGY-MIXIN)
	    graph
	    (style (eql :bar)))
  "Draws a bar centered at datum.  If WIDTH is not provided BAR-WIDTH
   is used. If neither WIDTH nor BAR-WIDTH are provided, the width of the
   bar is determined from point spacing and points are assume ordered in X."
  (multiple-value-bind (x0 y0) (xy-to-uv graph 0.0 0.0)	; baseline position.
    (let* ((pattern (pattern self))
	   (line-style (line-style self))
	   (thickness (thickness self))
	   (line-end-shape (line-end-shape self))
	   (dash-ds 0.0)
	   (bar-width (bar-width self))
	   (default-width (and bar-width
			       (values (round (* bar-width (x-u-scale graph))))))
	   (alu (alu self))
	   (transform-baseline t)
	   x-last
	   y-last)
	  ;;; KRA: Factor pattern out of lambda.
      #'(lambda (stream x y datum)
	  (declare (ignore datum))
	  (when transform-baseline
	    ;; have to wait to do this till we have a stream.
	    (multiple-value-setq (x0 y0) (uv-to-screen stream x0 y0))
	    (setq transform-baseline nil))
	  (let* ((width default-width))
	    (if width
		(let* ((half (values (truncate width 2)))
		       (x- (- x half))
		       (x+ (+ x half)))
		  ;; center of bar corresponds to x,y position
		  (if pattern (device-draw-rectangle stream
						     x- x+ y0 y
						     :alu alu
						     :filled t 
						     :pattern pattern)
		      (setq dash-ds
			    (with-stack-list (points x- y0 x- y x+ y x+ y0)
			      (device-draw-lines stream points
						 :alu alu
						 :dash-ds dash-ds
						 :dash-pattern line-style
						 :pattern pattern
						 :thickness thickness
						 :line-end-shape
						 line-end-shape
						 :transform nil)))))
		;; No width case:  end of bar corresponds to x,y position.
		;; This is pretty much like :step is drawn, so that variable sampling
		;; rates look right.
		(when x-last
		  (let* ((width (values (truncate (- x x-last) 2)))
			 (x++ (+ x-last width)))
		    (if pattern
			(device-draw-rectangle stream (- x width) (+ x width) y0 y
					       :filled t :alu alu :pattern pattern)
			(setq dash-ds
			      (with-stack-list (points
						 x-last y-last x++ y-last
						 x++ y0 x++ y x y)
				(device-draw-lines stream points
						   
						   :alu alu
						   :dash-ds dash-ds
						   :dash-pattern line-style
						   :pattern pattern
						   :thickness thickness
						   :line-end-shape
						   line-end-shape
						   :transform nil)))))))
	    (setq x-last x y-last y))))))


(defclass GRAPH-DATUM-STEP-SYMBOLOGY-MIXIN
	  (graph-datum-line-symbology-mixin
	    basic-graph-datum-symbology-mixin
	    basic-graph-data)
    ())

(defmethod symbology-choices append ((self graph-datum-step-symbology-mixin))
  (list '("Step" :value :step)))

;;; This style isn't on menu.
(defmethod datum-style-displayer
    ((self graph-datum-line-symbology-mixin)
     graph
     (type (eql :CENTERED-STEP)))
  "Draws a line between points."
  (declare (ignore graph))
  (let ((line-style (line-style self))
	(alu (alu self))
	(pattern (pattern self))
	(thickness (thickness self))
	(line-end-shape (line-end-shape self))
	(dash-ds 0.0)
	(last-u NIL)
	(last-v NIL))
    #'(lambda (stream u v datum)
	(declare (ignore datum))
	dash-ds
	(cond ((not last-u))
	      ((not u))
	      (t
	       (let ((u+ (+ last-u (values (truncate (- u last-u) 2)))))
		 (setq dash-ds
		   (with-stack-list (points last-u last-v u+ last-v u+ v u v)
				    (device-draw-lines STREAM points
						       :alu alu
						       :dash-pattern line-style
						       :pattern pattern
						       :thickness thickness
						       :line-end-shape line-end-shape
						       :transform nil))))))
	(setq last-u u last-v v))))

(defmethod datum-style-displayer
	   ((self graph-datum-line-symbology-mixin)
	    graph
	    (type (eql :STEP)))
  "Draws a line between points."
  (declare (ignore graph))
  (let* ((line-style (line-style self))
	 (alu (alu self))
	 (pattern (pattern self))
	 (thickness (thickness self))
	 (line-end-shape (line-end-shape self))
	 (clip-rectangle *clip-rectangle*)
	 (left (pop clip-rectangle))
	 (right (pop clip-rectangle))
	 (bottom (pop clip-rectangle))
	 (top (pop clip-rectangle))
	 (dash-ds 0.0)
	 (last-in nil)
	 (last-u NIL)
	 (last-v NIL))
    (declare (fixnum thickness))
    (if (< bottom top) (psetq top bottom bottom top))
    (if (zerop line-style)
	(let ((displayer (compute-line-displayer self)))
	  #'(lambda (stream u v datum)
	      (declare (fixnum u v)
		       (ignore datum))
	      ;; This is the most common case.
	      (let ((this-in (and u (< left u right) (< top v bottom))))
		(cond ((null last-u))
		      ((null u))
		      ((and (= u last-u) (= v last-v)))
		      ((and last-in this-in)
		       ;; dont bother to clip.
		       (funcall displayer stream last-u last-v u last-v)
		       (funcall displayer stream u last-v u v))
		      (t
		       ;; do it the slow way (rarely)
		       (with-stack-list (points last-u last-v u last-v u v)
			 (device-draw-lines STREAM points
					    :alu alu :transform nil))))
		(setq last-in this-in)
		(setq last-u u last-v v))))
	#'(lambda (stream u v datum)
	    (declare (fixnum u v)
		     (ignore datum))
	    dash-ds
	    (cond ((not last-u))
		  ((not u))
		  (t
		   (setq dash-ds
			 (with-stack-list (points last-u last-v u last-v u v)
			   (device-draw-lines STREAM points
					      :alu alu
					      :dash-pattern line-style
					      :pattern pattern
					      :thickness thickness
					      :line-end-shape line-end-shape
					      :transform nil)))))
	    (setq last-u u last-v v)))))


(defclass GRAPH-DATUM-SCATTER-SYMBOLOGY-MIXIN
	  (basic-graph-datum-symbology-mixin basic-graph-data)
    ((data-symbol :initform :+ :initarg :data-symbol :accessor data-symbol)
     (symbol-height :initform 10 :initarg :symbol-height :accessor symbol-height))
  (:documentation
    "Provides a datum-style-displayer method that allows data
     to be displayed in several different symbologies."))

(defmethod symbology-choices append ((self graph-datum-scatter-symbology-mixin))
  (list '("Scatter" :value :scatter)))

(defmethod datum-style-displayer
	   ((self graph-datum-scatter-symbology-mixin)
	    graph
	    (style (eql :SCATTER)))
  (declare (ignore graph))
  (let* ((data-symbol (data-symbol self))
	 (symbol-height (values (truncate (symbol-height self) 2)))
	 (pattern (pattern self))
	 (alu (alu self))
	 (thickness (thickness self))
	 (clip-rectangle *clip-rectangle*)
	 (left (pop clip-rectangle))
	 (right (pop clip-rectangle))
	 (bottom (pop clip-rectangle))
	 (top (pop clip-rectangle))
	 (displayer (symbol-displayer data-symbol alu thickness pattern)))
    (declare (fixnum left right bottom top)
	     (compiled-function displayer))
    (if (< bottom top) (psetq top bottom bottom top))
    #'(lambda (stream u v datum)
	(declare (fixnum u v)
		 (ignore datum))
	(when (and u v (<= left u right) (<= top v bottom))
	  (funcall displayer stream u v symbol-height)))))

(defclass dataset-datum-size-mixin () ()
   (:documentation
    "Produces scatter graphs where each datum could be a different size."))

;;; Stub.
(defmethod datum-size ((dataset dataset-datum-size-mixin) (datum t)) nil)

(defmethod datum-style-displayer
	   ((self dataset-datum-size-mixin)
	    graph
	    (style (eql :SCATTER)))
  (declare (ignore graph))
  (let* ((data-symbol (data-symbol self))
	 (symbol-height (symbol-height self))
	 (pattern (pattern self))
	 (alu (alu self))
	 (thickness (thickness self))
	 (clip-rectangle *clip-rectangle*)
	 (left (pop clip-rectangle))
	 (right (pop clip-rectangle))
	 (bottom (pop clip-rectangle))
	 (top (pop clip-rectangle))
	 (displayer (symbol-displayer data-symbol alu thickness pattern)))
    (declare (fixnum left right bottom top)
	     (compiled-function displayer))
    (if (< bottom top) (psetq top bottom bottom top))
    #'(lambda (stream u v datum)
	(declare (fixnum u v))
	(when (and u v (<= left u right) (<= top v bottom))
	  (funcall displayer stream u v
		   (values (truncate (or (datum-size self datum) symbol-height) 2)))))))


(defclass GRAPH-DATA-SYMBOLOGY-MIXIN
	  (graph-datum-bar-symbology-mixin
	    graph-datum-step-symbology-mixin
	    graph-datum-line-symbology-mixin
	    graph-datum-scatter-symbology-mixin
	    basic-graph-datum-symbology-mixin)
    ()
  (:documentation
    "Provides :SCATTER :LINE :STEP and :BAR symbologies for plotting data."))


(defclass GRAPH-DATA-COLOR-MIXIN ()
    ((color :initform :gold :initarg :color :accessor color)))

(defmethod update-alu-for-stream ((self graph-data-color-mixin) (stream t))
  "Match the alu to the stream."
  ;; Don't forget that encapsulation may occur, and the stream may not
  ;; be a window at all.  (continuation-output-size, hardcopy streams, etc.)
  ;; Therefore specializing the stream argument may be a temptation worth avoiding.
  (setf (alu self) (alu-for-stream stream (color self))))

(defmethod display-data :before ((self graph-data-color-mixin) (stream t) (graph t))
  (update-alu-for-stream self stream))

(defmethod display-legend-dataset :before ((self graph-data-color-mixin)
					   STREAM graph left bottom width height)
  (declare (ignore graph left bottom width height))
  (update-alu-for-stream self stream))


#|
HOW AUTO-SCALING WORKS.

Auto scaling of a graph can be either :X, :Y, :BOTH, or NIL.  For each
axis that should be scaled, the graph asks each dataset what limits it
wants to contribute.  Each dataset can compute its limits in its own
way.  The graph takes the union of the limits returned. 

|#

(defclass GRAPH-DATA-AUTO-SCALE-MIXIN (basic-graph-data)
  ; Can this dataset provide auto-scaling info?
  ((auto-scale? :initform nil :initarg :auto-scale? :accessor auto-scale?))
  (:documentation "Allows a dataset to provide auto scale limits"))

(defmethod auto-scale-limits ((self t) auto-scale-type xll xur yll yur)
  ;; Default method
  (declare (ignore auto-scale-type xll xur yll yur))
  (list nil nil nil nil))

(defmethod auto-scale-limits ((self graph-data-auto-scale-mixin)
			      auto-scale-type xll xur yll yur)
  "Scale graph to minimum and maximum of x axis, y axis or both.  
   If :BOTH the x and y limits will be set to the min or max values in data.
   If :X the min and max value of x for data with y values between yll
   and yur will be used. Similarly for :Y.
   The LIST (xmin xmax ymin ymax) describing the limits is returned."
  (cond ((not (auto-scale? self)) nil)
	(auto-scale-type
	 (auto-scale-limits-internal self auto-scale-type xll xur yll yur
			     (data self)))))

(defmethod auto-scale-limits-internal ((self graph-data-auto-scale-mixin)
			       type x-left x-right y-bottom y-top the-data)
  ;; This is called once for the graph and once for each legend dataset.
  (macrolet
    ((collect-range (relation min value max)
       `(progn (when (or (null ,min) (,relation ,value ,min))
		 (setq ,min ,value))
	       (when (or (null ,max) (,relation ,max ,value))
		 (setq ,max ,value)))))
    (let ((xmin nil)
	  (xmax nil)
	  (ymin nil)
	  (ymax nil))
      (map-data-xy self
		   #'(lambda (x y)
		       (when (or (eq type :both)
				 (and (eq type :x) (<= y-bottom y y-top)))
			 (collect-range < xmin x xmax))
		       (when (or (eq type :both)
				 (and (eq type :y) (<= x-left x x-right)))
			 (collect-range < ymin y ymax)))
		   the-data)
      (list xmin xmax ymin ymax))))
	   

(defclass GRAPH-DATA-LIMITS-MIXIN ()
    ()
  (:documentation
    "Lets a dataset provide :limit-specs when auto scaling.  See GRAPH-LIMITS-MIXIN."))

(defmethod limit-specs ((self GRAPH-DATA-LIMITS-MIXIN)) ())

(defmethod AUTO-SCALE-LIMITS :around ((self graph-data-limits-mixin)
				      auto-scale-type xll xur yll yur)
  "Constrain graph edges to be within limits."
  (let ((the-limits (limit-specs self))
	(stuff (call-next-method SELF auto-scale-type xll xur yll yur)))
    (let ((xmin (first stuff))
	  (xmax (second stuff))
	  (ymin (third stuff))
	  (ymax (fourth stuff)))
      (when the-limits
	(multiple-value-bind (left right bottom top)
	    (apply #'values the-limits)
	  (when xmin (setq xmin (limit-value xmin left)))
	  (when xmax (setq xmax (limit-value xmax right)))
	  (when ymin (setq ymin (limit-value ymin bottom)))
	  (when ymax (setq ymax (limit-value ymax top))))
	(when (and xmin xmax (< xmax xmin)) ; When limit spec is
					    ; really inappropriate 
	  (psetq xmin xmax xmax xmin))	    ; for the data, min max
					    ; gets trashed. 
	(when (and ymin ymax (< ymax ymin))
	  (psetq ymin ymax ymax ymin)))
      (list xmin xmax ymin ymax))))

(defclass GRAPH-DATA-ADD-DATUM-MIXIN
	  (basic-graph-data)
    ()
  (:Documentation "Provides a protocol for adding a individual datum to the dataset.")) 

(defmethod add-datum ((self graph-data-add-datum-mixin) datum)
  "Add a datum to the end of the data."
  (vector-push-extend datum (data self)))

(defmethod display-datum ((self GRAPH-DATA-ADD-DATUM-MIXIN) graph STREAM datum displayer)
  (let ((H (stream-height stream)))
    (multiple-value-bind (x y) (datum-position self datum)
      (multiple-value-setq (x y) (xy-to-uv graph x y))
      (funcall (the compiled-function displayer) stream x (- H (the fixnum y)) datum))))
  
(defmethod prepare-graph-for-datum ((self GRAPH-DATA-ADD-DATUM-MIXIN) graph stream datum displayer)
  "Here is your chance to scroll the graph, if necessary, before displaying."
  (when (and graph (not
		    (multiple-value-bind (x y) (datum-position self datum)
		      (is-inside graph x y))))
    ;; see method AUTO-SCALE-EXTENSIONS
    (setf (auto-scale-needed graph) t)
    (refresh graph STREAM)
    ;; Cached clip rectangle is now invalid, need to make a new displayer
    (setq displayer (datum-displayer self graph))
    ;; This initializes the displayer:
    (display-datum self graph stream datum displayer))
  displayer)

(defmethod add-and-display-datum ((self GRAPH-DATA-ADD-DATUM-MIXIN) graph STREAM datum displayer)
  "Add and display DATUM."
  ;; displayer is the function you get from the DATUM-DISPLAYER method.
  (add-datum self datum)
  (when (and graph (displayed? graph))
    (setq displayer (prepare-graph-for-datum self graph stream datum displayer))
    (display-datum self graph STREAM datum displayer))
  displayer)


(defclass simple-data-statistics-mixin () ()
  (:documentation "Provide some commonly used statistical metrics."))

(defmethod x-mean ((dataset simple-data-statistics-mixin))
  (let ((sumx 0) (count 0))
    (map-data-xy dataset #'(lambda (x y)
			     (declare (ignore y))
			     (incf sumx x)
			     (incf count))
		 (data dataset))
    (if (zerop count)
	(values nil 0)
      (values (/ sumx (float count)) count))))

(defmethod y-mean ((dataset simple-data-statistics-mixin))
  (let ((sumy 0) (count 0))
    (declare (fixnum count))
    (map-data-xy dataset #'(lambda (x y)
			     (declare (ignore x))
			     (incf sumy y)
			     (incf count))
		 (data dataset))
    (if (zerop count)
	(values nil 0)
      (values (/ sumy (float count)) count))))

(defmethod x-mean-and-stdev ((dataset simple-data-statistics-mixin))
  (multiple-value-bind (meanx count) (x-mean dataset)
    (let ((sumsqx 0))
      (if (= count 1)
	  (values meanx 0)
	  (progn
	    (map-data-xy dataset #'(lambda (x y)
				     (declare (ignore y))
				     (incf sumsqx (expt (- x meanx) 2)))
			 (data dataset))
	    (values meanx (sqrt (/ sumsqx (float (1- count))))))))))

(defmethod y-mean-and-stdev ((dataset simple-data-statistics-mixin))
  (multiple-value-bind (meany count) (y-mean dataset)
    (let ((sumsqy 0))
      (if (= count 1)
	  (values meany 0)
	  (progn
	    (map-data-xy dataset #'(lambda (x y)
				     (declare (ignore x))
				     (incf sumsqy (expt (- y meany) 2)))
			 (data dataset))
	    (values meany (sqrt (/ sumsqy (float (1- count))))))))))

(defmethod x-min-and-max ((dataset simple-data-statistics-mixin))
  (let (minx maxx)
    (map-data-xy dataset #'(lambda (x y)
			     (declare (ignore y))
			     (when (or (not minx) (> minx x)) (setq minx x))
			     (when (or (not maxx) (< maxx x)) (setq maxx x)))
		 (data dataset))
    (values minx maxx)))


(defmethod y-min-and-max ((dataset simple-data-statistics-mixin))
  (let (miny maxy)
    (map-data-xy dataset #'(lambda (x y)
			     (declare (ignore x))
			     (when (or (not miny) (> miny y)) (setq miny y))
			     (when (or (not maxy) (< maxy y)) (setq maxy y)))
		 (data dataset))
    (values miny maxy)))


(defclass gated-data (graph-data)
    ((corners :initform nil :initarg :corners :accessor corners))
  (:documentation
    "A type of dataset that 'gates' or filters a source dataset.
     A point from the source dataset passes through gate if it is contained
     within a region in xy space.  A region is defined by an arbitrary list 
     of xy pairs, defining a closed polygon that need not be convex.
     [Used by region annotations.]"))

(defun point-in-polygon (x y map-polygon polygon)
  "Returns T if point X,Y is inside a closed boundary.  Points on the 
  boundary edge are ambiguously either inside or outside.  The
  algorithm counts the number of line segements that intersect the
  semi-infinite line from (x,y) to (+infinity,y).  If there is an odd 
  number of such segements, the point is inside the polygon.  
  The function MAP-POLYGON maps a function over consecutive line 
  segments (X1 Y1 X2 Y2) of the polygon, such that the
  first point is the same as the last.  (The polygon need not be convex.)

  See:

  Shimrat, M., 1962, Algorithm 112: position of point relative to
  polygon, CACM,5,434.

  Hall, J.H., 1975, PTLOC - A FORTRAN subroutine for determining the
  position of a point relative to a closed boundary, Math. Geol., 7,
  75-79.

  Anderson, K.R., 1975, Letter to the editor of Math. Geol.

  The final version of this algorithm followed from discussions with
  David Fitterman of the US Geological Survey, Jan 1975."

  (let ((point-in NIL))
    (funcall map-polygon
	     #'(lambda (x1 y1 x2 y2)
		 (if (eq (<= y y1) (>  y y2))	; Segement crosses ray.
		     (when (and (not (= y1 y2))	; Ignore horizontal segement.
				(< (- x x1 (/ (* (- y y1) (- x2 x1))	
					      (- y2 y1)))	; Point is to left.
				   0))
		       ;; (print (list x1 y1 x2 y2))
		       (setq point-in (not point-in)))))
	     polygon)
    point-in))

(defmethod surrounded-p ((object gated-data) x y)
  (point-in-polygon x y #'map-polygon-edges (corners object)))

(defmethod map-data ((dataset gated-data) function (data t))
  ;; assumes DATA argument is itself a dataset.
  (declare (compiled-function function))
  (map-data data
	    #'(lambda (datum)
		(multiple-value-bind (x y)
		    (datum-position data datum)
		  (if (surrounded-p dataset x y)
		      (funcall function datum))))
	    (data data)))

(defmethod map-data-xy ((dataset gated-data) function (data t))
  ;; assumes DATA argument is itself a dataset.
  (declare (compiled-function function))
  (map-data data
	    #'(lambda (datum)
		(multiple-value-bind (x y)
		    (datum-position data datum)
		  (if (surrounded-p dataset x y)
		      (funcall function x y))))
	    (data data)))

(defmethod datum-position ((dataset gated-data) datum)
  (datum-position (data dataset) datum))

(defmethod create-gated-dataset (graph dataset stream &optional
						      (type 'gated-data) corners)
  (or corners (and graph (setq corners (select-xy-polygon graph stream))))
  (when corners
    (make-instance type
      :data dataset
      :symbologies (symbologies dataset)
      :corners corners)))

;;; Where should this be?
(defmethod select-xy-polygon (graph stream)
  (let ((points (select-screen-polygon stream))
	(xy nil))
    (if points
	(dolist (point points xy)
	  (let ((x (car point))
		(y (second point)))
	    (multiple-value-setq (x y) (screen-to-uv stream x y))
	    (multiple-value-setq (x y) (uv-to-xy graph x y))
	    (push (list x y) xy))))))



;;; Clim 1.0 requires this be defined before any presentation type
;;; that depends on it.  Hence moved here from present.lisp.

(defvar *repainting-dataset* nil)

(defclass presentable-mixin
    ()
    ((present-self-p :initform t :initarg :present-self-p
		     :accessor present-self-p)		      
     (graph-present-inferiors-p :initform t :initarg :graph-present-inferiors-p
				:accessor graph-present-inferiors-p)))
	
(defclass presentable-data-mixin (presentable-mixin) ())

(defmethod graph-presentation-type ((self presentable-data-mixin)) 'graph-data)

(defmethod display-data :around ((self presentable-data-mixin) STREAM graph)
  (if (present-self-p self)
      (with-output-as-presentation
       (:stream stream
		:single-box nil
		:object self
		:type (graph-presentation-type self)
		:allow-sensitive-inferiors t)
       (call-next-method SELF stream graph))
    (progn
      (call-next-method SELF stream graph))))
   

(defmethod display-legend-dataset :around ((self presentable-data-mixin) STREAM
					   graph left bottom width height)
  ;; you always get the legend mouse-sensitive.
  (multiple-value-bind (sl st) (uv-to-screen stream left (+ bottom height))
    ;; see comment above regarding cursor positioning
    (with-temporary-cursor-position (stream sl st)				
      (with-output-as-presentation
	(:stream stream
	 :single-box t :object self
	 :type (graph-presentation-type self)
	 :allow-sensitive-inferiors nil)
	(call-next-method SELF STREAM graph left bottom width height)))))

(defmethod datum-presentation-type ((self presentable-data-mixin) (datum t))
  'expression)

(defmethod datum-presentation ((self presentable-data-mixin) datum)
  ;; Just in case it isn't the datum itself.
  datum)

(defmethod datum-displayer :around ((self presentable-data-mixin) graph)
  (let ((f (call-next-method self graph)))
    (if (graph-present-inferiors-p self)
	#'(lambda (stream u v datum)
	    (with-output-as-presentation
	      (:stream stream
	       :object (datum-presentation self datum)
	       :type (datum-presentation-type self datum))
	      (funcall f stream u v datum))
	    (values u v))
	f)))

