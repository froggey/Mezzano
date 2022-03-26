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

;;; BASIC GRAPH CLASSES.

;;; To do:

;;; o High level idea: Separate transform, from graphics state and style
;;; o Move essential-graph-display into a basic-object mixin.

(defvar *max-digits* 6 
  "Default number of digits of precision in a float for axis labeling.")

(defvar *width-between-graphs* 3 "Pixels between graphs horizontally.")
(defvar *height-between-graphs* 3 "Pixels between graphs vertically.")

;;; THE FOLLOWING MACROS ARE USED IN METHOD BODIES FOR DRAWING.
(defmacro inside-rectangle? (x y left bottom right top)
  `(and (>= ,x ,left) (<= ,x ,right)
	(>= ,y ,bottom) (<= ,y ,top)))

(defun constrain-value-between (min x max)
  ;; this is wrong (min (max min x) max)
  (if (<= min max) (min (max min x) max) (min (max max x) min)))

(defun constrain-point-in-rectangle (x y left bottom right top)
  (values (constrain-value-between left x right)
	  (constrain-value-between bottom y top)))


;;; Boxes are specified by their OUTSIDE edges, but their inside edges
;;; are stored...  Users should only user -OUTSIDE methods.
(defclass UV-BOX		     ; Corners of box (poor choice of names)
	  ()
    ((ull :initform 50 :initarg :ull :accessor ull)	  ; u lower left
     (vll :initform 50 :initarg :vll :accessor vll)	  ; v lower left
     (uur :initform 700 :initarg :uur :accessor uur)	  ; u upper right
     (vur :initform 700 :initarg :vur :accessor vur))	  ; v upper right
  )
  
(defmethod uv-inside ((self uv-box))
  "Returns graph's inside UV edges: LEFT RIGTH BOTTOM TOP."
  (with-slots (ull vll uur vur) self
    (values ull uur vll vur)))

(defmethod uv-is-inside ((self uv-box) u v)
  (with-slots (ull vll uur vur) self
     (inside-rectangle? u v ull vll uur vur)))

(defmethod set-uv-inside ((self uv-box) STREAM ul ur vb vt)
  (declare (ignore STREAM))
  (with-slots (ull vll uur vur) self
    (when ul (setq ull (values (round ul))))
    (when ur (setq uur (values (round ur))))
    (when vb (setq vll (values (round vb))))
    (when vt (setq vur (values (round vt))))))

;;; Inside = Outside.
(defmethod uv-outside ((self uv-box))
  (uv-inside self))

(defmethod set-uv-outside ((self uv-box) STREAM ul ur vb vt &optional verify-p)
  (declare (ignore verify-p))
  (set-uv-inside SELF STREAM ul ur vb vt))


(defclass XY-BOX
	  ()
    ((xll :initform 0.0 :initarg :xll :accessor xll)	  ; Lower left and upper
					  		  ;   right corners 
     (yll :initform 0.0 :initarg :yll :accessor yll)	  ; Of the x-y box
     (xur :initform 1.0 :initarg :xur :accessor xur)
     (yur :initform 1.0 :initarg :yur :accessor yur)))

(defmethod xy-inside ((self xy-box))
  (with-slots (xll yll xur yur) self
    (values xll xur yll yur)))

(defmethod set-xy-inside ((self xy-box) STREAM left right bottom top)
  (declare (ignore STREAM))
  (with-slots (xll yll xur yur) self
    (when left   (setq xll left))
    (when right  (setq xur right))
    (when bottom (setq yll bottom))
    (when top    (setq yur top))))


(defclass BASIC-GRAPH-COORDINATES-MIXIN
	  (xy-box
	    uv-box)
    ((x-u-scale :initarg :x-u-scale :accessor x-u-scale)  ; Scale factors for the mapping
     (y-v-scale :initarg :y-v-scale :accessor y-v-scale))
  (:documentation
    "A mapping between two coordinate systems the (x y) coordinate system normally 
     used by the user and the (u v) coordinate system which are integer device 
     coordinates.  A mapping between these two coordinate systems is set up by specifying 
     the lower left and upper right corners of a window in each system that correspond."))

(defmethod with-graph-coordinates-1 ((self basic-graph-coordinates-mixin) STREAM
				     ul ur vb vt xl xr yb yt continuation)
  "Warp graph inside coordinates to these new coordinates."
  ;; Used by legend drawing.
  (multiple-value-bind (oul our ovb ovt)
      (uv-inside self)
    (multiple-value-bind (oxl oxr oyb oyt)
	(xy-inside self)
      (unwind-protect
	  (progn
	    (set-uv-inside self STREAM ul ur vb vt)
	    (set-xy-inside self STREAM xl xr yb yt)
	    (funcall continuation))
	(set-xy-inside self STREAM oxl oxr oyb oyt)
	(set-uv-inside self STREAM oul our ovb ovt)))))

(defmacro with-graph-coordinates ((graph stream ul ur vb vt xl xr yb yt) &body body)
  `(with-graph-coordinates-1
     ,graph ,stream ,ul ,ur ,vb ,vt ,xl ,xr ,yb ,yt
    #'(lambda () ,@body)))

(defmethod set-xy-inside :after ((self basic-graph-coordinates-mixin)
				 STREAM left right bottom top)
  (declare (ignore stream))
  (when (or left right bottom top)
    (rescale self)))

(defmethod set-uv-inside :after ((self basic-graph-coordinates-mixin)
				 STREAM ul ur vb vt)
  (declare (ignore stream))
  (when (or ul ur vb vt)
    (rescale self)))

(defmethod uv-to-xy ((self basic-graph-coordinates-mixin) u v)
  (with-slots (xll ull x-u-scale yll vll y-v-scale) self
    (values (+ XLL (/ (- U ULL) X-U-SCALE))
	    (+ YLL (/ (- V VLL) Y-V-SCALE)))))

(defmethod uv-to-xy-transform ((self basic-graph-coordinates-mixin))
  (let ((xll (slot-value self 'xll))
	(ull (slot-value self 'ull))
	(x-u-scale (slot-value self 'x-u-scale))
	(yll (slot-value self 'yll))
	(vll (slot-value self 'vll))
	(y-v-scale (slot-value self 'y-v-scale)))
    #'(lambda (u v)
        (values (+ xll (/ (- u ull) x-u-scale))
		(+ yll (/ (- v vll) y-v-scale))))))

(defmethod xy-to-uv ((self basic-graph-coordinates-mixin) x y)
  (with-slots (xll ull x-u-scale yll vll y-v-scale) self
    (values (truncate (+ ULL (* (- X XLL) X-U-SCALE)))
	    (truncate (+ VLL (* (- Y YLL) Y-V-SCALE))))))

(defmethod xy-to-stream ((self basic-graph-coordinates-mixin) stream x y)
  (multiple-value-bind (u v)
      (xy-to-uv self x y)
    (uv-to-screen stream u v)))

(defmethod xy-to-uv-transform ((self basic-graph-coordinates-mixin))
  ;; Carefully optimized to death.  Calling the closure should not
  ;; cons any floats, unless the inputs X and Y are not floats.
  (let* ((xll       (slot-value self 'xll))
	 (yll       (slot-value self 'yll))
	 (ull       (slot-value self 'ull))
	 (vll       (slot-value self 'vll))
	 (x-u-scale (float (slot-value self 'x-u-scale)))
	 (y-v-scale (float (slot-value self 'y-v-scale)))
	 (temp1     (float (- ull (* xll x-u-scale))))
	 (temp2     (float (- vll (* yll y-v-scale)))))
    (declare (short-float x-u-scale temp1 y-v-scale temp2))
    #'(lambda (x y)
        (values (truncate
		 (the (short-float -1000000.0 1000000.0)
		   (+ temp1 (* (the short-float (float x))
			       x-u-scale))))
		(truncate
		 (the (short-float -1000000.0 1000000.0)
		   (+ temp2 (* (the short-float (float y))
			       y-v-scale))))))))

(defmethod xy-to-stream-transform ((self basic-graph-coordinates-mixin)
				   stream)
  (let* ((xll       (slot-value self 'xll))
	 (yll       (slot-value self 'yll))
	 (ull       (slot-value self 'ull))
	 (vll       (slot-value self 'vll))
	 (x-u-scale (float (slot-value self 'x-u-scale)))
	 (y-v-scale (float (slot-value self 'y-v-scale)))
	 (height (the fixnum (stream-height stream)))
	 (temp1     (float (- ull (* xll x-u-scale))))
	 (temp2     (float (- vll (* yll y-v-scale)))))
    (declare (short-float x-u-scale temp1 y-v-scale temp2))
    (setq y-v-scale (- y-v-scale))
    (setq temp2 (- height temp2))
    #'(lambda (x y)
        (values (truncate
		 (the (short-float -1000000.0 1000000.0)
		      (+ temp1 (* (the short-float (float x))
				  x-u-scale))))
		(truncate
		 (the (short-float -1000000.0 1000000.0)
		      (+ temp2 (* (the short-float (float y))
				  y-v-scale))))))))

(defmethod rescale ((self basic-graph-coordinates-mixin))
  "Determine scales used in mapping between x-y and u-v."
  (with-slots (xll ull x-u-scale yll vll y-v-scale uur xur yur vur) self
    ;; Float to avoid consing big-rats.
    (setq x-u-scale (/ (- uur ull) (float (- xur xll))))
    (setq y-v-scale (/ (- vur vll) (float (- yur yll))))))

(defmethod is-inside ((self basic-graph-coordinates-mixin) x y)
  "Is x-y point inside graph?"
  (multiple-value-setq (x y) (xy-to-uv self x y))
  (uv-is-inside self x y))


(defclass ESSENTIAL-GRAPH-MARGIN-MIXIN (basic-graph-coordinates-mixin)
  ;; Margin sizes in pixels.
  ((left-margin-size   :initform 0 :reader left-margin-size)
   (right-margin-size  :initform 0 :reader right-margin-size)
   (bottom-margin-size :initform 0 :reader bottom-margin-size)
   (top-margin-size    :initform 0 :reader top-margin-size))
  (:documentation
   "Provides graphs with a margin outside the normal plotting region."))

(defgeneric verify-new-outside (self left right bottom top
				     left-margin right-margin
				     bottom-margin top-margin)
  (:method-combination or #-Lucid :most-specific-last ))

(defmethod display :before ((self essential-graph-margin-mixin) STREAM)
  (reset-margins self STREAM))

(defmethod margins ((self essential-graph-margin-mixin))
  (with-slots (left-margin-size right-margin-size
	       bottom-margin-size top-margin-size) self
    (values left-margin-size right-margin-size
	    bottom-margin-size top-margin-size)))

;;; Have the committee on code purity look at this!
;;; BW:  this resets the old values of uv-inside
;;;      if the new ones (from :set-uv-outside) were bad!!!!
(defmethod set-margins ((self essential-graph-margin-mixin) lm rm bm tm)
  (with-slots (left-margin-size right-margin-size
	       bottom-margin-size top-margin-size) self
    (setf left-margin-size lm
	  right-margin-size rm
	  bottom-margin-size bm
	  top-margin-size tm)))

(defmethod reset-margins ((self essential-graph-margin-mixin) STREAM
			  &optional verifyp)
  "Recompute margins after verifying they are OK."
  (multiple-value-bind (out-l out-r out-b out-t)
      (uv-outside self)
    (multiple-value-bind (lm rm bm tm)
	(compute-margins self STREAM)
      (unless (or (new-outside-check self verifyp
				     out-l out-r out-b out-t lm rm bm tm)
		  verifyp)
	(set-margins self lm rm bm tm)
	(set-uv-outside self STREAM out-l out-r out-b out-t)))))

(defmethod compute-margins ((self essential-graph-margin-mixin) STREAM)
  "Compute the margin sizes.  Whopperize this to add margin size.
  DON'T SIDE EFFECT."		     
  (declare (ignore STREAM))
  (values 0 0 0 0))

(defmethod uv-outside ((self essential-graph-margin-mixin))
  (with-slots (left-margin-size right-margin-size
	       bottom-margin-size top-margin-size
	       ull uur vll vur) self
    (values
     (- ull left-margin-size)
     (+ uur right-margin-size)
     (- vll bottom-margin-size)
     (+ vur top-margin-size))))

(defmethod set-uv-outside :around ((self essential-graph-margin-mixin) STREAM
				   u-ll u-ur v-ll v-ur
				   &optional verifyp)
  "Specify the outside edges of the u-v window in pixels."
  (with-slots (left-margin-size right-margin-size
	       bottom-margin-size top-margin-size) self
    (let ((complaint
	   (new-outside-check self (not verifyp) u-ll u-ur v-ll v-ur
			      left-margin-size right-margin-size
			      bottom-margin-size top-margin-size)))
      (unless (or verifyp complaint)
	(call-next-method self STREAM u-ll u-ur v-ll v-ur verifyp))
      complaint)))

(defmethod set-uv-outside ((self essential-graph-margin-mixin) STREAM
			   u-ll u-ur v-ll v-ur
			   &optional verifyp)
  (declare (ignore verifyp))
  (with-slots (left-margin-size right-margin-size
	       bottom-margin-size top-margin-size) self
    (set-uv-inside self STREAM
		   (values (round (+ u-ll left-margin-size)))
		   (values (round (- u-ur right-margin-size)))
		   (values (round (+ v-ll bottom-margin-size)))
		   (values (round (- v-ur top-margin-size))))))

;;; BW: change name from :new-outside-ok-p
(defmethod new-outside-check ((self essential-graph-margin-mixin)
			      complain-p
			      left right bottom top
			      left-margin right-margin
			      bottom-margin top-margin)
  "Check that new edges and margin is OK, complaining"
  (let ((error (verify-new-outside
		self
		left right bottom top
		left-margin right-margin bottom-margin top-margin)))
    (when (and error complain-p)
	  (cerror error nil))
    ;; BW: return error, rather than success flag
    error))

(defmethod verify-new-outside or ((self essential-graph-margin-mixin)
				   left right bottom top
				   left-margin right-margin
				   bottom-margin top-margin)
  "Return a error message if you don't like these edges and margins.
   This method just checks that there is room left to plot."
  (when (or (< (- right left left-margin right-margin) 0)
	    (< (- top bottom bottom-margin top-margin) 0))
    "Not enough room for inside of graph."))

(defmethod SCREEN-OUTSIDE ((self ESSENTIAL-GRAPH-MARGIN-MIXIN) STREAM)
  "Returns outside edges of graph in screen coordinates."
  ;;(declare (values left right bottom top))
  (multiple-value-bind (le re be te) (uv-outside self)
    (multiple-value-setq (le be) (uv-to-screen stream le be))
    (multiple-value-setq (re te) (uv-to-screen stream re te))
    (values le re be te)))

(defmethod SCREEN-INSIDE ((self ESSENTIAL-GRAPH-MARGIN-MIXIN)  STREAM)
  "Returns inside edges of graph in screen coordinates."
  ;;(declare (values left right bottom top))
  (with-slots (ull vll uur vur) self
    (multiple-value-bind (le be) (uv-to-screen stream ull vll)
      (multiple-value-bind (re te) (uv-to-screen stream uur vur)
	(values le re be te)))))


;;; This is now used by graphs, sliders, and annotations so that an object
;;; knows when it is displayed.
(defclass essential-display-mixin ()
  ((Displayed? :initform nil :initarg :displayed? :accessor displayed?)))

(defmethod display :after ((self essential-display-mixin) (STREAM t))
  (setf (displayed? self) t))

(defmethod erase :after ((self essential-display-mixin) (STREAM t))
  (setf (displayed? self) nil))

(defmethod refresh ((self essential-display-mixin) STREAM)
  "Erase, then draw."
  (erase self stream)
  (display self STREAM))


(defclass BASIC-GRAPH-DRAW-MIXIN (basic-graph-coordinates-mixin) ()
  (:documentation "Provide basic draw functionality for drawing lines and text."))

(let ((cosines-cache (make-hash-table)))
  (defun ROTATION-COSINES (rotation)
    (let ((cache cosines-cache))
      (multiple-value-bind (value foundp)
	  (gethash rotation cache)
	(when (not foundp)
	  (setq value (cis rotation))
	  (setf (gethash rotation cache) value))
	(values (realpart value) (imagpart value))))))

(defmethod char-position-relative-to-uv
	   ((self BASIC-GRAPH-DRAW-MIXIN) STREAM x-rel y-rel dx dy rotation)
  ;; This assumes fixed-width fonts, and should be made obsolete.  jpm.
  ;; m-X drop unnecessary funcalls
  (or (zerop dx) (setq dx (* dx (stream-character-width stream))))
  (or (zerop dy) (setq dy (* dy (stream-line-height stream))))
  (let ((c 1.0) (s 0.0))
    (or (zerop rotation) (multiple-value-setq (c s) (rotation-cosines rotation)))
    (values (values (round (+ x-rel (* c dx) (- (* s dy)))))
	    (values (round (+ y-rel (* s dx) (* c dy)))))))

(defmethod xy-draw-line ((self basic-graph-draw-mixin) STREAM x1 y1 x2 y2 &rest keys)
  (multiple-value-setq (x1 y1) (xy-to-uv self x1 y1))
  (multiple-value-setq (x2 y2) (xy-to-uv self x2 y2))
  (apply #'device-draw-line stream x1 y1 x2 y2 keys))

(defmethod xy-draw-lines ((self basic-graph-draw-mixin) STREAM points &rest keys)
  "Draw a line between points x1 y1 y2 y2 ..."
  (let ((ds (or (getf keys :dash-ds) 0)))
    (multiple-value-bind (x1 y1)
	(xy-to-uv self (first points) (second points))
      (do ((x1 x1 x2)
	   (y1 y1 y2)
	   (x2 (third points) (third points))
	   (y2 (fourth points) (fourth points))
	   (points (cddr points) (cddr points)))
	  ((null points))
	(multiple-value-setq (x2 y2) (xy-to-uv self x2 y2))
	(setq ds (apply #'device-draw-line stream x1 y1 x2 y2
			:dash-ds ds keys))))
    ds))

(defmethod xy-draw-point ((self basic-graph-draw-mixin) x y &rest keys)
  (with-slots (stream) self
    (multiple-value-bind (u v) (xy-to-uv self x y)
      (apply #'device-draw-point stream u v keys))))

(defmethod text ((self basic-graph-draw-mixin) STREAM u v message &rest keys)
  "Print message on graph with lower left of first character at last position moved to."
  (multiple-value-setq (u v) (uv-to-screen stream u v))
  (apply #'device-text stream u v
	 (cond ((stringp message) message)
	       ((characterp message) (string message))
	       ((floatp message) (float-to-string message))
	       (t (format nil "~a" message)))
	 keys))


(defclass BASIC-GRAPH 	(essential-graph-margin-mixin
			 basic-graph-draw-mixin
			 essential-display-mixin)
  ((edge-spec :initform nil :initarg :edge-spec))) ; How edges were specified.

(defmethod display ((graph basic-graph) (STREAM t))
  "Render a graph on its display medium.  Primary method is a stub."
  nil)

;;; KRA: temporary place for this.  Generalize so fraction is in terms
;;; of a "superior" 
(defmethod set-uv-outside :around ((self basic-graph) STREAM left right bottom top 
				   &optional verifyp)
  "Lets edges be defined in terms of fractions of the window size."
  (with-slots (edge-spec) self
    (flet ((is-fractional (number)
	     "Is NUMBER a fraction?"
	     (and (numberp number) (<= 0.0 number 1.0))))
      (setq edge-spec (list left right bottom top))
      (multiple-value-bind (screen-width screen-height) (stream-viewport-size stream)
	(when (and stream
		   (is-fractional left) (is-fractional right)
		   (is-fractional bottom) (is-fractional top))
	  (setq left   (+ (* left screen-width)
			  *width-between-graphs*)
		right  (- (* right screen-width)
			  *width-between-graphs*)
		bottom (+ (* bottom screen-height)
			  *height-between-graphs*)
		top    (- (* top screen-height)
			  *height-between-graphs*)))
	(call-next-method self STREAM
			  (values (round left)) (values (round right))
			  (values (round bottom)) (values (round top))
			  verifyp)))))

(defmethod uv-outside-from ((self basic-graph) STREAM &rest where)
  "Set edges from specification.  WHERE can be:
 NIL                         - Reset edges from current edge-spec.
 :mouse                      - Specify with mouse
 :window                     - Fill the window.
 ((row rows) (col cols))     - Position graph in position (row col) of a
                               rows by cols matrix of graphs.
 (left bottom right top)     - Position of lower left and upper right corners of graph.
                              If fractions, that fraction of window is used.  Otherwise,
                              absolute pixel positions are used."
  (with-slots (edge-spec) self
    (setq where (or (and where (copy-list where))
		    edge-spec
		    (list :window)))
    (cond ((eq (first where) :mouse) (mouse-specify-edges self STREAM))
	  ((eq (first where) :window) (window-specify-edges self STREAM))
	  ((listp (first where)) (matrix-specify-edges self STREAM where))
	  ((numberp (first where)) (apply #'set-uv-outside self STREAM where))
	  (t (error "(graph uv-outside-from): invalid where specification ~a" where)))
    (setq edge-spec where)))

(defmethod window-specify-edges ((self basic-graph) STREAM)
  (set-uv-outside self STREAM 0.0 1.0 0.0 1.0))

(defmethod mouse-specify-edges ((self basic-graph) STREAM)
  (with-slots (displayed?) self
    (multiple-value-bind (left top right bottom)
	(device-specify-rectangle stream)
      (if displayed? (erase self STREAM))
      (set-uv-outside self STREAM left right bottom top))))

(defmethod matrix-specify-edges ((self basic-graph) STREAM spec)
  (let ((ROW (first (first SPEC)))
	(ROWS (second (first SPEC)))
	(COL (first (second SPEC)))
	(COLS (second (second SPEC))))
    (let ((dx (/ 1.0 cols))
	  (dy (/ 1.0 rows)))
      (set-uv-outside self STREAM
		      (* col dx) (* (1+ col) dx)
		      (* (- rows row 1) dy) (* (- rows row) dy)))))

(defmethod graph-with-clipping ((self basic-graph) STREAM inside-p continuation)
  ;; Internal to WITH-CLIPPING-TO-GRAPH macro.
  (multiple-value-bind (le re be te)
      (if inside-p (screen-inside self STREAM) (screen-outside self STREAM))
    (with-clipping-screen-rectangle (stream le re te be) 
      ;; (draw-rectangle le re be te :filled nil :stream stream)
      (funcall continuation))))

(defmethod default-text-style ((graph basic-graph) stream)
  (medium-text-style stream))

(defmacro WITH-CLIPPING-TO-GRAPH ((graph STREAM inside-p) &body body)
  "Perform body while constraining clipping to GRAPH.  If INSIDE-P,
   the clipping rectangle is the inside of the graph, otherwise it is the
   outside." 
  `(graph-with-clipping
     ,graph ,STREAM ,inside-p
     #'(lambda ()
	 ,@body)))


;;; put commands in both the GRAPH and GLOBAL command tables.
(defmacro define-graph-command (name arguments &body body)
  `(progn (define-command (,name :command-table :graph) ,arguments ,@body)
	  (install-command :global ',name)))


