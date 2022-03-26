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

;;; SOME GRAPH MIXINS



(defclass GRAPH-MOUSE-RESOLUTION-MIXIN (basic-graph)
  ((dx-mouse :initform nil :initarg :dx-mouse)  ; Resolution (in units)
   (dy-mouse :initform nil :initarg :dy-mouse)) ; used to convert from
						; mouse -> xy coordinates
 )

(defmethod rescale :after ((self graph-mouse-resolution-mixin))
  (compute-mouse-resolution self))

(defmethod compute-mouse-resolution ((self graph-mouse-resolution-mixin))
  ;;This method provides at least 1 pixel accuracy.
  (with-slots (dx-mouse dy-mouse x-u-scale y-v-scale) self
    (setq dx-mouse (expt 10.0 (values (floor (log (/ x-u-scale) 10))))
	  dy-mouse (expt 10.0 (values (floor (log (/ y-v-scale) 10)))))))

(defmethod uv-to-xy :around ((self graph-mouse-resolution-mixin) u v)
  (with-slots (dx-mouse dy-mouse) self
    (multiple-value-bind (x y)
	(call-next-method self u v)
      (values (* (values (round x dx-mouse)) dx-mouse)
	      (* (values (round y dy-mouse)) dy-mouse)))))


;;; This should probably be several mixins, its gaining a lot of baggage...
(defclass GRAPH-BORDER-MIXIN (basic-graph named-mixin) 
  ((show-border :initform t :initarg :show-border :accessor show-border)	
   (tick-size :initform 7. :initarg :tick-size)	   ;In pixels.
   (title :initform nil :initarg :title :reader title) ;Title centered
   
   (x-label :initform nil :initarg :x-label        ;Label for left x axis.
	    :accessor x-label)
   (x-digits :initform 6 :initarg :x-digits :accessor x-digits)
   (x-auto-tick :initform t :initarg :x-auto-tick)  ;Auto-tick?
   (x-dtick :initform nil :initarg :x-dtick)        ;Tk Spacing if not auto.
   (x-tick-numbering :initform :minimal	           ; :minimal, :each, or nil.
		     :initarg :x-tick-numbering)

   (y-label :initform nil :initarg :y-label :accessor y-label)
   (y-digits :initform 6 :initarg :y-digits :accessor y-digits)
   (y-auto-tick :initform t :initarg :y-auto-tick)
   (y-dtick :initform nil :initarg :y-dtick)
   (y-tick-numbering :initform :minimal
		     :initarg :y-tick-numbering)
   (visible-borders :initform (copy-list '(:left :bottom :right :top))
		    :initarg :visible-borders
		    :accessor visible-borders))

  (:documentation "Simple Border and axis labeling."))

(defmethod initialize-instance :after ((self graph-border-mixin)
					&key &allow-other-keys)
  (with-slots (title) self
    (when title (setf (title self) title))))

(defmethod (setf title) (new-title (self graph-border-mixin))
  (with-slots (title) self
    (setq title 
	  (when title
	    (cond ((stringp new-title) new-title)
		  ((symbolp new-title) (string new-title))
		  (t (format nil "~a" new-title)))))))

(defmethod (setf x-dtick) (arg (self graph-border-mixin))
  (declare (ignore arg))
  (with-slots (x-auto-tick) self
    (setq x-auto-tick nil)))

(defmethod (setf y-dtick) (arg (self graph-border-mixin))
  (declare (ignore arg))
  (with-slots (y-auto-tick) self
    (setq y-auto-tick nil)))

(defmethod COMPUTE-MARGINS :around ((self GRAPH-BORDER-MIXIN) STREAM)
  (multiple-value-bind (left right bottom top) (call-next-method)
    (with-slots (x-tick-numbering) self
      (values left	
	      (+ (stream-character-width stream) right)	; 1 character right
	      (+ (* (if x-tick-numbering 3 2)	; X numbers and label
		    (stream-line-height stream)) bottom)
	      (+ (* 2 (stream-line-height stream)) top))))) ; title + 1

(defmethod axis-number-horizontal ((self GRAPH-BORDER-MIXIN) STREAM
				   x y number-string the-tick-size)
  "Put a centered number string at current u v position on a horizontal axis."
  (multiple-value-bind (cu cv)
      (char-position-relative-to-uv
	self STREAM x y (- (* (length number-string) 0.5))
	(if (> the-tick-size 0) -1.0 0.5) 0)
    (text self STREAM cu cv number-string :alu %draw)))

(defmethod axis-number-vertical ((self GRAPH-BORDER-MIXIN) STREAM
				 x y number-string the-tick-size)
  "Put a centered number string at current u v position on a vertical axis."
  (multiple-value-bind (cu cv)		; Y axis.
      (char-position-relative-to-uv
       self STREAM x y (if (> the-tick-size 0) 0.5
			   (- (+ (length number-string) 0.5))) -.5 0)
    (setq cu (max 10 cu))		; Make sure it fits on window!?
    (text self STREAM cu cv number-string :alu %draw)))

(defmethod AXIS-LABEL-HORIZONTAL ((self GRAPH-BORDER-MIXIN) STREAM
				  x y label the-tick-size)
  ;; Put label centered below numbers
  (with-slots (x-tick-numbering) self
    (multiple-value-bind (cu cv)
	(char-position-relative-to-uv
	  self STREAM x y (- (* (length label) 0.5))
	  (if (< the-tick-size 0) 1.5
	     (if (null x-tick-numbering) -1.0 -2.0))
	  0)
      (text self STREAM cu cv label :alu %draw))))

(defmethod AXIS-LABEL-VERTICAL ((self GRAPH-BORDER-MIXIN) STREAM
				x y label the-tick-size)
  (declare (ignore the-tick-size))
  (with-slots (y-tick-numbering) self
    (do ((x-char (if (null y-tick-numbering) -2.0 
		     (+ 3 (- (/ (left-margin-size self)
				(stream-character-width STREAM))))))
	 (y-char (- (* (length label) 0.5) 1.0)  (- y-char 1.0))
	 (ichar 0 (1+ ichar)))
	((>= ichar (length label)) nil)
      (multiple-value-bind (cu cv)
	  (char-position-relative-to-uv self STREAM x y x-char y-char 0)
	(text self STREAM cu cv (aref label ichar) :alu %draw)))))

;;; These methods determine what the tick spacing should be.
(defmethod x-tick-spacing ((self GRAPH-BORDER-MIXIN))
  (with-slots (x-auto-tick xll xur x-dtick) self
    (if x-auto-tick (auto-tick xll xur) x-dtick)))

(defmethod y-tick-spacing ((self GRAPH-BORDER-MIXIN))
  (with-slots (y-auto-tick yll yur y-dtick) self
    (if y-auto-tick (auto-tick yll yur) y-dtick)))

(defmethod display-x-label ((self graph-border-mixin) stream)
  (with-slots (ull uur vll tick-size) self
    (let ((label (x-label self))
	  (x (values (round (+ ull uur) 2)))
	  (y vll))
      (when label
	(axis-label-horizontal self STREAM x y
			       label tick-size)))))

(defmethod display-y-label ((self graph-border-mixin) stream)
  (with-slots (ull vll vur tick-size) self
    (let ((label (y-label self))
	  (x ull)
	  (y (values (round (+ vll vur) 2))))
      (when label
	(axis-label-vertical self STREAM x y label
			     tick-size)))))

(defmethod display-title ((self graph-border-mixin) STREAM)
  (with-slots (uur ull vur) self
    (let (the-title)
      (when (setq the-title (title self))
	(multiple-value-bind (cu cv)
	    (char-position-relative-to-uv self STREAM
					  (values (truncate (+ uur ull) 2)) vur
					  (- (/ (length the-title) 2.0)) 0.5 0)
	  (text self STREAM cu cv the-title :alu %draw))))))

(defmethod display-labels ((self graph-border-mixin) stream)
  (display-x-label self stream)
  (display-y-label self stream)
  (display-title self stream)
  (force-output stream))

(defmethod display-border ((self graph-border-mixin) STREAM)
  (when (show-border self)
    (with-clipping-to-graph (self STREAM nil)
      (let* ((visible (visible-borders self))
	     (drawer (make-optimized-line-displayer %draw 1 t))
	     (line-drawer
	      #'(lambda (x1 y1 x2 y2)
		  (multiple-value-setq (x1 y1) (uv-to-screen stream x1 y1))
		  (multiple-value-setq (x2 y2) (uv-to-screen stream x2 y2))
		  (funcall drawer stream x1 y1 x2 y2))))
	(when (member :right visible)
	  (display-right-border self STREAM line-drawer))
	(when (member :top visible)
	  (display-top-border self STREAM line-drawer))
	(when (member :left visible)
	  (display-left-border self STREAM line-drawer))
	(when (member :bottom visible)
	  (display-bottom-border self STREAM line-drawer))
	(when (member :zero-abcissa visible)
	  (display-zero-abcissa self stream line-drawer))
	(when (member :zero-ordinate visible)
	  (display-zero-ordinate self stream line-drawer))
	(force-output stream)
	))))

(defmethod DISPLAY-ZERO-ABCISSA ((self GRAPH-BORDER-MIXIN) STREAM line-drawer)
  (with-slots (tick-size ull vll uur xll xur x-tick-numbering)
      self
    (let ((dtick (x-tick-spacing self))
	  (dtick-size tick-size))
      (multiple-value-bind (u0 v0) (xy-to-uv self 0 0)
	(declare (ignore u0))
	(linear-axis ull v0 uur v0 xll xur dtick 
		     tick-size x-tick-numbering line-drawer
		     #'(lambda (x y number)
			 (axis-number-horizontal
			  self STREAM
			 (if (zerop number) (+ x (stream-character-width stream)) x)
			  y
			  (float-to-string number (x-digits self))
			  dtick-size))
		     nil)))))


(defmethod DISPLAY-BOTTOM-BORDER ((self GRAPH-BORDER-MIXIN) STREAM line-drawer)
  (with-slots (tick-size ull vll uur xll xur x-tick-numbering)
	      self
    (let ((dtick (x-tick-spacing self))
	  (dtick-size tick-size))
      (linear-axis ull vll uur vll xll xur dtick 
		   tick-size x-tick-numbering line-drawer
		   #'(lambda (x y number)
		       (axis-number-horizontal
			 self STREAM x y
			 (float-to-string number (x-digits self))
			 dtick-size))
		   nil))))

(defmethod display-top-border ((self graph-border-mixin) STREAM line-drawer)
  (declare (ignore stream))
  (with-slots (tick-size ull vll uur xll xur x-tick-numbering vur)
	      self
    (let ((dtick (x-tick-spacing self)))
      (linear-axis ull vur uur vur xll xur dtick 
		   (- tick-size) nil line-drawer
		   nil
		   nil))))

(defmethod DISPLAY-ZERO-ORDINATE ((self GRAPH-BORDER-MIXIN) STREAM line-drawer)
  (with-slots (tick-size ull vll vur yll yur y-tick-numbering) self
    (let ((dtick (y-tick-spacing self))
	  (dtick-size (- tick-size)))
      (multiple-value-bind (u0 v0) (xy-to-uv self 0 0)
	(declare (ignore v0))
	(linear-axis u0 vll u0 vur yll yur dtick 
		     dtick-size y-tick-numbering line-drawer
		     #'(lambda (x y number)
			 (axis-number-vertical
			  self STREAM
			  x
			  (if (zerop number) (+ y (stream-line-height stream)) y)
			  (float-to-string number (y-digits self))
			  dtick-size))
		     nil)))))

(defmethod DISPLAY-LEFT-BORDER ((self GRAPH-BORDER-MIXIN) STREAM line-drawer)
  (with-slots (tick-size ull vll vur yll yur y-tick-numbering) self
    (let ((dtick (y-tick-spacing self))
	  (dtick-size (- tick-size)))
      (linear-axis ull vll ull vur yll yur dtick 
		   dtick-size y-tick-numbering line-drawer
		   #'(lambda (x y number)
		       (axis-number-vertical self STREAM x y
					     (float-to-string number (y-digits self))
					     dtick-size))
		   nil))))

(defmethod DISPLAY-RIGHT-BORDER ((self GRAPH-BORDER-MIXIN) STREAM line-drawer)
  (declare (ignore stream))
  (with-slots (tick-size ull vll vur yll yur y-tick-numbering uur) self
    (let ((dtick (y-tick-spacing self)))
      (linear-axis uur vll uur vur yll yur dtick 
		   tick-size nil line-drawer
		   nil
		   nil))))

(defmethod display :after ((self graph-border-mixin) STREAM)
  (display-border self STREAM)
  (display-labels self stream))

(defclass GRAPH-BORDER-OB-MIXIN (graph-border-mixin)
    ())


(defclass HORIZONTAL-Y-BORDER-MIXIN (graph-border-mixin)
  ()
  (:documentation "Y axis labeling without rotation."))

;;; KRA: This assumes the potential of having y-digits + 6 more charaters.
;;; this should really called something else so 6 isn't hard coded.
;;; KRA 27JUL93: The 6 is now a 4
(defmethod compute-margins :around ((self horizontal-y-border-mixin) STREAM)
  (multiple-value-bind (left right bottom top)
      (call-next-method)
    (with-slots (y-tick-numbering y-digits) self
      (let ((extra (* (if (and y-tick-numbering y-digits) (+ y-digits 4) 3)
		      (stream-character-width stream))))
	(values (+ extra left) right bottom top)))))


(defclass VERTICAL-Y-BORDER-MIXIN (graph-border-mixin)
  ()
  (:documentation "Y axis labels with rotated characters."))

(defmethod COMPUTE-MARGINS :around ((self VERTICAL-Y-BORDER-MIXIN) STREAM)
  (multiple-value-bind (left right bottom top)
      (call-next-method)
    (with-slots (y-tick-numbering) self
      (values (+ (* (if y-tick-numbering 3 2)
		    (stream-line-height stream)) left)
	      right
	      bottom
	      top))))

(defmethod AXIS-NUMBER-VERTICAL ((self VERTICAL-Y-BORDER-MIXIN) STREAM x y
				 number-string the-tick-size)
  "Put centered number string at current u v position on a vertical axis."
  (declare (ignore the-tick-size))
  (decf x (string-size stream nil number-string))
  (decf y (values (truncate (- (stream-line-height stream) 2) 2)))
  (setq x (max 10 x))
  (text self STREAM x y number-string :alu %draw))

(defmethod AXIS-LABEL-VERTICAL ((self VERTICAL-Y-BORDER-MIXIN) STREAM
				x y label the-tick-size)
  (with-slots (x-tick-numbering) self
    (multiple-value-bind (cu cv)
	(char-position-relative-to-uv
	  self STREAM x y (- (* (length label) 0.5))
	  (if (< the-tick-size 0) 1.5
	     (if (null x-tick-numbering) -1.0 -2.0))
	  #.(/ pi 2))
      (text self STREAM cu cv label :alu %draw :rotation #.(/ pi 2)))))


(defclass GRAPH-GRID-MIXIN (basic-graph) 
  ((show-grid :initform nil :initarg :show-grid)   ; Show grid?
   (x-auto-grid :initform t :initarg :x-auto-grid) ; Auto-space grid?
   (x-dgrid :initform nil :initarg :x-dgrid :accessor x-dgrid)       ; Spacing if not
   (y-auto-grid :initform t :initarg :y-auto-grid)
   (y-dgrid :initform nil :initarg :y-dgrid :accessor y-dgrid))
  (:documentation "Simple Grid overlay."))

(defmethod (SETF X-DGRID) :after (ignore (self graph-grid-mixin))
    (declare (ignore ignore))
    (setf (slot-value self 'x-auto-grid) nil))

(defmethod (setf Y-DGRID) :after (ignore (self graph-grid-mixin))
    (declare (ignore ignore))
    (setf (slot-value self 'y-auto-grid) nil))

(defmethod alu-for-grid ((self graph-grid-mixin) stream)
  (declare (ignore stream))
  ;; a dark gray.
  (clim:make-rgb-color .4 .4 .4))

;;; find U value of first grid line from left (or bottom)
(defun FIND-UFIRST (usmall interval)
  (let ((ufirst (+ (down usmall interval) interval)))
    ;; "ufirst" wants to be 0 but isn't due to roundoff. So make it 0.
    (when (< (/ (abs ufirst) interval) 0.001)
      (setq ufirst 0.0))
    (when (< (abs (- ufirst usmall)) (* 0.1 interval))
      (setq ufirst (+ ufirst interval)))
    ufirst))
  
(defun LINEAR-GRAPH-GRID
       (the-graph				; Graph flavor to draw grid on.
	STREAM
	xmin ymin				; Coordinates of axis on x-y window
	xmax ymax				; from which to draw grid lines
						; (dont confuse with graph outline)
	umin umax				; Axis units corres. to min and max points
	dgrid					; Grid spacing in axis units.
	grid-size				; length of grid-line (in pixels!!)
	)
  ;;  plot grid lines from a given axis
  ;;   modified from linear-axis - without the axis, the numbers and the labels
  ;;    grid lines instead of ticks.  Assume grids are drawn at 0 or 90 degrees.
  (let ((cos (if (eq xmax xmin) 0 1))
	(sin (if (eq ymax ymin) 0 1))
	(usmall (min umin umax))		; Minimum u value
	(ularge (max umin umax))		; Maximum u value
	(alu (alu-for-grid the-graph stream)))
    (setq dgrid (abs dgrid))
    ;; just like ticks, except tick size is the size of the other axis
    (loop for u from (find-ufirst usmall dgrid) below ularge by dgrid
	  with u-grid = (values (round (* grid-size sin)))
	  with v-grid = (values (round (* grid-size cos)))
	  as xnew = (+ xmin (* (- u umin) cos))	;(x-along u)
	  as ynew = (+ ymin (* (- u umin) sin))	;(y-along u)
	  doing
      (multiple-value-bind (u1 v1)
	  (xy-to-uv the-graph xnew ynew)
	(device-draw-line STREAM u1 v1
			  (+ u1 u-grid) (+ v1 v-grid) :alu alu)))))

(defun auto-grid (xmin xmax)
  ;; use default values for ticks as defaults for grids
  (auto-tick xmin xmax))

(defmethod DISPLAY-HORIZONTAL-GRID ((self graph-grid-mixin) STREAM)
  (with-slots (yll yur y-dgrid uur ull xll y-auto-grid) self
    (let ((dgrid (cond (y-auto-grid (auto-grid yll yur))
		       (t y-dgrid)))
	  (grid-size (- uur ull)))
      (linear-graph-grid self STREAM xll yll xll yur yll yur dgrid grid-size))))

(defmethod DISPLAY-VERTICAL-GRID ((self graph-grid-mixin) STREAM)
  (with-slots (xll xur x-dgrid vll vur yll x-auto-grid) self
    (let ((dgrid (cond (x-auto-grid (auto-grid xll xur))
		       (t x-dgrid)))
	  (grid-size (- vur vll)))		; pixels
      (linear-graph-grid self STREAM xll yll xur yll xll xur dgrid grid-size))))

(defmethod DISPLAY-GRID ((self graph-grid-mixin) STREAM)
  (with-slots (show-grid) self
    (when show-grid
      (display-vertical-grid self STREAM)
      (display-horizontal-grid self STREAM))))

(defmethod DISPLAY :before ((self graph-grid-mixin) STREAM)
  (display-grid self STREAM))

(defclass GRAPH-GRID-OB-MIXIN (graph-grid-mixin) ())


(defclass GRAPH-DATASETS-MIXIN (graph-border-mixin basic-graph)
  ((datasets :initform nil :initarg :datasets :reader datasets)
   (hidden-datasets :initform nil :initarg :datasets :accessor hidden-datasets))
  (:documentation 
   "Allows several sets of data to be displayed on the graph, each in its own way."))

(defmethod (setf hidden-datasets) :after ((new t) (graph GRAPH-DATASETS-MIXIN))
  (setf (auto-scale-needed graph) t))

(defmethod after-fasd-forms ((self graph-datasets-mixin))
  (with-slots (datasets) self
    `((setf (datasets ',self) ',datasets))))

(defmethod (setf datasets) (new-datasets (self graph-datasets-mixin))
  (with-slots (datasets) self
    (setq datasets nil)
    (dolist (dataset new-datasets)
      (add-dataset self dataset))))

(defmethod initialize-instance :after ((self graph-datasets-mixin)
				       &key &allow-other-keys)
  (with-slots (datasets) self
    (when datasets (setf (datasets self) datasets))))

(defmethod display :after ((self graph-datasets-mixin) STREAM)
  (graph-display-data self STREAM))

(defmethod rescale :after ((self graph-datasets-mixin))
  (dolist (d (datasets self)) (rescale d)))

(defmethod add-dataset ((self graph-datasets-mixin) dataset)
  (with-slots (datasets) self
    (when (not (member dataset datasets :test #'eq))
      (if datasets (setf (cdr (last datasets)) (cons dataset nil))
	  (setq datasets (cons dataset nil))))))

;;; KRA: This used to take name-or-dataset, now it just takes dataset.
(defmethod remove-dataset ((self graph-datasets-mixin) dataset)
  (with-slots (hidden-datasets datasets) self
    (setq datasets (delete dataset datasets :test #'eq))
    (setq hidden-datasets (delete dataset hidden-datasets :test #'eq))))

(defmethod graph-display-data :around ((self graph-datasets-mixin) STREAM)
  (with-clipping-to-graph (self STREAM t)
    (call-next-method self STREAM)))

(defmethod graph-display-data ((self graph-datasets-mixin) STREAM)
  (let ((hidden (hidden-datasets self)))
    (dolist (set (datasets self))
      (or (member set hidden) (display-data set STREAM self)))
    (Force-output stream)))

(defmethod x-label :around ((self graph-datasets-mixin))
  (or (call-next-method)
      (some #'x-label (datasets self))))

(defmethod y-label :around ((self graph-datasets-mixin))
  (or (call-next-method)
      (some #'y-label (datasets self))))

;;;NLC08NOV90 - The g8>graph> version returns a string or nil. Whereas this
;;;		returns a string or " ".
(defmethod title :around ((self graph-datasets-mixin))
  (let ((the-title nil))
    (setq the-title (or (call-next-method)
			(some #'title (datasets self))
			(name-string self)))
    (when the-title
      (unless (stringp the-title)
	(setq the-title (format nil "~a" the-title))))
    (or the-title " ")))


(defclass GRAPH-DATASETS-OB-MIXIN (graph-datasets-mixin) ())


(defclass GRAPH-AUTO-SCALE-MIXIN (graph-datasets-mixin basic-graph) 
  ((auto-scale-needed :initform nil	; Data changed since last auto-scale?
		      :initarg :auto-scale-needed
		      :accessor auto-scale-needed)
   (auto-scale :initform :both		; Auto scale: (choose :x :y :both nil)
	       :initarg :auto-scale
	       :accessor auto-scale))
  (:documentation
   "Allows the axes of a graph to be automatically scaled from its datasets."))

(defmethod add-dataset :after ((self graph-auto-scale-mixin) ignore)
  (declare (ignore ignore))
  (with-slots (auto-scale-needed) self
    (setq auto-scale-needed t)))

(defmethod remove-dataset :after ((self graph-auto-scale-mixin) ignore)
  (declare (ignore ignore))
  (with-slots (auto-scale-needed) self
    (setq auto-scale-needed t)))

;;; Never auto-scale if user changes axis definitions.
(defmethod (setf xur) :after ((new t) (self graph-auto-scale-mixin))
  (setf (auto-scale self)
	(case (auto-scale self) (:both :y) (:x nil) (otherwise (auto-scale self)))))

(defmethod (setf xll) :after ((new t) (self graph-auto-scale-mixin))
  (setf (auto-scale self)
	(case (auto-scale self) (:both :y) (:x nil) (otherwise (auto-scale self)))))

(defmethod (setf yll) :after ((new t) (self graph-auto-scale-mixin))
  (setf (auto-scale self)
	(case (auto-scale self) (:both :x) (:y nil) (otherwise (auto-scale self)))))

(defmethod (setf yur) :after ((new t) (self graph-auto-scale-mixin))
  (setf (auto-scale self)
	(case (auto-scale self) (:both :x) (:y nil) (otherwise (auto-scale self)))))

(defmethod display :before ((self graph-auto-scale-mixin) STREAM)
  (declare (ignore stream))
  (with-slots (auto-scale auto-scale-needed) self
    (and auto-scale auto-scale-needed (do-auto-scale self))))

(defmethod (setf auto-scale) ((self graph-auto-scale-mixin) type)
  ;; Tells graph to do auto-scaling before graph is redisplayed.  Type may be:
  ;; :x      Auto scale the X axis.
  ;; :y      Auto scale the Y axis.
  ;; :both   Auto scale both axes
  ;; nil     No auto scaling.
  (if (not (member type '(:x :y :both nil)))
      (error "~&(auto-scale graph-datasets-mixin): ~a is invalid option" type))
  (with-slots (auto-scale auto-scale-needed) self
    (setq auto-scale-needed t)
    (setq auto-scale type)  ;;;NLC08NOV90 - This should return the value of AUTO-SCALE.
    ))

(defmethod graph-auto-scale-limits ((self graph-auto-scale-mixin))
  "Returns limits needed to show all datasets."
  (with-slots (datasets hidden-datasets xll xur yll yur auto-scale) self
    (loop with xmin and xmax and ymin and ymax
	for dataset in datasets
	as limits = (unless (member dataset hidden-datasets)
		      (auto-scale-limits dataset auto-scale
					 xll xur yll yur))
	when limits
	do (multiple-value-bind (left right bottom top)
	       (apply #'values limits)
	     (if (or (null xmin) (and left   (< left xmin)))
		 (setq xmin left))
	     (if (or (null xmax) (and right  (> right xmax)))
		 (setq xmax right))
	     (if (or (null ymin) (and bottom (< bottom ymin)))
		 (setq ymin bottom))
	     (if (or (null ymax) (and top    (> top ymax)))
		 (setq ymax top)))
	finally (return (values (or xmin 0) (or xmax 1)
				(or ymin 0) (or ymax 1))))))

(defmethod do-auto-scale :around ((self graph-auto-scale-mixin))
  ;; Only actually do auto scaling if you need to and can.
  (with-slots (auto-scale-needed datasets auto-scale) self
    (when (and auto-scale-needed datasets auto-scale)
      (call-next-method self)
      (rescale self)
      (setq auto-scale-needed nil))))

(defmethod do-auto-scale ((self graph-auto-scale-mixin))
  "Actually do the auto scaling."
  (with-slots (auto-scale xll xur yll yur) self
    (multiple-value-bind (xmin xmax ymin ymax)
	(graph-auto-scale-limits self)
      (when (member auto-scale '(:x :both) :test #'eq)
	(if xmin (setq xll xmin))
	(if xmax (setq xur xmax)))
      (when (member auto-scale '(:y :both) :test #'eq)
	(if ymin (setq yll ymin))
	(if ymax (setq yur ymax))))
    (when (= xll xur)			; Degenerate limits. set limits
      (decf xll 1.0)			;  so data will be plotted.
      (incf xur 1.0))
    (when (= yll yur)
      (decf yll 1.0)
      (incf yur 1.0))))
 

(defclass graph-limits-mixin (graph-auto-scale-mixin)
  ()
  (:documentation
    "Allows a graph or a dataset to restrict the limits
      of the graph after auto scaling."))

(defmethod limit-specs ((self graph-limits-mixin))
  "Returns a list of limits-specs that specify how the auto scaled limit of the
  LEFT, RIGHT, BOTTOM, and TOP (respectively) of the graph are restricted.
  A limit-spec can have the form:

  (min max) - (<= min limit max)
  (nil max) - (<= limit max)
  (min nil) - (<= min limit)
  number    - (= limit number)
  nil       - limit is unconstrained.

  If the list is (), the graph limits are unrestricted."
  ())

(defun limit-value (value limit)
  (cond ((null limit) value)
	((listp limit)
	 (when (first limit)
	   (setq value (max (first limit) value)))
	 (when (second limit)
	   (setq value (min (second limit) value)))
	 value)
	(t limit)))

(defmethod graph-auto-scale-limits :around ((self graph-limits-mixin))
  "Constrain graph edges to be within limits."
  (let ((the-limits (limit-specs self)))
    (multiple-value-bind (xmin xmax ymin ymax)
	(call-next-method)
      (when the-limits
	(multiple-value-bind (left right bottom top)
	    (apply #'values the-limits)
	  (when xmin (setq xmin (limit-value xmin left)))
	  (when xmax (setq xmax (limit-value xmax right)))
	  (when ymin (setq ymin (limit-value ymin bottom)))
	  (when ymax (setq ymax (limit-value ymax top)))))
      (values xmin xmax ymin ymax))))


(defclass GRAPH-AUTO-SCALE-EXTENSIONS-MIXIN (graph-auto-scale-mixin)
  ((auto-scale-extensions		; list of (left right bottom top) %
    :initform (list 5.0 5.0 5.0 5.0)	;of X or Y axis to extend when auto
    :initarg :auto-scale-extensions)	;scaling.
   ))

(defmethod graph-auto-scale-limits :around ((self graph-auto-scale-extensions-mixin))
  "Extend limits needed by the data."
  (multiple-value-bind (xmin xmax ymin ymax)
      (call-next-method)
    (with-slots (auto-scale-extensions) self
      (when auto-scale-extensions
      (multiple-value-bind (left right bottom top)
	  (apply #'values auto-scale-extensions)
	(when (and xmin xmax)
	  (let ((range (abs (- xmax xmin))))
	    (when xmin (setq xmin (- xmin (* 0.01 left   range))))
	    (when xmax (setq xmax (+ xmax (* 0.01 right  range))))))
	(when (and ymin ymax)
	  (let ((range (abs (- ymax ymin))))
	    (when ymin (setq ymin (- ymin (* 0.01 bottom range))))
	    (when ymax (setq ymax (+ ymax (* 0.01 top    range))))))))
      (values xmin xmax ymin ymax))))




;;; Clim 1.0 requires this be defined before any presentation type
;;; that depends on it.  Hence moved here from present.lisp.

(defclass presentable-graph-mixin ()
	  ((presentation :initform nil :accessor presentation)
	   (tick :initform 0 :accessor redisplay-tick)))

(defmacro with-temporary-cursor-position ((stream x y) &body body)
  `(with-output-truncation (,stream)
     (multiple-value-bind (.x. .y.) (stream-cursor-position* ,stream)
       (unwind-protect
	   (progn (stream-set-cursor-position* ,stream ,x ,y) ,@body)
	 (stream-set-cursor-position* ,stream .x. .y.)))))

(defmethod display :around ((self presentable-graph-mixin) STREAM)
  "Display the graph as a presentation."
  ;; Lessons learned.
  ;; 1. Don't do ERASE type operations inside of with-output-as-presentation.
  ;;     DISPLAY generates output; removing it should be done elsewhere.
  ;; 2. Enable output truncation to prevent unwanted viewport scrolling.
  ;; 3. Move the cursor to within the bounds of the graph, since cursor position
  ;;     affects mouse-sensitive area (clim 0.9 bug).
  (with-output-truncation (stream)
    (setf (presentation self)
      (with-redisplayable-output (:stream stream 
					  :unique-id self
					  :cache-value (redisplay-tick self)
					  :cache-test #'=)
	(multiple-value-bind (x1 x2 y1 y2) (screen-outside self stream)
	  (declare (ignore x2 y1))
	  (with-temporary-cursor-position (stream x1 y2)
	    (with-output-as-presentation
		(:stream STREAM
			 :single-box t
			 :object self
			 :type (graph-presentation-type self)
			 :allow-sensitive-inferiors 
			 (graph-present-inferiors-p self))
	      (call-next-method self STREAM)))))))
  (force-output stream))

(defun incrementally-redisplayable-presentation (presentation)
  "Determine if a presentation is a part of an incremental redisplay."
  (if (not presentation) nil
    (or 
     (typep presentation 'standard-updating-output-record)
     (incrementally-redisplayable-presentation
      (clim:output-record-parent presentation)))))

(defmethod erase ((self presentable-graph-mixin) stream)
  (with-output-truncation (stream)
    (let ((presentation (presentation self)))
      (when presentation
	(clim:erase-output-record presentation stream nil)
	(setq presentation nil)))))

(defmethod refresh :around ((self presentable-graph-mixin) stream)
  "By default, graphs refresh by erasing and then drawing.
   This breaks incremental redisplay, so watch out!"
  (let ((p (presentation self)))
    (cond ((and p (incrementally-redisplayable-presentation 
		   (clim:output-record-parent p)))
	   (incf (redisplay-tick self))
	   ;; Do nothing.  Expect redisplay-frame-panes to do the rest.
	   nil)
	  (t (call-next-method self stream)))))

(defmethod graph-presentation-type ((self presentable-graph-mixin)) 'graph)
(defmethod graph-present-inferiors-p ((self presentable-graph-mixin)) 't)
(defmethod present-self-p ((any t)) nil)
