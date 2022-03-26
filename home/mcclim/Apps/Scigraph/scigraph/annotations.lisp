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

;;;An annotation is an object that can be visualized on a graph.  Usually, it is a
;;;  bit of text, but sometimes annotations are arbitrary graphical shapes like
;;;  polygons, lines, etc.
;;;
;;;Annotations can be moved, edited, and deleted using the mouse, much like
;;;  MacIntosh MacDraw object.  Character styles can also be controlled.
;;;


(defclass basic-annotation (moving-point)
    ((graph :initform nil :initarg :graph :accessor graph)
     (alu :initform %draw :initarg :alu :accessor alu)
     ;; We could probably have a color-mixin since we have an alu variable.
     (style :initform nil :initarg :style :accessor style)
     (PRESENTATION :initform nil :initarg :presentation :accessor presentation)
     (editable :initform nil :initarg :editable :accessor editable))
  (:documentation "The essential characteristics of an annotation."))

(defmethod annotation-text ((any basic-annotation)) (princ-to-string any))

(defmethod annotation-p ((any t)) nil)
(defmethod annotation-p ((annotation basic-annotation)) t)

(defmethod editable ((object t)) nil)

(defmethod annotation-presentation-type ((self basic-annotation)) 'annotation)
(defmethod annotation-single-box ((self basic-annotation)) nil)

(defmethod display :around ((self basic-annotation) stream)
  "Set up the display environment and capture output as a presentation."
  ;; with-every-trick-in-the-book ...
  (with-slots (graph style) self
    ;; Check to see if I'm clipped.
    (when (setf (displayed? self) (display-p self))
      ;; Okay, try to display, but let the graph clip me too.
      (with-clipping-to-graph (graph stream nil)
	;; Move the cursor to shadow clim bugs.
	(with-character-style ((or style (medium-text-style stream))
			       stream)	
	  (multiple-value-bind (sl st) (uv-for-display self)
	    (multiple-value-setq (sl st) (uv-to-screen stream sl st))
	    (with-temporary-cursor-position (stream sl st)
	      (setf (presentation self)
		;; Capture the output of all the display methods in one big
		;; presentation, mainly so we can erase it later.
		(with-output-as-presentation 
		    (:stream stream
			     :object self
			     :single-box (annotation-single-box self)
			     :type (annotation-presentation-type self))
		  (call-next-method self stream))))))))))

(defmethod display-p ((self basic-annotation)) t)

(defmethod width ((self basic-annotation)) 0)
(defmethod height ((self basic-annotation)) 0)

(defmethod uv-for-display ((self basic-annotation))
  "Get the uv position of (left,top), but constrain it within the graph"
  (multiple-value-bind (l r b to) (uv-outside (graph self))
    (decf r (width self))
    (incf b (height self))
    (multiple-value-bind (left top) (uv-position self)
      (multiple-value-setq (left top)
	(constrain-point-in-rectangle left top l b r to))
      (values (truncate left) (truncate top)))))

(defmethod erase ((self basic-annotation) STREAM)
  "Erase an annotation by erasing the underlying presentation."
  (with-slots (presentation) self
    (when presentation
      (clim:erase-output-record presentation stream nil)
      (setq presentation nil))))

(defmethod kill ((self basic-annotation) STREAM)
  "Erase the annotation and resign from the graph."
  (with-slots (graph) self
    (erase self STREAM)				
    (setf (annotations graph) (remove self (annotations graph)))))

(defmethod draw-at ((self basic-annotation) stream x y)
  (set-stream-position self stream x y)
  (mark self stream))

(defmethod erase-at ((self basic-annotation) stream x y)
  (declare (ignore x y))
  (unmark self stream))

(defmethod unmark ((self basic-annotation) stream)
  "Undraw the box highlighting the annotation."
  ;; You cant just erase the presentation because we don't take time
  ;; to make them during mouse tracking.  If we use the :flip alu, then we can just
  ;; mark it again.
  (mark self stream))

(defmethod move :around ((self basic-annotation) stream)
  "Clip to graph-outside edges while moving the annotation."
  (with-clipping-to-graph ((graph self) stream nil)
    (call-next-method self stream)))

;; Stubs to hang methods upon.
(defmethod mark ((self basic-annotation) stream) stream)
(defmethod display ((self basic-annotation) stream) stream)
(defmethod figure-geometry ((self basic-annotation) stream) stream)
(defmethod recompute-annotation-appearance ((self basic-annotation) stream) stream)


;;;ANNOTATION
;;;
;;; This basic annotation type is simply a string that may contain return
;;; characters.  

(defclass ANNOTATION (basic-annotation)
    ((text :initform nil :initarg :text :accessor annotation-text)
     (width :initform 0 :initarg :width :accessor width)
     (height :initform 0 :initarg :height :accessor height)
     (ANGLE :initform 0 :initarg :angle :accessor angle))	; usually 0 or pi/2
  (:default-initargs :editable t))

(defmethod (setf width) (value (self basic-annotation))
  ;; it is crucial that width and height are fixnums.
  (setf (slot-value self 'width) (values (truncate value))))

(defmethod (setf height) (value (self basic-annotation))
  (setf (slot-value self 'height) (values (truncate value))))

(defmethod display-p ((self annotation))
  "Let the annotation decide when to clip itself."
  (with-slots (graph width height) self
    (multiple-value-bind (left top) (uv-position self)
      (or (uv-is-inside graph left top)
	  (uv-is-inside graph (+ left width) (+ top height) )))))

(defmethod display ((self annotation) stream)
  (let ((angle (angle self)))
    (multiple-value-bind (u v) (uv-for-display self)
      (multiple-value-setq (u v) (uv-to-screen stream u v))
      (let ((record (with-output-as-presentation (:stream stream
							  :object self
							  :type 'annotation
							  :single-box t)
		      (draw-text-internal stream u v (annotation-text self)
					  :alu (alu self)
					  :rotation angle
					  :stream stream ))))
	;; Even though we have dimensions by now, we can get a better estimate 
	;; because there is a presentation box to look at.
	(when record
	  (let ((*standard-output* stream)) ; bounding-rectangle cares!
	    (multiple-value-bind (le te re be) (bounding-rectangle* record)
	      (setf (width self) (- re le)
		    (height self) (- be te)))))))))

(defvar *annotation-whitespace* '(#\return #\space #\tab #\page))

(defmethod figure-text-dimensions ((self annotation) STREAM)
  "Compute the width and height that the text will cover on the stream."
  (with-slots (graph style width height text angle) self
    (let* ((whitespace *annotation-whitespace*)
	   (beginning 0))
      ;; If the string starts out with empty lines, hack them off.
      (dotimes (i (length text))
	(let ((ch (aref text i)))
	  (cond ((not (member ch whitespace :test #'char=)) (return))
		((char= ch #\return) (setq beginning (1+ i))))))
      (if (not (zerop beginning)) (setq text (subseq text beginning)))
      (setq text (string-trim whitespace text))
      (multiple-value-setq (width height)
	(string-size stream style text))
      (unless (zerop angle) (rotatef width height))
      (values width height)
      )))

(defgeneric rescale-annotation (SELF)
  (:method-combination progn :most-specific-last)
  (:documentation "Called when the graph has been rescaled."))

(defmethod rescale-annotation progn ((self t)) nil)

(defmethod figure-geometry ((self annotation) STREAM)
  (with-slots (presentation width height angle) self
    (with-output-truncation (stream)
      (figure-text-dimensions self STREAM))
    (if (< (abs (- angle (/ pi 2))) single-float-epsilon)
	;; Kludge the case of an annotation rotated 90 degrees.
	(setq height (- height)))))

(defmethod set-text ((self annotation) STREAM new-string)
  ;; This method defined for convenience but not strictly necessary...
  (setf (annotation-text self) new-string 
	(presentation self) nil)
  (figure-geometry self STREAM))

(defmethod mark ((self annotation) stream)
  "Draw a rectangle marking the edges of the annotation."
  (with-slots (width height) self
    (multiple-value-bind (left top) (uv-for-display self)
      (let ((right (+ left width))
	    (bottom (- top height)))
	(multiple-value-setq (left top) (uv-to-screen stream left top))
	(multiple-value-setq (right bottom) (uv-to-screen stream right bottom))
	(draw-rectangle left right bottom top :alu %flip :stream stream
		  :opaque nil :filled nil)))))

(defmethod edit ((self annotation) STREAM &optional the-string)
  "Interactively edit the text of the annotation."
  (with-slots (graph width height text) self
    (or the-string (setq the-string text))
    (erase self STREAM)
    (with-output-truncation
     (stream)
     (unwind-protect ;; Make sure that ABORTing does the right thing.
	 (multiple-value-bind (u v) (uv-for-display self)
	   (multiple-value-bind (u1 v1) (uv-to-screen stream u v)
	     (let ((u2 (+ u1 width))
		   (v2 (+ v1 height)))
	       (setq the-string (window-edit-text stream u1 v1 u2 v2 the-string))
	       (set-text self STREAM the-string))))
       (display self stream)))))

(defmethod default-point1 ((a annotation))
  (multiple-value-list (uv-for-display a)))

(defmethod map-points-1 ((function t) (a annotation))
  ;; point1 is one of the corners (UV) of the text annotation.
  (multiple-value-bind (l to) (uv-for-display a)
    (let* ((width (or (width a) 0))
	   (height (or (height a) 0))
	   (u-mid (+ l (values (truncate width 2))))
	   (v-mid (- to (values (truncate height 2))))
	   (points '(:top :bottom :left :right :ltop :lbottom :rtop :rbottom)))
      (labels ((coordinates (corner)
		 (ecase corner
		   (:top (values u-mid to ))
		   (:bottom (values u-mid (- to height)))
		   (:left (values l v-mid))
		   (:right (values (+ l width) v-mid))
		   (:ltop (values l to))
		   (:lbottom (values l (- to height)))
		   (:rtop (values (+ l width) to))
		   (:rbottom (values (+ l width) (- to height))))))
	(dolist (point points)
	  (multiple-value-bind (u v) (coordinates point)
	    (funcall function u v point)))))))

;;; Does anybody call this guy anymore?  JPM 8 May 92.
;;; KRA 09JUN93: Yes (method annotate ( annotated-graph-mixin t t))
(defmethod create ((self annotation) STREAM)
  "Create an initial string and interactively edit it."
  (with-slots (graph width height) self
    (let ((initial-string (make-string 0)))
      (set-uv-position self (ull graph) (vur graph))
      (set-text self STREAM initial-string)
      (setf (style self) (default-text-style graph stream))
      ;; A nice size:
      (setq width 220 height 100)
      (move self STREAM)
      (edit self STREAM)
      self)))				; Return Non-NIL for success.

(defun ANNOTATE-GRAPH (graph STREAM text x y &optional (display t) (type 'annotation))
  "Noninteractively add an annotation."
  (let ((annotation (make-instance type :graph graph)))
    (install-annotation annotation graph stream text x y nil)
    (if display (display annotation stream))
    annotation))

(defmacro WITH-OUTPUT-TO-ANNOTATION ((stream graph x y) &body body)
  "All output to STREAM really goes to an annotation located at x,y on graph"
  (let ((g (gensym)) (x0 (gensym)) (y0 (gensym)) (string (gensym)))
    `(let* ((,stream nil) (,g ,graph) ,string (,x0 ,x) (,y0 ,y))
       (when (setq ,string (with-output-to-string (,stream) ,@body))
	 (annotate-graph ,g ,stream ,string ,x0 ,y0)))))    


;;;POINT-ANNOTATION
;;;
;;;  an annotation with a line that points to something.

(defclass recompute-annotation-mixin ()
    ((compute-function :initform nil :initarg :compute-function :accessor compute-function))
  (:documentation
    "The compute-function is either a closure or NIL.  The closure is used in a
     variety of ways to recompute something about the annotation.  It was invented
     for the purpose of updating the text of an annotation when the thing that it
     points to has been changed in some way."))

(defmethod recompute ((self recompute-annotation-mixin))
  (with-slots (compute-function) self
    (when compute-function (funcall compute-function self) t)))

(defmethod recompute-annotation-appearance ((self recompute-annotation-mixin) (STREAM t))
  (let* ((recompute-p (compute-function self))
	 (refresh-p (displayed? self)))
    (when recompute-p
      (cond (refresh-p
	     (erase self stream) (recompute self) (display self stream))
	    (t (recompute self))))))

(defclass moving-annotation-piece-mixin ()
    ((annotation :initform nil :initarg :annotation :accessor annotation))
  (:documentation "Couple a moving-object to an annotation"))

(defmethod graph ((object moving-annotation-piece-mixin))
  (graph (annotation object)))

(defmethod after-motion ((self moving-annotation-piece-mixin) (STREAM t))
  (recompute-annotation-appearance (annotation self) STREAM))

(defmethod display ((self moving-annotation-piece-mixin) stream)
  (display (annotation self) stream))

(defmethod erase ((self moving-annotation-piece-mixin) stream)
  (erase (annotation self) stream))

(defmethod draw-at ((self moving-annotation-piece-mixin) stream u v)
  (set-stream-position self stream u v)
  (figure-geometry (annotation self) stream)
  (mark (annotation self) stream))

(defmethod erase-at ((self moving-annotation-piece-mixin) stream u v)
  (declare (ignore u v))
  (unmark (annotation self) stream))

(defclass moving-annotation-point (moving-annotation-piece-mixin moving-point)
    ((radius :initform 5 :initarg :radius :accessor point-radius))
  (:documentation "The 'point' part of a point-annotation"))

(defmethod draw-xy-point ((object moving-annotation-point) stream &optional (alu %draw))
  (with-slots (x y radius) object
    (multiple-value-bind (u v) (xy-to-uv (graph object) x y)
      (multiple-value-setq (u v) (uv-to-screen stream u v))
      (device-draw-circle stream u v radius :alu alu :filled nil))))

(define-presentation-type moving-point ()
  :description "a moving point"
  :printer ((object stream)
	    (format stream "~S" object))
  :parser ((stream)
	   (read-char stream)
	   (error "You must select an annotation with the mouse.")))

(define-presentation-to-command-translator
  com-move-annotation-point
  (moving-point :command-name com-move-object
		:command-table :graph
		;; Annotations are covered by a different translator.
		:tester ((object) 
			 (not (typep object 'basic-annotation))) ; NLC
		:gesture :select :menu t :documentation "Move")
  (object &key WINDOW)
  `(,object ,WINDOW))

(defclass annotation-link-mixin ()
    ((point1 :initform nil :initarg :point1 :accessor point1)
     (point2 :initform nil :initarg :point2 :accessor point2))
  (:documentation
    "Defines a protocol for connecting two widgets with a line.
     Subclasses must provide MAP-POINTS-1 and MAP-POINTS-2  for
     mapping over consecutive corners of the two widgets."))

(defmethod draw-link ((object annotation-link-mixin) stream &optional (alu %draw))
  (let* ((p1 (point1 object))
	 (p2 (point2 object))
	 (x1 (pop p1))
	 (y1 (pop p1))
	 (x2 (pop p2))
	 (y2 (pop p2)))
    (device-draw-line stream x1 y1 x2 y2 :alu alu)))

(defmethod display :after ((object annotation-link-mixin) stream)
  (draw-link object stream))

(defmethod mark :after ((object annotation-link-mixin) stream)
  (draw-link object stream %flip))

(defmethod compute-point2-position ((object annotation-link-mixin))
  (let ((point1 (or (point1 object) (default-point1 object))))
    (multiple-value-bind (choice x2 y2)
	(closest-point
	  (first point1) (second point1)
	  #'map-points-2
	  object)
      (declare (ignore choice))
      (setf (point2 object) (list x2 y2)))))

(defmethod compute-point1-position ((object annotation-link-mixin))
  (let ((point2 (or (point2 object) (default-point2 object))))
    (multiple-value-bind (choice x1 y1)
	(closest-point
	  (first point2) (second point2)
	  #'map-points-1
	  object)
      (declare (ignore choice))
      (setf (point1 object) (list x1 y1)))))

(defmethod rescale-annotation progn ((self annotation-link-mixin))
  ;; Since point positions are in uv coordinates, we need to recache
  ;; their values.
  (compute-point2-position self)
  (compute-point1-position self))

(defmethod figure-geometry :after ((self annotation-link-mixin) STREAM)
  (declare (ignore STREAM))
  (compute-point2-position self)
  (compute-point1-position self))

(defmethod set-xy-position :after ((self annotation-link-mixin) x y &optional z)
  (declare (ignore x y z))
  (compute-point2-position self)
  (compute-point1-position self))

(defclass POINT-ANNOTATION (annotation-link-mixin
                            recompute-annotation-mixin
                            annotation)
  ((point :initform nil :initarg :point :accessor point-annotation-point))
  (:documentation "An annotation with a pointer to some uv point."))

(defmethod display :after ((self point-annotation) stream)
  (with-output-as-presentation (:stream stream
				:object (point-annotation-point self)
				:single-box t
				:type 'moving-point)
    (draw-xy-point (point-annotation-point self) stream %draw)))

(defmethod mark :after ((self point-annotation) stream)
  (draw-xy-point (point-annotation-point self) stream %flip))

(defmethod display-p :around ((self point-annotation))
  (or (call-next-method)
      (let ((point (point-annotation-point self)))
	(multiple-value-bind (u v) (xy-position point)
	  (multiple-value-setq (u v) (xy-to-uv (graph self) u v))
	  (uv-is-inside (graph self) u v)))))

(defmethod default-point2 ((a point-annotation))
  (multiple-value-bind (u v) (xy-position (point-annotation-point a))
    (multiple-value-setq (u v) (xy-to-uv (graph a) u v))
    (list u v)))

(defmethod map-points-2 ((function t) (a point-annotation))
  ;; point2 is one of the corners (UV) of the polygon.
  (multiple-value-bind (u v) (xy-position (point-annotation-point a))
    (multiple-value-setq (u v) (xy-to-uv (graph a) u v))
    (funcall function u v (point-annotation-point a))))

;;; Does anybody call this guy anymore?  JPM 8 May 92.
;;; KRA 09JUN93: Yes (method annotate ( annotated-graph-mixin t t))
(defmethod create ((self point-annotation) STREAM)
  "Create an initial string and interactively edit it."
  (with-slots (graph width height) self
    (multiple-value-bind (u v)
	(device-mouse-point STREAM "Choose Point With Mouse")
      (when (and u v)
	(multiple-value-setq (u v) (uv-to-xy (graph self) u v))
	(setf (point-annotation-point self)
	      (make-instance 'moving-annotation-point :x u :y v :annotation self))
	(let ((initial-string (make-string 0)))
	  (set-uv-position self (ull graph) (vur graph))
	  (set-text self STREAM initial-string)
	  (setf (style self) (default-text-style graph stream))
	  ;; A nice size:
	  (setq width 220 height 100) 
	  (figure-geometry self stream)
	  (move self STREAM)
	  (edit self STREAM)
	  self)))))			; Return non-nil for success.

(defun ANNOTATE-POINT (graph STREAM text x y point-x point-y &optional text-fn)
  "Noninteractively annotate a point"
  (let ((annotation (make-instance 'point-annotation :graph graph)))
    (setf (point-annotation-point annotation) (make-instance 'moving-annotation-point
                                                             :x point-x
                                                             :y point-y
                                                             :annotation annotation))
    (install-annotation annotation graph stream text x y text-fn)
    (display annotation STREAM)
    annotation))

(defun install-annotation (annotation graph STREAM text x y text-fn)
  ;; KRA 09JUN93: This code was factored out from 3 places.
  ;; It could probably be a good :after initialize method.
  (set-xy-position annotation x y)
  (set-text annotation STREAM text)
  ;; Fixed-width looks better here.
  (setf (style annotation) (default-text-style graph stream))
  (if text-fn
      (setf (slot-value annotation 'compute-function) text-fn))
  (recompute-annotation-appearance annotation stream)
  (figure-geometry annotation stream)
  (setf (annotations graph) (cons annotation (annotations graph)))
  (values))


(defmethod choose-datum (dataset STREAM graph)
  "Return the datum nearest a mouse click."
  (multiple-value-bind (u1 v1) (device-mouse-point STREAM)
    (when u1
      (multiple-value-bind (ignore1 ignore2 datum)
	  (nearest-datum graph dataset u1 v1)
	(declare (ignore ignore1 ignore2))
	datum))))

(defmethod nearest-datum (graph dataset u v)
  "Return the x,y datum nearest the given u,v coordinates."
  ;; Do this in uv coordinates, because what matters is what looks close on the
  ;; screen, not what seems close in x,y space.
  (multiple-value-bind (datum u0 v0)
      (closest-point
	u v
	#'(lambda (function dataset)
	    (map-data dataset
		      #'(lambda (datum)
			  (multiple-value-bind (fric frac) (datum-position dataset datum)
			    (multiple-value-bind (u1 v1) (xy-to-uv graph fric frac)
			      (funcall function u1 v1 datum))))
		      (data dataset)))
	dataset)
    (multiple-value-setq (u0 v0) (uv-to-xy graph u0 v0))
    (values u0 v0 datum)))

(defmethod maybe-change-datum ((point-annotation point-annotation) STREAM dataset)
  ;; Constraint function for point annotations that always have to point to a datum.
  (let* ((point (point-annotation-point point-annotation))
	 (graph (graph point-annotation)))
    (multiple-value-bind (x y) (xy-position point)
      (multiple-value-bind (new-x new-y datum)
	  (multiple-value-bind (u v) (xy-to-uv graph x y)
	    (nearest-datum graph dataset u v))
	(when (not (and (= x new-x) (= y new-y)))
	  (cond ((displayed? point-annotation)
		 (erase point-annotation STREAM)
		 (set-xy-position point new-x new-y)
		 (figure-geometry point-annotation stream)
		 (display point-annotation stream))
		(t (set-xy-position point new-x new-y)
		   (figure-geometry point-annotation stream))))
	datum))))

(defmethod text-for-datum (graph dataset datum)
  (let ((yname (and graph (string (y-label graph))))
	(xname (and graph (string (x-label graph))))
	(*print-circle* nil))
    (when (or (not yname) (zerop (length yname))) (setq yname "Y"))
    (when (or (not xname) (zerop (length xname))) (setq xname "X"))
    (multiple-value-bind (x y) (datum-position dataset datum)
      (if (floatp x) (setq x (float-to-string x)))
      (if (floatp y) (setq y (float-to-string y)))
      (format nil "~A~%~A = ~A~%~A = ~A" (name dataset) xname x yname y))))

(defmethod annotate-data-point ((self graph-data) STREAM &optional graph datum)
  "Add a graph annotation describing a chosen data point."
  ;; This is essentially the same as DataProbe's Label Point command.
  (unless graph (setq graph (graph self)))
  (unless datum (setq datum (choose-datum self stream graph)))
  (when datum
    (let ((closure #'(lambda (annotation) 
		       (setq datum (maybe-change-datum annotation STREAM self))
		       (set-text annotation STREAM (text-for-datum graph self datum)))))
      (multiple-value-bind (x y) (datum-position self datum)
	(let ((a (annotate-point graph STREAM ""
				 (xll graph) 
				 (yur graph) 
				 x y
				 closure)))
	  (when a (move a STREAM)))))))


;;;
;;; Region annotations
;;;

(defclass annotation-polygon (moving-annotation-piece-mixin moving-polygon)
    ((annotation :initform nil :initarg :annotation :accessor annotation))
  (:documentation "Couple a polygon to an annotation")) 

(define-presentation-type polygon-presentation ()
  :description "a polygon"
  :printer ((object stream)
	    (format stream "~S" object))
  :parser ((stream)
	   (read-char stream)
	   (error "You must select a polygon with the mouse.")))

(define-presentation-to-command-translator
    com-move-polygon
    (polygon-presentation :command-name com-move-object
			  :command-table :graph
			  :gesture :select 
			  :menu t 
			  :tester ((object) (moveable-polygon-p object))
			  :documentation "Move")
  (object &key WINDOW X Y)
  (progn (cache-moving-corner object window x y)
	 `(,object ,WINDOW)))

(defclass region-annotation (annotation-link-mixin
			      recompute-annotation-mixin
			      annotation)
    ((data :initform nil :initarg :data :accessor data)
     (region :initform nil :initarg :region :accessor region))
  (:documentation
    "Annotate a set of data points contained within a region of the graph."))

(defmethod initialize-instance :after ((self region-annotation) &key &allow-other-keys)
  (let ((data (data self)))
    (when data
      (let ((corners (corners data)))
	(when corners
	  (setf (region self)
		(make-instance 'annotation-polygon
			       :corners corners
			       :annotation self)))))))

(defmethod display :after ((a region-annotation) stream)
  (let ((polygon (region a)))
    (with-output-as-presentation (:stream stream
				  :object polygon
				  :type 'polygon-presentation)
      (draw-xy-polygon polygon stream %draw))))

(defmethod display-p :around ((self region-annotation))
  (or (call-next-method)
      (let ((graph (graph self)))
	(some #'(lambda (corner)
		  (let ((u (first corner))
			(v (second corner)))
		    (multiple-value-setq (u v) (xy-to-uv graph u v))
		    (uv-is-inside graph u v)))
	      (corners (region self))))))

(defmethod mark :after ((a region-annotation) stream)
  (draw-xy-polygon (region a) stream %flip))

(defmethod set-xy-position :after ((self region-annotation) x y &optional z)
  (declare (ignore x y z))
  (compute-point2-position self)
  (compute-point1-position self))

(defmethod default-point2 ((a region-annotation))
  (first (corners (region a))))

(defmethod map-points-2 ((function t) (a region-annotation))
  ;; point2 is one of the corners (UV) of the polygon.
  (let ((graph (graph a)))
    (dolist (point (corners (region a)))
      (let ((x0 (car point)) (y0 (cadr point)))
	(multiple-value-setq (x0 y0) (xy-to-uv graph x0 y0))
	(funcall function x0 y0 point)))))

(defun ANNOTATE-REGION (graph stream text x y dataset &optional text-fn)
  "Noninteractively annotate a region of data."
  (let ((annotation (make-instance 'region-annotation
				   :graph graph
				   :data dataset
				   )))
    (install-annotation annotation graph stream text x y text-fn)
    (display annotation stream)
    annotation))

(defmethod annotate-data-region (dataset graph STREAM)
  "Add a graph annotation describing a chosen data interval."
  (let* ((choices (description-choices dataset))
	 gated)
    (when choices
      (setq gated (create-gated-dataset graph dataset stream))
      (when gated
	(let* ((text-x (xll graph))
	       (text-y (yur graph))
	       (title (name dataset))
	       (closure (choose-description-function title STREAM choices gated))
	       (a (and closure
		       (annotate-region graph stream "Hello"
					text-x text-y gated closure))))
	  (when a (move a STREAM)))))))


;;; This allows you to annotate a graph with a description tailored  from a menu.  All you
;;; need to do is provide a method for DESCRIPTION-CHOICES.   You get a list of strings like
;;; "<descriptive string> <tab> <value>".   Your method should return an association list of
;;; 1) descriptive string and 2)  function or message name.  The function gets applied to an
;;; object to compute the  value.

(defgeneric DESCRIPTION-CHOICES (object)
  (:method-combination append :most-specific-first) ; Fixed in 1991?
  (:documentation "Returns an association list of descriptive string, function name."))

(defun COMPUTE-DESCRIPTOR (function argument &rest arguments)
  ;; If function is a keyword, assume it is a Flavor message to be sent to argument.
  ;; Otherwise, apply it as in common lisp.
  (if (keywordp function)
      (apply argument function arguments)
      (apply function argument arguments)))

(defun COMPUTE-DESCRIPTION-STRING (title choices &rest arguments)
  (let ((strings nil)
	(N 0)
	control-string
	(*print-circle* nil))
    ;; First, figure out the tab stops.
    (dolist (choice choices (setq control-string
				     (format nil "~~A~~~AT ~~A" (1+ N))))
      (setq N (max N (length (first choice)))))
    ;; Now make the strings.
    (dolist (choice choices (setq strings (nreverse strings)))
      (let* ((value (apply #'compute-descriptor (second choice) arguments))
	     (string-value (cond ((stringp value) value)
				 ((characterp value) (string value))
				 ((floatp value) (float-to-string value))
				 (t (format nil "~A" value)))))
	(push (format nil control-string (first choice) string-value)
	      strings)))
    (when title
      (if (listp title)
	  (setq strings (append title strings))
	  (setq strings (cons title strings))))
    (format nil "~{~A~%~}" strings)))

(defun CHOOSE-DESCRIPTORS (choices &optional (title "the selected data"))
  ;; CHOICES is a list of (string symbol) pairs.  This function returns
  ;; a subset of those pairs.
  (multiple-value-bind (list abort-p)
      (several-choose
       choices
       :label (format nil "Choose several descriptors of ~A:" title))
    (if abort-p
	(values nil t)
      (let ((result nil))
	(dolist (item list)
	  (push (find item choices :key #'second) result))
	result))))

(defun CHOOSE-DESCRIPTION-STRING (title choices &rest arguments)
  ;; Used to annotate boxplots.
  (multiple-value-bind (the-choices abort-p) (choose-descriptors choices title)
    (unless abort-p
      (apply #'compute-description-string title the-choices arguments))))

(defun CHOOSE-DESCRIPTION-FUNCTION (title STREAM choices dataset)
  "Generate a closure that can update the description text."
  (multiple-value-bind (the-choices abort-p) (choose-descriptors choices title)
    (unless abort-p
      #'(lambda (annotation) 
	  (set-text annotation STREAM
		    (compute-description-string title the-choices dataset))))))

;;; When you annotate using all of these, some things obviously get computed twice.
;;; The cost is negligible, however, unless you have really huge datasets.  

(defmacro 2nd (form)
  "Return the second value returned by FORM"
  `(multiple-value-bind (ignore this) ,form (declare (ignore ignore)) this))
(defun n-samples (dataset) (2nd (x-mean dataset)))
(defun minimum-x (dataset) (values (x-min-and-max dataset)))
(defun maximum-x (dataset) (2nd (x-min-and-max dataset)))
(defun minimum-y (dataset) (values (y-min-and-max dataset)))
(defun maximum-y (dataset) (2nd (y-min-and-max dataset)))
(defun mean-x (dataset) (values (x-mean dataset)))
(defun mean-y (dataset) (values (y-mean dataset)))
(defun standard-deviation-x (dataset) (2nd (x-mean-and-stdev dataset)))
(defun standard-deviation-y (dataset) (2nd (y-mean-and-stdev dataset)))

(defmethod description-choices append ((self graph-data))
  (copy-list					; lucid uses Nconc instead of append?
						; 9 apr 91 jpm.
    `(("# Samples" n-samples)
      ("X Minimum" minimum-x)
      ("X Maximum" maximum-x)
      ("X Mean" mean-x)
      ("X St. Dev." standard-deviation-x)
      ("Y Minimum" minimum-y)
      ("Y Maximum" maximum-y)
      ("Y Mean" mean-y)
      ("Y St. Dev." standard-deviation-y))))

(define-graph-command com-identify
    ((dataset 'graph-data) 
     (graph 'graph)
     (datum 'invisible-object)
     (stream 'sheet))
  "Add an annotation describing a data point."
  (cond ((and dataset graph datum stream)
	 (annotate-data-point dataset stream graph datum))
	(t (beep))))

(defmethod datum-presentation-p ((object t) presentation)
  ;; Check that it's a part of a graph and not part of the legend.
  (and (eql (presentation-type presentation) 'expression)
       (graph-under-presentation presentation)
       (not (graph-under-annotation-under-presentation presentation))))

(defun dataset-sensitive-p (presentation)
  (let ((dataset (dataset-under-presentation presentation)))
    (and dataset (present-self-p dataset))))

;;; Make individual datums mouse-sensitive.  This is a little bit tricky,
;;; since datums can literally be any lisp object.  
(define-presentation-to-command-translator com-identify
  (expression :command-name com-identify
	      :command-table :graph
	      :gesture :select
	      :menu t
	      :documentation "Identify Data Point"
	      :tester ((object &key presentation)
		       (and (datum-presentation-p object presentation)
			    (dataset-sensitive-p presentation))))
  (object &key presentation window)
  (list (dataset-under-presentation presentation) 
	(graph-under-presentation presentation)
	object window))

