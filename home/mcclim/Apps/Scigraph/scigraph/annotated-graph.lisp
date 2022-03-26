;; -*- Syntax: Common-lisp; Package: GRAPH -*-
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

;;;ANNOTATED-GRAPH-MIXIN provides a graph with the ability to be annotated.
;;;   1) annotation           (put some arbitrary text on the graph)
;;;   2) point annotation     (draw a line from the text to some point on the graph)
;;;   3) region annotation    (draw a line from the text to some region of the graph)
;;;
;;;ANNOTATED-BORDERS-MIXIN turns the axis labels and the title of a graph into
;;;  "border" annotations.  A border annotation is like an ordinary annotation except
;;;  that it does the right thing when you zoom or rescale.


(define-presentation-type annotation ()
  ;; This has to be in a file separate from (defclass annotation ...) because
  ;; otherwise clim 1.0 complains.  jpm.
  :description "an annotation"
  :printer ((object stream)
	    (flet ((shorten-string
		     (string &optional (longest 15))
		     (let ((str (substitute #\space *return* string)))
		       (if (> (length str) longest)
			   (format nil "~A..." (subseq str 0 (1- longest)))
			   str))))
	      (write-string (shorten-string (annotation-text object)) stream)))
  :parser ((stream)
	   (read-char stream)
	   (error "You must select an annotation with the mouse.")))

(define-presentation-to-command-translator
  com-move-annotation
  (annotation :command-name com-move-object
	      :command-table :graph
	      :gesture :select
	      :menu t :documentation "Move")
  (object &key WINDOW)
  `(,object ,WINDOW))

(define-graph-command com-delete-annotation ((object 'annotation) (window 'sheet))
  "Removes an annotation from a graph."
  (kill object window))

(define-presentation-to-command-translator com-delete-annotation
   (annotation :command-name com-delete-annotation
	       :command-table :graph
	       :gesture nil :documentation "Delete")
   (object &key window)
  (list object window))

(define-graph-command com-change-annotation-style ((object 'annotation) (window 'sheet))
  "Set the text style (or font) of a given annotation."
  (let ((style (choose-character-style)))
     (when style
       (erase object window)
       (setf (style object) style)
       (display object window))))

(define-presentation-to-command-translator com-change-annotation-style 
   (annotation :command-name com-change-annotation-style
	       :command-table :graph
	       :gesture nil :documentation "Change Text Style")
   (object &key window)
  (list object window))

(define-graph-command com-edit-annotation ((object 'annotation) (window 'sheet))
  "Edit the text of the annotation."
   (edit object window))

(define-presentation-to-command-translator com-edit-annotation
   (annotation :command-name com-edit-annotation 
	       :gesture nil
	       :command-table :graph
	       :tester ((object) (editable object))
	       :documentation "Edit Annotation Text")
   (object &key WINDOW)
  (list object window))

(defclass ANNOTATED-GRAPH-MIXIN (essential-graph-margin-mixin) 
    ((annotations :initform () :initarg :annotations :accessor annotations)))

(defmethod annotated-graph-p ((object t)) nil)
(defmethod annotated-graph-p ((object annotated-graph-mixin)) t)

(defun annotate-something (graph STREAM)
  (let ((choice
	 (menu-choose
	  `(("Plain Text" :value annotation
	     :documentation "Add your own text to this graph")
	    ("Plain Text + Pointer" :value point-annotation
	     :documentation "Text with a pointer to a point"))
	  :prompt "Choose Type of Annotation")))
    (when choice (annotate graph STREAM choice))))

(define-graph-command com-annotations-menu ((object 'graph) (WINDOW 'sheet))
  "Create an annotation and prompt the user for the annotation text."
  (annotate-something object WINDOW))

(define-presentation-to-command-translator
  com-annotations-menu
  (graph :command-name com-annotations-menu
	 :command-table :graph
	 :documentation "Add Free Text..."
	 :gesture nil 
	 :tester ((object) (annotated-graph-p object)))
  (object &key WINDOW)
  (list object window))

(define-graph-command com-graph-identify-point ((graph 'graph) (window 'sheet))
  "Prompt the user to select a data point, and annotate it."
  (let* ((datasets (datasets graph))
	 (dataset (if (cdr datasets)
		      (menu-choose (mapcar #'(lambda (d) `(,(name d) :value ,d))
					   datasets)
				   :prompt "Choose a Dataset")	 
		    (car datasets))))
    (when dataset
      (annotate-data-point dataset window graph))))

(define-presentation-to-command-translator
  com-graph-identify-point
  (graph :command-name com-graph-identify-point
	 :command-table :graph
	 :documentation "Identify Data Point..."
	 :gesture nil 
	 :tester ((object) (annotated-graph-p object)))
  (object &key WINDOW)
  (list object window))

(define-graph-command com-graph-identify-region ((graph 'graph) (window 'sheet))
  "Prompt the user to select a data region, and annotate the points
   contained in the region with descriptive statistics."
  (let* ((datasets (datasets graph))
	 (dataset (if (cdr datasets)
		      (menu-choose (mapcar #'(lambda (d) `(,(name d) :value ,d))
					   datasets)
				   :prompt "Choose a Dataset:")
		    (car datasets))))
    (when dataset
      (annotate-data-region dataset graph window))))

(define-presentation-to-command-translator
  com-graph-identify-region
  (graph :command-name com-graph-identify-region
	 :command-table :graph
	 :documentation "Identify Data Region..."
	 :gesture nil 
	 :tester ((object) (annotated-graph-p object)))
  (object &key WINDOW)
  (list object window))

(defmethod rescale-annotation progn ((self annotated-graph-mixin))
  (dolist (a (annotations self)) (rescale-annotation a)))

(defmethod rescale :after ((self annotated-graph-mixin))
  (rescale-annotation self))

(defmethod display-annotations ((self annotated-graph-mixin) stream)
  (dolist (annotation (annotations self)) (display annotation stream)))

(defmethod display :after ((self annotated-graph-mixin) stream)
  (display-annotations self stream))

(defmethod erase :before ((self annotated-graph-mixin) stream)
  ;; After an annotation has been moved, it is no longer a part of the graph
  ;; presentation tree, it becomes a separate presentation.
  ;; So we have to take care of the annotations explicitly.
  (dolist (a (annotations self)) (erase a stream)))

(defmethod annotate ((self annotated-graph-mixin) GRAPH-WINDOW type)
  "Create an annotation and prompt the user for the text."
  ;; User interface provided by annotated-graph-mixin
  (let ((annotation (make-instance type :graph self)))
    (when
	(create annotation GRAPH-WINDOW) ; User can abort create with mouse-middle.
      (pushnew annotation (annotations self)))))


;;; BORDER-ANNOTATION
;;;
;;; These have their own "border" coordinate system, so that graph titles and the like 
;;; don't go flying off into infinity when you zoom or otherwise rescale.

(defclass BORDER-ANNOTATION-MIXIN ()
    ((border :initform nil :initarg :border :accessor border)
     ;; :left, :top, :right, or :bottom
     (border-u :initform nil :initarg :border-u :accessor border-u)
     ;; "border" coordinates
     (border-v :initform nil :initarg :border-v :accessor border-v)))

(defmethod border-to-uv ((self border-annotation-mixin) u v)
  ;; Coordinate system transformation.
  (with-slots (graph border) self
    (multiple-value-bind (ull uur vll vur) (uv-inside graph)
      (case border
	(:bottom (values (+ u (values (truncate (+ ull uur) 2)))
			 (+ v vll)))
	(:top    (values (+ u (values (truncate (+ ull uur) 2)))
			 (+ v vur)))
	(:left   (values (+ u ull)
			 (+ v (values (truncate (+ vll vur) 2)))))
	(:right  (values (+ u uur)
			 (+ v (values (truncate (+ vll vur) 2)))))))))

(defmethod uv-to-border ((self border-annotation-mixin) u v)
  ;; Coordinate system transformation.
  (with-slots (graph border) self
    (multiple-value-bind (ull uur vll vur) (uv-inside graph)
      (case border
	(:bottom (values (- u (values (truncate (+ ull uur) 2)))
			 (- v vll)))
	(:top    (values (- u (values (truncate (+ ull uur) 2)))
			 (- v vur)))
	(:left   (values (- u ull)
			 (- v (values (truncate (+ vll vur) 2)))))
	(:right  (values (- u uur)
			 (- v (values (truncate (+ vll vur) 2)))))))))

(defmethod set-xy-position :after ((self border-annotation-mixin) new-x new-y
				   &optional (constrain-p t))
  "Set the UV position of the annotation (but constrain it within the window)"
  (when constrain-p
    (multiple-value-bind (x y) (xy-to-uv (graph self) new-x new-y)
      (multiple-value-setq (x y) (uv-to-border SELF x y))
      (set-border-position self x y nil))))

(defmethod set-border-position ((self border-annotation-mixin) u v
				&optional (constrain-p t))
  (with-slots (border-u border-v) self
    (setq border-u u border-v v)
    (when constrain-p
      (multiple-value-bind (bu bv) (border-to-uv self u v)
	(set-uv-position self bu bv nil)))))

;;; Border annotations never get clipped.
(defmethod display-p ((self border-annotation-mixin)) t)

(defmethod annotation-single-box ((self border-annotation-mixin)) t)

(defmethod rescale-annotation progn ((self border-annotation-mixin))
  ;; border-to-xy transformation depends on scaling, so recache current
  ;; position.
  (with-slots (border-u border-v) self
    (when (and border-u border-v)
      (set-border-position self border-u border-v))))

(defclass BORDER-ANNOTATION (border-annotation-mixin annotation) ())

(defun MAKE-BORDER-ANNOTATION (graph STREAM text border u v &optional
			       (type 'border-annotation)
			       (angle 0) (display t))
  "Noninteractively add an annotation."
  (let ((annotation (make-instance type
				   :angle angle
				   :border border 
				   :graph graph)))
    (set-text annotation STREAM text)
    (setf (style annotation) (default-text-style graph stream))
    (set-uv-position annotation u v)
    (setf (annotations graph) (cons annotation (annotations graph)))
    (if display (display annotation stream))
    annotation))

;;; The classes X-LABEL, Y-LABEL, and TITLE are all types of border annotations that
;;; are coupled to the corresponding strings stored on the graph.  These methods
;;; solve the problem that the string is stored in two places.

(defclass x-label (border-annotation) ()
  (:default-initargs :border :bottom))

(defmethod display :before ((self x-label) (stream t))
  (setf (annotation-text self) (x-label (graph self))))

(defmethod set-text :after ((self x-label) stream string)
  (declare (ignore stream))
  (let ((graph (graph self)))
    (when graph (setf (x-label graph) string))))

(defclass y-label (border-annotation) ()
  (:default-initargs :border :left))

(defmethod display :before ((self y-label) (stream t))
  (setf (annotation-text self) (y-label (graph self))))

(defmethod set-text :after ((self y-label) stream string)
  (declare (ignore stream))
  (let ((graph (graph self)))
    (when graph (setf (y-label graph) string))))

(defclass title (border-annotation) ()
  (:default-initargs :border :top))

(defmethod display :before ((self title) (stream t))
  (setf (annotation-text self) (title (graph self))))

(defmethod set-text :after ((self title) stream string)
  (declare (ignore stream))
  (let ((graph (graph self)))
    (when graph (setf (title graph) string))))


(defclass ANNOTATED-BORDERS-MIXIN (annotated-graph-mixin graph-border-mixin basic-graph)
    ((x-annotation :initform nil :initarg :x-annotation
		   :accessor x-annotation)
     (y-annotation :initform nil :initarg :y-annotation
		   :accessor y-annotation)
     (title-annotation :initform nil :initarg :title-annotation
		       :accessor title-annotation))
  (:documentation "A mixin for graphs that turns titles and labels into annotations."))

(defmethod x-label-text-style ((graph ANNOTATED-BORDERS-MIXIN) stream)
  (default-text-style graph stream))
  
(defmethod y-label-text-style ((graph ANNOTATED-BORDERS-MIXIN) stream)
  (default-text-style graph stream))
  
(defmethod title-text-style ((graph ANNOTATED-BORDERS-MIXIN) stream)
  (default-text-style graph stream))

(defmethod compute-x-annotation ((self annotated-borders-mixin) STREAM)
  (with-slots (x-annotation ull uur vll x-label) self
    (when x-label
      (when (not x-annotation)
	(LET* ((umid (values (truncate (+ ull uur) 2)))
	       (annotation (make-border-annotation self STREAM x-label :bottom
						   umid ull 'x-label 0 nil))
	       (width nil))
	  (setf (style annotation) (x-label-text-style self stream))
	  (setq width (width annotation))
	  (set-uv-position annotation
			   (- (values (truncate (+ ull uur) 2))
			      (values (truncate width 2)))
			   (- vll (* (stream-line-height stream) 1)))
	  (setq x-annotation annotation))))))

(defmethod display-x-label ((self annotated-borders-mixin) STREAM)
  (compute-x-annotation self STREAM))


;;;
;;; KRA 27JUL93: ANNOTATED-HORIZONTAL-Y-BORDER-MIXIN-KLUDGE
;;;

#||

Here's what's going on.

The definition of the class GRAPH is dependent on if characters can be
rotated or not, with the following conditionalization in direct
superclasses:

      #-clim vertical-y-border-mixin
      #+clim horizontal-y-border-mixin

Unfortunately, annotated-borders-mixin is simply slapped on top of graph.
However, compute-y-annotation has to know which mixing it got.
We simply conditionalize the method the same way.

This is indicative of the original version of graph being simple minded
about fonts and pointer sensitivity.  It would be better to either drop the
older classes in favor of cleaned up newer ones.  Someday, ...

||#

(defmethod compute-y-annotation ((self annotated-borders-mixin) STREAM)
  (with-slots (y-annotation ull vll vur y-label y-digits) self
    (when y-label
      (when (not y-annotation)
	(let* ((annotation (make-border-annotation
			     self STREAM y-label :left
			     ull vll
			     'y-label #.(/ pi -2) nil))
	       (height nil))
	  (setf (style annotation) (y-label-text-style self stream))
	  (setq height (* (length y-label) (stream-line-height stream)))
	  (set-uv-position annotation
			   (- ull (* (stream-character-width stream)
				     (+ y-digits 2)))
			   (- (values (truncate (+ vll vur) 2))
			      (values (truncate height 2))))
	  (setq y-annotation annotation))))))

(defmethod display-y-label ((self annotated-borders-mixin) STREAM)
  (compute-y-annotation self STREAM))

(defmethod compute-title-annotation ((self annotated-borders-mixin) STREAM)
  (with-slots (title-annotation ull uur vur) self
    (when (title self)
      (when (not title-annotation)
	(multiple-value-bind (ignore1 ignore2 ignore3 top) (margins self)
	  (declare (ignore ignore1 ignore2 ignore3))
	  (let* ((umid (values (truncate (+ ull uur) 2)))
		 (annotation (make-border-annotation self STREAM (title self) 
						     :top umid vur 'title
						     0 nil))
		 (width nil))
	    (setf (style annotation) (title-text-style self stream))
	    (setq width (width annotation))
	    (set-uv-position annotation
		  (- (values (truncate (+ ull uur) 2)) (values (truncate width 2)))
		  (+ vur top (- 5)))
	    (setq title-annotation annotation)))))))

(defmethod display-title ((self annotated-borders-mixin) STREAM)
  (compute-title-annotation self STREAM))


(defclass legend-annotation (border-annotation-mixin basic-annotation)
    ((margin :initform 10 :initarg :margin :accessor margin)
     (width :initform 0 :initarg :width :accessor width)
     (height :initform 0 :initarg :height :accessor height))
  (:documentation "An annotation displaying an iconic description of each dataset."))

(defmethod annotation-text ((self legend-annotation))
  ;; Called by PRESENT method for annotations.
  ;; This text may appear in click-right menus.
  "Dataset Legend")

(defmethod draw-outline ((self legend-annotation) stream alu)
  "Draw a rectangle marking the edges of the annotation."
  (let ((fudge (margin self)))
    (multiple-value-bind (width height) 
	(legend-size (graph self) stream (style self))
      (setq width (truncate width))
      (setq height (truncate height))
      (setf (width self) width)
      (setf (height self) height)
      (multiple-value-bind (left top) (uv-for-display self)
	(let ((right (+ left width))
	      (bottom (- top height)))
	  (multiple-value-setq (left top) (uv-to-screen stream left top))
	  (multiple-value-setq (right bottom) (uv-to-screen stream right bottom))
	  (decf left fudge)
	  (incf right fudge)
	  (decf top fudge)
	  (incf bottom fudge)
	  (draw-rectangle left right bottom top :alu alu :stream stream
			  :opaque nil :filled nil))))))

(defmethod display ((self legend-annotation) stream)
  (let* ((line-height (truncate (stream-line-height stream)))
	 (alu (alu self))
	 (graph (graph self))
	 (datasets (datasets graph))
	 (hidden (hidden-datasets graph)))
    (when (show-graph-legend graph)
      (multiple-value-bind (width height)
	  (legend-size graph stream (style self))
	(setq width (truncate width))
	(setq height (truncate height))
	(setf (width self) width)
	(setf (height self) height)
	(setq height (values (truncate height 
				       (max 1 
					    (- (length datasets) 
					       (length hidden))))))
	(multiple-value-bind (left top) (uv-for-display self)
	  (draw-outline self stream alu)
	  (dolist (dataset datasets)
	    (unless (or (member dataset hidden)
			(not (show-legend dataset)))
	      (decf top line-height)
	      (display-legend-dataset dataset STREAM graph
				      left top width height))))))))

(defmethod mark ((self legend-annotation) stream)
  "Draw a rectangle marking the edges of the annotation."
  (draw-outline self stream %flip))

(defmethod kill :before ((self legend-annotation) stream)
  (declare (ignore stream))
  ;; make sure the legend doesnt come back if you kill it.
  (setf (show-legend (graph self)) nil))

(defclass annotated-legend-mixin () ()
  (:documentation
   "Convert the legend of the graph into a movable annotation.
    Place the legend inside the graph, but take care to place it
    where it won't obscure any data."))

(defvar *legend-positions*		; relative coordinates
  '((0.0 0.25)				; lower left
    (0.75 1.0)				; upper right
    (0.0 1.0)				; upper left
    (0.75 0.25)				; lower right
    (0.25 1.0)
    (0.5 1.0)
    (0.25 0.25)
    (0.5 0.25)
    (0.0 0.5)
    (0.0 0.75)
    (0.75 0.5)
    (0.75 0.75)
    (0.25 0.5)
    (0.25 0.75)
    (0.5 0.5)
    (0.5 0.75)))

(defun legend-positions (width height left top right bottom)
  "Make a list of xy positions where we might consider putting the legend."
  (let ((relative *legend-positions*)
	(dx (- right left))
	(dy (- top bottom))
	(xy nil))
    (dolist (p relative)
      (let ((x (+ left (* (first p) dx)))
	    (y (+ bottom (* (second p) dy))))
	(setq x (min x (- right width)))
	(setq y (max y (+ bottom height)))
	(push (list x y) xy)))
    (nreverse xy)))

(defun count-points-in-xy-rectangle (graph left top right bottom)
  (let ((count 0))
    (dolist (dataset (datasets graph))
      (map-data-xy dataset
		   #'(lambda (x y)
		       (when (and (<= left x right)
				  (<= bottom y top))
			 (incf count)))
		   (data dataset)))
    count))

(defmethod default-annotation-position (graph &optional (width 20) (height 20))
  "Find a place (UV) on the graph where the annotation won't obscure any data."
  (multiple-value-setq (width height) (uv-to-xy-distance graph width height))
  (multiple-value-bind (left right bottom top) (xy-inside graph)
    (let* ((positions (legend-positions width height left top right bottom))
	   smallest choice)
      (dolist (position positions)
	(let* ((left (car position))
	       (top (second position))
	       (right (+ left width))
	       (bottom (- top height))
	       (count (count-points-in-xy-rectangle graph left top right bottom)))
	  (if (or (not smallest) (< count smallest))
	      (setq smallest count choice position))))
      (apply #'xy-to-uv graph choice))))

(defmethod legend-text-style ((self annotated-legend-mixin) (stream t))
  (merge-text-styles (parse-text-style '(nil :roman :normal))
		     (medium-text-style stream)))

(defmethod create-legend ((self annotated-legend-mixin) stream)
  "Make a legend annotation and position it."
  (let* ((legend (make-instance 'legend-annotation :graph self :border :right))
	 (fudge nil))
    (setf (style legend) (legend-text-style self stream))
    (setq fudge (margin legend))
    (push legend (annotations self))
    (multiple-value-bind (width height) (legend-size self stream (style legend))
      (multiple-value-bind (le te)
	  (default-annotation-position self
	      (+ width (* 4 fudge)) (+ height (* 4 fudge)))
	(set-uv-position legend (+ le (* 2 fudge)) (- te (* 2 fudge)))))
    legend))

(defmethod legend-exists-p ((self annotated-legend-mixin))
  (dolist (ann (annotations self))
    (if (typep ann 'legend-annotation) (return-from legend-exists-p t))) ;;;NLC
  nil)

(defmethod display-annotations :before ((self annotated-legend-mixin) stream)
  (when (and (show-graph-legend self) (not (legend-exists-p self)))
    (create-legend self stream)))

(defmethod display-legend ((self annotated-legend-mixin) stream)
  ;; Cancel the usual way of displaying a legend.  Now the legend is an annotation,
  ;; therefore display is taken care of separately.
  (declare (ignore stream))
  nil)

(defmethod legend-compute-margins ((self annotated-legend-mixin) STREAM left right bottom top)
  ;; Don't use up valuable real-estate.  Stay out of the margins.
  (declare (ignore stream))
  (values left right bottom top))


(defclass ANNOTATED-GRAPH
    (annotated-legend-mixin
     annotated-borders-mixin
     annotated-graph-mixin
     graph)
  ())
