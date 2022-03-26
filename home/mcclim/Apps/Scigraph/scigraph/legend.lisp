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


;;; GRAPH-DATA-LEGEND-MIXIN PROTOCOL:
;;; 
;;; Allows a dataset to provide a legend item for graphs.  [Legends can grow quite
;;; large when many datasets are involved, so a major goal is keeping them
;;; individually small.]
;;; 
;;; DISPLAY-LEGEND-DATASET
;;;   Display a legend item for this dataset.  This has 2 parts: legend data and a
;;;   legend-string.
;;; DISPLAY-LEGEND-STRING 
;;;   Displays the string part of the legend
;;; DISPLAY-LEGEND-DATUMS
;;;   Displays the data part of the legend.  To display legend datums, there is a
;;;   subprotocol.  A second dataset (called the legend dataset) is used to provide
;;;   some data points (3) for display.  If the data structure of the points in the
;;;   legend dataset does not match the data structure of the "real" dataset,
;;;   applications may need to specialize one of the following methods.
;;;     LEGEND-DATASET
;;;       Returns a (small) dataset containing the actual points to display.  By
;;;       default, a single dataset is instantiated, cached, and reused for every
;;;       legend.  The datums are lists of the form (X Y).
;;;     LEGEND-DATUM-DISPLAYER
;;;       Returns a function (like DATUM-DISPLAYER) that takes arguments X Y and
;;;       DATUM and draws the datum in the proper symbologies.

(defclass SHOW-LEGEND-MIXIN ()
    ((show-legend :initform t :initarg :show-legend :accessor show-legend)) ; Show the legend?
  (:documentation "Provide a flag to show legend."))

(defmethod show-legend ((any t)) nil)

;;; needs some sort of symbology to work.
(defclass GRAPH-DATA-LEGEND-MIXIN (basic-graph-datum-symbology-mixin
				   show-legend-mixin)
    ()
  (:documentation
    "Allows a dataset to provide a legend item for graphs."))

(defmethod legend-string ((self graph-data-legend-mixin))
  (let ((name (name self)))
    (if name (string name))))		; You may want to override this.

(defvar *legend-symbology-width* 40 "Width of legend symbology, in pixels.")
(defvar *legend-gap* 10 "Gap between legend symbology and string, in pixels.")
(defvar *legend-style* '(:fix :roman :normal))

(defmethod legend-size ((self graph-data-legend-mixin) stream
			&optional (style (parse-text-style *legend-style*)))
  "WIDTH and HEIGHT in pixels of area needed to display legend."
  (let ((legend (legend-string self)))
    (if legend
	(multiple-value-bind (width height)
	    (string-size stream style "~A" legend)
	  (values (+ *legend-symbology-width* *legend-gap* width)
		  height))
	(values 0 0))))

(defmethod make-legend-datum ((self graph-data-legend-mixin) x y &rest args)
  `(,x ,y ,@args))

(let ((legend-dataset-cache nil))
  
  (defmethod LEGEND-DATA ((self graph-data-legend-mixin))
    "Return a dataset to use to draw the points in the legend."
    (or legend-dataset-cache
	(let* ((x .5)
	       (y .5)
	       (dataset (make-instance 'graph-data
				       :data
				       (list (make-legend-datum self (- x) y)
					     (make-legend-datum self 0 (- y))
					     (make-legend-datum self x y))
				       :bar-width x)))
	  (setq legend-dataset-cache dataset)
	  dataset))))

(defmethod legend-datum-displayer ((self graph-data-legend-mixin) graph)
  "Returns a function of arguments STREAM U V and DATUM that get applied to data
   from the legend dataset to display the data part of the legend."
  (datum-displayer self graph))

(defmethod dont-record-output-history ((self graph-data-legend-mixin)) nil)

(defmethod legend-scale-limits ((self graph-data-legend-mixin) legend-data)
  "A sort of poor man's auto scaling for legend data."
  (let ((fudge .1))
    (multiple-value-bind (left right bottom top)
	(apply #'values (auto-scale-limits legend-data :both nil nil nil nil))
      (values (- left fudge) (+ right fudge) (- bottom fudge) (+ top fudge)))))

(defmacro with-bar-width ((dataset width) &body body)
  `(let ((.old-width. (bar-width ,dataset)))
     (unwind-protect
	 (progn (setf (bar-width ,dataset) ,width) ,@body)
       (setf (bar-width ,dataset) .old-width.))))

(defmethod display-legend-datums ((self graph-data-legend-mixin) STREAM
				  graph left bottom width height)
  "Display some points in the legend area to show the current symbology settings."
  (let ((legend-data (legend-data self)))
    ;; The contract here is that the legend data knows about the data structure of
    ;; its datums, and the "real" data knows how to display them.  This works OK as
    ;; long as the displayer doesn't assume too much about the datums coming from the
    ;; legend data.
    (with-bar-width (self (bar-width legend-data))
      (multiple-value-bind (xll xur yll yur) (legend-scale-limits self legend-data)
	;; Warp the graph to think its edges are the edges of the legend.
	(with-graph-coordinates (graph STREAM
				       left (+ left width) bottom (+ bottom height)
				       xll xur yll yur)
	  (with-clipping-to-graph (graph STREAM t)
	    (with-alu (stream (alu self))
	      ;; SELF provides the displayer, but LEGEND-DATA provides the mapper.
	      (let ((displayer (legend-datum-displayer self graph)))
		(map-data legend-data
			  #'(lambda (datum)
			      (multiple-value-bind (x y)
				  (datum-position legend-data datum)
				(multiple-value-setq (x y) (xy-to-uv graph x y))
				(multiple-value-setq (x y) (uv-to-screen stream x y))
				(funcall displayer stream x y datum)))
			  (data legend-data))))))))))

(defmethod display-legend-string ((self graph-data-legend-mixin) STREAM graph u v)
  "Display the string part of the legend."
  (text graph STREAM u v (legend-string self)))

(defmethod display-legend-dataset ((self t) STREAM graph left bottom width height)
  (declare (ignore stream graph left bottom width height))
  ;; Default is a noop, so that graphs can assume everybody handles this method.
  nil)

(defmethod display-legend-dataset ((self graph-data-legend-mixin) STREAM
				   graph left bottom width height)
  "Display legend data and string in given UV region of graph."
  (declare (ignore width))
  (when (show-legend self)
    (display-legend-datums self STREAM graph
			   left bottom *legend-symbology-width* height)
    (display-legend-string
      self STREAM graph
      (+ left *legend-symbology-width* *legend-gap*) bottom)))


;;; GRAPH LEGENDS
(defclass GRAPH-LEGEND-MIXIN (graph-datasets-mixin show-legend-mixin)
  (;;   Distance from bottom of graph (px)
   (legend-offset :initform 0 :initarg :legend-offset :accessor legend-offset)) 
  (:documentation "Provide graph with a Legend describing each dataset."))

(defmethod show-graph-legend ((any t)) nil)
(defmethod show-graph-legend ((self graph-legend-mixin))
  (and (show-legend self) (some #'show-legend (datasets self))))

(defmethod legend-size ((self graph-legend-mixin) the-stream
			&optional (style (parse-text-style *legend-style*)))
  "Width and height of graph legend area."
  (if (show-graph-legend self)
      (let ((x 0)
	    (y 0))
	(with-character-style (style the-stream)	; bind line height
	  (dolist (d (datasets self))
	    (when (show-legend d)
	      (multiple-value-bind (dx dy)	; assumes vertical orientation of legends
		  (legend-size d the-stream style)
		(incf y dy)
		(setf x (max x dx))))))
	(values x y))
      (values 0 0)))

(defmethod legend-compute-margins ((self graph-legend-mixin) STREAM left right bottom top)
  ;; Give the legend a chance to modify the margin area of the graph.
  (setf (legend-offset self) bottom)
  (multiple-value-bind (foo legh) (legend-size self stream)
      (declare (ignore foo))
      (values left right (+ legh bottom) top)))

(defmethod compute-margins :around ((self graph-legend-mixin) STREAM)
  (multiple-value-bind (left right bottom top) (call-next-method)
    (legend-compute-margins self stream left right bottom top)))

(defmethod display-labels :after ((self graph-legend-mixin) STREAM)
  (when (show-graph-legend self) (display-legend self STREAM)))

(defmethod display-legend ((self graph-legend-mixin) STREAM)
  "Display each legend dataset in the margin area."
  (multiple-value-bind (px foo py bar) (uv-inside self)
    (declare (ignore foo bar))
    (setq py (- py (legend-offset self)))
    (with-character-style ((parse-text-style *legend-style*) stream)
      (let ((hidden (hidden-datasets self)))
	(dolist (dataset (datasets self))
	  (or (member dataset hidden)
	      (multiple-value-bind (dx dy) (legend-size dataset stream)
		(display-legend-dataset dataset STREAM self px py dx dy)
		(setq py (- py dy)))))))))
