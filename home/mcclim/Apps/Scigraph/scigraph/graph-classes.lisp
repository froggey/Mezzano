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

#||
;;; Finally you have enough mixins loaded to build a useful graph, and graph-data.

This is a useful graph data flavor to build on.  However, for some applications it may be too
powerful, and you may want to remove some of the mixins.
||#

;;; KRA: DONT DUMP DATA!  If you want this, create your own graph-data flavor that does
;;; it.

(defclass GRAPH-DATA
	  (
	   raw-graph-data
	   simple-data-statistics-mixin
	   presentable-data-mixin
	   graph-data-limits-mixin
	   graph-data-auto-scale-mixin
	   graph-data-color-mixin
	   graph-data-symbology-mixin
	   graph-data-add-datum-mixin
	   graph-data-legend-mixin
	   essential-graph-data-map-mixin
	   basic-graph-data
	   )
    ()
  (:default-initargs :auto-scale? t :symbologies '(:scatter))
  (:documentation "This is a useful graph data flavor to build on.
   However, for some applications it may be too powerful, and you may
   want to remove some of the mixins."))

(defmethod graph-data-p ((object t)) nil)
(defmethod graph-data-p ((object basic-graph-data)) t)

(defclass timeseries-data (graph-data) ()
  (:default-initargs
    :symbologies '(:line)
    :present-self-p nil
    :graph-present-inferiors-p nil)
  ;; By default, presentations are turned off.  This reduces
  ;; drawing time because individual points are not mouse-sensitive.
  (:documentation "Base class for a common type of data."))

(defmethod missing-data-threshold ((self timeseries-data))
  ;; Return a positive number when big gaps represent missing data.
  ;; This method determines how big such gaps have to be.
  ;; Display-data inhibits the line connecting the two adjacent datums.
  nil)

(defmethod display-data ((self timeseries-data) STREAM graph)
  "Add optimization because you know the data is sorted (along time axis)."
  (with-alu (stream (alu self))
    ;; Fixup the alu just once, since its the same for every datum.
    (let* ((displayer (datum-displayer self graph))
	   (thresh (let ((n (missing-data-threshold self)))
		     (if n (float (abs n)))))
	   (-thresh (and thresh (- thresh)))
	   (H (stream-height stream))
	   (Trans (xy-to-uv-transform graph))
	   (start (float (xll graph)))
	   (end (float (xur graph)))
	   (last-datum nil)
	   (last-x nil)
	   (last-y nil)
	   (last-in nil))
      (declare (short-float start end)
	       (fixnum H)
	       (compiled-function trans displayer))
      ;; Map over each datum between START and END plus the two bounding points
      ;; whose lines will get partially clipped.
      (flet ((draw-random-datum (d)
	       (multiple-value-bind (x0 y0)
		   (datum-position self d)
		 (multiple-value-setq (x0 y0) (funcall trans x0 y0))
		 (funcall displayer stream x0 (- H y0) d)))
	     (handle-missing-data-gap (x y)
	       ;; Reset the displayer because we dont connect datums
	       ;; that are this far apart.
	       (funcall displayer stream nil nil nil)
	       ;; Draw them as points so they aren't invisible.
	       (let ((size 2))
		 (multiple-value-bind (frog dog)
		     (xy-to-uv-distance graph (- last-x x) (- last-y y))
		   (when (or (> (abs frog) size) (> (abs dog) size))
		     (multiple-value-bind (u v) (xy-to-uv graph last-x last-y)
		       (multiple-value-bind (sx sy) (uv-to-screen stream u v)
			 (device-draw-circle stream (1- sx) (1+ sy) size
					     :filled t
					     :alu (alu self))))
		     (multiple-value-bind (u v) (xy-to-uv graph x y)
		       (multiple-value-bind (sx sy) (uv-to-screen stream u v)
			 (device-draw-circle stream (1- sx) (1+ sy) size
					     :filled t
					     :alu (alu self)))))))))
	(map-data self #'(lambda (datum)
			   (multiple-value-bind (x y) (datum-position self datum)
			     (or (floatp x) (setq x (float x)))
			     (or (floatp y) (setq y (float y)))
			     (when (and thresh last-x
					(>= (the short-float -thresh)
					    (- (the short-float x)
					       (the short-float last-x))
					    (the short-float thresh)))
			       (handle-missing-data-gap x y))
			     (setq last-x x last-y y)
			     (cond ((<= start (the short-float x) end)
				    (multiple-value-setq (x y) (funcall trans x y))
				    (if (and last-datum (not last-in))
					(draw-random-datum last-datum))
				    (funcall displayer stream x (- H y) datum)
				    (setq last-in t))
				   (t
				    (if (and last-datum last-in)
					(draw-random-datum datum))
				    (setq last-in nil)))
			     (setq last-datum datum)))
		  (data self))))))

(defmethod nearest-datum :around ((graph t) (dataset timeseries-data) u v)
  "If the data is really huge, it's better to be fast than to be exactly correct."
  (let ((raw-data (data dataset)))
    (if (and (arrayp raw-data) (> (length raw-data) 1000))
	(let ((last-datum nil))
	  (map-data dataset
		    #'(lambda (datum)
			(multiple-value-bind (x y) (datum-position dataset datum)
			  (multiple-value-setq (x y) (xy-to-uv graph x y))
			  (when (>= x u)
			    (return-from nearest-datum (or last-datum datum))))
			(setq last-datum datum))
		    (data dataset))
	  last-datum)
	(call-next-method graph dataset u v))))
					

(defclass GRAPH
    ( presentable-graph-mixin

      graph-datasets-ob-mixin
      graph-legend-mixin

      graph-zoom-mixin
      graph-slider-interaction-mixin
      graph-slider-mixin

      graph-mouse-resolution-mixin
      graph-auto-scale-extensions-mixin
      graph-limits-mixin			
      graph-auto-scale-mixin
      graph-grid-ob-mixin
      graph-grid-mixin
      ;; KRA 27JUL93: We don't know how to rotate charaters yet.
      ;; However see CLASS ANNOTATED-GRAPH and
      ;; ANNOTATED-HORIZONTAL-Y-BORDER-MIXIN-KLUDGE
      horizontal-y-border-mixin
      graph-datasets-mixin
      graph-border-ob-mixin
      graph-border-mixin
      basic-graph
      named-mixin
      )
  ()
  (:documentation "A bordered graph with datasets, and sliders."))

(defmethod graph-p ((object t)) nil)
(defmethod graph-p ((object basic-graph)) t)
