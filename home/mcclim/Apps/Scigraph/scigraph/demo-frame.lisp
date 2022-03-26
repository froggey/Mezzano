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

(defvar graph-number 0
  "Keep count of the number of graphs that have been made.")

;;; Making these into formal parameters helps for debugging new graph classes.
(defvar *dataset-class* 'graph-data)
(defvar *graph-class* 'annotated-graph)
(defvar *sampling-rate* .1)			; about 100 samples.

(defun make-sample-data ()
  (let* (;; Raw data is simply a list or array whose elements are each a list of
	 ;; length 2, where  X = (FIRST LIST), Y = (SECOND LIST).
	 (raw-data (pseudo-random-sequence
		     (random 3.0) (+ (random 3.0) 10.0) *sampling-rate*
		     #'sin .2)))
    ;; A dataset encapsulates the raw data in a CLOS instance and provides
    ;; services such as mapping over the raw data and displaying it as a
    ;; line graph, scatter graph, or whatever.
    (make-instance *dataset-class*
		   :data raw-data
		   :present-self-p t
		   :symbologies (list :line))))

(defun strange-attractor-array ()
  (let* ((X .1) y
	 (a 3.9)
	 (size 2000)
	 (array (make-array size)))
    (dotimes (i size)
      (setq y (* a x (- 1 x)))
      (setq x (* a y (- 1 y)))
      (setf (aref array i) (list x y)))
    array))

(defun make-strange-attractor ()
  (let* ((raw-data (strange-attractor-array)))
    ;; A dataset encapsulates the raw data in a CLOS instance and provides
    ;; services such as mapping over the raw data and displaying it as a
    ;; line graph, scatter graph, or whatever.
    (make-instance *dataset-class*
		   :data raw-data
		   :symbologies (list :line))))

(defun make-strange-graph ()
  (let* ((dataset (make-strange-attractor))
	 ;; Make an annotated graph
	 (graph (make-instance *graph-class*
			       :title (format nil "Graph Number ~D" (incf graph-number))
			       :x-label "Fortnights"
			       :y-label "Furlongs")))
    ;; For each dataset, add it to the graph.
    (add-dataset graph dataset)
    graph))

(defun make-sample-graph ()
  (let* ((dataset (make-sample-data))
	 ;; Make an annotated graph
	 (graph (make-instance *graph-class*
			       :title (format nil "Graph Number ~D" (incf graph-number))
			       :x-label "Fortnights"
			       :y-label "Furlongs"
			       :present-self-p nil)))
    ;; For each dataset, add it to the graph.
    (add-dataset graph dataset)
    graph))

(defun pseudo-random-sequence (from to increment function noise)
  "Generate some sample data."
  ;; Result is an array, where each element is a list of length 2 (x,y pair).
  (let* ((length (+ 2 (values (truncate (- to from) increment))))
	 (array (make-array length)))
    (do ((x from (+ x increment))
	 (i 0 (1+ i)))
	((>= i length) array)
      (setf (aref array i)
	    (list x (+ (funcall function x) (random noise)))))))

(defun make-demo-frame
    (&key
     create
     (left 0) (bottom 0)
     (width 600) (height 400)
     (wait-until-done nil))	; Start a new process?
  "Start a demo scigraph frame."
  (let ((graph (make-sample-graph)))
    (add-dataset graph
		 (make-instance
		  'equation-data
		  :color :salmon
		  :symbologies (list :line-symbol)
		  :data-symbol :circle
		  :pattern nil
		  :equation '(* (sin (* a x)) (sin (* b x)))
		  :variable 'x :min 0 :max 10 :increment .1
		  :parameters '((a 2) (b 3))))
    (view-graphs (list graph)
		 :title "Scigraph Demo"
		 :left left :bottom bottom
		 :width width :height height
		 :create create
		 :wait-until-done wait-until-done)))

(defun make-bar-demo ()
  (let* ((graph (make-instance 'annotated-graph
		  :name "Three Categories"
		  :auto-scale nil :xll -1 :yll 0.0 :xur 3.0 :yur 5.0
		  ))
	 (data '((0.0 3.0)
		 (1.0 4.0)
		 (2.0 3.5)))
	 (dataset (make-instance 'graph-data
		    :name "Categorical Data"
		    :data data
		    :color :red
		    :pattern t
		    :bar-width 0.9
		    :symbologies '(:bar))))
    (add-dataset graph dataset)
    (view-graphs (list graph)
		 :title "Bar Graph"
		 :wait-until-done nil)))

(defun save-sample-graph (&optional (filename "~/sample-graph.ps"))
  (save-postscript-graph (make-sample-graph) filename))


#||

This shows the minimal number of methods a dataset must provide to get
itself drawn on a graph.  PLAIN-DATA simply draws a line of slope 1 from 0
to 100.

||#
(defclass plain-data
    ()
  ())

(defmethod name ((data plain-data))
  (format nil "~A" data))

(defmethod auto-scale-limits ((data plain-data) type xll xur yll yur)
  (list
   (min 0 xll)
   (max 100 xur)
   (min 0 yll)
   (max 100 yur)))

(defmethod display-data ((data plain-data) STREAM graph)
  (multiple-value-bind (x1 y1)
      (xy-to-stream graph stream 0 0)
    (multiple-value-bind (x2 y2)
	(xy-to-stream graph stream 100 100)
      (draw-line* stream x1 y1 x2 y2))))

(defun make-plain-demo ()
  (let ((d (make-instance 'plain-data))
	(g (make-instance 'annotated-graph)))
    (add-dataset g d)
    (view-graphs (list g) :title "Plain" :left 0 :bottom 0
			 :width 600 :height 400 :create t
			 :wait-until-done nil)))
