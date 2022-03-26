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

(define-presentation-type graph ()
  :description "a graph" 
  :printer ((object stream)
	    (format stream "~A" (name object)))
  :parser ((stream)
	   (read-char stream)
	   (error "You must select a graph with the mouse.")))

(define-presentation-type graph-data ()
  :description "a graph dataset" 
  :printer ((object stream)
	    (format stream "~A" (name object)))
  :parser ((stream)
	   (read-char stream)
	   (error "You must select a graph dataset with the mouse.")))

(defun display-graph (graph &key
			    (stream *standard-output*)
			    (scroll-if-necessary nil)
			    (width 500)
			    (height 300))
  "Displays graph with upper-left corner starting at current cursor position."
  (declare (ignore scroll-if-necessary))
  (let ((*standard-output* stream))
    (multiple-value-bind (x1 y1) (stream-cursor-position* stream)
      (setq width (truncate width))
      (setq height (truncate height))
      #-clim				; dw bug?
      (when scroll-if-necessary
	(multiple-value-bind (left top ignore bottom) 
	    (stream-viewport stream)
	  (if (> (+ y1 height) bottom)
	      (scl:send-if-handles
	       stream :set-viewport-position left (+ top height)))))
      (stream-increment-cursor-position stream 0 height)
      (multiple-value-bind (u v) (screen-to-uv stream x1 y1)
	(set-uv-outside graph STREAM u (+ u width) (- v height) v)
	(display graph stream)))))

(defun save-postscript-graph (graph filename &key (width 400) (height 400))
  (with-open-file (s filename :direction :output)
    (clim:with-output-to-postscript-stream (stream s)
      (display-graph graph :stream stream :width width :height height))))

(defun display-graphs (graphs &key
			      (stream *standard-output*)
			      (width 500)
			      (height 500))
  "Display a column of graphs"
  (let ((h (ROUND height (length graphs)))) 
    (dolist (graph graphs)
      (display-graph graph
		     :stream stream
		     :height h :width width))))

(defun window-reverse-video (window &optional (fore :white) (back :black))
  "Change the foreground/background colors of the window."
  (setf (medium-foreground window) (alu-for-stream window fore)
        (medium-background window) (alu-for-stream window back)))

(defun autoscale-graphs (graphs autoscale-type)
  "Let the graphs mutually decide what scaling limits to use.
   Use this when many different graphs should have the same scale."
  (when (> (length graphs) 1)
    (let* ((minx 1.0e+30)
	   (maxx -1.0e+30)
	   (miny minx)
	   (maxy maxx))
      (dolist (graph graphs)
	(setf (auto-scale graph) :both)
	;; I can't tell if i have to do this, so i will:
	(do-auto-scale graph)
	(multiple-value-bind (x0 x1 y0 y1)
	    (graph-auto-scale-limits graph)
	  (when (member autoscale-type '(:x :both))
	    (setq minx (min minx x0))
	    (setq maxx (max maxx x1)))
	  (when (member autoscale-type '(:y :both))
	    (setq miny (min miny y0))
	    (setq maxy (max maxy y1)))))
      (dolist (graph graphs)
	(when (member autoscale-type '(:x :both))
	  (setf (xll graph) minx)
	  (setf (xur graph) maxx))
	(when (member autoscale-type '(:y :both))
	  (setf (yll graph) miny)
	  (setf (yur graph) maxy))
	(setf (auto-scale graph)
	      (case autoscale-type
		(:x :y)
		(:y :x)
		(:both nil)
		(otherwise :both)))))
    t))

(defun fill-window-with-graphs (graphs
				&key
				autoscale ; :X, :Y, :BOTH, or NIL
				(right-margin 0)
				(columns 1)
				(stream *standard-output*)
				(reverse-video :own-color))
  "Fill the window with columns graphs."
  (when (and (color-stream-p stream) (not (eql reverse-video :own-color)))
    (if reverse-video
	(window-reverse-video stream :white :black)
        (window-reverse-video stream :black :white)))
  (if autoscale (autoscale-graphs graphs autoscale))
  (window-clear stream)
  (when graphs
    (multiple-value-bind (w h) (stream-viewport-size stream)
      (decf w right-margin)
      (with-output-truncation (stream) ; don't wrap or scroll
	(if (= columns 1)
	    (display-graphs graphs :stream stream :height h :width w)
	  (let ((rows (values (ceiling (length graphs) columns))))
	    (dotimes (column columns)
	      (let ((g nil))
		(dotimes (row rows)
		  (let ((temp (pop graphs)))
		    (and temp (push temp g))))
		(stream-set-cursor-position*
		 stream (* (values (truncate w columns)) column) 0)
		(display-graphs (nreverse g)
				:stream stream
				:height h
				:width (values (truncate w columns)))))))))))

;;;
;;; Manipulating presentations
;;;

(defun graph-under-presentation (presentation)
  (when (presentation-p presentation)
    (let ((object (presentation-object presentation)))
      (if (graph-p object) object
	  (let ((superior (clim:output-record-parent presentation)))
	    (when superior (graph-under-presentation superior)))))))

(defun dataset-under-presentation (presentation)
  (when (presentation-p presentation)
    (let ((object (presentation-object presentation)))
      (if (graph-data-p object) object
	  (let ((superior (clim:output-record-parent presentation)))
	    (when superior (dataset-under-presentation superior)))))))

(defun graph-under-annotation-under-presentation (presentation)
  (when (presentation-p presentation)
    (let ((object (presentation-object presentation)))
      (if (annotation-p object) (graph object)
	  (let ((superior (clim:output-record-parent presentation)))
	    (when superior
	      (graph-under-annotation-under-presentation superior)))))))

(defun graph-under-mouse ()
  (let ((stream (window-under-mouse)))
    (let ((p (presentation-under-pointer stream)))
      (when p (graph-under-presentation p)))))



;;;
;;; CP commands for graphs and graph-data.
;;;
(define-graph-command com-zoom-in ((graph 'graph) (WINDOW 'sheet))
  "Zoom in on a selected rectangle of the graph."
  (zoom-in graph window))

(define-presentation-to-command-translator zoom-in
  (graph :command-name com-zoom-in 
	 :command-table :graph
	 :gesture nil :documentation "Zoom In...")
  (object &key window)
  (list object window))

(define-graph-command com-zoom-out ((graph 'graph) (WINDOW 'sheet))
  "Undo the results of the most recent zoom-in command."
  (zoom-out graph WINDOW))

(define-presentation-to-command-translator zoom-out
  (graph :command-name com-zoom-out 
	 :command-table :graph
	 :tester ((object) (and (graph-p object) (zoom-stack object)))
	 :gesture nil :documentation "Zoom Out")
  (object &key window)
  (list object window))

(define-graph-command com-slider-crosshairs ((graph 'graph) (WINDOW 'sheet))
  "Display crosshairs on the graph at the current pointer position."
   (multiple-value-bind (x y) (uv-under-mouse window)
     (multiple-value-setq (x y)
       (uv-to-xy graph x y))
     (multiple-value-setq (x y) (slider-interact graph WINDOW x y t))
     (and x y (describe-point graph x y))))

(define-presentation-to-command-translator slider-crosshairs
  (graph :command-name com-slider-crosshairs
	 :command-table :graph
	 :gesture nil :documentation "Crosshairs")
  (object &key window)
  (list object window))

(define-graph-command com-redraw-graph ((graph 'graph) (window 'sheet))
  "Erase and then redraw a graph."
  (refresh graph window))

(define-presentation-to-command-translator com-redraw-graph
   (graph :command-name com-redraw-graph
	  :command-table :graph
	  :gesture nil :documentation "Redraw Graph")
   (object &key window)
  (list object window))

(define-graph-command com-reveal-datasets ((graph 'graph) (WINDOW 'sheet))
  "Reveals any data previously hidden by 'Remove Dataset'."
  (setf (hidden-datasets graph) nil)
  (refresh graph window))

(define-presentation-to-command-translator com-reveal-datasets
  (graph :command-name com-reveal-datasets
	 :command-table :graph
	 :gesture nil :documentation "Reveal Hidden Data"
	 :tester ((object)
		  (and (graph-p object)
		       (not (null (hidden-datasets object))))))
  (object &key window)
  (list object window))

(define-graph-command com-remove-dataset ((dataset 'graph-data) (window 'sheet) (presentation 't))
  "Hides some graph data.  Unhide it using 'Reveal Datasets.'"
  (let ((g (graph-under-presentation presentation)))
    (when g
      (push dataset (hidden-datasets g))
      (refresh g window))))

(define-presentation-to-command-translator com-remove-dataset
   (graph-data :command-name com-remove-dataset
	       :command-table :graph
	       :gesture nil :documentation "Hide Data"
	       :Tester ((object &key presentation)
			(declare (ignore object))
			(or (graph-under-presentation presentation)
			    (graph-under-annotation-under-presentation
			     presentation))))
   (object &key window presentation)
  (list object window presentation))


(defun draw-dash-sample (stream dash-pattern pretty-name selected-p)
  "Show what this dash pattern looks like."
  (declare (ignore pretty-name))
  (multiple-value-bind (x y) (stream-cursor-position* stream)
    ;; (format stream "(~A ~A)" x y)
    (let ((width (* 3 (stream-character-width stream)))
	  (height (* 6 (stream-line-height stream)))
	  (thick (+ 2 %thickness))
	  (thin %thickness)
	  (fudge 2))
      (device-draw-line stream (+ x fudge) (+ y fudge)
			(- (+ x width) fudge) (- (+ y height) fudge)
			:thickness (if selected-p thick thin)
			:dash-pattern dash-pattern
			:transform nil
			:alu %draw)
      (draw-rectangle x (+ x width) (+ y height) y
		      :stream stream
		      :filled nil
		      :alu (if selected-p %draw %erase))
      (force-output stream))))

#-clim-2
(define-presentation-type dash-pattern ()
  :description "a line dash pattern"
  :parser ((stream)
	   (completing-from-suggestions (stream)
	     (dotimes (i 7)
	       (suggest (princ-to-string i) i))))
  :printer ((object stream)
	    (draw-dash-sample stream object nil nil))
  :accept-values-displayer
  ((stream object query-identifier)
   (accept-values-choose-from-sequence
     stream *dash-pattern-alist* object query-identifier
     :drawer #'draw-dash-sample)))

(define-presentation-type-abbreviation dash-pattern ()
  `((member ,@(let ((numbers nil))
		(dotimes (i 7) (push i numbers))
		(nreverse numbers)))
    :name-key princ-to-string
    :printer present-line-style
    :highlighter highlight-line-style))

(defun present-line-style (object stream &key acceptably)
  (declare (ignore acceptably))
  (if (stringp object) (setq object (read-from-string object)))
  (with-room-for-graphics (stream)
    (draw-dash-sample stream object (princ-to-string object) nil)))

(defun highlight-line-style (continuation object stream)
  (clim:surrounding-output-with-border
   (stream)
   (funcall continuation object stream)))
  


;;; Clim 0.9 seems to be missing a bunch of presentation types,
;;; probably because AND and OR are missing.  Here we kludge up 
;;; a solution until CLIM gets better.
;;; CLIM IS BETTER NOW (CLIM 2.0.BETA).  LETS GET RID OF THIS.  JPM.
(define-presentation-type string-or-none ()
  :description "a string or None"
  :printer ((object stream)
	    (if (or (not object) (equal object ""))
		(write-string "None" stream)
	      (present object 'string :stream stream)))
  :parser ((stream)
	   (let ((string (accept 'string :stream stream :prompt nil :default nil)))
	     (setq string (string-trim '(#\space) string))
	     (if (or (string= string "") 
		     (string-equal string "None"))
		 (values nil 'string-or-none)
	       (values string 'string-or-none)))))

(define-presentation-type number-or-none ()
  :description "a number or None"
  :printer ((object stream)
	    (if object
		(present object 'number :stream stream)
		(write-string "None" stream)))
  :parser ((stream)
	   (let ((string (read-token stream)))
	     (if (string-equal (string-trim '(#\space) string) "None")
		 (values nil 'number-or-none)
		 (let ((number (read-from-string string)))
		   (if (numberp number)
		       (values number 'number-or-none)
		       (input-not-of-required-type stream string 'number-or-none)))))))

