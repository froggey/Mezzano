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

;;; Make popup-accept work for graphs and graph data.

(defmethod popup-accept ((SELF basic-graph-data) STREAM)
  (popup-accept-standard-loop self stream))

(defmethod popup-accept ((SELF basic-graph) STREAM)
  (popup-accept-standard-loop self stream))

(defmethod popup-accept-from-presentation :around
	   ((self basic-graph-data) stream presentation)
  "Refresh the graph if there is one under this presentation."
  (multiple-value-bind (ignore aborted-p)
      (call-next-method self stream presentation)
    (declare (ignore ignore))
    (when (not aborted-p)
      (let ((graph (or (graph-under-presentation presentation)
		       (graph-under-annotation-under-presentation presentation))))
	(when graph 
	  (setf (auto-scale-needed graph) t) ; just in case
	  (refresh graph stream))))
    (values self aborted-p)))

(defmethod popup-accept-from-presentation :around
	   ((self basic-graph) stream presentation)
  "Refresh the graph if there is one under this presentation."
  (multiple-value-bind (ignore aborted-p)
      (call-next-method self stream presentation)
    (declare (ignore ignore))
    (when (not aborted-p)
      (let ((graph (graph-under-presentation presentation)))
	(when (eq graph self) 
	  (setf (auto-scale-needed graph) t) ; just in case
	  (refresh graph stream))))
    (values self aborted-p)))

(defmethod pop-accept-label ((self basic-graph-data)) (name-string self))
(defmethod pop-accept-label ((self basic-graph)) (name-string self))

(define-graph-command com-pop-edit-graph
    ((graph 'graph) (window 'sheet) (presentation 'invisible-object))
  "Edit the attributes of a graph in a dialog box."
  (popup-accept-from-presentation graph WINDOW PRESENTATION))

(define-presentation-to-command-translator com-pop-edit-graph
  (graph :command-name com-pop-edit-graph
	 :command-table :graph
	 :documentation "Edit Graph Borders & Labels..."
	 :menu t
	 :gesture :select) 
  (object &key presentation window)
  (list object window presentation))

(define-graph-command com-pop-edit-dataset
    ((dataset 'graph-data) (window 'sheet) (presentation 'invisible-object))
  "Edit the attributes of some graph data in a dialog box."
  (popup-accept-from-presentation dataset WINDOW PRESENTATION))

(define-presentation-to-command-translator com-pop-edit-dataset
   (graph-data :command-name com-pop-edit-dataset
	       :command-table :graph
	       :documentation "Change Data Symbols..."
	       :menu t
	       ;; Genera screws up on mouse sensitivity
	       ;; unless some translator has a :left gesture.
	       :gesture :select) 
   (object &key presentation window)
  (list object window presentation))

(install-command 'clim::accept-values 'com-pop-edit-dataset)

(define-presentation-to-command-translator com-pop-edit-dataset
   (graph-data :command-name com-pop-edit-dataset
	       :command-table clim::accept-values
	       :documentation "Change Data Symbols..."
	       :menu t
	       :gesture :select) 
   (object &key presentation window)
  (list object window presentation))


(defmethod pop-accept-items progn ((self named-mixin) MENU-STREAM GRAPH-WINDOW)
    (declare (ignore GRAPH-WINDOW))
    ;; Names are symbols.  For the sake of user-friendliness, this method accepts a
    ;; string, which is more obvious to the user, and coerces it to a symbol.
    (let* ((name (name self))
	   (name-string (string name)))
      (setq name-string
	    (clim:accept 'string
                         :stream menu-stream
                         ;; this used to be text-field-view, but
                         ;; McCLIM doesn't have an %accept method for
                         ;; that, so let's use textual-dialog-view for
                         ;; now. Suggestions welcome on the correct
                         ;; thing to do here.
                         :view 'textual-dialog-view
                         :prompt "Name"
                         :default name-string))
      (terpri menu-stream)
      (setf (name self) name)))

(defmethod pop-accept-items progn ((self graph-data-x-offset-mixin) MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (popup-accept-forms (MENU-STREAM)
    (pa-slot x-offset " X Offset" 'number-or-none)))

(defmethod pop-accept-items progn ((self graph-data-y-offset-mixin) MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (popup-accept-forms (MENU-STREAM)
    (pa-slot y-offset " Y Offset" 'number-or-none)))

(defmethod pop-accept-items progn ((self graph-data-xy-offset-mixin) MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (popup-accept-forms (MENU-STREAM)
    (pa-slot x-offset " X Offset" 'number-or-none)
    (pa-slot y-offset " Y Offset" 'number-or-none)))

(defmethod pop-accept-items progn ((self graph-data-dither-mixin) MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (popup-accept-forms (MENU-STREAM)
    (pa-slot x-dither " X Dither" 'number-or-none)
    (pa-slot y-dither " Y Dither" 'number-or-none))
  
  (with-slots (x-dither y-dither) self
    (if x-dither (setq x-dither (max 0.0 x-dither)))   ; make it positive.
    (if y-dither (setq y-dither (max 0.0 y-dither)))
    ))

(defmethod pop-accept-items progn ((SELF graphics-style-mixin) MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (popup-accept-forms (MENU-STREAM)
    (pa-string "Drawing style")
    (pa-slot pattern        "  Pattern" `(alist-member :alist ,*SCI-GRAPH-AVAILABLE-STIPPLES*))
    (pa-slot THICKNESS      "  Line Thickness" 'number)
    ))

(defmethod pop-accept-items progn ((self basic-graph-datum-symbology-mixin)
				   MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (let ((symbology-choices (symbology-choices self)))
    (when (cdr symbology-choices)
      (popup-accept-forms (MENU-STREAM)
        (pa-slot symbologies "Plotting styles"
		 `(alist-subset :alist ,symbology-choices))))))

(defmethod pop-accept-items progn ((self graph-datum-line-symbology-mixin)
				   MENU-STREAM GRAPH-WINDOW)
 (declare (ignore GRAPH-WINDOW))
 (with-slots (line-style symbologies) self
    (when (contains-symbology-class symbologies :line)
      (setf line-style
	(accept 'dash-pattern
		:view '(radio-box-view :orientation :horizontal)
		:stream menu-stream
		:default line-style
		:prompt "Line Style"))
      (terpri menu-stream))
   (popup-accept-forms
    (MENU-STREAM) 
    (when (contains-symbology-class symbologies :line-symbol)
      (pa-slot min-symbol-spacing "Minimum Symbol Spacing" 'integer)))))

(defmethod pop-accept-items progn ((self graph-datum-bar-symbology-mixin)
				   MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (with-slots (bar-width symbologies) self
    (when (contains-symbology-class symbologies :bar)
      (popup-accept-forms (MENU-STREAM)
        (pa-slot bar-width "Bar Width" 'number-or-none)))))

(defmethod pop-accept-items progn ((self graph-datum-scatter-symbology-mixin)
				   MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (with-slots (data-symbol symbol-height symbologies) self

    ;;;NLC19NOV90 - Do side-effect before SO "DISPLAY WON'T CHANGE BETWEEN REDISPLAYS"
    (unless symbologies
      (setq symbologies (list :scatter)))	  ; DEFAULT

    (when (contains-symbology-class symbologies :scatter)
      (popup-accept-forms (MENU-STREAM)
        (pa-slot symbol-height "Symbol Height" 'number)
	(progn
	  (setq data-symbol
	    (accept `(graph-symbol 
		      :symbols
		      (:+ :x :* :point :triangle :box :diamond :circle))
		    :view +list-pane-view+
		    :stream menu-stream
		    :default data-symbol
		    :prompt "Symbol"))
	  (terpri menu-stream))
	))
    
    (unless symbologies
      (setq symbologies (list :scatter)))	  ; DEFAULT
    ))

(defmethod pop-accept-items progn ((self graph-data-color-mixin)
				   MENU-STREAM GRAPH-WINDOW)
  (when (color-stream-p graph-window)
    (multiple-value-bind (x y) (stream-cursor-position* menu-stream)
      (setf (slot-value self 'color)
	(accept 'color-presentation
		:view +list-pane-view+
		:stream menu-stream
		:default (slot-value self 'color)
		:prompt "Color"))
      (terpri menu-stream)
      (multiple-value-bind (x1 y1) (stream-cursor-position* menu-stream)
	(stream-set-cursor-position* menu-stream (+ x 250) (+ y 50))
	(with-room-for-graphics (menu-stream)
	  (draw-color-swatch menu-stream (slot-value self 'color) nil nil 35))
	(stream-set-cursor-position* menu-stream x1 y1)))))

(defmethod pop-accept-items progn ((self graph-data-auto-scale-mixin)
				   MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (popup-accept-forms (MENU-STREAM)
    (pa-slot auto-scale? "Contribute to Auto Scaling?" 'boolean)))

(defmethod pop-accept-items progn ((self presentable-mixin)
				   MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (popup-accept-forms (MENU-STREAM)
    (pa-slot present-self-p "Mouse sensitive data points?" 'boolean))
  (setf (graph-present-inferiors-p self) (present-self-p self)))

(defmethod pop-accept-items progn ((self basic-graph)
				   MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (let ((type (auto-scale self)))
    (when (not (eq type :both))
      (popup-accept-forms (MENU-STREAM)
	(pa-string "Axis Limits")
	(when (member type '(:y nil))
	  (pa-slot xll " X Left"   'number)
	  (pa-slot xur " X Right"  'number))
	(when (member type '(:x nil))
	  (pa-slot yll " Y Bottom" 'number)
	  (pa-slot yur " Y Top"    'number))))))

(defmethod pop-accept-items progn ((self GRAPH-BORDER-OB-MIXIN)
				   MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (with-slots (show-border x-auto-tick x-dtick
			   y-auto-tick y-dtick visible-borders)
      SELF
    (popup-accept-forms (MENU-STREAM)
	(pa-string "Labels and Borders")
	(pa-slot title       	  "  Title"          'string-or-none)
	(pa-slot x-label     	  "  X axis label"   'string-or-none)
	(pa-slot x-digits    	  "  X digits"       'number-or-none)
	(pa-slot x-tick-numbering "  X Tick numbering"
		 '(alist-member :alist
				(("None" :value nil)
				 ("First & Last" :value :minimal)
				 ("Each" :value :each))))
	(pa-slot x-auto-tick 	  "  X Auto tick"      'boolean)
	(unless x-auto-tick
	  (pa-slot x-dtick        "    X Tick spacing" 'number))
	(unless (or x-auto-tick x-dtick)
	  (pa-warn "For X axis: Choose Auto tick or provide a tick spacing"))
	(pa-slot y-label	  "  Y axis label"   'string-or-none)
	(pa-slot y-digits	  "  Y digits"       'number-or-none)
	(pa-slot y-tick-numbering "  Y Tick numbering"
		 '(alist-member :alist
				(("None" :value nil)
				 ("First & Last" :value :minimal)
				 ("Each" :value :each))))
	(pa-slot y-auto-tick	  "  Y Auto tick"      'boolean)
	(unless y-auto-tick
	  (pa-slot y-dtick        "    Y Tick spacing" 'number))
	(unless (or y-auto-tick y-dtick)
	  (pa-warn "For Y axis: Choose Auto tick or provide a tick spacing"))
	(pa-slot visible-borders "  Visible Borders"
		 '(alist-subset :alist (:left :right :bottom :top
					:zero-abcissa :zero-ordinate)))
	)))

(defmethod pop-accept-unsatisfied-warnings or ((self GRAPH-BORDER-OB-MIXIN))
  (with-slots (x-auto-tick x-dtick y-auto-tick y-dtick) SELF
    (popup-accept-forms (STREAM)
      (or (not (or x-auto-tick x-dtick))
	  (not (or y-auto-tick y-dtick))))))

(defmethod pop-accept-items progn ((self graph-grid-ob-mixin)
				   MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (with-slots (show-grid x-auto-grid x-dgrid y-auto-grid y-dgrid) self
    (popup-accept-forms (MENU-STREAM)
      (pa-slot show-grid "Display Grid?" 'boolean)
      (when show-grid
	(unless (or x-auto-grid x-dgrid)
	  (pa-warn "For X axis: Choose Auto Grid or provide a grid spacing"))
	(pa-slot x-auto-grid "  X Default Grid" 'boolean)
	(unless x-auto-grid
	  (pa-slot x-dgrid "  Grid spacing" 'number))
	(unless (or y-auto-grid y-dgrid)
	  (pa-warn "For Y axis: Choose Auto Grid or provide a grid spacing"))
	(pa-slot y-auto-grid "  Y Default Grid" 'boolean)
	(unless y-auto-grid
	  (pa-slot y-dgrid "  Grid spacing" 'number))))))

(defmethod pop-accept-unsatisfied-warnings or ((SELF graph-grid-ob-mixin))
  (with-slots (x-auto-grid x-dgrid y-auto-grid y-dgrid) SELF
    (popup-accept-forms (STREAM)
      (or (not (or x-auto-grid x-dgrid))
	  (not (or y-auto-grid y-dgrid))))))

(defmethod pop-accept-items progn ((self graph-datasets-ob-mixin)
				   MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (with-slots (datasets) self
    (when datasets
      (popup-accept-forms (MENU-STREAM)
	(pa-string1 "Datasets:")
	(loop for DS in DATASETS
	      do (write-char #\space MENU-STREAM)
		 (present DS 'graph-data :stream MENU-STREAM))
	(terpri MENU-STREAM)
	))))

(defmethod pop-accept-items progn ((self graph-auto-scale-mixin)
				   MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (popup-accept-forms (MENU-STREAM)
    (pa-slot auto-scale "Auto Scaling?" '(alist-member :alist
						 (("X" :value :x)
						  ("Y" :value :y)
						  ("Both" :value :both)
						  ("None" :value nil))))))

(defmethod popup-accept :after ((self equation-data) stream)
  (declare (ignore stream))
  (with-slots (equation) self
    (setf (equation self) equation)))

(defmethod pop-accept-items progn ((self equation-data) MENU-STREAM GRAPH-WINDOW)
    (declare (ignore GRAPH-WINDOW))
    (with-slots (parameters) self
      (popup-accept-forms (MENU-STREAM)
			  (pa-slot equation "Equation" 'expression)
			  (pa-slot variable "Variable" 'expression)
			  (pa-slot min "Mininum variable value" 'number)
			  (pa-slot max "Maximum variable value" 'number)
			  (pa-slot increment "Increment" 'number)
			  )
      (format menu-stream "Parameters:")
      (terpri menu-stream)
      (loop for INDEX from 0 below (length PARAMETERS)
	    for QUERY-ID = (nth INDEX PARAMETERS)
	    do (setf (second (nth INDEX PARAMETERS))
		     (popup-accept-forms-accept
		      MENU-STREAM
		      (concatenate 'string "  "
				   (string (first (nth INDEX parameters))))
		      'number
		      (second (nth INDEX PARAMETERS))
		      QUERY-ID))
	    (terpri menu-stream))))

(defmethod popup-accept :after ((self graph-sample-data-mixin) STREAM)
  (declare (ignore STREAM))
  (when (sample-data self) (compute self)))

(defmethod pop-accept-items progn ((self histogram-data)
				   MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (popup-accept-forms (MENU-STREAM)
    (pa-slot min "Minimum Value" 'number-or-none)
    (pa-slot max "Maximum Value" 'number-or-none)
    (pa-slot bin-count "Number of Bins" 'number-or-none)
    (pa-slot bin-size "Bin Size" 'number-or-none))

  (with-slots (bin-count bin-size min max) self
    (when (and (numberp min) (numberp max))
      (when (numberp bin-count)
	(setq bin-size (or bin-size (float (/ (- max min) bin-count)))))
      (when (numberp bin-size)
	(setq bin-count (or bin-count (values (truncate (- max min) bin-size))))))))

(defmethod pop-accept-items progn ((self line-mixin) MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (multiple-value-bind (slope intercept)
      (slope-intercept self)
    (popup-accept-forms (MENU-STREAM)
      (pa-string "Slope: ~a" slope)
      (pa-string "Intercept: ~a" intercept))))

(defmethod pop-accept-items progn ((self GRAPH-DATA-LEGEND-MIXIN)
				   MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (popup-accept-forms (MENU-STREAM)
    (pa-slot show-legend "Display on legend?" 'boolean)))

(defmethod pop-accept-items progn ((self graph-legend-mixin)
				   MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (popup-accept-forms (MENU-STREAM)
    (pa-slot show-legend "Display legend?" 'boolean)))

