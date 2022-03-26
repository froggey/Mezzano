;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Lowercase: Yes -*-
 
(defvar *classes*)

(eval-when (compile load eval)
  (pushnew :eps-bbox cl:*features*))

(defmacro with-postscript-stream ((stream pathname) &body body)
  `(with-postscript-stream-1 ,pathname #'(lambda (,stream) ,@body)))

(defun with-postscript-stream-1 (pathname continuation)
  (declare (sys:downward-funarg continuation))
  (let ((edges nil))
    #+eps-bbox
    (let ((stream *standard-output*))
      (send stream :clear-history)
      (funcall continuation stream)
      (let ((left nil)
	    (top nil)
	    (right nil)
	    (bottom nil))
	(flet ((compute-box (presentations)
		 (map nil #'(lambda (presentation)
			      (multiple-value-bind (ll tt rr bb)
				  (dw:box-edges (dw:presentation-displayed-box presentation))
				(dw::minf-or left ll)
				(dw::minf-or top  tt)
				(dw::maxf-or right  rr)
				(dw::maxf-or bottom bb)))
		      presentations)))
	  (compute-box (send stream :displayed-presentations))
	  (compute-box (send stream :displayed-strings))
	  (compute-box (send stream :displayed-graphics)))
	(setq edges (list left top right bottom))))
    (with-open-stream (stream (hardcopy:make-hardcopy-stream (list :file pathname)))
      #+eps-bbox (send stream :set-eps-bounding-box edges)
      (funcall continuation stream))))

#---ignore	;SWM's version
(progn

(setq *classes*
      '((design colored colorless uniform non-uniform solid translucent)
	(colored color)
	(colorless opacity region)
	(solid color region)
	(translucent opacity)
	(uniform color opacity unbounded-region)
	(non-uniform bounded-region)
	(region unbounded-region bounded-region region-set)
	(unbounded-region everywhere nowhere)
	(bounded-region path point area)
	(path polyline elliptical-arc)
	(point)
	(area polygon ellipse)
	(polyline line)
	(polygon rectangle)
	))

(defun design-classes (&optional (stream *standard-output*) 
				 (pathname #p"sys:clim;rel-2;specs;design-classes.ps"))
  (with-postscript-stream (stream pathname)
    (fresh-line stream)
    (let ((direction :after)
	  (roots '(design)))
      (labels ((do-one (class stream &rest connections)
		 (let ((graph-node (dw:find-graph-node stream class)))
		   (if graph-node
		       (dw:connect-graph-nodes stream graph-node connections
					       :drawing-options '(:thickness 0))
		       (setq graph-node
			     (dw:formatting-graph-node
			       (stream :id class
				       :connections connections
				       :drawing-options '(:thickness 0))
			       (if (member class '(design
						   color opacity
						   region region-set
						   everywhere nowhere
						   point path area
						   polyline line elliptical-arc
						   polygon rectangle ellipse))
				   (with-character-face (:bold stream)
				     (format stream "~(~S~)" class))
				   (format stream "~(~S~)" class))))
		     (map nil #'(lambda (node)
				  (do-one node stream direction graph-node))
			  (cdr (assoc class *classes*))))
		   graph-node)))
	(formatting-graph (stream :orientation :vertical
				  :branch-point :at-parent
				  :allow-overlap t
				  :balance-evenly nil
				  :row-spacing 30
				  :within-row-spacing 15)
	  (dolist (root roots)
	    (do-one root stream)))))))

)	;SWM's version


#+++ignore	;Moon's version
(progn

(setq *classes*
      '((design () color opacity region)
	(color (colored solid uniform))
	(opacity (colorless translucent uniform))
	(region (colorless solid) bounded-region unbounded-region region-set)
	(unbounded-region () everywhere nowhere)
	(everywhere (colorless solid uniform))
	(nowhere (colorless solid uniform))
	(bounded-region () point path area)
	(path () polyline elliptical-arc)
	(polyline () line)
	(area () polygon ellipse)
	(polygon () rectangle)
	))

(defun design-classes (&optional (stream *standard-output*) 
				 (pathname #p"sys:clim;rel-2;specs;design-classes.ps"))
  (with-postscript-stream (stream pathname)
    (fresh-line stream)
    (let ((direction :after)
	  (roots '(design)))
      (labels ((do-one (class stream &rest connections)
		 (let ((graph-node (dw:find-graph-node stream class)))
		   (if graph-node
		       (dw:connect-graph-nodes stream graph-node connections
					       :drawing-options '(:thickness 0))
		       (setq graph-node
			     (dw:formatting-graph-node
			       (stream :id class
				       :connections connections
				       :drawing-options '(:thickness 0))
			       (surrounding-output-with-border (stream :shape :oval :margin 3)
				 (with-character-face (:bold stream)
				   (format stream "~(~S~)" class))
				 (when (second (assoc class *classes*))
				   (terpri stream)
				   (with-character-size (:smaller stream)
				     (dolist (attribute (second (assoc class *classes*)))
				       (format stream "~(~A~) " attribute)))))))
		     (map nil #'(lambda (node)
				  (do-one node stream direction graph-node))
			  (cddr (assoc class *classes*))))
		   graph-node)))
	(formatting-graph (stream :orientation :vertical
				  :branch-point :at-parent
				  :allow-overlap t
				  :balance-evenly nil
				  :row-spacing 40
				  :within-row-spacing 15)
	  (dolist (root roots)
	    (do-one root stream)))))))

)	;Moon's version