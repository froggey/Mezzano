;;; cl-pdf copyright 2002-2009 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)

;;WARNING this part of cl-pdf is an alpha version. Use with care!

;; Basic charts: histogram, pie and plot-xy
;; (Thanks to Carlos Ungil <Carlos.Ungil@cern.ch> for plot-xy)


;; the pre and post draw functions must have this signature:
;; (defun pre-draw-chart (object dx dy x-scale y-scale x-min y-min x-max y-max) ...)

;;(defvar *default-chart-font* (get-font))

(defclass chart-item ()
  ((x :accessor x :initform 0 :initarg :x)
   (y :accessor y :initform 0 :initarg :y)
   (width :accessor width :initform 0 :initarg :width)
   (height :accessor height :initform 0 :initarg :height)
   (background-color :accessor background-color :initform '(1 1 1) :initarg :background-color)
   (title :accessor title :initform "" :initarg :title)
   (title-font :accessor title-font :initform (get-font) :initarg :title-font)
   (title-font-size :accessor title-font-size :initform 12 :initarg :title-font-size)
   (title-color :accessor title-color :initform '(0 0 0) :initarg :title-color)
   (line-width :accessor line-width :initform 0.5 :initarg :line-width)
   (line-color :accessor line-color :initform '(0 0 0) :initarg :line-color)
   (pre-draw-chart-fn :accessor pre-draw-chart-fn :initform nil :initarg :pre-draw-chart-fn)
   (post-draw-chart-fn :accessor post-draw-chart-fn :initform nil :initarg :post-draw-chart-fn)
   ))

(defclass axis (chart-item)
  ((label-font :accessor label-font :initform (get-font) :initarg :label-font)
   (label-font-size :accessor label-font-size :initform 10.0 :initarg :label-font-size)
   (label-position :accessor label-position :initform :center :initarg :label-position)
   (label-rotation :accessor label-rotation :initform 0 :initarg :label-rotation)
   (label-color :accessor label-color :initform '(0 0 0) :initarg :label-color)
   (tick-length :accessor tick-length :initform 6 :initarg :tick-length)
   (tick-width :accessor tick-width :initform 0.25 :initarg :tick-width)
   (ticks-positions :accessor ticks-positions)
   (ticks-separation :accessor ticks-separation)
   ))

(defclass value-axis (axis)
  ((min-value :accessor min-value :initform 0 :initarg :min-value)
   (max-value :accessor max-value :initform 100 :initarg :max-value)
   (locked-values :accessor locked-values :initform nil :initarg :locked-values)
   (subtick-length :accessor subtick-length :initform 2 :initarg :subtick-length)
   (subtick-width :accessor subtick-width :initform 0.25 :initarg :subtick-width)
   (integer-tick :accessor integer-tick :initform nil :initarg :integer-tick)
   (nb-ticks :accessor nb-ticks :initform 10 :initarg :nb-ticks)
   (tick-value :accessor tick-value)
   (axis-scale :accessor axis-scale)
   (axis-min :accessor axis-min)
   (axis-max :accessor axis-max)
   (nb-subticks :accessor nb-subticks)
   (format-string :accessor format-string :initform nil :initarg :format-string)
   ))

(defmethod initialize-instance :after ((axis value-axis) &rest init-options &key &allow-other-keys)
  (declare (ignore init-options))
  (compute-scale axis))

(defclass histo-axis (axis)
  ((label-names :accessor label-names :initform nil :initarg :label-names)
   ))

(defclass vertical-value-axis (value-axis)
  ())

(defclass horizontal-value-axis (value-axis)
  ())

(defclass vertical-histo-axis (histo-axis)
  ())

(defclass horizontal-histo-axis (histo-axis)
  ())

(defgeneric axis-size (axis))

(defmethod axis-size (axis)
  (width axis))

(defmethod axis-size ((axis vertical-value-axis))
  (height axis))

(defmethod axis-size ((axis vertical-histo-axis))
  (height axis))

(defgeneric draw-object (obj))

(defmethod draw-object (obj)
  (declare (ignore obj)))

(defun nice-number (n approx integer-p)
  (let* ((n10 (expt 10 (floor (log n 10))))
	 (nf (/ n n10))
	 (value
	  (loop for (max val) in (if approx '((1.5 1)(3 2)(7 5)(15 10)) '((1 1)(2 2)(5 5)(15 10)))
		when (<= nf max) do (return (* val n10)))))
    (if integer-p
	(max 1 (round value))
	value)))

(defun compute-scale (axis)
  (let* ((min (min-value axis))
	 (max (max-value axis))
	 d nfrac)
    (when (= min max)
      (if (zerop min)
	(setf max 1.0)
	(if (plusp max)
	  (setf min 0.0 max (* max 1.01))
	  (setf min (* min 1.01) max 0.0))))
    (setf d (nice-number (/ (nice-number (- max min) nil (integer-tick axis)) (nb-ticks axis))
			 t (integer-tick axis)))
    (setf nfrac (max (- (floor (log d 10))) 0))
    (setf (tick-value axis) d
	  (axis-min axis) (* d (floor min d))
	  (axis-max axis) (* d (ceiling max d))
	  (nb-ticks axis) (1+ (ceiling (- (axis-max axis)(axis-min axis)) d))
	  (axis-scale axis) (/ (axis-size axis)(- (axis-max axis) (axis-min axis)))
	  (ticks-separation axis)(/ (axis-size axis) (1- (nb-ticks axis)))
	  (format-string axis)
	  (or (format-string axis)
	      (if (integer-tick axis)
		  "~d"
		  (format nil "~~,~df" nfrac)))
	  (ticks-positions axis) (make-array (nb-ticks axis)))
    (loop for tick from 0 below (nb-ticks axis)
	  for pos from 0 by (ticks-separation axis) do
	  (setf (aref (ticks-positions axis) tick) pos))))

(defmethod draw-object ((axis horizontal-histo-axis))
  (with-saved-state
    (set-line-width (line-width axis))
    (apply #'set-rgb-stroke (line-color axis))
    (translate (x axis) (y axis))
    (move-to 0 0)
    (line-to (width axis) 0)
    (stroke)
    (set-line-width (tick-width axis))
    (move-to 0 0)
    (line-to 0 (- (tick-length axis)))
    (stroke)
    (apply #'set-rgb-fill (label-color axis))
    (loop with nb = (length (label-names axis))
	  with d  = (/ (width axis) nb)
	  with l = (- (tick-length axis))
	  with font-size = (label-font-size axis)
	  with max-width = (- d (* 0.6 font-size))
	  with text-y = (* -1.25 font-size)
	  for name in (label-names axis)
	  for tx from d by d
	  for text-x from (* 0.5 d) by d do
	  (move-to tx 0)
	  (line-to tx l)
	  (stroke)
	  (draw-centered-text text-x text-y name (label-font axis) font-size max-width))))

(defmethod draw-object ((axis vertical-value-axis))
  (with-saved-state
    (set-line-width (line-width axis))
    (apply #'set-rgb-stroke (line-color axis))
    (translate (x axis) (y axis))
    (move-to 0 0)
    (line-to 0 (height axis))
    (stroke)
    (set-line-width (tick-width axis))
    (move-to 0 0)
    (line-to (- (tick-length axis)) 0)
    (stroke)
    (apply #'set-rgb-fill (label-color axis))
    (loop with nb = (nb-ticks axis)
	  with d  = (ticks-separation axis)
	  with l = (- (tick-length axis))
	  with font-size = (label-font-size axis)
	  with text-x = (* l 1.25)
	  with format = (format-string axis)
	  repeat nb
	  for value from (axis-min axis) by (tick-value axis)
	  for y from 0 by d
	  for text-y from (* -0.35 font-size) by d do
	  (move-to 0 y)
	  (line-to l y)
	  (stroke)
	  (when (integer-tick axis) (setf value (round value)))
	  (draw-left-text text-x text-y (format nil format value) (label-font axis) font-size))))

(defclass legend (chart-item)
  ((label-font :accessor label-font :initform (get-font) :initarg :label-font)
   (label-font-size :accessor label-font-size :initform 10.0 :initarg :label-font-size)
   (label-color :accessor label-color :initform '(0 0 0) :initarg :label-color)
   (labels&colors :accessor labels&colors :initform () :initarg :labels&colors)
   ))

(defmethod draw-object ((obj legend))
  (let* ((nb (length (labels&colors obj)))
	 (font-size (label-font-size obj))
	 (space (* 0.2 font-size))
	 (line-height (+ space font-size))
	 (height (+ (* nb line-height) space))
	 (width (+ (* 3 space) font-size
		   (reduce 'max (labels&colors obj)
			   :key #'(lambda (lc) (text-width (first lc) (label-font obj)font-size))))))
    (with-saved-state
      (set-line-width (line-width obj))
      (apply #'set-rgb-stroke (line-color obj))
      (apply #'set-rgb-fill (background-color obj))
      (translate (x obj) (+ (y obj)(* 0.5 (- (height obj) height))))
      (basic-rect 0 0 width height)
      (fill-and-stroke)
      (basic-rect 0 0 width height)
      (clip-path)
      (end-path-no-op)
      (loop with dx = font-size
	    with dy = font-size
	    with text-x = (+ dx (* 2 space))
	    for (name color) in (labels&colors obj)
	    for y downfrom (- height line-height) by line-height do
	    (apply #'set-rgb-fill color)
	    (basic-rect space y dx dy)
	    (fill-and-stroke)
	    (apply #'set-rgb-fill (label-color obj))
	    (in-text-mode
	     (move-text text-x y)
	     (set-font (label-font obj) font-size)
	     (show-text name))
	    ))))

(defclass histogram (chart-item)
  ((label-names :accessor label-names :initform nil :initarg :label-names)
   (series :accessor series :initform () :initarg :series)
   (stacked-series :accessor stacked-series :initform () :initarg :stacked-series)
   (labels&colors :accessor labels&colors :initform () :initarg :labels&colors)
   (h-lines-width :accessor h-lines-width :initform 0.2 :initarg :h-lines-width)
   (h-lines-color :accessor h-lines-color :initform '(0.5 0.5 0.5) :initarg :h-lines-color)
   (groups-spacing :accessor groups-spacing :initform 0.2 :initarg :groups-spacing)
   (x-axis :accessor x-axis)
   (y-axis :accessor y-axis)
   (legend :accessor legend :initform nil)
   ))

(defmethod initialize-instance :after ((histo histogram) &rest init-options &key
				       x-axis-options y-axis-options legend-options &allow-other-keys)
  (declare (ignore init-options))
  (setf (y-axis histo)
	(apply #'make-instance 'vertical-value-axis
	       :x (x histo) :y (y histo) :height (height histo)
               (append y-axis-options
                       (list
                        :min-value (if (stacked-series histo)
				       0.0
				       (reduce #'min (mapcar #'(lambda (values)
								 (reduce #'min values))
							     (series histo))))
                        :max-value (reduce #'max
					   (if (stacked-series histo)
					       (apply 'mapcar #'(lambda (&rest values)
								  (apply '+ values))
						      (series histo))
					       (mapcar #'(lambda (values)
							   (reduce #'max values))
						       (series histo))))))))
  (setf (x-axis histo)
	(apply #'make-instance 'horizontal-histo-axis
               :x (x histo) :y (y histo) :width (width histo)
               :label-names (label-names histo)
               x-axis-options))
  (when (> (length (series histo)) 1)
    (setf (legend histo)
	(apply #'make-instance 'legend
	       :x (+ (x histo) (width histo) 10) :y (y histo) :width 60 :height (height histo)
	       :labels&colors (labels&colors histo)
	       legend-options))))

(defmethod draw-object ((obj histogram))
  (let* ((nb-series (if (stacked-series obj) 1 (length (series obj))))
	 (nb-values (length (first (series obj))))
	 (width (width obj))
	 (group-width (/ width nb-values))
	 (spacing (* (groups-spacing obj) group-width))
	 (bar-width (/ (- group-width spacing) nb-series))
	 (min-value (axis-min (y-axis obj)))
	 (scale (axis-scale (y-axis obj))))
    (with-saved-state
	(translate (x obj)(y obj))
      (set-line-width (line-width obj))
      (apply #'set-rgb-stroke (line-color obj))
      (apply #'set-rgb-fill (background-color obj))
      (basic-rect 0 0 width (height obj))
      (fill-and-stroke)
      (set-line-width (h-lines-width obj))
      (apply #'set-rgb-stroke (h-lines-color obj))
      (loop for tick-y across (ticks-positions (y-axis obj)) do
	    (move-to 0 tick-y)
	    (line-to width tick-y)
	    (stroke))
      (set-line-width (line-width obj))
      (apply #'set-rgb-stroke (line-color obj))
      (if (stacked-series obj)
	  (loop for values in (apply 'mapcar 'list (series obj))
		for gx from (* 0.5 spacing) by bar-width
		for bx from gx by group-width do
		(loop for y = 0.0 then (+ y dy)
		      for value in values
		      for (nil color) in (labels&colors obj)
		      for dy = (* value scale) do
		      (apply #'set-rgb-fill color)
		      (basic-rect bx y bar-width dy)
		      (fill-and-stroke)))
	  (loop for serie in (series obj)
		for gx from (* 0.5 spacing) by bar-width
		for (nil color) in (labels&colors obj) do
		(apply #'set-rgb-fill color)
		(loop for value in serie
		      for dy = (* (- value min-value) scale)
		      for bx from gx by group-width do
		      (basic-rect bx 0 bar-width dy)
		      (fill-and-stroke))))))
  (draw-object (x-axis obj))
  (draw-object (y-axis obj))
  (draw-object (legend obj)))


(defclass pie-chart (chart-item)
  ((serie  :accessor serie  :initform () :initarg :serie)
   (labels&colors :accessor labels&colors :initform () :initarg :labels&colors)
   (legend :accessor legend :initform nil)
   ))

(defmethod initialize-instance :after ((obj pie-chart) &rest init-options &key no-legend
				       legend-options &allow-other-keys)
  (declare (ignore init-options))
  (unless no-legend
    (setf (legend obj)
	  (apply #'make-instance 'legend
		 :x (+ (x obj) (width obj) 10) :y (y obj) :width 60 :height (height obj)
		 :labels&colors (labels&colors obj)
		 legend-options))))

(defmethod draw-object ((obj pie-chart))
  (let* ((sum (reduce #'+ (serie obj)))
	 (radius (* (width obj) 0.45))
	 angles)
    (when (zerop sum) (setf sum 1))
    (setf angles (mapcar #'(lambda (v) (/ (* 2 pi v) sum)) (serie obj)))
    (with-saved-state
      (translate (+ (x obj)(* 0.5 (width obj)))(+ (y obj)(* 0.5 (height obj))))
      (set-line-width (line-width obj))
      (apply #'set-rgb-stroke (line-color obj))
      (apply #'set-rgb-fill (background-color obj))
      (fill-and-stroke)
      (loop for angle in angles
	    for (nil color) in (labels&colors obj)
	    for start = 0 then end
	    for end = (+ start angle) do
	    (apply #'set-rgb-fill color)
	    (pie 0 0 radius start angle)
	    (fill-and-stroke))))
  (draw-object (legend obj)))


(defmethod draw-object ((axis horizontal-value-axis))
  (with-saved-state
    (set-line-width (line-width axis))
    (apply #'set-rgb-stroke (line-color axis))
    (translate (x axis) (y axis))
    (move-to 0 0)
    (line-to (width axis) 0)
    (stroke)
    (set-line-width (tick-width axis))
    (move-to 0 0)
    (line-to 0 (- (tick-length axis)))
    (stroke)
    (apply #'set-rgb-fill (label-color axis))
    (loop with nb = (nb-ticks axis)
	  with d  = (ticks-separation axis)
	  with l = (- (tick-length axis))
	  with font-size = (label-font-size axis)
	  with text-y = (+ (* l 1.25) (* -1 font-size))
	  with format = (format-string axis)
	  repeat nb
	  for value from (axis-min axis) by (tick-value axis)
	  for x from 0 by d
	  for text-x from (* -0.35 font-size) by d do
	  (move-to x 0)
	  (line-to x l)
	  (stroke)
	  (draw-centered-text text-x text-y (format nil format value) (label-font axis) font-size d))))

(defclass plot-xy (chart-item)
  ((series :accessor series :initform () :initarg :series)
   (labels&colors :accessor labels&colors :initform () :initarg :labels&colors)
   (h-lines-width :accessor h-lines-width :initform 0.2 :initarg :h-lines-width)
   (h-lines-color :accessor h-lines-color :initform '(0.5 0.5 0.5) :initarg :h-lines-color)
   (point-radius :accessor point-radius :initform 2 :initarg :point-radius)
   (x-axis :accessor x-axis)
   (y-axis :accessor y-axis)
   (legend :accessor legend :initform nil)
   ))

(defmethod initialize-instance :after ((plot plot-xy) &rest init-options &key
				       x-axis-options y-axis-options legend-options &allow-other-keys)
  (declare (ignore init-options))
  (setf (y-axis plot)
	(apply #'make-instance 'vertical-value-axis
	       :x (x plot) :y (y plot) :height (height plot)
               (append y-axis-options
                       (list
			:min-value
			(reduce #'min
				(mapcar #'(lambda (values)
					    (reduce #'min
						    (remove nil (mapcar #'second values))))
					(series plot)))
			:max-value
			(reduce #'max
				(mapcar #'(lambda (values)
					    (reduce #'max
						    (remove nil (mapcar #'second values))))
					(series plot)))))))
  (setf (x-axis plot)
	(apply #'make-instance 'horizontal-value-axis
	       :x (x plot) :y (y plot) :width (width plot)
               (append x-axis-options
                       (list
			:min-value
			(reduce #'min
				(mapcar #'(lambda (values)
					    (reduce #'min
						    (remove nil (mapcar #'first values))))
					(series plot)))
			:max-value
			(reduce #'max
				(mapcar #'(lambda (values)
					    (reduce #'max
						    (remove nil (mapcar #'first values))))
					(series plot)))))))
  (when (labels&colors plot)
    (setf (legend plot)
	(apply #'make-instance 'legend
	       :x (or (getf legend-options :x) (+ (x plot) (width plot) 10))
	       :y (or (getf legend-options :y) (y plot))
	       :width 60 :height (height plot)
	       :labels&colors (labels&colors plot)
	       legend-options))))


(defmethod draw-object ((obj plot-xy))
  (let* ((width (width obj))
	 (height (height obj))
	 (min-value-y (axis-min (y-axis obj)))
	 (min-value-x (axis-min (x-axis obj)))
	 (max-value-y (axis-max (y-axis obj)))
	 (max-value-x (axis-max (x-axis obj)))
	 (scale-y (axis-scale (y-axis obj)))
	 (scale-x (axis-scale (x-axis obj))))
    (with-saved-state
	(translate (x obj)(y obj))
      (set-line-width (line-width obj))
      (apply #'set-rgb-stroke (line-color obj))
      (apply #'set-rgb-fill (background-color obj))
      (basic-rect 0 0 width height)
      (fill-and-stroke)
      (when (pre-draw-chart-fn obj)
	(funcall (pre-draw-chart-fn obj) obj width height scale-x scale-y
		 min-value-x min-value-y max-value-x max-value-y))
      (set-line-width (h-lines-width obj))
      (apply #'set-rgb-stroke (h-lines-color obj))
      (loop for tick-y across (ticks-positions (y-axis obj)) do
	   (move-to 0 tick-y)
	   (line-to width tick-y)
	   (stroke))
      (loop for tick-x across (ticks-positions (x-axis obj)) do
	   (move-to tick-x 0)
	   (line-to tick-x height)
	   (stroke))
      (set-line-width (line-width obj))
      (set-line-join 2)
      (loop for serie in (series obj)
	    for (nil color) in (labels&colors obj) do
	   (apply #'set-rgb-stroke color)
	   (apply #'set-rgb-fill color)
	   (let ((points '())
		 (all-points '()))
	     (loop for (xx yy) in serie
		for x = (when xx (* (- xx min-value-x) scale-x))
		for y = (when yy (* (- yy min-value-y) scale-y))
		do
		  (if (and x y)
		      (progn
			(push (list x y) points)
			(unless (zerop (point-radius obj))
			  (circle x y (point-radius obj))
                          (fill-and-stroke)))
		      (when points
			(push points all-points)
			(setf points '()))))
	     (when points (push points all-points))
	     (map nil 'polyline all-points)
	     (stroke)))
      (when (post-draw-chart-fn obj)
	(funcall (post-draw-chart-fn obj) obj width height scale-x scale-y
		 min-value-x min-value-y max-value-x max-value-y))))
  (draw-object (x-axis obj))
  (draw-object (y-axis obj))
  (draw-object (legend obj)))
