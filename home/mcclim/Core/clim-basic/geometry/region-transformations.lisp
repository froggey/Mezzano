(in-package #:climi)

(defmethod transform-region (transformation (region standard-point))
  (with-slots (x y) region
    (multiple-value-bind (x* y*) (transform-position transformation x y)
      (make-point x* y*))))

(defmethod transform-region (transformation (region standard-polyline))
  (with-slots (points closed) region
    (make-polyline
     (mapcar (lambda (p)
               (multiple-value-bind (x* y*)
                   (transform-position transformation (point-x p) (point-y p))
                 (make-point x* y*)))
             points)
     :closed closed)))

(defmethod transform-region (transformation (region standard-polygon))
  (with-slots (points) region
    (make-polygon
     (mapcar (lambda (p)
               (multiple-value-bind (x* y*)
                   (transform-position transformation (point-x p) (point-y p))
                 (make-point x* y*)))
             points))))

(defmethod transform-region (transformation (line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (multiple-value-bind (x1* y1*) (transform-position transformation x1 y1)
      (multiple-value-bind (x2* y2*) (transform-position transformation x2 y2)
        (make-line* x1* y1* x2* y2*)))))

(defmethod transform-region (transformation (rect standard-rectangle))
  (cond ((rectilinear-transformation-p transformation)
         (with-standard-rectangle (x1 y1 x2 y2)
               rect
           (multiple-value-bind (x1* y1*)
               (transform-position transformation x1 y1)
             (multiple-value-bind (x2* y2*)
                 (transform-position transformation x2 y2)
               (make-rectangle* x1* y1* x2* y2*)))))
        (t
         (make-polygon (mapcar (lambda (p) (transform-region transformation p))
                               (polygon-points rect))))))

(defmethod transform-region (transformation (region elliptical-thing))
  (with-slots (start-angle end-angle tr) region
    ;; I think this should be untransform-angle below, as the ellipse angles
    ;; go counter-clockwise in screen coordinates, whereas our transformations
    ;; rotate clockwise..  -Hefner
    (let ((start-angle* (and start-angle
                             (untransform-angle transformation start-angle)))
          (end-angle*   (and end-angle
                             (untransform-angle transformation end-angle))))
      (when (reflection-transformation-p transformation)
        (rotatef start-angle* end-angle*))
      (make-instance (type-of region)
        :tr (compose-transformations transformation tr)
        :start-angle start-angle*
        :end-angle end-angle*))))

(defmethod transform-region (tr (region standard-rectangle-set))
  (cond ((scaling-transformation-p tr)
         (multiple-value-bind (mxx mxy myx myy tx ty)
             (get-transformation tr)
           (declare (ignore mxy myx))
           (let ((rev-x-p (< mxx 0))
                 (rev-y-p (< myy 0)))
             (flet ((correct (bands)
                      (loop for ((y . nil) (nil . xs)) on (nreverse bands)
                         collect `(,y . ,xs))))
               (make-standard-rectangle-set
                (loop for band in (standard-rectangle-set-bands region)
                   for new-band = (loop for x in (cdr band)
                                     collect (+ (* mxx x) tx) into new-xs
                                     finally (return (cons (+ (* myy (car band)) ty)
                                                           (if rev-x-p
                                                               (nreverse new-xs)
                                                               new-xs))))
                   collect new-band into new-bands
                   finally (return (if rev-y-p
                                       (correct new-bands)
                                       new-bands))))))))
        (t
         ;; We have insufficient knowledge about the transformation,
         ;; so we have to take the union of all transformed rectangles.
         ;; Maybe there is a faster way to do this.
         (let ((res +nowhere+))
           (map-over-region-set-regions
            (lambda (rect)
              (setf res (region-union res (transform-region tr rect))))
            region)
           res))))

(defmethod transform-region (tr (region everywhere-region))
  (declare (ignore tr))
  +everywhere+)

(defmethod transform-region (tr (region nowhere-region))
  (declare (ignore tr))
  +nowhere+)

(defmethod transform-region (tr (region standard-region-difference))
  (with-slots (a b) region
    (make-instance 'standard-region-difference
      :a (transform-region tr a)
      :b (transform-region tr b))))

(defmethod transform-region (tr (region standard-region-union))
  (with-slots (regions) region
    (make-instance 'standard-region-union
      :regions (mapcar (lambda (r) (transform-region tr r)) regions))))

(defmethod transform-region (tr (region standard-region-intersection))
  (with-slots (regions) region
    (make-instance 'standard-region-intersection
      :regions (mapcar (lambda (r) (transform-region tr r)) regions))))
