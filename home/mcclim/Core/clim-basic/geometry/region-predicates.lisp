(in-package #:climi)

;;; This file contains implementation of the region predicate protocol
;;; for several region classes. A few rules to follow:
;;;
;;; - use coordinate functions for comparisons
;;; - when possible provide a method for a protocol class, and when
;;;   beneficial provide, also provide a method for standard class
;;; - all "region" arguments must be specialized
;;; - the most general specialization is bounding-rectangle
;;;
;;; Regions which must be handled:
;;;
;;;   - 0 dimensions: point
;;;   - 1 dimensions: polyline, elliptical-arc
;;;   - 2 dimensions: polygon, ellipse
;;;
;;; Additionally a line and a rectangle should be handled (they are
;;; frequently used special cases of a polyline and a polygon).
;;;
;;; Methods should be defined from the most general to the most
;;; specific. Methods specialized on REGION, methods being a
;;; consequence of the dimensionality rule and finally methods
;;; specific to a particular region type. Unbounded regions and region
;;; sets have their methods defined elsewhere.


(defmethod region-contains-position-p ((region point) x y)
  (multiple-value-bind (px py) (point-position region)
    (and (coordinate= px x)
         (coordinate= py y))))

(defmethod region-contains-position-p ((region polyline) x y)
  (setf x (coordinate x)
        y (coordinate y))
  (block nil
    (map-over-polygon-segments
     (lambda (x1 y1 x2 y2)
       (when (segment-contains-point-p x1 y1 x2 y2 x y)
         (return t)))
     region)
    nil))

(defmethod region-contains-position-p ((region line) x y)
  (multiple-value-bind (x1 y1) (line-start-point* region)
    (multiple-value-bind (x2 y2) (line-end-point* region)
      (segment-contains-point-p x1 y1 x2 y2 x y))))

(defmethod region-contains-position-p ((region elliptical-arc) x y)
  (flet ((position-contains-p (polar->screen)
           (multiple-value-bind (polar-x polar-y)
               (untransform-position polar->screen x y)
             ;; FIXME we don't need to factor the additional epsilon
             ;; but rotated elliptoids are naively rendered in clx.
             (multiple-value-bind (polar-dx polar-dy)
                 (untransform-distance polar->screen 1 1)
               (let ((point-radii (+ (square polar-dx) (square polar-dy))))
                 (coordinate-between* (- 1 point-radii)
                                      (+ (square polar-x) (square polar-y))
                                      (+ 1 point-radii)))))))
    (if-let ((alpha (ellipse-start-angle region)))
      (and (multiple-value-bind (cx cy) (ellipse-center-point* region)
             (arc-contains-point-p alpha (ellipse-end-angle region) (- x cx) (- y cy)))
           (position-contains-p (polar->screen region)))
      (position-contains-p (polar->screen region)))))

(defmethod region-contains-position-p ((region polygon) x y)
  (and (region-contains-position-p (bounding-rectangle region) x y)
       ;; The following algorithm is a Winding Number (wn) method implementation
       ;; based on a description by Dan Sunday "Inclusion of a Point in a
       ;; Polygon" (http://geomalgorithms.com/a03-_inclusion.html).
       (flet ((is-left (x0 y0 x1 y1 x2 y2)
                (- (* (- x1 x0) (- y2 y0))
                   (* (- x2 x0) (- y1 y0)))))
         (let ((x (coordinate x))
               (y (coordinate y))
               (wn 0))
           (map-over-polygon-segments
            (lambda (x1 y1 x2 y2)
              ;; Algorithm is not predictable for polygon edges - we
              ;; need to test for them explicitly. -- jd 2019-09-27
              (when (segment-contains-point-p x1 y1 x2 y2 x y)
                (return-from region-contains-position-p t))
              (if (<= y1 y)
                  (when (and (> y2 y)
                             (> (is-left x1 y1 x2 y2 x y) 0))
                    (incf wn))
                  (when (and (<= y2 y)
                             (< (is-left x1 y1 x2 y2 x y) 0))
                    (decf wn))))
            region)
           (not (zerop wn))))))

(defmethod region-contains-position-p ((region rectangle) x y)
  (multiple-value-bind (x1 y1 x2 y2)
      (rectangle-edges* region)
    (and (coordinate-between* x1 x x2)
         (coordinate-between* y1 y y2))))

(defmethod region-contains-position-p ((region standard-rectangle) x y)
  (with-standard-rectangle (x1 y1 x2 y2) region
    (and (coordinate-between* x1 x x2)
         (coordinate-between* y1 y y2))))

(defmethod region-contains-position-p ((region ellipse) x y)
  (flet ((position-contains-p (polar->screen)
           (multiple-value-bind (polar-x polar-y)
               (untransform-position polar->screen x y)
             ;; FIXME we don't need to factor the additonal epsilon
             ;; but rotated elliptoids are naively rendered in clx.
             (multiple-value-bind (polar-dx polar-dy)
                 (untransform-distance polar->screen 1 1)
               (coordinate<= (+ (square polar-x) (square polar-y))
                             (+ 1 (square polar-dx) (square polar-dy)))))))
    (if-let ((alpha (ellipse-start-angle region)))
      (and (multiple-value-bind (cx cy) (ellipse-center-point* region)
             (arc-contains-point-p alpha (ellipse-end-angle region) (- x cx) (- y cy)))
           (position-contains-p (polar->screen region)))
      (position-contains-p (polar->screen region)))))


;;   REGION-INTERSECTS-REGION-P region1 region2
;;
;;        Returns nil if region-intersection of the two regions region1 and
;;        region2 would be +nowhere+; otherwise, it returns t.
;;
;;        aka region1 and region2 are not disjoint, aka A ∩ B ≠ ∅
;;

;; "generic" version
(defmethod region-intersects-region-p ((a bounding-rectangle) (b bounding-rectangle))
  (not (region-equal +nowhere+ (region-intersection a b))))

(defmethod region-intersects-region-p :around ((a bounding-rectangle) (b bounding-rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* a)
    (multiple-value-bind (u1 v1 u2 v2) (bounding-rectangle* b)
      (cond ((and (<= u1 x2) (<= x1 u2)
                  (<= v1 y2) (<= y1 v2))
             (call-next-method))
            (t
             nil)))))

(defmethod region-intersects-region-p ((a standard-rectangle) (b standard-rectangle))
  (declare (ignorable a b))
  ;; for rectangles, the bounding rectangle test is correct, so if we
  ;; wind up here, we can just return T.
  t)


;;   REGION-CONTAINS-REGION-P region1 region2
;;
;;        Returns T if all points in the region REGION1 are members of
;;        the region REGION2; otherwise, it returns NIL.
;;
;;        aka region2 is a subset of region1, aka B\A = ∅
;;

;;; "generic" version
(defmethod region-contains-region-p ((a bounding-rectangle) (b bounding-rectangle))
  (or (eq a b)
      (region-equal +nowhere+ (region-difference b a))))

(defmethod region-contains-region-p ((a standard-ellipse) (b standard-ellipse))
  (multiple-value-bind (bcx bcy) (ellipse-center-point* b)
    (and (region-contains-position-p a bcx bcy)
         (null (intersection-ellipse/ellipse a b))
         (or (null (ellipse-start-angle a))
             (multiple-value-bind (sx sy) (%ellipse-angle->position a (ellipse-start-angle a))
               (multiple-value-bind (ex ey) (%ellipse-angle->position a (ellipse-end-angle a))
                 (multiple-value-bind (cx cy) (ellipse-center-point* a)
                   (and (null (region-intersection b (make-line* sx sy cx cy)))
                        (null (region-intersection b (make-line* ex ey cx cy)))))))))))

;;; Ellipse is a convex object. That Implies that if each of the rectangle
;;; vertexes lies inside it, then whole rectangle fits as well. We take a
;;; special care for ellipses with start/end angle.
(defmethod region-contains-region-p ((a standard-ellipse) (b standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2) b
    (if (null (ellipse-start-angle a))
        (and (region-contains-position-p a x1 y1)
             (region-contains-position-p a x2 y1)
             (region-contains-position-p a x1 y2)
             (region-contains-position-p a x2 y2))
        (flet ((fits (l) (region-equal l (region-intersection l a))))
          (and (fits (make-line* x1 y1 x2 y1))
               (fits (make-line* x2 y1 x2 y2))
               (fits (make-line* x2 y2 x1 y2))
               (fits (make-line* x1 y2 x1 y1)))))))

(defmethod region-contains-region-p ((a standard-ellipse) (polygon standard-polygon))
  (if (null (ellipse-start-angle a))
      (map-over-polygon-coordinates
       #'(lambda (x y)
           (unless (region-contains-position-p a x y)
             (return-from region-contains-region-p nil)))
       polygon)
      (map-over-polygon-segments
       #'(lambda (x1 y1 x2 y2
                  &aux (line (make-line* x1 y1 x2 y2)))
           (unless (region-equal line (region-intersection line a))
             (return-from region-contains-region-p nil)))
       polygon))
  T)

(defmethod region-contains-region-p ((a bounding-rectangle) (b point))
  (region-contains-position-p a (point-x b) (point-y b)))



;; "generic" version
(defmethod region-equal ((a bounding-rectangle) (b bounding-rectangle))
  (region-equal +nowhere+ (region-exclusive-or a b)))

;;; dimensionality rule
(defmethod region-equal ((a point) (b path))  nil)
(defmethod region-equal ((a point) (b area))  nil)
(defmethod region-equal ((a path)  (b point)) nil)
(defmethod region-equal ((a path)  (b area))  nil)
(defmethod region-equal ((a area)  (b point)) nil)
(defmethod region-equal ((a area)  (b path))  nil)

(defmethod region-equal ((a point) (b point))
  (and (coordinate= (point-x a) (point-x b))
       (coordinate= (point-y a) (point-y b))))

(defmethod region-equal ((xs standard-rectangle-set) (ys standard-rectangle-set))
  ;; Our bands representation is canonic
  (equal (standard-rectangle-set-bands xs)
         (standard-rectangle-set-bands ys)))

(defmethod region-equal ((a standard-rectangle) (b standard-rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* a)
    (multiple-value-bind (u1 v1 u2 v2) (rectangle-edges* b)
      (and (coordinate= x1 u1)
           (coordinate= y1 v1)
           (coordinate= x2 u2)
           (coordinate= y2 v2)))))

(defmethod region-equal ((a standard-rectangle) (b path)) nil)
(defmethod region-equal ((a path) (b standard-rectangle)) nil)

(defmethod region-equal ((a standard-line) (b standard-line))
  (or (and (region-equal (line-start-point a) (line-start-point b))
           (region-equal (line-end-point a) (line-end-point b)))
      (and (region-equal (line-start-point a) (line-end-point b))
           (region-equal (line-end-point a) (line-start-point b)))))

