(in-package #:clim-tests)

;;; TODO
;;;
;;; - add tests for the standard region sets (union, difference,
;;;   intersection and rectangle-set). Sets should be tested for
;;;   elements of different dimensions (points, paths and areas).
;;; - add tests for unbounded regions (should signal error).
;;; - add tests which verify correctness of a bounding rectangle.
;;; - add tests for rotated regions
;;; - randomize coordinates (translation and scaling affect the
;;;   bounding-rectangle in a predictable way)
;;; - test other instationable bounding-rectangle subclasses


(def-suite* :mcclim.bounding-rectangles
  :in :mcclim)

(defun has-valid-bounding-rectangle (region x1 y1 x2 y2)
  (is (bounding-rectangle-p (bounding-rectangle region)))
  (with-bounding-rectangle* (rx1 ry1 rx2 ry2) region
    (is (= x1 rx1))
    (is (= y1 ry1))
    (is (= x2 rx2))
    (is (= y2 ry2))))

(test bounding-rectangle.point
  (let ((point (make-point 0 0)))
    (has-valid-bounding-rectangle point 0 0 0 0)))

(test bounding-rectangle.polyline
  (let ((polyline1 (make-polyline* '(0 0 1 1 4 0)))
        (polyline2 (make-polyline* '(0 0 1 1 4 0) :closed t)))
    (has-valid-bounding-rectangle polyline1 0 0 4 1)
    (has-valid-bounding-rectangle polyline2 0 0 4 1)))

(test bounding-rectangle.line
  (let ((line (make-line* 0 0 1 1)))
    (has-valid-bounding-rectangle line 0 0 1 1)))

(test bounding-rectangle.elliptical-arc
  (let ((elliptical-arc1 (make-elliptical-arc* 0 0 10 0 0 15))
        (elliptical-arc2 (make-elliptical-arc* 0 0 10 0 0 15
                                               :start-angle 0
                                               :end-angle (/ pi 2))))
    (has-valid-bounding-rectangle elliptical-arc1 -10 -15 10 15)
    (has-valid-bounding-rectangle elliptical-arc1 0 -15 10 0)))

(test bounding-rectangle.polygon
  (let ((polygon (make-polygon* '(0 0 1 0 1 1 -1 1))))
    (has-valid-bounding-rectangle polygon -1 0 1 1)))

(test bounding-rectangle.rectangle
  (let ((rectangle (make-rectangle* 5 5 0 0)))
    (has-valid-bounding-rectangle rectangle 0 0 5 5)))

(test bounding-rectangle.ellipse
  (let ((ellipse1 (make-ellipse* 0 0 10 0 0 15))
        (ellipse2 (make-ellipse* 0 0 10 0 0 15
                                 :start-angle 0
                                 :end-angle (/ pi 2))))
    (has-valid-bounding-rectangle ellipse1 -10 -15 10 15)
    (has-valid-bounding-rectangle ellipse2 0 -15 10 0)))
