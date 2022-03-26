(cl:in-package #:clim-tests)

(def-suite* :mcclim.regions
  :in :mcclim)

(test regions.bounding-rectangles
  (is-false (subtypep 'bounding-rectangle 'region))
  (is (subtypep 'standard-bounding-rectangle 'bounding-rectangle))
  (is (subtypep 'standard-bounding-rectangle 'rectangle))

  (is-true (bounding-rectangle-p (make-point 23 99)))

  (let* ((x1 234) (y1 838) (x2 -234) (y2 22)
         (l (make-line* x1 y1 x2 y2))
         (br (make-bounding-rectangle x1 y1 x2 y2)))
    (is-true (bounding-rectangle-p l))
    (is (typep br 'standard-bounding-rectangle))
    (multiple-value-bind (xx1 yy1 xx2 yy2) (bounding-rectangle* l)
      (is (= (coordinate (min x1 x2)) xx1))
      (is (= (coordinate (min y1 y2)) yy1))
      (is (= (coordinate (max x1 x2)) xx2))
      (is (= (coordinate (max y1 y2)) yy2)))
    (multiple-value-bind (xx1 yy1 xx2 yy2) (bounding-rectangle* br)
      (is (= (coordinate (min x1 x2)) xx1))
      (is (= (coordinate (min y1 y2)) yy1))
      (is (= (coordinate (max x1 x2)) xx2))
      (is (= (coordinate (max y1 y2)) yy2)))
    (with-bounding-rectangle* (xx1 yy1 xx2 yy2) l
      (is (= (coordinate (min x1 x2)) xx1))
      (is (= (coordinate (min y1 y2)) yy1))
      (is (= (coordinate (max x1 x2)) xx2))
      (is (= (coordinate (max y1 y2)) yy2)))
    (with-bounding-rectangle* (xx1 yy1 xx2 yy2) br
      (is (= (coordinate (min x1 x2)) xx1))
      (is (= (coordinate (min y1 y2)) yy1))
      (is (= (coordinate (max x1 x2)) xx2))
      (is (= (coordinate (max y1 y2)) yy2)))
    ;; test failed.  b-r-p returns two values instead of a point
    ;;  (is (region-equal (bounding-rectangle-position l)
    ;;                    (make-point (min x1 x2) (min y1 y2))))
    (is (= (bounding-rectangle-min-x l) (coordinate (min x1 x2))))
    (is (= (bounding-rectangle-min-y l) (coordinate (min y1 y2))))
    (is (= (bounding-rectangle-max-x l) (coordinate (max x1 x2))))
    (is (= (bounding-rectangle-max-y l) (coordinate (max y1 y2))))
    (with-bounding-rectangle* (xx1 yy1 xx2 yy2) l
      (is (= (bounding-rectangle-width l) (- xx2 xx1)))
      (is (= (bounding-rectangle-height l) (- yy2 yy1))))
    (multiple-value-bind (w h) (bounding-rectangle-size l)
      (is (= (bounding-rectangle-width l) w))
      (is (= (bounding-rectangle-height l) h)))))


;;; Regions
;;; ============================================================================

(test regions.region
  (is (subtypep 'region 'design)))

(test regions.path
  (is (subtypep 'path 'region))
  (is (subtypep 'path 'bounding-rectangle)))

(test regions.area
  (is (subtypep 'area 'region))
  (is (subtypep 'area 'bounding-rectangle)))

(test regions.coordinate
  (is-true (or (and (subtypep 'coordinate t)
                    (subtypep t 'coordinate))
               (subtypep 'coordinate 'real))))

(test regions.point
  (is (subtypep 'standard-point 'point))

  (let ((p (make-point 1/2 55/33)))
    (is (typep p 'standard-point))
    (is-true (pointp p))
    (is-false (pathp p))
    (is-false (areap p))
    (is (typep p 'region))
    (is-true (regionp p))
    (multiple-value-bind (x y) (point-position p)
      (is (= (point-x p) x))
      (is (= (point-y p) y))
      (is (typep x 'coordinate))
      (is (typep y 'coordinate)))))

(test regions.+everywhere+-and-+nowhere+
  (is-false (region-equal +everywhere+ +nowhere+))

  (is-true (region-contains-region-p +everywhere+ +nowhere+))
  (is-false (region-contains-region-p +nowhere+ +everywhere+))
  (is-false (region-intersects-region-p +nowhere+ +everywhere+))
  (is-true (region-contains-position-p +everywhere+ 10 -10))
  (is-false (region-contains-position-p +nowhere+ -10 10)))

(test regions.region-set
  (is (subtypep 'region-set 'region))
  (is (subtypep 'region-set 'bounding-rectangle))
  (is (subtypep 'standard-region-union 'region-set))
  (is (subtypep 'standard-region-intersection 'region-set))
  (is (subtypep 'standard-region-difference 'region-set)))

;;; union of two different points
(test regions.union
  (let* ((p1 (make-point 10 -10))
         (p2 (make-point -10 10))
         (u (region-union p1 p2))
         (regions (region-set-regions u)))
    (is (typep u 'standard-region-union))
    (is (= 2 (length regions)))
    (is (member p1 regions :test #'region-equal))
    (is (member p2 regions :test #'region-equal))))

;;; intersection of two different points
(test regions.intersection
  (let* ((p1 (make-point 10 -10))
         (p2 (make-point -10 10))
         (i (region-intersection p1 p2)))
    (is (region-equal +nowhere+ i))))

;;; difference of two different points
(test regions.difference
  (let* ((p1 (make-point 10 -10))
         (p2 (make-point -10 10))
         (d (region-difference p1 p2))
         (regions (region-set-regions d)))
    (is-true (or (typep d 'standard-region-difference)
                 (pointp d)))
    (is (member (length regions) '(1 2)))
    (is (member p1 regions :test #'region-equal))
    (let ((regions2 '()))
      (map-over-region-set-regions
       (lambda (region) (push region regions2))
       d)
      (is-true (null (set-difference regions regions2 :test #'region-equal)))
      (is-true (null (set-difference regions2 regions :test #'region-equal))))))

(test regions.standard-rectangle-set
  "STANDARD-RECTANGLE-SET and containment calculation."
  (let* ((r1 (make-rectangle* 0 0 1 1))
         (r2 (make-rectangle* 2 0 3 1))
         (ru (region-union r1 r2)))
    (is-false (region-contains-position-p ru -1/2 1/2))
    (is-true (region-contains-position-p ru 1/2 1/2))
    (is-false(region-contains-position-p ru 3/2 1/2))
    (is-true (region-contains-position-p ru 5/2 1/2))
    (is-false (region-contains-position-p ru 7/2 1/2))
    (is-false (region-contains-position-p ru 1/2 3/2))
    (is-false (region-contains-position-p ru 5/2 -1/2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; polyline

(test regions.polyline
  (is (subtypep 'polyline 'path))
  (is (subtypep 'standard-polyline 'polyline))

  (let* ((x1 10) (y1 22) (x2 30) (y2 30) (x3 50) (y3 5)
         (p1 (make-point x1 y1)) (p2 (make-point x2 y2)) (p3 (make-point x3 y3))
         (pl1 (make-polyline (list p1 p2 p3)))
         (pl2 (make-polyline* (list x1 y1 x2 y2 x3 y3)))
         (pl3 (make-polyline (list p1 p2 p3) :closed t))
         (pl4 (make-polyline* (list x1 y1 x2 y2 x3 y3) :closed t))
         (points '()))
    (is (typep pl1 'standard-polyline))
    (is-true (polylinep pl1))
    (is (typep pl2 'standard-polyline))
    (is-true (polylinep pl2))
    (is (region-equal pl1 pl2))
    (is (typep pl3 'standard-polyline))
    (is-true (polylinep pl3))
    (is (typep pl4 'standard-polyline))
    (is-true (polylinep pl4))
    (is (region-equal pl3 pl4))
    (is-true (null (set-difference (polygon-points pl1) (list p1 p2 p3) :test #'region-equal)))
    (is-true (null (set-difference (list p1 p2 p3) (polygon-points pl1) :test #'region-equal)))
    (is-true (null (set-difference (polygon-points pl2) (list p1 p2 p3) :test #'region-equal)))
    (is-true (null (set-difference (list p1 p2 p3) (polygon-points pl2) :test #'region-equal)))
    (is-true (null (set-difference (polygon-points pl3) (list p1 p2 p3) :test #'region-equal)))
    (is-true (null (set-difference (list p1 p2 p3) (polygon-points pl3) :test #'region-equal)))
    (is-true (null (set-difference (polygon-points pl4) (list p1 p2 p3) :test #'region-equal)))
    (is-true (null (set-difference (list p1 p2 p3) (polygon-points pl4) :test #'region-equal)))
    (map-over-polygon-coordinates
     (lambda (x y)
       (push (make-point x y) points))
     pl1)
    (is-true (null (set-difference (list p1 p2 p3) points :test #'region-equal)))
    (is-true (null (set-difference points (list p1 p2 p3) :test #'region-equal)))
    (is-true (polyline-closed pl3))
    (is-false (polyline-closed pl2))))

(test regions.polygon
  (is (subtypep 'polygon 'area))
  (is (subtypep 'standard-polygon 'polygon))

  (let* ((x1 10) (y1 22) (x2 30) (y2 30) (x3 50) (y3 5)
         (p1 (make-point x1 y1)) (p2 (make-point x2 y2)) (p3 (make-point x3 y3))
         (pg1 (make-polygon (list p1 p2 p3)))
         (pg2 (make-polygon* (list x1 y1 x2 y2 x3 y3)))
         (points '()))
    (is (typep pg1 'standard-polygon))
    (is-true (polygonp pg1))
    (is (typep pg2 'standard-polygon))
    (is-true (polygonp pg2))
    (is (region-equal pg1 pg2))
    (is-true (null (set-difference (polygon-points pg1) (list p1 p2 p3) :test #'region-equal)))
    (is-true (null (set-difference (list p1 p2 p3) (polygon-points pg1) :test #'region-equal)))
    (is-true (null (set-difference (polygon-points pg2) (list p1 p2 p3) :test #'region-equal)))
    (is-true (null (set-difference (list p1 p2 p3) (polygon-points pg2) :test #'region-equal)))
    (map-over-polygon-coordinates
     (lambda (x y)
       (push (make-point x y) points))
     pg1)
    (is-true (null (set-difference (list p1 p2 p3) points :test #'region-equal)))
    (is-true (null (set-difference points (list p1 p2 p3) :test #'region-equal)))))

(test regions.line
  (is (subtypep 'line 'polyline))
  (is (subtypep 'standard-line 'line))

  (let* ((x1 234) (y1 876) (x2 345) (y2 -55)
         (p1 (make-point x1 y1)) (p2 (make-point x2 y2))
         (l1 (make-line p1 p2)) (l2 (make-line* x1 y1 x2 y2)))
    (is (typep l1 'standard-line))
    (is-true (linep l1))
    (is (region-equal l1 l2))
    (multiple-value-bind (xx1 yy1) (line-start-point* l1)
      (is (= (coordinate x1) xx1))
      (is (= (coordinate y1) yy1)))
    (multiple-value-bind (xx2 yy2) (line-end-point* l1)
      (is (= (coordinate x2) xx2))
      (is (= (coordinate y2)yy2)))
    (is (region-equal p1 (line-start-point l1)))
    (is (region-equal p2 (line-end-point l1)))))

(test regions.rectangle
  (is (subtypep 'rectangle 'polygon))
  (is (subtypep 'standard-rectangle 'rectangle))

  (let* ((x1 234) (y1 876) (x2 345) (y2 -55)
         (p1 (make-point x1 y1)) (p2 (make-point x2 y2))
         (r1 (make-rectangle p1 p2)) (r2 (make-rectangle* x1 y1 x2 y2)))
    (is (typep r1 'standard-rectangle))
    (is-true (rectanglep r1))
    (is (region-equal r1 r2))
    (multiple-value-bind (min-x min-y max-x max-y) (rectangle-edges* r1)
      (is (= (rectangle-min-x r1) min-x))
      (is (= (rectangle-min-y r1) min-y))
      (is (= (rectangle-max-x r1) max-x))
      (is (= (rectangle-max-y r1) max-y))
      (is (= (coordinate x1) min-x))
      (is (= (coordinate y1) max-y))
      (is (= (coordinate x2) max-x))
      (is (= (coordinate y2) min-y))
      (multiple-value-bind (width height) (rectangle-size r1)
        (is (= width (rectangle-width r1)))
        (is (= height (rectangle-height r1)))
        (is (= width (- max-x min-x)))
        (is (= height (- max-y min-y)))))
    (is (region-equal (make-point x1 y2) (rectangle-min-point r1)))
    (is (region-equal (make-point x2 y1) (rectangle-max-point r1)))))

(test regions.ellipse
  (is (subtypep 'ellipse 'area))
  (is (subtypep 'standard-ellipse 'ellipse))

  (let* ((xc 234) (yc 345) (xdr1 -858) (ydr1 44) (xdr2 -55) (ydr2 5)
         (sa 10) (ea 270)
         (pc (make-point xc yc))
         (e1 (make-ellipse* xc yc xdr1 ydr1 xdr2 ydr2 :start-angle sa :end-angle ea))
         (e2 (make-ellipse pc xdr1 ydr1 xdr2 ydr2 :start-angle sa :end-angle ea))
         (e3 (make-ellipse pc xdr1 ydr1 xdr2 ydr2)))
    (is (typep e1 'standard-ellipse))
    (is-true (ellipsep e1))
    ;; this test fails
    ;;  (is (region-equal e1 e2))
    (multiple-value-bind (x y) (ellipse-center-point* e1)
      (is (= (coordinate xc) x))
      (is (= (coordinate yc) y))
      (is (region-equal (make-point x y) (ellipse-center-point e2))))
    (multiple-value-bind (xr11 yr11 xr12 yr12) (ellipse-radii e1)
      (multiple-value-bind (xr21 yr21 xr22 yr22) (ellipse-radii e2)
        (is (= xr11 xr21))
        (is (= yr11 yr21))
        (is (= xr12 xr22))
        (is (= yr12 yr22))))
    (is (= (coordinate sa) (coordinate (ellipse-start-angle e1))))
    (is (= (coordinate ea) (coordinate (ellipse-end-angle e1))))
    (is-true (null (ellipse-start-angle e3)))
    (is-true (null (ellipse-end-angle e3))))

  (let* ((xc 200) (yc 200) (xdr1 100) (ydr1 0) (xdr2 0) (ydr2 50) (sa 0) (ea (/ pi 2))
         (el0 (make-ellipse* xc yc xdr1 ydr1 xdr2 ydr2))
         (el1 (make-ellipse* xc yc xdr1 ydr1 xdr2 ydr2 :start-angle sa :end-angle ea))
         (el2 (make-ellipse* xc yc xdr2 ydr2 xdr1 ydr1 :start-angle sa :end-angle ea))
         (el3 (make-ellipse* xc yc (- xdr1) (- ydr1) (- xdr2) (- ydr2) :start-angle sa :end-angle ea))
         (el4 (make-ellipse* xc yc (- xdr2) (- ydr2) (- xdr1) (- ydr1) :start-angle sa :end-angle ea))
         (some-el (list el1 el2 el3 el4))
         (all-el (list* el0 some-el)))
    (declare (ignorable el0 el1 el2 el3 el4))
    (macrolet ((rcpt (xc yc elts)
                 `(mapcar (lambda (el)
                            (is-true (region-contains-position-p el ,xc ,yc)))
                          ,elts))
               (rcpn (xc yc elts)
                 `(mapcar (lambda (el)
                            (is-false (region-contains-position-p el ,xc ,yc)))
                          ,elts)))
      ;; trivial cases which may be judged based on distance from the center
      (rcpt xc yc             all-el)
      (rcpt (+ xc 100) yc     all-el)
      ;; FIXME this test may fail because we add an additional epsilon
      ;; for sake of CLX rendering of rotated ellipses. -- jd 2019-11-19
      (rcpn (+ xc 1) (- yc 50) all-el)
      (rcpn (+ xc 3) (- yc 50) all-el)
      ;; less trivial cases (we accept only 1st quadrent in el1-el4)
      ;; point between 4th and 1st quadrent (on the start-angle)
      (rcpt (+ xc 10) yc all-el)
      ;; point lies in 1st quadrent
      (rcpt (+ xc 10) (- yc 10) all-el)
      ;; point between 1st and 2nd quadrent (on the end-angle)
      (rcpt xc (- yc 50) all-el)
      ;; point lies in 2nd quadrent
      (rcpt (- xc 10) (- yc 10) (list el0))
      (rcpn (- xc 10) (- yc 10) some-el)
      ;; point between 2nd and 3rd quadrent
      (rcpt (- xc 10) yc (list el0))
      (rcpn (- xc 10) yc some-el)
      ;; point lies in 3rd quadrent
      (rcpt (- xc 10) (+ yc 10) (list el0))
      (rcpn (- xc 10) (+ yc 10) some-el)
      ;; point between 3rd and 4th quadrent
      (rcpt xc (+ yc 10) (list el0))
      (rcpn xc (+ yc 10) some-el)
      ;; point lies in 4th quadrent
      (rcpt (+ xc 10) (+ yc 10) (list el0))
      (rcpn (+ xc 10) (+ yc 10) some-el)))

  ;; non xy-aligned ellipses (one ellipse put in various coordinates)
  (let* ((el1 (make-ellipse* 200 200 +100 -100 -10 -10 :start-angle (* 3 (/ pi 2)) :end-angle pi))
         (el2 (make-ellipse* 200 200 +100 -100 +10 +10 :start-angle (* 3 (/ pi 2)) :end-angle pi))
         (el3 (make-ellipse* 200 200 -100 +100 -10 -10 :start-angle (* 3 (/ pi 2)) :end-angle pi))
         (el4 (make-ellipse* 200 200 -100 +100 +10 +10 :start-angle (* 3 (/ pi 2)) :end-angle pi))
         (el5 (make-ellipse* 200 200 -10 -10 +100 -100 :start-angle (* 3 (/ pi 2)) :end-angle pi))
         (el6 (make-ellipse* 200 200 +10 +10 +100 -100 :start-angle (* 3 (/ pi 2)) :end-angle pi))
         (el7 (make-ellipse* 200 200 -10 -10 -100 +100 :start-angle (* 3 (/ pi 2)) :end-angle pi))
         (el8 (make-ellipse* 200 200 +10 +10 -100 +100 :start-angle (* 3 (/ pi 2)) :end-angle pi))
         (all-ellipses (list el1 el2 el3 el4 el5 el6 el7 el8)))
    (mapcar (lambda (el)
              ;; tips of the ellipse
              (is-true (region-contains-position-p el 300 100))
              (is-true (region-contains-position-p el 190 190))
              (is-true (region-contains-position-p el 210 210))
              (is-false (region-contains-position-p el 100 300)) ; outside the angle
              ;; points outside the ellipse
              ;;
              ;; FIXME this test may fail because we add an additional epsilon
              ;; for sake of CLX rendering of rotated ellipses. -- jd 2019-11-19
              (is-false (region-contains-position-p el 301 101)) ; too far away
              (is-false (region-contains-position-p el 303 103)) ; too far away
              (is-false (region-contains-position-p el 200 100)) ; y-aligned tip
              (is-false (region-contains-position-p el 300 200)) ; x-aligned tip
              )
            all-ellipses)))

(test regions.elliptical-arc
  (is (subtypep 'elliptical-arc 'path))
  (is (subtypep 'standard-elliptical-arc 'elliptical-arc))

  (let* ((xc 234) (yc 345) (xdr1 -858) (ydr1 44) (xdr2 -55) (ydr2 5)
         (sa 10) (ea 270)
         (pc (make-point xc yc))
         (ea1 (make-elliptical-arc* xc yc xdr1 ydr1 xdr2 ydr2 :start-angle sa :end-angle ea))
         (ea2 (make-elliptical-arc pc xdr1 ydr1 xdr2 ydr2 :start-angle sa :end-angle ea))
         (ea3 (make-elliptical-arc pc xdr1 ydr1 xdr2 ydr2)))
    (is (typep ea1 'standard-elliptical-arc))
    (is-true (elliptical-arc-p ea1))
    ;; this test fails
    ;;  (is (region-equal ea1 ea2))
    (multiple-value-bind (x y) (ellipse-center-point* ea1)
      (is (= (coordinate xc) x))
      (is (= (coordinate yc) y))
      (is (region-equal (make-point x y) (ellipse-center-point ea2))))
    (multiple-value-bind (xr11 yr11 xr12 yr12) (ellipse-radii ea1)
      (multiple-value-bind (xr21 yr21 xr22 yr22) (ellipse-radii ea2)
        (is (= xr11 xr21))
        (is (= yr11 yr21))
        (is (= xr12 xr22))
        (is (= yr12 yr22))))
    (is (= (coordinate sa) (coordinate (ellipse-start-angle ea1))))
    (is (= (coordinate ea) (coordinate (ellipse-end-angle ea1))))
    (is-true (null (ellipse-start-angle ea3)))
    (is-true (null (ellipse-end-angle ea3)))))
