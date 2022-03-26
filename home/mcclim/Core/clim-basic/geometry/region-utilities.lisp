(in-package #:climi)


;;; point and angle utilities

;;; CLIM "native" coordinate system is left-handed (y grows down).
;;; Angles are specified to grow in the counter-clockwise direction
;;; (disregarding the coordinate system), so we need to invert the y
;;; coordinate in a call to atan.

(declaim (inline arc-contains-angle-p))
(defun arc-contains-angle-p (start-angle end-angle delta)
  (if (< start-angle end-angle)
      (coordinate-between* start-angle delta end-angle)
      (or (coordinate<= start-angle delta)
          (coordinate<= delta end-angle))))

(declaim (inline arc-contains-point-p))
(defun arc-contains-point-p (start-angle end-angle x y)
  (when (= 0 x y)
    (return-from arc-contains-point-p t))
  (arc-contains-angle-p start-angle end-angle (atan* x (- y))))

(defun colinear-p (p1 p2 p3)
  (multiple-value-bind (x1 y1) (point-position p1)
    (multiple-value-bind (x2 y2) (point-position p2)
      (multiple-value-bind (x3 y3) (point-position p3)
        (coordinate= (* (- x2 x1) (- y3 y2))
                     (* (- x3 x2) (- y2 y1)))))))

(defun square (x)
  "Returns the number X squared."
  (* x x))

(defun normalize-angle (angle)
  "Takes an angle ANGLE and returns the corresponding non-negative angle
less than or equal to 2pi. Note that 4pi would be normalized to 0, not
2pi, but 2pi is normalized to 2pi."
  (if (or (< angle 0)
          (> angle (* pi 2)))
      (mod angle (* pi 2))
      angle))

(defun find-angle* (x1 y1 x2 y2)
  "Returns the angle between two vectors described by x1, y1 and x2,
y2."
  (let ((theta (- (phase (complex y2 x2))
                  (phase (complex y1 x1)))))
    (normalize-angle theta)))


;;; line utilities
(declaim (inline line-contains-point-p))
(defun line-contains-point-p (x1 y1 x2 y2 px py)
  (coordinate= (* (- py y1) (- x2 x1))
               (* (- px x1) (- y2 y1))))

(declaim (inline segment-contains-point-p))
(defun segment-contains-point-p (x1 y1 x2 y2 x y)
  (and (coordinate-between x1 x x2)
       (coordinate-between y1 y y2)
       (line-contains-point-p x1 y1 x2 y2 x y)))

(declaim (inline segment-difference))
(defun segment-difference (ax ay bx by
                           cx cy dx dy)
  ;; Return either:
  ;; T   - no change
  ;; NIL - nowhere
  ;; list of coordinates
  (unless (and (line-contains-point-p ax ay bx by cx cy)
               (line-contains-point-p ax ay bx by dx dy))
    ;; Segments are not colinear.
    (return-from segment-difference t))
  (macrolet ((sort-points (x1 y1 x2 y2)
               `(when (or (> ,x1 ,x2)
                          (and (= ,x1 ,x2)
                               (> ,y1 ,y2)))
                  (rotatef ,x1 ,x2)
                  (rotatef ,y1 ,y2))))
    (sort-points ax ay bx by)
    (sort-points cx cy dx dy))
  ;; We already know that lines are colienar, no need to use
  ;; segment-contains-point-p (it is enough to compare a single
  ;; coordinate).
  (let ((a-in-cd (coordinate-between* cx ax dx))
        (b-in-cd (coordinate-between* cx bx dx))
        (c-in-ab (coordinate-between* ax cx bx))
        (d-in-ab (coordinate-between* ax dx bx)))
    (cond
      ;; Segment AB is part of the segment CD.
      ((and a-in-cd b-in-cd) nil)
      ;; a-in-cd and (not b-in-cd) implies d-in-ab
      (a-in-cd               (list (list dx dy bx by)))
      ;; b-in-cd and (not a-in-cd) implies c-in-ab
      (b-in-cd               (list (list ax ay cx cy)))
      ;; Segment CD is part of the segment AB
      ((and c-in-ab d-in-ab) (list (list ax ay cx cy)
                                   (list dx dy bx by)))
      ;; Segments do not overlap.
      (t t))))

(defun line-intersection* (x1 y1 x2 y2 u1 v1 u2 v2)
  (let ((dx (- x2 x1)) (dy (- y2 y1))
        (du (- u2 u1)) (dv (- v2 v1)))
    (let ((q (- (* dx dv) (* du dy))))
      (cond ((not (and (<= (min x1 x2) (max u1 u2)) (<= (min u1 u2) (max x1 x2))
                       (<= (min y1 y2) (max v1 v2)) (<= (min v1 v2) (max y1 y2))))
             nil)
            ((coordinate= 0 q)
             (cond ((coordinate= (* (- v1 y1) dx) (* (- u1 x1) dy))
                    ;; koninzident
                    (cond ((> (abs dx) (abs dy))
                           (let* ((sx1 (max (min x1 x2) (min u1 u2)))
                                  (sx2 (min (max x1 x2) (max u1 u2)))
                                  (sy1 (+ (* (- sx1 x1) (/ dy dx)) x1))
                                  (sy2 (+ (* (- sx2 x1) (/ dy dx)) x1)))
                             (values :coincident sx1 sy1 sx2 sy2)))
                          (t
                           (let* ((sy1 (max (min y1 y2) (min v1 v2)))
                                  (sy2 (min (max y1 y2) (max v1 v2)))
                                  (sx1 (+ (* (- sy1 y1) (/ dx dy)) y1))
                                  (sx2 (+ (* (- sy2 y1) (/ dx dy)) y1)))
                             (values :coincident sx1 sy1 sx2 sy2)))))
                   (t
                    ;;paralell -- kein Schnitt
                    nil)))
            ((or (<= (abs dx) single-float-epsilon) (<= (abs du) single-float-epsilon))
             ;; infinite slope (vertical line) - previous case covers two vlines
             (let (a b x y)
               (if (zerop dx)           ; ugly setf - I'm ashamed
                   (setf a (/ dv du)
                         b (- v1 (* a u1))
                         x x1
                         y (+ (* a x1) b))
                   (setf a (/ dy dx)
                         b (- y1 (* a x1))
                         x u1
                         y (+ (* a x) b)))
               (if (and (or (<= y1 y y2) (<= y2 y y1))
                        (or (<= v1 y v2) (<= v2 y v1)))
                   (values :hit x y)
                   nil)))
            (t
             (let ((x (/ (+ (* dx (- (* u1 dv) (* v1 du)))
                            (* du (- (* y1 dx) (* x1 dy))))
                         q))
                   (y (/ (+ (* dy (- (* u1 dv) (* v1 du)))
                            (* dv (- (* y1 dx) (* x1 dy))))
                         q)))
               (if (and (or (<= x1 x x2) (<= x2 x x1))
                        (or (<= u1 x u2) (<= u2 x u1)))
                   (values :hit x y)
                   nil)))))))

(defun line-intersection** (x1 y1 x2 y2 u1 v1 u2 v2)
  (let ((dx (- x2 x1)) (dy (- y2 y1))
        (du (- u2 u1)) (dv (- v2 v1)))
    (let ((q (- (* dx dv) (* du dy))))
      (cond ((coordinate= 0 q)
             nil)
            (t
             (let ((x (/ (+ (* dx (- (* u1 dv) (* v1 du)))
                            (* du (- (* y1 dx) (* x1 dy))))
                         q))
                   (y (/ (+ (* dy (- (* u1 dv) (* v1 du)))
                            (* dv (- (* y1 dx) (* x1 dy))))
                         q)))
               (values x y)))))))

(defun line-equation (x0 y0 x1 y1 px py)
  ;; ??? This somehow tries to calculate the distance between a point
  ;; and a line. The sign of the result depends upon the side the point
  ;; is on wrt to the line. --GB
  (- (* (- py y0) (- x1 x0))
     (* (- px x0) (- y1 y0))))

(defun position->line-fktn (x0 y0 x1 y1 px py)
  ;; This function assumes that (px py) lies on the same line as the
  ;; segment (x0 y0 x1 y1). Returned value is a scalar which denotes a
  ;; position of the point on the segment where 0d0 is (x0 y0) and 1d0
  ;; is (x1 y1). 0.5d0 is in a middle of the segment and not in the
  ;; interval (0d0 1d0) don't belong to the segment. -- jd 2019-10-10
  (let ((dx (- x1 x0)) (dy (- y1 y0)))
    (if (> (abs dx) (abs dy))
        (/ (- px x0) dx)
        (/ (- py y0) dy))))


;;; polygon
;;; -- Set operations on polygons --------------------------------------------

(defstruct (pg-edge (:constructor make-pg-edge* (x1 y1 x2 y2 extra)))
  x1 y1 x2 y2 extra)

(defstruct pg-splitter
  links                                 ; "links" means "left"
                                        ;list of points
  rechts)                               ; "rechts" means "right"
                                        ; from the top down

(defun make-pg-edge (p1 p2 extra)
  (multiple-value-bind (x1 y1) (point-position p1)
    (multiple-value-bind (x2 y2) (point-position p2)
      (make-pg-edge* x1 y1 x2 y2 extra))))

(defun polygon-op (pg1 pg2 &optional logop)
  (let ((sps nil))
    (over-sweep-bands pg1 pg2
                      (lambda (sy0 sy1 S &aux (ys nil))
                        (setq ys (list sy0 sy1))
                        (dolist (k1 S)
                          (dolist (k2 S)
                            (multiple-value-bind (px py)
                                (line-intersection**
                                 (pg-edge-x1 k1) (pg-edge-y1 k1)
                                 (pg-edge-x2 k1) (pg-edge-y2 k1)
                                 (pg-edge-x1 k2) (pg-edge-y1 k2)
                                 (pg-edge-x2 k2) (pg-edge-y2 k2))
                              (when (and px (< sy0 py sy1))
                                (pushnew py ys :test #'coordinate=)))))
                        (setq ys (sort ys #'<))
                        (do ((q ys (cdr q)))
                            ((null (cdr q)))
                          (let ((by0 (car q)) (by1 (cadr q))
                                (R nil))
                            (dolist (k S)
                              (when (> (pg-edge-y2 k) (pg-edge-y1 k))
                                (multiple-value-bind (x1 y1 x2 y2)
                                    (restrict-line-on-y-interval*
                                     (pg-edge-x1 k) (pg-edge-y1 k)
                                     (pg-edge-x2 k) (pg-edge-y2 k)
                                     by0 by1)
                                  (declare (ignore y1 y2))
                                  (push (list x1 x2 (pg-edge-extra k)) R))))
                            (setq R (sort R #'<
                                          :key (lambda (x) (+ (first x) (second x)))))
                            (labels
                                ((add (lo lu ro ru)
                                   (dolist (s sps
                                             ;; otherwise
                                             (push (make-pg-splitter
                                                    :links  (list lu lo)
                                                    :rechts (list ru ro))
                                                   sps))
                                     (when (and (region-equal
                                                 lo (car (pg-splitter-links s)))
                                                (region-equal
                                                 ro (car (pg-splitter-rechts s))))
                                       (push lu (pg-splitter-links s))
                                       (push ru (pg-splitter-rechts s))
                                       (return)))))
                              (let ((eintritt nil)
                                    (ina 0)
                                    (inb 0))
                                (dolist (k R)
                                  (ecase (third k)
                                    (:a (setq ina (- 1 ina)))
                                    (:b (setq inb (- 1 inb))))
                                  (cond ((/= 0 (funcall logop ina inb))
                                         (when (null eintritt)
                                           (setq eintritt k)))
                                        (t
                                         (when eintritt
                                           (add (make-point (first eintritt) by0)
                                                (make-point (second eintritt) by1)
                                                (make-point (first k) by0)
                                                (make-point (second k) by1))
                                           (setq eintritt nil)))))))))))
    (setq sps (delete +nowhere+ (mapcar #'pg-splitter->polygon sps)))
    (cond ((null sps) +nowhere+)
          ((null (cdr sps))
           (car sps))
          ((make-instance 'standard-region-union :regions sps)))))

(defun over-sweep-bands (pg1 pg2 fun)
  (let ((es (nconc (polygon->pg-edges pg1 :a) (polygon->pg-edges pg2 :b))))
    (setq es (sort es #'< :key #'pg-edge-y1))
    (let ((ep es)
          (sy (pg-edge-y1 (car es)))
          (S nil))
      (do () ((null ep))
        (setq S (delete-if (lambda (e)
                             (<= (pg-edge-y2 e) sy))
                           S))

        (do () ((or (null ep) (/= sy (pg-edge-y1 (car ep)))))
          (push (pop ep) S))

        (let ((sy2 (or (and ep (pg-edge-y1 (car ep)))
                       (reduce #'max (mapcar #'pg-edge-y2 S)))))

          (funcall fun sy sy2 S)
          (setq sy sy2))))))

(defun polygon->pg-edges (pg extra)
  (let ((pts (polygon-points pg))
        (res nil))
    (let ((prev pts)
          (cur (cdr pts))
          (next (cddr pts)))
      (loop
        (nest
         (multiple-value-bind (cur-x cur-y)   (point-position (car cur)))
         (multiple-value-bind (next-x next-y) (point-position (car next)))
         (multiple-value-bind (prev-x prev-y) (point-position (car prev))
           (when (or (> next-y cur-y)
                     (and (= next-y cur-y)
                          (> next-x cur-x)))
             (push (make-pg-edge (car cur) (car next) extra) res))
           (when (or (> prev-y cur-y)
                     (and (= prev-y cur-y)
                          (> prev-x cur-x)))
             (push (make-pg-edge (car cur) (car prev) extra) res))
           (when (not (or (> next-y cur-y)
                          (and (= next-y cur-y)
                               (> next-x cur-x))))
             (push (make-pg-edge (car cur) (car cur) extra) res))))
        (psetq prev cur
               cur next
               next (or (cdr next) pts))
        (when (eq prev pts)
          (return))))
    res))

(defun restrict-line-on-y-interval* (x1 y1 x2 y2 ry0 ry1)
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (values (+ (* (- ry0 y1) (/ dx dy)) x1) ry0
            (+ (* (- ry1 y1) (/ dx dy)) x1) ry1)))

(defun pg-splitter->polygon (s)
  (make-polygon (clean-up-point-sequence (nconc (pg-splitter-links s) (reverse (pg-splitter-rechts s))))))

(defun clean-up-point-sequence (pts)
  (cond ((null (cdr pts)) pts)
        ((region-equal (car pts) (cadr pts))
         (clean-up-point-sequence (cdr pts)))
        ((null (cddr pts)) pts)
        ((colinear-p (car pts) (cadr pts) (caddr pts))
         (clean-up-point-sequence (list* (car pts) (caddr pts) (cdddr pts))))
        (t
         (cons (car pts) (clean-up-point-sequence (cdr pts))))))

;;; -- Intersection Line/Polygon ---------------------------------------------

;;; By "overcut" we mean a scalar computed from the intersection point
;;; between a polygon segment and an unbounded line.
(defun map-over-overcuts-line/polygon (fun x1 y1 x2 y2 points
                                       &aux
                                         (n (length points))
                                         (fun (alexandria:ensure-function fun)))
  ;; FUN is called for some intersection points between a line going
  ;; through the segment (x1 y1 x2 y2) and the polygon. Each of these
  ;; points may be potentially a vertex of a segment which is the
  ;; intersection of the polygon and the line. Function argument is a
  ;; parameter indicating where on the segment S the intersection
  ;; point is positioned (where 0d0 is [x1,y1] and 1d0 is [x2,y2]). If
  ;; parameter is not (<= 0d0 param 1d0) position falls outside the
  ;; segment (but is still on the line). -- jd 2019-10-10
  (flet ((call-fun (point)
           (multiple-value-bind (px py)
               (point-position point)
             (funcall fun (position->line-fktn x1 y1 x2 y2 px py)))))
    (declare (inline call-fun))
    (dotimes (i n)
      (let ((pv  (elt points (mod (- i 1) n)))  ;the point before
            (po  (elt points (mod i n)))        ;the "current" point
            (pn  (elt points (mod (+ i 1) n)))  ;the point after
            (pnn (elt points (mod (+ i 2) n)))) ;the point after**2
        (cond
          ;; The line goes directly through PO
          ((multiple-value-bind (px py) (point-position po)
             (line-contains-point-p x1 y1 x2 y2 px py))
           (multiple-value-bind (pnx pny) (point-position pn)
             (multiple-value-bind (pvx pvy) (point-position pv)
               (let ((sign-1 (line-equation x1 y1 x2 y2 pnx pny))
                     (sign-2 (line-equation x1 y1 x2 y2 pvx pvy)))
                 (cond ((or (and (> sign-1 0) (< sign-2 0))
                            (and (< sign-1 0) (> sign-2 0)))
                        ;; clear cases: the line croses the polygon's border
                        (call-fun po))
                       ((= sign-1 0)
                        ;; more difficult:
                        ;; The line is coincident with the edge po/pn
                        (multiple-value-bind (px py) (point-position pnn)
                          (let ((sign-1 (line-equation x1 y1 x2 y2 px py)))
                            (cond ((or (and (> sign-1 0) (< sign-2 0))
                                       (and (< sign-1 0) (> sign-2 0)))
                                   ;; The line goes through the polygons border, by edge po/pn
                                   (call-fun po))
                                  (t
                                   ;; otherwise the line touches the polygon at the edge po/pn,
                                   ;; return both points
                                   (call-fun po)
                                   (call-fun pn))))))
                       (t
                        ;; all other cases: Line either touches polygon in
                        ;; a point or in an edge [handled above]. --GB
                        nil))))))
          ;; The line goes directly through PN (handled later)
          ((multiple-value-bind (px py) (point-position pn)
             (line-contains-point-p x1 y1 x2 y2 px py))
           nil)
          ;; The line doesn't go throuh PO nor PN points. It may cross
          ;; a segment PO-PN or fall outside.
          (t
           (multiple-value-bind (x3 y3) (point-position po)
             (multiple-value-bind (x4 y4) (point-position pn)
               (let* ((dx12 (- x2 x1))
                      (dy12 (- y2 y1))
                      (dx34 (- x4 x3))
                      (dy34 (- y4 y3))
                      (quot (- (* dx34 dy12) (* dx12 dy34))))
                 ;; two straights (lines) given as
                 ;; g : s -> (x1 + s*dx12, y1 + s*dy12)
                 ;; h : t -> (x3 + t*dx34, y3 + t*dy34)
                 ;; -> NIL | (s ; t)
                 (unless (coordinate= quot 0)
                   (let ((k (- (/ (+ (* dx34 (- y1 y3)) (* dy34 x3) (- (* dy34 x1))) quot)))
                         (m (- (/ (+ (* dx12 (- y1 y3)) (* dy12 x3) (- (* dy12 x1))) quot))))
                     (when (<= 0 m 1) ; possible numerical instability
                       (funcall fun k)))))))))))))

(defun overcuts-line/polygon (x1 y1 x2 y2 points)
  (let ((res nil))
    (map-over-overcuts-line/polygon (lambda (k) (push k res)) x1 y1 x2 y2 points)
    (sort res #'<)))

(defun intersection-segment/polygon (x1 y1 x2 y2 polygon)
  (let ((ks (overcuts-line/polygon x1 y1 x2 y2 (polygon-points polygon))))
    (assert (evenp (length ks)))
    (let ((res nil))
      (do ((q ks (cddr q)))
          ((null q))
        (let ((k1 (max 0d0 (min 1d0 (car q))))
              (k2 (max 0d0 (min 1d0 (cadr q)))))
          (when (/= k1 k2)
            (push (make-line* (+ x1 (* k1 (- x2 x1))) (+ y1 (* k1 (- y2 y1)))
                              (+ x1 (* k2 (- x2 x1))) (+ y1 (* k2 (- y2 y1))))
                  res))))
      (cond ((null res) +nowhere+)
            ((null (cdr res)) (car res))
            (t (make-instance 'standard-region-union :regions res))))))

(defun difference-segment/polygon (x1 y1 x2 y2 polygon)
  (let ((ks (overcuts-line/polygon x1 y1 x2 y2 (polygon-points polygon))))
    (assert (evenp (length ks)))
    (let ((res nil)
          (res2 nil))
      (push 0d0 res)
      (do ((q ks (cddr q)))
          ((null q))
        (let ((k1 (max 0d0 (min 1d0 (car q))))
              (k2 (max 0d0 (min 1d0 (cadr q)))))
          (when (/= k1 k2)
            (push k1 res)
            (push k2 res))))
      (push 1d0 res)
      (setf res (nreverse res))
      (do ((q res (cddr q)))
          ((null q))
        (let ((k1 (car q))
              (k2 (cadr q)))
          (when (/= k1 k2)
            (push (make-line* (+ x1 (* k1 (- x2 x1))) (+ y1 (* k1 (- y2 y1)))
                              (+ x1 (* k2 (- x2 x1))) (+ y1 (* k2 (- y2 y1))))
                  res2))))
      (cond ((null res2) +nowhere+)
            ((null (cdr res2)) (car res2))
            (t (make-instance 'standard-region-union :regions res2))))))


;;; rectangle-set

;;; -- interval sums ---------------------------------------------------------

(defun isum-union* (xs ys)        (isum-op xs ys boole-ior   0 0 nil))
(defun isum-difference* (xs ys)   (isum-op xs ys boole-andc2 0 0 nil))
(defun isum-intersection* (xs ys) (isum-op xs ys boole-and   0 0 nil))

;;; You could optimize all this like hell, but I better let the code
;;; alone.
;;; BTW this is the first time I make use of boole-xyz

(defun isum-op (as bs boole-op in-a in-b x0)
  (let (x)
    (cond ((and (null as) (null bs))
           nil)
          (t
           (cond ((null bs)
                  (setq in-a (- 1 in-a))
                  (setq x (pop as)))

                 ((null as)
                  (setq in-b (- 1 in-b))
                  (setq x (pop bs)))

                 ((< (first as) (first bs))
                  (setq in-a (- 1 in-a))
                  (setq x (pop as)))

                 ((< (first bs) (first as))
                  (setq in-b (- 1 in-b))
                  (setq x (pop bs)))

                 (t
                  (setq in-a (- 1 in-a)
                        in-b (- 1 in-b))
                  (setq x (pop as))
                  (pop bs)))

           (cond ((zerop (boole boole-op in-a in-b))
                  (if x0
                      (list* x0 x (isum-op as bs boole-op in-a in-b nil))
                    (isum-op as bs boole-op in-a in-b x0)))
                 (t
                  (if (null x0)
                      (isum-op as bs boole-op in-a in-b x)
                    (isum-op as bs boole-op in-a in-b x0))))))))

;;; -- Bands -----------------------------------------------------------------


;;; A band list is represented by

;;;  ((x_0 . a_0) (x_1 . a_1) ... (x_n . nil))

;;; The a_i are the relevant interval sums for x in [x_i, x_(i+1)].

;;; The empty band could have been representated as
;;;  ((x . nil))  x arbitrary
;;; But to get a canonic representation, I'll choose simply NIL.

;;; A better representation would be
;;;  (x_0 a_0 x_1 a_1 ... x_n)
;;; Pro: Unlimited bands could be represented by simply skipping the
;;; first or last 'x'. So similar representation could apply to
;;; interval sums also. But I let the representation as it is, since
;;; this version is well tested.

(defun bands-op (as bs isum-op z0 a b)
  (let (z1)
    (cond ((and (null as) (null bs))
           (if z0
               (list (cons z0 nil))
             nil))
          (t
           (setq z1 (cond ((null as) (caar bs))
                          ((null bs) (caar as))
                          (t (min (caar as) (caar bs)))))
           (let ((rest (bands-op (if (and as (= z1 (caar as))) (cdr as) as)
                                 (if (and bs (= z1 (caar bs))) (cdr bs) bs)
                                 isum-op
                                 z1
                                 (if (and as (= z1 (caar as))) (cdar as) a)
                                 (if (and bs (= z1 (caar bs))) (cdar bs) b)))
                 (isum (funcall isum-op a b)))
             (if z0
                 (if (and rest (equal isum (cdar rest)))
                     (cons (cons z0 isum)
                           (cdr rest))
                   (cons (cons z0 isum)
                         rest))
               rest))))))

(defun canon-empty-bands (x)
  (cond ((null (cdr x)) nil)
        (t x)))

(defun bands-union (as bs)
  (canon-empty-bands (bands-op as bs #'isum-union* nil nil nil)))

(defun bands-intersection (as bs)
  (canon-empty-bands (bands-op as bs #'isum-intersection* nil nil nil)))

(defun bands-difference (as bs)
  (canon-empty-bands (bands-op as bs #'isum-difference* nil nil nil)))


(defun rectangle->xy-bands* (x1 y1 x2 y2)
  (list (list y1 x1 x2)
        (cons y2 nil)))

(defun rectangle->yx-bands* (x1 y1 x2 y2)
  (list (list x1 y1 y2)
        (cons x2 nil)))

(defun xy-bands->yx-bands (bands)
  ;; Das kann man sicherlich noch viel geschicker machen ...
  (let ((res nil))
    (map-over-bands-rectangles
     (lambda (x1 y1 x2 y2)
       (setf res (bands-union res (rectangle->yx-bands* x1 y1 x2 y2))))
     bands)
    res))

(defun map-over-bands-rectangles (fun bands)
  (map-over-bands (lambda (y1 y2 isum)
                    (do ((p isum (cddr p)))
                        ((null p))
                      (funcall fun (car p) y1 (cadr p) y2)))
                  bands))

(defun map-over-bands (fun bands)
  (do ((q bands (cdr q)))
      ((null (cdr q)))
    (funcall fun (caar q) (caadr q) (cdar q))))

(defun isum-member (elt isum)
  (cond ((null isum) nil)
        ((< elt (car isum)) nil)
        ((<= elt (cadr isum)) t)
        (t (isum-member elt (cddr isum)))))

(defun rectangle->standard-rectangle-set (rect)
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rect)
    (make-instance 'standard-rectangle-set
      :bands (rectangle->xy-bands* x1 y1 x2 y2))))


;;; ellipse

(defun %ellipse-angle->position (ellipse angle)
  (with-slots (tr) ellipse
    (let* ((base-angle (untransform-angle tr (- (* 2 pi) angle)))
           (x0 (cos base-angle))
           (y0 (sin base-angle)))
      (transform-position tr x0 y0))))

(defun %ellipse-position->angle (ellipse x y)
  (multiple-value-bind (xc yc) (ellipse-center-point* ellipse)
    ;; remember, that y-axis is reverted
    (coordinate (atan* (- x xc) (- (- y yc))))))

;;; -- Ellipse simplified representation ---------------------------------------

(defun ellipse-coefficients (ell)
  ;; Returns the coefficients of the equation specifying the ellipse
  ;; as in ax^2 + by^2 + cxy + dx + dy - f = 0

  ;; Note 1:
  ;;   The `f' here may seem to be superfluous, since you
  ;;   could simply multiply the whole equation by 1/f. But this is
  ;;   not the case, since `f' may as well be 0.

  ;; Note 2:
  ;;   In the literature you often find something like
  ;;   (x^2)/a + (y^2)/b - 1 = 0 for an axis aligned ellipse, but
  ;;   I rather choose to treat all coefficients as simple factors instead
  ;;   of denominators.

  (with-slots (tr) ell
    ;; Why the inverse here?
    (multiple-value-bind (a b d e c f)
        (get-transformation (invert-transformation tr))
      (values
       (+ (* a a) (* d d))              ; x**2
       (+ (* b b) (* e e))              ; y**2
       (+ (* 2 a b) (* 2 d e))          ; xy
       (+ (* 2 a c) (* 2 d f))          ; x
       (+ (* 2 b c) (* 2 e f))          ; y
       (+ (* c c) (* f f) -1)))))

;;; Straight from the horse's mouth -- moore
;;;
;;; Axis of an ellipse
;;; -------------------------

;;; Given an ellipse with its center at the origin, as

;;;    ax^2 + by^2 + cxy - 1 = 0

;;; The two axis of an ellipse are characterized by minimizing and
;;; maximizing the radius. Let (x,y) be a point on the delimiter of
;;; the ellipse. It's radius (distance from the origin) then is:

;;;    r^2 = x^2 + y^2

;;; To find the axis can now be stated as an minimization problem with
;;; constraints. So mechanically construct the auxiliarry function H:

;;;   H = x^2 + y^2 - k(ax^2 + by^2 + cxy - 1)

;;; So the following set of equations remain to be solved

;;;   (I)   dH/dx = 0 = 2x + 2kax + kcy
;;;  (II)   dH/dy = 0 = 2y + 2kby + kcx
;;; (III)   dH/dk = 0 = ax^2 + by^2 + cxy - 1

;;; Unfortunately, as I always do the math work - hopelessly, even -
;;; Maxima is the tool of my choice:

;;; g1: 2*x + 2*k*a*x + k*c*y$
;;; g2: 2*y + 2*k*b*y + k*c*x$
;;; g3: a*x*x + b*y*y + c*x*y -1$

;;; sol1: solve ([g1,g2],[k,y])$

;;; /* This yields two solutions because of the squares with occur. The
;;;  * last equation (G3) must therefore be handled for both solutions for
;;;  * y.
;;;  */

;;; y1: rhs(first(rest(first(sol1))))$
;;; y2: rhs(first(rest(first(rest(sol1)))))$

;;; /* Substitute the 'y' found. */
;;; sol2: solve(subst(y1,y,g3),x);
;;; x11: rhs(first(sol2));
;;; x12: rhs(first(rest(sol2)));

;;; sol3: solve(subst(y2,y,g3),x);
;;; x21: rhs(first(sol3));
;;; x22: rhs(first(rest(sol3)));

;;; /* dump everything */
;;; dumpsol([[x=x11,y=y1], [x=x12,y=y1], [x=x21,y=y2], [x=x22,y=y2]]);

(defun ellipse-normal-radii* (ell)
  (multiple-value-bind (a b c) (ellipse-coefficients ell)
    (cond ((coordinate= 0 c)
           ;; this is the unit circle
           (values  0 (sqrt (/ 1 b))
                    (sqrt (/ 1 a)) 0))
          (t
           (let* ((x1 (- (/ c
                            (sqrt (+ (- (* (* c c)
                                           (sqrt (+ (* c c)
                                                    (* b b)
                                                    (- (* 2 a b)) (* a a)))))
                                     (- (* 2 (* b b)
                                           (sqrt (+ (* c c) (* b b)
                                                    (- (* 2 a b)) (* a a)))))
                                     (* 2 a b (sqrt (+ (* c c) (* b b)
                                                       (- (* 2 a b))
                                                       (* a a))))
                                     (* 2 b (* c c))
                                     (* 2 (expt b 3))
                                     (- (* 4 a (* b b))) (* 2 (* a a) b))))))
                  (y1 (- (/ (+ (* (sqrt (+ (* c c)
                                           (* b b)
                                           (- (* 2 a b))
                                           (* a a)))
                                  x1)
                               (- (* b x1)) (* a x1))
                            c)))
                  (x2 (- (/ c
                            (sqrt (+ (* (* c c)
                                        (sqrt (+ (* c c)
                                                 (* b b)
                                                 (- (* 2 a b))
                                                 (* a a))))
                                     (* 2 (* b b) (sqrt (+ (* c c)
                                                           (* b b)
                                                           (- (* 2 a b))
                                                           (* a a))))
                                     (- (* 2 a b (sqrt (+ (* c c)
                                                          (* b b)
                                                          (- (* 2 a b))
                                                          (* a a)))))
                                     (* 2 b (* c c))
                                     (* 2 (expt b 3))
                                     (- (* 4 a (* b b))) (* 2 (* a a) b))))))
                  (y2 (- (/ (+ (- (* (sqrt (+ (* c c)
                                              (* b b)
                                              (- (* 2 a b))
                                              (* a a)))
                                     x2))
                               (- (* b x2)) (* a x2))
                            c))))
             (values x1 y1 x2 y2))))))

;;; this function is used in `ellipse-simplified-representation' to fixup
;;; normalized radius lengths. Can't be interchanged with
;;; `%ellipse-angle->position' because of the rotation inversion.
(defun %ellipse-simplified-representation/radius (ellipse angle)
  (declare (optimize (speed 3)) (inline))
  (with-slots (tr) ellipse
    (let* ((base-angle (untransform-angle tr angle))
           (x (cos base-angle))
           (y (sin base-angle)))
      (multiple-value-bind (mxx mxy myx myy tx ty) (get-transformation tr)
        (values (+ (* mxx x) (* mxy y) tx)
                (+ (* myx x) (* myy y) ty))))))

(defun ellipse-simplified-representation (el)
  ;; returns H (horizontal radius), V (vertical radius) and rotation
  ;; angle in screen coordinates. `ellipse-normal-radii*' returns
  ;; vectors with correct direction, but radius length is shorter than
  ;; in reality (verified with experimentation, not analytically), so
  ;; we compute radius from phi.  If the length were right, we'd
  ;; compute h/v with the following: (sqrt (+ (expt (* hx (cos phi))
  ;; 2) (expt (* hy (sin phi)) 2))) (sqrt (+ (expt (* vx (sin phi)) 2)
  ;; (expt (* vy (cos phi)) 2)))
  (multiple-value-bind (center-x center-y) (ellipse-center-point* el)
    (multiple-value-bind (hx hy) (ellipse-normal-radii* el)
      (let* ((phi (atan* hx hy)))
        ;(multiple-value-bind (hx hy vx vy) (%ellipse-angle->distance el phi))
        (multiple-value-bind (hx hy)
            (%ellipse-simplified-representation/radius el phi)
          (multiple-value-bind (vx vy)
              (%ellipse-simplified-representation/radius el (+ phi (/ pi 2)))
           (values center-x
                   center-y
                   (sqrt (+ (expt (- center-x hx) 2) (expt (- center-y hy) 2)))
                   (sqrt (+ (expt (- center-x vx) 2) (expt (- center-y vy) 2)))
                   phi)))))))

;;; -- Intersection of Ellipse vs. Line ----------------------------------------

(defun intersection-hline/ellipse (el y)
  "Returns coordinates where ellipse intersects with a horizontal line."
  (multiple-value-bind (cx cy h v phi) (ellipse-simplified-representation el)
    (let* ((y (- y cy))
           (cos (cos phi))
           (sin (sin phi))
           (a (+ (expt (* v cos) 2)
                 (expt (* h sin) 2)))
           (b (* 2 y cos sin
                 (- (* v v) (* h h))))
           (c (- (+ (expt (* y v sin) 2)
                    (expt (* y h cos) 2))
                 (expt (* h v) 2)))
           (dc (sqrt (- (* b b) (* 4 a c))))
           (x1 (/ (- (- b) dc)
                  (* 2 a)))
           (x2 (/ (+ (- b) dc)
                  (* 2 a))))
      (values (+ cx x1) (+ cy y) (+ cx x2) (+ cy y)))))

(defun intersection-vline/ellipse (el x)
  "Returns coordinates where ellipse intersects with a vertical line."
  (multiple-value-bind (cx cy h v phi) (ellipse-simplified-representation el)
    (let* ((x (- x cx))
           (cos (cos phi))
           (sin (sin phi))
           (a (+ (expt (* v sin) 2)
                 (expt (* h cos) 2)))
           (b (* 2 x cos sin
                 (- (* v v) (* h h))))
           (c (- (+ (expt (* x v cos) 2)
                    (expt (* x h sin) 2))
                 (expt (* h v) 2)))
           (dc (sqrt (- (* b b) (* 4 a c))))
           (y1 (/ (- (- b) dc)
                  (* 2 a)))
           (y2 (/ (+ (- b) dc)
                  (* 2 a))))
      (values (+ cx x) (+ cy y1) (+ cx x) (+ cy y2)))))

(defun intersection-line/ellipse (el lx1 ly1 lx2 ly2)
  "Returns coordinates where ellipse intersects with arbitrary line (except vertical)."
  (multiple-value-bind (cx cy h v phi) (ellipse-simplified-representation el)
    (let* ((lx1 (- lx1 cx)) (ly1 (- ly1 cy)) (lx2 (- lx2 cx)) (ly2 (- ly2 cy))
           (m-slope (/ (- ly1 ly2) (- lx1 lx2)))
           (b-slope (- ly1 (* m-slope lx1)))
           (cos (cos phi))
           (sin (sin phi))
           (a (+ (* v v
                    (+ (* cos cos)
                       (* 2 m-slope cos sin)
                       (expt (* m-slope sin) 2)))
                 (* h h
                    (+ (expt (* m-slope cos) 2)
                       (* -2 m-slope cos sin)
                       (* sin sin)))))
           (b (+ (* 2 v v b-slope
                    (+ (* cos sin) (* m-slope sin sin)))
                 (* 2 h h b-slope
                    (- (* m-slope cos cos) (* cos sin)))))
           (c (- (* b-slope b-slope
                    (+ (expt (* v sin) 2)
                       (expt (* h cos) 2)))
                 (* h h v v)))
           (dc (sqrt (- (* b b) (* 4 a c))))
           (x1 (/ (- (- b) dc)
                  (* 2 a)))
           (y1 (+ (* m-slope x1) b-slope))
           (x2 (/ (+ (- b) dc)
                  (* 2 a)))
           (y2 (+ (* m-slope x2) b-slope)))
      (values (+ cx x1) (+ cy y1) (+ cx x2) (+ cy y2)))))

;;; -- Intersection of Ellipse vs. Ellipse -----------------------------------

;;; This entire thing is so incomprehensible, that I have to look for
;;; my notes, to present the derivation for the solution of the
;;; conic section problem.

(defun intersection-ellipse/ellipse (e1 e2)
  ;; We reduce one of the two ellipses to the unit circle.
  (let ((a (invert-transformation (slot-value e1 'tr))))
    (let ((r (intersection-ellipse/unit-circle (transform-region a e2))))
      (if (atom r)
          r
        (mapcar (lambda (p)
                  (multiple-value-bind (x y)
                      (transform-position (slot-value e1 'tr) (car p) (cdr p))
                    (make-point x y)))
                r)))))

(defun intersection-ellipse/unit-circle (ell)
  (multiple-value-bind (a b c d e f) (ellipse-coefficients ell)
    (let ((pn (elli-polynom ell)))
      (cond ((= (length pn) 0)
             :coincident)
            (t
             (let ((ys (newton-iteration pn 0d0))
                   (res nil))
               (dolist (y ys)
                 (let ((x (sqrt (- 1 (* y y)))))
                   (when (realp x)
                     (when (coordinate= 0 (ellipse-equation a b c d e f x y))
                       (pushnew (cons x y) res :test #'equal))
                     (when (coordinate= 0 (ellipse-equation a b c d e f (- x) y))
                       (pushnew (cons (- x) y) res :test #'equal)))))
               res))))))

(defun ellipse-equation (a b c d e f x y)
  (+ (* a x x) (* b y y) (* c x y) (* d x) (* e y) f))

(defun elli-polynom (ell)
  ;; It is rather funny that for two circles we always get a polynomial
  ;; of degree two.
  (multiple-value-bind (a b c d e f) (ellipse-coefficients ell)
    (canonize-polynom
     (vector (+ (* (- b a) (- b a)) (* c c))
             (+ (* 2 b e) (* -2 a e) (* 2 c d))
             (+ (* e e) (* 2 (- b a) (+ a f)) (* -1 c c) (* d d))
             (+ (* 2 e a) (* 2 e f) (* -2 c d))
             (+ (* (+ a f) (+ a f)) (* -1 d d))))))

;;; We just build ourselves a simple newton iteration. Sometimes we fail
;;; desperately at local minima. But apart from that convergence behaviour for
;;; our problem is quite good. But we partly still obtain sizable errors by
;;; dividing at the function roots; I'm trying to alleviate this by executing a
;;; few newton steps (newton-ziel-gerade, meaning "newton home stretch") with
;;; the original polynomial after finding a root.

;;; I shouldn't be so lazy and consult the comprehensive literature; there must
;;; be something better than newton iteration. I vaguely remember a numerics
;;; lecture ...

(defun newton-ziel-gerade (pn x &optional (n 4))
  (cond ((= n 0) x)
        ((multiple-value-bind (f p2) (horner-schema pn x)
           (multiple-value-bind (f*) (horner-schema p2 x)
             (newton-ziel-gerade pn (- x (/ f f*)) (- n 1)))))))

(defun solve-p1 (b c)
  (if (= b 0)
      nil
    (list (- (/ c b)))))

(defun solve-p2 (a b c)
  (cond ((= a 0)
         (solve-p1 b c))
        (t
         (let* ((p (/ b a))
                (q (/ c a))
                (d (- (/ (* p p) 4) q)))
           (cond ((< d 0)
                  nil)
                 ((= d 0)
                  (list (/ p 2)))
                 (t
                  (list (+ (/ p 2) (sqrt d))
                        (- (/ p 2) (sqrt d)))))))))

(defun maybe-solve-polynom-trivially (pn)
  (case (length pn)
    (0 (values nil t))
    (1 (values nil t))
    (2 (values (solve-p1 (aref pn 0) (aref pn 1)) t))
    (3 (values (solve-p2 (aref pn 0) (aref pn 1) (aref pn 2)) t))
    (t (values nil nil))))

(defun canonize-polynom (pn)
  (cond ((= (length pn) 0) pn)
        ((coordinate= (aref pn 0) 0)
         (canonize-polynom (subseq pn 1)))
        (t pn)))

(defun newton-iteration (polynom x-start)
  ;; ATTENTION: Adapted specifically to our problem, do not use this without
  ;; reading!
  (multiple-value-bind (sol done?) (maybe-solve-polynom-trivially polynom)
    (cond (done?
           sol)
          (t
           (let ((x x-start)
                 x1
                 (n 0)
                 (pn polynom)
                 (eps-f 0d0)
                 (eps-f* 0d-16)
                 (eps-x 1d-20)
                 (m 20)                 ;maximum number of steps
                 (res nil))
             (loop
               (cond ((> n m)
                      (return)))
               (multiple-value-bind (f p2) (horner-schema pn x)
                 (multiple-value-bind (f*) (horner-schema p2 x)
                   (cond ((<= (abs f*) eps-f*)
                          ;; We are stuck at an extremum -- continue with random
                          ;; starting value
                          (setf x1 (+ 1d0 (random 2d0))))
                         (t
                          (setf x1 (- x (/ f f*)))
                          (cond ((or (<= (abs f) eps-f)
                                     (<= (abs (- x1 x)) eps-x))
                                 ;; a few more steps of newton, to improve
                                 ;; the result
                                 (setf x1 (newton-ziel-gerade polynom x1))
                                 (push x1 res)
                                 ;; divide (roots)
                                 (multiple-value-bind (f p2) (horner-schema pn x1)
                                   f
                                   (setq pn (canonize-polynom p2))
                                   (multiple-value-bind (sol done?)
                                       (maybe-solve-polynom-trivially pn)
                                     (when done?
                                       ;; iterate more nonetheless here -- is
                                       ;; this a good idea?
                                       (setf sol
                                             (mapcar (lambda (x)
                                                       (newton-ziel-gerade
                                                        polynom x))
                                                     sol))
                                       (setf res (nconc sol res))
                                       (return))))
                                 (setf x1 x-start)
                                 (setq n 0))))))
                 (setf x (min 1d0 (max -1d0 x1)))        ;Is this allowed?
                 (incf n)))
             res)))))

(defun horner-schema (polynomial x)
  ;; Evaluate POLYNOMIAL by means of horner's method at the
  ;; place `x'; returns two values:
  ;; - the value of the function
  ;; - the last line of horner's method (result of division)
  (let ((n (length polynomial)))
    (cond ((= n 0) (values 0))
          ((= n 1) (values (aref polynomial 0) '#()))
          (t
           (let ((b (make-array (1- n))))
             (setf (aref b 0) (aref polynomial 0))
             (do ((i 1 (+ i 1)))
                 ((= i (- n 1))
                  (values
                   (+ (* (aref b (- i 1)) x) (aref polynomial i))
                   b))
               (setf (aref b i) (+ (* (aref b (- i 1)) x) (aref polynomial i)))))))))


;;; routines for approximating ellipses as bezier curves

;;;
;;; Many backends, such as PDF and PostScript don't provide shape
;;; drawing functions per se, but rather primitives for working with
;;; paths such as lines and bezier curves. One can closely approximate
;;; arbitrary ellipse path with appropriate bezier curves. A good
;;; primer on drawing ellipses with lines, or quadratic or cubic
;;; bezier curves can be found here:
;;;
;;; <https://www.spaceroots.org/documents/ellipse/index.html>.
;;;
;;; We use the algorithm described in the above reference to construct
;;; cubic bezier curves.

;;; I doubt that these functions belong in geometry utilities but I
;;; move them anyway. -- jd 2019-10-10

;;
;; CLIM describes general (not neccessarily axis-aligned) ellipses by
;; their center and two vectors describing the radii. The formulation
;; for general ellipses described by Luc Maisonobe, referenced above,
;; computes the ellipse paths based on the center, the length of each
;; radius vectors, and the angle between the two radii. Just as a
;; circle could be (over-)described by its center and a specific 2-d
;; radius, but only the length of the radius is needed to definitively
;; describe the circle, any two (non-colinear) radius vectors could
;; describe a general ellipse. Maisonobe describes an ellipse by two
;; vectors, the semi-major axis, the semi-minor axis at a right angle
;; to the semi-major axis, and the angle of the semi-major axis
;; relative to the positive x-axis. So, given two radii, we call the
;; code in clim-basic/region.lisp that gives a, b, and theta.
(defun reparameterize-ellipse (radius1-dx radius1-dy radius2-dx radius2-dy)
  "Returns three values, the length of radius 1, the length of radius
2, and the angle (CCW in cartesian coordinates) between the two
vectors."

  (let ((ell (climi::make-ellipse* 0 0 radius1-dx radius1-dy radius2-dx radius2-dy)))
    (multiple-value-bind (cx cy a b theta)
        (climi::ellipse-simplified-representation ell)
      (declare (ignore cx cy))
      (values a b theta))))

(defun ellipse-point (lambda0 center-x center-y a b theta)
  "Given an ellipse having center CENTER-X, CENTER-Y, and two radii of
length A and B, with angle THETA between the radii from the center of
the ellipse, returns two values, the x and y coordinates of a point on
the ellipse having angle LAMBDA0 (CCW) relative to the major axis of the
ellipse."
  (let ((eta (atan (/ (sin lambda0) b)
                   (/ (cos lambda0) a))))
    (values (+ center-x
               (* a (cos theta) (cos eta))
               (- (* b (sin theta) (sin eta))))
            (+ center-y
               (* a (sin theta) (cos eta))
               (* b (cos theta) (sin eta))))))

(defun ellipse-point* (lambda0
                       center-x center-y
                       radius1-dx radius1-dy radius2-dx radius2-dy)
  "Given an ellipse having center CENTER-X, CENTER-Y, and two radii,
one described by RADIUS1-DX and RADIUS1-DY, and the other described by
RADIUS2-DX and RADIUS2-DY, returns two values, the x and y coordinates
of a point on the ellipse having angle LAMBDA0 (CCW) relative to the
positive direction of the line parallel to the x-axis that runs
through the center of the ellipse. Note that this parameterization of
LAMBDA0 is different from that used in ELLIPSE-POINT, which is relative
to the major axis."
  (multiple-value-bind (a b theta)
      (reparameterize-ellipse radius1-dx radius1-dy radius2-dx radius2-dy)
    (ellipse-point (- lambda0 theta) center-x center-y a b theta)))

(defun ellipse-derivative (eta a b theta)
  "Given an ellipse having two radii of length A and B, with angle
THETA between the radii from the center of the ellipse, returns two
values, the x and y coordinates of the derivative of the
parametrically curve of ellipse at the parametric angle eta. Note that
this eta is not the angle ANGLE, as in ellipse-derivative*, but rather
is computed parametricly from theta. See the paper from Luc Maisonobe
for details."
  (values (+ (- (* a (cos theta) (sin eta)))
             (- (* b (sin theta) (cos eta))))
          (+ (- (* a (sin theta) (sin eta)))
             (* b (cos theta) (cos eta)))))

(defun ellipse-cubic-bezier-control-points (lambda1 lambda2 a b theta)
  "Given two angles, LAMBDA1 and LAMBDA2 of an ellipse having two
radii of length A and B, with angle THETA between the radii from the
center of the ellipse, returns 4 values, the relative x and y
distances of two control points from each of two edge points of a
quadratic bezier curve approximating the ellipse."
  (let ((eta1 (atan (/ (sin lambda1) b)
                    (/ (cos lambda1) a)))
        (eta2 (atan (/ (sin lambda2) b)
                    (/ (cos lambda2) a))))
    (let ((alpha (* (sin (- eta2 eta1))
                    (/ (- (sqrt (+ 4 (* 3 (square (tan (/ (- eta2 eta1) 2)))))) 1)
                       3))))
      (multiple-value-bind (e1x e1y)
          (ellipse-derivative eta1 a b theta)
        (multiple-value-bind (e2x e2y)
            (ellipse-derivative eta2 a b theta)
          (values (* alpha e1x)
                  (* alpha e1y)
                  (* alpha e2x)
                  (* alpha e2y)))))))

(defun ellipse-cubic-bezier-control-points* (lambda1 lambda2
                                             radius1-dx radius1-dy
                                             radius2-dx radius2-dy)
  "Given an ellipse having center CENTER-X, CENTER-Y, and two radii,
one described by RADIUS1-DX and RADIUS1-DY, and the other described by
RADIUS2-DX and RADIUS2-DY, returns four values corresponding to x1,
y1, and x2, y2, of the two control points of a cubic bezier curve
approximation of the elliptical arc from angle lambda1 to lambda2."
  (multiple-value-bind (a b theta)
      (reparameterize-ellipse radius1-dx radius1-dy radius2-dx radius2-dy)
    (let ((lambda1 (- lambda1 theta))
          (lambda2 (- lambda2 theta)))
      (ellipse-cubic-bezier-control-points lambda1 lambda2 a b theta))))

(defun ellipse-cubic-bezier-points (lambda1 lambda2
                                    center-x center-y
                                    a b theta)
  "Returns 8 values, the x and y points of ellipse point 1, control
point 1, control point 2 and ellipse point 2 of a cubic bezier curve
approximating the elliptical arc from angle lambda1 to lambda2 of the
ellipse having center CENTER-X, CENTER-Y, and two radii of length A
and B, with angle THETA between the radii from the center of the
ellipse."
  (multiple-value-bind (p1x p1y)
      (ellipse-point lambda1 center-x center-y a b theta)
    (multiple-value-bind (p2x p2y)
        (ellipse-point lambda2 center-x center-y a b theta)
      (multiple-value-bind (e1x e1y e2x e2y)
          (ellipse-cubic-bezier-control-points lambda1 lambda2 a b theta)
        (values p1x p1y
                (+ p1x e1x) (+ p1y e1y)
                (- p2x e2x) (- p2y e2y)
                p2x p2y)))))

(defun ellipse-cubic-bezier-points* (lambda1 lambda2
                                     center-x center-y
                                     radius1-dx radius1-dy
                                     radius2-dx radius2-dy)
  "Returns 8 values, the x and y points of ellipse point 1, control
point 1, control point 2 and ellipse point 2 of a cubic bezier curve
approximating the elliptical arc from angle lambda1 to lambda2 of the
ellipse having center CENTER-X, CENTER-Y, and two radii, one described
by RADIUS1-DX and RADIUS1-DY, and the other described by RADIUS2-DX
and RADIUS2-DY"
  (multiple-value-bind (p1x p1y)
      (ellipse-point* lambda1 center-x center-y
                      radius1-dx radius1-dy radius2-dx radius2-dy)
    (multiple-value-bind (p2x p2y)
        (ellipse-point* lambda2 center-x center-y
                        radius1-dx radius1-dy radius2-dx radius2-dy)
      (multiple-value-bind (e1x e1y e2x e2y)
          (ellipse-cubic-bezier-control-points* lambda1 lambda2
                                                radius1-dx radius1-dy radius2-dx radius2-dy)
        (values p1x p1y
                (+ p1x e1x) (+ p1y e1y)
                (- p2x e2x) (- p2y e2y)
                p2x p2y)))))
