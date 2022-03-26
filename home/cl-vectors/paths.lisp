;;;; cl-vectors -- Rasterizer and paths manipulation library
;;;; Copyright (C) 2007-2013  Frédéric Jolliton <frederic@jolliton.com>
;;;; This code is licensed under the MIT license.

;;;; This file provides facilities to create and manipulate vectorial paths.

#+nil(error "This file assume that #+NIL is never defined.")

(in-package #:net.tuxee.paths)

(defvar *bezier-distance-tolerance* 0.5
  "The default distance tolerance used when rendering Bezier
curves.")

(defvar *bezier-angle-tolerance* 0.05
  "The default angle tolerance (in radian) used when rendering
Bezier curves")

(defvar *arc-length-tolerance* 1.0
  "The maximum length of segment describing an arc.")

(defvar *miter-limit* 4.0
  "Miter limit before reverting to bevel joint. Must be >=1.0.")

;;;--[ Math utilities ]------------------------------------------------------

;;; http://mathworld.wolfram.com/Line-LineIntersection.html
(defun line-intersection (x1 y1 x2 y2
                          x3 y3 x4 y4)
  "Compute the intersection between 2 lines (x1,y1)-(x2,y2)
and (x3,y3)-(x4,y4). Return the coordinates of the intersection
points as 2 values. If the 2 lines are colinears, return NIL."
  (flet ((det (a b c d)
           (- (* a d)
              (* b c))))
    (let* ((dx1 (- x2 x1))
           (dy1 (- y2 y1))
           (dx2 (- x4 x3))
           (dy2 (- y4 y3))
           (d (det dx2 dy2 dx1 dy1)))
      (unless (zerop d)
        (let ((a (det x1 y1 x2 y2))
              (b (det x3 y3 x4 y4)))
          (values (/ (det a dx1 b dx2) d)
                  (/ (det a dy1 b dy2) d)))))))

(defun line-intersection/delta (x1 y1 dx1 dy1
                                x2 y2 dx2 dy2)
  "Compute the intersection between the line by (x1,y1) and
direction (dx1,dy1) and the line by (x2,y2) and
direction (dx2,dy2). Return the coordinates of the intersection
points as 2 values. If the 2 lines are colinears, return NIL."
  (flet ((det (a b c d)
           (- (* a d)
              (* b c))))
    (let ((d (det dx2 dy2 dx1 dy1)))
      (unless (zerop d)
        (let ((a (det x1 y1 (+ x1 dx1) (+ y1 dy1)))
              (b (det x2 y2 (+ x2 dx2) (+ y2 dy2))))
          (values (/ (det a dx1 b dx2) d)
                  (/ (det a dy1 b dy2) d)))))))

(defun normalize (x y &optional (length 1.0))
  "Normalize the vector (X,Y) such that its length is LENGTH (or
1.0 if unspecified.) Return the component of the resulting vector
as 2 values. Return NIL if the input vector had a null length."
  (if (zerop length)
      (values 0.0 0.0)
      (let ((norm (/ (sqrt (+ (* x x) (* y y))) length)))
        (unless (zerop norm)
          (values (/ x norm) (/ y norm))))))

(defun line-normal (x1 y1 x2 y2)
  "Normalize the vector (X2-X1,Y2-Y1). See NORMALIZE."
  (normalize (- x2 x1) (- y2 y1)))

;;;--[ Points ]--------------------------------------------------------------

;;; Points are supposed to be immutable

(declaim (inline make-point point-x point-y))
(defun make-point (x y) (cons x y))
(defun point-x (point) (car point))
(defun point-y (point) (cdr point))

;;; Utility functions for points

(defun p+ (p1 p2)
  (make-point (+ (point-x p1) (point-x p2))
              (+ (point-y p1) (point-y p2))))

(defun p- (p1 p2)
  (make-point (- (point-x p1) (point-x p2))
              (- (point-y p1) (point-y p2))))

(defun p* (point scale &optional (scale-y scale))
  (make-point (* (point-x point) scale)
              (* (point-y point) scale-y)))

(defun point-rotate (point angle)
  "Rotate POINT by ANGLE radian around the origin."
  (let ((x (point-x point))
        (y (point-y point)))
    (make-point (- (* x (cos angle)) (* y (sin angle)))
                (+ (* y (cos angle)) (* x (sin angle))))))

(defun point-angle (point)
  "Compute the angle of POINT relatively to the X axis."
  (atan (point-y point) (point-x point)))

(defun point-norm (point)
  "Compute the distance of POINT from origin."
  (sqrt (+ (expt (point-x point) 2)
           (expt (point-y point) 2))))

;; (point-norm (p- p2 p1))
(defun point-distance (p1 p2)
  "Compute the distance between P1 and P2."
  (sqrt (+ (expt (- (point-x p2) (point-x p1)) 2)
           (expt (- (point-y p2) (point-y p1)) 2))))

;; (p* (p+ p1 p2) 0.5)
(defun point-middle (p1 p2)
  "Compute the point between P1 and P2."
  (make-point (/ (+ (point-x p1) (point-x p2)) 2.0)
              (/ (+ (point-y p1) (point-y p2)) 2.0)))

;;;--[ Paths ]---------------------------------------------------------------

(defstruct path
  (type :open-polyline :type (member :open-polyline :closed-polyline :polygon))
  (orientation :unknown :type (member :unknown :cw :ccw))
  (knots (make-array 0 :adjustable t :fill-pointer 0))
  (interpolations (make-array 0 :adjustable t :fill-pointer 0)))

(defun create-path (type)
  "Create a new path of the given type. The type must be one of
the following keyword:

:open-polyline -- An open polyline path,
:closed-polyline -- A closed polyline path,
:polygon -- Like :closed-polyline, but implicitly filled."
  (assert (member type '(:open-polyline :closed-polyline :polygon)))
  (make-path :type type))

(defun path-clear (path)
  "Clear the path such that it is empty."
  (setf (path-orientation path) :unknown
        (fill-pointer (path-knots path)) 0
        (fill-pointer (path-interpolations path)) 0))

(defun path-reset (path knot)
  "Reset the path such that it is a single knot."
  (path-clear path)
  (vector-push-extend knot (path-knots path))
  (vector-push-extend (make-straight-line) (path-interpolations path)))

(defun path-extend (path interpolation knot)
  "Extend the path to KNOT, with INTERPOLATION."
  (vector-push-extend interpolation (path-interpolations path))
  (vector-push-extend knot (path-knots path))
  ;; Extending the path can change how the orientation is
  ;; auto-detected.
  (setf (path-orientation path) :unknown))

(defun path-concatenate (path interpolation other-path)
  "Append OTHER-PATH to PATH, joined by INTERPOLATION."
  (let ((interpolations (path-interpolations other-path))
        (knots (path-knots other-path)))
    (loop for i below (length knots)
       do (path-extend path
                       (interpolation-clone (if (and (zerop i) interpolation)
                                                interpolation
                                                (aref interpolations i)))
                       (aref knots i)))))

(defun path-replace (path other-path)
  "Replace PATH with contents of OTHER-PATH."
  (path-clear path)
  (path-concatenate path nil other-path))

(defun path-size (path)
  "Return the number of knots on the path."
  (length (path-knots path)))

(defun path-last-knot (path)
  "Return the last knot of the path. Return NIL if the path is
empty."
  (let ((knots (path-knots path)))
    (when (plusp (length knots))
      (aref knots (1- (length knots))))))

(defun path-guess-orientation (path)
  "Guess the orientation of the path.

This is implemented loosely because we don't take care about
interpolations. We only consider a polygon described by the
knots. However, it should work..

Update path orientation flag, and returns either :CW or :CCW."
  (let ((knots (path-knots path)))
    (let ((loose-area (loop for last-knot-index = (1- (length knots)) then knot-index
                         for knot-index below (length knots)
                         sum (- (* (point-x (aref knots last-knot-index))
                                   (point-y (aref knots knot-index)))
                                (* (point-x (aref knots knot-index))
                                   (point-y (aref knots last-knot-index)))))))
      (setf (path-orientation path) (if (plusp loose-area) :ccw :cw)))))

(defun path-orient (path orientation &optional other-paths)
  "Orient the path in the given orientation.

If OTHER-PATHS is specified, then the paths are reversed
inconditionnaly if PATH is also reversed."
  (assert (member orientation '(:cw :ccw)) (orientation) "Expected either :CW or :CCW")
  (when (eq (path-orientation path) :unknown)
    (path-guess-orientation path))
  (unless (eq (path-orientation path) orientation)
    (path-reverse path)
    (map nil #'path-reverse other-paths))
  (values))

;;; Iterators

(defgeneric path-iterator-reset (iterator)
  (:documentation "Reset the iterator before the first knot."))

(defgeneric path-iterator-next (iterator)
  (:documentation "Move the iterator to the next knot, and return
3 values: INTERPOLATION, KNOT and END-P. INTERPOLATION is the
interpolation between the previous knot and the current one. For
the first iteration, INTERPOLATION is usually the implicit
straight line between the last knot and the first knot. KNOT and
INTERPOLATION are null if the path is empty. END-P is true if the
knot is the last on the path or if the path is empty."))

(defun path-from-iterator (iterator type)
  "Construct a new path from the given iterator."
  (let ((path (create-path type)))
    (loop
       (multiple-value-bind (iterator knot end-p) (path-iterator-next iterator)
         (path-extend path iterator knot)
         (when end-p
           (return path))))))

;;; Classic iterator

(defstruct path-iterator-state
  path index)

(defun path-iterator (path)
  (make-path-iterator-state :path path :index nil))

(defmethod path-iterator-reset ((iterator path-iterator-state))
  (setf (path-iterator-state-index iterator) nil))

(defmethod path-iterator-next ((iterator path-iterator-state))
  (let* ((index (path-iterator-state-index iterator))
         (path (path-iterator-state-path iterator))
         (knots (path-knots path))
         (interpolations (path-interpolations path)))
    (cond
      ((zerop (length knots))
       (values nil nil t))
      (t
       ;; Update index to the next place
       (setf index
             (setf (path-iterator-state-index iterator)
                   (if (null index) 0 (mod (1+ index) (length knots)))))
       (values (aref interpolations index)
               (aref knots index)
               (= index (1- (length knots))))))))

;;; Segmented iterator
;;;
;;; This iterator iterate over segmented interpolation, if the
;;; interpolation is matched by the predicate. This is useful for
;;; algorithms that doesn't handle certain type of interpolations.
;;; The predicate could test the type, but also certain type of
;;; interpolation (such as arc of circle vs arc of ellipse, or degree
;;; of the Bezier curves.)

;;; Note: I use PI prefix instead of PATH-ITERATOR to shorten names.

(defstruct pi-segmented-state
  path index predicate end-p queue)

(defun path-iterator-segmented (path &optional (predicate (constantly t)))
  (make-pi-segmented-state :path path :index nil
                                      :predicate predicate
                                      :end-p nil :queue nil))

(defmethod path-iterator-reset ((iterator pi-segmented-state))
  (setf (pi-segmented-state-index iterator) nil
        (pi-segmented-state-queue iterator) nil))

(defmethod path-iterator-next ((iterator pi-segmented-state))
  (flet ((update-queue (interpolation k1 k2 last-p)
           (let (new-queue)
             (interpolation-segment interpolation k1 k2 (lambda (p) (push p new-queue)))
             (push k2 new-queue)
             (setf (pi-segmented-state-end-p iterator) last-p
                   (pi-segmented-state-queue iterator) (nreverse new-queue))))
         (dequeue ()
           (let* ((knot (pop (pi-segmented-state-queue iterator)))
                  (end-p (and (pi-segmented-state-end-p iterator)
                              (null (pi-segmented-state-queue iterator)))))
             (values (make-straight-line) knot (when end-p t)))))
    (cond
      ((pi-segmented-state-queue iterator)
       ;; Queue is not empty, process it first.
       (dequeue))
      (t
       ;; Either refill the queue, or return the next straight line
       ;; from the sub iterator.
       (let* ((index (pi-segmented-state-index iterator))
              (path (pi-segmented-state-path iterator))
              (knots (path-knots path))
              (interpolations (path-interpolations path)))
         (cond
           ((zerop (length knots))
            ;; Empty path.
            (values nil nil t))
           (t
            ;; Update index to the next place
            (setf index
                  (setf (pi-segmented-state-index iterator)
                        (if (null index) 0 (mod (1+ index) (length knots)))))
            (let ((interpolation (aref interpolations index))
                  (knot (aref knots index))
                  (end-p (= index (1- (length knots)))))
              ;; Check if we have to segment the next interpolation
              (if (funcall (pi-segmented-state-predicate iterator)
                           interpolation)
                  (let ((previous-index (mod (1- index) (length knots))))
                    (update-queue interpolation
                                  (aref knots previous-index)
                                  knot end-p)
                    (dequeue))
                  (values interpolation knot end-p))))))))))

;;; Iterate distinct

;;; This iterator filter out identical knots. That is, the knots with
;;; the same positions, with any interpolation. (All interpolations
;;; currently implemented are empty when knot around them are not
;;; distinct.)

;;; When cyclic-p is true, the first knot of the iterator is the first
;;; knot distinct from the first knot of the reference iterator.

;;; When cyclic-p is false, the first knot of the iterator if the
;;; first knot of the reference iterator, and if the path ends with a
;;; knot which is not distinct from the first, it is kept.

(defclass filter-distinct-state ()
  ((iterator :initarg :iterator)
   (cyclic-p :initarg :cyclic-p)
   (fixed :initarg :fixed)
   (next :initarg :next)
   (next-is-end-p)))

(defun filter-distinct (iterator &optional (preserve-cyclic-end-p nil))
  (make-instance 'filter-distinct-state
                 :iterator iterator
                 :cyclic-p (not preserve-cyclic-end-p)
                 :fixed nil
                 :next nil))

(defmethod path-iterator-reset ((iterator filter-distinct-state))
  (with-slots ((sub iterator) next next-is-end-p) iterator
    (path-iterator-reset sub)
    (setf next nil
          next-is-end-p nil)))

(defmethod path-iterator-next ((iterator filter-distinct-state))
  (with-slots ((sub iterator) cyclic-p fixed next next-is-end-p) iterator
    (when fixed
      ;; constant result cached
      (return-from path-iterator-next (values-list fixed)))
    (labels ((get-next ()
               "Get the next knot information as a list (not as
               multiple values)."
               (multiple-value-list (path-iterator-next sub)))
             (distinct-p (a b)
               "Test if A and B have distinct knots."
               (not (zerop (point-distance (second a) (second b)))))
             (move-to-next (previous loop-p)
               "Move iterator to find a knot distinct from the
               PREVIOUS. Also indicate if the resulting knot is
               the first of the sub iterator, and if end of path
               was encountered. This is needed to compute the
               effective END-P flag for the resulting iterator."
               (loop
                  with first-p = (third previous)
                  with end-encountered-p = (third previous)
                  for current = (get-next)
                  until (or (distinct-p previous current)
                            (and (not loop-p) first-p))
                  do (setf first-p (third current))
                  when (third current)
                  do (setf end-encountered-p t)
                  finally (return (values current first-p end-encountered-p)))))
      (let (result)
        (unless next
          ;; First time we iterate.
          (setf next-is-end-p nil)
          (let ((first (get-next)))
            (cond
              ((or (not (second first))
                   (third first))
               ;; It was an empty path or a single knot path. Cache it
               ;; and returns it for each further iterations.
               (setf fixed first
                     result first))
              (cyclic-p
               (multiple-value-bind (first-in-cycle first-p end-p) (move-to-next first nil)
                 (declare (ignore first-p))
                 (cond
                   (end-p
                    (setf (third first) t
                          fixed first
                          result first))
                   (t
                    (setf next first-in-cycle)))))
              (t
               (setf next first)))))
        (unless result
          ;; We copy NEXT because we need to modify RESULT, and since
          ;; NEXT is kept for the next iteration, we take care of not
          ;; modifying it.
          (setf result (copy-seq next)
                (third result) next-is-end-p)
          (multiple-value-bind (current first-p end-encountered-p) (move-to-next next cyclic-p)
            (setf next current)
            ;; Set end marker
            (cond
              (cyclic-p
               (setf next-is-end-p first-p)
               (when (and end-encountered-p (not first-p))
                 (setf (third result) t)))
              (t
               (setf (third result) end-encountered-p)))))
        (values-list result)))))

;;; Misc

(defun path-clone (path)
  (let ((new-interpolations (copy-seq (path-interpolations path))))
    (loop for i below (length new-interpolations)
       do (setf (aref new-interpolations i)
                (interpolation-clone (aref new-interpolations i))))
    (let ((new-path (create-path (path-type path))))
      (setf (path-knots new-path) (copy-seq (path-knots path))
            (path-interpolations new-path) new-interpolations
            (path-orientation new-path) (path-orientation path))
      new-path)))

(defun path-reverse (path)
  ;; reverse the order of knots
  (setf (path-knots path) (nreverse (path-knots path)))
  ;; reverse the order of interpolations 1..n (not the first one,
  ;; which is the implicit straight line.)
  (loop with interpolations = (path-interpolations path)
     with length = (length interpolations)
     for i from 1 upto (floor (1- length) 2)
     do (rotatef (aref interpolations i)
                 (aref interpolations (- length i))))
  ;; reverse each interpolation
  (loop for interpolation across (path-interpolations path)
     do (interpolation-reverse interpolation))
  (unless (eq (path-orientation path) :unknown)
    (setf (path-orientation path) (ecase (path-orientation path)
                                    (:cw :ccw)
                                    (:ccw :cw))))
  path)

(defun path-reversed (path)
  (let ((new-path (path-clone path)))
    (path-reverse new-path)
    new-path))

(defmacro do-path ((path interpolation knot) &body body)
  (let ((path-sym (gensym))
        (knots (gensym))
        (interpolations (gensym))
        (index (gensym)))
    `(symbol-macrolet ((,interpolation (aref ,interpolations ,index))
                       (,knot (aref ,knots ,index)))
       (loop
          with ,path-sym = ,path
          with ,knots = (path-knots ,path-sym)
          with ,interpolations = (path-interpolations ,path-sym)
          for ,index below (length ,knots)
          do (progn ,@body)))))

(defun path-translate (path vector)
  "Translate the whole path accordingly to VECTOR."
  (if (listp path)
      (dolist (path-item path)
        (path-translate path-item vector))
      (unless (and (zerop (point-x vector))
                   (zerop (point-y vector)))
        (do-path (path interpolation knot)
          (setf knot (p+ knot vector))
          (interpolation-translate interpolation vector))))
  path)

(defun path-rotate (path angle &optional center)
  "Rotate the whole path by ANGLE radian around CENTER (which is
the origin if unspecified.)"
  (if (listp path)
      (dolist (path-item path)
        (path-rotate path-item angle center))
      (unless (zerop angle)
        (when center
          (path-translate path (p* center -1.0)))
        (do-path (path interpolation knot)
          (setf knot (point-rotate knot angle))
          (interpolation-rotate interpolation angle))
        (when center
          (path-translate path center))))
  path)

(defun path-scale (path scale-x scale-y &optional center)
  "Scale the whole path by (SCALE-X,SCALE-Y) from CENTER (which
is the origin if unspecified.) Warning: not all interpolations
support non uniform scaling (when scale-x /= scale-y)."
  ;;; FIXME: What to do about path-orientation?
  (if (listp path)
      (dolist (path-item path)
        (path-scale path-item scale-x scale-y center))
      (progn
        (when center
          (path-translate path (p* center -1.0)))
        (do-path (path interpolation knot)
          (setf knot (p* knot scale-x scale-y))
          (interpolation-scale interpolation scale-x scale-y))
        (when center
          (path-translate path center))
        (when (minusp (* scale-x scale-y))
          (path-reverse path))))
  path)

(defun path-end-info (path side)
  (when (>= (path-size path) 2)
    (if (not side)
        (values (aref (path-knots path) 0)
                (interpolation-normal (aref (path-interpolations path) 1)
                                      (aref (path-knots path) 0)
                                      (aref (path-knots path) 1)
                                      nil))
        (let ((ks (length (path-knots path)))
              (is (length (path-interpolations path))))
          (values (aref (path-knots path) (1- ks))
                  (interpolation-normal (aref (path-interpolations path) (1- is))
                                        (aref (path-knots path) (- is 2))
                                        (aref (path-knots path) (- is 1))
                                        t))))))

(defun path-transform-as-marker (path path-reference side &key (offset 0.0) (scale 1.0) (angle 0.0))
  "Translate, rotate and scale PATH representing a marker such
that it is adapted to the PATH-REFERENCE. If SIDE is false, it is
placed at the start of the path, otherwise it is placed at the
end of the path."
  (multiple-value-bind (knot normal) (path-end-info path-reference side)
    (when knot
      (path-rotate path (+ (/ pi -2) angle (point-angle normal)))
      (path-scale path scale scale)
      (path-translate path (p+ knot (p* normal offset)))
      path)))

;;;--[ Interpolations ]------------------------------------------------------

(defgeneric interpolation-segment (interpolation k1 k2 function)
  (:documentation "Segment the path between K1 and K2 described
by the INTERPOLATION. Call FUNCTION for each generated point on
the interpolation path."))

(defgeneric interpolation-normal (interpolation k1 k2 side)
  (:documentation "Compute the normal, going \"outside\" at
either K1 (if SIDE is false) or K2 (if SIDE is true). Return NIL
if the normal cannot be computed. Return a point otherwise."))

(defgeneric interpolation-clone (interpolation)
  (:documentation "Duplicate INTERPOLATION."))

(defgeneric interpolation-reverse (interpolation)
  (:documentation "Reverse the path described by INTERPOLATION
in-place."))

(defgeneric interpolation-reversed (interpolation)
  (:method (interpolation)
    (let ((cloned-interpolation (interpolation-clone interpolation)))
      (interpolation-reversed cloned-interpolation)
      cloned-interpolation))
  (:documentation "Duplicate and reverse the INTERPOLATION."))

(defgeneric interpolation-translate (interpolation vector))

(defgeneric interpolation-rotate (interpolation angle))

(defgeneric interpolation-scale (interpolation scale-x scale-y))

;;; Straight lines

(defun make-straight-line ()
  :straight-line)

(defun straight-line-p (value)
  (eq value :straight-line))

(defmethod interpolation-segment ((interpolation (eql :straight-line)) k1 k2 function)
  (declare (ignore interpolation k1 k2 function)))

(defmethod interpolation-normal ((interpolation (eql :straight-line)) k1 k2 side)
  (let* ((x1 (point-x k1))
         (y1 (point-y k1))
         (x2 (point-x k2))
         (y2 (point-y k2))
         (dx (- x2 x1))
         (dy (- y2 y1))
         (dist (sqrt (+ (expt dx 2) (expt dy 2)))))
    (when (plusp dist)
      (if side
          (make-point (/ dx dist)
                      (/ dy dist))
          (make-point (- (/ dx dist))
                      (- (/ dy dist)))))))

(defmethod interpolation-clone ((interpolation (eql :straight-line)))
  (make-straight-line))

(defmethod interpolation-reverse ((interpolation (eql :straight-line)))
  (declare (ignore interpolation)))

(defmethod interpolation-translate ((interpolation (eql :straight-line)) vector)
  (declare (ignore interpolation vector)))

(defmethod interpolation-rotate ((interpolation (eql :straight-line)) angle)
  (declare (ignore interpolation angle)))

(defmethod interpolation-scale ((interpolation (eql :straight-line)) scale-x scale-y)
  (declare (ignore interpolation scale-x scale-y)))

;;; Arc (SVG style)

(defclass arc ()
  ((rx :initarg rx)
   (ry :initarg ry)
   (x-axis-rotation :initarg x-axis-rotation)
   (large-arc-flag :initarg large-arc-flag) ; t = choose the longest arc, nil = choose the smallest arc
   (sweep-flag :initarg sweep-flag))) ; t = arc on the right, nil = arc on the left

(defun make-arc (rx ry &key (x-axis-rotation 0.0) (large-arc-flag nil) (sweep-flag nil))
  (make-instance 'arc
                 'rx rx
                 'ry ry
                 'x-axis-rotation x-axis-rotation
                 'large-arc-flag large-arc-flag
                 'sweep-flag sweep-flag))

(defun svg-arc-parameters/reverse (center rx ry rotation start-angle delta-angle)
  "Conversion from center to endpoint parameterization of SVG arc.

Returns values P1, P2, LARGE-ARC-FLAG-P, SWEEP-FLAG-P."
  (let ((p1 (point-rotate (make-point rx 0) start-angle))
        (p2 (point-rotate (make-point rx 0) (+ start-angle delta-angle))))
    (flet ((transform (p)
             (p+
              (point-rotate
               (p* p 1.0 (/ rx ry))
               rotation)
              center)))
      (values (transform p1) (transform p2)
              (> (abs delta-angle) pi)
              (plusp delta-angle)))))

(defun svg-arc-parameters (p1 p2 rx ry rotation large-arc-flag-p sweep-flag-p)
  "Conversion from endpoint to center parameterization of SVG arc.

Returns values RC, RX, RY, START-ANGLE and DELTA-ANGLE, where RC is
the center of the ellipse, RX and RY are the normalized
radii (needed if scaling was necessary)."
  (when (and (/= rx 0)
             (/= ry 0))
    ;; [SVG] "If rX or rY have negative signs, these are dropped; the
    ;; absolute value is used instead."
    (setf rx (abs rx)
          ry (abs ry))
    ;; normalize boolean value to nil/t
    (setf large-arc-flag-p (when large-arc-flag-p t)
          sweep-flag-p (when sweep-flag-p t))
    ;; rp1 and rp2 are p1 and p2 into the coordinate system such
    ;; that rotation is cancelled and ellipse ratio is 1 (a circle.)
    (let* ((rp1 (p* (point-rotate p1 (- rotation)) 1.0 (/ rx ry)))
           (rp2 (p* (point-rotate p2 (- rotation)) 1.0 (/ rx ry)))
           (rm (point-middle rp1 rp2))
           (drp1 (p- rm rp1))
           (dist (point-norm drp1)))
      (when (plusp dist)
        (let ((diff-sq (- (expt rx 2) (expt dist 2)))
              rc)
          (cond
            ((not (plusp diff-sq))
             ;; a/ scale the arc if it is too small to touch the points
             (setf ry (* dist (/ ry rx))
                   rx dist
                   rc rm))
            (t
             ;; b/ otherwise compute the center of the circle
             (let ((d (/ (sqrt diff-sq) dist)))
               (unless (eq large-arc-flag-p sweep-flag-p)
                 (setf d (- d)))
               (setf rc (make-point (+ (point-x rm) (* (point-y drp1) d))
                                    (- (point-y rm) (* (point-x drp1) d)))))))
          (let* ((start-angle (point-angle (p- rp1 rc)))
                 (end-angle (point-angle (p- rp2 rc)))
                 (delta-angle (- end-angle start-angle)))
            (when (minusp delta-angle)
              (incf delta-angle (* 2 pi)))
            (unless sweep-flag-p
              (decf delta-angle (* 2 pi)))
            (values (point-rotate (p* rc 1.0 (/ ry rx)) rotation) rx ry start-angle delta-angle)))))))

(defmethod interpolation-segment ((interpolation arc) k1 k2 function)
  (let ((rotation (slot-value interpolation 'x-axis-rotation)))
    (multiple-value-bind (rc rx ry start-angle delta-angle)
        (svg-arc-parameters k1 k2
                            (slot-value interpolation 'rx)
                            (slot-value interpolation 'ry)
                            rotation
                            (slot-value interpolation 'large-arc-flag)
                            (slot-value interpolation 'sweep-flag))
      (when rc
        (loop with n = (max 3 (* (max rx ry) (abs delta-angle)))
           for i from 1 below n
           for angle = (+ start-angle (/ (* delta-angle i) n))
           for p = (p+ (point-rotate
                        (p*
                         (make-point (* rx (cos angle))
                                     (* rx (sin angle)))
                         1.0 (/ ry rx))
                        rotation)
                       rc)
           do (funcall function p))))))

(defmethod interpolation-normal ((interpolation arc) k1 k2 side)
  (let ((rotation (slot-value interpolation 'x-axis-rotation)))
    (multiple-value-bind (rc rx ry start-angle delta-angle)
        (svg-arc-parameters k1 k2
                            (slot-value interpolation 'rx)
                            (slot-value interpolation 'ry)
                            rotation
                            (slot-value interpolation 'large-arc-flag)
                            (slot-value interpolation 'sweep-flag))
      (flet ((adjust (normal)
               (let* ((p (point-rotate (p* normal 1.0 (/ ry rx)) rotation))
                      (d (point-norm p)))
                 (when (plusp delta-angle)
                   (setf d (- d)))
                 (make-point (/ (point-x p) d) (/ (point-y p) d)))))
        (when rc
          (let ((end-angle (+ start-angle delta-angle)))
            (adjust (if side
                        (make-point (sin end-angle)
                                    (- (cos end-angle)))
                        (make-point (- (sin start-angle))
                                    (cos start-angle))))))))))

(defmethod interpolation-clone ((interpolation arc))
  (make-arc (slot-value interpolation 'rx)
            (slot-value interpolation 'ry)
            :x-axis-rotation (slot-value interpolation 'x-axis-rotation)
            :large-arc-flag (slot-value interpolation 'large-arc-flag)
            :sweep-flag (slot-value interpolation 'sweep-flag)))

(defmethod interpolation-reverse ((interpolation arc))
  (setf (slot-value interpolation 'sweep-flag)
        (not (slot-value interpolation 'sweep-flag))))

(defmethod interpolation-translate ((interpolation arc) vector)
  (declare (ignore interpolation vector)))

(defmethod interpolation-rotate ((interpolation arc) angle)
  (incf (slot-value interpolation 'x-axis-rotation) angle))

(defmethod interpolation-scale ((interpolation arc) scale-x scale-y)
  ;; FIXME: Return :segment-me if scaling is not possible?
  (assert (and (not (zerop scale-x))
               (= scale-x scale-y)))
  (with-slots (rx ry) interpolation
    (setf rx (* rx scale-x)
          ry (* ry scale-y))))

;;; Catmull-Rom

(defclass catmull-rom ()
  ((head
    :initarg head)
   (control-points
    :initform (make-array 0)
    :initarg control-points)
   (queue
    :initarg queue)))

(defun make-catmull-rom (head control-points queue)
  (make-instance 'catmull-rom
                 'head head
                 'control-points (coerce control-points 'vector)
                 'queue queue))

(defmethod interpolation-segment ((interpolation catmull-rom) k1 k2 function)
  (let* ((control-points (slot-value interpolation 'control-points))
         (points (make-array (+ (length control-points) 4))))
    (replace points control-points :start1 2)
    (setf (aref points 0) (slot-value interpolation 'head)
          (aref points 1) k1
          (aref points (- (length points) 2)) k2
          (aref points (- (length points) 1)) (slot-value interpolation 'queue))
    (labels ((eval-catmull-rom (a b c d p)
               ;; http://www.mvps.org/directx/articles/catmull/
               (* 0.5
                  (+ (* 2 b)
                     (* (+ (- a) c) p)
                     (* (+ (* 2 a) (* -5 b) (* 4 c) (- d)) (expt p 2))
                     (* (+ (- a) (* 3 b) (* -3 c) d) (expt p 3))))))
      (loop for s below (- (length points) 3)
         for a = (aref points (+ s 0)) then b
         for b = (aref points (+ s 1)) then c
         for c = (aref points (+ s 2)) then d
         for d = (aref points (+ s 3))
         do (funcall function b)
         (loop with n = 32
            for i from 1 below n
            for p = (/ (coerce i 'float) n)
            for x = (eval-catmull-rom (point-x a)
                                      (point-x b)
                                      (point-x c)
                                      (point-x d)
                                      p)
            for y = (eval-catmull-rom (point-y a)
                                      (point-y b)
                                      (point-y c)
                                      (point-y d)
                                      p)
            do (funcall function (make-point x y)))
         (funcall function c)))))

(defmethod interpolation-normal ((interpolation catmull-rom) k1 k2 side)
  (with-slots (head control-points queue) interpolation
    (let (a b)
      (if (zerop (length control-points))
          (if side
              (setf a k1
                    b queue)
              (setf a k2
                    b head))
          (if side
              (setf a (aref control-points (1- (length control-points)))
                    b queue)
              (setf a (aref control-points 0)
                    b head)))
      (let* ((x1 (point-x a))
             (y1 (point-y a))
             (x2 (point-x b))
             (y2 (point-y b))
             (dx (- x2 x1))
             (dy (- y2 y1))
             (dist (sqrt (+ (expt dx 2) (expt dy 2)))))
        (when (plusp dist)
          (make-point (/ dx dist)
                      (/ dy dist)))))))

(defmethod interpolation-clone ((interpolation catmull-rom))
  (make-catmull-rom (slot-value interpolation 'head)
                    (copy-seq (slot-value interpolation 'control-points))
                    (slot-value interpolation 'queue)))

(defmethod interpolation-reverse ((interpolation catmull-rom))
  (rotatef (slot-value interpolation 'head)
           (slot-value interpolation 'queue))
  (nreverse (slot-value interpolation 'control-points)))

(defmethod interpolation-translate ((interpolation catmull-rom) vector)
  (with-slots (head control-points queue) interpolation
    (setf head (p+ head vector)
          queue (p+ queue vector))
    (loop for i below (length control-points)
       do (setf (aref control-points i) (p+ (aref control-points i) vector)))))

(defmethod interpolation-rotate ((interpolation catmull-rom) angle)
  (with-slots (head control-points queue) interpolation
    (setf head (point-rotate head angle)
          queue (point-rotate queue angle))
    (loop for i below (length control-points)
       do (setf (aref control-points i) (point-rotate (aref control-points i) angle)))))

(defmethod interpolation-scale ((interpolation catmull-rom) scale-x scale-y)
  (with-slots (head control-points queue) interpolation
    (setf head (p* head scale-x scale-y)
          queue (p* queue scale-x scale-y))
    (loop for i below (length control-points)
       do (setf (aref control-points i) (p* (aref control-points i)
                                                     scale-x scale-y)))))

;;; Bezier curves

;;; [http://www.fho-emden.de/~hoffmann/bezier18122002.pdf]

(defclass bezier ()
  ((control-points
    :initform (make-array 0)
    :initarg control-points)))

(defun make-bezier-curve (control-points)
  (make-instance 'bezier
                 'control-points (make-array (length control-points)
                                             :initial-contents control-points)))

(defun split-bezier (points &optional (position 0.5))
  "Split the Bezier curve described by POINTS at POSITION into
two Bezier curves of the same degree. Returns the curves as 2
values."
  (let* ((size (length points))
         (stack (make-array size))
         (current points))
    (setf (aref stack 0) points)
    (loop for j from 1 below size
       for next-size from (1- size) downto 1
       do (let ((next (make-array next-size)))
            (loop for i below next-size
               for a = (aref current i)
               for b = (aref current (1+ i))
               do (setf (aref next i)
                        (make-point (+ (* (- 1.0 position) (point-x a))
                                       (* position (point-x b)))
                                    (+ (* (- 1.0 position) (point-y a))
                                       (* position (point-y b))))))
            (setf (aref stack j) next
                  current next)))
    (let ((left (make-array (length points)))
          (right (make-array (length points))))
      (loop for i from 0 below size
         for j from (1- size) downto 0
         do (setf (aref left i) (aref (aref stack i) 0)
                  (aref right i) (aref (aref stack j) i)))
      (values left right))))

(defun evaluate-bezier (points position)
  "Evaluate the point at POSITION on the Bezier curve described
by POINTS."
  (let* ((size (length points))
         (temp (make-array (1- size))))
    (loop for current = points then temp
       for i from (length temp) downto 1
       do (loop for j below i
             for a = (aref current j)
             for b = (aref current (1+ j))
             do (setf (aref temp j)
                      (make-point (+ (* (- 1.0 position) (point-x a))
                                     (* position (point-x b)))
                                  (+ (* (- 1.0 position) (point-y a))
                                     (* position (point-y b)))))))
    (let ((p (aref temp 0)))
      (values (point-x p) (point-y p)))))

(defun discrete-bezier-curve (points function
                              &key
                              (include-ends t)
                              (min-subdivide nil)
                              (max-subdivide 10)
                              (distance-tolerance *bezier-distance-tolerance*)
                              (angle-tolerance *bezier-angle-tolerance*))
  "Subdivize Bezier curve up to certain criterions."
  ;; FIXME: Handle cusps correctly!
  (unless min-subdivide
    (setf min-subdivide (floor (log (1+ (length points)) 2))))
  (labels ((norm (a b)
             (sqrt (+ (expt a 2) (expt b 2))))
           (refine-bezier (points depth)
             (let* ((a (aref points 0))
                    (b (aref points (1- (length points))))
                    (middle-straight (point-middle a b)))
               (multiple-value-bind (bx by) (evaluate-bezier points 0.5)
                 (when (or (< depth min-subdivide)
                           (and (<= depth max-subdivide)
                                (or (> (norm (- bx (point-x middle-straight))
                                             (- by (point-y middle-straight)))
                                       distance-tolerance)
                                    (> (abs (- (atan (- by (point-y a)) (- bx (point-x a)))
                                               (atan (- (point-y b) by) (- (point-x b) bx))))
                                       angle-tolerance))))
                   (multiple-value-bind (a b) (split-bezier points 0.5)
                     (refine-bezier a (1+ depth))
                     (funcall function bx by)
                     (refine-bezier b (1+ depth))))))))
    (when include-ends
      (let ((p (aref points 0)))
        (funcall function (point-x p) (point-y p))))
    (refine-bezier points 0)
    (when include-ends
      (let ((p (aref points (1- (length points)))))
        (funcall function (point-x p) (point-y p)))))
  (values))

(defmethod interpolation-segment ((interpolation bezier) k1 k2 function)
  (with-slots (control-points) interpolation
    (let ((points (make-array (+ 2 (length control-points)))))
      (replace points control-points :start1 1)
      (setf (aref points 0) k1
            (aref points (1- (length points))) k2)
      (discrete-bezier-curve points
                             (lambda (x y) (funcall function (make-point x y)))
                             :include-ends nil))))

(defmethod interpolation-normal ((interpolation bezier) k1 k2 side)
  (let ((control-points (slot-value interpolation 'control-points))
        a b)
    (if (zerop (length control-points))
        (if side
            (setf a k1
                  b k2)
            (setf a k2
                  b k1))
        (if side
            (setf a (aref control-points (1- (length control-points)))
                  b k2)
            (setf a (aref control-points 0)
                  b k1)))
    (let* ((x1 (point-x a))
           (y1 (point-y a))
           (x2 (point-x b))
           (y2 (point-y b))
           (dx (- x2 x1))
           (dy (- y2 y1))
           (dist (sqrt (+ (expt dx 2) (expt dy 2)))))
      (when (plusp dist)
        (make-point (/ dx dist)
                    (/ dy dist))))))

(defmethod interpolation-clone ((interpolation bezier))
  (let ((control-points (copy-seq (slot-value interpolation 'control-points))))
    (loop for i below (length control-points)
       do (setf (aref control-points i) (aref control-points i)))
    (make-bezier-curve control-points)))

(defmethod interpolation-reverse ((interpolation bezier))
  (nreverse (slot-value interpolation 'control-points)))

(defmethod interpolation-translate ((interpolation bezier) vector)
  (with-slots (control-points) interpolation
    (loop for i below (length control-points)
       do (setf (aref control-points i) (p+ (aref control-points i) vector)))))

(defmethod interpolation-rotate ((interpolation bezier) angle)
  (with-slots (control-points) interpolation
    (loop for i below (length control-points)
       do (setf (aref control-points i) (point-rotate (aref control-points i) angle)))))

(defmethod interpolation-scale ((interpolation bezier) scale-x scale-y)
  (with-slots (control-points) interpolation
    (loop for i below (length control-points)
       do (setf (aref control-points i) (p* (aref control-points i)
                                                     scale-x scale-y)))))

;;;--[ Building paths ]------------------------------------------------------

(defun make-discrete-path (path)
  "Construct a path with only straight lines."
  (let ((result (create-path (path-type path)))
        (knots (path-knots path))
        (interpolations (path-interpolations path)))
    (when (plusp (length knots))
      ;; nicer, but slower too.. (But not profiled. Premature optimization?)
      #+nil(loop with iterator = (path-iterator-segmented path)
              for (interpolation knot end-p) = (multiple-value-list (path-iterator-next iterator))
              do (path-extend result interpolation knot)
              until end-p)
      (path-reset result (aref knots 0))
      (loop
         for i below (1- (length knots))
         for k1 = (aref knots i)
         for k2 = (aref knots (1+ i))
         for interpolation = (aref interpolations (1+ i))
         do (interpolation-segment interpolation k1 k2
                                   (lambda (knot)
                                     (path-extend result
                                                  (make-straight-line)
                                                  knot)))
         do (path-extend result (make-straight-line) k2)
         finally (unless (eq (path-type path) :open-polyline)
                   (interpolation-segment (aref interpolations 0) k2 (aref knots 0)
                                          (lambda (knot)
                                            (path-extend result
                                                         (make-straight-line)
                                                         knot))))))
    result))

(defun make-circle-path (cx cy radius &optional (radius-y radius) (x-axis-rotation 0.0))
  "Construct a path to represent a circle centered at CX,CY of
the specified RADIUS."
  ;; Note: We represent the circle with 2 arcs
  (let ((path (create-path :polygon)))
    (setf radius (abs radius)
          radius-y (abs radius-y))
    (when (= radius radius-y)
      (setf x-axis-rotation 0.0))
    (when (and (plusp radius) (plusp radius-y))
      (let* ((center (make-point cx cy))
             (p (point-rotate (make-point radius 0) x-axis-rotation))
             (left (p+ center p))
             (right (p- center p)))
        (path-extend path (make-arc radius radius-y :x-axis-rotation x-axis-rotation) left)
        (path-extend path (make-arc radius radius-y :x-axis-rotation x-axis-rotation) right)))
    path))

(defun make-rectangle-path (x1 y1 x2 y2
                            &key (round nil) (round-x nil) (round-y nil))
  ;; FIXME: Instead: center + width + height + rotation ?
  ;; FIXME: Round corners? (rx, ry)
  (when (> x1 x2)
    (rotatef x1 x2))
  (when (> y1 y2)
    (rotatef y1 y2))
  (let ((path (create-path :closed-polyline))
        (round-x (or round-x round))
        (round-y (or round-y round)))
    (cond
      ((and round-x (plusp round-x)
            round-y (plusp round-y))
       (path-reset path (make-point (+ x1 round-x) y1))
       (path-extend path (make-arc round-x round-y) (make-point x1 (+ y1 round-y)))
       (path-extend path (make-straight-line) (make-point x1 (- y2 round-y)))
       (path-extend path (make-arc round-x round-y) (make-point (+ x1 round-x) y2))
       (path-extend path (make-straight-line) (make-point (- x2 round-x) y2))
       (path-extend path (make-arc round-x round-y) (make-point x2 (- y2 round-y)))
       (path-extend path (make-straight-line) (make-point x2 (+ y1 round-y)))
       (path-extend path (make-arc round-x round-y) (make-point (- x2 round-x) y1)))
      (t
       (path-reset path (make-point x1 y1))
       (path-extend path (make-straight-line) (make-point x1 y2))
       (path-extend path (make-straight-line) (make-point x2 y2))
       (path-extend path (make-straight-line) (make-point x2 y1))))
    path))

(defun make-rectangle-path/center (x y dx dy &rest args)
  (apply #'make-rectangle-path (- x dx) (- y dy) (+ x dx) (+ y dy) args))

(defun make-regular-polygon-path (x y radius sides &optional (start-angle 0.0))
  (let ((path (create-path :closed-polyline)))
    (loop for i below sides
         for angle = (+ start-angle (/ (* i 2 pi) sides))
         do (path-extend path (make-straight-line)
                         (make-point (+ x (* (cos angle) radius))
                                     (- y (* (sin angle) radius)))))
    path))

(defun make-simple-path (points &optional (type :open-polyline))
  "Create a path with only straight line, by specifying only knots."
  (let ((path (create-path type)))
    (dolist (point points)
      (path-extend path (make-straight-line) point))
    path))

;;;--[ Transformations ]-----------------------------------------------------

(defmacro define-for-multiple-paths (name-multiple name-single &optional documentation)
  "Define a new function named by NAME-MULTIPLE which accepts
either a single path or a list of paths as input from a function
named by NAME-SINGLE accepting only a single path and producing a
list of paths."
  `(defun ,name-multiple (paths &rest args)
     ,@(when documentation (list documentation))
     (loop for path in (if (listp paths) paths (list paths))
        nconc (apply #',name-single path args))))

;;; Stroke

(defun stroke-path/1 (path thickness
                      &key (caps :butt) (joint :none) (inner-joint :none)
                      assume-type)
  "Stroke the path."
  (setf thickness (abs thickness))
  (let ((half-thickness (/ thickness 2.0))
        target)
    ;; TARGET is the path updated by the function LINE-TO and
    ;; EXTEND-TO below.
    (labels ((filter-interpolation (interpolation)
               ;; We handle only straight-line and arc of circle. The
               ;; rest will be segmented.
               (not (or (straight-line-p interpolation)
                        (and (typep interpolation 'arc)
                             (= (slot-value interpolation 'rx)
                                (slot-value interpolation 'ry))))))
             (det (a b c d)
               (- (* a d) (* b c)))
             (arc (model)
               "Make a new arc similar to MODEL but with a radius
                updated to match the stroke."
               (assert (= (slot-value model 'rx)
                          (slot-value model 'ry)))
               (let ((shift (if (slot-value model 'sweep-flag)
                                (- half-thickness)
                                half-thickness)))
                 (make-arc (+ (slot-value model 'rx) shift)
                           (+ (slot-value model 'ry) shift)
                           :sweep-flag (slot-value model 'sweep-flag)
                           :large-arc-flag (slot-value model 'large-arc-flag))))
             (line-to (p)
               "Extend the path to knot P with a straight line."
               (path-extend target (make-straight-line) p))
             (extend-to (i p)
               "EXtend the path to knot P with the given interpolation."
               (path-extend target i p))
             (do-single (k1)
               "Produce the resulting path when the input path
                contains a single knot."
               (ecase caps
                 (:butt
                  nil)
                 (:square
                  (path-replace target
                                (make-rectangle-path/center (point-x k1)
                                                            (point-y k1)
                                                            half-thickness
                                                            half-thickness)))
                 (:round
                  (path-replace target
                                (make-circle-path (point-x k1)
                                                  (point-y k1)
                                                  half-thickness)))))
             (do-first (k1 i2 k2)
               "Process the first interpolation."
               (let* ((normal (interpolation-normal i2 k1 k2 nil))
                      (n (p* normal half-thickness))
                      (d (point-rotate n (/ pi 2))))
                 (ecase caps
                   (:butt
                    (line-to (p- k1 d)))
                   (:square
                    (line-to (p+ (p+ k1 d) n))
                    (line-to (p+ (p- k1 d) n))
                    (unless (straight-line-p i2)
                      (line-to (p- k1 d))))
                   (:round
                    (extend-to (make-arc half-thickness half-thickness) (p- k1 d))))))
             (do-last (k1 i2 k2)
               "Process the last interpolation."
               (let* ((normal (interpolation-normal i2 k1 k2 t))
                      (d (p* (point-rotate normal (/ pi 2)) half-thickness)))
                 (cond
                   ((typep i2 'arc)
                    (extend-to (arc i2) (p+ k2 d)))
                   ((straight-line-p i2)
                    (unless (eq caps :square)
                      (line-to (p+ k2 d))))
                   (t
                    (error "unexpected interpolation")))))
             (do-segment (k1 i2 k2 i3 k3)
               "Process intermediate interpolation."
               (let* ((normal-a (interpolation-normal i2 k1 k2 t))
                      (normal-b (interpolation-normal i3 k2 k3 nil))
                      (outer-p (plusp (det (point-x normal-a) (point-y normal-a)
                                           (point-x normal-b) (point-y normal-b))))
                      (d-a (p* (point-rotate normal-a (/ pi 2)) half-thickness))
                      (d-b (p* (point-rotate normal-b (/ pi -2)) half-thickness)))
                 (cond
                   ((and (not outer-p)
                         (eq inner-joint :miter)
                         (straight-line-p i2)
                         (straight-line-p i3))
                    ;; Miter inner joint between 2 straight lines
                    (multiple-value-bind (xi yi)
                        (line-intersection/delta
                         (point-x (p+ k2 d-a)) (point-y (p+ k2 d-a))
                         (point-x normal-a) (point-y normal-a)
                         (point-x (p+ k2 d-b)) (point-y (p+ k2 d-b))
                         (point-x normal-b) (point-y normal-b))
                      (cond
                        ((and xi
                              (plusp (+ (* (- xi (point-x k1))
                                           (point-x normal-a))
                                        (* (- yi (point-y k1))
                                           (point-y normal-a))))
                              (plusp (+ (* (- xi (point-x k3))
                                           (point-x normal-b))
                                        (* (- yi (point-y k3))
                                           (point-y normal-b)))))
                         ;; ok, intersection point
                         ;; is behind segments
                         ;; ends
                         (extend-to (make-straight-line) (make-point xi yi)))
                        (t
                         ;; revert to basic joint
                         (line-to (p+ k2 d-a))
                         (line-to (p+ k2 d-b))))))
                   ((and outer-p
                         (eq joint :miter)
                         (straight-line-p i2)
                         (straight-line-p i3))
                    ;; Miter outer joint between 2 straight lines
                    (multiple-value-bind (xi yi)
                        (line-intersection/delta
                         (point-x (p+ k2 d-a)) (point-y (p+ k2 d-a))
                         (point-x normal-a) (point-y normal-a)
                         (point-x (p+ k2 d-b)) (point-y (p+ k2 d-b))
                         (point-x normal-b) (point-y normal-b))
                      (let ((i (make-point xi yi)))
                        (cond
                          ((and xi
                                (<= (point-distance i k2)
                                    (* half-thickness *miter-limit*)))
                           (line-to (make-point xi yi)))
                          (t
                           ;; FIXME: Ugh. My math skill show its
                           ;; limits. This is probably possible to
                           ;; compute the same thing with less steps.
                           (let* ((p (p+ k2 (point-middle d-a d-b)))
                                  (a (point-distance (p+ k2 d-a) i))
                                  (b (- (* half-thickness *miter-limit*)
                                        (point-distance k2 p)))
                                  (c (point-distance p i))
                                  (d (/ (* a b) c))
                                  (p1 (p+ (p+ k2 d-a) (p* normal-a d)))
                                  (p2 (p+ (p+ k2 d-b) (p* normal-b d))))
                             (line-to p1)
                             (line-to p2)))))))
                   (t
                    (extend-to (if (typep i2 'arc)
                                   (arc i2)
                                   (make-straight-line))
                               (p+ k2 d-a))
                    ;; joint
                    (if outer-p
                        (ecase joint
                          ((:none :miter)
                           (line-to (p+ k2 d-b)))
                          (:round
                           (extend-to (make-arc half-thickness half-thickness
                                                :sweep-flag nil)
                                      (p+ k2 d-b))))
                        (ecase inner-joint
                          ((:none :miter)
                           (line-to (p+ k2 d-b)))
                          (:round
                           (extend-to (make-arc half-thickness half-thickness
                                                :sweep-flag t)
                                      (p+ k2 d-b)))))))))
             (do-contour-half (path new-target first-half-p)
               (setf target new-target)
               (let ((iterator (filter-distinct (path-iterator-segmented path #'filter-interpolation)
                                                t)))
                 (flet ((next ()
                          (path-iterator-next iterator)))
                   (multiple-value-bind (i1 k1 e1) (next)
                     (when k1
                       (cond
                         (e1
                          (when first-half-p
                            (do-single k1)))
                         (t
                          ;; at least 2 knots
                          (multiple-value-bind (i2 k2 e2) (next)
                            (do-first k1 i2 k2)
                            ;; rest of the path
                            (unless e2
                              (loop
                                 (multiple-value-bind (i3 k3 e3) (next)
                                   (do-segment k1 i2 k2 i3 k3)
                                   (shiftf i1 i2 i3)
                                   (shiftf k1 k2 k3)
                                   (when e3
                                     (return)))))
                            (do-last k1 i2 k2)))))))))
             (do-contour-polygon (path new-target first-p)
               (setf target new-target)
               (let ((iterator (filter-distinct (path-iterator-segmented path #'filter-interpolation))))
                 (flet ((next ()
                          (path-iterator-next iterator)))
                   (multiple-value-bind (i1 k1 e1) (next)
                     (when k1
                       (cond
                         (e1
                          (when first-p
                            (do-single k1)))
                         (t
                          ;; at least 2 knots
                          (multiple-value-bind (i2 k2 e2) (next)
                            ;; rest of the path
                            (let (extra-iteration)
                              (when e2
                                (setf extra-iteration 2))
                              (loop
                                 (multiple-value-bind (i3 k3 e3) (next)
                                   (when (and extra-iteration (zerop extra-iteration))
                                     (return))
                                   (do-segment k1 i2 k2 i3 k3)
                                   (shiftf i1 i2 i3)
                                   (shiftf k1 k2 k3)
                                   (cond
                                     (extra-iteration
                                      (decf extra-iteration))
                                     (e3
                                      (setf extra-iteration 2)))))))))))))))
      (when (plusp half-thickness)
        (ecase (or assume-type (path-type path))
          (:open-polyline
           (let ((result (create-path :polygon)))
             (do-contour-half path result t)
             (do-contour-half (path-reversed path) result nil)
             (list result)))
          (:closed-polyline
           (let ((result-a (create-path :polygon))
                 (result-b (create-path :polygon)))
             ;; FIXME: What happen for single knot path?
             (do-contour-polygon path result-a t)
             (do-contour-polygon (path-reversed path) result-b nil)
             (list result-a result-b)))
          (:polygon
           (let ((result (create-path :polygon)))
             (do-contour-polygon path result t)
             (list result))))))))

(define-for-multiple-paths stroke-path stroke-path/1)

;;; Dash

(defun dash-path/1 (path sizes &key (toggle-p nil) (cycle-index 0))
  "Dash path. If TOGGLE-P is true, segments of odd indices are
kept, while if TOGGLE-P is false, segments of even indices are
kept. CYCLE indicate where to cycle the SIZES once the end is
reached."
  (assert (<= 0 cycle-index (1- (length sizes)))
          (cycle-index) "Invalid cycle index")
  (assert (loop for size across sizes never (minusp size))
          (sizes) "All sizes must be non-negative.")
  (assert (loop for size across sizes thereis (plusp size))
          (sizes) "At least one size must be positive.")
  (flet ((interpolation-filter (interpolation)
           (or (not (typep interpolation 'arc))
               (/= (slot-value interpolation 'rx)
                   (slot-value interpolation 'ry)))))
    (let (result
          (current (create-path :open-polyline))
          (current-length 0.0)
          (toggle (not toggle-p))
          (index 0)
          (size (aref sizes 0))
          (iterator (path-iterator-segmented path #'interpolation-filter)))
      (flet ((flush ()
               (when toggle
                 (push current result))
               (setf toggle (not toggle))
               (setf current (create-path :open-polyline)
                     current-length 0.0)
               (incf index)
               (when (= index (length sizes))
                 (setf index cycle-index))
               (setf size (aref sizes index)))
             (extend (interpolation knot length)
               (path-extend current interpolation knot)
               (incf current-length length)))
        (loop
           for previous-knot = nil then knot
           for stop-p = nil then end-p
           for (interpolation knot end-p) = (multiple-value-list (path-iterator-next iterator))
           if (not previous-knot)
           do (path-reset current knot)
           else
           do (etypecase interpolation
                ((eql :straight-line)
                 (let* ((delta (p- knot previous-knot))
                        (length (point-norm delta))
                        (pos 0.0))
                   (loop
                      (let ((missing (- size current-length))
                            (available (- length pos)))
                        (when (> missing available)
                          (extend (make-straight-line) knot available)
                          (return))
                        (incf pos missing)
                        (let ((end (p+ previous-knot (p* delta (/ pos length)))))
                          (extend (make-straight-line) end missing)
                          (flush)
                          (path-reset current end))))))
                (arc
                 (with-slots (rx ry x-axis-rotation large-arc-flag sweep-flag) interpolation
                   (assert (= rx ry))
                   (multiple-value-bind (rc nrx nry start-angle delta-angle)
                       (svg-arc-parameters previous-knot knot rx ry
                                           x-axis-rotation
                                           large-arc-flag
                                           sweep-flag)
                     (let* ((length (* (abs delta-angle) nrx))
                            (pos 0.0))
                       (loop
                          (let ((missing (- size current-length))
                                (available (- length pos)))
                            (when (> missing available)
                              (extend (make-arc nrx nry
                                                :x-axis-rotation x-axis-rotation
                                                :large-arc-flag (>= (/ available nrx) pi)
                                                :sweep-flag sweep-flag)
                                      knot
                                      available)
                              (return))
                            (incf pos missing)
                            (let ((end (p+
                                        (point-rotate (make-point nrx 0)
                                                      (+ x-axis-rotation
                                                         (if (plusp delta-angle)
                                                             (+ start-angle (/ pos nrx))
                                                             (- start-angle (/ pos nrx)))))
                                        rc)))
                              (extend (make-arc nrx nry
                                                :x-axis-rotation x-axis-rotation
                                                :large-arc-flag (>= (/ missing nrx) pi)
                                                :sweep-flag sweep-flag)
                                      end
                                      missing)
                              (flush)
                              (path-reset current end)))))))))
           until (if (eq (path-type path) :open-polyline) end-p stop-p))
        (flush))
      (nreverse result))))

(define-for-multiple-paths dash-path dash-path/1)

;;; Clip path

(defun clip-path/1 (path x y dx dy)
  (let (result
        (current (create-path (path-type path)))
        (iterator (path-iterator-segmented path)))
    (labels ((next ()
               (path-iterator-next iterator))
             (det (a b c d)
               (- (* a d) (* b c)))
             (inside-p (p)
               (plusp (det (- (point-x p) x)
                           (- (point-y p) y)
                           dx dy)))
             (clip-left (k1 k2)
               (let ((k1-inside-p (when (inside-p k1) t))
                     (k2-inside-p (when (inside-p k2) t)))
                 (when k1-inside-p
                   (path-extend current (make-straight-line) k1))
                 (when (not (eq k1-inside-p k2-inside-p))
                   (multiple-value-bind (xi yi)
                       (line-intersection/delta x y dx dy
                                                (point-x k1) (point-y k1)
                                                (- (point-x k2) (point-x k1))
                                                (- (point-y k2) (point-y k1)))
                     (when xi
                       (path-extend current (make-straight-line) (make-point xi yi))))))))
      (multiple-value-bind (i1 k1 e1) (next)
        (let ((first-knot k1))
          (when k1
            (cond
              (e1
               (when (inside-p k1)
                 (path-reset current k1)))
              (t
               (loop
                  (multiple-value-bind (i2 k2 e2) (next)
                    (clip-left k1 k2)
                    (when e2
                      (if (eq (path-type path) :open-polyline)
                          (when (inside-p k2)
                            (path-extend current (make-straight-line) k2))
                          (clip-left k2 first-knot))
                      (return))
                    (setf i1 i2)
                    (setf k1 k2)))))))))
    (push current result)
    result))

(define-for-multiple-paths clip-path clip-path/1)

(defun clip-path/path/1 (path limit)
  (let ((iterator (filter-distinct (path-iterator-segmented limit)))
        (result (list path)))
    (multiple-value-bind (i1 k1 e1) (path-iterator-next iterator)
      (declare (ignore i1))
      (when (and k1 (not e1))
        (let ((stop-p nil))
          (loop
             (multiple-value-bind (i2 k2 e2) (path-iterator-next iterator)
               (declare (ignore i2))
               (setq result (loop for path in result
                               nconc (clip-path path
                                                (point-x k1) (point-y k1)
                                                (point-x (p- k2 k1)) (point-y (p- k2 k1)))))
               (when stop-p
                 (return result))
               (when e2
                 (setf stop-p t))
               (setf k1 k2))))))))

(define-for-multiple-paths clip-path/path clip-path/path/1)

#|
;;; Round path

(defun round-path/1 (path &optional max-radius)
  (declare (ignore max-radius))
  (list path))

(define-for-multiple-paths round-path round-path/1)
|#
