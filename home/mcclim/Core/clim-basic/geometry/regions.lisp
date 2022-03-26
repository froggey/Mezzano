;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ----------------------------------------------------------------------------
;;;     Title: The CLIM Region Datatype
;;;   Created: 1998-12-02 19:26
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;;       $Id: regions.lisp,v 1.39 2009/06/03 20:33:16 ahefner Exp $
;;; ----------------------------------------------------------------------------
;;;  (c) copyright 1998,1999,2001 by Gilbert Baumann
;;;  (c) copyright 2001 by Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;  (c) copyright 2014 by Robert Strandh (robert.strandh@gmail.com)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; ---- TODO ------------------------------------------------------------------

;;; - ellipses: The intersection of two ellipses is there, but
;;;   handling the start/end angle is not implemented.

;;; - provide better (faster) implementations for REGION-EQUAL,
;;;   REGION-CONTAINS-REGION-P, and REGION-INTERSECTS-REGION-P.

;;; - Compute a union/intersection/difference of an union of polygon
;;;   vs another polygon or union of polygons directly via POLYGON-OP.

;;; - STANDARD-REGION-UNION should either become a subclass
;;;   'STANDARD-DISJUNCT-REGION-UNION' or a flag. Some set operations
;;;   could take advantage out the information, if the subregions of
;;;   an union are disjunct.

;;; - provide sensible PRINT-OBJECT methods.

;;; - while you are are at it; provide a reasonable fast vertical scan
;;;   routine.  polygons should make use of the sweep line algorithm.

;;; - MAKE-POLY{LINE,GON} should canonise its arguments; no edges of
;;;   length 0 and no co-linear vertexes. Maybe: canonise rectangles?
;;;   Also a polygon of less than three vertexes is to be considered
;;;   empty aka +nowhere+.

(in-package :clim-internals)

;;;; Design <-> Region Equivalences

;;; As Gilbert points in his notes, transparent ink is in every
;;; respect interchangable with the nowhere region, and likewise
;;; foreground ink is interchangable with the everywhere region.
;;; By defining the following mixins and adding them to the
;;; appropriate ink/region class pairs, we can reduce the number
;;; of methods necessary (in design.lisp).

(defclass everywhere-mixin () ())
(defclass nowhere-mixin    () ())

(defclass nowhere-region    (region nowhere-mixin)    ())
(defclass everywhere-region (region everywhere-mixin) ())

(defconstant +everywhere+ (make-instance 'everywhere-region))
(defconstant +nowhere+    (make-instance 'nowhere-region))

;;; Unbounded regions have very general (and very mundane!)
;;; methods. They are all defined here.
;;;
;;; Other region protocol methods should specialize at least on
;;; bounded-rectangle to prevent superseding methods for unbounded
;;; regions. For instance the following would supersede
;;; implementation for an unbounded region:
;;;
;;;    (defmethod region-difference ((a rectangle) (b region))
;;;      (make-instance 'standard-region-difference a b))
;;;
(macrolet
    ((def-method (name e-vs-e e-vs-n e-vs-r
                       n-vs-e n-vs-n n-vs-r
                       r-vs-e r-vs-n)
       (let ((bodies (list e-vs-e e-vs-n e-vs-r
                           n-vs-e n-vs-n n-vs-r
                           r-vs-e r-vs-n)))
         (collect (methods)
           (dolist (a '(everywhere-region nowhere-region region))
             (dolist (b '(everywhere-region nowhere-region region))
               (methods
                `(defmethod ,name ((a ,a) (b ,b))
                   (declare (ignorable a b))
                   ,(pop bodies)))))
           `(progn ,@(butlast (methods)))))))
  (def-method region-intersects-region-p t nil t nil nil nil t nil)
  (def-method region-contains-region-p t t t nil nil nil nil t)
  (def-method region-equal t nil nil nil t nil nil nil)
  (def-method region-union a a a b b b b a)
  (def-method region-intersection b b b a a a a b)
  ;; We don't support unbounded regions which are not +everywhere+ or
  ;; +nowhere+ (that would complicate the geometry module). If we
  ;; decide otherwise don't use standard-region-difference because it
  ;; is a subclass of a bounded-rectangle so it can't represent
  ;; unbounded region.  -- jd 2019-09-10
  (def-method region-difference
    +nowhere+ a (error "Unsupported unbounded region operation.")
    a a a
    +nowhere+ a))

(defmethod region-contains-position-p ((region everywhere-region) x y)
  (declare (ignore region x y))
  t)

(defmethod region-contains-position-p ((region nowhere-region) x y)
  (declare (ignore region x y))
  nil)

;;; 2.5.1.2 Composition of CLIM Regions

(defclass standard-region-union (region-set)
  ((regions :initarg :regions :reader standard-region-set-regions)))

(defclass standard-region-intersection (region-set)
  ((regions :initarg :regions :reader standard-region-set-regions)))

(defclass standard-region-difference (region-set)
  ((a :initarg :a :reader standard-region-difference-a)
   (b :initarg :b :reader standard-region-difference-b)))

;;; -- 2.5.2 CLIM Point Objects ----------------------------------------------

(defclass standard-point (point)
  ((x :type coordinate :initarg :x)
   (y :type coordinate :initarg :y)))

(defun make-point (x y)
  (make-instance 'standard-point
    :x (coordinate x)
    :y (coordinate y)))

(defmethod slots-for-pprint-object append ((object standard-point))
  '(x y))

(defmethod print-object ((region standard-point) sink)
  (maybe-print-readably (region sink)
    (print-unreadable-object (region sink :identity nil :type t)
      (with-slots (x y) region
        (format sink "~S ~S" x y)))))

;;; Point protocol: point-position

(defmethod point-position ((region standard-point))
  (with-slots (x y) region
    (values x y)))

(defmethod point-x ((region point))
  (nth-value 0 (point-position region)))

(defmethod point-y ((region point))
  (nth-value 1 (point-position region)))

;;; -- 2.5.3 Polygons and Polylines in CLIM ----------------------------------

(defclass cached-polygon-bbox-mixin ()
  ((bbox :reader bounding-rectangle)))

;; Protocol:
(defclass standard-polyline (cached-polygon-bbox-mixin polyline)
  ((points :initarg :points :reader polygon-points)
   (closed :initarg :closed)))

(defclass standard-polygon (cached-polygon-bbox-mixin polygon)
  ((points :initarg :points :reader polygon-points)))

(defmethod slots-for-pprint-object append ((object standard-polyline))
  '(points closed))

(defmethod print-object ((region standard-polyline) sink)
  (maybe-print-readably (region sink)
    (print-unreadable-object (region sink :identity t :type t))))

;;; -- 2.5.3.1 Constructors for CLIM Polygons and Polylines  -----------------

(defun make-polyline (point-seq &key closed)
  (assert (every #'pointp point-seq))
  (setq point-seq (remove-duplicated-points point-seq closed))
  (if (< (length point-seq) 2)
      +nowhere+
      (make-instance 'standard-polyline :points point-seq :closed closed)))

(defun make-polyline* (coord-seq &key closed)
  (make-polyline (coord-seq->point-seq coord-seq) :closed closed))

(defun make-polygon (point-seq)
  (assert (every #'pointp point-seq))
  (setq point-seq (remove-duplicated-points point-seq t))
  (if (< (length point-seq) 3)
      +nowhere+
      (make-instance 'standard-polygon :points point-seq)))

(defun make-polygon* (coord-seq)
  (make-polygon (coord-seq->point-seq coord-seq)))

(defmethod map-over-polygon-coordinates (fun (region standard-polygon))
  (with-slots (points) region
    (mapc (lambda (p) (funcall fun (point-x p) (point-y p))) points)))

(defmethod map-over-polygon-segments (fun (region standard-polygon))
  (with-slots (points) region
    (do ((q points (cdr q)))
        ((null (cdr q))
         (funcall fun
                  (point-x (car q)) (point-y (car q))
                  (point-x (car points)) (point-y (car points))))
      (funcall fun
               (point-x (car q)) (point-y (car q))
               (point-x (cadr q)) (point-y (cadr q))))))

(defmethod map-over-polygon-coordinates (fun (region standard-polyline))
  (with-slots (points) region
    (mapc (lambda (p) (funcall fun (point-x p) (point-y p))) points)))

(defmethod map-over-polygon-segments (fun (region standard-polyline))
  (with-slots (points closed) region
    (do ((q points (cdr q)))
        ((null (cdr q))
         (when closed
           (funcall fun
                    (point-x (car q)) (point-y (car q))
                    (point-x (car points)) (point-y (car points)))))
      (funcall fun (point-x (car q)) (point-y (car q))
               (point-x (cadr q)) (point-y (cadr q))))))

(defmethod polyline-closed ((region standard-polyline))
  (with-slots (closed) region
    closed))


;;; -- 2.5.4 Lines in CLIM ---------------------------------------------------

;;; Line protocol: line-start-point* line-end-point*

(defclass standard-line (line)
  ((x1 :type coordinate :initarg :x1)
   (y1 :type coordinate :initarg :y1)
   (x2 :type coordinate :initarg :x2)
   (y2 :type coordinate :initarg :y2)))

(defun make-line (start-point end-point)
  (make-line* (point-x start-point) (point-y start-point)
              (point-x end-point) (point-y end-point)))

(defun make-line* (start-x start-y end-x end-y)
  (setf start-x (coordinate start-x)
        start-y (coordinate start-y)
        end-x (coordinate end-x)
        end-y (coordinate end-y))
  (if (and (coordinate= start-x end-x)
           (coordinate= start-y end-y))
      +nowhere+
    (make-instance 'standard-line :x1 start-x :y1 start-y :x2 end-x :y2 end-y)))

(defmethod line-start-point* ((line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (values x1 y1)))

(defmethod line-end-point* ((line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (values x2 y2)))

(defmethod line-start-point ((line line))
  (multiple-value-bind (x y) (line-start-point* line)
    (make-point x y)))

(defmethod line-end-point ((line line))
  (multiple-value-bind (x y) (line-end-point* line)
    (make-point x y)))

;;; polyline protocol for standard-line's:

(defmethod polygon-points ((line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (list (make-point x1 y1) (make-point x2 y2))))

(defmethod map-over-polygon-coordinates (fun (line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (funcall fun x1 y1)
    (funcall fun x2 y2)))

(defmethod map-over-polygon-segments (fun (line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (funcall fun x1 y1 x2 y2)))

(defmethod polyline-closed ((line standard-line))
  nil)

(defmethod slots-for-pprint-object append ((object standard-line))
  '(x1 y1 x2 y2))

(defmethod print-object ((region standard-line) sink)
  (maybe-print-readably (region sink)
    (print-unreadable-object (region sink :identity nil :type t)
         (with-slots (x1 y1 x2 y2) region
           (format sink "~D ~D ~D ~D" x1 y1 x2 y2)))))

;;; -- 2.5.5 Rectangles in CLIM ----------------------------------------------

;;; protocol:
;;;     rectangle-edges*

(defclass standard-rectangle (rectangle)
  ((coordinates :initform (make-array 4 :element-type 'coordinate))))

(defmethod initialize-instance :after ((obj standard-rectangle)
                                       &key (x1 0.0d0) (y1 0.0d0)
                                       (x2 0.0d0) (y2 0.0d0))
  (let ((coords (slot-value obj 'coordinates)))
    (declare (type standard-rectangle-coordinate-vector coords))
    (setf (aref coords 0) x1)
    (setf (aref coords 1) y1)
    (setf (aref coords 2) x2)
    (setf (aref coords 3) y2)))

(defmacro with-standard-rectangle ((x1 y1 x2 y2) rectangle &body body)
  (with-gensyms (coords)
    `(let ((,coords (slot-value ,rectangle 'coordinates)))
       (declare (type standard-rectangle-coordinate-vector ,coords))
       (let ((,x1 (aref ,coords 0))
             (,y1 (aref ,coords 1))
             (,x2 (aref ,coords 2))
             (,y2 (aref ,coords 3)))
         (declare (type coordinate ,x1 ,y1 ,x2 ,y2))
         ,@body))))

(defmacro with-standard-rectangle* ((&key x1 y1 x2 y2) rectangle &body body)
  (with-gensyms (coords)
    `(let ((,coords (slot-value ,rectangle 'coordinates)))
       (declare (type standard-rectangle-coordinate-vector ,coords))
       (let (,@(and x1 `((,x1 (aref ,coords 0))))
             ,@(and y1 `((,y1 (aref ,coords 1))))
             ,@(and x2 `((,x2 (aref ,coords 2))))
             ,@(and y2 `((,y2 (aref ,coords 3)))))
         (declare (type coordinate
                        ,@(and x1 `(,x1))
                        ,@(and y1 `(,y1))
                        ,@(and x2 `(,x2))
                        ,@(and y2 `(,y2))))
         ,@body))))

(defun make-rectangle (point1 point2)
  (make-rectangle* (point-x point1) (point-y point1)
                   (point-x point2) (point-y point2)))

(defun make-rectangle* (x1 y1 x2 y2)
  (let ((x1 (coordinate x1))
        (y1 (coordinate y1))
        (x2 (coordinate x2))
        (y2 (coordinate y2)))
    (multiple-value-bind (x1 x2)
        (cond ((= x1 x2) (return-from make-rectangle* +nowhere+))
              ((< x1 x2) (values x1 x2))
              (t         (values x2 x1)))
      (multiple-value-bind (y1 y2)
          (cond ((= y1 y2) (return-from make-rectangle* +nowhere+))
                ((< y1 y2) (values y1 y2))
                (t         (values y2 y1)))
        ;; XXX: This seems to not be right. -- jd 2019-09-30
        (make-instance 'standard-bounding-rectangle :x1 x1 :y1 y1 :x2 x2 :y2 y2)))))

(defmethod rectangle-edges* ((rect standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2)
      rect
    (values x1 y1 x2 y2)))

;;; standard-rectangles are immutable and all that, but we still need
;;; to set their positions and dimensions (in output recording)
(defgeneric* (setf rectangle-edges*) (x1 y1 x2 y2 rectangle))

(defmethod* (setf rectangle-edges*)
  (x1 y1 x2 y2 (rectangle standard-rectangle))
  (let ((coords (slot-value rectangle 'coordinates)))
    (declare (type standard-rectangle-coordinate-vector coords))
    (setf (aref coords 0) x1)
    (setf (aref coords 1) y1)
    (setf (aref coords 2) x2)
    (setf (aref coords 3) y2))
  (values x1 y1 x2 y2))

(defmethod rectangle-min-point ((rect rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rect)
    (declare (ignore x2 y2))
    (make-point x1 y1)))

(defmethod rectangle-min-point ((rect standard-rectangle))
  (with-standard-rectangle* (:x1 x1 :y1 y1)
      rect
    (make-point x1 y1)))

(defmethod rectangle-max-point ((rect rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rect)
    (declare (ignore x1 y1))
    (make-point x2 y2)))

(defmethod rectangle-max-point ((rect standard-rectangle))
  (with-standard-rectangle* (:x2 x2 :y2 y2)
      rect
    (make-point x2 y2)))

(defmethod rectangle-min-x ((rect rectangle))
  (nth-value 0 (rectangle-edges* rect)))

(defmethod rectangle-min-x ((rect standard-rectangle))
  (with-standard-rectangle* (:x1 x1)
      rect
    x1))

(defmethod rectangle-min-y ((rect rectangle))
  (nth-value 1 (rectangle-edges* rect)))

(defmethod rectangle-min-y ((rect standard-rectangle))
  (with-standard-rectangle* (:y1 y1)
      rect
    y1))

(defmethod rectangle-max-x ((rect rectangle))
  (nth-value 2 (rectangle-edges* rect)))

(defmethod rectangle-max-x ((rect standard-rectangle))
  (with-standard-rectangle* (:x2 x2)
      rect
    x2))

(defmethod rectangle-max-y ((rect rectangle))
  (nth-value 3 (rectangle-edges* rect)))

(defmethod rectangle-max-y ((rect standard-rectangle))
  (with-standard-rectangle* (:y2 y2)
      rect
    y2))

(defmethod rectangle-width ((rect rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rect)
    (declare (ignore y1 y2))
    (- x2 x1)))

(defmethod rectangle-width ((rect standard-rectangle))
  (with-standard-rectangle* (:x1 x1 :x2 x2)
      rect
    (- x2 x1)))

(defmethod rectangle-height ((rect rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rect)
    (declare (ignore x1 x2))
    (- y2 y1)))

(defmethod rectangle-height ((rect standard-rectangle))
  (with-standard-rectangle* (:y1 y1 :y2 y2)
      rect
    (- y2 y1)))

(defmethod rectangle-size ((rect rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rect)
    (values (- x2 x1) (- y2 y1))))

(defmethod rectangle-size ((rect standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2)
      rect
    (values (- x2 x1) (- y2 y1))))

;;; polyline/polygon protocol for STANDARD-RECTANGLEs

(defmethod polygon-points ((rect standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2)
      rect
    (list (make-point x1 y1)
          (make-point x1 y2)
          (make-point x2 y2)
          (make-point x2 y1))))

(defmethod map-over-polygon-coordinates (fun (rect standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2)
      rect
    (funcall fun x1 y1)
    (funcall fun x1 y2)
    (funcall fun x2 y2)
    (funcall fun x2 y1)))

(defmethod map-over-polygon-segments (fun (rect standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2)
      rect
    (funcall fun x1 y1 x1 y2)
    (funcall fun x1 y2 x2 y2)
    (funcall fun x2 y2 x2 y1)
    (funcall fun x2 y1 x1 y1)))

;;; -- 2.5.6 Ellipses and Elliptical Arcs in CLIM ----------------------------

;;; internal protocol
(defgeneric polar->screen (ellipse)
  ;; specialized on t, we expect that ellipse protocol is implemented.
  (:method (ellipse)
    (nest
     (multiple-value-bind (rdx1 rdy1 rdx2 rdy2) (ellipse-radii ellipse))
     (multiple-value-bind (cx cy) (ellipse-center-point* ellipse))
     (let ((cx (coordinate cx))
           (cy (coordinate cy))
           (rdx1 (coordinate rdx1))
           (rdy1 (coordinate rdy1))
           (rdx2 (coordinate rdx2))
           (rdy2 (coordinate rdy2)))
       (make-3-point-transformation* 0 0 1 0 0 1
                                     cx cy
                                     (+ cx rdx1) (+ cy rdy1)
                                     (+ cx rdx2) (+ cy rdy2))))))

(defclass elliptical-thing ()
  ((start-angle :initarg :start-angle)
   (end-angle   :initarg :end-angle)
   ;; A transformation from the unit circle to get the elliptical
   ;; object.
   (tr          :initarg :tr :reader polar->screen)))

(defmethod slots-for-pprint-object append ((object elliptical-thing))
  '(start-angle end-angle tr))

(defmethod print-object ((ell elliptical-thing) stream)
  (maybe-print-readably (ell stream)
    (print-unreadable-object (ell stream :type t :identity t)
       (with-slots (start-angle end-angle tr) ell
         (format stream "[~A ~A] ~A"
                 (and start-angle (* (/ 180 pi) start-angle))
                 (and end-angle (* (/ 180 pi) end-angle))
                 tr)))))

(defclass standard-ellipse (elliptical-thing ellipse) ())
(defclass standard-elliptical-arc (elliptical-thing elliptical-arc) ())

;;; -- 2.5.6.1 Constructor Functions for Ellipses and Elliptical Arcs in CLIM -

(defun make-ellipse (center-point
                     radius-1-dx radius-1-dy
                     radius-2-dx radius-2-dy
                     &key start-angle end-angle)
  (make-ellipse* (point-x center-point) (point-y center-point)
                 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                 :start-angle start-angle
                 :end-angle end-angle))

(defun make-ellipse* (center-x center-y
                      radius-1-dx radius-1-dy
                      radius-2-dx radius-2-dy
                      &key start-angle end-angle)
  (make-elliptical-thing 'standard-ellipse
                        center-x center-y
                        radius-1-dx radius-1-dy
                        radius-2-dx radius-2-dy
                        start-angle end-angle))

(defun make-elliptical-arc (center-point
                            radius-1-dx radius-1-dy
                            radius-2-dx radius-2-dy
                            &key start-angle end-angle)
  (make-elliptical-arc* (point-x center-point) (point-y center-point)
                        radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                        :start-angle start-angle
                        :end-angle end-angle))

(defun make-elliptical-arc* (center-x center-y
                             radius-1-dx radius-1-dy
                             radius-2-dx radius-2-dy
                             &key start-angle end-angle)
  (make-elliptical-thing 'standard-elliptical-arc
                        center-x center-y
                        radius-1-dx radius-1-dy
                        radius-2-dx radius-2-dy
                        start-angle end-angle))

(defun make-elliptical-thing (class
                             center-x center-y
                             radius-1-dx radius-1-dy
                             radius-2-dx radius-2-dy
                             start-angle end-angle)
  (setf center-x (coordinate center-x)
        center-y (coordinate center-y)
        radius-1-dx (coordinate radius-1-dx)
        radius-1-dy (coordinate radius-1-dy)
        radius-2-dx (coordinate radius-2-dx)
        radius-2-dy (coordinate radius-2-dy)
        start-angle (and start-angle (coordinate start-angle))
        end-angle (and end-angle (coordinate end-angle)))
  (let ((tr (make-3-point-transformation*
             0 0 1 0 0 1
             center-x center-y
             (+ center-x radius-1-dx) (+ center-y radius-1-dy)
             (+ center-x radius-2-dx) (+ center-y radius-2-dy))))
    (cond ((and (null start-angle) (null end-angle)))
          ((null start-angle) (setf start-angle 0))
          ((null end-angle) (setf end-angle (* 2 pi))))
    (make-instance class :tr tr :start-angle start-angle :end-angle end-angle)))

;;; -- 2.5.6.2 Accessors for CLIM Elliptical Objects -------------------------

(defmethod ellipse-center-point* ((region elliptical-thing))
  (with-slots (tr) region
    (transform-position tr 0 0)))

(defmethod ellipse-center-point ((region elliptical-thing))
  (with-slots (tr) region
    (transform-region tr (make-point 0 0))))

(defmethod ellipse-radii ((region elliptical-thing))
  (with-slots (tr) region
    (multiple-value-bind (dx1 dy1) (transform-distance tr 1 0)
      (multiple-value-bind (dx2 dy2) (transform-distance tr 0 1)
        (values dx1 dy1 dx2 dy2)))))

(defmethod ellipse-start-angle ((region elliptical-thing))
  (with-slots (start-angle) region
    start-angle))

(defmethod ellipse-end-angle ((region elliptical-thing))
  (with-slots (end-angle) region
    end-angle))

;;; -- Rectangle Sets --------------------------------------------------------

(defclass standard-rectangle-set (region-set)
  ((bands
    ;; Represents the set of rectangles. This is list like:
    ;;
    ;;  ((<y_1> . <x_band_1>)
    ;;   (<y_2> . <x_band_2>)
    ;;   :
    ;;   (<y_n>))
    ;;
    ;; <x_band_i> := (x_i_1 u_i_1  x_i_2 u_i_2 ... x_i_m u_i_m)
    ;;
    ;; Now a point (x,y) is member of the rectangle set, if there is an
    ;; i, such that y member of [y_i, y_(i+1)] and x member of x_band_i.
    ;;
    ;; An x is member of an band i, if there is an j, such that x
    ;; member [x_i_j, u_i_j].
    ;;
    ;; That is <x_band_i> describes the possible x-coordinates in the
    ;; y-range [y_i, y_(i+1)].
    ;;
    :initarg :bands
    :reader  standard-rectangle-set-bands)
   ;;
   (bounding-rectangle
    ;; Caches the regions bounding-rectangle. Is either NIL or the
    ;; bounding-rectangle, represented by a list (x1 y1 x2 y2).
    :initform nil)))

(defmethod map-over-region-set-regions
    (fun (region standard-rectangle-set) &key normalize)
  (with-slots (bands) region
    (cond ((or (null normalize) (eql normalize :x-banding))
           (map-over-bands-rectangles
            (lambda (x1 y1 x2 y2)
              (funcall fun (make-rectangle* x1 y1 x2 y2)))
            bands))
          ((eql normalize :y-banding)
           (map-over-bands-rectangles
            (lambda (y1 x1 y2 x2)
              (funcall fun (make-rectangle* x1 y1 x2 y2)))
            (xy-bands->yx-bands bands)))
          (t
           (error "Bad ~S argument to ~S: ~S"
                  :normalize 'map-over-region-set-regions normalize)))))

(defmethod region-set-regions ((region standard-rectangle-set) &key normalize)
  (let ((res nil))
    (map-over-region-set-regions
     (lambda (r) (push r res))
     region :normalize normalize)
    res))

(defun make-standard-rectangle-set (bands)
  (cond ((null bands) +nowhere+)
        ((and (= (length bands) 2)
              (null (cdr (second bands)))
              (= (length (cdr (first bands))) 2))
         (make-rectangle* (first (cdar bands)) (caar bands)
                          (second (cdar bands)) (caadr bands)))
        ((= (length (first bands)) 1)
         (make-standard-rectangle-set (rest bands)))
        (t
         (make-instance 'standard-rectangle-set :bands bands))))

;;; ============================================================================

(defmethod region-set-regions ((region standard-region-union) &key normalize)
  (declare (ignorable normalize))
  (standard-region-set-regions region))

(defmethod region-set-regions ((region standard-region-intersection) &key normalize)
  (declare (ignorable normalize))
  (standard-region-set-regions region))

(defmethod region-set-regions ((region standard-region-difference) &key normalize)
  (declare (ignorable normalize))
  (list (standard-region-difference-a region)
        (standard-region-difference-b region)))

(defmethod region-set-regions ((region region) &key normalize)
  (declare (ignorable normalize))
  (list region))

(defmethod map-over-region-set-regions
    (fun (region standard-region-union) &key normalize)
  (declare (ignorable normalize))
  (mapc fun (standard-region-set-regions region)))

(defmethod map-over-region-set-regions
    (fun (region standard-region-intersection) &key normalize)
  (declare (ignorable normalize))
  (mapc fun (standard-region-set-regions region)))

(defmethod map-over-region-set-regions
    (fun (region standard-region-difference) &key normalize)
  (declare (ignorable normalize))
  (funcall fun (standard-region-difference-a region))
  (funcall fun (standard-region-difference-b region)))

(defmethod map-over-region-set-regions
    (fun (region region-set) &key normalize)
  (mapc fun (region-set-regions region :normalize normalize)))

(defmethod map-over-region-set-regions (fun (region region) &key normalize)
  (declare (ignorable normalize))
  (funcall fun region))

;;;; ===========================================================================

(defmethod simple-pprint-object-args (stream (object standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2) object
    (loop for (slot-name slot-value) in `((x1 ,x1)
                                          (y1 ,y1)
                                          (x2 ,x2)
                                          (y2 ,y2))
       do
         (write-char #\Space stream)
         (pprint-newline :fill stream)
         (write-char #\: stream)
         (princ slot-name stream)
         (write-char #\Space stream)
         (unless (atom slot-value)
           (princ "'" stream))
         (write slot-value :stream stream))))

(defmethod print-object ((region standard-rectangle) stream)
  (maybe-print-readably (region stream)
    (print-unreadable-object (region stream :type t :identity nil)
      (with-standard-rectangle (x1 y1 x2 y2)
          region
        (format stream "X ~S:~S Y ~S:~S" x1 x2 y1 y2)))))

;;; Internal helpers

(defmacro with-grown-rectangle* (((out-x1 out-y1 out-x2 out-y2)
                                  (in-x1 in-y1 in-x2 in-y2)
                                  &key
                                  radius
                                  (radius-x radius)
                                  (radius-y radius)
                                  (radius-left  radius-x)
                                  (radius-right radius-x)
                                  (radius-top    radius-y)
                                  (radius-bottom radius-y))
                                  &body body)
  `(multiple-value-bind (,out-x1 ,out-y1 ,out-x2 ,out-y2)
    (values (- ,in-x1 ,radius-left)
     (- ,in-y1 ,radius-top)
     (+ ,in-x2 ,radius-right)
     (+ ,in-y2 ,radius-bottom))
    ,@body))
