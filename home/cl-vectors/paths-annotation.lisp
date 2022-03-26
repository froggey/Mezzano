;;;; cl-vectors -- Rasterizer and paths manipulation library
;;;; Copyright (C) 2007-2013  Frédéric Jolliton <frederic@jolliton.com>
;;;; This code is licensed under the MIT license.

(in-package #:net.tuxee.paths)

;;;--[ Path annotation ]-----------------------------------------------------

(defun path-annotated (paths &key (include-tangents nil) (decimate-knots t) (assume-type nil))
  "Annotate the path with visual effect (like color for each type
of interpolation, circle to represent knots,..)

path -- a path or a list of path

Return a list of (color . paths)."
  (let (layer-surface
        layer-lines
        layer-arcs
        layer-bezier
        layer-bezier-cpl
        layer-bezier-cp
        layer-catmull-rom
        layer-catmull-rom-cp
        layer-knots
        layer-implicit
        layer-tangents)
    (dolist (path (if (listp paths) paths (list paths)))
      (let ((path-type (or assume-type (path-type path))))
        (when (plusp (path-size path))
          ;;
          ;; Surfaces
          ;;
          (unless (eq path-type :open-polyline)
            (push path layer-surface))
          ;;
          ;; Interpolations
          ;;
          (loop with iterator = (path-iterator path)
             for stop-p = nil then end-p
             for k1 = nil then k2
             for (interpolation k2 end-p) = (multiple-value-list (path-iterator-next iterator))
             until stop-p
             when k1
             do
             ;;
             ;; Tangents
             ;;
             (when include-tangents
               (let ((t1 (interpolation-normal interpolation k1 k2 nil))
                     (t2 (interpolation-normal interpolation k1 k2 t)))
                 (when t1
                   (setf layer-tangents
                         (nconc (stroke-path
                                 (make-simple-path
                                  (list k1 (p+ k1 (p* t1 25.0)))) 1.0)
                                layer-tangents)))
                 (when t2
                   (setf layer-tangents
                         (nconc (stroke-path
                                 (make-simple-path
                                  (list k2 (p+ k2 (p* t2 25.0)))) 1.0)
                                layer-tangents)))))
             ;;
             ;; Interpolation
             ;;
             (etypecase interpolation
               ((eql :straight-line)
                (setf layer-lines
                      (nconc (stroke-path (make-simple-path (list k1 k2)) 1.0)
                             layer-lines)))
               (bezier
                (let ((control (create-path :open-polyline)))
                  (path-reset control k1)
                  (loop for cp across (slot-value interpolation 'control-points)
                     do (path-extend control (make-straight-line) cp)
                     (push (make-circle-path (point-x cp) (point-y cp) 5.0)
                           layer-bezier-cp)
                     (push (path-reversed (make-circle-path (point-x cp) (point-y cp) 3.5))
                           layer-bezier-cp))
                  (path-extend control (make-straight-line) k2)
                  (push (first (stroke-path control 1.0))
                        layer-bezier-cpl))
                (let ((arc (create-path :open-polyline)))
                  (path-reset arc k1)
                  (path-extend arc interpolation k2)
                  (push (first (stroke-path (make-discrete-path arc) 1.0))
                        layer-bezier)))
               (arc
                (let ((arc (create-path :open-polyline)))
                  (path-reset arc k1)
                  (path-extend arc interpolation k2)
                  (setf layer-arcs
                        (nconc (stroke-path (make-discrete-path arc) 1.0)
                               layer-arcs))))
               (catmull-rom
                (loop for cp in (list* (slot-value interpolation 'head)
                                       (slot-value interpolation 'queue)
                                       (coerce (slot-value interpolation 'control-points)
                                               'list))
                   do
                   (push (make-circle-path (point-x cp) (point-y cp) 5.0)
                         layer-catmull-rom-cp)
                   (push (path-reversed (make-circle-path (point-x cp) (point-y cp) 3.5))
                         layer-catmull-rom-cp))
                (let ((spline (create-path :open-polyline)))
                  (path-reset spline k1)
                  (path-extend spline interpolation k2)
                  (push (first (stroke-path (make-discrete-path spline) 1.0))
                        layer-catmull-rom)))))
          ;;
          ;; Implicit straight line
          ;;
          (unless (eq path-type :open-polyline)
            (let ((k1 (aref (path-knots path) (1- (length (path-knots path)))))
                  (i2 (aref (path-interpolations path) 0))
                  (k2 (aref (path-knots path) 0))
                  (path (create-path :open-polyline)))
              (path-reset path k1)
              (path-extend path i2 k2)
              (setf layer-implicit
                    (nconc (stroke-path (dash-path path #(5 5)) 1.0)
                           layer-implicit))))
          ;;
          ;; Knots (decimated)
          ;;
          (loop with knots = (path-knots path)
             with last-added-knot = nil
             with first-knot = t
             with second-knot = nil
             for i below (length knots)
             for knot = (aref knots i)
             for last-knot = (= i (- (length knots) 1))
             do (when (or (not decimate-knots)
                          last-knot
                          (null last-added-knot)
                          (> (point-distance last-added-knot knot) 10))
                  (when first-knot
                    (push (make-circle-path (point-x knot) (point-y knot) 8.0)
                          layer-knots)
                    (push (path-reversed (make-circle-path (point-x knot) (point-y knot) 6.5))
                          layer-knots))
                  (push (make-circle-path (point-x knot) (point-y knot) 5.0)
                        layer-knots)
                  (unless second-knot
                    (push (path-reversed (make-circle-path (point-x knot) (point-y knot) 3.5))
                          layer-knots))
                  (setf last-added-knot knot
                        second-knot first-knot
                        first-knot nil))))))
    ;; Put everything together
    (list (cons #(230 245 255) (nreverse layer-surface))
          (cons #(90 120 180) (nreverse layer-implicit))
          (cons #(90 120 180) (nreverse layer-lines))
          (cons #(255 0 0) (nreverse layer-tangents))
          (cons #(255 0 255) (nreverse layer-arcs))
          (cons #(255 0 0) (nreverse layer-bezier))
          (cons #(0 255 0) (nreverse layer-catmull-rom))
          (cons #(255 128 0) (nreverse layer-bezier-cpl))
          (cons #(0 0 255) (nreverse layer-knots))
          (cons #(100 100 100) (nreverse layer-catmull-rom-cp))
          (cons #(255 0 0) (nreverse layer-bezier-cp)))))
