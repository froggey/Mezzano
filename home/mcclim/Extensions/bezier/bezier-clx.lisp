(defpackage :mcclim-bezier-clx
  (:use #:clim #:clim-lisp #:mcclim-bezier)
  (:import-from #:clim-clx
                #:round-coordinate
                #:clx-medium
                #:with-clx-graphics)
  (:import-from #:climi
                #:standard-translation))

(in-package :mcclim-bezier-clx)

;;; CLX drawing routines for bezier designs

(defun bezier-region-to-coord-vec (region)
  (let* ((poly (polygonalize region))
         (points (polygon-points poly))
         (coord-vec (make-array (* 2 (length points)) :fill-pointer 0)))
    (map nil (lambda (point)
               (multiple-value-bind (x y) (point-position point)
                 (vector-push (round-coordinate x) coord-vec)
                 (vector-push (round-coordinate y) coord-vec)))
         points)
    coord-vec))

(defun %clx-medium-draw-bezier-design (medium design &key filled)
  (let* ((tr (sheet-native-transformation (medium-sheet medium)))
         (region (transform-region tr design))
         (coord-vec (bezier-region-to-coord-vec region)))
    (with-clx-graphics (mirror line-style ink gc)
        medium
      (xlib:draw-lines mirror gc coord-vec :fill-p filled))))

(defmethod medium-draw-bezier-design* ((medium clx-medium) (design bezier-curve))
  (%clx-medium-draw-bezier-design medium design))

(defmethod medium-draw-bezier-design* ((medium clx-medium) (design bezier-area))
  (%clx-medium-draw-bezier-design medium design :filled t))

(defmethod medium-draw-bezier-design* ((medium clx-medium) (design bezier-union))
  (dolist (area (areas design))
    (medium-draw-bezier-design* medium area)))

(defmethod medium-draw-bezier-design* ((medium clx-medium) (design bezier-difference))
  (multiple-value-bind (min-x min-y max-x max-y)
      (bounding-rectangle* design)
    (let ((imin-x (floor min-x))
          (imin-y (floor min-y))
          (imax-x (ceiling max-x))
          (imax-y (ceiling max-y)))
      (let ((tr (make-instance 'standard-translation :dx (- imin-x)  :dy (- imin-y))))
        (let ((width (- imax-x imin-x))
              (height (- imax-y imin-y)))
          (with-clx-graphics (drawable line-style ink gc)
              medium
            ;; 1. create mask pixmap and GC
            (let* ((mask-pixmap (xlib:create-pixmap :drawable drawable
                                                    :width width
                                                    :height height
                                                    :depth 1))
                   (mask-gc (xlib:create-gcontext :drawable mask-pixmap
                                                  :foreground 1
                                                  :background 0)))
              (setf (xlib:gcontext-foreground mask-gc) 0)
              (xlib:draw-rectangle mask-pixmap mask-gc 0 0 width height t)
              ;; 2. for each of the positive areas, draw 1's in the
              ;; clip-pixmap for the corresponding shape
              (setf (xlib:gcontext-foreground mask-gc) 1)
              (dolist (area (positive-areas design))
                (let ((t-area (transform-region tr area)))
                  (let ((coord-vec (bezier-region-to-coord-vec t-area)))
                    (xlib:draw-lines mask-pixmap mask-gc coord-vec :fill-p t))))
              ;; 3. for each of the negative areas, draw 0s in the
              ;; clip-pixmap for the corresponding shape
              (setf (xlib:gcontext-foreground mask-gc) 0)
              (dolist (area (negative-areas design))
                (let ((t-area (transform-region tr area)))
                  (let ((coord-vec (bezier-region-to-coord-vec t-area)))
                    (xlib:draw-lines mask-pixmap mask-gc coord-vec :fill-p t))))
              ;; 4. set the clipmask, clip-x-origin, and clip-y-origin
              ;; 5. actually do the (masked) drawing
              (let ((old-clip-mask (xlib:gcontext-clip-mask gc))
                    (old-clip-x (xlib:gcontext-clip-x gc))
                    (old-clip-y (xlib:gcontext-clip-y gc)))

                (setf (xlib:gcontext-clip-mask gc) mask-pixmap
                      (xlib:gcontext-clip-x gc) imin-x
                      (xlib:gcontext-clip-y gc) imin-y)

                (dolist (area (positive-areas design))
                  (let* ((tr (sheet-native-transformation (medium-sheet medium)))
                         (region (transform-region tr area))
                         (coord-vec (bezier-region-to-coord-vec region)))
                    (xlib:draw-lines drawable gc coord-vec :fill-p t)))

                ;; 6. restore things on the way back out
                ;; reset the clipmask, clip-x-origin, and clip-y-origin to their original values
                (setf (xlib:gcontext-clip-mask gc) old-clip-mask
                      (xlib:gcontext-clip-x gc) old-clip-x
                      (xlib:gcontext-clip-y gc) old-clip-y))
              ;; 7. free the clipmask
              (xlib:free-gcontext mask-gc)
              (xlib:free-pixmap mask-pixmap))))))))
