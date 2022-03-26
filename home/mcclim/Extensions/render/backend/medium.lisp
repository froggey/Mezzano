(in-package :mcclim-render-internals)

(defclass render-medium-mixin (basic-medium
                               climb:multiline-text-medium-mixin
                               climb:font-rendering-medium-mixin)
  ())

(defun %medium-stroke-paths (medium paths)
  (alexandria:when-let* ((msheet (sheet-mirrored-ancestor (medium-sheet medium)))
                         (mirror (sheet-mirror msheet))
                         (transformation (sheet-native-transformation (medium-sheet medium))))
    (%stroke-paths medium mirror paths
                   (medium-line-style medium)
                   transformation
                   (climi::medium-device-region medium)
                   (transform-region transformation (medium-ink medium)))))

(defun %medium-fill-paths (medium paths)
  (alexandria:when-let* ((msheet (sheet-mirrored-ancestor (medium-sheet medium)))
                         (mirror (sheet-mirror msheet))
                         (transformation (sheet-native-transformation (medium-sheet medium))))
    (%fill-paths mirror paths
                 transformation
                 (climi::medium-device-region medium)
                 (transform-region transformation (medium-ink medium)))))

(defun %medium-draw-image (medium image x y width height to-x to-y)
  (alexandria:when-let* ((msheet (sheet-mirrored-ancestor (medium-sheet medium)))
                         (mirror (sheet-mirror msheet))
                         (image (etypecase image
                                  (basic-sheet (image-mirror-image (sheet-mirror image)))
                                  (clime:image-pattern image))))
    (%draw-image mirror image
                 (round x) (round y)
                 (round width) (round height)
                 (round to-x) (round to-y)
                 (climi::medium-device-region medium))))

;;; XXX: used only for medium-draw-text* for now.
(defun %medium-fill-image-mask (medium mask-image from-x from-y width height to-x to-y)
  (alexandria:when-let* ((msheet (sheet-mirrored-ancestor (medium-sheet medium)))
                         (mirror (sheet-mirror msheet)))
    (%fill-image mirror
                 (round from-x) (round from-y)
                 (round width) (round height)
                 (transform-region (sheet-native-transformation (medium-sheet medium))
                                   (medium-ink medium))
                 (climi::medium-device-region medium)
                 ;; Stencil
                 mask-image (round to-x) (round to-y))))

(defun %medium-fill-image (medium x y width height)
  (alexandria:when-let* ((msheet (sheet-mirrored-ancestor (medium-sheet medium)))
                         (mirror (sheet-mirror msheet)))
    (%fill-image mirror
                 (round x) (round y)
                 (round width) (round height)
                 (transform-region (sheet-native-transformation (medium-sheet medium))
                                   (medium-ink medium))
                 (climi::medium-device-region medium))))

;;;
;;; standard medium protocol
;;;

(defmethod medium-draw-rectangle* ((medium render-medium-mixin) left top right bottom filled)
  (when (< right left) (rotatef left right))
  (when (< bottom top) (rotatef top bottom))
  (let* ((region (region-intersection
                  (climi::medium-device-region medium)
                  (transform-region (sheet-native-transformation (medium-sheet medium))
                                    (make-rectangle* left top right bottom)))))
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y) region
      (if filled
          (%medium-fill-image medium min-x min-y (- max-x min-x) (- max-y min-y))
          (let ((path (make-path left top)))
            (line-to path right top)
            (line-to path right bottom)
            (line-to path left bottom)
            (close-path path)
            (%medium-stroke-paths medium (list path)))))))

(defmethod medium-draw-polygon* ((medium render-medium-mixin) coord-seq closed filled)
  (let ((x (elt coord-seq 0))
        (y (elt coord-seq 1)))
    (let ((path (make-path x y)))
      (do ((v 2 (+ 2 v)))
          ((>= v (length coord-seq)))
        (let ((x (elt coord-seq v))
              (y (elt coord-seq (1+ v))))
          (line-to path x y)))
      (when closed
        (close-path path))
      (if filled
          (%medium-fill-paths medium (list path))
          (%medium-stroke-paths medium (list path))))))

(defmethod medium-draw-line* ((medium render-medium-mixin) x1 y1 x2 y2)
  (let ((path (make-path x1 y1)))
    (line-to path x2 y2)
    (%medium-stroke-paths medium (list path))))

(defmethod medium-draw-point* ((medium render-medium-mixin) x y)
  (let ((path (arc x y
                   (max 1 (/ (line-style-thickness (medium-line-style medium)) 2))
                   pi
                   (+ pi (* 2 pi)))))
    (%medium-fill-paths medium (list path))))

(defmethod medium-draw-circle* ((medium render-medium-mixin)
                                center-x center-y radius start-angle end-angle
                                filled)
  (let ((path (arc center-x center-y radius (+ pi start-angle) (+ pi end-angle))))
    (if filled
        (%medium-fill-paths medium (list path))
        (%medium-stroke-paths medium (list path)))))

(defmethod medium-draw-ellipse* ((medium render-medium-mixin) center-x center-y
                                 radius-1-dx radius-1-dy
                                 radius-2-dx radius-2-dy
                                 start-angle end-angle filled
                                 &aux (el (make-ellipse*
                                           center-x center-y
                                           radius-1-dx radius-1-dy
                                           radius-2-dx radius-2-dy
                                           :start-angle start-angle
                                           :end-angle end-angle)))
  (multiple-value-bind (cx cy hx hy theta) (climi::ellipse-simplified-representation el)
    (declare (ignorable cx cy))
    (let* ((sa (- (* 2 pi) end-angle theta))
           (dalpha (- end-angle start-angle))
           (path (ellipse-arc center-x center-y hx hy theta
                              sa (+ sa dalpha))))
      (when filled
        (line-to path center-x center-y))
      (if filled
          (%medium-fill-paths medium (list path))
          (%medium-stroke-paths medium (list path))))))

(defmethod medium-draw-text* ((medium render-medium-mixin) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs
                              &aux (end (if (null end)
                                            (length string)
                                            (min end (length string)))))
  (declare (ignore toward-x toward-y))
  (setq string (subseq string start end))
  (string-primitive-paths medium x y string align-x align-y transform-glyphs))

(defmethod medium-copy-area ((from-drawable render-medium-mixin) from-x from-y width height
                             (to-drawable render-medium-mixin) to-x to-y)
  (medium-force-output from-drawable)
  (multiple-value-bind (w2 h2)
      (untransform-distance (medium-transformation from-drawable)
                            width height)
    (multiple-value-bind (w h)
        (transform-distance (sheet-transformation (medium-sheet to-drawable))
                            w2 h2)
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
          (region-intersection
           (climi::medium-device-region to-drawable)
           (transform-region
            (sheet-native-transformation (medium-sheet to-drawable))
            (make-rectangle* to-x to-y (+ to-x w) (+ to-y h))))
        (multiple-value-bind (x1 y1)
            (transform-position
             (sheet-native-transformation (medium-sheet to-drawable))
             to-x to-y)
          (multiple-value-bind (x2 y2)
              (transform-position
               (sheet-native-transformation (medium-sheet from-drawable))
               from-x from-y)
            (%medium-draw-image to-drawable
                                (medium-sheet from-drawable)
                                (+ x2 (- min-x x1))
                                (+ y2 (- min-y y1))
                                (- max-x min-x) (- max-y min-y)
                                min-x min-y)))))))

(defmethod medium-copy-area ((from-drawable render-medium-mixin) from-x from-y width height
                             (to-drawable image-sheet-mixin) to-x to-y)
  (medium-force-output from-drawable)
  (let* ((msheet (sheet-mirrored-ancestor to-drawable)))
    (when (and msheet (sheet-mirror msheet))
      (multiple-value-bind (w2 h2)
          (untransform-distance (medium-transformation from-drawable)
                                width height)
        (multiple-value-bind (w h)
            (transform-distance (sheet-transformation to-drawable)
                                w2 h2)
          (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
              (region-intersection
               (climi::sheet-native-region to-drawable)
               (transform-region
                (sheet-native-transformation to-drawable)
                (make-rectangle* to-x to-y (+ to-x w) (+ to-y h))))
            (multiple-value-bind (x1 y1)
                (transform-position
                 (sheet-native-transformation to-drawable)
                 to-x to-y)
              (multiple-value-bind (x2 y2)
                  (transform-position
                   (sheet-native-transformation (medium-sheet from-drawable))
                   from-x from-y)
                (climi::with-pixmap-medium (to-medium to-drawable)
                  (%medium-draw-image (sheet-medium to-drawable)
                                      (medium-sheet from-drawable)
                                      (+ x2 (- min-x x1))
                                      (+ y2 (- min-y y1))
                                      (- max-x min-x) (- max-y min-y)
                                      min-x min-y))))))))))

(defmethod medium-copy-area ((from-drawable image-sheet-mixin) from-x from-y width height
                             (to-drawable render-medium-mixin) to-x to-y)
  (medium-force-output (sheet-medium from-drawable))
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet to-drawable))))
    (when (and msheet (sheet-mirror msheet))
      (multiple-value-bind (w2 h2)
          (untransform-distance (medium-transformation (sheet-medium from-drawable))
                                width height)
        (multiple-value-bind (w h)
            (transform-distance (sheet-transformation (medium-sheet to-drawable))
                                w2 h2)
          (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
              (region-intersection
               (climi::medium-device-region to-drawable)
               (transform-region
                (sheet-native-transformation (medium-sheet to-drawable))
                (make-rectangle* to-x to-y (+ to-x w) (+ to-y h))))
            (multiple-value-bind (x1 y1)
                (transform-position
                 (sheet-native-transformation (medium-sheet to-drawable))
                 to-x to-y)
              (multiple-value-bind (x2 y2)
                  (transform-position
                   (sheet-native-transformation from-drawable)
                   from-x from-y)
                (%medium-draw-image to-drawable
                                    from-drawable
                                    (+ x2 (- min-x x1))
                                    (+ y2 (- min-y y1))
                                    (- max-x min-x) (- max-y min-y)
                                    min-x min-y)))))))))

(defmethod medium-finish-output ((medium render-medium-mixin))
  (when (sheet-mirror (medium-sheet medium))
    (%mirror-force-output (sheet-mirror (medium-sheet medium)))))

(defmethod medium-force-output ((medium render-medium-mixin))
  (when (sheet-mirror (medium-sheet medium))
    (%mirror-force-output (sheet-mirror (medium-sheet medium)))))
