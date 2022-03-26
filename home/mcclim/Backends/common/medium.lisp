(in-package #:climi)

(defclass multiline-text-medium-mixin () ()
  (:documentation "Takes care of splitting string into multiple lines and adjusts Y-position."))

(defmethod clim:medium-draw-text* :around ((medium multiline-text-medium-mixin) string x y
                                           start end
                                           align-x align-y
                                           toward-x toward-y transform-glyphs)
  (unless (position #\newline string :start start :end end)
    (return-from clim:medium-draw-text* (call-next-method)))
  (setq string (subseq string start end))
  (let* ((font (text-style-mapping (port medium) (medium-text-style medium)))
         (y-dx (font-leading font)))
    ;; Single line centering is figured out in the primary method, we just fix
    ;; the X/Y if it will be different for the supplied positioning and then
    ;; increase it for each line. -- jd 2018-10-08
    (case align-y
      (:center
       (setq y (- y (/ (* y-dx (count #\newline string)) 2))))
      ((:bottom :baseline*)
       (setq y (- y (* y-dx (count #\newline string))))))
    (dolines (line string)
      (unless (alexandria:emptyp line)
        (call-next-method medium line x y 0 (length line)
                          align-x align-y toward-x toward-y
                          transform-glyphs))
      (incf y y-dx))))

;; For multiline text alignment may change the bbox. For instance longest line
;; may start with a character with left-bearing=0 and shorter line starts with a
;; character which has left-bearing=-10. If text is left-aligned then bbox
;; starts from coordinate x=-10, but if text is right-aligned it is x=0. This
;; mixin provides decent adjustment for alignment for simpler algorithms. Method
;; is not pixel-perfect hence it should be used sparingly for early prototypes.
(defclass approx-bbox-medium-mixin () ()
  (:documentation "Adjusts bounding rectangle to alignment with a decent heuristic."))

(defmethod climb:text-bounding-rectangle* :around
    ((medium approx-bbox-medium-mixin) string &key text-style start end
                                                (align-x :left)
                                                (align-y :baseline)
                                                (direction :ltr))
  (declare (ignore start end direction))
  (multiple-value-bind (left top right bottom) (call-next-method)
    (let ((width (- right left)))
      (ecase align-x
        (:left)
        (:right
         (decf left width)
         (decf right width))
        (:center
         (decf left (/ width 2.0))
         (decf right (/ width 2.0)))))
    (let ((ascent (text-style-ascent text-style medium))
          (descent (text-style-descent text-style medium))
          (height (- bottom top)))
      (ecase align-y
        (:baseline)
        (:baseline*
         (setf top (- descent height))
         (setf bottom descent))
        (:top
         (setf top (- ascent (abs top))
               bottom (+ top height)))
        (:bottom
         (decf top bottom)
         (decf bottom bottom))
        (:center
         (setf top (- (/ height 2.0)))
         (setf bottom (/ height 2.0)))))
    (values left top right bottom)))
