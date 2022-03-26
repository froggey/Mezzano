(cl:in-package #:clim-tests)

(def-suite* :mcclim.transforms
  :in :mcclim)

(test transforms.+identity-transformation+
  (is (subtypep (class-of +identity-transformation+) 'transformation))
  (is (transformationp +identity-transformation+)))

(test transforms.condition-type-relations
  (is (subtypep 'transformation-error 'error))

  (is (subtypep 'transformation-underspecified  'transformation-error))
  (is (typep (make-condition 'transformation-underspecified
                             :points (list (make-point 1 1)
                                           (make-point 2 1)
                                           (make-point 3 1)))
             'transformation-error))

  (is (subtypep 'reflection-underspecified  'transformation-error))
  (is (typep (make-condition 'reflection-underspecified
                             :points (list (make-point 1 1)
                                           (make-point 3 1)))
             'transformation-error))


  (is (subtypep 'singular-transformation  'transformation-error))
  (is (typep (make-condition 'singular-transformation
                             :transformation +identity-transformation+)
             'transformation-error)))

(test transforms.classes
  (is (typep (make-translation-transformation 10 20)
             'transformation))

  (is (typep (make-transformation 10 20 30 40 50 60) 'transformation))

  (let* ((x1 30) (y1 40) (x2 10) (y2 20) (x3 30) (y3 55)
         (p1 (make-point x1 y1)) (p2 (make-point x2 y2)) (p3 (make-point x3 y3)))
    (is (typep (make-rotation-transformation 10) 'transformation))
    (is (typep (make-rotation-transformation 10 p2) 'transformation))
    (is (typep (make-rotation-transformation* 10) 'transformation))
    (is (typep (make-rotation-transformation* 10 x1 y1) 'transformation))
    (is (typep (make-scaling-transformation 10 50) 'transformation))
    (is (typep (make-scaling-transformation 10 50 p1) 'transformation))
    (is (typep (make-scaling-transformation* 10 50) 'transformation))
    (is (typep (make-scaling-transformation* 10 50 x1 y1) 'transformation))
    (is (typep (make-reflection-transformation p2 p3) 'transformation))
    (is (typep (make-reflection-transformation* x2 y2 x3 y3) 'transformation))
    (let* ((x4 66) (y4 89) (x5 85) (y5 13) (x6 867) (y6 -58)
           (p4 (make-point x4 y4)) (p5 (make-point x5 y5)) (p6 (make-point x6 y6)))
      (is (typep (make-3-point-transformation p1 p2 p3 p4 p5 p6) 'transformation))
      (is (typep (make-3-point-transformation* x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6)
                 'transformation)))))

(test transforms.protocol
  (let* ((t1 (make-rotation-transformation 0))
         (t2 (make-scaling-transformation 1 1)))
    (is (identity-transformation-p t1))
    (is (identity-transformation-p t2))
    (is (transformation-equal t1 t2))
    (is (invertible-transformation-p t1))
    (is (invertible-transformation-p t2))
    (is (translation-transformation-p t1))
    (is (translation-transformation-p t2))
    ;; tests fail
    ;;  (is (reflection-transformation-p t1))
    ;;  (is (reflection-transformation-p t2))
    (is (rigid-transformation-p t1))
    (is (rigid-transformation-p t2))
    (is (even-scaling-transformation-p t1))
    (is (even-scaling-transformation-p t2))
    (is (scaling-transformation-p t1))
    (is (scaling-transformation-p t2))
    (is (rectilinear-transformation-p t1))
    (is (rectilinear-transformation-p t2))
    (is (transformation-equal t1 (compose-transformations t1 t2)))
    (is (transformation-equal t1 (invert-transformation t1)))
    (is (transformation-equal t1 (compose-translation-with-transformation t1 0 0)))
    (is (transformation-equal t1 (compose-rotation-with-transformation t1 0)))
    (is (transformation-equal t1 (compose-scaling-with-transformation t1 1 1)))
    ;; tests fail
    ;;  (is (transformation-equal t1 (compose-transformation-with-translation t1 0 0)))
    (is (transformation-equal t1 (compose-transformation-with-rotation t1 0)))
    (is (transformation-equal t1 (compose-transformation-with-scaling t1 1 1))))

  (let ((tr (make-rotation-transformation 0))
        (r (make-rectangle* 10 20 30 40))
        (x 10) (y 20))
    (is (region-equal r (transform-region tr r)))
    (is (region-equal r (untransform-region tr r)))
    (multiple-value-bind (xx yy) (transform-position tr x y)
      (is (= (coordinate x) xx))
      (is (= (coordinate y) yy)))
    (multiple-value-bind (xx yy) (untransform-position tr x y)
      (is (= (coordinate x) xx))
      (is (= (coordinate y) yy)))
    (multiple-value-bind (xx yy) (transform-distance tr x y)
      (is (= (coordinate x) xx))
      (is (= (coordinate y) yy)))
    (multiple-value-bind (xx yy) (untransform-distance tr x y)
      (is (= (coordinate x) xx))
      (is (= (coordinate y) yy)))
    (let ((x2 55) (y2 -20))
      (multiple-value-bind (xx1 yy1 xx2 yy2) (transform-rectangle* tr x y x2 y2)
        (is (= xx1 (min (coordinate x) (coordinate x2))))
        (is (= yy1 (min (coordinate y) (coordinate y2))))
        (is (= xx2 (max (coordinate x) (coordinate x2))))
        (is (= yy2 (max (coordinate y) (coordinate y2)))))
      (multiple-value-bind (xx1 yy1 xx2 yy2) (untransform-rectangle* tr x y x2 y2)
        (is (= xx1 (min (coordinate x) (coordinate x2))))
        (is (= yy1 (min (coordinate y) (coordinate y2))))
        (is (= xx2 (max (coordinate x) (coordinate x2))))
        (is (= yy2 (max (coordinate y) (coordinate y2))))))))
