(in-package #:climi)

(defmethod region-contains-position-p ((region standard-rectangle-set) x y)
  (block nil
    (map-over-bands (lambda (y1 y2 isum)
                      (when (<= y1 y y2)
                        (when (isum-member x isum)
                          (return t)))
                      (when (< y y2)
                        (return nil)))
                    (standard-rectangle-set-bands region))
    nil))

(defmethod region-contains-position-p ((region standard-region-union) x y)
  (some (lambda (r) (region-contains-position-p r x y))
        (standard-region-set-regions region)))

(defmethod region-contains-position-p ((region standard-region-intersection) x y)
  (every (lambda (r) (region-contains-position-p r x y))
         (standard-region-set-regions region)))

(defmethod region-contains-position-p ((region standard-region-difference) x y)
  (let ((region-a (standard-region-difference-a region))
        (region-b (standard-region-difference-b region)))
    (and (region-contains-position-p region-a x y)
         (not (region-contains-position-p region-b x y)))))
