(in-package #:clim-tests)

(def-suite* :mcclim.drawing
  :in :mcclim)

(test make-text-style.cache
  "Test caching behavior of `make-text-style'."

  (let ((climi::*text-style-hash-table* (make-hash-table :test #'eql)))
    (let ((styles '()))
      (flet ((test-style (family face size
                          &optional (family2 family) (face2 face) (size2 size))
               (let ((first  (make-text-style family  face  size))
                     (second (make-text-style family2 face2 size2)))
                 (push first styles)
                 (is (eq first second)))))
        (test-style nil nil nil)
        (test-style :serif :italic :normal)
        (test-style :serif :roman 10
                    :serif :roman 10.0)
        (test-style :serif '(:bold :italic) 10
                    :serif '(:italic :bold)))
      (loop for (style1 . rest) on styles
            do (loop for style2 in rest
                     do (is (not (eq style1 style2))))))))
