(in-package #:clim-tests)

(def-suite* :mcclim.design
  :in :mcclim)

(defvar *mcclim.design/foo*)

(test opacity-value.standard-color
  "Test that the standard-color is a solid design."
  (is (= 1 (opacity-value +red+))))

(test color-rgb.standard-color
  "Smoke test for color-rgb."
  (is (equalp '(1.0 1.0 1.0) (multiple-value-list (color-rgb +white+)))))

(test color-rgba.standard-color
  "Smoke test for color-rgba."
  (is (equalp '(1.0 1.0 1.0 1.0) (multiple-value-list (color-rgba +white+)))))

(test color-rgba.indirect-ink
  "Test that color-rgba works on indirect inks."
  (let ((ink (make-instance 'climi::indirect-ink
                            :default clim:+white+
                            :symbol '*mcclim.design/foo*)))
    (is (equalp '(1.0 1.0 1.0 1.0) (multiple-value-list (color-rgba ink))))
    (let ((*mcclim.design/foo* +brown+))
      (is (equalp (multiple-value-list (color-rgba ink))
                  (multiple-value-list (color-rgba +brown+)))))
    (let ((*mcclim.design/foo* +flipping-ink+))
      (signals error (color-rgba ink)))))

(test design-ink.standard-color
  "Smoke test for design-ink."
  (is (eql +red+ (design-ink +red+ 0 0))))

(test design-ink.region
  "Test that regions can be treated as inks."
  (let ((region (make-rectangle* 10 10 20 20)))
    (is (= 0 (opacity-value (design-ink region 0 0))))
    (is (= 1 (opacity-value (design-ink region 15 15))))))
