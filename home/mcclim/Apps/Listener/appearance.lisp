

(in-package :clim-listener)

;;; Apropos

(defparameter *apropos-symbol-unbound-family* :fix)
(defparameter *apropos-symbol-unbound-face*   :roman)
(defparameter *apropos-symbol-bound-family*   :fix)
(defparameter *apropos-symbol-bound-face*     :roman)

;;; Show Class Slots

(defparameter *slot-name-ink*     +black+)
(defparameter *slot-type-ink*     +gray50+)
(defparameter *slot-initargs-ink* +red+)
(defparameter *slot-initform-ink* +goldenrod3+)
(defparameter *slot-readers-ink*  +black+)
(defparameter *slot-writers-ink*  +black+)
(defparameter *slot-documentation-ink* +turquoise4+)

;;; Graphing (classes and packages)

(defparameter *graph-edge-ink* (make-rgb-color 0.72 0.72 0.72))
(defparameter *graph-text-style* (make-text-style :fix :roman :normal))



