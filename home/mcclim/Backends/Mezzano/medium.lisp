(in-package #:clim-mezzano)

(defvar *debug-format-control* nil)
(defvar *debug-format-messages* NIL)

(defun debug-format (string &rest args)
  (cond ((eq *debug-format-control* :console)
         (mos:debug-print-line (apply #'format nil string args)))
        ((eq *debug-format-control* :list)
         (push (apply #'format nil string args) *debug-format-messages*))
        ((streamp *debug-format-control*)
         (apply #'format *debug-format-control* string args))))

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  "Function used for rounding coordinates."
  ;; We use "mercantile rounding", instead of the CL round to nearest
  ;; even number, when in doubt.
  ;;
  ;; Reason: As the CLIM drawing model is specified, you quite often
  ;; want to operate with coordinates, which are multiples of 1/2.
  ;; Using CL:ROUND gives you "random" results. Using "mercantile
  ;; rounding" gives you consistent results.
  ;;
  ;; Note that CLIM defines pixel coordinates to be at the corners,
  ;; while in X11 they are at the centers. We don't do much about the
  ;; discrepancy, but rounding up at half pixel boundaries seems to
  ;; work well.
  (floor (+ x .5)))

;;; MEZZANO-MEDIUM class

(defclass mezzano-medium (render-medium-mixin basic-medium)
  ())

(defmethod text-style-fixed-width-p (text-style (medium mezzano-medium))
  (eql (text-style-family text-style) :fixed))
