(in-package #:sys.int)

(defun integerp (object)
  (system:fixnump object))

(defun realp (object)
  (integerp object))

(defun numberp (object)
  (realp object))

(defun expt (base power)
  (let ((accum 1))
    (dotimes (i power accum)
      (setf accum (* accum base)))))
