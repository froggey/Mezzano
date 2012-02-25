(in-package #:sys.int)

(defun integerp (object)
  (system:fixnump object))

(defun numberp (object)
  (integerp object))

(defun expt (base power)
  (let ((accum 1))
    (dotimes (i power accum)
      (setf accum (* accum base)))))
