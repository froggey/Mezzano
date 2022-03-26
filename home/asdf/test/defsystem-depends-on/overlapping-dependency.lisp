(in-package :asdf-test)
(defvar *od* 0)
(incf *od*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *cod* 0))

(eval-when (:compile-toplevel :execute)
  (incf *cod*))
