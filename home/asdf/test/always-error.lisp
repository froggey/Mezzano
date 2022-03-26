(in-package :asdf-test)

(defparameter *always-error-was-read-p* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "ALWAYS ERROR"))
