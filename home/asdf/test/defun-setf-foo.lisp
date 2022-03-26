(in-package :asdf-test/deferred-warnings)

(defvar *foo* (make-hash-table :test 'equal))

(defun (setf foo) (v x)
  (setf (gethash x *foo*) v))
