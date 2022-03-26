(in-package :asdf-test/deferred-warnings)

(defvar *foo* (make-hash-table :test 'equal))

(defun set-foo (x v)
  (setf (gethash x *foo*) v))

(defsetf foo set-foo)
