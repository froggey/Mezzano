;; part of the test-module-pathnames test

(in-package :test-package)

(defparameter *test-tmp-cl* t)

(eval-when (:compile-toplevel :execute)
  (format t "compiling test-tmp.cl~%"))
(eval-when (:load-toplevel :execute)
  (format t "loading test-tmp.cl~%"))
