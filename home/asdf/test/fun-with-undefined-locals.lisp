(defpackage fun-with-undefined-locals-package
  (:use :common-lisp))

(in-package :fun-with-undefined-locals-package)

(defun foo ()
  (+ a b))
