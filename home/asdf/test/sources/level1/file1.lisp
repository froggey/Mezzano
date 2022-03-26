(defpackage :test-package (:use :cl))
(in-package :test-package)
(defvar *file-tmp* t)

(eval-when (:compile-toplevel :execute)
  (format t "compiling level1/file1~%"))
(eval-when (:load-toplevel :execute)
  (format t "loading level1/file1~%"))
