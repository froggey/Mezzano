;;; -*- Lisp -*-
(defsystem file3-only
  :components ((:file "file3")))

(defvar *file3-only-asd-loaded* 0)
(incf *file3-only-asd-loaded*)
