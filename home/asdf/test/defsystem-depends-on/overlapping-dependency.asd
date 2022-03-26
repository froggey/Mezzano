(in-package :asdf-test)
(defvar *od* 0)
(defsystem "overlapping-dependency"
  ;;:perform (load-op (o c) (incf *od*)) ;; Now done in the file.
  :components ((:file "overlapping-dependency")))
