;;; 
;;; Copyright (c) 2006, Timothy Moore (moore@bricoworks.com)
;;;

(defsystem #:functional-geometry
  :name #:functional-geometry
  :license "LGPL-2.1+"
  :depends-on (:clim-listener)
  :components ((:file "package")
	       (:file "geometry" :depends-on ("package"))))

