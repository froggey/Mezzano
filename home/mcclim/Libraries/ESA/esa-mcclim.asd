;;;
;;; Copyright (c) 2005-2006, Robert Strandh (strandh@labri.u-bordeaux.fr)
;;; Copyright (c) 2006, Troels Henriksen (athas@sigkill.dk)
;;;           

;;; ASDF system definition for ESA.

(defsystem #:esa-mcclim
  :depends-on (#:clim-core #:alexandria)
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "esa" :depends-on ("packages" "utils"))
               (:file "esa-buffer" :depends-on ("packages" "esa"))
               (:file "esa-io" :depends-on ("packages" "esa" "esa-buffer"))
               (:file "esa-command-parser" :depends-on ("packages" "esa"))))
