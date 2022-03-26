;;; CLIM-PostScript is not a backend in the normal sense.
;;; It is an extension (Chap. 35.1 of the spec) and is an
;;; "included" part of McCLIM. Hence the defsystem is here.
;;;
;;; Move the whole system to the Modules/ directory - dk

(defsystem "clim-postscript"
  :depends-on ("clim-basic"
               "clim-postscript-font")
  :serial t
  :components ((:file "package")
               (:file "paper")
               (:file "class")
               (:file "graphics")
               (:file "sheet")
               (:file "output-destination"))
  :in-order-to ((test-op (test-op "clim-postscript/test"))))

(defsystem "clim-postscript/test"
  :depends-on ("clim-postscript"
               "fiveam"
               "mcclim/test-util")
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "smoke")
                             (:file "output-destination"))))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:clim-postscript.test '#:run-tests)))
