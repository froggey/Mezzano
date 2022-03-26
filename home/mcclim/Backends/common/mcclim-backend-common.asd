(cl:in-package #:asdf-user)

(defsystem "mcclim-backend-common"
  :depends-on ("clim")
  :components ((:file "ports")
               (:file "fonts")
               (:file "medium" :depends-on ("fonts")))
  :in-order-to ((test-op (test-op "mcclim-backend-common/test"))))

(defsystem "mcclim-backend-common/test"
  :depends-on ("mcclim-backend-common"
               "fiveam")
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "fonts"))))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:mcclim-backend-common.test '#:run-tests)))
