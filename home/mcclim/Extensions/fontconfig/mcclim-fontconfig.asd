(asdf:defsystem #:mcclim-fontconfig
  :description "CFFI interface to Fontconfig"
  :license "Apache"
  :defsystem-depends-on (:cffi-grovel)
  :serial t
  :depends-on (:cffi
               :alexandria)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (cffi-grovel:grovel-file "grovel")
                                     (:file "conditions")
                                     (:file "functions")
                                     (:file "fontconfig")))))
