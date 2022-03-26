(asdf:defsystem #:mcclim-harfbuzz
  :description "CFFI interface to Harfbuzz"
  :license "Apache"
  :serial t
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:cffi
               :alexandria
               :trivial-garbage)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (cffi-grovel:grovel-file "grovel")
                                     (:file "functions")
                                     (:file "harfbuzz")))))
