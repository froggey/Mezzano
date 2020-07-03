(defsystem "mezzano-virgl"
  :serial t
  :components ((:file "package")
               (:file "virgl-protocol")
               (:file "virgl")
               (:file "tgsi")
               (:file "test")
               (:file "demo-menu")))
