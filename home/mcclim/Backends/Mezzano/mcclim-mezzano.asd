(defsystem "mcclim-mezzano"
    :depends-on ("mcclim-backend-common"
                 "mcclim-render")
    :serial t
    :components
    ((:file "package")
     (:file "events")
     (:file "graft")
     (:file "medium")
     (:file "port")
     (:file "mirror")
     (:file "mirrored-sheets")
     (:file "frame-manager")
     (:file "text-selection")))
