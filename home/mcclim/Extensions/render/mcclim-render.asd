(cl:in-package #:asdf-user)

(defsystem #:mcclim-render
  :description "Support for raster images McCLIM."
  :depends-on (#:clim-basic #:mcclim-fonts/truetype #:cl-vectors)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "image")
               (:module "render"
                        :serial t
                        :components
                        ((:file "prim-arc")
                         (:file "prim-text")))
               (:module "cl-vectors"
                        :serial t
                        :components
                        ((:file "vectors")
                         (:file "vectors-image-ops")))
               (:module "backend"
                        :serial t
                        :components
                        ((:file "mirror")
                         (:file "mirrored-sheet")
                         (:file "pixmap")
                         (:file "medium")
                         (:file "fonts")
                         (:file "port")))))
