(in-package #:asdf-user)

(defsystem #:clim-basic
  :depends-on ("clim-lisp"
               "alexandria"
               "spatial-trees"
               (:version "flexichain" "1.5.1")
               "bordeaux-threads"
               "trivial-garbage"
               "trivial-features"
               "babel")
  :components
  ((:file "setf-star")
   (:file "decls" :depends-on ("setf-star"))
   (:file "protocol-classes" :depends-on ("decls"))
   (:file "multiprocessing" :depends-on ("decls"))
   (:file "utils" :depends-on ("decls" "multiprocessing"))
   (:module "geometry"
    :depends-on ("decls" "protocol-classes" "multiprocessing" "utils" "setf-star")
    :serial t
    :components ((:file "coordinates")
                 (:file "transforms")
                 (:file "bounding-rectangle")
                 (:file "regions")
                 (:file "region-utilities")
                 (:file "region-bounding-rectangles")
                 (:file "region-transformations")
                 (:file "region-predicates")
                 (:file "region-composition")
                 (:file "region-set-predicates")
                 (:file "region-set-composition")))
   (:module "windowing"
    :depends-on ("utils" "decls" "protocol-classes" "multiprocessing" "geometry")
    :components ((:file "events")
                 (:file "output")
                 (:file "pixmap"  :depends-on ("output"))
                 (:file "sheets")
                 (:file "mirrors" :depends-on ("sheets"))
                 (:file "ports"   :depends-on ("sheets" "pixmap" "events"))
                 (:file "input"   :depends-on ("sheets" "ports"))
                 (:file "grafts"  :depends-on ("sheets" "ports"))
                 (:file "repaint" :depends-on ("sheets" "ports" "grafts" "events" "output"))))
   (:module "drawing"
    :depends-on ("utils" "decls" "protocol-classes" "geometry")
    :components ((:file "design")
                 (:file "colors"   :depends-on ("design"))
                 (:file "pattern"  :depends-on ("design"))
                 (:file "medium"   :depends-on ("design" "colors"))
                 (:file "graphics" :depends-on ("design" "medium"))))
   (:module "extended-streams"
    :depends-on ("setf-star" "decls" "utils" "protocol-classes" "multiprocessing" "geometry" "windowing" "drawing")
    :components ((:file "text-formatting") ; standard-page-layout
                 (:file "views")           ; stream-default-view
                 (:file "dead-keys")       ; dead-key merging
                 (:file "stream-output"    :depends-on ("text-formatting" "views"))
                 (:file "recording"        :depends-on ("stream-output"))
                 (:file "text-selection"   :depends-on ("recording"))
                 (:file "encapsulate"      :depends-on ("stream-output" "recording"))
                 (:file "stream-input"     :depends-on ("encapsulate" "dead-keys"))
                 (:file "pointer-tracking" :depends-on ("stream-output" "stream-input"))))))
