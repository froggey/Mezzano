;;;; foo.asd

(asdf:defsystem #:test-asdf-location-change
  :serial t
  :components ((:file "a")
               (:file "b" :depends-on ("a"))))
