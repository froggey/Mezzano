(defsystem "intermediate-dependency"
  :defsystem-depends-on ("defsystem-dependency")
  :components ((:my-cl-source-file "intermediate-dependency")))
