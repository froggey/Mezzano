(defsystem "defsystem-dependency"
  :depends-on ("overlapping-dependency")
  :components ((:file "defsystem-dependency"))
  :in-order-to ((test-op (test-op "test-system"))))
