(defsystem "test-system"
  :defsystem-depends-on ("defsystem-dependency")
  :components ((:my-cl-source-file "test-system"))
  :perform (test-op (o c) (symbol-call :asdf-test :run-test)))
