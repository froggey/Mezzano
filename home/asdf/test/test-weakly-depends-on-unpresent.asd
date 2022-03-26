(defsystem test-weakly-depends-on-unpresent
  :weakly-depends-on (does-not-exist)
  :components ((:file "file1")))
