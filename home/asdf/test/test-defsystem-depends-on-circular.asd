;;; -*- Lisp -*-
(defsystem "test-defsystem-depends-on-circular"
  :defsystem-depends-on ("test-defsystem-depends-on-circular"))
