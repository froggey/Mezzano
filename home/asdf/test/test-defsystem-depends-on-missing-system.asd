;;; -*- Lisp -*-
(defsystem test-defsystem-depends-on-missing-system
  :defsystem-depends-on (nonexistent-system))
