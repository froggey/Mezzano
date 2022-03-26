;;; NB: This way of managing packages is explicitly NOT recommended.
;;; However, it is found in the wild, and debugging it is a pain, so
;;; we should probably not break.  The thing that this is testing is
;;; that unrelated definitions of symbols naming ASDF keywords should
;;; not affect the parsing of a system.

(in-package :cl-user) ; BAD BAD BAD

(asdf:defsystem test-package
  :components ((:module "foo" :components ((:file "bar") (:file "baz")))))
