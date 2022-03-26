(cl:eval-when (:load-toplevel :execute)
  (asdf:load-system :fiveam)
  (asdf:load-system :checkl))

(defsystem :fast-io-test
  :description "Tests for fast-io"

  :depends-on (:fast-io :checkl)

  :pathname "t"
  :serial t

  :components
  ((:file "package")
   (:file "benchmark-defs")

   (checkl:tests "basic")
   (checkl:tests "gray")
   (checkl:test-values "test-values"
                       :package :fast-io.test)))

(checkl:define-test-op :fast-io :fast-io-test)
(checkl:define-test-op :fast-io-test)
