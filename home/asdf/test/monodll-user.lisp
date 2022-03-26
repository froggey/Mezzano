(defpackage :test-asdf/monodll-user (:use)) ;; dummy, for package-inferred-system dependencies.

(in-package :asdf-test)
(ffi:def-function "always_42" () :returning :int)
