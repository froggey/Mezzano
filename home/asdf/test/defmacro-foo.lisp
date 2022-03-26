(in-package :asdf-test/deferred-warnings)

(defmacro foo (x)
  `(1+ ,x))
