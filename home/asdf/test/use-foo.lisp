(in-package :asdf-test/deferred-warnings)

(defun use-foo (x)
  (foo x))

#+clisp (eval-when (:compile-toplevel :load-toplevel :execute)
          (format t "~&~S~%" `(sys::*unknown-functions* ,sys::*unknown-functions*)))
