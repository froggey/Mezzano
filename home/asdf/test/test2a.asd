;;; -*- Lisp -*-
(asdf:defsystem test2a
    :version "1.1"
    :components ((:file "file4" :in-order-to ((compile-op (load-op "file3"))
                                              (load-op (load-op "file3"))))
                 (:file "file3")))
#|
this system is referenced by test2b[12]
|#
