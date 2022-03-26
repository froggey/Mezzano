;;; -*- Lisp -*-
(asdf:defsystem test2b
    :version "1.0"
    :components ((:file "file2" :in-order-to ((compile-op (load-op "file1"))
                                              (load-op (load-op "file1"))))
                 (:file "file1"))
    :depends-on (version 'test2a "1.1"))
