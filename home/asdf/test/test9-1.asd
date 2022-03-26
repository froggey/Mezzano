;;; -*- Lisp -*-
(asdf:defsystem test9-1
    :version "1.1"
    :components ((:file "file1"))
    :depends-on ((:version :test9-2 "2.0")))


