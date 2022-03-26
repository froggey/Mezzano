;;; -*- Lisp -*-
(asdf:defsystem test-modules
  :components
  ((:module "a"
            :pathname "."
            :components
            ((:file "file1")))
   (:module "b"
            :pathname "b"
            :depends-on ("a")
            :components
            ((:file "file2")))))

