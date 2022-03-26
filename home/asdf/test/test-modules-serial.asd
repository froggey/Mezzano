;;; -*- Lisp -*-
(asdf:defsystem test-modules-serial
  :serial t
  :components
  ((:module "a"
            :serial t
            :pathname "."
            :components
            ((:file "file1")
             (:file "file2")))
   (:module "b"
            :pathname "b"
            :components
            ((:file "file3")))))

