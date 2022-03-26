;;; -*- Lisp -*-
(asdf:defsystem test-samedir-modules
  :components ((:module "here"
                        :components
                        ((:file "file2" :in-order-to ((compile-op (load-op "file1"))
                                                      (load-op (load-op "file1"))))
                         (:file "file1"))
                        :pathname "")))

#|
from clean, check that all fasl files build and that some function
   defined in the second file is present
|#
