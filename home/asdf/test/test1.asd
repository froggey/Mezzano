;;; -*- Lisp -*-
(defsystem test1
  :components ((:file "file2" :in-order-to ((compile-op (load-op "file1"))
                                            (load-op (load-op "file1"))))
               (:file "file1")))
#|
1) from clean, check that all fasl files build and that some function
   defined in the second file is present

2) delete the second fasl file, and build again.  do test 1 again and
   also check the date on file1.fasl
|#
