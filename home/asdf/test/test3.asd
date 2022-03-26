;;; -*- Lisp -*-
(defsystem test3
  :components
  ((:file "file1" :if-feature :asdf)
   (:file "file2" :if-feature (:not :asdf))))
