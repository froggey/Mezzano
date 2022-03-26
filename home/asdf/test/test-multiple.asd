;;; -*- Lisp -*-
(defsystem test-multiple
  :components
  ((:file "file3")))

(defsystem test-multiple-too
  :components
  ((:file "file1")
   (:file "file2" :depends-on ("file1"))))

(defsystem test-multiple-free
  :depends-on (:test-multiple :test-multiple-dep)
  :components ((:file "file4")))

(defsystem test-multiple-dep
  :depends-on (:test-multiple)
  :components ((:file "file3")))
