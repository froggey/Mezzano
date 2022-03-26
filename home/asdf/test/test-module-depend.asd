(defsystem :test-module-depend
  :components
  ((:file "file1")
   (:module "quux"
    :pathname ""
    :depends-on ("file1")
    :components
    ((:file "file2")
     (:module "file3mod"
      :pathname ""
      :components
      ((:file "file3")))))))
