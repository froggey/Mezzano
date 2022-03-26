;;;---------------------------------------------------------------------------
;;; Sample system to test inclusion of lines from other files and
;;; correct reasoning about system modifications.
;;;---------------------------------------------------------------------------

(defsystem test-include
    :version (:read-file-form "../build/random-version2.lisp-expr")
  :components ((:file "file2" :in-order-to ((compile-op (load-op "file1"))
                                            (load-op (load-op "file1"))))
               (:file "file1")))