(defsystem alexandria-tests
  :depends-on (:alexandria #+sbcl :sb-rt #-sbcl :rt)
  :components ((:file "tests")))

(defmethod operation-done-p
    ((o test-op) (c (eql (find-system :alexandria-tests))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :alexandria-tests))))
  (flet ((run-tests (&rest args)
           (apply (intern (string '#:run-tests) '#:alexandria-tests) args)))
    (run-tests :compiled nil)
    (run-tests :compiled t)))