(asdf:defsystem :spatial-trees.nns.test
  :depends-on (:spatial-trees
               :spatial-trees.nns
               :optima
               :alexandria
               :iterate
               :optima
               :fiveam)
  :components
  ((:file :nearest-search-test))
  :perform (load-op (op c)
                    (eval (read-from-string "(fiveam:run! :spatial-trees.nns)"))))
