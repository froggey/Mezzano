(asdf:defsystem :spatial-trees.nns
  :depends-on (:spatial-trees
               :optima
               :alexandria
               :iterate)
  :components
  ((:file :nearest-search))
  :perform (test-op :after (op c) 
		    (asdf:load-system :spatial-trees.nns.test)))
