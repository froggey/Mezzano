;;; -*- mode: lisp -*-
(asdf:defsystem :spatial-trees.test
  :depends-on (:spatial-trees :fiveam)
  :components
  ((:file "spatial-tree-test"))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :spatial-trees)"))
		    (asdf:clear-system c)))

