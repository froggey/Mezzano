;;; -*- mode: lisp -*-
(asdf:defsystem :spatial-trees
  :depends-on ()
  :components
  ((:module base
            :pathname #.(make-pathname :directory '(:relative))
            :components
            ((:file "package")
             (:file "basedefs" :depends-on ("package"))
             (:file "rectangles" :depends-on ("package"))))
   (:module tree-impls
            :depends-on (base)
            :pathname #.(make-pathname :directory '(:relative))
            :components
            ((:file "r-trees")
             (:file "greene-trees" :depends-on ("r-trees"))
             (:file "rstar-trees" :depends-on ("r-trees"))
             (:file "rplus-trees" :depends-on ("r-trees"))
             (:file "x-trees" :depends-on ("r-trees" "rstar-trees"))))
   (:static-file "LICENCE")
   (:static-file "TODO"))
  :perform (test-op :after (op c) 
		    (asdf:load-system :spatial-trees.test)
                    (asdf:test-system :spatial-trees.nns)))
