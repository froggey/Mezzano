;;; -*- mode: lisp -*-

(asdf:defsystem :spatial-trees-viz
  :depends-on (:spatial-trees
               :mcclim)
  :components
  ((:file "spatial-tree-viz")))

