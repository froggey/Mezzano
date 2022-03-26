(in-package :cl-user)

(defpackage :spatial-trees
  (:use :cl)
  (:shadow :delete :search)
  (:export :delete :insert :search :bounding-rectangle
           :make-spatial-tree))

(defpackage :spatial-trees-protocol
  (:use :cl :spatial-trees)
  (:shadowing-import-from :spatial-trees :delete :search)
  (:export
   ;; interface definitions
   :delete :insert :search
   ;; protocol functions
   :choose-leaf :split-node :children :records :root-node
   ;; protocol classes
   :spatial-tree :spatial-tree-node :spatial-tree-leaf-node
   ))

(defpackage :rectangles
  (:use :cl)
  (:shadow :intersection)
  (:export :rectangle :make-rectangle :intersection :intersectp
           :area :minimum-bound :lows :highs))

(defpackage :spatial-trees-impl
  (:use :cl
        :spatial-trees
        :spatial-trees-protocol
        :rectangles)
  (:export :rectfun
           :leaf-node-entry
           :datum
           :mbr)
  (:shadowing-import-from :spatial-trees :delete :search :bounding-rectangle)
  (:shadowing-import-from :rectangles :intersection))
