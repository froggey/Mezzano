;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tutorial.lisp
;;;;
;;;; describes the basic usage. C-c it and copy/paste it into repl
;;;; buffer. you see how it works.

(in-package :cl-user)
(asdf:load-system :alexandria)

(defpackage spatial-trees-tutorial
  (:use :cl
        :alexandria
        :spatial-trees
        :rectangles)
  (:shadowing-import-from :spatial-trees :delete :search :bounding-rectangle)
  (:shadowing-import-from :rectangles :intersection))

(in-package :spatial-trees-tutorial)

(defstruct (p (:constructor p (x y)))
  x y)

(p 5 6)

(defun random-p (x y)
  (p (random (float x)) (random (float y))))

(random-p 5 5)

(defun p-rect (p)
  "make a bounding box function."
  (with-slots (x y) p
    (make-rectangle ;; << in package `rectangles'
     :lows (list (- x 0.1) (- y 0.1))
     :highs (list (+ x 0.1) (+ y 0.1)))))

(defvar tree (make-spatial-tree :r :rectfun #'p-rect))

(defparameter ps (loop repeat 10 collect (random-p 5 5)))

(mapc (lambda (p) (insert p tree)) ps)

;; see whats in it
(inspect tree)

;; are there any points whose bounding rectangle intersects [3.4 3.4] -- [3.6 3.6] ?
(search (p 3.5 3.5) tree)

;; do whatever you want... test how it works