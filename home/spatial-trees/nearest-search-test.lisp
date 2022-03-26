
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :spatial-trees.nns.test)
    (defpackage :spatial-trees.nns.test
      (:use :cl
            :rectangles
            :spatial-trees
            :spatial-trees.nns
            :rectangles
            :optima
            :alexandria
            :iterate
            :fiveam)
      (:shadowing-import-from :spatial-trees :delete :search :bounding-rectangle)
      (:shadowing-import-from :rectangles :intersection)
      (:shadow :fail))))

(in-package :spatial-trees.nns.test)

(def-suite :spatial-trees.nns)
(in-suite :spatial-trees.nns)

;;;; picked from tutorial.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (p (:constructor p (x y)))
    x y))

(defun random-p (x y)
  (p (random (float x)) (random (float y))))

(defun p-list (p)
  (match p
    ((p x y)
     (list x y))
    ((list _ _)
     p)))

(defun =~ (a b)
  (< (abs (- a b)) 0.01))

(defun ^2 (x) (* x x))

(defun distance (p1 p2)
  (sqrt (distance2 p1 p2)))

(defun distance2 (p1 p2)
  (ematch p2
    ((or (p (x x2) (y y2))
         (list x2 y2))
     (ematch p1
       ((or (p x y) (list x y))
        (+ (^2 (- x2 x)) (^2 (- y2 y))))))))

(test :point
  (finishes (p 5 6)
            (random-p 5 5))
  (is (=~ 1 (distance (p 5 6) (p 5 7))))
  (is (=~ 2 (distance (p 5 6) (p 7 6))))
  (is (=~ (sqrt 2) (distance (p 5 6) (p 6 7)))))

(defun p-rect (p)
  "make a bounding box function."
  (with-slots (x y) p
    (make-rectangle ;; << in package `rectangles'
     :lows (list (- x 0.1) (- y 0.1))
     :highs (list (+ x 0.1) (+ y 0.1)))))

(defun make-tree-of-points (kind ps)
  (let ((tree (make-spatial-tree kind :rectfun #'p-rect)))
    (mapc (lambda (p) (insert p tree)) ps)
    tree))

(defparameter *xmax* 5)
(defparameter *ymax* 5)

(defparameter *kinds* '(:r
                        ;; #+sbcl :r*
                        :x
                        :greene))

(test :nns
  (iter (for kind in *kinds*)
        (format *trace-output* "~&testing ~S...~&" kind)
        (let (ps tree result expected)
            (for-all ((center (lambda () (p-list (random-p *xmax* *ymax*)))))
              (finishes
                (setf ps (iter (repeat 1000) (collect (random-p *xmax* *ymax*))))
                (setf tree (make-tree-of-points kind ps))
                (setf result (nearest-neighbor-search center tree #'distance2))
                (setf expected (iter (for p in ps)
                                     (finding p minimizing (distance p center)))))
              
              (is (eq expected result))))))

