;;; Somewhat rudimentary tests of external functionality

(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :spatial-trees-test)
    (defpackage spatial-trees-test
      (:use :cl
            :spatial-trees
            :spatial-trees-protocol
            :rectangles
            :fiveam)
      (:export :make-random-rectangle)
      (:shadowing-import-from :spatial-trees :delete :search :bounding-rectangle)
      (:shadowing-import-from :rectangles :intersection))))

(in-package :spatial-trees-test)
(def-suite :spatial-trees)
(in-suite :spatial-trees)


(defparameter *kinds* '(:r :greene
                        ;; :r*
                        :x))

(defun make-random-rectangle (&optional (x-bias 0.0) (y-bias 0.0))
  (let* ((lx (+ (random 0.9) x-bias))
         (ly (+ (random 0.9) y-bias))
         (hx (+ (random 0.9) lx))
         (hy (+ (random 0.9) ly)))
    (make-rectangle :lows (list lx ly) :highs (list hx hy))))

(defun gen-rectangle ()
  (lambda ()
    (make-random-rectangle)))

(def-fixture tree-kind ()
  (let (seen)
    (for-all ((kind (apply #'gen-one-element *kinds*) (not (find kind seen))))
      (push kind seen)
      (&body))))

(defparameter *howmany* 201)

(test :insert
  (with-fixture tree-kind ()
    (for-all ((list (gen-list :length (constantly *howmany*)
                              :elements (gen-rectangle))))
      (let ((tree (make-spatial-tree kind :rectfun #'identity)))
        (finishes
          (dolist (r list)
            (insert r tree)))))))

;; minimal failure case:
;; (loop for kind in '(:r :greene :r* :x)
;;    do (loop repeat 100
;;          for list = (loop repeat 200 collect (make-random-rectangle))
;;          do (let ((tree (make-spatial-tree kind :rectfun #'identity)))
;;               (dolist (r list)
;;                 (insert r tree)))))

(test (:random-search :fixture tree-kind)
  (for-all ((list (gen-list :length (constantly *howmany*)
                            :elements (gen-rectangle))))
    (let ((tree (make-spatial-tree kind :rectfun #'identity)))
      (finishes
        (dolist (r list)
          (insert r tree)))
      (let* ((r (make-random-rectangle))
             (expected (remove-if-not (lambda (x) (intersectp x r)) list)))
        (let (result)
          (finishes
            (setf result (search r tree)))
          (is-true result)
          (is (null (set-difference result expected
                                    :key (lambda (x)
                                           (list (lows x) (highs x)))
                                    :test #'equal)))
          (is (null (set-difference result expected
                                    :key (lambda (x)
                                           (list (lows x) (highs x)))
                                    :test #'equal))))))))

(test (:trisected-search :fixture tree-kind)
  (for-all ((list (lambda ()
                    (loop repeat *howmany*
                         collect (make-random-rectangle 0.0 0.0)
                         collect (make-random-rectangle -2.0 -2.0)
                         collect (make-random-rectangle 2.0 2.0)))))
    (let ((tree (make-spatial-tree kind :rectfun #'identity)))
      (dolist (r list)
        (insert r tree))
      (let ((r (make-rectangle :lows '(0.0 0.0) :highs '(1.0 1.0))))
        ;; FIXME: find a way to test the relative speed of the following
        ;; (sbcl-specifically if necessary).
        (search r tree)
        (remove-if-not (lambda (x) (intersectp x r)) list)
        (is (= *howmany* (length (search r tree))))))))

(test (:arbitrary-object-search :fixture tree-kind)
  (let* ((list (loop repeat *howmany*
                  for r = (make-random-rectangle)
                  collect (cons (lows r) (highs r))))
         (rectfun (lambda (x) (make-rectangle :lows (car x) :highs (cdr x))))
         (tree (make-spatial-tree kind :rectfun rectfun)))
    (dolist (r list)
      (insert r tree))
    (let* ((r (make-random-rectangle))
           (result (search r tree))
           (expected (remove-if-not
                      (lambda (x) (intersectp (funcall rectfun x) r))
                      list)))
      (is (null (set-difference
                 result expected
                 :key (lambda (x)
                        (let ((r (funcall rectfun x)))
                          (list (lows r) (highs r))))
                 :test #'equal))))))


(test (:deletion :fixture tree-kind)
  (let* ((list (loop repeat *howmany* collect (make-random-rectangle)))
         (tree (make-spatial-tree kind :rectfun #'identity)))
    (dolist (r list)
      (insert r tree))
    (dolist (r (cdr list))
      (delete r tree))
    (let* ((results (search (car list) tree))
           (length (length results)))
      (is (= (length results) 1)
          "aargh: wrong amount of stuff (~D entries) in ~S"
          length tree))))

