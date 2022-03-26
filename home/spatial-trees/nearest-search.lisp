
(defpackage :spatial-trees.nns
  (:use :cl
        :rectangles
        :spatial-trees-impl
        :spatial-trees-protocol
        :optima
        :alexandria
        :iterate)
  (:shadowing-import-from :spatial-trees :delete :search :bounding-rectangle)
  (:shadowing-import-from :rectangles :intersection))

(in-package :spatial-trees.nns)

;; Nearest Neighbor Queries
;; N Roussopoulos, S Kelley, F Vincent - ACM sigmod record, 1995

(declaim (inline ^2))
(declaim (ftype (function (number) number) ^2))
(defun ^2 (x) (* x x))

(defun mindist (coordinates r)
  (declare (type list coordinates))
  (declare (type rectangle r))
  (reduce #'+
          (map 'vector
               (lambda (s_i t_i p_i)
                 (^2 (- (clamp p_i s_i t_i) p_i)))
               (lows r)
               (highs r)
               coordinates)))

(defun minimax-dist (coordinates r)
  (declare (type list coordinates))
  (declare (type rectangle r))
  (flet ((rmk (s_k t_k p_k)
           (if (<= p_k (/ (+ s_k t_k) 2)) s_k t_k))
         (rMi (s_i t_i p_i)
           (if (>= p_i (/ (+ s_i t_i) 2)) s_i t_i)))
    
    (let ((indices (iota (length coordinates))))
      (reduce #'min
              (map 'vector
                   (lambda (s_k t_k p_k k)
                     (+ (^2 (- p_k (rmk s_k t_k p_k)))
                        (reduce #'+
                                (map 'vector
                                     (lambda (s_i t_i p_i i)
                                       (if (= i k)
                                           0
                                           (^2 (- p_i (rMi s_i t_i p_i)))))
                                     (lows r)
                                     (highs r)
                                     coordinates
                                     indices))))
                   (lows r)
                   (highs r)
                   coordinates
                   indices)))))

;; Section 3.2
;; Originally, our guess for
;; the nearest neighbor distance (call it Nearest) is infinity.

(export '(nearest-neighbor-search
          mindist
          minimax-dist
          abl-element
          abl-mindist
          abl-node
          abl-minimax-dist))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; required, because these structures should be loaded before the optima expansion
  ;;  -- this was missed during the development.
  (defstruct (abl-element (:constructor abl-element (node mindist minimax-dist))
                          (:conc-name abl-))
    (node nil :type spatial-tree-node)
    (mindist 0 :type number)
    (minimax-dist 0 :type number))

  (defstruct (nearest-element (:constructor nearest-element (object dist))
                              (:conc-name nearest-))
    (object nil)
    (dist 0 :type number)))

(defvar *tree*)
(defun nearest-neighbor-search (point *tree* distance-function)
  "point (list numbers*): a list in the same form as lows and highs.
tree spatial-tree : spatial tree.
distance-function point,point -> number : returns a *square* of the euclid distance.
 between 2 points"
  (ematch *tree*
    ((spatial-tree root-node)
     (ematch (%nns root-node point
                   (nearest-element nil MOST-POSITIVE-DOUBLE-FLOAT)
                   distance-function)
       ((nearest-element (object (leaf-node-entry datum)))
        datum)))))


(defun %nns (node point nearest fn)
  (ematch node
    ((spatial-tree-leaf-node children)
     (reduce (lambda (best current)
               (ematch best
                 ((nearest-element dist)
                  (ematch current
                    ((leaf-node-entry datum)
                     (let ((newdist (funcall fn point datum)))
                       (if (< newdist dist)
                           (nearest-element current newdist)
                           best)))))))
             children
             :initial-value nearest))
    ((spatial-tree-node children)
     ;; Note: I don't like the procedual style but I try to follow the
     ;; notation in the paper

     ;; During the descending phase, at each newly visited nonleaf
     ;; node, the algorithm computes the ordering metric
     ;; bounds (e.g. MINDIST, Definition 2) for all its MBRs
     ;; and sorts them (associated with their corresponding
     ;; node) into an Active Branch List (ABL).

     ;; We then apply pruning strategies 1 and 2 to the ABL to
     ;; remove unnecessary branches.
     (iter (with last = (prune-downward-1
                         node point nearest
                         (sort (mapcar (lambda (child)
                                         (let ((mbr (mbr child *tree*)))
                                           (abl-element child
                                                        (mindist point mbr)
                                                        (minimax-dist point mbr))))
                                       children) #'<
                               :key #'abl-mindist)))

           (for new-abl-node in last)
           (setf nearest (%nns (abl-node new-abl-node) point nearest fn))
           (setf last
                 (prune-upward
                  node point nearest
                  last))
           (finally (return nearest))))))


(defun prune-downward (node point nearest abl)
  (prune-downward-1 node point nearest abl))

;; (prune-downward-2
;;    node point nearest
;;    (prune-downward-1 node point nearest abl))

(defun prune-downward-1 (node point nearest abl)
  "1. an MBR M with MINDIST(P,M) greater than MINMAXDIST(P,M')
of another MBR M’ is discarded because
it cannot contain the NN (thorems 1 and 2)."
  (declare (ignorable node point nearest))
  (let ((min-minimax (iter (for node in abl)
                           (minimizing (abl-minimax-dist node)))))
    (remove-if (lambda (node)
                 (< min-minimax (abl-mindist node)))
               abl)))

(defun prune-downward-2 (node point nearest abl)
  "2. an actual distance from P to a given object O
which is greater than the MINMAXDIST(P,M) for
an MBR M can be discarded (actually replaced by
it as an estimate of the NN distance) because M
contains an object O’ which is nearer to P (theorem
2). This is also used in downward pruning."

  ;; ????
  ;; does it mean there are raw objects in abl ?
  )
  
(defun prune-upward (node point nearest abl)
  "(3.1) 3. every MBR M with MINDIST(P,M) greater than
the actual distance from P to a given object O is
discarded because it cannot enclose an object nearer
than O (theorem 1). We use this in upward pruning.

 (3.2) we take this new estimate of the NN and
apply pruning strategy 3 to remove all branches with
M1ND1S7’(P, M) > Nearest for all MBRs M in the
ABL."
  (declare (ignorable node point))
  (match nearest
    ((nearest-element (dist nearest-dist))
     (remove-if (lambda (node)
                  (< nearest-dist (abl-mindist node)))
                abl))))