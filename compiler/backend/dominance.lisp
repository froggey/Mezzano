;;;; The dominator tree, immediate dominators and the dominance frontier.

(in-package :mezzano.compiler.backend.dominance)

(defclass dominance ()
  ((%dominators)
   (%immediate-dominators)
   (%dominator-tree)
   (%dominance-frontier)))

(defvar *use-simple-dominator-algorithm* nil)

;; TODO: This numbers basic blocks, switch most hash tables over to using
;; fixed length arrays and pass around block numbers instead of the blocks
;; themselves.
(defun lengauer-tarjan-dominators (entry-basic-block bb-preds bb-succs)
  (let ((n* 0)
        (bucket (make-hash-table :test 'eq))
        (dfnum (make-hash-table :test 'eq))
        (semi (make-hash-table :test 'eq))
        (ancestor (make-hash-table :test 'eq))
        (best (make-hash-table :test 'eq))
        (idom (make-hash-table :test 'eq))
        (samedom (make-hash-table :test 'eq))
        (vertex (make-array 0 :adjustable t :fill-pointer 0))
        (parent (make-hash-table :test 'eq)))
    (labels ((ancestor-with-lowest-semi (v)
               (let ((a (gethash v ancestor)))
                 (when (gethash a ancestor)
                   (let ((b (ancestor-with-lowest-semi a)))
                     (setf (gethash v ancestor) (gethash a ancestor))
                     (when (< (gethash (gethash b semi) dfnum 0)
                              (gethash (gethash (gethash v best) semi) dfnum 0))
                       (setf (gethash v best) b)))))
               (gethash v best))
             (link (p n)
               (setf (gethash n ancestor) p
                     (gethash n best) n)))
      ;; Non-recursive depth-first search over the blocks to number them.
      (let ((dfs-stack (make-array 100 :fill-pointer 0 :adjustable t)))
        (vector-push-extend nil dfs-stack)
        (vector-push-extend entry-basic-block dfs-stack)
        (loop
           (when (eql (length dfs-stack) 0) (return))
           (let* ((n (vector-pop dfs-stack))
                  (p (vector-pop dfs-stack)))
             (when (eql (gethash n dfnum 0) 0)
               (setf (gethash n dfnum) n*)
               (vector-push-extend n vertex)
               (setf (gethash n parent) p)
               (incf n*)
               ;; This is reversed to maintain the same traversal order as
               ;; the original recursive implementation.
               (dolist (w (reverse (gethash n bb-succs)))
                 (vector-push-extend n dfs-stack)
                 (vector-push-extend w dfs-stack))))))
      (loop for i from (1- n*) downto 1 do
           (let* ((n (aref vertex i))
                  (p (gethash n parent))
                  (s p)
                  (s* nil))
             (dolist (v (gethash n bb-preds))
               (cond ((<= (gethash v dfnum 0) (gethash n dfnum 0))
                      (setf s* v))
                     (t
                      (setf s* (gethash (ancestor-with-lowest-semi v) semi))))
               (when (< (gethash s* dfnum 0) (gethash s dfnum 0))
                 (setf s s*)))
             (setf (gethash n semi) s)
             (pushnew n (gethash s bucket '()))
             (link p n)
             (dolist (v (gethash p bucket))
               (let ((y (ancestor-with-lowest-semi v)))
                 (cond ((eql (gethash y semi) (gethash v semi))
                        (setf (gethash v idom) p))
                       (t
                        (setf (gethash v samedom) y)))))
             (setf (gethash p bucket) '()))))
    (loop for i from 1 below n* do
         (let ((n (aref vertex i)))
           (when (gethash n samedom)
             (setf (gethash n idom) (gethash (gethash n samedom) idom)))))
    (setf (gethash entry-basic-block idom) nil)
    idom))

(defun build-dominator-sets (backend-function basic-blocks bb-preds)
  (let ((dominators (make-hash-table :test 'eq)))
    ;; Iteratively compute the dominators for each basic block.
    ;; For all other nodes, set all nodes as the dominators.
    (dolist (bb basic-blocks)
      (setf (gethash bb dominators) basic-blocks))
    ;; The only dominator of the start node is the start node itself.
    (let ((start-bb (first-instruction backend-function)))
      (setf (gethash start-bb dominators) (list start-bb))
      ;; iteratively eliminate nodes that are not dominators.
      (let ((changes t))
        (loop
           (when (not changes)
             (return))
           (setf changes nil)
           (dolist (bb basic-blocks)
             (unless (eql bb start-bb)
               ;; Dom(n) = {n} union with intersection over Dom(p) for all p in pred(n)
               (let ((dom-n
                      (union (list bb)
                             (reduce #'intersection
                                     (mapcar (lambda (p)
                                               (gethash p dominators))
                                             (gethash bb bb-preds))))))
                 (unless (mezzano.compiler.backend::set-equal dom-n (gethash bb dominators))
                   (setf changes t)
                   (setf (gethash bb dominators) dom-n))))))))
    dominators))

(defun build-dominator-tree (backend-function basic-blocks dominators)
  "Construct the dominator tree & immediate dominator table."
  (let ((dom-tree (make-hash-table :test 'eq))
        (idoms (make-hash-table :test 'eq)))
    (dolist (bb basic-blocks)
      (setf (gethash bb dom-tree) '()))
    (dolist (bb basic-blocks)
      (cond ((eql bb (first-instruction backend-function))
             ;; Entry basic block has no immediate dominator.
             (setf (gethash bb idoms) nil))
            (t
             ;; The immediate dominator of a node N is the unique node that strictly
             ;; dominates N but does not strictly dominate any other node that strictly
             ;; dominates N.
             (let* ((strict-doms (remove bb ; Nodes do not strictly dominate themselves.
                                         (gethash bb dominators)))
                    (idom nil))
               (dolist (sdom strict-doms)
                 (flet ((is-idom ()
                          (dolist (other strict-doms t)
                                  (unless (eql other sdom)
                                    (when (member sdom (gethash other dominators))
                                      (return nil))))))
                   (when (is-idom)
                     (assert (not idom) () "Found multiple immediate dominators for ~S" bb)
                     (setf idom sdom))))
               (assert idom () "Unable to compute immediate dominator for ~S" bb)
               (setf (gethash bb idoms) idom)
               (push bb (gethash idom dom-tree))))))
    (values dom-tree idoms)))

(defun build-dominance-frontier (backend-function dom-tree idoms bb-succs)
  (let ((df (make-hash-table :test 'eq)))
    (labels ((frob (n)
               (let ((s '()))
                 (dolist (y (gethash n bb-succs))
                   (when (not (eql (gethash y idoms) n))
                     (pushnew y s)))
                 (dolist (c (gethash n dom-tree))
                   (frob c)
                   (dolist (w (gethash c df))
                     (when (not (member w (gethash n dom-tree)))
                       (pushnew w s))))
                 (setf (gethash n df) s))))
      (frob (first-instruction backend-function)))
    df))

(defun build-dom-tree-from-idoms (idoms)
  (let ((tree (make-hash-table :test 'eq)))
    (maphash (lambda (bb idom)
               (when idom
                 (pushnew bb (gethash idom tree))))
             idoms)
    tree))

(defun compute-dominance (backend-function)
  (multiple-value-bind (basic-blocks bb-preds bb-succs)
      (mezzano.compiler.backend::build-cfg backend-function)
    (let ((dominance (make-instance 'dominance)))
      (cond (*use-simple-dominator-algorithm*
             (let ((dominators (build-dominator-sets backend-function basic-blocks bb-preds)))
               (setf (slot-value dominance '%dominators) dominators)
               (multiple-value-bind (dom-tree idoms)
                   (build-dominator-tree backend-function basic-blocks dominators)
                 (setf (slot-value dominance '%dominator-tree) dom-tree)
                 (setf (slot-value dominance '%immediate-dominators) idoms))))
            (t
             (let ((idoms (lengauer-tarjan-dominators (first basic-blocks) bb-preds bb-succs)))
               (setf (slot-value dominance '%immediate-dominators) idoms)
               (setf (slot-value dominance '%dominator-tree) (build-dom-tree-from-idoms idoms)))))
      (setf (slot-value dominance '%dominance-frontier)
            (build-dominance-frontier backend-function
                                      (slot-value dominance '%dominator-tree)
                                      (slot-value dominance '%immediate-dominators)
                                      bb-succs))
      dominance)))

(defun dominatep (dominance dominator basic-block)
  "Is BASIC-BLOCK dominated by DOMINATOR?"
  (do ((dom basic-block
            (dominator-tree-parent dominance dom)))
      ((null dom)
       nil)
    (when (eql dom dominator)
      (return t))))

(defun dominator-tree-parent (dominance basic-block)
  (gethash basic-block (slot-value dominance '%immediate-dominators)))

(defun dominator-tree-children (dominance basic-block)
  (gethash basic-block (slot-value dominance '%dominator-tree)))

(defun dominance-frontier (dominance basic-block)
  (gethash basic-block (slot-value dominance '%dominance-frontier)))
