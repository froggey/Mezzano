;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; The dominator tree, immediate dominators and the dominance frontier.

(in-package :mezzano.compiler.backend.dominance)

(defclass dominance ()
  ((%dominators)
   (%immediate-dominators)
   (%dominator-tree)
   (%dominance-frontier)))

(defun build-dominator-sets (backend-function basic-blocks bb-preds)
  (let ((dominators (make-hash-table)))
    ;; Iteratively compute the dominators for each basic block.
    ;; TODO: Replace this with the Lengauer-Tarjan algorithm.
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
  (let ((dom-tree (make-hash-table))
        (idoms (make-hash-table)))
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
  (let ((df (make-hash-table)))
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

(defun compute-dominance (backend-function)
  (sys.c:with-metering (:backend-compute-dominance)
    (multiple-value-bind (basic-blocks bb-preds bb-succs)
        (mezzano.compiler.backend::build-cfg backend-function)
      (let ((dominance (make-instance 'dominance))
            (dominators (build-dominator-sets backend-function basic-blocks bb-preds)))
        (setf (slot-value dominance '%dominators) dominators)
        (multiple-value-bind (dom-tree idoms)
            (build-dominator-tree backend-function basic-blocks dominators)
          (setf (slot-value dominance '%dominator-tree) dom-tree)
          (setf (slot-value dominance '%immediate-dominators) idoms)
          (setf (slot-value dominance '%dominance-frontier)
                (build-dominance-frontier backend-function dom-tree idoms bb-succs))
          dominance)))))

(defun dominatep (dominance dominator basic-block)
  "Is BASIC-BLOCK dominated by DOMINATOR?"
  (member dominator
          (gethash basic-block (slot-value dominance '%dominators))))

(defun dominator-tree-parent (dominance basic-block)
  (gethash basic-block (slot-value dominance '%immediate-dominators)))

(defun dominator-tree-children (dominance basic-block)
  (gethash basic-block (slot-value dominance '%dominator-tree)))

(defun dominance-frontier (dominance basic-block)
  (gethash basic-block (slot-value dominance '%dominance-frontier)))
