;;; Modifications to the basic R-tree following "THE R+-TREE: A
;;; DYNAMIC INDEX FOR MULTI-DIMENSIONAL OBJECTS", Sellis, Roussopoulos
;;; and Faloutsos, Proc. 15th Int. Conf. on Very Large Databases,
;;; 1987
;;;
;;; By this stage, not very much of the original R-tree is left, mind
;;; you.

(in-package "SPATIAL-TREES-IMPL")

(defclass r+-tree (r-tree)
  ((fill-factor :initarg :fill-factor :accessor fill-factor)))
(defmethod make-spatial-tree :around ((kind (eql :r+)) &rest initargs)
  (declare (ignore initargs))
  (cerror "Make an R+-tree nevertheless"
          "R+-trees' dynamic insertion is broken.")
  (call-next-method))
(defmethod make-spatial-tree ((kind (eql :r+)) &rest initargs)
  (apply #'make-instance 'r+-tree
         :root-node (make-instance 'spatial-tree-leaf-node :records nil)
         initargs))
(defmethod initialize-instance :after ((tree r+-tree) &rest args)
  (declare (ignore args))
  (unless (slot-boundp tree 'fill-factor)
    (setf (slot-value tree 'fill-factor)
          (round (* (max-per-node tree) 1/3)))))

(defmethod search :around ((r rectangle) (tree r+-tree))
  (remove-duplicates (call-next-method)))

;;; 3.3. Insertion
(defmethod insert ((r t) (tree r+-tree))
  (labels ((%insert (r node tree)
             (cond
               ((typep node 'spatial-tree-leaf-node)
                (if (>= (length (records node)) (max-per-node tree))
                    (split-node tree r node)
                    (push r (records node))))
               (t (dolist (child (children node))
                    (when (intersectp (mbr child tree) (mbr r tree))
                      (%insert r child tree)))))))
    (%insert r (root-node tree) tree)
    tree))

;;; 3.4. Deletion
(defun unlink-node (node tree)
  (cond
    ((eq node (root-node tree)))
    (t (let ((parent (parent node)))
         (cond
           ((= (length (children parent)) 1)
            (if (eq parent (root-node tree))
                (setf (root-node tree)
                      (make-instance 'spatial-tree-leaf-node :records nil))
                (unlink-node parent tree)))
           (t
            (setf (children parent) (remove node (children parent))
                  (slot-value parent 'mbr) (minimum-bound-of (children parent) tree))))))))

(defmethod delete ((r t) (tree r+-tree))
  (labels ((%delete (r node tree)
             (cond
               ((typep node 'spatial-tree-leaf-node)
                (setf (records node) (remove r (records node)))
                (when (null (records node))
                  (unlink-node node tree)))
               (t (dolist (child (children node))
                    (when (intersectp (mbr child tree) (mbr r tree))
                      (%delete r child tree)))))))
    (%delete r (root-node tree) tree)
    tree))

;;; 3.5. Node Splitting
(defun cost (r1 r2)
  (+ (area r1) (area r2)))

(defun generate-partition-rectangles (entries tree)
  (let ((mbr (minimum-bound-of entries tree)))
    (do* ((lows (lows (mbr (car entries) tree)) (cdr lows))
          (axis 0 (1+ axis))
          (mincost)
          (low)
          (high))
         ((null lows) (values low high))
      (let ((sorted (sort (copy-list entries) #'<
                          :key (lambda (x) (nth axis (lows (mbr x tree)))))))
        (cond
          ;; FIXME: this is the wrong way to write a loop which needs
          ;; initialization.  There are a number of these dotted around
          ;; now.
          ((null mincost)
           (let* ((one (subseq sorted 0 (fill-factor tree)))
                  (two (subseq sorted (fill-factor tree)))
                  (new-low-coordinate
                   (/ (+ (nth axis (highs (mbr (car (last one)) tree)))
                         (nth axis (lows (mbr (car two) tree))))
                      2)))
             ;; FIXME: this does not guarantee optimality, because if
             ;; one and two do not overlap we can shrink this high
             ;; rectangle to the MBR of two.
           (setf low (make-rectangle
                      :lows (lows mbr)
                      :highs (substitute-if new-low-coordinate (constantly t)
                                            (highs mbr) :start axis :count 1))
                 high (make-rectangle
                       :lows (substitute-if new-low-coordinate (constantly t)
                                            (lows mbr) :start axis :count 1)
                       :highs (highs mbr))
                 mincost (cost low high))))
          (t
           (let* ((one (subseq sorted 0 (fill-factor tree)))
                  (two (subseq sorted (fill-factor tree)))
                  (new-low-coordinate
                   (/ (+ (nth axis (highs (mbr (car (last one)) tree)))
                         (nth axis (lows (mbr (car two) tree))))
                      2))
                  (new-low (make-rectangle
                            :lows (lows mbr)
                            :highs (substitute-if
                                    new-low-coordinate (constantly t)
                                    (highs mbr) :start axis :count 1)))
                  (new-high (make-rectangle
                             :lows (substitute-if
                                    new-low-coordinate (constantly t)
                                    (lows mbr) :start axis :count 1)
                             :highs (highs mbr)))
                  (cost (cost new-low new-high)))
             (when (< cost mincost)
               (setf mincost cost
                     low new-low
                     high new-high)))))))))

(defun split-node-by (tree node entries rone rtwo)
  (let ((new-node (make-node-like node)))
    (setf (children node) nil
          (children new-node) nil)
    (dolist (entry entries)
      (cond
        ((null (intersection rone (mbr entry tree)))
         (push entry (children new-node))
         (unless (typep node 'spatial-tree-leaf-node)
           (setf (parent entry) new-node)))
        ((null (intersection rtwo (mbr entry tree)))
         (push entry (children node))
         (unless (typep node 'spatial-tree-leaf-node)
           (setf (parent entry) node)))
        ((typep node 'spatial-tree-leaf-node)
         (push entry (records new-node))
         (push entry (records node)))
        (t (let ((new (split-node-by tree entry (children entry) rone rtwo)))
             (push new (children new-node))
             (setf (parent new) new-node)
             (push entry (children node))
             (setf (parent entry) node)))))
    (setf (slot-value node 'mbr) (intersection rone (minimum-bound-of (children node) tree))
          (slot-value new-node 'mbr) (intersection rtwo (minimum-bound-of (children new-node) tree)))
    new-node))

(defmethod split-node ((tree r+-tree) new node)
  (let ((entries (cons new (children node))))
    (multiple-value-bind (rone rtwo)
        (generate-partition-rectangles entries tree)
      (let ((new-node (split-node-by tree node entries rone rtwo)))
        (if (eq (root-node tree) node)
            (let ((new-root (make-instance 'spatial-tree-node
                                           :children (list node new-node))))
              (setf (root-node tree) new-root
                    (parent node) new-root
                    (parent new-node) new-root))
            (let ((pr (parent node)))
              (if (>= (length (children pr)) (max-per-node tree))
                  (split-node tree new-node pr)
                  (progn
                    (push new-node (children pr))
                    (setf (parent new-node) pr)))))))))
