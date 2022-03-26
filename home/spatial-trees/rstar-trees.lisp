;;; Modifications to the basic R-tree following "The R*-tree: An
;;; Efficient and Robust Access Method for Points and Rectangles",
;;; Beckmann, Kriegel, Schneider and Seeger, Proc. ACM Int. Conf. on
;;; Management of Data, 1990

(in-package "SPATIAL-TREES-IMPL")

(defclass r*-tree (r-tree)
  ((removed-number :initarg :removed-number :accessor removed-number)))
(defmethod initialize-instance :after ((tree r*-tree) &rest args)
  (declare (ignore args))
  (unless (slot-boundp tree 'removed-number)
    (setf (slot-value tree 'removed-number)
          (round (* (max-per-node tree) 3/10)))))
(defmethod make-spatial-tree ((kind (eql :r*)) &rest initargs)
  (apply #'make-instance 'r*-tree
         :root-node (make-instance 'spatial-tree-leaf-node :records nil)
         initargs))

;;; 4.1 Algorithm ChooseSubtree
(defun overlap (x others tree)
  (loop for y in others
        sum (let ((i (intersection x (mbr y tree))))
              (if i (area i) 0))))

(defun height (tree)
  (labels ((height-above (node)
             (if (typep node 'spatial-tree-leaf-node)
                 0
                 (1+ (height-above (car (children node)))))))
    (height-above (root-node tree))))

(defun choose-subtree (thing node level tree)
  (cond
    ((= level 0) node)
    ((typep (car (children node)) 'spatial-tree-leaf-node)
     (check (= level 1) "Search in CHOOSE-SUBTREE too deep")
     ;; NOTE: does not contain the probabilistic optimization near the
     ;; end of section 4.1
     (do* ((children (children node) (cdr children))
           (child (car children) (car children))
           (candidate child)
           (min-overlap-extension
            (- (overlap (minimum-bound (mbr thing tree) (mbr child tree))
                        (remove child (children node))
                        tree)
               (overlap (mbr child tree)
                        (remove child (children node))
                        tree))))
          ((null children)
           (check (typep candidate 'spatial-tree-leaf-node)
                  "CHOOSE-SUBTREE candidate ~S is not a leaf node" candidate)
           candidate)
       (let* ((new-overlap (overlap (minimum-bound (mbr thing tree) (mbr child tree))
                                    (remove child (children node))
                                    tree))
              (old-overlap (overlap (mbr child tree)
                                    (remove child (children node))
                                    tree))
              (extension (- new-overlap old-overlap)))
         (when (or (< extension min-overlap-extension)
                   (and (= extension min-overlap-extension)
                        (< (- (area (minimum-bound (mbr thing tree) (mbr child tree)))
                              (area (mbr child tree)))
                           (- (area (minimum-bound (mbr thing tree) (mbr candidate tree)))
                              (area (mbr candidate tree))))))
           (setf min-overlap-extension extension
                 candidate child)))))
    (t (do* ((children (children node) (cdr children))
             (child (car children) (car children))
             (candidate child)
             (min-extension (- (area (minimum-bound (mbr child tree) (mbr thing tree)))
                               (area (mbr child tree)))))
            ((null children) (choose-subtree thing candidate (1- level) tree))
         (let* ((new-area (area (minimum-bound (mbr child tree) (mbr thing tree))))
                (old-area (area (mbr child tree)))
                (extension (- new-area old-area)))
           (when (or (< extension min-extension)
                     (and (= extension min-extension)
                          (< old-area (area (mbr candidate tree)))))
             (setf min-extension extension
                   candidate child)))))))
     
(defmethod choose-leaf (r (tree r*-tree))
  (choose-subtree r (root-node tree) (height tree) tree))

;;; 4.2 Split of the R*-tree
(defun margin (rectangle)
  (let ((result 0))
    (do ((lows (lows rectangle) (cdr lows))
         (highs (highs rectangle) (cdr highs)))
        ((null lows) result)
      (incf result (- (car highs) (car lows))))))

(defun choose-split-axis (entries tree)
  (let ((max-per-node (max-per-node tree))
        (min-per-node (min-per-node tree)))
    (do ((lows (lows (mbr (car entries) tree)) (cdr lows))
         (axis 0 (1+ axis))
         (min-margin)
         (min-axis 0))
        ((null lows) min-axis)
      (let ((sort-by-low (sort (copy-list entries) #'<
                               :key (lambda (x) (nth axis (lows (mbr x tree))))))
            (sort-by-high (sort (copy-list entries) #'<
                                :key (lambda (x) (nth axis (highs (mbr x tree)))))))
        (do ((k 1 (1+ k)))
            ((> k (- (+ 2 max-per-node) (* 2 min-per-node))))
          (cond
            ((null min-margin)
             (setf min-margin (min (+ (margin (minimum-bound-of (subseq sort-by-low 0 (+ min-per-node k -1)) tree))
                                      (margin (minimum-bound-of (subseq sort-by-low (+ min-per-node k -1)) tree)))
                                   (+ (margin (minimum-bound-of (subseq sort-by-high 0 (+ min-per-node k -1)) tree))
                                      (margin (minimum-bound-of (subseq sort-by-high (+ min-per-node k -1)) tree))))))
            (t (let ((min (min (+ (margin (minimum-bound-of (subseq sort-by-low 0 (+ min-per-node k -1)) tree))
                                  (margin (minimum-bound-of (subseq sort-by-low (+ min-per-node k -1)) tree)))
                               (+ (margin (minimum-bound-of (subseq sort-by-high 0 (+ min-per-node k -1)) tree))
                                  (margin (minimum-bound-of (subseq sort-by-high (+ min-per-node k -1)) tree))))))
                 (when (< min min-margin)
                   (setf min-margin min
                         min-axis axis))))))))))

(defun choose-split-index (entries axis tree)
  (let ((max-per-node (max-per-node tree))
        (min-per-node (min-per-node tree)))
    (let ((one) (two)
          ;; this is a safe upper bound to the minimum overlap value
          (min-overlap-value (area (minimum-bound-of entries tree))))
      (let ((sort-by-low (sort (copy-list entries) #'<
                               :key (lambda (x) (nth axis (lows (mbr x tree))))))
            (sort-by-high (sort (copy-list entries) #'<
                                :key (lambda (x) (nth axis (highs (mbr x tree)))))))
        (do ((k 1 (1+ k)))
            ((> k (- (+ 2 max-per-node) (* 2 min-per-node))) (values one two))
          (let ((a (subseq sort-by-low 0 (+ min-per-node k -1)))
                (b (subseq sort-by-low (+ min-per-node k -1))))
            (let* ((i (intersection (minimum-bound-of a tree)
                                    (minimum-bound-of b tree)))
                   (overlap-value (if i (area i) 0)))
              (when (or (< overlap-value min-overlap-value)
                        (and (= overlap-value min-overlap-value)
                             (< (+ (area (minimum-bound-of a tree))
                                   (area (minimum-bound-of b tree))))))
                (setf min-overlap-value overlap-value
                      one a
                      two b))))
          (let ((a (subseq sort-by-high 0 (+ min-per-node k -1)))
                (b (subseq sort-by-high (+ min-per-node k -1))))
            (let* ((i (intersection (minimum-bound-of a tree)
                                    (minimum-bound-of b tree)))
                   (overlap-value (if i (area i) 0)))
              (when (or (< overlap-value min-overlap-value)
                        (and (= overlap-value min-overlap-value)
                             (< (+ (area (minimum-bound-of a tree))
                                   (area (minimum-bound-of b tree))))))
                (setf min-overlap-value overlap-value
                      one a
                      two b)))))))))

(defmethod split-node ((tree r*-tree) new node)
  (let ((new-node (make-node-like node))
        (entries (cons new (children node))))
    (let ((axis (choose-split-axis entries tree)))
      (multiple-value-bind (one two)
          (choose-split-index entries axis tree)
        (setf (children node) one
              (slot-value node 'mbr) (minimum-bound-of one tree))
        (setf (children new-node) two
              (slot-value new-node 'mbr) (minimum-bound-of two tree))
        new-node))))

;;; 4.3 Forced Reinsert
(defvar *data-rectangle*)
(defvar *overflowed-levels* nil)

(defun reinsert (thing tree node level)
  (let* ((entries (cons thing (children node)))
         (mbr (minimum-bound-of entries tree))
         (cmbr (mapcar #'- (highs mbr) (lows mbr)))
         (sorted (sort (copy-list entries) #'>
                       :key (lambda (entry)
                              (let* ((r (mbr entry tree))
                                     (cs (mapcar #'- (highs r) (lows r))))
                                (loop for cm in cmbr
                                      for c in cs
                                      ;; squared distance is fine
                                      sum (expt (- cm c) 2)))))))
    (let ((removed (subseq sorted 0 (removed-number tree))))
      (setf (children node) (subseq sorted (removed-number tree))
            (slot-value node 'mbr) (minimum-bound-of (children node) tree))
      (dolist (entry (reverse removed))
        (%insert entry tree level)))))

(defun overflow-treatment (thing tree node level)
  (if (member level *overflowed-levels*)
      (split-node tree thing node)
      (let ((*overflowed-levels* (cons level *overflowed-levels*)))
        (reinsert thing tree node level))))

(defun %insert (thing tree level)
  (let ((node (choose-subtree thing (root-node tree) level tree)))
    (cond
      ((< (length (children node)) (max-per-node tree))
       (push thing (children node))
       (adjust-tree tree node))
      (t
       (let ((new-node (overflow-treatment thing tree node level)))
         (let ((new (adjust-tree tree node new-node)))
           (when new
             (let ((new-root
                    (make-instance 'spatial-tree-node
                                   :children (list (root-node tree) new))))
               (setf (parent (root-node tree)) new-root
                     (root-node tree) new-root
                     (parent new) new-root)))))))))

(defmethod insert ((r t) (tree r*-tree))
  (let ((*data-rectangle* r))
    (%insert (make-leaf-node-entry :datum r
                                   :rectangle (funcall (rectfun tree) r))
             tree
             (height tree)))
  tree)
