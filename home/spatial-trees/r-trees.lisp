;;;; A more-or-less direct implementation of "R-TREES: A DYNAMIC INDEX
;;;; STRUCTURE FOR SPATIAL SEARCHING", Antonin Guttman, Proc. ACM
;;;; SIGMOD Int. Conf. on Management of Data, 1984.
;;;;
;;;; Differences with the algorithms described in that paper:
;;;;
;;;; * we keep the minimum bounding rectangle of a node's children in
;;;; the node itself and not in an index pointing to the node, except
;;;; for leaves where there is one extra level of indirection (to
;;;; assist in implementation of R+-trees).
;;;;
;;;; * we implement (in SPLIT-NODE) a guarantee of the stated
;;;; invariant that each node contains at least m elements.  This is
;;;; referred to in Section "4.  Performance Tests", "... decreased
;;;; cost of insertion with a stricter node balance reflects the fact
;;;; that when one group becomes too full, all split algorithms simply
;;;; put the remaining elements in the other group without further
;;;; comparisons."

(in-package "SPATIAL-TREES-IMPL")

(defgeneric mbr (thing tree))
(defmethod mbr ((n spatial-tree-node) (tree spatial-tree))
  (declare (ignore tree))
  #+nil
  (check (not (eq n (root-node tree)))
         "Root of ~S asked for its MBR" tree)
  (slot-value n 'mbr))

(defun minimum-bound-of (objects tree)
  (reduce #'minimum-bound objects :key (lambda (x) (mbr x tree))))

(defun bounding-rectangle (tree)
  (let ((root (root-node tree)))
    (if (slot-boundp root 'mbr)
        (mbr root tree)
        (minimum-bound-of (children root) tree))))

(defstruct leaf-node-entry rectangle datum)
(defmethod mbr ((o leaf-node-entry) (tree spatial-tree))
  (declare (ignore tree))
  (leaf-node-entry-rectangle o))

(defclass r-tree (spatial-tree)
  ())
(defmethod make-spatial-tree ((kind (eql :r)) &rest initargs)
  (apply #'make-instance 'r-tree
         :root-node (make-instance 'spatial-tree-leaf-node :records nil)
         initargs))

;;; 3.1. Searching
(defmethod search ((o t) (tree r-tree))
  (search (funcall (rectfun tree) o) tree))

(defmethod search ((r rectangle) (tree r-tree))
  (labels ((%search (r node)
             (cond
               ((typep node 'spatial-tree-leaf-node)
                (let (result)
                  (dolist (entry (records node) (nreverse result))
                    (when (intersectp r (leaf-node-entry-rectangle entry))
                      (push (leaf-node-entry-datum entry) result)))))
               (t
                (let (result)
                  (dolist (child (children node) result)
                    (when (intersectp r (mbr child tree))
                      (setq result (append (%search r child) result)))))))))
    (let ((root (root-node tree)))
      (%search r root))))

;;; 3.2. Insertion
(defmethod choose-leaf (r (tree r-tree))
  (labels ((%choose-leaf (r node)
             (cond
               ((typep node 'spatial-tree-leaf-node) node)
               (t (do* ((children (children node) (cdr children))
                        (child (car children) (car children))
                        (candidate child)
                        (min-extension (- (area (minimum-bound (mbr child tree) (mbr r tree)))
                                          (area (mbr child tree)))))
                       ((null children) (%choose-leaf r candidate))
                    (let* ((new-area (area (minimum-bound (mbr child tree)
                                                          (mbr r tree))))
                           (old-area (area (mbr child tree)))
                           (extension (- new-area old-area)))
                      (when (or (< extension min-extension)
                                (and (= extension min-extension)
                                     (< old-area (area (mbr candidate tree)))))
                        (setf min-extension extension
                              candidate child))))))))
    (let ((n (root-node tree)))
      (%choose-leaf r n))))

(defmethod insert ((r t) (tree r-tree))
  (let* ((r (make-leaf-node-entry :datum r
                                  :rectangle (funcall (rectfun tree) r)))
         (leaf-node (choose-leaf r tree)))
    (cond
      ((< (length (records leaf-node)) (max-per-node tree))
       (push r (records leaf-node))
       (adjust-tree tree leaf-node))
      (t
       (let ((new-node (split-node tree r leaf-node)))
         (check (<= (min-per-node tree)
                    (length (records new-node))
                    (max-per-node tree))
                "invariant (1) violated for the new node ~S in ~S"
                new-node tree)
         (check (<= (min-per-node tree)
                    (length (records leaf-node))
                    (max-per-node tree))
                "invariant (1) violated for the old node ~S in ~S"
                leaf-node tree)
         (let ((new (adjust-tree tree leaf-node new-node)))
           (when new
             (let ((new-root
                    (make-instance 'spatial-tree-node
                                   :children (list (root-node tree) new))))
               (setf (parent (root-node tree)) new-root
                     (root-node tree) new-root
                     (parent new) new-root))))))))
  tree)

(defgeneric adjust-tree (tree node &optional new))
(defmethod adjust-tree ((tree r-tree) node &optional new)
  (cond
    ((eq node (root-node tree)) new)
    (t
     (setf (slot-value node 'mbr) (minimum-bound-of (children node) tree))
     (let ((parent (parent node)))
       (if new
           (cond
             ((< (length (children parent)) (max-per-node tree))
              (push new (children parent))
              (setf (parent new) parent)
              (adjust-tree tree parent))
             (t
              (let ((new-parent (split-node tree new parent)))
                (check (<= (min-per-node tree)
                           (length (children new-parent))
                           (max-per-node tree))
                       "invariant (3) violated for the new parent node ~S in ~S"
                       new-parent tree)
                (check (<= (min-per-node tree)
                           (length (children parent))
                           (max-per-node tree))
                       "invariant (3) violated for the old parent node ~S in ~S"
                       parent tree)
                (dolist (child (children parent))
                  (setf (parent child) parent))
                (dolist (child (children new-parent))
                  (setf (parent child) new-parent))
                (adjust-tree tree parent new-parent))))
           (adjust-tree tree parent))))))

;;; 3.3. Deletion
(defmethod delete ((r t) (tree r-tree))
  (let ((leaf-node (find-leaf r (root-node tree) tree)))
    (when leaf-node
      (setf (records leaf-node) (remove r (records leaf-node) :key #'leaf-node-entry-datum))
      (condense-tree leaf-node tree)
      (unless (typep (root-node tree) 'spatial-tree-leaf-node)
        (when (null (cdr (children (root-node tree))))
          (check (car (children (root-node tree)))
                 "non-leaf root node with no children")
          (setf (root-node tree) (car (children (root-node tree))))
          (slot-makunbound (root-node tree) 'parent)
          (slot-makunbound (root-node tree) 'mbr)))
      tree)))

(defun find-leaf (obj node tree)
  (labels ((%find-leaf (leaf-entry node tree)
             (if (typep node 'spatial-tree-leaf-node)
                 (when (member (leaf-node-entry-datum leaf-entry)
                               (records node)
                               :key #'leaf-node-entry-datum)
                   (return-from find-leaf node))
                 (dolist (entry (children node))
                   (when (intersectp (mbr leaf-entry tree) (mbr entry tree))
                     (%find-leaf leaf-entry entry tree))))))
    (%find-leaf (make-leaf-node-entry :datum obj
                                      :rectangle (funcall (rectfun tree) obj))
                node
                tree)))

(defun condense-tree (node tree)
  (labels ((all-leaves-below (node)
             (if (typep node 'spatial-tree-leaf-node)
                 (records node)
                 (apply #'append
                        (mapcar #'all-leaves-below (children node))))))
    (do ((node node (parent node))
         (q nil))
        ((eq node (root-node tree))
         (dolist (orphan q)
           ;; NOTE: this interpretation (reinsert every leaf)
           ;; disagrees with BKSS (R*-trees), section 4.3, "... is
           ;; based on the ability of the insert routine to insert
           ;; entries on every level of the tree as already required
           ;; by the deletion algorithm [Gut 84]."
           (dolist (oleaf (all-leaves-below orphan))
             (insert (leaf-node-entry-datum oleaf) tree))))
      (cond
        ((< (length (children node)) (min-per-node tree))
         (setf (children (parent node)) (remove node (children (parent node))))
         (push node q))
        (t
         (setf (slot-value node 'mbr) (minimum-bound-of (children node) tree)))))))

;;; 3.5. Node Splitting
(defun d (r1 r2 tree)
  (- (area (minimum-bound (mbr r1 tree) (mbr r2 tree)))
     (area (mbr r1 tree))
     (area (mbr r2 tree))))

(defun pick-seeds (entries tree)
  (do* ((entry1 (car entries) (car entries))
        (entries (cdr entries) (cdr entries))
        (maxentry1 entry1)
        (maxentry2 (car entries))
        (maxd (d maxentry1 maxentry2 tree)))
       ((null entries) (values maxentry1 maxentry2))
    (dolist (entry2 entries)
      (when (> (d entry1 entry2 tree) maxd)
        (setf maxd (d entry1 entry2 tree)
              maxentry1 entry1
              maxentry2 entry2)))))

(defun pick-next (entries node new-node tree)
  (let* ((maxentry (car entries))
         (maxdelta (- (d maxentry new-node tree) (d maxentry node tree))))
    (dolist (entry (cdr entries) (values maxentry maxdelta))
      (let ((delta (- (d entry new-node tree) (d entry node tree))))
        (when (> (abs delta) (abs maxdelta))
          (setf maxentry entry
                maxdelta delta))))))

(defun make-node-like (node)
  (make-instance (class-of node)))

(defmethod split-node ((tree r-tree) new node)
  (let ((new-node (make-node-like node))
        (entries (cons new (children node))))
    (multiple-value-bind (s1 s2)
        (pick-seeds entries tree)
      (setf (children node) (list s1)
            (slot-value node 'mbr) (mbr s1 tree))
      (setf (children new-node) (list s2)
            (slot-value new-node 'mbr) (mbr s2 tree))
      (do ((entries (remove s1 (remove s2 entries))))
          ((null entries) new-node)
        (multiple-value-bind (maxentry maxdelta)
            (pick-next entries node new-node tree)
          (cond
            ((or (plusp maxdelta)
                 (and (zerop maxdelta)
                      (or (< (area (mbr node tree)) (area (mbr new-node tree)))
                          (and (= (area (mbr node tree)) (area (mbr new-node tree)))
                               (> (length (children new-node))
                                  (length (children node)))))))
             (push maxentry (children node))
             (setf (slot-value node 'mbr) (minimum-bound-of (children node) tree)))
            (t
             (push maxentry (children new-node))
             (setf (slot-value new-node 'mbr)
                   (minimum-bound-of (children new-node) tree))))
          (setf entries (remove maxentry entries))
          ;; the extra bit (see notes at head of file)
          (when (= (length entries)
                   (- (min-per-node tree) (length (children node))))
            (setf (children node) (append entries (children node))
                  (slot-value node 'mbr) (minimum-bound-of (children node) tree))
            (return new-node))
          (when (= (length entries)
                   (- (min-per-node tree) (length (children new-node))))
            (setf (children new-node) (append entries (children new-node))
                  (slot-value new-node 'mbr) (minimum-bound-of (children new-node) tree))
            (return new-node)))))))
