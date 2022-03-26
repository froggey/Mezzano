(in-package :clim-user)

;;;; spatial-tree Visualization Toy.  Mostly by Andy Hefner; some
;;;; modifications by Christophe Rhodes

;; For best results, use a McCLIM newer than Nov.11, 2004  :)

(define-presentation-type spatial-tree-node ())
(define-presentation-type entry ())

(define-application-frame spatial-tree-viz ()
  ((tree :initarg :tree :reader tree)
   (scale :initarg :scale :initform 200 :accessor scale)
   (expanded-nodes :initform (make-hash-table) :reader expanded-nodes))
  (:panes (hierarchy-pane (make-pane 'application-pane
                                     :display-time t
                                     :end-of-page-action :allow
                                     :end-of-line-action :allow
                                     :text-style (make-text-style :sans-serif :roman :normal)
                                     :display-function 'print-tree-hierarchy))
          (inspect        (make-pane 'application-pane :display-time nil
                                     :end-of-page-action :allow
                                     :end-of-line-action :allow))
          (viz            (make-pane 'application-pane :display-time t                                     
                                     :display-function 'draw-layout))
          (zoom-in        (make-pane 'push-button :label "Zoom In"
                                     :activate-callback 'zoom-in))
          (zoom-out       (make-pane 'push-button :label "Zoom Out"
                                     :activate-callback 'zoom-out)))
  (:command-table (spatial-tree-viz))
  (:pointer-documentation t)  
  (:layouts
   (default
       (vertically ()
         (horizontally ()
           (labelling (:label "Hierarchy")
             (scrolling (:scroll-bars :vertical)
               hierarchy-pane))
           (make-pane 'clim-extensions:box-adjuster-gadget)
           (labelling (:label "Layout" :width +fill+)
             (vertically ()
               (scrolling (:suggested-width 500 :suggested-height 500)
                 viz)
               (horizontally () zoom-in zoom-out))))
         (make-pane 'clim-extensions:box-adjuster-gadget)
         (labelling (:label "Details")
           (scrolling (:suggested-width 600)
             inspect))))))

;;; Display Code

(defun print-tree-node (frame pane node &key (indent 0))
  (indenting-output (pane indent)
    (etypecase node
      (spatial-trees-protocol:spatial-tree-node
       (with-output-as-presentation (pane node 'spatial-tree-node)
         (format pane "~A (~A children)~%" (type-of node) (length (spatial-trees-protocol:children node)))))
      (spatial-trees-impl::leaf-node-entry
       ;; FIXME: this should also be presented as the object in the
       ;; LEAF-NODE-ENTRY-DATUM slot
       (with-output-as-presentation (pane node 'entry)
         (multiple-value-call #'format pane
                              "Rectangle (~1,2F,~1,2F)-(~1,2F,~1,2F)~%"
                              (rect* (spatial-trees-impl::leaf-node-entry-rectangle node))))))
    (when (gethash node (expanded-nodes frame))
      (dolist (child (spatial-trees-protocol:children node))
        (print-tree-node frame pane child :indent (+ indent 16))))))

(defun print-tree-hierarchy (frame pane)
  (print-tree-node frame pane (spatial-trees-protocol:root-node (tree frame))))

(defun rect* (rectangle)
  (values
   (first (rectangles:lows rectangle)) (second (rectangles:lows rectangle))
   (first (rectangles:highs rectangle)) (second (rectangles:highs rectangle))))
   
(defun draw-layout (frame pane &optional (node (tree frame)))
  (etypecase node
    (spatial-trees-protocol:spatial-tree
     (with-room-for-graphics (pane :first-quadrant nil)     
       (with-scaling (pane (scale frame))
         (draw-layout frame pane (spatial-trees-protocol:root-node node))))
     (change-space-requirements pane               ;; FIXME: McCLIM should do this itself.
                                :width  (bounding-rectangle-width (stream-output-history pane))
                                :height (bounding-rectangle-height (stream-output-history pane))))
    (spatial-trees-protocol:spatial-tree-leaf-node
     (dolist (child (spatial-trees-protocol:records node))
       (draw-layout frame pane child))
     (when (slot-boundp node 'spatial-trees-impl::mbr)
       (multiple-value-call #'draw-rectangle*
         pane (rect* (slot-value node 'spatial-trees-impl::mbr))
         :ink +red+ :filled nil)))
    (spatial-trees-protocol:spatial-tree-node
     (dolist (child (spatial-trees-protocol:children node))
       (draw-layout frame pane child))
     (when (slot-boundp node 'spatial-trees-impl::mbr)
       (multiple-value-call #'draw-rectangle*
         pane (rect* (slot-value node 'spatial-trees-impl::mbr))
         :ink +black+ :filled nil)))
    (spatial-trees-impl::leaf-node-entry
     (with-output-as-presentation (pane node 'entry)
       (multiple-value-call #'draw-rectangle*
       pane (rect* (spatial-trees-impl::leaf-node-entry-rectangle node))
       :ink +blue+ :filled nil :line-dashes #(1 1))))))

;;; Callbacks

(defun zoom-in (pane)
  (declare (ignore pane))
  (setf (scale *application-frame*)
        (* 2 (scale *application-frame*)))
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'viz) :force-p t))

(defun zoom-out (pane)
  (declare (ignore pane))
  (setf (scale *application-frame*)
        (/ (scale *application-frame*) 2))
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'viz) :force-p t))

;;; Commands

(define-spatial-tree-viz-command (com-toggle-node :name "Toggle Expand Node")
    ((node 'spatial-tree-node :prompt :node :gesture :select))
  (if (gethash node (expanded-nodes *application-frame*))
      (remhash node (expanded-nodes *application-frame*))
      (setf (gethash node (expanded-nodes *application-frame*)) t))
  (setf (pane-needs-redisplay (get-frame-pane *application-frame* 'hierarchy-pane)) t))

(define-spatial-tree-viz-command (com-describe-node :name "Describe Node")
    ((node 'spatial-tree-node :prompt :node :gesture :describe))
  (describe node (get-frame-pane *application-frame* 'inspect)))

(define-spatial-tree-viz-command (com-describe-entry :name "Describe Entry")
    ((node 'entry :prompt :node :gesture :describe))
  (describe node (get-frame-pane *application-frame* 'inspect)))

;;; Foo

(defun inspect-spatial-tree (tree)
  (run-frame-top-level
   (make-application-frame 'spatial-tree-viz
                           :tree tree :pretty-name "Spatial Tree Visualizer")))

(defun make-random-rectangle (&optional (x-bias 0.0) (y-bias 0.0))
  (let* ((lx (+ (random 1.0) x-bias))
         (ly (+ (random 1.0) y-bias))
         (hx (+ (random 1.0) lx))
         (hy (+ (random 1.0) ly)))
    (rectangles:make-rectangle :lows (list lx ly) :highs (list hx hy))))

(defun test-inspect-spatial-tree (&optional (kind :r))
  "kind: one of '(:r :greene :r* :x)"
  (let* ((list (loop repeat 1000 collect
                    (make-random-rectangle)))
         (tree (spatial-trees:make-spatial-tree kind :rectfun #'identity)))
    (dolist (r list)
      (spatial-trees:insert r tree))
    (inspect-spatial-tree tree)))

(test-inspect-spatial-tree)