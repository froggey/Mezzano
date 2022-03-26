;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;---------------------------------------------------------------------------
;;; Copyright (c) 2005-2007 Robert P. Goldman and Smart Information
;;; Flow Technologies, d/b/a SIFT, LLC
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.
;;;
;;; All rights reserved.
;;;
;;;---------------------------------------------------------------------------
;;; File Description:
;;;
;;; File for definitions of a new graph type that should allow tree
;;; style layouts with edges across in a level. [2005/05/05:rpg]
;;;
;;; History/Bugs/Notes:
;;;
;;;   [2005/05/05:rpg] Created.
;;;
;;;---------------------------------------------------------------------------

(in-package "CLIM-INTERNALS")

;;;---------------------------------------------------------------------------
;;; A graph with cross trees will have an additional type option: a
;;; cross-edge-producer
;;;---------------------------------------------------------------------------


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-graph-type :tree-with-cross-edges cross-tree-output-record))

(defun standard-cross-arc-drawer (stream from-node to-node x1 y1 x2 y2
				  &rest drawing-options
				  &key edge-type &allow-other-keys)
  "The standard cross-arc-drawer simply ignores the edge-type keyword argument."
  (declare (ignore edge-type))
  (remf drawing-options :edge-type)
  (apply #'standard-arc-drawer stream from-node to-node x1 y1 x2 y2 drawing-options))

(defclass cross-tree-output-record (tree-graph-output-record)
     ((cross-arc-drawer
       :initarg :cross-arc-drawer
       :reader cross-arc-drawer
       :documentation
       "This slot should be bound to a function that 
takes all the arguments accepted by a normal arc-drawer,
but also an edge-type keyword argument, which it is free
to ignore."
       :initform #'standard-cross-arc-drawer
       )
      (cross-arc-producer
       :initarg :cross-arc-producer
       ;; by default, this just acts like a tree...
       :initform nil 
       :reader cross-arc-producer
       :documentation
       "This should be bound to a function that 
takes a graph-node as argument, like inferior-producer,
but that returns two values:  a list of destination
nodes and (optionally) a list of type-designators, that
can be passed to the cross-arc-drawer, as the value of the
:edge-type keyword argument."
       )
      (cross-arc-drawing-options
       :reader cross-arc-drawing-options
       )
      )
  )

;;;---------------------------------------------------------------------------
;;; This is very yucky.  It will be expensive on large graphs (perhaps
;;; a mixin for using a hash-table would be better), and needs some
;;; kind of good way of specifying the test in your graph class, which
;;; will be difficult... [2005/05/06:rpg]
;;;---------------------------------------------------------------------------

(defmethod lookup-node (source-node (graph graph-output-record)
			&key (test #'eql)
			     (default :error))
  (let ((hash-table (make-hash-table :test #'eq)))
    (flet ((visitedp (node)
	     (gethash node hash-table nil))
	   (mark (node)
	     (setf (gethash node hash-table) t)))
      (or
       (loop with openlist = (graph-root-nodes graph)
	   for node = (pop openlist)
	   while node
	   unless (visitedp node)
	   when (funcall test source-node (graph-node-object node))
	   return node
	   end
	   and do (mark node)
		  (setf openlist (append openlist (graph-node-children node))))
       (when (eq default :error)
	 (error "Unable to find graph node for ~S in ~S"
		source-node graph))
       default))))
	    
(defmethod initialize-instance :after ((obj cross-tree-output-record) &key cross-arc-drawing-options
				       arc-drawing-options)
  "A possibly reasonable default is to draw cross-arcs as if they were 
normal tree edge arcs."
  (unless cross-arc-drawing-options
    (setf (slot-value obj 'cross-arc-drawing-options)
	  arc-drawing-options)))

;;; note that this could later be made into a function argument, so
;;; that programmers could customize [2005/05/06:rpg]
(defgeneric cross-arc-routing (from to orientation)
  (:documentation "Return four values, x1, y1, x2, y2 for
the arc-drawing for a cross-arc.  More complex than for
the tree case."))

(defun middle (dim1 dim2)
  (/ (+ dim1 dim2) 2))
  
(defmethod cross-arc-routing (from to (orientation (eql :horizontal)))
  (with-bounding-rectangle* (x1 y1 x2 y2) from
    (with-bounding-rectangle* (u1 v1 u2 v2) to
      (cond ((< x2 u1)
	     ;; node entirely to the left of k
	     (values x2 (middle y1 y2) u1 (middle v1 v2)))
	    ((< u2 x1)
	     ;; node entirely to the right of k
	     ;; draw from the top or bottom to make distinguishable...
	     (if (<= v1 y1)
		 ;; draw from the top to the x middle of TO on the
		 ;; bottom
		 (values x1 y1 (middle u1 u2) v2)
	       ;; draw from the bottom to the x middle of TO on the
	       ;; top...
	       (values x1 y2 (middle u1 u2) v1)))
	    ;; overlapping in X -- as long as this is a tree, means
	    ;; they are siblings.
	    ((< y2 v1)
	     ;; FROM above: middle x of FROM to middle x of TO, bottom to top...
	     (values (middle x1 x2) y2 (middle u1 u2) v1))
	    ((< v2 y1)
	     ;; TO above: middle x of FROM to middle x of TO, top to bottom...
	     (values (middle x1 x2) y1 (middle u1 u2) v2))
	    (t
	     (error "Unforeseen node positioning."))))))

;;; copied from original layout-graph-edges and enhanced to add cross
;;; edges.
(defmethod layout-graph-edges :after ((graph cross-tree-output-record)
				      stream arc-drawer arc-drawing-options)
  "After the main method has drawn the tree, add the cross-edges."
  (declare (ignore arc-drawer arc-drawing-options))
;;;  (format excl:*initial-terminal-io* "~&Invoking after method to layout cross-edges.~%")
;;;  (unless (cross-arc-producer graph)
;;;    (format excl:*initial-terminal-io* "~&Uh-oh!  No cross-arc-producer!~%"))
  (with-slots (orientation) graph
    ;; We tranformed the position of the nodes when we inserted them into
    ;; output history, so the bounding rectangles queried below will be
    ;; transformed. Therefore, disable the transformation now, otherwise
    ;; the transformation is effectively applied twice to the edges.
    (when (cross-arc-producer graph)
      (with-identity-transformation (stream)
	;; for some damn reason, this graph traversal isn't working....
	(traverse-graph-nodes graph
			      (lambda (node children continuation)
;;;				(format excl:*initial-terminal-io*
;;;					"~&Invoking traverse function on ~S and ~S!~%" node children)
				(unless (eq node graph)
				  (multiple-value-bind (source-siblings types)
				      (funcall (cross-arc-producer graph)
					       (graph-node-object node))
				    ;; there's a kind of odd loop here
				    ;; because types might be NIL.  Using
				    ;; a built-in stepper would cause the
				    ;; loop to terminate too soon if types
				    ;; was nil [2005/05/06:rpg]
				    (loop for ss in source-siblings
					for k = (lookup-node ss graph)
					for typelist = types then (cdr typelist)
					for type = (when typelist (car typelist))
					do (multiple-value-bind (fromx fromy tox toy)
					       (cross-arc-routing node k orientation)
					     (apply (cross-arc-drawer graph) stream node k
						    fromx fromy 
						    tox  toy
						    :edge-type type
						    (cross-arc-drawing-options graph))))))
				(map nil continuation children)))))))