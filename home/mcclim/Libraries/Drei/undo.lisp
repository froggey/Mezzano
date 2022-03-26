;;; -*- Mode: Lisp; Package: DREI-UNDO -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)

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

;;; General-purpose undo module

(in-package :drei-undo)

(defgeneric add-undo (undo-record undo-tree)
  (:documentation "Add an undo record to the undo tree below the
current state, and set the current state to be below the transition
represented by the undo record."))

(defgeneric flip-undo-record (undo-record)
  (:documentation "This function is called by the undo module whenever
the current state is changed from its current value to that of the
parent state (presumably as a result of a call to undo) or to that of
one of its child states.

Client code is required to supply methods for this function on
client-specific subclasses of `undo-record'."))

(defgeneric undo (undo-tree &optional n)
  (:documentation "Move the current state `n' steps up the undo
tree and call `flip-undo-record' on each step.  If the current
state is at a level less than `n', a `no-more-undo' condition is
signaled and the current state is not moved (and no calls to
`flip-undo-record' are made).

As long as no new record are added to the tree, the undo module
remembers which branch it was in before a sequence of calls to undo."))

(defgeneric redo (undo-tree &optional n)
  (:documentation "Move the current state `n' steps down the
remembered branch of the undo tree and call `flip-undo-record' on
each step.  If the remembered branch is shorter than `n', a
`no-more-undo' condition is signaled and the current state is not
moved (and no calls to `flip-undo-record' are made)."))

(define-condition no-more-undo (simple-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "No more undo")))
  (:documentation "A condition of this type is signaled whenever
an attempt is made to call undo when the application is in its
initial state."))

(defclass undo-tree () ()
  (:documentation "The base class for all undo trees."))

(defclass standard-undo-tree (undo-tree)
  ((current-record :accessor current-record)
   (leaf-record :accessor leaf-record)
   (redo-path :initform '() :accessor redo-path)
   (children :initform '() :accessor children)
   (depth :initform 0 :reader depth))
  (:documentation "The base class for all undo records.

Client code typically derives subclasses of this class that are
specific to the application."))

(defmethod initialize-instance :after ((tree standard-undo-tree) &rest args)
  (declare (ignore args))
  (setf (current-record tree) tree
	(leaf-record tree) tree))

(defclass undo-record () ()
  (:documentation "The base class for all undo records."))

(defgeneric undo-tree (record)
  (:documentation "The undo tree to which the undo record
`record' belongs."))

(defclass standard-undo-record (undo-record)
  ((parent :initform nil :accessor parent)
   (tree :initform nil
         :accessor undo-tree
         :documentation "The undo tree to which the undo record
belongs.")
   (children :initform '() :accessor children)
   (depth :initform nil :accessor depth))
  (:documentation "Standard instantiable class for undo records."))

(defmethod add-undo ((record standard-undo-record) (tree standard-undo-tree))
  (push record (children (current-record tree)))
  (setf (undo-tree record) tree
	(parent record) (current-record tree)
	(depth record) (1+ (depth (current-record tree)))
	(current-record tree) record
	(leaf-record  tree) record
	(redo-path tree) '()))

(defmethod undo ((tree standard-undo-tree) &optional (n 1))
  (assert (<= n (depth (current-record tree)))
	  ()
	  (make-condition 'no-more-undo))
  (loop repeat n
	do (flip-undo-record (current-record tree))
	   (push (current-record tree) (redo-path tree))
	   (setf (current-record tree) (parent (current-record tree)))))

(defmethod redo ((tree standard-undo-tree) &optional (n 1))
  (assert (<= n (- (depth (leaf-record tree))
		   (depth (current-record tree))))
	  ()
	  (make-condition 'no-more-undo))
  (loop repeat n
	do (setf (current-record tree) (pop (redo-path tree)))
	   (flip-undo-record (current-record tree))))
