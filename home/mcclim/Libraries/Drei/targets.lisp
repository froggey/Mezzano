;;; -*- Mode: Lisp; Package: DREI-CORE -*-

;;;  (c) copyright 2007 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

;;; Facilities and protocols for iterating through view objects, the
;;; point being that the view may be magically exchanged for some
;;; other view, permitting easy iteration through multiple views as a
;;; single sequence. This is meant to support Climacs' Group-facility,
;;; I'm not sure what else it could be used for.

(in-package :drei-core)

(defclass target-specification ()
  ((%drei :reader drei-instance-of
          :initarg :drei-instance
          :initform (error "A Drei instance must be provided for a target specification")))
  (:documentation "The base class for target specifications,
objects that permit browsing through targets for various
operations. `Target-specification' instances start off
deactivated."))

(defgeneric activate-target-specification (target-specification)
  (:documentation "Cause the Drei instance associated with
`target-specification' to switch to the \"current\" target of
`target-specification', whatever that is. It is illegal to call
any other target function on a `target-specification' object
until it has been activated by this function, and it is illegal
to call this function on an already activated
`target-specification' instance."))

(defgeneric deactivate-target-specification (target-specification)
  (:documentation "Deactivate the `target-specification'
instance, restoring whatever state the call to
`activate-target-specification' modified. It is illegal to call
`deactivate-target-specification' on a deactivated
`target-specification' instance."))

(defgeneric subsequent-targets-p (target-specification)
  (:documentation "Return true if there are more targets to act
on, that is, if the `next-target' function would not signal an
error."))

(defgeneric preceding-targets-p (target-specification)
  (:documentation "Return true if there are targets to act on in
sequence before the current target, that is, if the
`previous-target' function would not signal an error."))

(defgeneric next-target (target-specification)
  (:documentation "Change to the next target specified by the
target specification. Signals an error of type `no-more-targets'
if `subsequent-targets-p' is false."))

(defgeneric previous-target (target-specification)
  (:documentation "Change to the previous target specified by the
target specification. Signals an error of type `no-more-targets'
if `preceding-targets-p' is false."))

(define-condition no-more-targets (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
	     (format stream "No more targets available for iteration")))
  (:documentation "Signal that there are no more targets
available for iteration, either forward or backwards in the
sequence of targets."))

(defclass current-view-target (target-specification)
  ((%view :accessor view))
  (:documentation "A target specification class specifying just
one view, the current view of the Drei instance at the time of
object creation. This is mostly used as a dummy target
specification to make target-aware commands behave \"normally\"
when no particular targets are specified."))

(defmethod initialize-instance :after ((obj current-view-target) &rest initargs)
  (declare (ignore initargs))
  (setf (view obj) (view (drei-instance obj))))

(defmethod activate-target-specification ((spec current-view-target))
  ;; Noop.
  )

(defmethod deactivate-target-specification ((spec current-view-target))
  ;; Noop.
  )

(defmethod subsequent-targets-p ((spec current-view-target))
  nil)

(defmethod preceding-targets-p ((spec current-view-target))
  nil)

(defmethod next-target ((spec current-view-target))
  (error 'no-more-targets))

(defmethod previous-target ((spec current-view-target))
  (error 'no-more-targets))

(defvar *default-target-creator* #'(lambda (drei)
                                     (make-instance 'current-view-target
                                      :drei-instance drei))
  "A function of a single argument, the Drei instance, that
creates a target specification object (or subtype thereof) that
should be used for aquiring targets.")

(defclass view-list-target-specification (target-specification)
  ((%views :accessor views
           :initform '()
           :initarg :views)
   (%view-count :accessor view-count)
   (%current-view-index :initform 0
                        :accessor current-view-index)
   (%original-view :accessor original-view
                   :initform nil)
   (%original-offset :accessor original-offset
                     :initform nil))
  (:documentation "A target specification that has a provided
list of existing views as its target."))

(defun reorder-views (list-spec)
  "Reorder the views of a `view-list-target-specification' object
to be more useful. If the current view is in the list of views,
it is moved to the head of the list, since it makes sense to make
it the starting point."
  (unless (eq (view (drei-instance list-spec)) (first (views list-spec)))
    (let ((filtered-views (remove (view (drei-instance list-spec)) (views list-spec))))
      (when (/= (length filtered-views) (view-count list-spec))
        (setf (views list-spec) (cons (view (drei-instance list-spec)) filtered-views))))))

(defmethod initialize-instance :after ((obj view-list-target-specification)
                                       &rest initargs)
  (declare (ignore initargs))
  (setf (view-count obj) (length (views obj)))
  (reorder-views obj))

(defmethod (setf views) :after (new-views (obj view-list-target-specification))
  (setf (view-count obj) (length (views obj)))
  (reorder-views obj))

(defmethod activate-target-specification ((spec view-list-target-specification))
  (unless (or (null (views spec))
              (eq (view (drei-instance spec)) (first (views spec))))
    (setf (original-view spec) (view (drei-instance spec))
          (original-offset spec) (offset (point (view (drei-instance spec))))
          (view (drei-instance spec)) (first (views spec)))
    (beginning-of-buffer (point (view (drei-instance spec))))))

(defmethod deactivate-target-specification ((spec view-list-target-specification))
  (when (original-view spec)
    (setf (view (drei-instance spec)) (original-view spec)
          (offset (point (view (drei-instance spec)))) (original-offset spec))))

(defmethod subsequent-targets-p ((spec view-list-target-specification))
  (/= (1+ (current-view-index spec)) (view-count spec)))

(defmethod preceding-targets-p ((spec view-list-target-specification))
  (plusp (current-view-index spec)))

(defmethod next-target ((spec view-list-target-specification))
  (if (subsequent-targets-p spec)
      (progn
        (setf (view (drei-instance spec))
              (elt (views spec) (incf (current-view-index spec)))
              (active (view (drei-instance spec))) t)
        (beginning-of-buffer (point (view (drei-instance spec)))))
      (error 'no-more-targets)))

(defmethod previous-target ((spec view-list-target-specification))
  (if (preceding-targets-p spec)
      (progn
        (setf (view (drei-instance spec))
              (elt (views spec) (decf (current-view-index spec))))
        (end-of-buffer (point (view (drei-instance spec))))
        (active (view (drei-instance spec))) t)
      (error 'no-more-targets)))
