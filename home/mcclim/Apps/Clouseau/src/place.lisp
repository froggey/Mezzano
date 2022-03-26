;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clouseau)

;;; `basic-place'

(defclass basic-place ()
  ((%parent    :initarg  :parent
               :reader   parent)
   (%children  :accessor children
               :initform '())
   (%container :initarg  :container
               :reader   container)
   (%cell      :initarg  :cell
               :reader   cell)
   (%state     :initarg  :state
               :accessor state
               :initform nil))
  (:default-initargs
   :parent (error "missing required initarg ~S for class ~S"
                  :parent 'basic-place)))

(defmethod print-object ((object basic-place) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (ignore-errors (princ (cell object) stream))))

(defmethod ensure-child ((container t)
                         (cell      t)
                         (class     t)
                         (place     basic-place)
                         (thunk     function))
  ;; Lookup: container -> cell -> class
  ;; * There will typically be a single container, so we use an alist
  ;;   which will typically have a single entry.
  ;; * There can be many cells within a container, so we use a
  ;;   hash-table for that step.
  ;; * There will typically be a single class for each cell, so we use
  ;;   another alist which will typically have a single entry.
  (let ((cell-table (or (assoc-value (children place) container :test #'eql)
                        (setf (assoc-value (children place) container :test #'eql)
                              (make-hash-table :test #'eql)))))
    (symbol-macrolet ((class-table (gethash cell cell-table)))
      (or (assoc-value class-table class :test #'eq)
          (setf (assoc-value class-table class :test #'eq)
                (funcall thunk))))))

(defmethod ensure-state ((object t) (place basic-place) (thunk function))
  (let ((existing (state place)))
    (if (and existing (state-applicable-p existing object place))
        existing
        (setf (state place) (funcall thunk)))))

(defmethod supportsp ((place basic-place) (operation (eql 'setf)))
  t)

(defmethod supportsp ((place basic-place) (operation (eql 'remove-value)))
  nil)

(defmethod accepts-value-p ((place basic-place) (value t))
  t)

(defmethod valuep ((place basic-place))
  t)

(defmethod note-changed ((place basic-place))
  (when-let ((parent (parent place)))
    (note-changed parent)))

;;; `read-only-place'

(defclass read-only-place (basic-place)
  ())

(defmethod supportsp ((place read-only-place) (operation (eql 'setf)))
  nil)

;;; `sequence-element-place'

(defclass sequence-element-place (basic-place)
  ())

(defmethod value ((place sequence-element-place))
  (elt (container place) (cell place)))

(defmethod (setf value) ((new-value t) (place sequence-element-place))
  (setf (elt (container place) (cell place)) new-value))

;;; `key-value-place'
;;;
;;; A place that represents either the key or the value of a key-value
;;; pair. Examples include hash-table entries and alist elements.

(defclass key-value-place (basic-place)
  ())

;;; `key-place'

(defclass key-place (key-value-place)
  ())

;;; `value-place'

(defclass value-place (key-value-place)
  ())

;;; `function-backed-place'

(defclass function-backed-place (basic-place)
  ((%cell :type symbol)))

(defmethod value ((place function-backed-place))
  (funcall (cell place) (container place)))

;;; `reader-place'

(defclass reader-place (function-backed-place
                        read-only-place)
  ())

;;; `accessor-place'

(defclass accessor-place (function-backed-place)
  ())

(defmethod (setf value) ((new-value t) (place accessor-place))
  (funcall (fdefinition `(setf ,(cell place))) (container place)))

;;; `pseudo-place'
;;;
;;; A place that is in some way computed or derived and not backed by
;;; a concrete location such as an instance slot or array element.

(defclass pseudo-place (read-only-place)
  ((%cell :reader value)))

;;; `root-place'

(defclass root-place (basic-place)
  ((%cell :reader   value
          :writer   (setf value)))
  (:default-initargs
   :parent nil))

(defmethod valuep ((place root-place))
  (slot-boundp place '%cell))

(defmethod (setf state) :after ((new-value t) (place root-place))
  (setf (style new-value) :expanded))

(defmethod note-changed ((place root-place))
  ;; The container is the `inspector-state' instance -- notify it.
  (note-changed (container place)))
