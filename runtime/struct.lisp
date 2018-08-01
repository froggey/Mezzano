;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Low-level support functions for structures.

(in-package :mezzano.runtime)

(sys.int::defglobal sys.int::*structure-type-type* nil)
(sys.int::defglobal sys.int::*structure-slot-type* nil)

(sys.int::defglobal *structure-types*)

(declaim (inline sys.int::structure-object-p))

(defun sys.int::%struct-type (object)
  (sys.int::%struct-type object))

(defun sys.int::structure-object-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-structure-object+))

(defun sys.int::structure-slot-index (definition slot)
  ;; Make sure these accessors get open-coded.
  (declare (optimize (speed 3) (safety 0))
           (type sys.int::structure-definition definition)
           (type sys.int::structure-slot-definition slot))
  (position (sys.int::structure-slot-definition-name slot)
            (sys.int::structure-definition-slots definition)
            :key #'sys.int::structure-slot-definition-name))

(defun sys.int::%struct-slot (object definition slot)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (sys.int::%object-ref-t object
                          (sys.int::structure-slot-index definition slot)))

(defun (setf sys.int::%struct-slot) (value object definition slot)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
    (assert (typep value (sys.int::structure-slot-definition-type slot))))
  (setf (sys.int::%object-ref-t object
                                (sys.int::structure-slot-index definition slot))
        value))

(defun (sys.int::cas sys.int::%struct-slot) (old new object definition slot)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
    (assert (typep old (sys.int::structure-slot-definition-type slot))))
  (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
    (assert (typep new (sys.int::structure-slot-definition-type slot))))
  (multiple-value-bind (successp actual-value)
      (sys.int::%cas-object object
                            (sys.int::structure-slot-index definition slot)
                            old new)
    (declare (ignore successp))
    actual-value))

(defun sys.int::structure-type-p (object struct-type)
  "Test if OBJECT is a structure object of type STRUCT-TYPE."
  (when (sys.int::structure-object-p object)
    (let ((ty (sys.int::%struct-type object)))
      (if (eq ty struct-type)
          't
          (do ((object-type ty (sys.int::structure-definition-parent object-type)))
              ((not object-type)
               nil)
            (when (eq object-type struct-type)
              (return t)))))))

(defun copy-structure (structure)
  (assert (sys.int::structure-object-p structure) (structure) "STRUCTURE is not a structure!")
  (let* ((struct-type (sys.int::%struct-type structure))
         (new (sys.int::%make-struct struct-type)))
    (dolist (slot (sys.int::structure-definition-slots struct-type))
      (setf (sys.int::%struct-slot new struct-type slot)
            (sys.int::%struct-slot structure struct-type slot)))
    new))

(defun sys.int::make-struct-definition (name slots parent area)
  (sys.int::%make-struct-definition name
                                    ;; Slots list must be wired.
                                    (sys.int::copy-list-in-area slots :wired)
                                    parent
                                    area))
