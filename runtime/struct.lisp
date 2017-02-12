;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Low-level support functions for structures.

(in-package :mezzano.runtime)

(sys.int::defglobal sys.int::*structure-type-type* nil)
(sys.int::defglobal sys.int::*structure-slot-type* nil)

(declaim (inline sys.int::structure-object-p
                 sys.int::%struct-slot
                 (setf sys.int::%struct-slot)
                 (sys.int::cas sys.int::%struct-slot)))

(defun sys.int::structure-object-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-structure-object+))

(defun sys.int::%struct-slot (object slot)
  (sys.int::%object-ref-t object slot))

(defun (setf sys.int::%struct-slot) (value object slot)
  (setf (sys.int::%object-ref-t object slot) value))

(defun (sys.int::cas sys.int::%struct-slot) (old new object slot)
  (multiple-value-bind (successp actual-value)
      (sys.int::%cas-object object slot old new)
    (declare (ignore successp))
    actual-value))

(defun sys.int::structure-type-p (object struct-type)
  "Test if OBJECT is a structure object of type STRUCT-TYPE."
  (when (sys.int::structure-object-p object)
    (let ((ty (sys.int::%object-ref-t object 0)))
      (if (eq ty struct-type)
          't
          (do ((object-type ty (sys.int::structure-parent object-type)))
              ;; Stop when the object-type stops being a structure-definition, not
              ;; when it becomes NIL.
              ;; This avoids a race condition in the GC when it is
              ;; scavenging a partially initialized structure.
              ((not (and (sys.int::structure-object-p object-type)
                         (eq (sys.int::%struct-slot object-type 0)
                             sys.int::*structure-type-type*)))
               nil)
            (when (eq object-type struct-type)
              (return t)))))))

;;; Manually define accessors & constructors for the structure-definition type.
;;; This is required because the structure-definition for structure-definition
;;; must be kept in *structure-type-type* and it must be wired.
;;; The structure-definition is currently created by the cold-generator.

(defun sys.int::make-struct-definition (name slots parent area)
  (let ((x (sys.int::%make-struct 6 :wired)))
    (setf (sys.int::%struct-slot x 0) sys.int::*structure-type-type*
          (sys.int::%struct-slot x 1) name
          (sys.int::%struct-slot x 2) slots
          (sys.int::%struct-slot x 3) parent
          (sys.int::%struct-slot x 4) area
          (sys.int::%struct-slot x 5) nil)
    x))

(defun sys.int::structure-definition-p (object)
  (eq (sys.int::%struct-slot object 0) sys.int::*structure-type-type*))

(macrolet ((def (name field)
             `(defun ,name (object)
                (unless (sys.int::structure-definition-p object)
                  (error 'type-error :datum object :expected-type 'sys.int::structure-definition))
                (sys.int::%struct-slot object ,field))))
  (def sys.int::structure-name 1)
  (def sys.int::structure-slots 2)
  (def sys.int::structure-parent 3)
  (def sys.int::structure-area 4)
  (def sys.int::structure-definition-class 5))

(defun (setf sys.int::structure-definition-class) (value object)
  (unless (sys.int::structure-definition-p object)
    (error 'type-error :datum object :expected-type 'sys.int::structure-definition))
  (setf (sys.int::%struct-slot object 5) value))

;;; Structure slot definitions.

(defun sys.int::make-struct-slot-definition (name accessor initform type read-only)
  (let ((x (sys.int::%make-struct 6 :wired)))
    (setf (sys.int::%struct-slot x 0) sys.int::*structure-slot-type*
          (sys.int::%struct-slot x 1) name
          (sys.int::%struct-slot x 2) accessor
          (sys.int::%struct-slot x 3) initform
          (sys.int::%struct-slot x 4) type
          (sys.int::%struct-slot x 5) read-only)
    x))

(defun sys.int::structure-slot-definition-p (object)
  (eq (sys.int::%struct-slot object 0) sys.int::*structure-slot-type*))

(macrolet ((def (name field)
             `(defun ,name (object)
                (unless (sys.int::structure-slot-definition-p object)
                  (error 'type-error :datum object :expected-type 'sys.int::structure-slot-definition))
                (sys.int::%struct-slot object ,field))))
  (def sys.int::structure-slot-name 1)
  (def sys.int::structure-slot-accessor 2)
  (def sys.int::structure-slot-initform 3)
  (def sys.int::structure-slot-type 4)
  (def sys.int::structure-slot-read-only 5))
