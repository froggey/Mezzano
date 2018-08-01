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

(defun sys.int::%struct-slot (object definition slot)
  ;; Make sure these accessors get open-coded.
  (declare (optimize (speed 3) (safety 0))
           (type sys.int::structure-definition definition)
           (type sys.int::structure-slot-definition slot))
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (funcall (sys.int::structure-slot-definition-ref-fn slot)
           object
           (sys.int::structure-slot-definition-index slot)))

(defun (setf sys.int::%struct-slot) (value object definition slot)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
    (assert (typep value (sys.int::structure-slot-definition-type slot))))
  (funcall (case (sys.int::structure-slot-definition-ref-fn slot)
             (sys.int::%object-ref-t #'(setf sys.int::%object-ref-t))
             (t
              (fdefinition `(setf ,(sys.int::structure-slot-definition-ref-fn slot)))))
           value
           object
           (sys.int::structure-slot-definition-index slot)))

(defun (sys.int::cas sys.int::%struct-slot) (old new object definition slot)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
    (assert (typep old (sys.int::structure-slot-definition-type slot))))
  (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
    (assert (typep new (sys.int::structure-slot-definition-type slot))))
  (when (not (eql (sys.int::structure-slot-definition-ref-fn slot) 'sys.int::%object-ref-t))
    (error "Can't cas unboxed slots"))
  (multiple-value-bind (successp actual-value)
      (sys.int::%cas-object object
                            (sys.int::structure-slot-definition-index definition slot)
                            old new)
    (declare (ignore successp))
    actual-value))

(defun sys.int::%fast-structure-type-p (object structure-header)
  (sys.int::%fast-structure-type-p object structure-header))

(defun sys.int::structure-type-p (object struct-type)
  "Test if OBJECT is a structure object of type STRUCT-TYPE."
  (when (sys.int::structure-object-p object)
    (do ((object-type (sys.int::%struct-type object)
                      (sys.int::structure-definition-parent object-type)))
        ((not object-type)
         nil)
      (when (eq object-type struct-type)
        (return t)))))

(defun copy-structure (structure)
  (assert (sys.int::structure-object-p structure) (structure) "STRUCTURE is not a structure!")
  (let* ((struct-type (sys.int::%struct-type structure))
         (new (sys.int::%make-struct struct-type)))
    (dolist (slot (sys.int::structure-definition-slots struct-type))
      (setf (sys.int::%struct-slot new struct-type slot)
            (sys.int::%struct-slot structure struct-type slot)))
    new))

(defun sys.int::make-struct-definition (name slots parent area size layout)
  (sys.int::%make-struct-definition name
                                    ;; Slots list must be wired.
                                    (sys.int::copy-list-in-area slots :wired)
                                    parent
                                    area
                                    size
                                    ;; Layout must be pinned or wired.
                                    ;; Used by the GC.
                                    (if (bit-vector-p layout)
                                        (make-array (length layout)
                                                    :element-type 'bit
                                                    :initial-contents layout
                                                    :area :wired)
                                        layout)))

(defun %make-structure-header (structure-definition)
  (with-live-objects (structure-definition)
    (sys.int::%%assemble-value
     (logior (ash (sys.int::lisp-object-address structure-definition)
                  sys.int::+object-data-shift+)
             (ash sys.int::+object-tag-structure-object+
                  sys.int::+object-type-shift+))
     sys.int::+tag-structure-header+)))

(defun %unpack-structure-header (structure-header)
  (sys.int::%%assemble-value
   (ash (sys.int::lisp-object-address structure-header) (- sys.int::+object-data-shift+))
   0))
