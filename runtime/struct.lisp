;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Low-level support functions for structures.

(in-package :mezzano.runtime)

(sys.int::defglobal sys.int::*structure-type-type*)
(sys.int::defglobal sys.int::*structure-slot-type*)

(sys.int::defglobal *structure-types*)

(declaim (inline sys.int::instance-p sys.int::funcallable-instance-p))

(defun sys.int::%instance-layout (object)
  (sys.int::%instance-layout object))

(defun sys.int::instance-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-instance+))

(defun sys.int::funcallable-instance-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-funcallable-instance+))

(declaim (inline sys.int::structure-object-p))

(defun sys.int::structure-object-p (object)
  (and (sys.int::instance-p object)
       (eq sys.int::*structure-type-type*
           ;; If the object's metaclass is structure-defintion, then it's a structure object.
           (sys.int::layout-class
            (sys.int::%instance-layout
             (sys.int::layout-class (sys.int::%instance-layout object)))))))

(defun find-struct-slot (definition slot-name &optional (errorp t))
  (or (find slot-name (sys.int::structure-definition-slots definition)
            :key #'sys.int::structure-slot-definition-name)
      (if errorp
          (error "Slot ~S missing from structure definition ~S."
                 slot-name definition)
          nil)))

(defun sys.int::%struct-slot (object definition slot-name)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (let ((slot (find-struct-slot definition slot-name)))
    (funcall (sys.int::structure-slot-definition-ref-fn slot)
             object
             (sys.int::structure-slot-definition-index slot))))

(defun (setf sys.int::%struct-slot) (value object definition slot-name)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (let ((slot (find-struct-slot definition slot-name)))
    (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
      (assert (typep value (sys.int::structure-slot-definition-type slot))))
    (funcall (case (sys.int::structure-slot-definition-ref-fn slot)
               (sys.int::%object-ref-t #'(setf sys.int::%object-ref-t))
               (t
                (fdefinition `(setf ,(sys.int::structure-slot-definition-ref-fn slot)))))
             value
             object
             (sys.int::structure-slot-definition-index slot))))

(defun (sys.int::cas sys.int::%struct-slot) (old new object definition slot-name)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (let ((slot (find-struct-slot definition slot-name)))
    (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
      (assert (typep old (sys.int::structure-slot-definition-type slot))))
    (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
      (assert (typep new (sys.int::structure-slot-definition-type slot))))
    (when (not (eql (sys.int::structure-slot-definition-ref-fn slot) 'sys.int::%object-ref-t))
      (error "Can't cas unboxed slots"))
    (multiple-value-bind (successp actual-value)
        (sys.int::%cas-object object
                              (sys.int::structure-slot-definition-index slot)
                              old new)
      (declare (ignore successp))
      actual-value)))

(defun sys.int::%fast-structure-type-p (object structure-header)
  (sys.int::%fast-structure-type-p object structure-header))

(defun sys.int::structure-type-p (object struct-type)
  "Test if OBJECT is a structure object of type STRUCT-TYPE."
  (when (sys.int::structure-object-p object)
    (do ((object-type (sys.int::layout-class (sys.int::%instance-layout object))
                      (sys.int::structure-definition-parent object-type)))
        ((not object-type)
         nil)
      (when (eq object-type struct-type)
        (return t)))))

(defun sys.int::%make-struct (definition)
  (sys.int::%allocate-instance (sys.int::structure-definition-layout definition)))

(defun copy-structure (structure)
  (assert (sys.int::instance-p structure) (structure)
          "STRUCTURE is not an instance!")
  (let* ((layout (sys.int::%instance-layout structure))
         (struct-type (sys.int::layout-class layout)))
    (assert (typep struct-type 'sys.int::structure-definition)
            (structure)
            "STRUCTURE is not a structure!")
    (let ((new (sys.int::%make-struct struct-type)))
      (loop
         for slot in (sys.int::structure-definition-slots struct-type)
         for slot-name = (sys.int::structure-slot-definition-name slot)
         do
           (setf (sys.int::%struct-slot new struct-type slot-name)
                 (sys.int::%struct-slot structure struct-type slot-name)))
      new)))

(defun sys.int::make-struct-definition (name slots parent area size layout)
  (let* ((def (sys.int::%make-struct-definition name
                                                ;; Slots list must be wired.
                                                (sys.int::copy-list-in-area slots :wired)
                                                parent
                                                area
                                                size
                                                nil))
         (layout-object (sys.int::make-layout
                         :class def
                         :obsolete nil
                         :heap-size size
                         ;; Layout must be pinned or wired.
                         ;; Used by the GC.
                         :heap-layout (if (bit-vector-p layout)
                                          (make-array (length layout)
                                                      :element-type 'bit
                                                      :initial-contents layout
                                                      :area :wired)
                                          layout)
                         :area area
                         ;; ### Not currently supported for structs.
                         :instance-slots nil)))
    (setf (sys.int::structure-definition-layout def) layout-object)
    def))

(defun %make-instance-header (layout)
  (check-type layout sys.int::layout)
  (with-live-objects (layout)
    (sys.int::%%assemble-value
     (logior (ash (sys.int::lisp-object-address layout)
                  sys.int::+object-data-shift+)
             (ash sys.int::+object-tag-instance+
                  sys.int::+object-type-shift+))
     sys.int::+tag-instance-header+)))

(defun %unpack-instance-header (instance-header)
  (sys.int::%%assemble-value
   (ash (sys.int::lisp-object-address instance-header)
        (- sys.int::+object-data-shift+))
   0))
