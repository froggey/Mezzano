;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Low-level support functions for structures.

(in-package :mezzano.runtime)

(sys.int::defglobal sys.int::*structure-type-type*)
(sys.int::defglobal sys.int::*structure-slot-type*)

(sys.int::defglobal *structure-types*)

(declaim (inline sys.int::structure-object-p))

(defun sys.int::structure-object-p (object)
  ;; If the object's metaclass is structure-defintion,
  ;; then it's a structure object.
  (when (sys.int::instance-p object)
    (let ((layout (sys.int::%instance-layout object)))
      ;; Be careful around obsolete instances
      (and (sys.int::layout-p layout)
           (eq sys.int::*structure-type-type*
               ;; Classes should never be obsolete. Class metaobjects must not
               ;; be redefined.
               (sys.int::layout-class
                (sys.int::%instance-layout
                 (sys.int::layout-class layout))))))))

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
  (instance-access object
                   (sys.int::structure-slot-definition-location
                    (find-struct-slot definition slot-name))))

(defun (setf sys.int::%struct-slot) (value object definition slot-name)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (let ((slot (find-struct-slot definition slot-name)))
    (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
      (assert (typep value (sys.int::structure-slot-definition-type slot))))
    (setf (instance-access object
                           (sys.int::structure-slot-definition-location slot))
          value)))

(defun (sys.int::cas sys.int::%struct-slot) (old new object definition slot-name)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (let ((slot (find-struct-slot definition slot-name)))
    (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
      (assert (typep old (sys.int::structure-slot-definition-type slot))))
    (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
      (assert (typep new (sys.int::structure-slot-definition-type slot))))
    (sys.int::cas (instance-access object
                                   (sys.int::structure-slot-definition-location slot))
                  old new)))

(defun check-vector-slot-bounds (slot index)
  (check-type index fixnum)
  (assert (<= 0 index (1- (sys.int::structure-slot-definition-fixed-vector slot)))))

(defun sys.int::%struct-vector-slot (object definition slot-name index)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (let ((slot (find-struct-slot definition slot-name)))
    (check-vector-slot-bounds slot index)
    (instance-access object
                     (sys.int::structure-slot-definition-location slot)
                     index)))

(defun (setf sys.int::%struct-vector-slot) (value object definition slot-name index)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (let ((slot (find-struct-slot definition slot-name)))
    (check-vector-slot-bounds slot index)
    (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
      (assert (typep value (sys.int::structure-slot-definition-type slot))))
    (setf (instance-access object
                           (sys.int::structure-slot-definition-location slot)
                           index)
          value)))

(defun (sys.int::cas sys.int::%struct-vector-slot) (old new object definition slot-name index)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (let ((slot (find-struct-slot definition slot-name)))
    (check-vector-slot-bounds slot index)
    (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
      (assert (typep old (sys.int::structure-slot-definition-type slot))))
    (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
      (assert (typep new (sys.int::structure-slot-definition-type slot))))
    (sys.int::cas (instance-access object
                                   (sys.int::structure-slot-definition-location slot)
                                   index)
                  old new)))

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

(defun sys.int::make-struct-definition (name slots parent area size layout sealed)
  (when (and parent
             (sys.int::structure-definition-sealed parent))
    (error "Attempted to make structure definition that includes sealed structure ~S" parent))
  (let* ((def (sys.int::%make-struct-definition name
                                                ;; Slots list must be wired.
                                                (sys.int::copy-list-in-area slots :wired)
                                                parent
                                                area
                                                size
                                                nil
                                                sealed))
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
                         :instance-slots (make-array (* (length slots) 2)
                                                     :initial-contents (loop
                                                                          for slot in slots
                                                                          collect (sys.int::structure-slot-definition-name slot)
                                                                          collect (sys.int::structure-slot-definition-location slot))
                                                     :area :wired))))
    (setf (sys.int::structure-definition-layout def) layout-object)
    def))

(in-package :sys.int)

(defstruct (structure-definition
             (:area :wired)
             (:constructor %make-struct-definition
                           (name slots parent area size layout sealed))
             :sealed)
  (name nil :read-only t)
  (slots nil :read-only t :type list)
  (parent nil :read-only t)
  (area nil :read-only t)
  (size nil :read-only t)
  (layout nil)
  (class nil)
  (sealed nil :read-only t))

(defstruct (structure-slot-definition
             (:area :wired)
             (:constructor make-struct-slot-definition
                           (name accessor initform type read-only location fixed-vector align))
             :sealed)
  (name nil :read-only t)
  (accessor nil :read-only t)
  (initform nil :read-only t)
  (type t :read-only t)
  (read-only nil :read-only t)
  (location nil :read-only t)
  (fixed-vector nil :read-only t)
  (align nil :read-only t))
