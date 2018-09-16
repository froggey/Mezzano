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

(defun check-vector-slot-bounds (slot index)
  (check-type index fixnum)
  (assert (<= 0 index (1- (sys.int::structure-slot-definition-fixed-vector slot)))))

(defun accessor-element-scale (accessor)
  (ecase accessor
    (sys.int::%object-ref-t 1)
    (sys.int::%object-ref-unsigned-byte-8-unscaled  1)
    (sys.int::%object-ref-unsigned-byte-16-unscaled 2)
    (sys.int::%object-ref-unsigned-byte-32-unscaled 4)
    (sys.int::%object-ref-unsigned-byte-64-unscaled 8)
    (sys.int::%object-ref-signed-byte-8-unscaled    1)
    (sys.int::%object-ref-signed-byte-16-unscaled   2)
    (sys.int::%object-ref-signed-byte-32-unscaled   4)
    (sys.int::%object-ref-signed-byte-64-unscaled   8)
    (sys.int::%object-ref-single-float-unscaled     4)
    (sys.int::%object-ref-double-float-unscaled     8)))

(defun sys.int::%struct-vector-slot (object definition slot-name index)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (let ((slot (find-struct-slot definition slot-name)))
    (check-vector-slot-bounds slot index)
    (funcall (sys.int::structure-slot-definition-ref-fn slot)
             object
             (+ (sys.int::structure-slot-definition-index slot)
                (* (accessor-element-scale (sys.int::structure-slot-definition-ref-fn slot))
                   index)))))

(defun (setf sys.int::%struct-vector-slot) (value object definition slot-name index)
  (when (not (sys.int::structure-type-p object definition))
    (sys.int::raise-type-error object (sys.int::structure-definition-name definition))
    (sys.int::%%unreachable))
  (let ((slot (find-struct-slot definition slot-name)))
    (check-vector-slot-bounds slot index)
    (when (not (eq (sys.int::structure-slot-definition-type slot) 't))
      (assert (typep value (sys.int::structure-slot-definition-type slot))))
    (funcall (case (sys.int::structure-slot-definition-ref-fn slot)
               (sys.int::%object-ref-t #'(setf sys.int::%object-ref-t))
               (t
                (fdefinition `(setf ,(sys.int::structure-slot-definition-ref-fn slot)))))
             value
             object
             (+ (sys.int::structure-slot-definition-index slot)
                (* (accessor-element-scale (sys.int::structure-slot-definition-ref-fn slot))
                   index)))))

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
    (when (not (eql (sys.int::structure-slot-definition-ref-fn slot) 'sys.int::%object-ref-t))
      (error "Can't cas unboxed slots"))
    (multiple-value-bind (successp actual-value)
        (sys.int::%cas-object object
                              (+ (sys.int::structure-slot-definition-index slot)
                                 (* (accessor-element-scale (sys.int::structure-slot-definition-ref-fn slot))
                                    index))
                              old new)
      (declare (ignore successp))
      actual-value)))

(defun sys.int::%fast-instance-layout-eq-p (object instance-header)
  (sys.int::%fast-instance-layout-eq-p object instance-header))

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

(defstruct (obsolete-instance-layout
             ;; Pinned, as the GC needs to read it.
             ;; Don't make it wired to avoid thrashing the wired area.
             (:area :pinned))
  new-instance
  old-layout)

(defun supersede-instance (old-instance replacement)
  (let ((layout (sys.int::%instance-layout old-instance)))
    (cond ((sys.int::layout-p layout)
           ;; This really is a layout, not a superseded instance
           (let ((new-layout (make-obsolete-instance-layout
                              :old-layout layout
                              :new-instance replacement)))
             (with-live-objects (new-layout)
               ;; ###: Should this be a CAS?
               ;; FIXME: This needs to keep the GC bits intact.
               ;; Not a problem for objects on the dynamic heap, but will
               ;; be an issue when dealing wired/pinned objects.
               (setf (sys.int::%object-ref-unsigned-byte-64 old-instance -1)
                     ;; Construct a new obsolete-instance header containing
                     ;; our new obsolete layout.
                     (logior (ash (sys.int::lisp-object-address new-layout)
                                  sys.int::+object-data-shift+)
                             (if (sys.int::funcallable-instance-p old-instance)
                                 (ash sys.int::+object-tag-funcallable-instance+
                                      sys.int::+object-type-shift+)
                                 (ash sys.int::+object-tag-instance+
                                      sys.int::+object-type-shift+)))))))
          (t
           ;; This instance has already been superseded, replace it in-place.
           ;; FIXME: Can race with the GC. It can snap the old instance away
           ;; from underneath us, losing the replacement.
           ;; Check if the old instance's layout matches after this?
           (setf (obsolete-instance-layout-new-instance layout)
                 replacement))))
  (values))
