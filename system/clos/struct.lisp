;;;; MOP support for structures.

(in-package :mezzano.clos)

(defmethod slot-definition-allocation ((slot-definition structure-slot-definition))
  :instance)
(defmethod slot-definition-initargs ((slot-definition structure-slot-definition))
  '())
(defmethod slot-definition-initform ((slot-definition structure-slot-definition))
  (declare (notinline slot-value)) ; bootstrap hack
  (slot-value slot-definition 'initform))
(defmethod slot-definition-initfunction ((slot-definition structure-slot-definition))
  nil)
(defmethod slot-definition-name ((slot-definition structure-slot-definition))
  (declare (notinline slot-value)) ; bootstrap hack
  (slot-value slot-definition 'name))
(defmethod slot-definition-type ((slot-definition structure-slot-definition))
  (declare (notinline slot-value)) ; bootstrap hack
  (slot-value slot-definition 'type))
(defmethod slot-definition-readers ((direct-slot-definition structure-direct-slot-definition))
  '())
(defmethod slot-definition-writers ((direct-slot-definition structure-direct-slot-definition))
  '())
(defmethod slot-definition-location ((effective-slot-definition structure-effective-slot-definition))
  (declare (notinline slot-value)) ; bootstrap hack
  (slot-value effective-slot-definition 'location))

(defgeneric structure-slot-definition-read-only (slot-definition)
  (:method ((slot-definition structure-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'read-only)))
(defgeneric structure-slot-definition-fixed-vector (slot-definition)
  (:method ((slot-definition structure-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'fixed-vector)))
(defgeneric structure-slot-definition-align (slot-definition)
  (:method ((slot-definition structure-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'align)))
(defgeneric structure-slot-definition-dcas-sibling (slot-definition)
  (:method ((slot-definition structure-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'dcas-sibling)))

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (sys.int::%allocate-struct class))

(defmethod direct-slot-definition-class ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-direct-slot-definition))
(defmethod effective-slot-definition-class ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-effective-slot-definition))

(defmethod initialize-instance :before ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (error "Cannot initialize structure classes."))

(defmethod reinitialize-instance :before ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (error "Cannot reinitialize structure classes."))

(defmethod slot-value-using-class ((class structure-class) instance (slot structure-effective-slot-definition))
  (let ((fv (structure-slot-definition-fixed-vector slot)))
    (cond (fv
           (let ((value (make-array fv)))
             (dotimes (i fv)
               (setf (aref value i)
                     (sys.int::%struct-vector-slot instance class slot i)))
             value))
          (t
           (sys.int::%struct-slot instance class slot)))))

(defmethod (setf slot-value-using-class) (new-value (class structure-class) instance (slot structure-effective-slot-definition))
  (when (structure-slot-definition-read-only slot)
    (error "The slot ~S in class ~S is read-only."
           (slot-definition-name slot)
           (class-name class)))
  (let ((fv (structure-slot-definition-fixed-vector slot)))
    (cond (fv
           (dotimes (i fv)
             (setf (sys.int::%struct-vector-slot instance class slot i)
                   (aref new-value i)))
           new-value)
          (t
           (setf (sys.int::%struct-slot instance class slot) new-value)))))

(defmethod (sys.int::cas slot-value-using-class) (old new (class structure-class) instance (slot structure-effective-slot-definition))
  (when (structure-slot-definition-read-only slot)
    (error "The slot ~S in class ~S is read-only."
           (slot-definition-name slot)
           (class-name class)))
  (let ((fv (structure-slot-definition-fixed-vector slot)))
    (cond (fv
           (error "CAS on fixed-vector slots not supported"))
          (t
           (sys.int::cas (sys.int::%struct-slot instance class slot) old new)))))

(defmethod slot-boundp-using-class ((class structure-class) instance (slot structure-effective-slot-definition))
  t)

(defmethod slot-makunbound-using-class ((class structure-class) instance (slot structure-effective-slot-definition))
  (error "Cannot make structure slots unbound."))

(defmethod slot-exists-p-using-class ((class structure-class) object slot-name)
  (std-slot-exists-p object slot-name))

;;; Structure redefinition.

;; This does not use the normal REINITIALIZE-INSTANCE machinery as
;; the new structure definition has a specific layout that it is expecting
;; the redefined class to follow.
(defun redefine-structure-type (existing-class sdef)
  (assert (not (eql existing-class (find-class 'structure-object)))) ; Can't redefine structure-object.
  (assert (not (class-sealed existing-class)) () "Cannot redefined sealed struct ~S" existing-class)
  (let ((new-layout (sys.int::make-layout :class existing-class
                                          :obsolete nil
                                          :heap-size (sys.int::structure-definition-size sdef)
                                          :heap-layout (sys.int::layout-heap-layout (sys.int::structure-definition-layout sdef))
                                          :area (sys.int::structure-definition-area sdef)
                                          :instance-slots (sys.int::convert-structure-definition-instance-slots sdef)))
        (prev-layout (class-layout existing-class)))
    ;; FIXME: If the parent class changes, call add-/remove-direct-subclass
    (sys.int::populate-struct-class-from-structure-defintion existing-class sdef)
    (when (not (class-layouts-compatible-p prev-layout new-layout))
      ;; Make instances obsolete if the layout changes.
      (setf (mezzano.runtime::instance-access-by-name existing-class 'mezzano.clos::slot-storage-layout)
            new-layout)
      (setf (sys.int::layout-obsolete prev-layout) new-layout)))
  existing-class)

(defmethod update-instance-for-redefined-class ((instance structure-object) added-slots discarded-slots property-list &rest initargs)
  (check-update-instance-for-redefined-class-initargs instance added-slots discarded-slots property-list initargs)
  (apply #'shared-initialize instance added-slots initargs))

(defmethod shared-initialize ((instance structure-object) slot-names &rest all-keys)
  (dolist (slot (class-slots (class-of instance)))
    (let ((slot-name (slot-definition-name slot)))
      (multiple-value-bind (init-key init-value foundp)
            (get-properties
             all-keys
             (list (intern (string slot-name) 'keyword)))
         (declare (ignore init-key))
         (cond (foundp
                (setf (slot-value instance slot-name) init-value))
               ((or (eq slot-names t)
                    (member slot-name slot-names))
                (setf (slot-value instance slot-name) (eval (slot-definition-initform slot))))))))
  instance)

(defmethod make-instance ((class structure-class) &rest initargs &key &allow-other-keys)
  (when (not (slot-value class 'has-standard-constructor))
    (error "Structure class ~S does not have a standard constructor" class))
  ;; TODO: Permit initargs based on allocate-instance/initialize-instance/shared-initialize as well, like STD-CLASS.
  (unless (getf initargs :allow-other-keys)
    (let ((invalid-initargs
           (loop
              with slots = (class-slots class)
              for (name value) on initargs by #'cddr
              unless (or (eql name :allow-other-keys)
                         (and (keywordp name)
                              (find name slots
                                    :key #'slot-definition-name
                                    :test #'string=)))
              collect name)))
      (when invalid-initargs
        (error "Invalid initargs ~:S when creating instance of ~S (~S)"
               invalid-initargs class (class-name class)))))
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

(defmethod initialize-instance ((instance structure-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))
