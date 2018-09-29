;;;; Copyright (c) 2018 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; MOP support for structures.

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
  (error "Cannot reinitialize structure classes."))

(defmethod reinitialize-instance :before ((class structure-class) &rest args)
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

(defmethod (setf slot-value-using-class) (new-value (class structure-class) instance (slot standard-effective-slot-definition))
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

(defmethod (sys.int::cas slot-value-using-class) (old new (class structure-class) instance (slot standard-effective-slot-definition))
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
