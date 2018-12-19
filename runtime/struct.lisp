;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Low-level support functions for structures.

(in-package :mezzano.runtime)

(sys.int::defglobal *structure-class-layout*) ; layout of structure-class instances

(defun structure-class-p (object)
  ;; If the object's layout is structure-class's slot layout,
  ;; then it's a structure class.
  (and (sys.int::instance-p object)
       ;; The structure-class class can never be reinitialized, so this
       ;; layout is permanent. There will never be obsolete instances of it.
       (eq (sys.int::%instance-layout object) *structure-class-layout*)))

(defun sys.int::structure-object-p (object)
  (structure-object-class object))

(defun find-struct-slot (class slot-name &optional (errorp t))
  (when (typep slot-name 'mezzano.clos:structure-effective-slot-definition)
    (return-from find-struct-slot slot-name))
  (or (find slot-name (mezzano.clos:class-slots class)
            :key #'mezzano.clos:slot-definition-name)
      (if errorp
          (error "Slot ~S missing from structure definition ~S."
                 slot-name definition)
          nil)))

(defun sys.int::%struct-slot (object class-name slot-name)
  (let ((class (sys.int::get-structure-type class-name)))
    (unless (sys.int::structure-type-p object class)
      (sys.int::raise-type-error object class)
      (sys.int::%%unreachable))
    (let* ((slot (find-struct-slot class slot-name))
           (loc (mezzano.clos:slot-definition-location slot)))
      (instance-access (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object) loc))))

(defun (setf sys.int::%struct-slot) (value object class-name slot-name)
  (let ((class (sys.int::get-structure-type class-name)))
    (unless (sys.int::structure-type-p object class)
      (sys.int::raise-type-error object class)
      (sys.int::%%unreachable))
    (let* ((slot (find-struct-slot class slot-name))
           (type (mezzano.clos:slot-definition-type slot))
           (loc (mezzano.clos:slot-definition-location slot)))
      (unless (eq type 't)
        (assert (typep value type)))
      (setf (instance-access (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object) loc) value))))

(defun (sys.int::cas sys.int::%struct-slot) (old new object class-name slot-name)
  (let ((class (sys.int::get-structure-type class-name)))
    (unless (sys.int::structure-type-p object class)
      (sys.int::raise-type-error object class)
      (sys.int::%%unreachable))
    (let* ((slot (find-struct-slot class slot-name))
           (type (mezzano.clos:slot-definition-type slot))
           (loc (mezzano.clos:slot-definition-location slot)))
      (unless (eq type 't)
        (assert (typep old type))
        (assert (typep new type)))
      (sys.int::cas (instance-access (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object) loc)
                    old new))))

(defun check-vector-slot-bounds (slot index)
  (check-type index fixnum)
  (assert (<= 0 index (1- (mezzano.clos:structure-slot-definition-fixed-vector slot)))))

(defun sys.int::%struct-vector-slot (object class-name slot-name index)
  (let ((class (sys.int::get-structure-type class-name)))
    (unless (sys.int::structure-type-p object class)
      (sys.int::raise-type-error object class)
      (sys.int::%%unreachable))
    (let* ((slot (find-struct-slot class slot-name))
           (loc (mezzano.clos:slot-definition-location slot)))
      (check-vector-slot-bounds slot index)
      (instance-access (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object) loc index))))

(defun (setf sys.int::%struct-vector-slot) (value object class-name slot-name index)
  (let ((class (sys.int::get-structure-type class-name)))
    (unless (sys.int::structure-type-p object class)
      (sys.int::raise-type-error object class)
      (sys.int::%%unreachable))
    (let* ((slot (find-struct-slot class slot-name))
           (type (mezzano.clos:slot-definition-type slot))
           (loc (mezzano.clos:slot-definition-location slot)))
      (check-vector-slot-bounds slot index)
      (unless (eq type 't)
        (assert (typep value type)))
      (setf (instance-access (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object) loc index) value))))

(defun (sys.int::cas sys.int::%struct-vector-slot) (old new object class-name slot-name index)
  (let ((class (sys.int::get-structure-type class-name)))
    (unless (sys.int::structure-type-p object class)
      (sys.int::raise-type-error object class)
      (sys.int::%%unreachable))
    (let* ((slot (find-struct-slot class slot-name))
           (type (mezzano.clos:slot-definition-type slot))
           (loc (mezzano.clos:slot-definition-location slot)))
      (check-vector-slot-bounds slot index)
      (unless (eq type 't)
        (assert (typep old type))
        (assert (typep new type)))
      (sys.int::cas (instance-access (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object) loc index) old new))))

(defun structure-object-class (object)
  "If OBJECT is a structure object, return the object's class. Otherwise return NIL."
  (when (sys.int::instance-p object)
    (let* ((layout (sys.int::%instance-layout object))
           (class (sys.int::layout-class
                   (if (sys.int::layout-p layout)
                       layout
                       (obsolete-instance-layout-old-layout layout)))))
      (when (structure-class-p class)
        class))))

(defun sys.int::structure-type-p (object structure-class)
  "Test if OBJECT is a structure object of type STRUCTURE-CLASS."
  (do ((object-class (structure-object-class object)
                     ;; The parent field is used instead of the CPL as
                     ;; the CPL might not be in wired memory.
                     (instance-access-by-name object-class 'mezzano.clos::parent)))
      ((not object-class)
       nil)
    (when (eq object-class structure-class)
      (return t))))

(defun sys.int::%allocate-struct (structure-class)
  (when (symbolp structure-class)
    (setf structure-class (find-class structure-class)))
  (assert (structure-class-p structure-class))
  (let ((layout (instance-access-by-name structure-class 'mezzano.clos::slot-storage-layout)))
    (sys.int::%allocate-instance layout)))

(defun copy-structure (structure)
  (check-type structure structure-object)
  (let* ((class (class-of structure))
         (new (sys.int::%allocate-struct class)))
    (loop
       for slot in (mezzano.clos:class-slots class)
       for slot-name = (mezzano.clos:slot-definition-name slot)
       for fixed-vector = (mezzano.clos:structure-slot-definition-fixed-vector slot)
       do
         (if fixed-vector
             (dotimes (i fixed-vector)
               (setf (sys.int::%struct-vector-slot new class slot-name i)
                     (sys.int::%struct-vector-slot structure class slot-name i)))
             (setf (sys.int::%struct-slot new class slot-name)
                   (sys.int::%struct-slot structure class slot-name))))
    new))

(defun sys.int::make-struct-definition (name slots parent area size layout sealed)
  (when (and parent
             (if (structure-class-p parent)
                 (mezzano.runtime::instance-access-by-name parent 'mezzano.clos::sealed)
                 (sys.int::structure-definition-sealed parent)))
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
