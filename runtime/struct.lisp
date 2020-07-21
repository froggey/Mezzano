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
          (error "Slot ~S missing from structure ~S."
                 slot-name class)
          nil)))

(defun raise-struct-type-error (object class slot-name)
  (error 'simple-type-error
         :datum object
         :expected-type class
         :format-control "Type error. ~S is not of structure type ~S when accessing slot ~S."
         :format-arguments (list object (class-name class) slot-name)))

(defun sys.int::%struct-slot (object class-name slot-name)
  (let ((class (sys.int::get-structure-type class-name)))
    (when (not (sys.int::structure-type-p object class))
      (raise-struct-type-error object class slot-name)
      (sys.int::%%unreachable))
    (let* ((slot (find-struct-slot class slot-name))
           (loc (mezzano.clos:slot-definition-location slot)))
      (instance-access (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object) loc))))

(defun (setf sys.int::%struct-slot) (value object class-name slot-name)
  (let ((class (sys.int::get-structure-type class-name)))
    (when (not (sys.int::structure-type-p object class))
      (raise-struct-type-error object class slot-name)
      (sys.int::%%unreachable))
    (let* ((slot (find-struct-slot class slot-name))
           (type (mezzano.clos:slot-definition-type slot))
           (loc (mezzano.clos:slot-definition-location slot)))
      (when (not (eq type 't))
        (assert (typep value type)))
      (setf (instance-access (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object) loc) value))))

(defun (sys.int::cas sys.int::%struct-slot) (old new object class-name slot-name)
  (let ((class (sys.int::get-structure-type class-name)))
    (when (not (sys.int::structure-type-p object class))
      (raise-struct-type-error object class slot-name)
      (sys.int::%%unreachable))
    (let* ((slot (find-struct-slot class slot-name))
           (type (mezzano.clos:slot-definition-type slot))
           (loc (mezzano.clos:slot-definition-location slot)))
      (when (not (eq type 't))
        (assert (typep old type))
        (assert (typep new type)))
      (sys.int::cas (instance-access (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object) loc)
                    old new))))

(macrolet ((def (name fn)
             `(defun ,name (object class-name slot-name value)
                (check-type value fixnum)
                (let ((class (sys.int::get-structure-type class-name)))
                  (when (not (sys.int::structure-type-p object class))
                    (raise-struct-type-error object class slot-name)
                    (sys.int::%%unreachable))
                  (let* ((slot (find-struct-slot class slot-name))
                         (type (mezzano.clos:slot-definition-type slot))
                         (loc (mezzano.clos:slot-definition-location slot)))
                    (when (not (or (eq type 'fixnum)
                                   (and (subtypep type 'fixnum)
                                        (subtypep 'fixnum type))))
                      (error "Slot ~S in struct ~S ~S is not of type FIXNUM"
                             slot-name object class-name))
                    (when (mezzano.clos:structure-slot-definition-read-only slot)
                      (error "Slot ~S in structure type ~S is read-only" slot class))
                    (,fn
                     (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object)
                     (location-offset-t loc)
                     value))))))
  (def sys.int::%atomic-fixnum-add-struct-slot sys.int::%atomic-fixnum-add-object)
  (def sys.int::%atomic-fixnum-logand-struct-slot sys.int::%atomic-fixnum-logand-object)
  (def sys.int::%atomic-fixnum-logior-struct-slot sys.int::%atomic-fixnum-logior-object)
  (def sys.int::%atomic-fixnum-logxor-struct-slot sys.int::%atomic-fixnum-logxor-object))

(defun sys.int::%atomic-swap-struct-slot (object class-name slot-name value)
  (let ((class (sys.int::get-structure-type class-name)))
    (when (not (sys.int::structure-type-p object class))
      (raise-struct-type-error object class slot-name)
      (sys.int::%%unreachable))
    (let* ((slot (find-struct-slot class slot-name))
           (type (mezzano.clos:slot-definition-type slot))
           (loc (mezzano.clos:slot-definition-location slot)))
      (when (not (eq type 't))
        (assert (typep value type)))
      (when (mezzano.clos:structure-slot-definition-read-only slot)
        (error "Slot ~S in structure type ~S is read-only" slot class))
      (sys.int::%xchg-object
       (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object)
       (location-offset-t loc)
       value))))

(defun sys.int::%dcas-struct-slot (object
                                   place-1-class-name place-1-slot-name
                                   place-2-class-name place-2-slot-name
                                   old-1 old-2 new-1 new-2)
  (let ((class-1 (sys.int::get-structure-type place-1-class-name))
        (class-2 (sys.int::get-structure-type place-2-class-name)))
    (when (not (sys.int::structure-type-p object class-1))
      (raise-struct-type-error object class-1 place-1-slot-name)
      (sys.int::%%unreachable))
    (when (not (sys.int::structure-type-p object class-2))
      (raise-struct-type-error object class-2 place-2-slot-name)
      (sys.int::%%unreachable))
    (let* ((slot-1 (find-struct-slot class-1 place-1-slot-name))
           (slot-2 (find-struct-slot class-2 place-2-slot-name))
           (type-1 (mezzano.clos:slot-definition-type slot-1))
           (type-2 (mezzano.clos:slot-definition-type slot-2))
           (loc-1 (mezzano.clos:slot-definition-location slot-1))
           (loc-2 (mezzano.clos:slot-definition-location slot-2)))
      (when (not (eq type-1 't))
        (assert (typep old-1 type-1))
        (assert (typep new-1 type-1)))
      (when (not (eq type-2 't))
        (assert (typep old-2 type-2))
        (assert (typep new-2 type-2)))
      (when (mezzano.clos:structure-slot-definition-read-only slot-1)
        (error "Slot ~S in structure type ~S is read-only" slot-1 class-1))
      (when (mezzano.clos:structure-slot-definition-read-only slot-2)
        (error "Slot ~S in structure type ~S is read-only" slot-2 class-2))
      (when (not (eql (mezzano.clos:structure-slot-definition-dcas-sibling slot-1) place-2-slot-name))
        (error "Slots ~S and ~S in structure types ~S and ~S are not DCAS siblings"
               slot-1 slot-2 class-1 class-2))
      (instance-dcas (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object)
                     loc-1 loc-2 old-1 old-2 new-1 new-2))))

(defun check-vector-slot-bounds (slot index)
  (check-type index fixnum)
  (assert (<= 0 index (1- (mezzano.clos:structure-slot-definition-fixed-vector slot)))))

(defun sys.int::%struct-vector-slot (object class-name slot-name index)
  (let ((class (sys.int::get-structure-type class-name)))
    (when (not (sys.int::structure-type-p object class))
      (raise-struct-type-error object class slot-name)
      (sys.int::%%unreachable))
    (let* ((slot (find-struct-slot class slot-name))
           (loc (mezzano.clos:slot-definition-location slot)))
      (check-vector-slot-bounds slot index)
      (instance-access (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object) loc index))))

(defun (setf sys.int::%struct-vector-slot) (value object class-name slot-name index)
  (let ((class (sys.int::get-structure-type class-name)))
    (when (not (sys.int::structure-type-p object class))
      (raise-struct-type-error object class slot-name)
      (sys.int::%%unreachable))
    (let* ((slot (find-struct-slot class slot-name))
           (type (mezzano.clos:slot-definition-type slot))
           (loc (mezzano.clos:slot-definition-location slot)))
      (check-vector-slot-bounds slot index)
      (when (not (eq type 't))
        (assert (typep value type)))
      (setf (instance-access (mezzano.clos::fetch-up-to-date-instance-slots-and-layout object) loc index) value))))

(defun (sys.int::cas sys.int::%struct-vector-slot) (old new object class-name slot-name index)
  (let ((class (sys.int::get-structure-type class-name)))
    (when (not (sys.int::structure-type-p object class))
      (raise-struct-type-error object class slot-name)
      (sys.int::%%unreachable))
    (let* ((slot (find-struct-slot class slot-name))
           (type (mezzano.clos:slot-definition-type slot))
           (loc (mezzano.clos:slot-definition-location slot)))
      (check-vector-slot-bounds slot index)
      (when (not (eq type 't))
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

(defun %copy-structure-slow (structure class new)
  ;; Copy STRUCTURE slot-by-slot.
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
                 (sys.int::%struct-slot structure class slot-name)))))

#-x86-64
(defun %copy-structure-fast (structure class new)
  (declare (ignore structure class new))
  nil)

#+x86-64
(sys.int::define-lap-function %copy-structure-fast ((structure class new))
  ;; Copy STRUCTURE by blasting words across.
  ;; STRUCTURE and NEW must have the same layout, neither instance must be obsolete.
  ;; Runs in a GC-restart region to avoid GC issues.
  (:gc :no-frame :layout #*0 :restart t)
  ;; Both objects must have the same layout (not obsolete!)
  (sys.lap-x86:mov64 :rax (:object :r8 -1)) ; rax = instance-header for STRUCTURE
  (sys.lap-x86:cmp64 :rax (:object :r10 -1)) ; compare with instance-header for NEW
  (sys.lap-x86:jne BAIL)
  ;; Turn the instance-header into a layout-like.
  (sys.lap-x86:shr64 :rax #.sys.int::+object-data-shift+)
  ;; Make sure it really is a layout
  (sys.lap-x86:mov64 :rcx (:object :rax -1))
  (sys.lap-x86:or64 :rcx 3)
  (sys.lap-x86:cmp64 :rcx :layout-instance-header)
  (sys.lap-x86:jne BAIL)
  ;; :RAX holds the layout for STRUCTURE & NEW, pull up the heap size and
  ;; start copying words.
  (sys.lap-x86:mov64 :rcx (:object-location :rax #.sys.int::+layout-heap-size+))
  (sys.lap-x86:sar64 :rcx #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:lea64 :rsi (:object :r8 0))
  (sys.lap-x86:lea64 :rdi (:object :r10 0))
  (sys.lap-x86:rep)
  (sys.lap-x86:movs64)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret) ; Return success, STRUCTURE is still in R8, it's not NIL.
  BAIL
  (sys.lap-x86:mov32 :r8d nil)
  (sys.lap-x86:ret))

(defun copy-structure (structure)
  (check-type structure structure-object)
  (let* ((class (class-of structure))
         (new (sys.int::%allocate-struct class)))
    (when (not (%copy-structure-fast structure class new))
      (%copy-structure-slow structure class new))
    new))

(defun sys.int::make-struct-definition (name slots parent area size layout sealed docstring has-standard-constructor)
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
                                                sealed
                                                docstring
                                                has-standard-constructor))
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

(in-package :mezzano.internals)

(defstruct (structure-definition
             (:area :wired)
             (:constructor %make-struct-definition
                           (name slots parent area size layout sealed docstring has-standard-constructor))
             :sealed)
  (name nil :read-only t)
  (slots nil :read-only t :type list)
  (parent nil :read-only t)
  (area nil :read-only t)
  (size nil :read-only t)
  (layout nil)
  (class nil)
  (sealed nil :read-only t)
  (docstring nil)
  (has-standard-constructor t))

(defstruct (structure-slot-definition
             (:area :wired)
             (:constructor make-struct-slot-definition
                           (name accessor initform type read-only location fixed-vector align dcas-sibling documentation))
             :sealed)
  (name nil :read-only t)
  (accessor nil :read-only t)
  (initform nil :read-only t)
  (type t :read-only t)
  (read-only nil :read-only t)
  (location nil :read-only t)
  (fixed-vector nil :read-only t)
  (align nil :read-only t)
  (dcas-sibling nil :read-only t)
  (documentation nil :read-only t))
