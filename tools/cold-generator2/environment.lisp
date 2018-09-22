;;;; Copyright (c) 2018 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.cold-generator.environment
  (:use :cl)
  (:shadow #:intern #:make-array #:make-symbol)
  (:export #:make-standard-environment
           #:environment
           #:environment-target
           #:add-special
           #:find-special
           #:object-area
           #:cons-in-area
           #:list-in-area
           #:make-array
           #:cross-array-element-type
           #:intern
           #:make-symbol
           #:translate-symbol
           #:cross-symbol-value
           #:do-all-environment-symbols
           #:do-all-environment-frefs
           #:do-all-environment-structs
           #:symbol-value-cell
           #:symbol-value-cell-value
           #:symbol-value-cell-symbol
           #:symbol-global-value-cell
           #:symbol-global-value
           #:symbol-global-boundp
           #:symbol-global-makunbound
           #:cross-symbol-name
           #:cross-symbol-package
           #:cross-symbol-plist
           #:cross-symbol-type
           #:%defconstant
           #:function-boundp
           #:function-makunbound
           #:%defun
           #:function-reference
           #:function-reference-name
           #:function-reference-function
           #:make-function
           #:compile-lap
           #:cross-compiled-function
           #:function-machine-code
           #:function-gc-metadata
           #:function-constants
           #:function-fixups
           #:make-structure-definition
           #:register-structure-definition
           #:find-structure-definition
           #:structure-definition
           #:structure-definition-name
           #:structure-definition-slots
           #:structure-definition-parent
           #:structure-definition-area
           #:structure-definition-size
           ;; Heap layout, not a layout object.
           #:structure-definition-layout
           #:structure-definition-sealed
           #:make-structure-slot-definition
           #:structure-slot-definition
           #:structure-slot-definition-name
           #:structure-slot-definition-accessor
           #:structure-slot-definition-initform
           #:structure-slot-definition-type
           #:structure-slot-definition-read-only
           #:structure-slot-definition-ref-fn
           #:structure-slot-definition-index
           #:structure-slot-definition-fixed-vector
           #:structure-slot-definition-align
           #:make-instance-header
           #:instance-header
           #:instance-header-object
           #:make-byte
           #:cross-byte
           #:cross-byte-size
           #:cross-byte-position
           #:make-structure
           #:instance-object
           #:instance-structure-definition
           #:structure-slot-value
           #:make-stack
           #:stack
           #:stack-size
))

(in-package :mezzano.cold-generator.environment)

(defclass environment ()
  ((%target :initarg :target :reader environment-target)
   ;; (package-keyword . symbol-name) => symbol
   (%package-symbols :initform (make-hash-table :test 'equal) :reader environment-package-symbol-table)
   ;; Symbol => package mapping
   (%symbol-packages :initform (make-hash-table :weakness :key) :reader environment-symbol-package-table)
   ;; Symbol => global-value-cell mapping
   (%symbol-global-value-cell :initform (make-hash-table :weakness :key) :reader environment-symbol-global-value-cell-table)
   ;; function-name => function-reference mapping
   ;; FIXME: Should be weak, but how to deal with complex keys?
   ;; Maybe have separate hash tables for symbol/setf/cas names.
   (%name-frefs :initform (make-hash-table :test 'equal) :reader environment-name-fref-table)
   ;; Object allocation areas for non-instances.
   (%object-area :initform (make-hash-table :weakness :key) :reader environment-object-area-table)
   ;; Array element-type tracker.
   (%array-element-type :initform (make-hash-table :weakness :key) :reader environment-array-element-type-table)
   ;; Structure definition registry.
   ;; name => structure-definition
   (%structure-definitions :initform (make-hash-table :weakness :key-or-value)
                           :reader environment-structure-definition-table)
   ;; Special named values.
   (%specials :initform (make-hash-table) :reader environment-specials)
   ))

(defun add-special (environment name value)
  (setf (gethash name (environment-specials environment)) value))

(defun find-special (environment name)
  (multiple-value-bind (value presentp)
      (gethash name (environment-specials environment))
    (when (not presentp)
      (error "Missing special ~S" name))
    value))

(defclass function-reference ()
  ((%name :initarg :name :reader function-reference-name)
   (%function :initarg :function :accessor function-reference-function))
  (:default-initargs :function nil))

(defmethod print-object ((object function-reference) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (function-reference-name object))))

(defclass symbol-value-cell ()
  ((%symbol :initarg :symbol :reader symbol-value-cell-symbol)
   (%value :initarg :value :accessor symbol-value-cell-value)))

(defmethod print-object ((object symbol-value-cell) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (symbol-value-cell-symbol object))))

(defclass cross-compiled-function ()
  ((%machine-code :initarg :mc :reader function-machine-code)
   (%gcmd :initarg :gcmd :reader function-gc-metadata)
   (%constants :initarg :constants :reader function-constants)
   (%fixups :initarg :fixups :reader function-fixups)
   (%area :initarg :area :initform nil)))

(defmethod object-area (environment (object cross-compiled-function))
  (slot-value object '%area))

(defmethod print-object ((object cross-compiled-function) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (elt (function-constants object) 0))))

(defclass structure-definition ()
  ((%name :initarg :name :reader structure-definition-name)
   (%slots :initarg :slots :reader structure-definition-slots)
   (%parent :initarg :parent :reader structure-definition-parent)
   (%area :initarg :area :reader structure-definition-area)
   (%size :initarg :size :reader structure-definition-size)
   (%layout :initarg :layout :accessor structure-definition-layout)
   (%sealed :initarg :sealed :reader structure-definition-sealed)
   (%native-class :initform nil :accessor structure-definition-native-class)))

(defun make-structure-definition (environment name slots parent area size layout sealed)
  (declare (ignore environment))
  ;; FIXME: Copy slots list & layout to wired area.
  (check-type name symbol)
  (check-type parent (or null structure-definition))
  (make-instance 'structure-definition
                 :name name
                 :slots slots
                 :parent parent
                 :area area
                 :size size
                 :layout layout
                 :sealed sealed))

(defmethod print-object ((object structure-definition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (structure-definition-name object))))

(defun find-structure-definition (environment name &optional (errorp t))
  (check-type name symbol)
  (or (gethash name (environment-structure-definition-table environment))
      (and errorp
           (error "Unknown structure definition ~S" name))))

(defun check-structure-slot-definition-compatibility (existing-slot new-slot)
  (assert (eql (structure-slot-definition-name existing-slot)
               (structure-slot-definition-name new-slot)))
  (assert (eql (structure-slot-definition-accessor existing-slot)
               (structure-slot-definition-accessor new-slot)))
  ;; FIXME: This should be a proper type= test.
  (assert (equal (structure-slot-definition-type existing-slot)
                 (structure-slot-definition-type new-slot)))
  (assert (eql (structure-slot-definition-read-only existing-slot)
               (structure-slot-definition-read-only new-slot)))
  (assert (eql (structure-slot-definition-ref-fn existing-slot)
               (structure-slot-definition-ref-fn new-slot)))
  (assert (eql (structure-slot-definition-index existing-slot)
               (structure-slot-definition-index new-slot)))
  (assert (eql (structure-slot-definition-fixed-vector existing-slot)
               (structure-slot-definition-fixed-vector new-slot)))
  (assert (eql (structure-slot-definition-align existing-slot)
               (structure-slot-definition-align new-slot))))

(defun check-structure-definition-compatibility (environment existing new-sdef)
  (declare (ignore environment))
  (assert (eql (structure-definition-name existing)
               (structure-definition-name new-sdef)))
  (assert (eql (structure-definition-parent existing)
               (structure-definition-parent new-sdef)))
  (assert (eql (length (structure-definition-slots existing))
               (length (structure-definition-slots new-sdef))))
  (loop
     for existing-slot in (structure-definition-slots existing)
     for new-slot in (structure-definition-slots new-sdef)
     do (check-structure-slot-definition-compatibility existing-slot new-slot))
  (assert (eql (structure-definition-area existing)
               (structure-definition-area new-sdef)))
  (assert (eql (structure-definition-size existing)
               (structure-definition-size new-sdef)))
  (assert (equal (structure-definition-layout existing)
                 (structure-definition-layout new-sdef)))
  (assert (eql (structure-definition-sealed existing)
               (structure-definition-sealed new-sdef))))

(defun register-structure-definition (environment sdef)
  (let* ((name (structure-definition-name sdef))
         (existing (find-structure-definition environment name nil)))
    (cond (existing
           ;; Existing structure definition, check compatibility and return
           ;; the old one.
           (check-structure-definition-compatibility environment existing sdef)
           existing)
          (t
           ;; New definition. Register it & return it.
           (setf (gethash name (environment-structure-definition-table environment))
                 sdef)
           sdef))))

(defclass structure-slot-definition ()
  ((%name :initarg :name :reader structure-slot-definition-name)
   (%accessor :initarg :accessor :reader structure-slot-definition-accessor)
   (%initform :initarg :initform :reader structure-slot-definition-initform)
   (%type :initarg :type :reader structure-slot-definition-type)
   (%read-only :initarg :read-only :reader structure-slot-definition-read-only)
   (%ref-fn :initarg :ref-fn :reader structure-slot-definition-ref-fn)
   (%index :initarg :index :reader structure-slot-definition-index)
   (%fixed-vector :initarg :fixed-vector :reader structure-slot-definition-fixed-vector)
   (%align :initarg :align :reader structure-slot-definition-align)))

(defun make-structure-slot-definition (environment name accessor initform type read-only ref-fn index fixed-vector align)
  (declare (ignore environment))
  (make-instance 'structure-slot-definition
                 :name name
                 :accessor accessor
                 :initform initform
                 :type type
                 :read-only read-only
                 :ref-fn ref-fn
                 :index index
                 :fixed-vector fixed-vector
                 :align align))

(defmethod print-object ((object structure-slot-definition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S"
            (list :name (structure-slot-definition-name object)
                  :accessor (structure-slot-definition-accessor object)
                  :initform (structure-slot-definition-initform object)
                  :type (structure-slot-definition-type object)
                  :read-only (structure-slot-definition-read-only object)
                  :ref-fn (structure-slot-definition-ref-fn object)
                  :index (structure-slot-definition-index object)
                  :fixed-vector (structure-slot-definition-fixed-vector object)
                  :align (structure-slot-definition-align object)))))

(defclass instance-header ()
  ((%object :initarg :object :reader instance-header-object)))

(defun make-instance-header (environment object)
  (declare (ignore environment))
  (make-instance 'instance-header :object object))

(defmethod print-object ((object instance-header) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (instance-header-object object))))

(defclass cross-byte ()
  ((%size :initarg :size :reader cross-byte-size)
   (%position :initarg :position :reader cross-byte-position)))

(defun make-byte (size position)
  (make-instance 'cross-byte :size size :position position))

(defmethod print-object ((object cross-byte) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (list :size (cross-byte-size object)
                              :position (cross-byte-position object)))))

(defun make-standard-environment (&rest initargs)
  (let ((env (apply #'make-instance 'environment initargs)))
    (do-external-symbols (symbol :cl)
      (setf (gethash (cons (symbol-name symbol) :common-lisp) (environment-package-symbol-table env)) symbol
            (gethash symbol (environment-symbol-package-table env)) :common-lisp
            (gethash (symbol-name symbol) (environment-object-area-table env)) :wired))
    (setf (cross-symbol-value env 'nil) 'nil)
    (setf (cross-symbol-value env 't) 't)
    env))

(defgeneric object-area (environment object))

(defmethod object-area (environment object)
  (values (gethash object (environment-object-area-table environment))))

(defun symbol-global-value-cell (environment symbol)
  (check-type symbol symbol)
  (let ((cell (gethash symbol (environment-symbol-global-value-cell-table environment))))
    (when (not cell)
      (setf cell (make-instance 'symbol-value-cell :symbol symbol)
            (gethash symbol (environment-symbol-global-value-cell-table environment)) cell)
      (when (keywordp symbol)
        (setf (symbol-global-value environment symbol) symbol)))
    cell))

(defun symbol-global-value (environment symbol)
  (symbol-value-cell-value (symbol-global-value-cell environment symbol)))

(defun (setf symbol-global-value) (value environment symbol)
  (setf (symbol-value-cell-value (symbol-global-value-cell environment symbol)) value))

(defun symbol-global-boundp (environment symbol)
  (slot-boundp (symbol-global-value-cell environment symbol) '%value))

(defun symbol-global-makunbound (environment symbol)
  (slot-makunbound (symbol-global-value-cell environment symbol) '%value))

(defun cross-symbol-name (environment symbol)
  ;; Make sure symbol names are in the wired area.
  (let ((name (symbol-name symbol)))
    (setf (gethash name (environment-object-area-table environment)) :wired)
    name))

(defun cross-symbol-package (environment symbol)
  (values (gethash symbol (environment-symbol-package-table environment))))

(defun cross-symbol-plist (environment symbol)
  (declare (ignore environment symbol))
  '())

(defun cross-symbol-type (environment symbol)
  (declare (ignore environment symbol))
  't)

(defun %defconstant (environment symbol value &optional docstring)
  (setf (symbol-global-value environment symbol) value))

(defmacro do-all-environment-symbols ((symbol environment &optional result) &body body)
  (let ((key (gensym)))
    `(progn
       (maphash (lambda (,key ,symbol)
                  (declare (ignore ,key))
                  ,@body)
                (environment-package-symbol-table ,environment))
       ,result)))

(defmacro do-all-environment-structs ((sdef environment &optional result) &body body)
  (let ((key (gensym)))
    `(progn
       (maphash (lambda (,key ,sdef)
                  (declare (ignore ,key))
                  ,@body)
                (environment-structure-definition-table ,environment))
       ,result)))

(defmacro do-all-environment-frefs ((fref environment &optional result) &body body)
  (let ((key (gensym)))
    `(progn
       (maphash (lambda (,key ,fref)
                  (declare (ignore ,key))
                  ,@body)
                (environment-name-fref-table ,environment))
       ,result)))

(defun translate-symbol (environment symbol)
  "Translate a host symbol to a cross symbol in ENVIRONMENT."
  (intern environment
          (symbol-name symbol)
          (package-name (symbol-package symbol))))

(defun cross-symbol-value (environment symbol)
  (symbol-global-value
   environment
   (translate-symbol environment symbol)))

(defun (setf cross-symbol-value) (value environment symbol)
  (setf (symbol-global-value
         environment
         (translate-symbol environment symbol))
        value))

(defun make-symbol (environment name)
  (let ((symbol (cl:make-symbol name)))
    (setf (gethash (symbol-name symbol) (environment-object-area-table environment)) :wired)
    symbol))

(defun intern (environment name package)
  (let* ((pkg-keyword (cl:intern (string package) :keyword))
         (name-string (string name))
         (key (cons name-string pkg-keyword)))
    (multiple-value-bind (symbol presentp)
        (gethash key (environment-package-symbol-table environment))
      (when (not presentp)
        (setf symbol (if (eql pkg-keyword :keyword)
                         (cl:intern name-string :keyword)
                         (cl:make-symbol name-string))
              (gethash key (environment-package-symbol-table environment)) symbol
              (gethash symbol (environment-symbol-package-table environment)) pkg-keyword
              (gethash (symbol-name symbol) (environment-object-area-table environment)) :wired))
      symbol)))

(defun function-boundp (environment name)
  (slot-boundp (function-reference environment name) '%function))

(defun function-makunbound (environment name)
  (slot-makunbound (function-reference environment name) '%function))

(defun %defun (environment name value)
  (setf (function-reference-function (function-reference environment name)) value)
  name)

(defun function-reference (environment name &optional (createp t))
  (check-type name (or symbol (cons symbol (cons symbol null))))
  (let ((fref (gethash name (environment-name-fref-table environment))))
    (when (and (not fref)
               createp)
      (setf fref (make-instance 'function-reference :name name)
            (gethash name (environment-name-fref-table environment)) fref))
    fref))

(defun make-function (environment machine-code gc-metadata constants fixups area)
  (declare (ignore environment))
  (make-instance 'cross-compiled-function
                 :mc machine-code
                 :gcmd gc-metadata
                 :constants (coerce constants 'vector)
                 :fixups fixups
                 :area area))

(defun compile-lap (environment code &key (area :pinned) name)
  "Compile a list of LAP code as a function."
  (multiple-value-bind (mc constants fixups symbols gc-info)
      (let ((sys.lap:*function-reference-resolver*
             (lambda (name)
               ;; Translate function-reference names from host names to
               ;; names in the environment.
               (function-reference environment (translate-symbol environment name)))))
        (sys.lap:perform-assembly-using-target
         (sys.c::canonicalize-target (environment-target environment))
         code
         :initial-symbols (list '(nil . :fixup)
                                '(t . :fixup))
         :info (list name nil)))
    (values
     (make-function environment mc gc-info constants fixups area)
     symbols)))

(defun cons-in-area (car cdr environment area)
  (let ((c (cons car cdr)))
    (when area
      (setf (gethash c (environment-object-area-table environment)) area))
    c))

(defun list-in-area (environment area &rest objects)
  (when area
    (do ((i objects (cdr i)))
        ((endp i))
      (setf (gethash i (environment-object-area-table environment)) area)))
  objects)

(defun make-array (environment dimensions &rest initargs &key (element-type t) area &allow-other-keys)
  ;; Remove :AREA key, if any.
  (remf initargs :area)
  (let ((array (apply #'cl:make-array dimensions initargs)))
    (when area
      (setf (gethash array (environment-object-area-table environment)) area))
    ;; Keep track of the element-type, not all implementations support all
    ;; element-types.
    (when (not (eql element-type 't))
      (setf (gethash array (environment-array-element-type-table environment)) element-type))
    array))

(defun cross-array-element-type (environment array)
  (values (gethash array (environment-array-element-type-table environment) 't)))

(defclass instance-class (standard-class)
  ((%sdef :initarg :sdef :reader instance-class-structure-definition)))

(defmethod sb-mop:validate-superclass ((class instance-class) (superclass standard-class))
  t)

(defclass instance-object (standard-object)
  ()
  (:metaclass instance-class))

(defun instance-structure-definition (instance)
  (instance-class-structure-definition (class-of instance)))

(defun make-structure (environment type &rest initargs)
  (let* ((sym (translate-symbol environment type))
         (sdef (find-structure-definition environment sym))
         (class (structure-definition-native-class sdef)))
    (when (not class)
      ;; Create an instance class for this structure definition.
      ;; TODO: Set direct superclasses properly. This involves figuring out
      ;; which slots are inherited.
      (setf class (make-instance
                   'instance-class
                   :name (structure-definition-name sdef)
                   :sdef sdef
                   :direct-superclasses (list (find-class 'instance-object))
                   :direct-slots (loop
                                    for slot in (structure-definition-slots sdef)
                                    collect (list :name (structure-slot-definition-name slot)
                                                  :initform (structure-slot-definition-initform slot)
                                                  :initfunction (let ((slot slot))
                                                                  (lambda ()
                                                                    ;; Ehhhhhhhhhhhhhhhhh.
                                                                    (let ((val (eval (structure-slot-definition-initform slot))))
                                                                      (if (structure-slot-definition-fixed-vector slot)
                                                                          (cl:make-array (structure-slot-definition-fixed-vector slot) :initial-element val)
                                                                          val))))
                                                  :initargs (list (cl:intern (symbol-name (structure-slot-definition-name slot)) :keyword)))))
            (structure-definition-native-class sdef) class))
    (apply #'make-instance class initargs)))

(defun structure-slot-value (environment object slot-name)
  (let ((sym (translate-symbol environment slot-name)))
    (slot-value object sym)))

(defun (setf structure-slot-value) (value environment object slot-name)
  (let ((sym (translate-symbol environment slot-name)))
    (setf (slot-value object sym) value)))

(defclass stack ()
  ((%size :initarg :size :reader stack-size)))

(defun make-stack (environment size)
  (declare (ignore environment))
  (make-instance 'stack :size size))
