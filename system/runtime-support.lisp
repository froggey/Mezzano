;;;; Miscellaneous runtime functions

(in-package :mezzano.internals)

(setf mezzano.lap:*function-reference-resolver* #'function-reference)

(defstruct function-info
  compiler-macro
  inline-form
  inline-mode)

(defvar *symbol-function-info*)
(defvar *setf-function-info*)
(defvar *cas-function-info*)
(defvar *function-info-lock*)

(defun function-info-for (name &optional (create t))
  (multiple-value-bind (name-root location)
      (decode-function-name name)
    (let* ((table (ecase location
                    (symbol *symbol-function-info*)
                    (setf *setf-function-info*)
                    (cas *cas-function-info*)))
           (entry (mezzano.supervisor:with-rw-lock-read (*function-info-lock*)
                    (gethash name-root table))))
      (when (and (not entry) create)
        (let ((new-entry (make-function-info)))
          (mezzano.supervisor:with-rw-lock-write (*function-info-lock*)
            (setf entry (or (cas (gethash name-root table) nil new-entry)
                            new-entry)))))
      entry)))

(defun proclaim-symbol-mode (symbol new-mode)
  (check-type symbol symbol)
  (when (not (or (null (symbol-mode symbol))
                 (eql (symbol-mode symbol) new-mode)))
    (cerror "Continue" "Symbol ~S being changed from ~S to ~S."
            symbol (symbol-mode symbol) new-mode))
  (setf (symbol-mode symbol) new-mode))

(defvar *known-declarations* '())

(defun proclaim-type (typespec vars)
  (dolist (name vars)
    (check-type name symbol)
    (when (and (boundp name)
               (not (typep (symbol-value name) typespec)))
      (cerror "Continue" "Symbol ~S's type being proclaimed to ~S, but current value ~S has an incompatible type."
              name typespec (symbol-value name)))
    (setf (mezzano.runtime::symbol-type name) typespec)))

(defun known-declaration-p (declaration)
  (or (member declaration '(special constant global inline notinline
                            maybe-inline type ftype declaration optimize))
      (type-specifier-p declaration)
      (member declaration *known-declarations*)))

(defun proclaim (declaration-specifier)
  (case (first declaration-specifier)
    (special
     (dolist (var (rest declaration-specifier))
       (proclaim-symbol-mode var :special)))
    (constant
     (dolist (var (rest declaration-specifier))
       (proclaim-symbol-mode var :constant)))
    (global
     (dolist (var (rest declaration-specifier))
       (proclaim-symbol-mode var :global)))
    (inline
     (dolist (name (rest declaration-specifier))
       (setf (function-info-inline-mode
              (function-info-for name))
             t)))
    (notinline
     (dolist (name (rest declaration-specifier))
       (setf (function-info-inline-mode
              (function-info-for name))
             nil)))
    (maybe-inline
     (dolist (name (rest declaration-specifier))
       (setf (function-info-inline-mode
              (function-info-for name))
             :maybe)))
    (type
     (destructuring-bind (typespec &rest vars)
         (rest declaration-specifier)
       (proclaim-type typespec vars)))
    (ftype)
    (declaration
     (dolist (name (rest declaration-specifier))
       (check-type name symbol)
       (pushnew name *known-declarations*)))
    (optimize
     (dolist (quality (rest declaration-specifier))
       (destructuring-bind (quality value)
           (if (symbolp quality)
               `(,quality 3)
               quality)
         (check-type quality (member compilation-speed debug safety space speed))
         (check-type value (member 0 1 2 3))
         (setf (getf mezzano.compiler::*optimize-policy* quality) value))))
    (t
     (cond ((type-specifier-p (first declaration-specifier))
            ;; Actually a type declaration.
            (proclaim-type (first declaration-specifier)
                           (rest declaration-specifier)))
           ((known-declaration-p (first declaration-specifier))
            (warn "Unknown declaration ~S" declaration-specifier))))))

(defun variable-information (symbol)
  (symbol-mode symbol))

(defun mezzano.compiler::function-inline-info (name)
  (let ((info (function-info-for name nil)))
    (if info
        (values (eql (function-info-inline-mode info) 't)
                (function-info-inline-form info))
        (values nil nil))))

;;; Turn (APPLY fn args...) into (%APPLY fn (list* args...)), bypassing APPLY's
;;; rest-list generation.
(define-compiler-macro apply (function arg &rest more-args)
  (if more-args
      (let ((function-sym (gensym))
            (args-sym (gensym)))
        `(let ((,function-sym ,function)
               (,args-sym (list* ,arg ,@more-args)))
           (declare (dynamic-extent ,args-sym))
           (mezzano.runtime::%apply (%coerce-to-callable ,function-sym) ,args-sym)))
      `(mezzano.runtime::%apply (%coerce-to-callable ,function) ,arg)))

(deftype function-designator ()
  `(or function symbol))

(defun apply (function arg &rest more-args)
  (declare (dynamic-extent more-args))
  (check-type function function-designator)
  (when (symbolp function)
    (setf function (%coerce-to-callable function)))
  (cond (more-args
         ;; Convert (... (final-list ...)) to (... final-list...)
         ;; This modifies the dx more-args rest list, but that's ok as apply
         ;; isn't inlined so it will never share structure with an existing list.
         (do* ((arg-list (cons arg more-args))
               (i arg-list (cdr i)))
              ((null (cddr i))
               (setf (cdr i) (cadr i))
               (mezzano.runtime::%apply function arg-list))
           (declare (dynamic-extent arg-list))))
        (t (mezzano.runtime::%apply function arg))))

(defun funcall (function &rest arguments)
  (declare (dynamic-extent arguments))
  (apply function arguments))

(defun mezzano.runtime::%funcall (function &rest arguments)
  (declare (dynamic-extent arguments))
  (apply function arguments))

(defun values (&rest values)
  (declare (dynamic-extent values))
  (values-list values))

(declaim (inline constantly))
(defun constantly (value)
  (lambda (&rest arguments)
    (declare (ignore arguments))
    value))

(defstruct macro-definition
  function
  lambda-list)

(defvar *macros*)

(defun macro-function (symbol &optional env)
  (check-type symbol symbol)
  (cond (env
         (mezzano.compiler::macro-function-in-environment symbol env))
        (t
         (let ((entry (gethash symbol *macros*)))
           (when entry
             (macro-definition-function entry))))))

(defun get-macro-definition (symbol &optional (createp t))
  (let ((entry (gethash symbol *macros*)))
    (when (and (not entry) createp)
      (let ((new-entry (make-macro-definition)))
        (setf entry (or (cas (gethash symbol *macros*) nil new-entry)
                        new-entry))))
    entry))

(defun (setf macro-function) (value symbol &optional env)
  (check-type value (or null function))
  (check-type symbol symbol)
  (when env
    (error "TODO: (Setf Macro-function) in environment."))
  (cond (value
         (setf (symbol-function symbol) (lambda (&rest r)
                                          (declare (ignore r))
                                          (error 'undefined-function :name symbol)))
         (let ((entry (get-macro-definition symbol)))
           (setf (macro-definition-function entry) value
                 (macro-definition-lambda-list entry) nil)))
        (t
         (let ((entry (get-macro-definition symbol nil)))
           (when (and entry (macro-definition-function entry))
             (fmakunbound symbol)
             (setf (macro-definition-function entry) nil
                   (macro-definition-lambda-list entry) nil)))))
  value)

(defun macro-function-lambda-list (symbol)
  (check-type symbol symbol)
  (let ((entry (gethash symbol *macros*)))
    (when entry
      (macro-definition-lambda-list entry))))

(defun compiler-macro-function (name &optional environment)
  (cond (environment
         (mezzano.compiler::compiler-macro-function-in-environment name environment))
        (t
         (let ((info (function-info-for name nil)))
           (if info
               (function-info-compiler-macro info)
               nil)))))

(defun (setf compiler-macro-function) (value name &optional environment)
  (check-type value (or function null))
  (when environment
    (error "TODO: (Setf Compiler-Macro-function) in environment."))
  (setf (function-info-compiler-macro (function-info-for name)) value)
  value)

(define-compiler-macro list-in-area (area &rest args)
  (let ((area-sym (gensym "AREA")))
    `(let ((,area-sym ,area))
       ,(loop
          with result = nil
          for arg in (reverse args)
          do (setf result `(cons-in-area ,arg ,result ,area-sym))
          finally (return result)))))

(defun list-in-area (area &rest args)
  (declare (dynamic-extent args))
  (copy-list-in-area args area))

(defun list (&rest args)
  args)

(defun copy-list-in-area (list &optional area)
  (cond ((null list) '())
        ((consp list)
         (do* ((result (cons nil nil))
               (tail result)
               (l list (cdr l)))
              ((not (consp l))
               (setf (cdr tail) l)
               (cdr result))
           (declare (dynamic-extent result))
           (setf (cdr tail) (cons-in-area (car l) nil area)
                 tail (cdr tail))))
        (t
         (error 'type-error :datum list :expected-type 'list))))

(defun copy-list (list)
  (cond ((null list) '())
        ((consp list)
         (do* ((result (cons nil nil))
               (tail result)
               (l list (cdr l)))
              ((not (consp l))
               (setf (cdr tail) l)
               (cdr result))
           (declare (dynamic-extent result))
           (setf (cdr tail) (cons (car l) nil)
                 tail (cdr tail))))
        (t
         (error 'type-error :datum list :expected-type 'list))))

;;; Will be overriden later in the init process.
(when (not (fboundp 'funcallable-instance-lambda-expression))
  (defun funcallable-instance-lambda-expression (function)
    (declare (ignore function))
    (values nil t nil))
  (defun funcallable-instance-debug-info (function)
    (declare (ignore function))
    nil)
  (defun funcallable-instance-compiled-function-p (function)
    (declare (ignore function))
    nil)
  )

;;; Implementations of DEFUN/etc, the cross-compiler defines these as well.

(defun %defmacro (name function &optional lambda-list documentation)
  (check-type name symbol)
  (check-type function function)
  (setf (macro-function name) function)
  (setf (macro-definition-lambda-list (get-macro-definition name)) lambda-list)
  (set-function-docstring name documentation)
  name)

(defun %define-compiler-macro (name function &optional documentation)
  (setf (compiler-macro-function name) function)
  (set-compiler-macro-docstring name documentation)
  name)

(defun %compiler-defun (name source-lambda)
  "Compile-time defun code. Store the inline form if required."
  (let ((info (function-info-for name nil)))
    (when (and info
               (or (function-info-inline-mode info)
                   (function-info-inline-form info)))
      (setf (function-info-inline-form info) source-lambda)))
  nil)

(defun %defun (name lambda &optional documentation)
  (setf (fdefinition name) lambda)
  (set-function-docstring name documentation)
  name)

(defun convert-structure-definition-direct-slots (sdef)
  (let* ((direct-slots (remove-if (lambda (slot)
                                    (and (structure-definition-parent sdef)
                                         (find (structure-slot-definition-name slot)
                                               (structure-definition-slots sdef)
                                               :key #'structure-slot-definition-name)))
                                  (structure-definition-slots sdef)))
         (s-d-s-d (find-class 'mezzano.clos::structure-direct-slot-definition))
         (s-d-s-d-layout (mezzano.runtime::instance-access-by-name s-d-s-d 'mezzano.clos::slot-storage-layout)))
    (loop
       for slot in direct-slots
       collect (let ((new (sys.int::%allocate-instance s-d-s-d-layout)))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::name)
                       (structure-slot-definition-name slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::type)
                       (structure-slot-definition-type slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::read-only)
                       (structure-slot-definition-read-only slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::initform)
                       (structure-slot-definition-initform slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::fixed-vector)
                       (structure-slot-definition-fixed-vector slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::align)
                       (structure-slot-definition-align slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::dcas-sibling)
                       (structure-slot-definition-dcas-sibling slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::documentation)
                       (structure-slot-definition-documentation slot))
                 new))))

(defun convert-structure-definition-effective-slots (sdef)
  (let* ((s-e-s-d (find-class 'mezzano.clos::structure-effective-slot-definition))
         (s-e-s-d-layout (mezzano.runtime::instance-access-by-name s-e-s-d 'mezzano.clos::slot-storage-layout)))
    (loop
       for slot in (structure-definition-slots sdef)
       collect (let ((new (sys.int::%allocate-instance s-e-s-d-layout)))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::name)
                       (structure-slot-definition-name slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::type)
                       (structure-slot-definition-type slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::read-only)
                       (structure-slot-definition-read-only slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::initform)
                       (structure-slot-definition-initform slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::fixed-vector)
                       (structure-slot-definition-fixed-vector slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::align)
                       (structure-slot-definition-align slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::dcas-sibling)
                       (structure-slot-definition-dcas-sibling slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::documentation)
                       (structure-slot-definition-documentation slot))
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::location)
                       (structure-slot-definition-location slot))
                 new))))

(defun convert-structure-definition-instance-slots (sdef)
  (loop
     with instance-slots = (make-array (* (length (structure-definition-slots sdef)) 2)
                                       :area :wired)
     for slot in (structure-definition-slots sdef)
     for i by 2
     do
       (setf (aref instance-slots i) (structure-slot-definition-name slot)
             (aref instance-slots (1+ i)) (structure-slot-definition-location slot))
     finally (return instance-slots)))

(defun populate-struct-class-from-structure-defintion (new-class sdef)
  (let* ((parent (structure-definition-parent sdef))
         (parent-class (or (and parent (%defstruct parent))
                           (find-class 'structure-object))))
    (when (mezzano.runtime::instance-access-by-name parent-class 'mezzano.clos::sealed)
      (error "Attempted to make structure class ~S that includes sealed structure ~S"
             (structure-definition-name sdef)
             (mezzano.runtime::instance-access-by-name parent-class 'mezzano.clos::name)))
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::name)
          (structure-definition-name sdef))
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::direct-superclasses)
          (list parent-class))
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::direct-slots)
          (convert-structure-definition-direct-slots sdef))
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::precedence-list)
          (list* new-class (mezzano.runtime::instance-access-by-name parent-class 'mezzano.clos::precedence-list)))
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::effective-slots)
          (convert-structure-definition-effective-slots sdef))
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::direct-default-initargs)
          '())
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::finalized-p)
          t)
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::default-initargs)
          '())
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::sealed)
          (structure-definition-sealed sdef))
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::allocation-area)
          (structure-definition-area sdef))
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::parent)
          parent-class)
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::constructor)
          nil)
    (setf (mezzano.runtime::instance-access-by-name new-class 'documentation)
          (structure-definition-docstring sdef))
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::has-standard-constructor)
          (structure-definition-has-standard-constructor sdef))))

(defun convert-structure-definition-to-class (sdef source-location)
  ;; CLOS might not be fully initialized at this point,
  ;; construct the new class by hand.
  (let* ((s-c (find-class 'structure-class))
         (s-c-layout (mezzano.runtime::instance-access-by-name s-c 'mezzano.clos::slot-storage-layout))
         (new-class (sys.int::%allocate-instance s-c-layout))
         (parent (structure-definition-parent sdef))
         (parent-class (or (and parent (%defstruct parent))
                           (find-class 'structure-object))))
    (populate-struct-class-from-structure-defintion
     new-class sdef)
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::dependents)
          '())
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::direct-subclasses)
          (mezzano.garbage-collection.weak-objects:make-weak-list '()))
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::direct-methods)
          '())
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::hash)
          (mezzano.clos::next-class-hash-value))
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::prototype)
          mezzano.clos::*secret-unbound-value*)
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::slot-storage-layout)
          (make-layout :class new-class
                       :obsolete nil
                       :heap-size (structure-definition-size sdef)
                       :heap-layout (layout-heap-layout (structure-definition-layout sdef))
                       :area (structure-definition-area sdef)
                       :instance-slots (convert-structure-definition-instance-slots sdef)))
    (setf (mezzano.runtime::instance-access-by-name new-class 'mezzano.clos::source-location)
          source-location)
    (pushnew new-class
             (mezzano.garbage-collection.weak-objects:weak-list-list
              (mezzano.runtime::instance-access-by-name parent-class 'mezzano.clos::direct-subclasses)))
    new-class))

(defun structure-slot-definition-trivially-compatible-p (existing-structure-class new-slot)
  (let ((existing-slot (find (sys.int::structure-slot-definition-name new-slot)
                             (mezzano.runtime::instance-access-by-name existing-structure-class 'mezzano.clos::effective-slots)
                             :key (lambda (slot)
                                    (mezzano.runtime::instance-access-by-name slot 'mezzano.clos::name)))))
    (and existing-slot
         (equal (sys.int::structure-slot-definition-type new-slot)
                (mezzano.runtime::instance-access-by-name existing-slot 'mezzano.clos::type))
         (eql (sys.int::structure-slot-definition-read-only new-slot)
              (mezzano.runtime::instance-access-by-name existing-slot 'mezzano.clos::read-only))
         (eql (sys.int::structure-slot-definition-location new-slot)
              (mezzano.runtime::instance-access-by-name existing-slot 'mezzano.clos::location))
         (eql (sys.int::structure-slot-definition-fixed-vector new-slot)
              (mezzano.runtime::instance-access-by-name existing-slot 'mezzano.clos::fixed-vector))
         (eql (sys.int::structure-slot-definition-align new-slot)
              (mezzano.runtime::instance-access-by-name existing-slot 'mezzano.clos::align))
         (eql (sys.int::structure-slot-definition-dcas-sibling new-slot)
              (mezzano.runtime::instance-access-by-name existing-slot 'mezzano.clos::dcas-sibling))
         (equal (sys.int::structure-slot-definition-initform new-slot)
                (mezzano.runtime::instance-access-by-name existing-slot 'mezzano.clos::initform)))))

(defun structure-definition-trivially-compatible-p (existing-structure-class sdef)
  (let* ((parent (structure-definition-parent sdef))
         (parent-class (or (and parent (%defstruct parent))
                           (find-class 'structure-object))))
    (and (eql (mezzano.runtime::instance-access-by-name existing-structure-class 'mezzano.clos::name)
              (structure-definition-name sdef))
         (eql (mezzano.runtime::instance-access-by-name existing-structure-class 'mezzano.clos::parent)
              parent-class)
         (eql (mezzano.runtime::instance-access-by-name existing-structure-class 'mezzano.clos::allocation-area)
              (structure-definition-area sdef))
         (eql (sys.int::layout-heap-size (mezzano.runtime::instance-access-by-name existing-structure-class 'mezzano.clos::slot-storage-layout))
              (structure-definition-size sdef))
         (equal (sys.int::layout-heap-layout (mezzano.runtime::instance-access-by-name existing-structure-class 'mezzano.clos::slot-storage-layout))
                (sys.int::layout-heap-layout (structure-definition-layout sdef)))
         (eql (mezzano.runtime::instance-access-by-name existing-structure-class 'mezzano.clos::sealed)
              (structure-definition-sealed sdef))
         (eql (length (mezzano.runtime::instance-access-by-name existing-structure-class 'mezzano.clos::effective-slots))
              (length (sys.int::structure-definition-slots sdef)))
         (every (lambda (slot)
                  (structure-slot-definition-trivially-compatible-p existing-structure-class slot))
                (sys.int::structure-definition-slots sdef)))))

(defun %defstruct (structure-type &key location)
  (when (mezzano.runtime::structure-class-p structure-type)
    ;; Happens during cold load.
    (when location
      (setf (mezzano.runtime::instance-access-by-name structure-type 'mezzano.clos::source-location)
            location))
    (return-from %defstruct structure-type))
  (let* ((name (structure-definition-name structure-type))
         (existing (find-class name nil)))
    (cond (existing
           (when (not (structure-definition-trivially-compatible-p existing structure-type))
             (mezzano.clos::redefine-structure-type existing structure-type))
           (when location
             (setf (slot-value existing 'mezzano.clos::source-location) location))
           existing)
          (t
           (setf (find-class name) (convert-structure-definition-to-class structure-type location))))))

(defparameter *incompatible-constant-redefinition-is-an-error* nil)
(defparameter *defconstant-redefinition-comparator* 'eql)

(defun %defconstant (name value source-location &optional docstring)
  (cond ((boundp name)
         (let ((old-value (symbol-value name)))
           (when (not (funcall (or (and (boundp '*defconstant-redefinition-comparator*)
                                        *defconstant-redefinition-comparator*)
                                   #'eql)
                               old-value value))
             (when *incompatible-constant-redefinition-is-an-error*
               (cerror "Redefine the constant"
                       'defconstant-uneql
                       :name name
                       :old-value old-value
                       :new-value value))
             (setf (symbol-mode name) :special)
             (setf (symbol-value name) value))))
        (t
         (setf (symbol-value name) value)))
  (setf (symbol-mode name) :constant)
  (when docstring
    (set-variable-docstring name docstring))
  (set-variable-source-location name source-location 'defconstant)
  name)

;;; Documentation helpers.
;;; Needed early because they're called before DOCUMENTATION et al is defined.

(defun set-function-docstring (name docstring)
  (check-type docstring (or null string))
  (if docstring
      (setf (gethash name *function-documentation*) docstring)
      (remhash name *function-documentation*)))

(defun set-compiler-macro-docstring (name docstring)
  (check-type docstring (or null string))
  (if docstring
      (setf (gethash name *compiler-macro-documentation*) docstring)
      (remhash name *compiler-macro-documentation*)))

(defun set-variable-docstring (name docstring)
  (check-type docstring (or null string))
  (if docstring
      (setf (gethash name *variable-documentation*) docstring)
      (remhash name *variable-documentation*)))

(defun set-setf-docstring (name docstring)
  (check-type docstring (or string null))
  (if docstring
      (setf (gethash name *setf-documentation*) docstring)
      (remhash name *setf-documentation*)))

(defvar *variable-source-locations*)

(defun set-variable-source-location (name source-location &optional (style 'defvar))
  (if source-location
      (setf (gethash name *variable-source-locations*) (cons source-location style))
      (remhash name *variable-source-locations*)))

(defun variable-source-location (name)
  (let ((entry (gethash name *variable-source-locations*)))
    (values (car entry) (cdr entry))))

;;; Function references, FUNCTION, et al.

(deftype function-name ()
  '(or
    symbol
    (cons (member setf cas) (cons symbol null))))

(defglobal *setf-fref-table*)
(defglobal *cas-fref-table*)

(defun make-function-reference (name)
  (let ((fref (mezzano.runtime::%allocate-function
               +object-tag-function-reference+ 0 8 t)))
    (setf (%object-ref-t fref +fref-name+) name)
    ;; Undefined frefs point directly at raise-undefined-function.
    (setf (%object-ref-unsigned-byte-64 fref +fref-undefined-entry-point+)
          (%function-reference-code-location
           (function-reference 'raise-undefined-function)))
    (setf (%object-ref-t fref +fref-function+) fref)
    ;; (nop (:rax))
    ;; (jmp <direct-target>) ; initially 0 to activate the fallback path.
    (setf (%object-ref-unsigned-byte-64 fref (+ +fref-code+ 0))
          #xE9001F0F)
    ;; (mov :rbx (:rip fref-function))
    ;; (jmp ...
    (setf (%object-ref-unsigned-byte-64 fref (+ sys.int::+fref-code+ 1))
          #xFFFFFFFFE91D8B48)
    ;; ... (:object :rbx entry-point))
    ;; (ud2)
    (setf (%object-ref-unsigned-byte-64 fref (+ sys.int::+fref-code+ 2))
          #x0B0FFF63)
    fref))

(defun valid-function-name-p (name)
  (typep name '(or symbol
                (cons (member setf cas)
                 (cons symbol null)))))

(defun decode-function-name (name)
  (cond ((symbolp name)
         (values name 'symbol))
        ((and (consp name) (valid-function-name-p name))
         (values (second name) (first name)))
        (t
         (error 'type-error
                  :expected-type 'function-name
                  :datum name))))

(defun function-reference (name &optional (create t))
  "Convert a function name to a function reference.
If no function-reference exists for NAME and CREATE is true, a new
fref will be created and associated with the name. If CREATE is false
then NIL will be returned."
  (multiple-value-bind (name-root location)
      (decode-function-name name)
    (ecase location
      (symbol
       (or (%object-ref-t name-root +symbol-function+)
           ;; No fref, create one and add it to the function.
           (and create
                (let ((new-fref (make-function-reference name-root)))
                  ;; Try to atomically update the function cell.
                  (multiple-value-bind (successp old-value)
                      (%cas-object name-root +symbol-function+ nil new-fref)
                    (if successp
                        new-fref
                        old-value))))))
      (setf
       (let ((fref (gethash name-root *setf-fref-table*)))
         (when (and (not fref) create)
           (let ((new-fref (make-function-reference name)))
             (setf fref (or (cas (gethash name-root *setf-fref-table*) nil new-fref)
                            new-fref))))
         fref))
      (cas
       (let ((fref (gethash name-root *cas-fref-table*)))
         (when (and (not fref) create)
           (let ((new-fref (make-function-reference name)))
             (setf fref (or (cas (gethash name-root *cas-fref-table*) nil new-fref)
                            new-fref))))
         fref)))))

(defun function-reference-p (object)
  (%object-of-type-p object +object-tag-function-reference+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.int::%define-type-symbol
   'function-reference
   'function-reference-p))

(defun function-reference-name (fref)
  (check-type fref function-reference)
  (%object-ref-t fref +fref-name+))

(defun function-reference-function (fref)
  (check-type fref function-reference)
  (let ((fn (%object-ref-t fref +fref-function+)))
    (if (eq fn fref)
        nil
        fn)))

(defun %activate-function-reference-full-path (fref)
  (setf (%object-ref-unsigned-byte-32 fref (1+ (* +fref-code+ 2))) 0))

(defun %function-reference-code-location (fref)
  (mezzano.runtime::%object-slot-address fref +fref-code+))

(defun %activate-function-reference-fast-path (fref entry-point)
  ;; Address is *after* the jump.
  (let* ((fref-jmp-address (+ (%function-reference-code-location fref) 8))
         (rel-jump (- entry-point fref-jmp-address)))
    (check-type rel-jump (signed-byte 32))
    (setf (%object-ref-signed-byte-32 fref (1+ (* +fref-code+ 2)))
          rel-jump)))

(defun (setf function-reference-function) (value fref)
  "Update the function field of a function-reference.
VALUE may be nil to make the fref unbound."
  (check-type value (or function null))
  (check-type fref function-reference)
  ;; FIXME: FREF should be locked for the duration.
  ;; FIXME: Fences.
  ;; FIXME: Cross-CPU synchronization.
  (cond
    ((not value)
     ;; Making it unbound.
     (setf (%object-ref-t fref +fref-function+) fref)
     (%activate-function-reference-full-path fref))
    ((%object-of-type-p value +object-tag-function+)
     ;; Plain function, use the fast path.
     (setf (%object-ref-t fref +fref-function+) value)
     (%activate-function-reference-fast-path
      fref (%object-ref-unsigned-byte-64 value +function-entry-point+)))
    (t
     ;; Bound to an unusual function. Full path.
     (setf (%object-ref-t fref +fref-function+) value)
     (%activate-function-reference-full-path fref)))
  value)

(defun trace-wrapper-p (object)
  (and (funcallable-instance-p object) ; Avoid typep in the usual case.
       (locally
           (declare (notinline find-class)) ; bootstrap hack.
         ;; The trace-wrapper class might not be defined during early boot
         (let ((class (find-class 'trace-wrapper nil)))
           (and class
                (typep object class))))))

(defun fdefinition (name)
  (let* ((fref (function-reference name nil))
         (fn (and fref (function-reference-function fref))))
    (when (not fn)
      (error 'undefined-function :name name))
    ;; Hide trace wrappers. Makes defining methods on traced generic functions work.
    (when (trace-wrapper-p fn)
      (setf fn (trace-wrapper-original fn)))
    fn))

(defun (setf fdefinition) (value name)
  (check-type value function)
  ;; Check for and update any existing TRACE-WRAPPER.
  ;; This is not very thread-safe, but if the user is tracing it shouldn't matter much.
  (when (symbolp name)
    (remhash name *macros*))
  (let* ((fref (function-reference name))
         (existing (function-reference-function fref)))
    (when (trace-wrapper-p existing)
      ;; Update the traced function instead of setting the fref's function.
      (setf (trace-wrapper-original existing) value)
      (return-from fdefinition value))
    (setf (function-reference-function fref) value)))

(defun fboundp (name)
  ;; Avoid allocating a fref just for the test.
  (let ((fref (function-reference name nil)))
    (and fref
         (not (null (function-reference-function fref))))))

(defun fmakunbound (name)
  (when (symbolp name)
    (remhash name *macros*))
  (let ((fref (function-reference name nil)))
    ;; Don't allocate a new fref if the function is already unbound.
    (when fref
      ;; Check for and update any existing TRACE-WRAPPER.
      ;; This is not very thread-safe, but if the user is tracing it shouldn't matter much.
      (let ((existing (function-reference-function fref)))
        (when (trace-wrapper-p existing)
          ;; Untrace the function.
          (%untrace (function-reference-name fref)))
        (setf (function-reference-function (function-reference name)) nil))))
  name)

(defun symbol-function (symbol)
  (check-type symbol symbol)
  (fdefinition symbol))

(defun (setf symbol-function) (value symbol)
  (check-type symbol symbol)
  (setf (fdefinition symbol) value))

(defun gensym-1 (prefix number)
  (let ((name (make-array (length prefix)
                          :element-type 'character
                          :initial-contents prefix
                          :adjustable t
                          :fill-pointer t)))
    ;; Open-code integer to string conversion.
    (labels ((frob (value)
               (multiple-value-bind (quot rem)
                   (truncate value 10)
                 (when (not (zerop quot))
                   (frob quot))
                 (vector-push-extend (code-char (+ 48 rem))
                                     name))))
      (if (zerop number)
          (vector-push-extend #\0 name)
          (frob number)))
    (make-symbol name)))

(defvar *gensym-counter* 0)
(defun gensym (&optional (thing "G"))
  (etypecase thing
    ((integer 0)
     (gensym-1 "G" thing))
    (string
     (prog1
         (gensym-1 thing *gensym-counter*)
       (incf *gensym-counter*)))))

;;; TODO: Expand this so it knows about the compiler's constant folders.
(defun constantp (form &optional environment)
  (declare (ignore environment))
  (typecase form
    (symbol (eql (symbol-mode form) :constant))
    (cons (eql (first form) 'quote))
    (t t)))

(defun get-structure-type (name &optional (errorp t))
  (cond ((typep name 'structure-class)
         name)
        (t
         (let ((class (find-class name nil)))
           (cond ((typep class 'structure-class)
                  class)
                 (errorp
                  (error "Unknown structure type ~S." name)))))))

(defun concat-symbols (&rest symbols)
  (intern (apply 'concatenate 'string (mapcar 'string symbols))))

(defvar *gentemp-counter* 0)

(defun gentemp (&optional (prefix "T") (package *package*))
  (check-type prefix string)
  (do () (nil)
    (let ((name (with-output-to-string (s)
                  (write-string prefix s)
                  (write (incf *gentemp-counter*) :stream s :base 10))))
      (multiple-value-bind (x status)
          (find-symbol name package)
        (declare (ignore x))
        (unless status
          (return (intern name package)))))))

(defun special-operator-p (symbol)
  (check-type symbol symbol)
  (member symbol '(block catch eval-when flet function go if labels
                   let let* load-time-value locally macrolet
                   multiple-value-call multiple-value-prog1
                   progn progv quote return-from setq symbol-macrolet
                   tagbody the throw unwind-protect)))

(defmacro define-lap-function (name (&optional lambda-list frame-layout environment-vector-offset environment-vector-layout) &body body)
  (let ((docstring nil))
    (when (stringp (first body))
      (setf docstring (pop body)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (%compiler-defun ',name nil))
       ;; This isn't great, it invokes the assembler at load to build the function.
       ;; But it's OK, compile-file special-cases define-lap-function and produces
       ;; a properly compiled function from it.
       (%defun ',name (assemble-lap ',body ',name
                                    ',(list :debug-info
                                            name
                                            frame-layout
                                            environment-vector-offset
                                            environment-vector-layout
                                            (when *compile-file-pathname*
                                              (princ-to-string *compile-file-pathname*))
                                            sys.int::*top-level-form-number*
                                            lambda-list
                                            docstring
                                            nil)
                                    nil
                                    #+x86-64 :x86-64))
       ',name)))

(macrolet ((def (op)
             `(setf (fdefinition ',op)
                    (lambda (&rest args)
                      (declare (ignore args)
                               (lambda-name (special-operator ,op)))
                      (error 'undefined-function :name ',op))))
           (all (&rest ops)
             `(progn
                ,@(loop
                     for op in ops
                     collect `(def ,op)))))
  (all block catch eval-when flet function go if labels
       let let* load-time-value locally macrolet
       multiple-value-call multiple-value-prog1
       progn progv quote return-from setq symbol-macrolet
       tagbody the throw unwind-protect))

(defun object-allocation-area (object)
  (cond ((or (%value-has-tag-p object +tag-cons+)
             (%value-has-tag-p object +tag-object+))
         (case (ldb (byte +address-tag-size+ +address-tag-shift+)
                    (lisp-object-address object))
           (#.+address-tag-pinned+
            (if (< (lisp-object-address object) #x80000000)
                :wired
                :pinned))
           (#.+address-tag-stack+
            :dynamic-extent)
           (t nil)))
        (t
         :immediate)))

(defun read-lookahead-substitute (value proxy)
  "Traverse VALUE and subobjects, substituting uses of PROXY with VALUE."
  (let ((seen-objects (make-hash-table)))
    (labels ((frob (object)
               ;; Avoid walking into objects that might pull in the entire heap.
               (when (and (not (symbolp object))
                          (not (function-reference-p object))
                          (not (mezzano.runtime::symbol-value-cell-p object))
                          (not (packagep object))
                          (not (layout-p object)))
                 (walk-object-references
                  object
                  (lambda (container inner-object index)
                    (when (eql inner-object proxy)
                      (cond ((eql index :car)
                             (setf (car container) value))
                            ((eql index :cdr)
                             (setf (cdr container) value))
                            (t
                             (setf (%object-ref-t container index) value))))
                    (when (not (gethash inner-object seen-objects))
                      (setf (gethash inner-object seen-objects) t)
                      (frob inner-object)))))))
      (frob value))))
