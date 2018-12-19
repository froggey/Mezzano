;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(setf sys.lap:*function-reference-resolver* #'function-reference)

(defun inline-info-location-for-name (name)
  (if (symbolp name)
      (values name 'inline-mode 'inline-form)
      (case (first name)
        ((setf)
         (values (second name) 'setf-inline-mode 'setf-inline-form))
        ((cas)
         (values (second name) 'cas-inline-mode 'cas-inline-form)))))

(defun proclaim-symbol-mode (symbol new-mode)
  (check-type symbol symbol)
  (unless (or (null (symbol-mode symbol))
              (eql (symbol-mode symbol) new-mode))
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
       (multiple-value-bind (sym indicator)
           (inline-info-location-for-name name)
         (setf (get sym indicator) t))))
    (notinline
     (dolist (name (rest declaration-specifier))
       (multiple-value-bind (sym indicator)
           (inline-info-location-for-name name)
         (setf (get sym indicator) nil))))
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
         (setf (getf sys.c::*optimize-policy* quality) value))))
    (t
     (cond ((or (get (first declaration-specifier) 'type-expander)
                (get (first declaration-specifier) 'compound-type)
                (get (first declaration-specifier) 'type-symbol))
            ;; Actually a type declaration.
            (proclaim-type (first declaration-specifier)
                           (rest declaration-specifier)))
           ((not (find (first declaration-specifier) *known-declarations*))
            (warn "Unknown declaration ~S" declaration-specifier))))))

(defun variable-information (symbol)
  (symbol-mode symbol))

(defun sys.c::function-inline-info (name)
  (multiple-value-bind (sym mode-name form-name)
      (inline-info-location-for-name name)
    (values (get sym mode-name)
            (get sym form-name))))

;;; Turn (APPLY fn args...) into (%APPLY fn (list* args...)), bypassing APPLY's
;;; rest-list generation.
(define-compiler-macro apply (&whole whole function arg &rest more-args)
  (if more-args
      (let ((function-sym (gensym))
            (args-sym (gensym)))
        `(let ((,function-sym ,function)
               (,args-sym (list* ,arg ,@more-args)))
           (declare (dynamic-extent ,args-sym))
           (mezzano.runtime::%apply (%coerce-to-callable ,function-sym) ,args-sym)))
      `(mezzano.runtime::%apply (%coerce-to-callable ,function) ,arg)))

(defun apply (function arg &rest more-args)
  (declare (dynamic-extent more-args))
  (check-type function (or function symbol) "a function-designator")
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

(defun constantly (value)
  (lambda (&rest arguments)
    (declare (ignore arguments))
    value))

(defun macro-function (symbol &optional env)
  (cond (env
         (sys.c::macro-function-in-environment symbol env))
        (t
         (get symbol '%macro-function))))

(defun (setf macro-function) (value symbol &optional env)
  (when env
    (error "TODO: (Setf Macro-function) in environment."))
  (setf (symbol-function symbol) (lambda (&rest r)
                                   (declare (ignore r))
                                   (error 'undefined-function :name symbol))
        (get symbol '%macro-function) value))

(defun compiler-macro-function (name &optional environment)
  (cond (environment
         (sys.c::compiler-macro-function-in-environment name environment))
        (t
         (multiple-value-bind (sym indicator)
             (if (symbolp name)
                 (values name '%compiler-macro-function)
                 (values (second name) '%setf-compiler-macro-function))
           (get sym indicator)))))

(defun (setf compiler-macro-function) (value name &optional environment)
  (when environment
    (error "TODO: (Setf Compiler-Macro-function) in environment."))
  (multiple-value-bind (sym indicator)
      (if (symbolp name)
          (values name '%compiler-macro-function)
          (values (second name) '%setf-compiler-macro-function))
    (setf (get sym indicator) value)))

(defun list (&rest args)
  args)

(defun copy-list-in-area (list &optional area)
  (check-type list list)
  (cond (list
         (do* ((result (cons-in-area nil nil area))
               (tail result)
               (l list (cdr l)))
              ((not (consp l))
               (setf (cdr tail) l)
               (cdr result))
           (setf (cdr tail) (cons-in-area (car l) nil area)
                 tail (cdr tail))))
        (t
         nil)))

(defun copy-list (list)
  (copy-list-in-area list))

;;; Will be overriden later in the init process.
(unless (fboundp 'funcallable-instance-lambda-expression)
  (defun funcallable-instance-lambda-expression (function)
    (values nil t nil))
  (defun funcallable-instance-debug-info (function)
    nil)
  (defun funcallable-instance-compiled-function-p (function)
    nil)
  )

;;; Implementations of DEFUN/etc, the cross-compiler defines these as well.

(defun %defmacro (name function &optional lambda-list)
  (setf (get name 'macro-lambda-list) lambda-list)
  (setf (macro-function name) function)
  name)

(defun %define-compiler-macro (name function)
  (setf (compiler-macro-function name) function)
  name)

(defun %compiler-defun (name source-lambda)
  "Compile-time defun code. Store the inline form if required."
  (multiple-value-bind (sym mode-name form-name)
      (inline-info-location-for-name name)
    (when (or (get sym mode-name)
              (get sym form-name))
      (setf (get sym form-name) source-lambda)))
  nil)

(defun %defun (name lambda)
  (setf (fdefinition name) lambda)
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
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::documentation)
                       nil)
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
                 (setf (mezzano.runtime::instance-access-by-name new 'mezzano.clos::documentation)
                       nil)
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
          parent-class)))

(defun convert-structure-definition-to-class (sdef)
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
          '())
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
    (push new-class (mezzano.runtime::instance-access-by-name parent-class 'mezzano.clos::direct-subclasses))
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

(defun %defstruct (structure-type)
  (when (mezzano.runtime::structure-class-p structure-type)
    ;; Happens during cold load.
    (return-from %defstruct structure-type))
  (let* ((name (structure-definition-name structure-type))
         (existing (find-class name nil)))
    (cond (existing
           (unless (structure-definition-trivially-compatible-p existing structure-type)
             (mezzano.clos::redefine-structure-type existing structure-type))
           existing)
          (t
           (setf (find-class name) (convert-structure-definition-to-class structure-type))))))

(defparameter *incompatible-constant-redefinition-is-an-error* nil)
(defparameter *defconstant-redefinition-comparator* 'eql)

(defun %defconstant (name value &optional docstring)
  (cond ((boundp name)
         (let ((old-value (symbol-value name)))
           (unless (funcall (or (and (boundp '*defconstant-redefinition-comparator*)
                                        *defconstant-redefinition-comparator*)
                                   #'eql)
                               old-value value)
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
  name)

;;; Function references, FUNCTION, et al.

(deftype function-name ()
  '(or
    symbol
    (cons (member setf cas) (cons symbol null))))

(defglobal *setf-fref-table*)
(defglobal *cas-fref-table*)

(defun make-function-reference (name)
  (let ((fref (mezzano.runtime::%allocate-object +object-tag-function-reference+ 0 3 :wired)))
    (setf (%object-ref-t fref +fref-name+) name
          (function-reference-function fref) nil)
    fref))

(defun decode-function-name (name)
  (cond ((symbolp name)
         (values name 'symbol))
        ((and (consp name)
              (consp (rest name))
              (null (rest (rest name)))
              (member (first name) '(setf cas))
              (symbolp (second name)))
         (values (second name) (first name)))
        (t
         (error 'type-error
                  :expected-type 'function-name
                  :datum name))))

(defun function-reference (name)
  "Convert a function name to a function reference."
  (multiple-value-bind (name-root location)
      (decode-function-name name)
    (ecase location
      (symbol
       (or (%object-ref-t name-root +symbol-function+)
           ;; No fref, create one and add it to the function.
           (let ((new-fref (make-function-reference name-root)))
             ;; Try to atomically update the function cell.
             (multiple-value-bind (successp old-value)
                 (%cas-object name-root +symbol-function+ nil new-fref)
               (if successp
                   new-fref
                   old-value)))))
      (setf
       (let ((fref (gethash name-root *setf-fref-table*)))
         (unless fref
           (let ((new-fref (make-function-reference name)))
             (setf fref (or (cas (gethash name-root *setf-fref-table*) nil new-fref)
                            new-fref))))
         fref))
      (cas
       (let ((fref (gethash name-root *cas-fref-table*)))
         (unless fref
           (let ((new-fref (make-function-reference name)))
             (setf fref (or (cas (gethash name-root *cas-fref-table*) nil new-fref)
                            new-fref))))
         fref)))))

(defun function-reference-p (object)
  (%object-of-type-p object +object-tag-function-reference+))

(deftype function-reference ()
  '(satisfies function-reference-p))

(defun function-reference-name (fref)
  (check-type fref function-reference)
  (%object-ref-t fref +fref-name+))

(defun function-reference-function (fref)
  (check-type fref function-reference)
  (let ((fn (%object-ref-t fref +fref-function+)))
    (if (%undefined-function-p fn)
        nil
        fn)))

(defun (setf function-reference-function) (value fref)
  "Update the function & entry-point fields of a function-reference.
VALUE may be nil to make the fref unbound."
  (check-type value (or function null))
  (check-type fref function-reference)
  (multiple-value-bind (new-fn new-entry-point)
      (cond
        ((not value)
         ;; Use the undefined function trampoline.
         ;; This must be stored in function slot so the closure-trampoline
         ;; works correctly.
         (values (%undefined-function)
                 (%object-ref-t (%undefined-function)
                                +function-entry-point+)))
        ((eql (%object-tag value) +object-tag-function+)
         ;; Normal call.
         (values value
                 (%object-ref-t value
                                +function-entry-point+)))
        (t ;; Something else, either a closure or funcallable-instance. Use the closure trampoline.
         (values value
                 (%object-ref-t (%closure-trampoline)
                                +function-entry-point+))))
    ;; Atomically update both values.
    ;; Functions is followed by entry point.
    ;; A 128-byte store would work instead of a CAS, but it needs to be atomic.
    (let ((old-1 (%object-ref-t fref +fref-function+))
          (old-2 (%object-ref-t fref +fref-entry-point+)))
      ;; Don't bother CASing in a loop. If another CPU beats us, then it as if
      ;; this write succeeded, but was immediately overwritten.
      (%dcas-object fref +fref-function+
                    old-1 old-2
                    new-fn new-entry-point)))
  value)

(defun fdefinition (name)
  (let ((fn (function-reference-function (function-reference name))))
    (unless fn
      (error 'undefined-function :name name))
    ;; Hide trace wrappers. Makes defining methods on traced generic functions work.
    ;; FIXME: Doesn't match the behaviour of FUNCTION.
    (when (and (funcallable-instance-p fn) ; Avoid typep in the usual case.
               (locally
                   (declare (notinline typep)) ; bootstrap hack.
                 (typep fn 'trace-wrapper)))
      (setf fn (trace-wrapper-original fn)))
    fn))

(defun (setf fdefinition) (value name)
  (check-type value function)
  ;; Check for and update any existing TRACE-WRAPPER.
  ;; This is not very thread-safe, but if the user is tracing it shouldn't matter much.
  (let* ((fref (function-reference name))
         (existing (function-reference-function fref)))
    (when (locally
              (declare (notinline typep)) ; bootstrap hack.
            (typep existing 'trace-wrapper))
      ;; Update the traced function instead of setting the fref's function.
      (setf (trace-wrapper-original existing) value)
      (return-from fdefinition value))
    (setf (function-reference-function fref) value)))

(defun fboundp (name)
  (not (null (function-reference-function (function-reference name)))))

(defun fmakunbound (name)
  ;; Check for and update any existing TRACE-WRAPPER.
  ;; This is not very thread-safe, but if the user is tracing it shouldn't matter much.
  (let* ((fref (function-reference name))
         (existing (function-reference-function fref)))
    (when (locally
              (declare (notinline typep)) ; bootstrap hack.
            (typep existing 'trace-wrapper))
      ;; Untrace the function.
      (%untrace (function-reference-name fref)))
    (setf (function-reference-function (function-reference name)) nil)
    name))

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
                 (unless (zerop quot)
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
