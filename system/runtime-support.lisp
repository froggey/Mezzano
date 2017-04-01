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

;;; Turn (APPLY fn arg) into (%APPLY fn arg), bypassing APPLY's
;;; rest-list generation in the single arg case.
(define-compiler-macro apply (&whole whole function arg &rest more-args)
  (if more-args
      `(mezzano.runtime::%apply (%coerce-to-callable ,function)
                                (list* ,arg ,@more-args))
      `(mezzano.runtime::%apply (%coerce-to-callable ,function) ,arg)))

(defun apply (function arg &rest more-args)
  (declare (dynamic-extent more-args))
  (check-type function (or function symbol) "a function-designator")
  (when (symbolp function)
    (setf function (symbol-function function)))
  (cond (more-args
         ;; Convert (... (final-list ...)) to (... final-list...)
         (do* ((arg-list (cons arg more-args))
               (i arg-list (cdr i)))
              ((null (cddr i))
               (setf (cdr i) (cadr i))
               (mezzano.runtime::%apply function arg-list))))
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

(define-compiler-macro list (&rest args)
  (cond
    ((null args) 'nil)
    (t `(cons ,(first args) (list ,@(rest args))))))

(defun list (&rest args)
  args)

(defun copy-list-in-area (list &optional area)
  (check-type list list)
  (do* ((result (cons nil nil))
        (tail result)
        (l list (cdr l)))
       ((not (consp l))
        (setf (cdr tail) l)
        (cdr result))
    (setf (cdr tail) (cons-in-area (car l) nil area)
          tail (cdr tail))))

(defun copy-list (list)
  (copy-list-in-area list))

;;; Will be overriden later in the init process.
(when (not (fboundp 'funcallable-instance-lambda-expression))
  (defun funcallable-instance-lambda-expression (function)
    (values nil t nil))
  (defun funcallable-instance-debug-info (function)
    nil)
  (defun funcallable-instance-compiled-function-p (function)
    t)
  )

(defun closure-p (object)
  (%object-of-type-p object +object-tag-closure+))

(defun %closure-function (closure)
  (assert (%object-of-type-p closure +object-tag-closure+))
  ;; Return the closed-over function associated with CLOSURE.
  (%object-ref-t closure +closure-function+))

(defun %closure-length (closure)
  (assert (%object-of-type-p closure +object-tag-closure+))
  (- (%object-header-data closure) 2))

(defun %closure-value (closure index)
  (assert (%object-of-type-p closure +object-tag-closure+))
  (assert (<= 0 index))
  (assert (< index (%closure-length closure)))
  (%object-ref-t closure (+ 1 +closure-function+ index)))

(defun (setf %closure-value) (value closure index)
  (assert (%object-of-type-p closure +object-tag-closure+))
  (assert (<= 0 index))
  (assert (< index (%closure-length closure)))
  (setf (%object-ref-t closure (+ 1 +closure-function+ index)) value))

(defun function-name (function)
  (check-type function function)
  (ecase (%object-tag function)
    (#.+object-tag-function+
     ;; Regular function. First entry in the constant pool.
     (function-pool-object function 0))
    (#.+object-tag-closure+
     ;; Closure. Return the name of the closed-over function.
     (function-name (%closure-function function)))
    (#.+object-tag-funcallable-instance+
     (multiple-value-bind (lambda closurep name)
         (funcallable-instance-lambda-expression function)
       (declare (ignore lambda closurep))
       name))))

(defun function-lambda-expression (function)
  (check-type function function)
  (ecase (%object-tag function)
    (#.+object-tag-function+
     (values nil nil (function-name function)))
    (#.+object-tag-closure+
     (values nil t (function-name function)))
    (#.+object-tag-funcallable-instance+
     (funcallable-instance-lambda-expression function))))

(defun function-debug-info (function)
  (check-type function function)
  (ecase (%object-tag function)
    (#.+object-tag-function+
     ;; Regular function. Second entry in the constant pool.
     (function-pool-object function 1))
    (#.+object-tag-closure+
     ;; Closure. Return the debug-info of the closed-over function.
     (function-debug-info (%closure-function function)))
    (#.+object-tag-funcallable-instance+
     (funcallable-instance-debug-info function))))

(declaim (inline funcallable-std-instance-p
                 funcallable-std-instance-class (setf funcallable-std-instance-class)
                 funcallable-std-instance-slots (setf funcallable-std-instance-slots)
                 funcallable-std-instance-layout (setf funcallable-std-instance-layout)))

(defun funcallable-std-instance-p (object)
  (%object-of-type-p object +object-tag-funcallable-instance+))

(defun funcallable-std-instance-function (funcallable-instance)
  (%type-check funcallable-instance +object-tag-funcallable-instance+ 'std-instance)
  (%object-ref-t funcallable-instance +funcallable-instance-function+))
(defun (setf funcallable-std-instance-function) (value funcallable-instance)
  (check-type value function)
  (%type-check funcallable-instance +object-tag-funcallable-instance+ 'std-instance)
  ;; TODO: If the function is an +OBJECT-TAG-FUNCTION+, then the entry point could point directly at it.
  ;; Same as in ALLOCATE-FUNCALLABLE-STD-INSTANCE.
  (setf (%object-ref-t funcallable-instance +funcallable-instance-function+) value))

(defun funcallable-std-instance-class (funcallable-instance)
  (%type-check funcallable-instance +object-tag-funcallable-instance+ 'std-instance)
  (%object-ref-t funcallable-instance +funcallable-instance-class+))
(defun (setf funcallable-std-instance-class) (value funcallable-instance)
  (%type-check funcallable-instance +object-tag-funcallable-instance+ 'std-instance)
  (setf (%object-ref-t funcallable-instance +funcallable-instance-class+) value))

(defun funcallable-std-instance-slots (funcallable-instance)
  (%type-check funcallable-instance +object-tag-funcallable-instance+ 'std-instance)
  (%object-ref-t funcallable-instance +funcallable-instance-slots+))
(defun (setf funcallable-std-instance-slots) (value funcallable-instance)
  (%type-check funcallable-instance +object-tag-funcallable-instance+ 'std-instance)
  (setf (%object-ref-t funcallable-instance +funcallable-instance-slots+) value))

(defun funcallable-std-instance-layout (funcallable-instance)
  (%type-check funcallable-instance +object-tag-funcallable-instance+ 'std-instance)
  (%object-ref-t funcallable-instance +funcallable-instance-layout+))
(defun (setf funcallable-std-instance-layout) (value funcallable-instance)
  (%type-check funcallable-instance +object-tag-funcallable-instance+ 'std-instance)
  (setf (%object-ref-t funcallable-instance +funcallable-instance-layout+) value))

(defun compiled-function-p (object)
  (when (functionp object)
    (ecase (%object-tag object)
      ((#.+object-tag-function+
        #.+object-tag-closure+)
       t)
      (#.+object-tag-funcallable-instance+
       (funcallable-instance-compiled-function-p object)))))

(deftype compiled-function ()
  '(satisfies compiled-function-p))

;;; Implementations of DEFUN/etc, the cross-compiler defines these as well.

(defun %defmacro (name function &optional lambda-list)
  (setf (get name 'macro-lambda-list) lambda-list)
  (setf (macro-function name) function)
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

(defun %defstruct (structure-type)
  (setf (get (structure-name structure-type) 'structure-type) structure-type))

(defparameter *incompatible-constant-redefinition-is-an-error* nil)
(defparameter *defconstant-redefinition-comparator* 'eql)

(defun %defconstant (name value &optional docstring)
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
  name)

;;; Function references, FUNCTION, et al.

(deftype function-name ()
  '(or
    symbol
    (cons (member setf cas) (cons symbol null))))

(defun make-function-reference (name)
  (let ((fref (mezzano.runtime::%allocate-object +object-tag-function-reference+ 0 4 :wired)))
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
      ;; FIXME: lock here. It's hard to lock a plist, need to switch to
      ;; a hash-table or something like that.
      (setf
       (let ((fref (get name-root 'setf-fref)))
         (unless fref
           (setf fref (make-function-reference name)
                 (get name-root 'setf-fref) fref))
         fref))
      (cas
       (let ((fref (get name-root 'cas-fref)))
         (unless fref
           (setf fref (make-function-reference name)
                 (get name-root 'cas-fref) fref))
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
  (or (function-reference-function (function-reference name))
      (error 'undefined-function :name name)))

(defun (setf fdefinition) (value name)
  (check-type value function)
  (setf (function-reference-function (function-reference name)) value))

(defun fboundp (name)
  (not (null (function-reference-function (function-reference name)))))

(defun fmakunbound (name)
  (setf (function-reference-function (function-reference name)) nil)
  name)

(defun symbol-function (symbol)
  (check-type symbol symbol)
  (fdefinition symbol))

(defun (setf symbol-function) (value symbol)
  (check-type symbol symbol)
  (setf (fdefinition symbol) value))

(defvar *gensym-counter* 0)
(defun gensym (&optional (thing "G"))
  (check-type thing (or string (integer 0)))
  (if (integerp thing)
      (make-symbol (with-output-to-string (s)
                     (write-char #\G s)
                     (write thing :stream s :base 10)))
      (prog1 (make-symbol (with-output-to-string (s)
                            (write-string thing s)
                            (write *gensym-counter* :stream s :base 10)))
        (incf *gensym-counter*))))

;;; TODO: Expand this so it knows about the compiler's constant folders.
(defun constantp (form &optional environment)
  (declare (ignore environment))
  (typecase form
    (symbol (eql (symbol-mode form) :constant))
    (cons (eql (first form) 'quote))
    (t t)))

(defun %progv (symbols values fn)
  (cond (symbols
         ;; Bind one.
         ;; Bindings must be done one at a time because the compiler
         ;; cannot emit an arbitrary number of special stack entries in a
         ;; single function.
         ;; It'd be possible to do in assembly, but complicated enough
         ;; that it'd not be worthwhile.
         (let ((symbol (car symbols))
               (value (if values
                          (car values)
                          (%unbound-value))))
           (check-type symbol symbol)
           (%%bind symbol value)
           (multiple-value-prog1
               (%progv (cdr symbols) (cdr values) fn)
             (%%unbind))))
        (t ;; No more to bind
         (funcall fn))))

(defun function-tag (function)
  (check-type function function)
  (%object-tag function))

(defun function-pool-size (function)
  (check-type function function)
  (ldb (byte +function-constant-pool-size+
             +function-constant-pool-position+)
       (%object-header-data function)))

(defun function-code-size (function)
  (check-type function function)
  (* (ldb (byte +function-machine-code-size+
                +function-machine-code-position+)
          (%object-header-data function))
     16))

(defun function-pool-object (function offset)
  (check-type function function)
  (let ((address (logand (lisp-object-address function) -16))
        (mc-size (truncate (function-code-size function) 8))) ; in words.
    (memref-t address (+ mc-size offset))))

(defun function-code-byte (function offset)
  (check-type function function)
  (let ((address (logand (lisp-object-address function) -16)))
    (memref-unsigned-byte-8 address offset)))

(defun function-gc-info (function)
  "Return the address of and the number of bytes in FUNCTION's GC info."
  (check-type function function)
  (let* ((address (logand (lisp-object-address function) -16))
         (gc-length (ldb (byte +function-gc-metadata-size+
                               +function-gc-metadata-position+)
                         (%object-header-data function)))
         (mc-size (function-code-size function))
         (n-constants (function-pool-size function)))
    (values (+ address mc-size (* n-constants 8)) ; Address.
            gc-length))) ; Length.

(defun decode-function-gc-info (function)
  (let ((result '()))
    (map-function-gc-metadata
     (lambda (offset
              framep interruptp
              pushed-values pushed-values-register
              layout-address layout-length
              multiple-values incoming-arguments
              block-or-tagbody-thunk extra-registers
              restart)
       (let ((layout (make-array 32 :element-type 'bit :adjustable t :fill-pointer 0)))
         ;; Consume layout bits.
         (dotimes (i (ceiling layout-length 8))
           (let ((byte (memref-unsigned-byte-8 layout-address i)))
             (dotimes (j 8)
               (vector-push-extend (ldb (byte 1 j) byte) layout))))
         (setf (fill-pointer layout) layout-length)
         ;; Assemble something that looks like a LAP GC entry.
         (let ((entry '()))
           (unless (zerop layout-length)
             (setf (getf entry :layout) layout))
           (when block-or-tagbody-thunk
             (setf (getf entry :block-or-tagbody-thunk) block-or-tagbody-thunk))
           (when incoming-arguments
             (setf (getf entry :incoming-arguments) incoming-arguments))
           (when multiple-values
             (setf (getf entry :multiple-values) multiple-values))
           (when pushed-values-register
             (setf (getf entry :pushed-values-register) pushed-values-register))
           (when extra-registers
             (setf (getf entry :extra-registers) extra-registers))
           (unless (zerop pushed-values)
             (setf (getf entry :pushed-values) pushed-values))
           (when interruptp
             (setf (getf entry :interrupt) t))
           (when restart
             (setf (getf entry :restart) t))
           (push (list* offset
                        (if framep :frame :no-frame)
                        entry)
                 result))))
     function)
    (reverse result)))

(defun get-structure-type (name &optional (errorp t))
  (or (get name 'structure-type)
      (and errorp
           (error "Unknown structure type ~S." name))))

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
                                            docstring)
                                    nil
                                    #+x86-64 :x86-64))
       ',name)))

(declaim (inline std-instance-p
                 std-instance-class (setf std-instance-class)
                 std-instance-slots (setf std-instance-slots)
                 std-instance-layout (setf std-instance-layout)))

(defun std-instance-p (object)
  (%object-of-type-p object +object-tag-std-instance+))

(defun std-instance-class (std-instance)
  (%type-check std-instance +object-tag-std-instance+ 'std-instance)
  (%object-ref-t std-instance 0))
(defun (setf std-instance-class) (value std-instance)
  (%type-check std-instance +object-tag-std-instance+ 'std-instance)
  (setf (%object-ref-t std-instance 0) value))

(defun std-instance-slots (std-instance)
  (%type-check std-instance +object-tag-std-instance+ 'std-instance)
  (%object-ref-t std-instance 1))
(defun (setf std-instance-slots) (value std-instance)
  (%type-check std-instance +object-tag-std-instance+ 'std-instance)
  (setf (%object-ref-t std-instance 1) value))

(defun std-instance-layout (std-instance)
  (%type-check std-instance +object-tag-std-instance+ 'std-instance)
  (%object-ref-t std-instance 2))
(defun (setf std-instance-layout) (value std-instance)
  (%type-check std-instance +object-tag-std-instance+ 'std-instance)
  (setf (%object-ref-t std-instance 2) value))

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
