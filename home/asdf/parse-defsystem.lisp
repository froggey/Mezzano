;;;; -------------------------------------------------------------------------
;;;; Defsystem

(uiop/package:define-package :asdf/parse-defsystem
  (:recycle :asdf/parse-defsystem :asdf/defsystem :asdf)
  (:nicknames :asdf/defsystem) ;; previous name, to be compatible with, in case anyone cares
  (:use :uiop/common-lisp :asdf/driver :asdf/upgrade
   :asdf/session :asdf/component :asdf/system :asdf/system-registry
   :asdf/find-component :asdf/action :asdf/lisp-action :asdf/operate)
  (:import-from :asdf/system #:depends-on #:weakly-depends-on)
  ;; these needed for record-additional-system-input-file
  (:import-from :asdf/operation #:make-operation)
  (:import-from :asdf/component #:%additional-input-files)
  (:import-from :asdf/find-system #:define-op)
  (:export
   #:defsystem #:register-system-definition
   #:class-for-type #:*default-component-class*
   #:determine-system-directory #:parse-component-form
   #:non-toplevel-system #:non-system-system #:bad-system-name
   #:*known-systems-with-bad-secondary-system-names*
   #:known-system-with-bad-secondary-system-names-p
   #:sysdef-error-component #:check-component-input
   #:explain))
(in-package :asdf/parse-defsystem)

;;; Pathname
(with-upgradability ()
  (defun determine-system-directory (pathname)
    ;; The defsystem macro calls this function to determine the pathname of a system as follows:
    ;; 1. If the pathname argument is an pathname object (NOT a namestring),
    ;;    that is already an absolute pathname, return it.
    ;; 2. Otherwise, the directory containing the LOAD-PATHNAME
    ;;    is considered (as deduced from e.g. *LOAD-PATHNAME*), and
    ;;    if it is indeed available and an absolute pathname, then
    ;;    the PATHNAME argument is normalized to a relative pathname
    ;;    as per PARSE-UNIX-NAMESTRING (with ENSURE-DIRECTORY T)
    ;;    and merged into that DIRECTORY as per SUBPATHNAME.
    ;;    Note: avoid *COMPILE-FILE-PATHNAME* because the .asd is loaded as source,
    ;;    but may be from within the EVAL-WHEN of a file compilation.
    ;; If no absolute pathname was found, we return NIL.
    (check-type pathname (or null string pathname))
    (pathname-directory-pathname
     (resolve-symlinks*
      (ensure-absolute-pathname
       (parse-unix-namestring pathname :type :directory)
       #'(lambda () (ensure-absolute-pathname
                     (load-pathname) 'get-pathname-defaults nil))
       nil)))))


;;; Component class
(with-upgradability ()
  ;; What :file gets interpreted as, unless overridden by a :default-component-class
  (defvar *default-component-class* 'cl-source-file)

  (defun class-for-type (parent type)
      (or (coerce-class type :package :asdf/interface :super 'component :error nil)
          (and (eq type :file)
               (coerce-class
                (or (loop :for p = parent :then (component-parent p) :while p
                      :thereis (module-default-component-class p))
                    *default-component-class*)
                :package :asdf/interface :super 'component :error nil))
          (sysdef-error "don't recognize component type ~S" type))))


;;; Check inputs
(with-upgradability ()
  (define-condition non-system-system (system-definition-error)
    ((name :initarg :name :reader non-system-system-name)
     (class-name :initarg :class-name :reader non-system-system-class-name))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while defining system ~S: class ~S isn't a subclass of ~S~@:>")
                       (non-system-system-name c) (non-system-system-class-name c) 'system))))

  (define-condition non-toplevel-system (system-definition-error)
    ((parent :initarg :parent :reader non-toplevel-system-parent)
     (name :initarg :name :reader non-toplevel-system-name))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while defining system: component ~S claims to have a system ~S as a child~@:>")
                       (non-toplevel-system-parent c) (non-toplevel-system-name c)))))

  (define-condition bad-system-name (warning)
    ((name :initarg :name :reader component-name)
     (source-file :initarg :source-file :reader system-source-file))
    (:report (lambda (c s)
               (let* ((file (system-source-file c))
                      (name (component-name c))
                      (asd (pathname-name file)))
                 (format s (compatfmt "~@<System definition file ~S contains definition for system ~S. ~
Please only define ~S and secondary systems with a name starting with ~S (e.g. ~S) in that file.~@:>")
                       file name asd (strcat asd "/") (strcat asd "/test"))))))

  (defun sysdef-error-component (msg type name value)
    (sysdef-error (strcat msg (compatfmt "~&~@<The value specified for ~(~A~) ~A is ~S~@:>"))
                  type name value))

  (defun check-component-input (type name weakly-depends-on
                                depends-on components)
    "A partial test of the values of a component."
    (unless (listp depends-on)
      (sysdef-error-component ":depends-on must be a list."
                              type name depends-on))
    (unless (listp weakly-depends-on)
      (sysdef-error-component ":weakly-depends-on must be a list."
                              type name weakly-depends-on))
    (unless (listp components)
      (sysdef-error-component ":components must be NIL or a list of components."
                              type name components)))


  (defun record-additional-system-input-file (pathname component parent)
    (let* ((record-on (if parent
                          (loop :with retval
                                :for par = parent :then (component-parent par)
                                :while par
                                :do (setf retval par)
                                :finally (return retval))
                          component))
           (comp (if (typep record-on 'component)
                     record-on
                     ;; at this point there will be no parent for RECORD-ON
                     (find-component record-on nil)))
           (op (make-operation 'define-op))
           (cell (or (assoc op (%additional-input-files comp))
                       (let ((new-cell (list op)))
                         (push new-cell (%additional-input-files comp))
                         new-cell))))
      (pushnew pathname (cdr cell) :test 'pathname-equal)
      (values)))

  ;; Given a form used as :version specification, in the context of a system definition
  ;; in a file at PATHNAME, for given COMPONENT with given PARENT, normalize the form
  ;; to an acceptable ASDF-format version.
  (defun* (normalize-version) (form &key pathname component parent)
    (labels ((invalid (&optional (continuation "using NIL instead"))
               (warn (compatfmt "~@<Invalid :version specifier ~S~@[ for component ~S~]~@[ in ~S~]~@[ from file ~S~]~@[, ~A~]~@:>")
                     form component parent pathname continuation))
             (invalid-parse (control &rest args)
               (unless (if-let (target (find-component parent component)) (builtin-system-p target))
                 (apply 'warn control args)
                 (invalid))))
      (if-let (v (typecase form
                   ((or string null) form)
                   (real
                    (invalid "Substituting a string")
                    (format nil "~D" form)) ;; 1.0 becomes "1.0"
                   (cons
                    (case (first form)
                      ((:read-file-form)
                       (destructuring-bind (subpath &key (at 0)) (rest form)
                         (let ((path (subpathname pathname subpath)))
                           (record-additional-system-input-file path component parent)
                           (safe-read-file-form path
                                                :at at :package :asdf-user))))
                      ((:read-file-line)
                       (destructuring-bind (subpath &key (at 0)) (rest form)
                         (let ((path (subpathname pathname subpath)))
                           (record-additional-system-input-file path component parent)
                           (safe-read-file-line (subpathname pathname subpath)
                                                :at at))))
                      (otherwise
                       (invalid))))
                   (t
                    (invalid))))
        (if-let (pv (parse-version v #'invalid-parse))
          (unparse-version pv)
          (invalid))))))


;;; "inline methods"
(with-upgradability ()
  (defparameter* +asdf-methods+
      '(perform-with-restarts perform explain output-files operation-done-p))

  (defun %remove-component-inline-methods (component)
    (dolist (name +asdf-methods+)
      (map ()
           ;; this is inefficient as most of the stored
           ;; methods will not be for this particular gf
           ;; But this is hardly performance-critical
           #'(lambda (m)
               (remove-method (symbol-function name) m))
           (component-inline-methods component)))
    (component-inline-methods component) nil)

  (defparameter *standard-method-combination-qualifiers*
    '(:around :before :after))

;;; Find inline method definitions of the form
;;;
;;;   :perform (test-op :before (operation component) ...)
;;;
;;; in REST (which is the plist of all DEFSYSTEM initargs) and define the specified methods.
  (defun %define-component-inline-methods (ret rest)
    ;; find key-value pairs that look like inline method definitions in REST. For each identified
    ;; definition, parse it and, if it is well-formed, define the method.
    (loop* :for (key value) :on rest :by #'cddr
           :for name = (and (keywordp key) (find key +asdf-methods+ :test 'string=))
           :when name :do
           ;; parse VALUE as an inline method definition of the form
           ;;
           ;;   (OPERATION-NAME [QUALIFIER] (OPERATION-PARAMETER COMPONENT-PARAMETER) &rest BODY)
           (destructuring-bind (operation-name &rest rest) value
             (let ((qualifiers '()))
               ;; ensure that OPERATION-NAME is a symbol.
               (unless (and (symbolp operation-name) (not (null operation-name)))
                 (sysdef-error "Ill-formed inline method: ~S. The first element is not a symbol ~
                              designating an operation but ~S."
                               value operation-name))
               ;; ensure that REST starts with either a cons (potential lambda list, further checked
               ;; below) or a qualifier accepted by the standard method combination. Everything else
               ;; is ill-formed. In case of a valid qualifier, pop it from REST so REST now definitely
               ;; has to start with the lambda list.
               (cond
                 ((consp (car rest)))
                 ((not (member (car rest)
                               *standard-method-combination-qualifiers*))
                  (sysdef-error "Ill-formed inline method: ~S. Only a single of the standard ~
                               qualifiers ~{~S~^ ~} is allowed, not ~S."
                                value *standard-method-combination-qualifiers* (car rest)))
                 (t
                  (setf qualifiers (list (pop rest)))))
               ;; REST must start with a two-element lambda list.
               (unless (and (listp (car rest))
                            (length=n-p (car rest) 2)
                            (null (cddar rest)))
                 (sysdef-error "Ill-formed inline method: ~S. The operation name ~S is not followed by ~
                              a lambda-list of the form (OPERATION COMPONENT) and a method body."
                               value operation-name))
               ;; define the method.
               (destructuring-bind ((o c) &rest body) rest
                 (pushnew
                  (eval `(defmethod ,name ,@qualifiers ((,o ,operation-name) (,c (eql ,ret))) ,@body))
                  (component-inline-methods ret)))))))

  (defun %refresh-component-inline-methods (component rest)
    ;; clear methods, then add the new ones
    (%remove-component-inline-methods component)
    (%define-component-inline-methods component rest)))


;;; Main parsing function
(with-upgradability ()
  (defun parse-dependency-def (dd)
    (if (listp dd)
        (case (first dd)
          (:feature
           (unless (= (length dd) 3)
             (sysdef-error "Ill-formed feature dependency: ~s" dd))
           (let ((embedded (parse-dependency-def (third dd))))
             `(:feature ,(second dd) ,embedded)))
          (feature
           (sysdef-error "`feature' has been removed from the dependency spec language of ASDF. Use :feature instead in ~s." dd))
          (:require
           (unless (= (length dd) 2)
             (sysdef-error "Ill-formed require dependency: ~s" dd))
           dd)
          (:version
           (unless (= (length dd) 3)
             (sysdef-error "Ill-formed version dependency: ~s" dd))
           `(:version ,(coerce-name (second dd)) ,(third dd)))
          (otherwise (sysdef-error "Ill-formed dependency: ~s" dd)))
      (coerce-name dd)))

  (defun parse-dependency-defs (dd-list)
    "Parse the dependency defs in DD-LIST into canonical form by translating all
system names contained using COERCE-NAME. Return the result."
    (mapcar 'parse-dependency-def dd-list))

  (defun* (parse-component-form) (parent options &key previous-serial-component)
    (destructuring-bind
        (type name &rest rest &key
                                (builtin-system-p () bspp)
                                ;; the following list of keywords is reproduced below in the
                                ;; remove-plist-keys form.  important to keep them in sync
                                components pathname perform explain output-files operation-done-p
                                weakly-depends-on depends-on serial
                                do-first if-component-dep-fails version
                                ;; list ends
         &allow-other-keys) options
      (declare (ignore perform explain output-files operation-done-p builtin-system-p))
      (check-component-input type name weakly-depends-on depends-on components)
      (when (and parent
                 (find-component parent name)
                 (not ;; ignore the same object when rereading the defsystem
                  (typep (find-component parent name)
                         (class-for-type parent type))))
        (error 'duplicate-names :name name))
      (when do-first (error "DO-FIRST is not supported anymore as of ASDF 3"))
      (let* ((name (coerce-name name))
             (args `(:name ,name
                     :pathname ,pathname
                     ,@(when parent `(:parent ,parent))
                     ,@(remove-plist-keys
                        '(:components :pathname :if-component-dep-fails :version
                          :perform :explain :output-files :operation-done-p
                          :weakly-depends-on :depends-on :serial)
                        rest)))
             (component (find-component parent name))
             (class (class-for-type parent type)))
        (when (and parent (subtypep class 'system))
          (error 'non-toplevel-system :parent parent :name name))
        (if component ; preserve identity
            (apply 'reinitialize-instance component args)
            (setf component (apply 'make-instance class args)))
        (component-pathname component) ; eagerly compute the absolute pathname
        (when (typep component 'system)
          ;; cache information for introspection
          (setf (slot-value component 'depends-on)
                (parse-dependency-defs depends-on)
                (slot-value component 'weakly-depends-on)
                ;; these must be a list of systems, cannot be features or versioned systems
                (mapcar 'coerce-name weakly-depends-on)))
        (let ((sysfile (system-source-file (component-system component)))) ;; requires the previous
          (when (and (typep component 'system) (not bspp))
            (setf (builtin-system-p component) (lisp-implementation-pathname-p sysfile)))
          (setf version (normalize-version version :component name :parent parent :pathname sysfile)))
        ;; Don't use the accessor: kluge to avoid upgrade issue on CCL 1.8.
        ;; A better fix is required.
        (setf (slot-value component 'version) version)
        (when (typep component 'parent-component)
          (setf (component-children component)
                (loop
                  :with previous-component = nil
                  :for c-form :in components
                  :for c = (parse-component-form component c-form
                                                 :previous-serial-component previous-component)
                  :for name = (component-name c)
                  :collect c
                  :when serial :do (setf previous-component name)))
          (compute-children-by-name component))
        (when previous-serial-component
          (push previous-serial-component depends-on))
        (when weakly-depends-on
          ;; ASDF4: deprecate this feature and remove it.
          (appendf depends-on
                   (remove-if (complement #'(lambda (x) (find-system x nil))) weakly-depends-on)))
        ;; Used by POIU. ASDF4: rename to component-depends-on?
        (setf (component-sideway-dependencies component) depends-on)
        (%refresh-component-inline-methods component rest)
        (when if-component-dep-fails
          (error "The system definition for ~S uses deprecated ~
            ASDF option :IF-COMPONENT-DEP-FAILS. ~
            Starting with ASDF 3, please use :IF-FEATURE instead"
           (coerce-name (component-system component))))
        component)))

  (defparameter* *known-systems-with-bad-secondary-system-names*
    (list-to-hash-set '("cl-ppcre")))
  (defun known-system-with-bad-secondary-system-names-p (asd-name)
    ;; Does .asd file with name ASD-NAME contain known exceptions
    ;; that should be screened out of checking for BAD-SYSTEM-NAME?
    (gethash asd-name *known-systems-with-bad-secondary-system-names*))

  (defun register-system-definition
      (name &rest options &key pathname (class 'system) (source-file () sfp)
                            defsystem-depends-on &allow-other-keys)
    ;; The system must be registered before we parse the body,
    ;; otherwise we recur when trying to find an existing system
    ;; of the same name to reuse options (e.g. pathname) from.
    ;; To avoid infinite recursion in cases where you defsystem a system
    ;; that is registered to a different location to find-system,
    ;; we also need to remember it in the asdf-cache.
    (nest
     (with-asdf-session ())
     (let* ((name (coerce-name name))
            (source-file (if sfp source-file (resolve-symlinks* (load-pathname))))))
     (flet ((fix-case (x) (if (logical-pathname-p source-file) (string-downcase x) x))))
     (let* ((asd-name (and source-file
                           (equal "asd" (fix-case (pathname-type source-file)))
                           (fix-case (pathname-name source-file))))
            ;; note that PRIMARY-NAME is a *syntactically* primary name
            (primary-name (primary-system-name name)))
       (when (and asd-name
                  (not (equal asd-name primary-name))
                  (not (known-system-with-bad-secondary-system-names-p asd-name)))
         (warn (make-condition 'bad-system-name :source-file source-file :name name))))
     (let* (;; NB: handle defsystem-depends-on BEFORE to create the system object,
            ;; so that in case it fails, there is no incomplete object polluting the build.
            (checked-defsystem-depends-on
             (let* ((dep-forms (parse-dependency-defs defsystem-depends-on))
                    (deps (loop :for spec :in dep-forms
                            :when (resolve-dependency-spec nil spec)
                            :collect :it)))
               (load-systems* deps)
               dep-forms))
            (system (or (find-system-if-being-defined name)
                        (if-let (registered (registered-system name))
                          (reset-system-class registered 'undefined-system
                                              :name name :source-file source-file)
                          (register-system (make-instance 'undefined-system
                                                          :name name :source-file source-file)))))
            (component-options
             (append
              (remove-plist-keys '(:defsystem-depends-on :class) options)
              ;; cache defsystem-depends-on in canonical form
              (when checked-defsystem-depends-on
                `(:defsystem-depends-on ,checked-defsystem-depends-on))))
            (directory (determine-system-directory pathname)))
       ;; This works hand in hand with asdf/find-system:find-system-if-being-defined:
       (set-asdf-cache-entry `(find-system ,name) (list system)))
     ;; We change-class AFTER we loaded the defsystem-depends-on
     ;; since the class might be defined as part of those.
     (let ((class (class-for-type nil class)))
       (unless (subtypep class 'system)
         (error 'non-system-system :name name :class-name (class-name class)))
       (unless (eq (type-of system) class)
         (reset-system-class system class)))
     (parse-component-form nil (list* :module name :pathname directory component-options))))

  (defmacro defsystem (name &body options)
    `(apply 'register-system-definition ',name ',options)))
