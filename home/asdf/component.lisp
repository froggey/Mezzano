;;;; -------------------------------------------------------------------------
;;;; Components

(uiop/package:define-package :asdf/component
  (:recycle :asdf/component :asdf/find-component :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session)
  (:export
   #:component #:component-find-path
   #:find-component ;; methods defined in find-component
   #:component-name #:component-pathname #:component-relative-pathname
   #:component-parent #:component-system #:component-parent-pathname
   #:child-component #:parent-component #:module
   #:file-component
   #:source-file #:c-source-file #:java-source-file
   #:static-file #:doc-file #:html-file
   #:file-type
   #:source-file-type #:source-file-explicit-type ;; backward-compatibility
   #:component-in-order-to #:component-sideway-dependencies
   #:component-if-feature #:around-compile-hook
   #:component-description #:component-long-description
   #:component-version #:version-satisfies
   #:component-inline-methods ;; backward-compatibility only. DO NOT USE!
   #:component-operation-times ;; For internal use only.
   ;; portable ASDF encoding and implementation-specific external-format
   #:component-external-format #:component-encoding
   #:component-children-by-name #:component-children #:compute-children-by-name
   #:component-build-operation
   #:module-default-component-class
   #:module-components ;; backward-compatibility. DO NOT USE.
   #:sub-components

   ;; conditions
   #:duplicate-names

   ;; Internals we'd like to share with the ASDF package, especially for upgrade purposes
   #:name #:version #:description #:long-description #:author #:maintainer #:licence
   #:components-by-name #:components #:children #:children-by-name
   #:default-component-class #:source-file
   #:defsystem-depends-on ; This symbol retained for backward compatibility.
   #:sideway-dependencies #:if-feature #:in-order-to #:inline-methods
   #:relative-pathname #:absolute-pathname #:operation-times #:around-compile
   #:%encoding #:properties #:component-properties #:parent))
(in-package :asdf/component)

(with-upgradability ()
  (defgeneric component-name (component)
    (:documentation "Name of the COMPONENT, unique relative to its parent"))
  (defgeneric component-system (component)
    (:documentation "Top-level system containing the COMPONENT"))
  (defgeneric component-pathname (component)
    (:documentation "Pathname of the COMPONENT if any, or NIL."))
  (defgeneric component-relative-pathname (component)
    ;; in ASDF4, rename that to component-specified-pathname ?
    (:documentation "Specified pathname of the COMPONENT,
intended to be merged with the pathname of that component's parent if any, using merged-pathnames*.
Despite the function's name, the return value can be an absolute pathname, in which case the merge
will leave it unmodified."))
  (defgeneric component-external-format (component)
    (:documentation "The external-format of the COMPONENT.
By default, deduced from the COMPONENT-ENCODING."))
  (defgeneric component-encoding (component)
    (:documentation "The encoding of the COMPONENT. By default, only :utf-8 is supported.
Use asdf-encodings to support more encodings."))
  (defgeneric version-satisfies (component version)
    (:documentation "Check whether a COMPONENT satisfies the constraint of being at least as recent
as the specified VERSION, which must be a string of dot-separated natural numbers, or NIL."))
  (defgeneric component-version (component)
    (:documentation "Return the version of a COMPONENT, which must be a string of dot-separated
natural numbers, or NIL."))
  (defgeneric (setf component-version) (new-version component)
    (:documentation "Updates the version of a COMPONENT, which must be a string of dot-separated
natural numbers, or NIL."))
  (defgeneric component-parent (component)
    (:documentation "The parent of a child COMPONENT,
or NIL for top-level components (a.k.a. systems)"))
  ;; NIL is a designator for the absence of a component, in which case the parent is also absent.
  (defmethod component-parent ((component null)) nil)

  ;; Deprecated: Backward compatible way of computing the FILE-TYPE of a component.
  (with-asdf-deprecation (:style-warning "3.4")
   (defgeneric source-file-type (component system)
     (:documentation "DEPRECATED. Use the FILE-TYPE of a COMPONENT instead.")))

  (define-condition duplicate-names (system-definition-error)
    ((name :initarg :name :reader duplicate-names-name))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while defining system: multiple components are given same name ~S~@:>")
                       (duplicate-names-name c))))))


(with-upgradability ()
  (defclass component ()
    ((name :accessor component-name :initarg :name :type string :documentation
           "Component name: designator for a string composed of portable pathname characters")
     ;; We might want to constrain version with
     ;; :type (and string (satisfies parse-version))
     ;; but we cannot until we fix all systems that don't use it correctly!
     (version :accessor component-version :initarg :version :initform nil)
     (description :accessor component-description :initarg :description :initform nil)
     (long-description :accessor component-long-description :initarg :long-description :initform nil)
     (sideway-dependencies :accessor component-sideway-dependencies :initform nil)
     (if-feature :accessor component-if-feature :initform nil :initarg :if-feature)
     ;; In the ASDF object model, dependencies exist between *actions*,
     ;; where an action is a pair of an operation and a component.
     ;; Dependencies are represented as alists of operations
     ;; to a list where each entry is a pair of an operation and a list of component specifiers.
     ;; Up until ASDF 2.26.9, there used to be two kinds of dependencies:
     ;; in-order-to and do-first, each stored in its own slot. Now there is only in-order-to.
     ;; in-order-to used to represent things that modify the filesystem (such as compiling a fasl)
     ;; and do-first things that modify the current image (such as loading a fasl).
     ;; These are now unified because we now correctly propagate timestamps between dependencies.
     ;; Happily, no one seems to have used do-first too much (especially since until ASDF 2.017,
     ;; anything you specified was overridden by ASDF itself anyway), but the name in-order-to remains.
     ;; The names are bad, but they have been the official API since Dan Barlow's ASDF 1.52!
     ;; LispWorks's defsystem has caused-by and requires for in-order-to and do-first respectively.
     ;; Maybe rename the slots in ASDF? But that's not very backward-compatible.
     ;; See our ASDF 2 paper for more complete explanations.
     (in-order-to :initform nil :initarg :in-order-to
                  :accessor component-in-order-to)
     ;; Methods defined using the "inline" style inside a defsystem form:
     ;; we store them here so we can delete them when the system is re-evaluated.
     (inline-methods :accessor component-inline-methods :initform nil)
     ;; ASDF4: rename it from relative-pathname to specified-pathname. It need not be relative.
     ;; There is no initform and no direct accessor for this specified pathname,
     ;; so we only access the information through appropriate methods, after it has been processed.
     ;; Unhappily, some braindead systems directly access the slot. Make them stop before ASDF4.
     (relative-pathname :initarg :pathname)
     ;; The absolute-pathname is computed based on relative-pathname and parent pathname.
     ;; The slot is but a cache used by component-pathname.
     (absolute-pathname)
     (operation-times :initform (make-hash-table)
                      :accessor component-operation-times)
     (around-compile :initarg :around-compile)
     ;; Properties are for backward-compatibility with ASDF2 only. DO NOT USE!
     (properties :accessor component-properties :initarg :properties
                 :initform nil)
     (%encoding :accessor %component-encoding :initform nil :initarg :encoding)
     ;; For backward-compatibility, this slot is part of component rather than of child-component. ASDF4: stop it.
     (parent :initarg :parent :initform nil :reader component-parent)
     (build-operation
      :initarg :build-operation :initform nil :reader component-build-operation)
     ;; Cache for ADDITIONAL-INPUT-FILES function.
     (additional-input-files :accessor %additional-input-files :initform nil))
    (:documentation "Base class for all components of a build"))

  (defgeneric find-component (base path &key registered)
    (:documentation "Find a component by resolving the PATH starting from BASE parent.
If REGISTERED is true, only search currently registered systems."))

  (defun component-find-path (component)
    "Return a path from a root system to the COMPONENT.
The return value is a list of component NAMES; a list of strings."
    (check-type component (or null component))
    (reverse
     (loop :for c = component :then (component-parent c)
           :while c :collect (component-name c))))

  (defmethod print-object ((c component) stream)
    (print-unreadable-object (c stream :type t :identity nil)
      (format stream "~{~S~^ ~}" (component-find-path c))))

  (defmethod component-system ((component component))
    (if-let (system (component-parent component))
      (component-system system)
      component)))


;;;; Component hierarchy within a system
;; The tree typically but not necessarily follows the filesystem hierarchy.
(with-upgradability ()
  (defclass child-component (component) ()
    (:documentation "A CHILD-COMPONENT is a COMPONENT that may be part of
a PARENT-COMPONENT."))

  (defclass file-component (child-component)
    ((type :accessor file-type :initarg :type)) ; no default
    (:documentation "a COMPONENT that represents a file"))
  (defclass source-file (file-component)
    ((type :accessor source-file-explicit-type ;; backward-compatibility
           :initform nil))) ;; NB: many systems have come to rely on this default.
  (defclass c-source-file (source-file)
    ((type :initform "c")))
  (defclass java-source-file (source-file)
    ((type :initform "java")))
  (defclass static-file (source-file)
    ((type :initform nil))
    (:documentation "Component for a file to be included as is in the build output"))
  (defclass doc-file (static-file) ())
  (defclass html-file (doc-file)
    ((type :initform "html")))

  (defclass parent-component (component)
    ((children
      :initform nil
      :initarg :components
      :reader module-components ; backward-compatibility
      :accessor component-children)
     (children-by-name
      :reader module-components-by-name ; backward-compatibility
      :accessor component-children-by-name)
     (default-component-class
      :initform nil
      :initarg :default-component-class
      :accessor module-default-component-class))
  (:documentation "A PARENT-COMPONENT is a component that may have children.")))

(with-upgradability ()
  ;; (Private) Function that given a PARENT component,
  ;; the list of children of which has been initialized,
  ;; compute the hash-table in slot children-by-name that allows to retrieve its children by name.
  ;; If ONLY-IF-NEEDED-P is defined, skip any (re)computation if the slot is already populated.
  (defun compute-children-by-name (parent &key only-if-needed-p)
    (unless (and only-if-needed-p (slot-boundp parent 'children-by-name))
      (let ((hash (make-hash-table :test 'equal)))
        (setf (component-children-by-name parent) hash)
        (loop :for c :in (component-children parent)
              :for name = (component-name c)
              :for previous = (gethash name hash)
              :do (when previous (error 'duplicate-names :name name))
                  (setf (gethash name hash) c))
        hash))))

(with-upgradability ()
  (defclass module (child-component parent-component)
    (#+clisp (components)) ;; backward compatibility during upgrade only
    (:documentation "A module is a intermediate component with both a parent and children,
typically but not necessarily representing the files in a subdirectory of the build source.")))


;;;; component pathnames
(with-upgradability ()
  (defgeneric component-parent-pathname (component)
    (:documentation "The pathname of the COMPONENT's parent, if any, or NIL"))
  (defmethod component-parent-pathname (component)
    (component-pathname (component-parent component)))

  ;; The default method for component-pathname tries to extract a cached precomputed
  ;; absolute-pathname from the relevant slot, and if not, computes it by merging the
  ;; component-relative-pathname (which should be component-specified-pathname, it can be absolute)
  ;; with the directory of the component-parent-pathname.
  (defmethod component-pathname ((component component))
    (if (slot-boundp component 'absolute-pathname)
        (slot-value component 'absolute-pathname)
        (let ((pathname
                (merge-pathnames*
                 (component-relative-pathname component)
                 (pathname-directory-pathname (component-parent-pathname component)))))
          (unless (or (null pathname) (absolute-pathname-p pathname))
            (error (compatfmt "~@<Invalid relative pathname ~S for component ~S~@:>")
                   pathname (component-find-path component)))
          (setf (slot-value component 'absolute-pathname) pathname)
          pathname)))

  ;; Default method for component-relative-pathname:
  ;; combine the contents of slot relative-pathname (from specified initarg :pathname)
  ;; with the appropriate source-file-type, which defaults to the file-type of the component.
  (defmethod component-relative-pathname ((component component))
    ;; SOURCE-FILE-TYPE below is strictly for backward-compatibility with ASDF1.
    ;; We ought to be able to extract this from the component alone with FILE-TYPE.
    ;; TODO: track who uses it in Quicklisp, and have them not use it anymore;
    ;; maybe issue a WARNING (then eventually CERROR) if the two methods diverge?
    (parse-unix-namestring
     (or (and (slot-boundp component 'relative-pathname)
              (slot-value component 'relative-pathname))
         (component-name component))
     :want-relative t
     :type (source-file-type component (component-system component))
     :defaults (component-parent-pathname component)))

  (defmethod source-file-type ((component parent-component) (system parent-component))
    :directory)

  (defmethod source-file-type ((component file-component) (system parent-component))
    (file-type component)))


;;;; Encodings
(with-upgradability ()
  (defmethod component-encoding ((c component))
    (or (loop :for x = c :then (component-parent x)
              :while x :thereis (%component-encoding x))
        (detect-encoding (component-pathname c))))

  (defmethod component-external-format ((c component))
    (encoding-external-format (component-encoding c))))


;;;; around-compile-hook
(with-upgradability ()
  (defgeneric around-compile-hook (component)
    (:documentation "An optional hook function that will be called with one argument, a thunk.
The hook function must call the thunk, that will compile code from the component, and may or may not
also evaluate the compiled results. The hook function may establish dynamic variable bindings around
this compilation, or check its results, etc."))
  (defmethod around-compile-hook ((c component))
    (cond
      ((slot-boundp c 'around-compile)
       (slot-value c 'around-compile))
      ((component-parent c)
       (around-compile-hook (component-parent c))))))


;;;; version-satisfies
(with-upgradability ()
  ;; short-circuit testing of null version specifications.
  ;; this is an all-pass, without warning
  (defmethod version-satisfies :around ((c t) (version null))
    t)
  (defmethod version-satisfies ((c component) version)
    (unless (and version (slot-boundp c 'version) (component-version c))
      (when version
        (warn "Requested version ~S but ~S has no version" version c))
      (return-from version-satisfies nil))
    (version-satisfies (component-version c) version))

  (defmethod version-satisfies ((cver string) version)
    (version<= version cver)))


;;; all sub-components (of a given type)
(with-upgradability ()
  (defun sub-components (component &key (type t))
    "Compute the transitive sub-components of given COMPONENT that are of given TYPE"
    (while-collecting (c)
      (labels ((recurse (x)
                 (when (if-let (it (component-if-feature x)) (featurep it) t)
                   (when (typep x type)
                     (c x))
                   (when (typep x 'parent-component)
                     (map () #'recurse (component-children x))))))
        (recurse component)))))

