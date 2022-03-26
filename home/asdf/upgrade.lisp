;;;; -------------------------------------------------------------------------
;;;; Handle upgrade as forward- and backward-compatibly as possible
;; See https://bugs.launchpad.net/asdf/+bug/485687

(uiop/package:define-package :asdf/upgrade
  (:recycle :asdf/upgrade :asdf)
  (:use :uiop/common-lisp :uiop)
  (:export
   #:asdf-version #:*previous-asdf-versions* #:*asdf-version*
   #:asdf-message #:*verbose-out*
   #:upgrading-p #:when-upgrading #:upgrade-asdf #:defparameter*
   #:*post-upgrade-cleanup-hook* #:cleanup-upgraded-asdf
   ;; There will be no symbol left behind!
   #:with-asdf-deprecation
   #:intern*)
  (:import-from :uiop/package #:intern* #:find-symbol*))
(in-package :asdf/upgrade)

;;; Special magic to detect if this is an upgrade

(with-upgradability ()
  (defun asdf-version ()
    "Exported interface to the version of ASDF currently installed. A string.
You can compare this string with e.g.: (ASDF:VERSION-SATISFIES (ASDF:ASDF-VERSION) \"3.4.5.67\")."
    (when (find-package :asdf)
      (or (symbol-value (find-symbol (string :*asdf-version*) :asdf))
          (let* ((revsym (find-symbol (string :*asdf-revision*) :asdf))
                 (rev (and revsym (boundp revsym) (symbol-value revsym))))
            (etypecase rev
              (string rev)
              (cons (format nil "~{~D~^.~}" rev))
              (null "1.0"))))))
  ;; This (private) variable contains a list of versions of previously loaded variants of ASDF,
  ;; from which ASDF was upgraded.
  ;; Important: define *p-a-v* /before/ *a-v* so that they initialize correctly.
  (defvar *previous-asdf-versions*
    (let ((previous (asdf-version)))
      (when previous
        ;; Punt on upgrade from ASDF1 or ASDF2, by renaming (or deleting) the package.
        (when (version< previous "2.27") ;; 2.27 is the first to have the :asdf3 feature.
          (let ((away (format nil "~A-~A" :asdf previous)))
            (rename-package :asdf away)
            (when *load-verbose*
              (format t "~&; Renamed old ~A package away to ~A~%" :asdf away))))
        (list previous))))
  ;; This public variable will be bound shortly to the currently loaded version of ASDF.
  (defvar *asdf-version* nil)
  ;; We need to clear systems from versions older than the one in this (private) parameter.
  ;; The latest incompatible defclass is 2.32.13 renaming a slot in component,
  ;; or 3.2.0.2 for CCL (incompatibly changing some superclasses).
  ;; the latest incompatible gf change is in 3.1.7.20 (see redefined-functions below).
  (defparameter *oldest-forward-compatible-asdf-version* "3.2.0.2")
  ;; Semi-private variable: a designator for a stream on which to output ASDF progress messages
  (defvar *verbose-out* nil)
  ;; Private function by which ASDF outputs progress messages and warning messages:
  (defun asdf-message (format-string &rest format-args)
    (when *verbose-out* (apply 'format *verbose-out* format-string format-args)))
  ;; Private hook for functions to run after ASDF has upgraded itself from an older variant:
  (defvar *post-upgrade-cleanup-hook* ())
  ;; Private function to detect whether the current upgrade counts as an incompatible
  ;; data schema upgrade implying the need to drop data.
  (defun upgrading-p (&optional (oldest-compatible-version *oldest-forward-compatible-asdf-version*))
    (and *previous-asdf-versions*
         (version< (first *previous-asdf-versions*) oldest-compatible-version)))
  ;; Private variant of defparameter that works in presence of incompatible upgrades:
  ;; behaves like defvar in a compatible upgrade (e.g. reloading system after simple code change),
  ;; but behaves like defparameter if in presence of an incompatible upgrade.
  (defmacro defparameter* (var value &optional docstring (version *oldest-forward-compatible-asdf-version*))
    (let* ((name (string-trim "*" var))
           (valfun (intern (format nil "%~A-~A-~A" :compute name :value))))
      `(progn
         (defun ,valfun () ,value)
         (defvar ,var (,valfun) ,@(ensure-list docstring))
         (when (upgrading-p ,version)
           (setf ,var (,valfun))))))
  ;; Private macro to declare sections of code that are only compiled and run when upgrading.
  ;; The use of eval portably ensures that the code will not have adverse compile-time side-effects,
  ;; whereas the use of handler-bind portably ensures that it will not issue warnings when it runs.
  (defmacro when-upgrading ((&key (version *oldest-forward-compatible-asdf-version*)
                               (upgrading-p `(upgrading-p ,version)) when) &body body)
    "A wrapper macro for code that should only be run when upgrading a
previously-loaded version of ASDF."
    `(with-upgradability ()
       (when (and ,upgrading-p ,@(when when `(,when)))
         (handler-bind ((style-warning #'muffle-warning))
           (eval '(progn ,@body))))))
  ;; Only now can we safely update the version.
  (let* (;; For bug reporting sanity, please always bump this version when you modify this file.
         ;; Please also modify asdf.asd to reflect this change. make bump-version v=3.4.5.67.8
         ;; can help you do these changes in synch (look at the source for documentation).
         ;; Relying on its automation, the version is now redundantly present on top of asdf.lisp.
         ;; "3.4" would be the general branch for major version 3, minor version 4.
         ;; "3.4.5" would be an official release in the 3.4 branch.
         ;; "3.4.5.67" would be a development version in the official branch, on top of 3.4.5.
         ;; "3.4.5.0.8" would be your eighth local modification of official release 3.4.5
         ;; "3.4.5.67.8" would be your eighth local modification of development version 3.4.5.67
         (asdf-version "3.3.3.3")
         (existing-version (asdf-version)))
    (setf *asdf-version* asdf-version)
    (when (and existing-version (not (equal asdf-version existing-version)))
      (push existing-version *previous-asdf-versions*)
      (when (or *verbose-out* *load-verbose*)
        (format (or *verbose-out* *trace-output*)
                (compatfmt "~&~@<; ~@;Upgrading ASDF ~@[from version ~A ~]to version ~A~@:>~%")
                existing-version asdf-version)))))

;;; Upon upgrade, specially frob some functions and classes that are being incompatibly redefined
(when-upgrading ()
  (let* ((previous-version (first *previous-asdf-versions*))
         (redefined-functions ;; List of functions that changed incompatibly since 2.27:
          ;; gf signature changed (should NOT happen), defun that became a generic function,
          ;; method removed that will mess up with new ones (especially :around :before :after,
          ;; more specific or call-next-method'ed method) and/or semantics otherwise modified. Oops.
          ;; NB: it's too late to do anything about functions in UIOP!
          ;; If you introduce some critical incompatibility there, you must change the function name.
          ;; Note that we don't need do anything about functions that changed incompatibly
          ;; from ASDF 2.26 or earlier: we wholly punt on the entire ASDF package in such an upgrade.
          ;; Also note that we don't include the defgeneric=>defun, because they are
          ;; done directly with defun* and need not trigger a punt on data.
          ;; See discussion at https://gitlab.common-lisp.net/asdf/asdf/merge_requests/36
          `(,@(when (version< previous-version "3.1.2") '(#:component-depends-on #:input-files)) ;; crucial methods *removed* before 3.1.2
            ,@(when (version< previous-version "3.1.7.20") '(#:find-component))))
         (redefined-classes
          ;; redefining the classes causes interim circularities
          ;; with the old ASDF during upgrade, and many implementations bork
          #-clozure ()
          #+clozure
          '((#:compile-concatenated-source-op (#:operation) ())
            (#:compile-bundle-op (#:operation) ())
            (#:concatenate-source-op (#:operation) ())
            (#:dll-op (#:operation) ())
            (#:lib-op (#:operation) ())
            (#:monolithic-compile-bundle-op (#:operation) ())
            (#:monolithic-concatenate-source-op (#:operation) ()))))
    (loop :for name :in redefined-functions
      :for sym = (find-symbol* name :asdf nil)
      :do (when sym (fmakunbound sym)))
    (labels ((asym (x) (multiple-value-bind (s p)
                           (if (consp x) (values (car x) (cadr x)) (values x :asdf))
                         (find-symbol* s p nil)))
             (asyms (l) (mapcar #'asym l)))
      (loop* :for (name superclasses slots) :in redefined-classes
             :for sym = (find-symbol* name :asdf nil)
             :when (and sym (find-class sym))
             :do (eval `(defclass ,sym ,(asyms superclasses) ,(asyms slots)))))))


;;; Self-upgrade functions
(with-upgradability ()
  ;; This private function is called at the end of asdf/footer and ensures that,
  ;; *if* this loading of ASDF was an upgrade, then all registered cleanup functions will be called.
  (defun cleanup-upgraded-asdf (&optional (old-version (first *previous-asdf-versions*)))
    (let ((new-version (asdf-version)))
      (unless (equal old-version new-version)
        (push new-version *previous-asdf-versions*)
        (when old-version
          (if (version<= new-version old-version)
              (error (compatfmt "~&~@<; ~@;Downgraded ASDF from version ~A to version ~A~@:>~%")
                     old-version new-version)
              (asdf-message (compatfmt "~&~@<; ~@;Upgraded ASDF from version ~A to version ~A~@:>~%")
                            old-version new-version))
          ;; In case the previous version was too old to be forward-compatible, clear systems.
          ;; TODO: if needed, we may have to define a separate hook to run
          ;; in case of forward-compatible upgrade.
          ;; Or to move the tests forward-compatibility test inside each hook function?
          (unless (version<= *oldest-forward-compatible-asdf-version* old-version)
            (call-functions (reverse *post-upgrade-cleanup-hook*)))
          t))))

  (defun upgrade-asdf ()
    "Try to upgrade of ASDF. If a different version was used, return T.
   We need do that before we operate on anything that may possibly depend on ASDF."
    (let ((*load-print* nil)
          (*compile-print* nil))
      (handler-bind (((or style-warning) #'muffle-warning))
        (symbol-call :asdf :load-system :asdf :verbose nil))))

  (defmacro with-asdf-deprecation ((&rest keys &key &allow-other-keys) &body body)
    `(with-upgradability ()
       (with-deprecation ((version-deprecation *asdf-version* ,@keys))
         ,@body))))
