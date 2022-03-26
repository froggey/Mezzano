;;;; -------------------------------------------------------------------------
;;;; Finding systems

(uiop/package:define-package :asdf/find-system
  (:recycle :asdf/find-system :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
        :asdf/session :asdf/component :asdf/system :asdf/operation :asdf/action :asdf/lisp-action
        :asdf/find-component :asdf/system-registry :asdf/plan :asdf/operate)
  (:import-from #:asdf/component #:%additional-input-files)
  (:export
   #:find-system #:locate-system #:load-asd #:define-op
   #:load-system-definition-error #:error-name #:error-pathname #:error-condition))
(in-package :asdf/find-system)

(with-upgradability ()
  (define-condition load-system-definition-error (system-definition-error)
    ((name :initarg :name :reader error-name)
     (pathname :initarg :pathname :reader error-pathname)
     (condition :initarg :condition :reader error-condition))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while trying to load definition for system ~A from pathname ~A: ~3i~_~A~@:>")
                       (error-name c) (error-pathname c) (error-condition c)))))


  ;;; Methods for find-system

  ;; Reject NIL as a system designator.
  (defmethod find-system ((name null) &optional (error-p t))
    (when error-p
      (sysdef-error (compatfmt "~@<NIL is not a valid system name~@:>"))))

  ;; Default method for find-system: resolve the argument using COERCE-NAME.
  (defmethod find-system (name &optional (error-p t))
    (find-system (coerce-name name) error-p))

  (defun find-system-if-being-defined (name)
    ;; This function finds systems being defined *in the current ASDF session*, as embodied by
    ;; its session cache, even before they are fully defined and registered in *registered-systems*.
    ;; The purpose of this function is to prevent races between two files that might otherwise
    ;; try overwrite each other's system objects, resulting in infinite loops and stack overflow.
    ;; This function explicitly MUST NOT find definitions merely registered in previous sessions.
    ;; NB: this function depends on a corresponding side-effect in parse-defsystem;
    ;; the precise protocol between the two functions may change in the future (or not).
    (first (gethash `(find-system ,(coerce-name name)) (asdf-cache))))

  (defclass define-op (non-propagating-operation) ()
    (:documentation "An operation to record dependencies on loading a .asd file."))

  (defmethod record-dependency ((plan null) (operation t) (component t))
    (unless (or (typep operation 'define-op)
                (and (typep operation 'load-op)
                     (typep component 'system)
                     (equal "asdf" (coerce-name component))))
      (if-let ((action (first (visiting-action-list *asdf-session*))))
        (let ((parent-operation (action-operation action))
              (parent-component (action-component action)))
          (cond
            ((and (typep parent-operation 'define-op)
                  (typep parent-component 'system))
             (let ((action (cons operation component)))
               (unless (gethash action (definition-dependency-set parent-component))
                 (push (cons operation component) (definition-dependency-list parent-component))
                 (setf (gethash action (definition-dependency-set parent-component)) t))))
            (t
             (warn 'recursive-operate
                   :operation operation :component component :action action)))))))

  (defmethod component-depends-on ((o define-op) (s system))
    `(;;NB: 1- ,@(system-defsystem-depends-on s)) ; Should be already included in the below.
      ;; 2- We don't call-next-method to avoid other methods
      ,@(loop* :for (o . c) :in (definition-dependency-list s) :collect (list o c))))

  (defmethod component-depends-on ((o operation) (s system))
    `(,@(when (and (not (typep o 'define-op))
                   (or (system-source-file s) (definition-dependency-list s)))
              `((define-op ,(primary-system-name s))))
      ,@(call-next-method)))

  (defmethod perform ((o operation) (c undefined-system))
    (sysdef-error "Trying to use undefined or incompletely defined system ~A" (coerce-name c)))

  ;; TODO: could this file be refactored so that locate-system is merely
  ;; the cache-priming call to input-files here?
  (defmethod input-files ((o define-op) (s system))
    (if-let ((asd (system-source-file s))) (list asd)))

  (defmethod perform ((o define-op) (s system))
    (nest
     (if-let ((pathname (first (input-files o s)))))
     (let ((readtable *readtable*) ;; save outer syntax tables. TODO: proper syntax-control
           (print-pprint-dispatch *print-pprint-dispatch*)))
     (with-standard-io-syntax)
     (let ((*print-readably* nil)
           ;; Note that our backward-compatible *readtable* is
           ;; a global readtable that gets globally side-effected. Ouch.
           ;; Same for the *print-pprint-dispatch* table.
           ;; We should do something about that for ASDF3 if possible, or else ASDF4.
           (*readtable* readtable) ;; restore inside syntax table
           (*print-pprint-dispatch* print-pprint-dispatch)
           (*package* (find-package :asdf-user))
           (*default-pathname-defaults*
            ;; resolve logical-pathnames so they won't wreak havoc in parsing namestrings.
            (pathname-directory-pathname (physicalize-pathname pathname)))))
     (handler-bind
         (((and error (not missing-component))
           #'(lambda (condition)
               (error 'load-system-definition-error
                      :name (coerce-name s) :pathname pathname :condition condition))))
       (asdf-message (compatfmt "~&~@<; ~@;Loading system definition~@[ for ~A~] from ~A~@:>~%")
                     (coerce-name s) pathname)
       ;; dependencies will depend on what's loaded via definition-dependency-list
       (unset-asdf-cache-entry `(component-depends-on ,o ,s))
       (unset-asdf-cache-entry `(input-files ,o ,s)))
     (load* pathname :external-format (encoding-external-format (detect-encoding pathname)))))

  (defun load-asd (pathname &key name)
    "Load system definitions from PATHNAME.
NAME if supplied is the name of a system expected to be defined in that file.

Do NOT try to load a .asd file directly with CL:LOAD. Always use ASDF:LOAD-ASD."
    (with-asdf-session ()
      ;; TODO: use OPERATE, so we consult the cache and only load once per session.
      (flet ((do-it (o c) (operate o c)))
        (let ((primary-name (primary-system-name (or name (pathname-name pathname))))
              (operation (make-operation 'define-op)))
          (if-let (system (registered-system primary-name))
            (progn
              ;; We already determine this to be obsolete ---
              ;; or should we move some tests from find-system to check for up-to-date-ness here?
              (setf (component-operation-time operation system) t
                    (definition-dependency-list system) nil
                    (definition-dependency-set system) (list-to-hash-set nil))
              (do-it operation system))
            (let ((system (make-instance 'undefined-system
                                         :name primary-name :source-file pathname)))
              (register-system system)
              (unwind-protect (do-it operation system)
                (when (typep system 'undefined-system)
                  (clear-system system)))))))))

  (defvar *old-asdf-systems* (make-hash-table :test 'equal))

  ;; (Private) function to check that a system that was found isn't an asdf downgrade.
  ;; Returns T if everything went right, NIL if the system was an ASDF at an older version,
  ;; or UIOP of the same or older version, that shall not be loaded.
  ;; Also issue a warning if it was a strictly older version of ASDF.
  (defun check-not-old-asdf-system (name pathname)
    (or (not (member name '("asdf" "uiop") :test 'equal))
        (null pathname)
        (let* ((asdfp (equal name "asdf")) ;; otherwise, it's uiop
               (version-pathname
                (subpathname pathname "version" :type (if asdfp "lisp-expr" "lisp")))
               (version (and (probe-file* version-pathname :truename nil)
                             (read-file-form version-pathname :at (if asdfp '(0) '(2 2 2)))))
               (old-version (asdf-version)))
          (cond
            ;; Same version is OK for ASDF, to allow loading from modified source.
            ;; However, do *not* load UIOP of the exact same version:
            ;; it was already loaded it as part of ASDF and would only be double-loading.
            ;; Be quiet about it, though, since it's a normal situation.
            ((equal old-version version) asdfp)
            ((version< old-version version) t) ;; newer version: Good!
            (t ;; old version: bad
             (ensure-gethash
              (list (namestring pathname) version) *old-asdf-systems*
              #'(lambda ()
                  (let ((old-pathname (system-source-file (registered-system "asdf"))))
                    (if asdfp
                        (warn "~@<~
        You are using ASDF version ~A ~:[(probably from (require \"asdf\") ~
        or loaded by quicklisp)~;from ~:*~S~] and have an older version of ASDF ~
        ~:[(and older than 2.27 at that)~;~:*~A~] registered at ~S. ~
        Having an ASDF installed and registered is the normal way of configuring ASDF to upgrade itself, ~
        and having an old version registered is a configuration error. ~
        ASDF will ignore this configured system rather than downgrade itself. ~
        In the future, you may want to either: ~
        (a) upgrade this configured ASDF to a newer version, ~
        (b) install a newer ASDF and register it in front of the former in your configuration, or ~
        (c) uninstall or unregister this and any other old version of ASDF from your configuration. ~
        Note that the older ASDF might be registered implicitly through configuration inherited ~
        from your system installation, in which case you might have to specify ~
        :ignore-inherited-configuration in your in your ~~/.config/common-lisp/source-registry.conf ~
        or other source-registry configuration file, environment variable or lisp parameter. ~
        Indeed, a likely offender is an obsolete version of the cl-asdf debian or ubuntu package, ~
        that you might want to upgrade (if a recent enough version is available) ~
        or else remove altogether (since most implementations ship with a recent asdf); ~
        if you lack the system administration rights to upgrade or remove this package, ~
        then you might indeed want to either install and register a more recent version, ~
        or use :ignore-inherited-configuration to avoid registering the old one. ~
        Please consult ASDF documentation and/or experts.~@:>~%"
                              old-version old-pathname version pathname)
                        ;; NB: for UIOP, don't warn, just ignore.
                        (warn "ASDF ~A (from ~A), UIOP ~A (from ~A)"
                              old-version old-pathname version pathname)
                        ))))
             nil))))) ;; only issue the warning the first time, but always return nil

  (defun locate-system (name)
    "Given a system NAME designator, try to locate where to load the system from.
Returns six values: FOUNDP FOUND-SYSTEM PATHNAME PREVIOUS PREVIOUS-TIME PREVIOUS-PRIMARY
FOUNDP is true when a system was found,
either a new unregistered one or a previously registered one.
FOUND-SYSTEM when not null is a SYSTEM object that may be REGISTER-SYSTEM'ed.
PATHNAME when not null is a path from which to load the system,
either associated with FOUND-SYSTEM, or with the PREVIOUS system.
PREVIOUS when not null is a previously loaded SYSTEM object of same name.
PREVIOUS-TIME when not null is the time at which the PREVIOUS system was loaded.
PREVIOUS-PRIMARY when not null is the primary system for the PREVIOUS system."
    (with-asdf-session () ;; NB: We don't cache the results. We once used to, but it wasn't useful,
      ;; and keeping a negative cache was a bug (see lp#1335323), which required
      ;; explicit invalidation in clear-system and find-system (when unsucccessful).
      (let* ((name (coerce-name name))
             (previous (registered-system name)) ; load from disk if absent or newer on disk
             (previous-primary-name (and previous (primary-system-name previous)))
             (previous-primary-system (and previous-primary-name
                                           (registered-system previous-primary-name)))
             (previous-time (and previous-primary-system
                                 (component-operation-time 'define-op previous-primary-system)))
             (found (search-for-system-definition name))
             (found-system (and (typep found 'system) found))
             (pathname (ensure-pathname
                        (or (and (typep found '(or pathname string)) (pathname found))
                            (system-source-file found-system)
                            (system-source-file previous))
                        :want-absolute t :resolve-symlinks *resolve-symlinks*))
             (foundp (and (or found-system pathname previous) t)))
        (check-type found (or null pathname system))
        (unless (check-not-old-asdf-system name pathname)
          (check-type previous system) ;; asdf is preloaded, so there should be a previous one.
          (setf found-system nil pathname nil))
        (values foundp found-system pathname previous previous-time previous-primary-system))))

  ;; TODO: make a prepare-define-op node for this
  ;; so we can properly cache the answer rather than recompute it.
  (defun definition-dependencies-up-to-date-p (system)
    (check-type system system)
    (or (not (primary-system-p system))
        (handler-case
            (loop :with plan = (make-instance *plan-class*)
              :for action :in (definition-dependency-list system)
              :always (action-up-to-date-p
                       plan (action-operation action) (action-component action))
              :finally
              (let ((o (make-operation 'define-op)))
                (multiple-value-bind (stamp done-p)
                    (compute-action-stamp plan o system)
                  (return (and (timestamp<= stamp (component-operation-time o system))
                               done-p)))))
          (system-out-of-date () nil))))

  ;; Main method for find-system: first, make sure the computation is memoized in a session cache.
  ;; Unless the system is immutable, use locate-system to find the primary system;
  ;; reconcile the finding (if any) with any previous definition (in a previous session,
  ;; preloaded, with a previous configuration, or before filesystem changes), and
  ;; load a found .asd if appropriate. Finally, update registration table and return results.
  (defmethod find-system ((name string) &optional (error-p t))
    (nest
     (with-asdf-session (:key `(find-system ,name)))
     (let ((name-primary-p (primary-system-p name)))
       (unless name-primary-p (find-system (primary-system-name name) nil)))
     (or (and *immutable-systems* (gethash name *immutable-systems*) (registered-system name)))
     (multiple-value-bind (foundp found-system pathname previous previous-time previous-primary)
         (locate-system name)
       (assert (eq foundp (and (or found-system pathname previous) t))))
     (let ((previous-pathname (system-source-file previous))
           (system (or previous found-system)))
       (when (and found-system (not previous))
         (register-system found-system))
       (when (and system pathname)
         (setf (system-source-file system) pathname))
       (if-let ((stamp (get-file-stamp pathname)))
         (let ((up-to-date-p
                (and previous previous-primary
                     (or (pathname-equal pathname previous-pathname)
                         (and pathname previous-pathname
                              (pathname-equal
                               (physicalize-pathname pathname)
                               (physicalize-pathname previous-pathname))))
                     (timestamp<= stamp previous-time)
                     ;; Check that all previous definition-dependencies are up-to-date,
                     ;; traversing them without triggering the adding of nodes to the plan.
                     ;; TODO: actually have a prepare-define-op, extract its timestamp,
                     ;; and check that it is less than the stamp of the previous define-op ?
                     (definition-dependencies-up-to-date-p previous-primary))))
           (unless up-to-date-p
             (restart-case
                 (signal 'system-out-of-date :name name)
               (continue () :report "continue"))
             (load-asd pathname :name name)))))
     ;; Try again after having loaded from disk if needed
     (or (registered-system name)
         (when error-p (error 'missing-component :requires name)))))

  ;; Resolved forward reference for asdf/system-registry.
  (defun mark-component-preloaded (component)
    "Mark a component as preloaded."
    (let ((component (find-component component nil :registered t)))
      ;; Recurse to children, so asdf/plan will hopefully be happy.
      (map () 'mark-component-preloaded (component-children component))
      ;; Mark the timestamps of the common lisp-action operations as 0.
      (let ((cot (component-operation-times component)))
        (dolist (o `(,@(when (primary-system-p component) '(define-op))
                       prepare-op compile-op load-op))
          (setf (gethash (make-operation o) cot) 0))))))
