;;;; -------------------------------------------------------------------------
;;;; Finding systems

(uiop/package:define-package :asdf/system-registry
  (:recycle :asdf/system-registry :asdf/find-system :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
    :asdf/session :asdf/component :asdf/system)
  (:export
   #:remove-entry-from-registry #:coerce-entry-to-directory
   #:registered-system #:register-system
   #:registered-systems* #:registered-systems
   #:clear-system #:map-systems
   #:*system-definition-search-functions* #:search-for-system-definition
   #:*central-registry* #:probe-asd #:sysdef-central-registry-search
   #:contrib-sysdef-search #:sysdef-find-asdf ;; backward compatibility symbols, functions removed
   #:sysdef-preloaded-system-search #:register-preloaded-system #:*preloaded-systems*
   #:find-system-if-being-defined #:mark-component-preloaded ;; forward references to asdf/find-system
   #:sysdef-immutable-system-search #:register-immutable-system #:*immutable-systems*
   #:*registered-systems* #:clear-registered-systems
   ;; defined in source-registry, but specially mentioned here:
   #:sysdef-source-registry-search))
(in-package :asdf/system-registry)

(with-upgradability ()
  ;;; Registry of Defined Systems

  (defvar *registered-systems* (make-hash-table :test 'equal)
    "This is a hash table whose keys are strings -- the names of systems --
and whose values are systems.
A system is referred to as \"registered\" if it is present in this table.")

  (defun registered-system (name)
    "Return a system of given NAME that was registered already,
if such a system exists.  NAME is a system designator, to be
normalized by COERCE-NAME. The value returned is a system object,
or NIL if not found."
    (gethash (coerce-name name) *registered-systems*))

  (defun registered-systems* ()
    "Return a list containing every registered system (as a system object)."
    (loop :for registered :being :the :hash-values :of *registered-systems*
          :collect registered))

  (defun registered-systems ()
    "Return a list of the names of every registered system."
    (mapcar 'coerce-name (registered-systems*)))

  (defun register-system (system)
    "Given a SYSTEM object, register it."
    (check-type system system)
    (let ((name (component-name system)))
      (check-type name string)
      (asdf-message (compatfmt "~&~@<; ~@;Registering system ~3i~_~A~@:>~%") name)
      (setf (gethash name *registered-systems*) system)))

  (defun map-systems (fn)
    "Apply FN to each defined system.

FN should be a function of one argument. It will be
called with an object of type asdf:system."
    (loop :for registered :being :the :hash-values :of *registered-systems*
          :do (funcall fn registered)))


  ;;; Preloaded systems: in the image even if you can't find source files backing them.

  (defvar *preloaded-systems* (make-hash-table :test 'equal)
    "Registration table for preloaded systems.")

  (declaim (ftype (function (t) t) mark-component-preloaded)) ; defined in asdf/find-system

  (defun make-preloaded-system (name keys)
    "Make a preloaded system of given NAME with build information from KEYS"
    (let ((system (apply 'make-instance (getf keys :class 'system)
                         :name name :source-file (getf keys :source-file)
                         (remove-plist-keys '(:class :name :source-file) keys))))
      (mark-component-preloaded system)
      system))

  (defun sysdef-preloaded-system-search (requested)
    "If REQUESTED names a system registered as preloaded, return a new system
with its registration information."
    (let ((name (coerce-name requested)))
      (multiple-value-bind (keys foundp) (gethash name *preloaded-systems*)
        (when foundp
          (make-preloaded-system name keys)))))

  (defun ensure-preloaded-system-registered (name)
    "If there isn't a registered _defined_ system of given NAME,
and a there is a registered _preloaded_ system of given NAME,
then define and register said preloaded system."
    (if-let (system (and (not (registered-system name)) (sysdef-preloaded-system-search name)))
      (register-system system)))

  (defun register-preloaded-system (system-name &rest keys &key (version t) &allow-other-keys)
    "Register a system as being preloaded. If the system has not been loaded from the filesystem
yet, or if its build information is later cleared with CLEAR-SYSTEM, a dummy system will be
registered without backing filesystem information, based on KEYS (e.g. to provide a VERSION).
If VERSION is the default T, and a system was already loaded, then its version will be preserved."
    (let ((name (coerce-name system-name)))
      (when (eql version t)
        (if-let (system (registered-system name))
          (setf (getf keys :version) (component-version system))))
      (setf (gethash name *preloaded-systems*) keys)
      (ensure-preloaded-system-registered system-name)))


  ;;; Immutable systems: in the image and can't be reloaded from source.

  (defvar *immutable-systems* nil
    "A hash-set (equal hash-table mapping keys to T) of systems that are immutable,
i.e. already loaded in memory and not to be refreshed from the filesystem.
They will be treated specially by find-system, and passed as :force-not argument to make-plan.

For instance, to can deliver an image with many systems precompiled, that *will not* check the
filesystem for them every time a user loads an extension, what more risk a problematic upgrade
 or catastrophic downgrade, before you dump an image, you may use:
   (map () 'asdf:register-immutable-system (asdf:already-loaded-systems))

Note that direct access to this variable from outside ASDF is not supported.
Please call REGISTER-IMMUTABLE-SYSTEM to add new immutable systems, and
contact maintainers if you need a stable API to do more than that.")

  (defun sysdef-immutable-system-search (requested)
    (let ((name (coerce-name requested)))
      (when (and *immutable-systems* (gethash name *immutable-systems*))
        (or (registered-system requested)
            (error 'formatted-system-definition-error
                   :format-control "Requested system ~A registered as an immutable-system, ~
but not even registered as defined"
                   :format-arguments (list name))))))

  (defun register-immutable-system (system-name &rest keys)
    "Register SYSTEM-NAME as preloaded and immutable.
It will automatically be considered as passed to FORCE-NOT in a plan."
    (let ((system-name (coerce-name system-name)))
      (apply 'register-preloaded-system system-name keys)
      (unless *immutable-systems*
        (setf *immutable-systems* (list-to-hash-set nil)))
      (setf (gethash system-name *immutable-systems*) t)))


  ;;; Making systems undefined.

  (defun clear-system (system)
    "Clear the entry for a SYSTEM in the database of systems previously defined.
However if the system was registered as PRELOADED (which it is if it is IMMUTABLE),
then a new system with the same name will be defined and registered in its place
from which build details will have been cleared.
Note that this does NOT in any way cause any of the code of the system to be unloaded.
Returns T if system was or is now undefined, NIL if a new preloaded system was redefined."
    ;; There is no "unload" operation in Common Lisp, and
    ;; a general such operation cannot be portably written,
    ;; considering how much CL relies on side-effects to global data structures.
    (let ((name (coerce-name system)))
      (remhash name *registered-systems*)
      (unset-asdf-cache-entry `(find-system ,name))
      (not (ensure-preloaded-system-registered name))))

  (defun clear-registered-systems ()
    "Clear all currently registered defined systems.
Preloaded systems (including immutable ones) will be reset, other systems will be de-registered."
    (map () 'clear-system (registered-systems)))


  ;;; Searching for system definitions

  ;; For the sake of keeping things reasonably neat, we adopt a convention that
  ;; only symbols are to be pushed to this list (rather than e.g. function objects),
  ;; which makes upgrade easier. Also, the name of these symbols shall start with SYSDEF-
  (defvar *system-definition-search-functions* '()
    "A list that controls the ways that ASDF looks for system definitions.
It contains symbols to be funcalled in order, with a requested system name as argument,
until one returns a non-NIL result (if any), which must then be a fully initialized system object
with that name.")

  ;; Initialize and/or upgrade the *system-definition-search-functions*
  ;; so it doesn't contain obsolete symbols, and does contain the current ones.
  (defun cleanup-system-definition-search-functions ()
    (setf *system-definition-search-functions*
          (append
           ;; Remove known-incompatible sysdef functions from old versions of asdf.
           ;; Order matters, so we can't just use set-difference.
           (let ((obsolete
                  '(contrib-sysdef-search sysdef-find-asdf sysdef-preloaded-system-search)))
             (remove-if #'(lambda (x) (member x obsolete)) *system-definition-search-functions*))
           ;; Tuck our defaults at the end of the list if they were absent.
           ;; This is imperfect, in case they were removed on purpose,
           ;; but then it will be the responsibility of whoever removes these symmbols
           ;; to upgrade asdf before he does such a thing rather than after.
           (remove-if #'(lambda (x) (member x *system-definition-search-functions*))
                      '(sysdef-central-registry-search
                        sysdef-source-registry-search)))))
  (cleanup-system-definition-search-functions)

  ;; This (private) function does the search for a system definition using *s-d-s-f*;
  ;; it is to be called by locate-system.
  (defun search-for-system-definition (system)
    ;; Search for valid definitions of the system available in the current session.
    ;; Previous definitions as registered in *registered-systems* MUST NOT be considered;
    ;; they will be reconciled by locate-system then find-system.
    ;; There are two special treatments: first, specially search for objects being defined
    ;; in the current session, to avoid definition races between several files;
    ;; second, specially search for immutable systems, so they cannot be redefined.
    ;; Finally, use the search functions specified in *system-definition-search-functions*.
    (let ((name (coerce-name system)))
      (flet ((try (f) (if-let ((x (funcall f name))) (return-from search-for-system-definition x))))
        (try 'find-system-if-being-defined)
        (try 'sysdef-immutable-system-search)
        (map () #'try *system-definition-search-functions*))))


  ;;; The legacy way of finding a system: the *central-registry*

  ;; This variable contains a list of directories to be lazily searched for the requested asd
  ;; by sysdef-central-registry-search.
  (defvar *central-registry* nil
    "A list of 'system directory designators' ASDF uses to find systems.

A 'system directory designator' is a pathname or an expression
which evaluates to a pathname. For example:

    (setf asdf:*central-registry*
          (list '*default-pathname-defaults*
                #p\"/home/me/cl/systems/\"
                #p\"/usr/share/common-lisp/systems/\"))

This variable is for backward compatibility.
Going forward, we recommend new users should be using the source-registry.")

  ;; Function to look for an asd file of given NAME under a directory provided by DEFAULTS.
  ;; Return the truename of that file if it is found and TRUENAME is true.
  ;; Return NIL if the file is not found.
  ;; On Windows, follow shortcuts to .asd files.
  (defun probe-asd (name defaults &key truename)
    (block nil
      (when (directory-pathname-p defaults)
        (if-let (file (probe-file*
                       (ensure-absolute-pathname
                        (parse-unix-namestring name :type "asd")
                        #'(lambda () (ensure-absolute-pathname defaults 'get-pathname-defaults nil))
                        nil)
                       :truename truename))
          (return file))
        #-(or clisp genera) ; clisp doesn't need it, plain genera doesn't have read-sequence(!)
        (os-cond
         ((os-windows-p)
          (when (physical-pathname-p defaults)
            (let ((shortcut
                    (make-pathname
                     :defaults defaults :case :local
                     :name (strcat name ".asd")
                     :type "lnk")))
              (when (probe-file* shortcut)
                (ensure-pathname (parse-windows-shortcut shortcut) :namestring :native)))))))))

  ;; Function to push onto *s-d-s-f* to use the *central-registry*
  (defun sysdef-central-registry-search (system)
    (let ((name (primary-system-name system))
          (to-remove nil)
          (to-replace nil))
      (block nil
        (unwind-protect
             (dolist (dir *central-registry*)
               (let ((defaults (eval dir))
                     directorized)
                 (when defaults
                   (cond ((directory-pathname-p defaults)
                          (let* ((file (probe-asd name defaults :truename *resolve-symlinks*)))
                            (when file
                              (return file))))
                         (t
                          (restart-case
                              (let* ((*print-circle* nil)
                                     (message
                                       (format nil
                                               (compatfmt "~@<While searching for system ~S: ~3i~_~S evaluated to ~S which is not an absolute directory.~@:>")
                                               system dir defaults)))
                                (error message))
                            (remove-entry-from-registry ()
                              :report "Remove entry from *central-registry* and continue"
                              (push dir to-remove))
                            (coerce-entry-to-directory ()
                              :test (lambda (c) (declare (ignore c))
                                      (and (not (directory-pathname-p defaults))
                                           (directory-pathname-p
                                            (setf directorized
                                                  (ensure-directory-pathname defaults)))))
                              :report (lambda (s)
                                        (format s (compatfmt "~@<Coerce entry to ~a, replace ~a and continue.~@:>")
                                                directorized dir))
                              (push (cons dir directorized) to-replace))))))))
          ;; cleanup
          (dolist (dir to-remove)
            (setf *central-registry* (remove dir *central-registry*)))
          (dolist (pair to-replace)
            (let* ((current (car pair))
                   (new (cdr pair))
                   (position (position current *central-registry*)))
              (setf *central-registry*
                    (append (subseq *central-registry* 0 position)
                            (list new)
                            (subseq *central-registry* (1+ position)))))))))))

