;;;; -----------------------------------------------------------------
;;;; Source Registry Configuration, by Francois-Rene Rideau
;;;; See the Manual and https://bugs.launchpad.net/asdf/+bug/485918

(uiop/package:define-package :asdf/source-registry
  ;; NB: asdf/find-system allows upgrade from <=3.2.1 that have initialize-source-registry there
  (:recycle :asdf/source-registry :asdf/find-system :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/system :asdf/system-registry)
  (:export
   #:*source-registry-parameter* #:*default-source-registries*
   #:invalid-source-registry
   #:source-registry-initialized-p
   #:initialize-source-registry #:clear-source-registry #:*source-registry*
   #:ensure-source-registry #:*source-registry-parameter*
   #:*default-source-registry-exclusions* #:*source-registry-exclusions*
   #:*wild-asd* #:directory-asd-files #:register-asd-directory
   #:*recurse-beyond-asds* #:collect-asds-in-directory #:collect-sub*directories-asd-files
   #:validate-source-registry-directive #:validate-source-registry-form
   #:validate-source-registry-file #:validate-source-registry-directory
   #:parse-source-registry-string #:wrapping-source-registry
   #:default-user-source-registry #:default-system-source-registry
   #:user-source-registry #:system-source-registry
   #:user-source-registry-directory #:system-source-registry-directory
   #:environment-source-registry #:process-source-registry #:inherit-source-registry
   #:compute-source-registry #:flatten-source-registry
   #:sysdef-source-registry-search))
(in-package :asdf/source-registry)

(with-upgradability ()
  (define-condition invalid-source-registry (invalid-configuration warning)
    ((format :initform (compatfmt "~@<Invalid source registry ~S~@[ in ~S~]~@{ ~@?~}~@:>"))))

  ;; Default list of directories under which the source-registry tree search won't recurse
  (defvar *default-source-registry-exclusions*
    '(;;-- Using ack 1.2 exclusions
      ".bzr" ".cdv"
      ;; "~.dep" "~.dot" "~.nib" "~.plst" ; we don't support ack wildcards
      ".git" ".hg" ".pc" ".svn" "CVS" "RCS" "SCCS" "_darcs"
      "_sgbak" "autom4te.cache" "cover_db" "_build"
      ;;-- debian often builds stuff under the debian directory... BAD.
      "debian"))

  ;; Actual list of directories under which the source-registry tree search won't recurse
  (defvar *source-registry-exclusions* *default-source-registry-exclusions*)

  ;; The state of the source-registry after search in configured locations
  (defvar *source-registry* nil
    "Either NIL (for uninitialized), or an equal hash-table, mapping
system names to pathnames of .asd files")

  ;; Saving the user-provided parameter to the source-registry, if any,
  ;; so we can recompute the source-registry after code upgrade.
  (defvar *source-registry-parameter* nil)

  (defun source-registry-initialized-p ()
    (typep *source-registry* 'hash-table))

  (defun clear-source-registry ()
    "Undoes any initialization of the source registry."
    (setf *source-registry* nil)
    (values))
  (register-clear-configuration-hook 'clear-source-registry)

  (defparameter *wild-asd*
    (make-pathname :directory nil :name *wild* :type "asd" :version :newest))

  (defun directory-asd-files (directory)
    (directory-files directory *wild-asd*))

  (defun collect-asds-in-directory (directory collect)
    (let ((asds (directory-asd-files directory)))
      (map () collect asds)
      asds))

  (defvar *recurse-beyond-asds* t
    "Should :tree entries of the source-registry recurse in subdirectories
after having found a .asd file? True by default.")

  ;; When walking down a filesystem tree, if in a directory there is a .cl-source-registry.cache,
  ;; read its contents instead of further recursively querying the filesystem.
  (defun process-source-registry-cache (directory collect)
    (let ((cache (ignore-errors
                  (safe-read-file-form (subpathname directory ".cl-source-registry.cache")))))
      (when (and (listp cache) (eq :source-registry-cache (first cache)))
        (loop :for s :in (rest cache) :do (funcall collect (subpathname directory s)))
        t)))

  (defun collect-sub*directories-asd-files
      (directory &key (exclude *default-source-registry-exclusions*) collect
                   (recurse-beyond-asds *recurse-beyond-asds*) ignore-cache)
    (let ((visited (make-hash-table :test 'equalp)))
      (flet ((collectp (dir)
               (unless (and (not ignore-cache) (process-source-registry-cache dir collect))
                 (let ((asds (collect-asds-in-directory dir collect)))
                   (or recurse-beyond-asds (not asds)))))
             (recursep (x)                    ; x will be a directory pathname
               (and
                (not (member (car (last (pathname-directory x))) exclude :test #'equal))
                (flet ((pathname-key (x)
                         (namestring (truename* x))))
                  (let ((visitedp (gethash (pathname-key x) visited)))
                    (if visitedp nil
                        (setf (gethash (pathname-key x) visited) t)))))))
      (collect-sub*directories directory #'collectp #'recursep (constantly nil)))))


  ;;; Validate the configuration forms

  (defun validate-source-registry-directive (directive)
    (or (member directive '(:default-registry))
        (and (consp directive)
             (let ((rest (rest directive)))
               (case (first directive)
                 ((:include :directory :tree)
                  (and (length=n-p rest 1)
                       (location-designator-p (first rest))))
                 ((:exclude :also-exclude)
                  (every #'stringp rest))
                 ((:default-registry)
                  (null rest)))))))

  (defun validate-source-registry-form (form &key location)
    (validate-configuration-form
     form :source-registry 'validate-source-registry-directive
          :location location :invalid-form-reporter 'invalid-source-registry))

  (defun validate-source-registry-file (file)
    (validate-configuration-file
     file 'validate-source-registry-form :description "a source registry"))

  (defun validate-source-registry-directory (directory)
    (validate-configuration-directory
     directory :source-registry 'validate-source-registry-directive
               :invalid-form-reporter 'invalid-source-registry))


  ;;; Parse the configuration string

  (defun parse-source-registry-string (string &key location)
    (cond
      ((or (null string) (equal string ""))
       '(:source-registry :inherit-configuration))
      ((not (stringp string))
       (error (compatfmt "~@<Environment string isn't: ~3i~_~S~@:>") string))
      ((find (char string 0) "\"(")
       (validate-source-registry-form (read-from-string string) :location location))
      (t
       (loop
         :with inherit = nil
         :with directives = ()
         :with start = 0
         :with end = (length string)
         :with separator = (inter-directory-separator)
         :for pos = (position separator string :start start) :do
           (let ((s (subseq string start (or pos end))))
             (flet ((check (dir)
                      (unless (absolute-pathname-p dir)
                        (error (compatfmt "~@<source-registry string must specify absolute pathnames: ~3i~_~S~@:>") string))
                      dir))
               (cond
                 ((equal "" s) ; empty element: inherit
                  (when inherit
                    (error (compatfmt "~@<Only one inherited configuration allowed: ~3i~_~S~@:>")
                           string))
                  (setf inherit t)
                  (push ':inherit-configuration directives))
                 ((string-suffix-p s "//") ;; TODO: allow for doubling of separator even outside Unix?
                  (push `(:tree ,(check (subseq s 0 (- (length s) 2)))) directives))
                 (t
                  (push `(:directory ,(check s)) directives))))
             (cond
               (pos
                (setf start (1+ pos)))
               (t
                (unless inherit
                  (push '(:ignore-inherited-configuration) directives))
                (return `(:source-registry ,@(nreverse directives))))))))))

  (defun register-asd-directory (directory &key recurse exclude collect)
    (if (not recurse)
        (collect-asds-in-directory directory collect)
        (collect-sub*directories-asd-files
         directory :exclude exclude :collect collect)))

  (defparameter* *default-source-registries*
    '(environment-source-registry
      user-source-registry
      user-source-registry-directory
      default-user-source-registry
      system-source-registry
      system-source-registry-directory
      default-system-source-registry)
    "List of default source registries" "3.1.0.102")

  (defparameter *source-registry-file* (parse-unix-namestring "common-lisp/source-registry.conf"))
  (defparameter *source-registry-directory* (parse-unix-namestring "common-lisp/source-registry.conf.d/"))

  (defun wrapping-source-registry ()
    `(:source-registry
      #+(or clasp ecl sbcl) (:tree ,(resolve-symlinks* (lisp-implementation-directory)))
      :inherit-configuration
      #+mkcl (:tree ,(translate-logical-pathname "SYS:"))
      #+cmucl (:tree #p"modules:")
      #+scl (:tree #p"file://modules/")))
  (defun default-user-source-registry ()
    `(:source-registry
      (:tree (:home "common-lisp/"))
      #+sbcl (:directory (:home ".sbcl/systems/"))
      (:directory ,(xdg-data-home "common-lisp/systems/"))
      (:tree ,(xdg-data-home "common-lisp/source/"))
      :inherit-configuration))
  (defun default-system-source-registry ()
    `(:source-registry
      ,@(loop :for dir :in (xdg-data-dirs "common-lisp/")
              :collect `(:directory (,dir "systems/"))
              :collect `(:tree (,dir "source/")))
      :inherit-configuration))
  (defun user-source-registry (&key (direction :input))
    (xdg-config-pathname *source-registry-file* direction))
  (defun system-source-registry (&key (direction :input))
    (find-preferred-file (system-config-pathnames *source-registry-file*)
                         :direction direction))
  (defun user-source-registry-directory (&key (direction :input))
    (xdg-config-pathname *source-registry-directory* direction))
  (defun system-source-registry-directory (&key (direction :input))
    (find-preferred-file (system-config-pathnames *source-registry-directory*)
                         :direction direction))
  (defun environment-source-registry ()
    (getenv "CL_SOURCE_REGISTRY"))


  ;;; Process the source-registry configuration

  (defgeneric process-source-registry (spec &key inherit register))

  (defun* (inherit-source-registry) (inherit &key register)
    (when inherit
      (process-source-registry (first inherit) :register register :inherit (rest inherit))))

  (defun* (process-source-registry-directive) (directive &key inherit register)
    (destructuring-bind (kw &rest rest) (if (consp directive) directive (list directive))
      (ecase kw
        ((:include)
         (destructuring-bind (pathname) rest
           (process-source-registry (resolve-location pathname) :inherit nil :register register)))
        ((:directory)
         (destructuring-bind (pathname) rest
           (when pathname
             (funcall register (resolve-location pathname :ensure-directory t)))))
        ((:tree)
         (destructuring-bind (pathname) rest
           (when pathname
             (funcall register (resolve-location pathname :ensure-directory t)
                      :recurse t :exclude *source-registry-exclusions*))))
        ((:exclude)
         (setf *source-registry-exclusions* rest))
        ((:also-exclude)
         (appendf *source-registry-exclusions* rest))
        ((:default-registry)
         (inherit-source-registry
          '(default-user-source-registry default-system-source-registry) :register register))
        ((:inherit-configuration)
         (inherit-source-registry inherit :register register))
        ((:ignore-inherited-configuration)
         nil)))
    nil)

  (defmethod process-source-registry ((x symbol) &key inherit register)
    (process-source-registry (funcall x) :inherit inherit :register register))
  (defmethod process-source-registry ((pathname pathname) &key inherit register)
    (cond
      ((directory-pathname-p pathname)
       (let ((*here-directory* (resolve-symlinks* pathname)))
         (process-source-registry (validate-source-registry-directory pathname)
                                  :inherit inherit :register register)))
      ((probe-file* pathname :truename *resolve-symlinks*)
       (let ((*here-directory* (pathname-directory-pathname pathname)))
         (process-source-registry (validate-source-registry-file pathname)
                                  :inherit inherit :register register)))
      (t
       (inherit-source-registry inherit :register register))))
  (defmethod process-source-registry ((string string) &key inherit register)
    (process-source-registry (parse-source-registry-string string)
                             :inherit inherit :register register))
  (defmethod process-source-registry ((x null) &key inherit register)
    (inherit-source-registry inherit :register register))
  (defmethod process-source-registry ((form cons) &key inherit register)
    (let ((*source-registry-exclusions* *default-source-registry-exclusions*))
      (dolist (directive (cdr (validate-source-registry-form form)))
        (process-source-registry-directive directive :inherit inherit :register register))))


  ;; Flatten the user-provided configuration into an ordered list of directories and trees
  (defun flatten-source-registry (&optional (parameter *source-registry-parameter*))
    (remove-duplicates
     (while-collecting (collect)
       (with-pathname-defaults () ;; be location-independent
         (inherit-source-registry
          `(wrapping-source-registry
            ,parameter
            ,@*default-source-registries*)
          :register #'(lambda (directory &key recurse exclude)
                        (collect (list directory :recurse recurse :exclude exclude))))))
     :test 'equal :from-end t))

  ;; MAYBE: move this utility function to uiop/pathname and export it?
  (defun pathname-directory-depth (p)
    (length (normalize-pathname-directory-component (pathname-directory p))))

  (defun preferred-source-path-p (x y)
    "Return T iff X is to be preferred over Y as a source path"
    (let ((lx (pathname-directory-depth x))
          (ly (pathname-directory-depth y)))
      (or (< lx ly)
          (and (= lx ly)
               (string< (namestring x)
                        (namestring y))))))

  ;; Will read the configuration and initialize all internal variables.
  (defun compute-source-registry (&optional (parameter *source-registry-parameter*)
                                    (registry *source-registry*))
    (dolist (entry (flatten-source-registry parameter))
      (destructuring-bind (directory &key recurse exclude) entry
        (let* ((h (make-hash-table :test 'equal))) ; table to detect duplicates
          (register-asd-directory
           directory :recurse recurse :exclude exclude :collect
           #'(lambda (asd)
               (let* ((name (pathname-name asd))
                      (name (if (typep asd 'logical-pathname)
                                ;; logical pathnames are upper-case,
                                ;; at least in the CLHS and on SBCL,
                                ;; yet (coerce-name :foo) is lower-case.
                                ;; won't work well with (load-system "Foo")
                                ;; instead of (load-system 'foo)
                                (string-downcase name)
                                name)))
                 (unless (gethash name registry) ; already shadowed by something else
                   (if-let (old (gethash name h))
                     ;; If the name appears multiple times,
                     ;; prefer the one with the shallowest directory,
                     ;; or if they have same depth, compare unix-namestring with string<
                     (multiple-value-bind (better worse)
                         (if (preferred-source-path-p asd old)
                             (progn (setf (gethash name h) asd) (values asd old))
                             (values old asd))
                       (when *verbose-out*
                         (warn (compatfmt "~@<In source-registry entry ~A~@[/~*~] ~
                                              found several entries for ~A - picking ~S over ~S~:>")
                               directory recurse name better worse)))
                     (setf (gethash name h) asd))))))
          (maphash #'(lambda (k v) (setf (gethash k registry) v)) h))))
    (values))

  (defun initialize-source-registry (&optional (parameter *source-registry-parameter*))
    ;; Record the parameter used to configure the registry
    (setf *source-registry-parameter* parameter)
    ;; Clear the previous registry database:
    (setf *source-registry* (make-hash-table :test 'equal))
    ;; Do it!
    (compute-source-registry parameter))

  ;; Checks an initial variable to see whether the state is initialized
  ;; or cleared. In the former case, return current configuration; in
  ;; the latter, initialize.  ASDF will call this function at the start
  ;; of (asdf:find-system) to make sure the source registry is initialized.
  ;; However, it will do so *without* a parameter, at which point it
  ;; will be too late to provide a parameter to this function, though
  ;; you may override the configuration explicitly by calling
  ;; initialize-source-registry directly with your parameter.
  (defun ensure-source-registry (&optional parameter)
    (unless (source-registry-initialized-p)
      (initialize-source-registry parameter))
    (values))

  (defun sysdef-source-registry-search (system)
    (ensure-source-registry)
    (values (gethash (primary-system-name system) *source-registry*))))


