;;;; -------------------------------------------------------------------------
;;;; Portability layer around Common Lisp filesystem access

(uiop/package:define-package :uiop/filesystem
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/os :uiop/pathname)
  (:export
   ;; Native namestrings
   #:native-namestring #:parse-native-namestring
   ;; Probing the filesystem
   #:truename* #:safe-file-write-date #:probe-file* #:directory-exists-p #:file-exists-p
   #:directory* #:filter-logical-directory-results #:directory-files #:subdirectories
   #:collect-sub*directories
   ;; Resolving symlinks somewhat
   #:truenamize #:resolve-symlinks #:*resolve-symlinks* #:resolve-symlinks*
   ;; merging with cwd
   #:get-pathname-defaults #:call-with-current-directory #:with-current-directory
   ;; Environment pathnames
   #:inter-directory-separator #:split-native-pathnames-string
   #:getenv-pathname #:getenv-pathnames
   #:getenv-absolute-directory #:getenv-absolute-directories
   #:lisp-implementation-directory #:lisp-implementation-pathname-p
   ;; Simple filesystem operations
   #:ensure-all-directories-exist
   #:rename-file-overwriting-target
   #:delete-file-if-exists #:delete-empty-directory #:delete-directory-tree))
(in-package :uiop/filesystem)

;;; Native namestrings, as seen by the operating system calls rather than Lisp
(with-upgradability ()
  (defun native-namestring (x)
    "From a non-wildcard CL pathname, a return namestring suitable for passing to the operating system"
    (when x
      (let ((p (pathname x)))
        #+clozure (with-pathname-defaults () (ccl:native-translated-namestring p)) ; see ccl bug 978
        #+(or cmucl scl) (ext:unix-namestring p nil)
        #+sbcl (sb-ext:native-namestring p)
        #-(or clozure cmucl sbcl scl)
        (os-cond
         ((os-unix-p) (unix-namestring p))
         (t (namestring p))))))

  (defun parse-native-namestring (string &rest constraints &key ensure-directory &allow-other-keys)
    "From a native namestring suitable for use by the operating system, return
a CL pathname satisfying all the specified constraints as per ENSURE-PATHNAME"
    (check-type string (or string null))
    (let* ((pathname
             (when string
               (with-pathname-defaults ()
                 #+clozure (ccl:native-to-pathname string)
                 #+cmucl (uiop/os::parse-unix-namestring* string)
                 #+sbcl (sb-ext:parse-native-namestring string)
                 #+scl (lisp::parse-unix-namestring string)
                 #-(or clozure cmucl sbcl scl)
                 (os-cond
                  ((os-unix-p) (parse-unix-namestring string :ensure-directory ensure-directory))
                  (t (parse-namestring string))))))
           (pathname
             (if ensure-directory
                 (and pathname (ensure-directory-pathname pathname))
                 pathname)))
      (apply 'ensure-pathname pathname constraints))))


;;; Probing the filesystem
(with-upgradability ()
  (defun truename* (p)
    "Nicer variant of TRUENAME that plays well with NIL, avoids logical pathname contexts, and tries both files and directories"
    (when p
      (when (stringp p) (setf p (with-pathname-defaults () (parse-namestring p))))
      (values
       (or (ignore-errors (truename p))
           ;; this is here because trying to find the truename of a directory pathname WITHOUT supplying
           ;; a trailing directory separator, causes an error on some lisps.
           #+(or clisp gcl) (if-let (d (ensure-directory-pathname p nil)) (ignore-errors (truename d)))
           ;; On Genera, truename of a directory pathname will probably fail as Genera
           ;; will merge in a filename/type/version from *default-pathname-defaults* and
           ;; will try to get the truename of a file that probably doesn't exist.
           #+genera (when (directory-pathname-p p)
                      (let ((d (scl:send p :directory-pathname-as-file)))
                        (ensure-directory-pathname (ignore-errors (truename d)) nil)))))))

  (defun safe-file-write-date (pathname)
    "Safe variant of FILE-WRITE-DATE that may return NIL rather than raise an error."
    ;; If FILE-WRITE-DATE returns NIL, it's possible that
    ;; the user or some other agent has deleted an input file.
    ;; Also, generated files will not exist at the time planning is done
    ;; and calls compute-action-stamp which calls safe-file-write-date.
    ;; So it is very possible that we can't get a valid file-write-date,
    ;; and we can survive and we will continue the planning
    ;; as if the file were very old.
    ;; (or should we treat the case in a different, special way?)
    (and pathname
         (handler-case (file-write-date (physicalize-pathname pathname))
           (file-error () nil))))

  (defun probe-file* (p &key truename)
    "when given a pathname P (designated by a string as per PARSE-NAMESTRING),
probes the filesystem for a file or directory with given pathname.
If it exists, return its truename if TRUENAME is true,
or the original (parsed) pathname if it is false (the default)."
    (values
     (ignore-errors
      (setf p (funcall 'ensure-pathname p
                       :namestring :lisp
                       :ensure-physical t
                       :ensure-absolute t :defaults 'get-pathname-defaults
                       :want-non-wild t
                       :on-error nil))
      (when p
        #+allegro
        (probe-file p :follow-symlinks truename)
        #+gcl
        (if truename
            (truename* p)
            (let ((kind (car (si::stat p))))
              (when (eq kind :link)
                (setf kind (ignore-errors (car (si::stat (truename* p))))))
              (ecase kind
                ((nil) nil)
                ((:file :link)
                 (cond
                   ((file-pathname-p p) p)
                   ((directory-pathname-p p)
                    (subpathname p (car (last (pathname-directory p)))))))
                (:directory (ensure-directory-pathname p)))))
        #+clisp
        #.(let* ((fs (or #-os-windows (find-symbol* '#:file-stat :posix nil)))
                 (pp (find-symbol* '#:probe-pathname :ext nil)))
            `(if truename
                 ,(if pp
                      `(values (,pp p))
                      '(or (truename* p)
                        (truename* (ignore-errors (ensure-directory-pathname p)))))
                 ,(cond
                    (fs `(and (,fs p) p))
                    (pp `(nth-value 1 (,pp p)))
                    (t '(or (and (truename* p) p)
                         (if-let (d (ensure-directory-pathname p))
                          (and (truename* d) d)))))))
        #-(or allegro clisp gcl)
        (if truename
            (probe-file p)
            (and
             #+(or cmucl scl) (unix:unix-stat (ext:unix-namestring p))
             #+(and lispworks os-unix) (system:get-file-stat p)
             #+sbcl (sb-unix:unix-stat (sb-ext:native-namestring p))
             #-(or cmucl (and lispworks os-unix) sbcl scl) (file-write-date p)
             p))))))

  (defun directory-exists-p (x)
    "Is X the name of a directory that exists on the filesystem?"
    #+allegro
    (excl:probe-directory x)
    #+clisp
    (handler-case (ext:probe-directory x)
           (sys::simple-file-error ()
             nil))
    #-(or allegro clisp)
    (let ((p (probe-file* x :truename t)))
      (and (directory-pathname-p p) p)))

  (defun file-exists-p (x)
    "Is X the name of a file that exists on the filesystem?"
    (let ((p (probe-file* x :truename t)))
      (and (file-pathname-p p) p)))

  (defun directory* (pathname-spec &rest keys &key &allow-other-keys)
    "Return a list of the entries in a directory by calling DIRECTORY.
Try to override the defaults to not resolving symlinks, if implementation allows."
    (apply 'directory pathname-spec
           (append keys '#.(or #+allegro '(:directories-are-files nil :follow-symbolic-links nil)
                               #+(or clozure digitool) '(:follow-links nil)
                               #+clisp '(:circle t :if-does-not-exist :ignore)
                               #+(or cmucl scl) '(:follow-links nil :truenamep nil)
                               #+lispworks '(:link-transparency nil)
                               #+sbcl (when (find-symbol* :resolve-symlinks '#:sb-impl nil)
                                        '(:resolve-symlinks nil))))))

  (defun filter-logical-directory-results (directory entries merger)
    "If DIRECTORY isn't a logical pathname, return ENTRIES. If it is,
given ENTRIES in the DIRECTORY, remove the entries which are physical yet
when transformed by MERGER have a different TRUENAME.
Also remove duplicates as may appear with some translation rules.
This function is used as a helper to DIRECTORY-FILES to avoid invalid entries
when using logical-pathnames."
    (if (logical-pathname-p directory)
        (remove-duplicates ;; on CLISP, querying ~/ will return duplicates
         ;; Try hard to not resolve logical-pathname into physical pathnames;
         ;; otherwise logical-pathname users/lovers will be disappointed.
         ;; If directory* could use some implementation-dependent magic,
         ;; we will have logical pathnames already; otherwise,
         ;; we only keep pathnames for which specifying the name and
         ;; translating the LPN commute.
         (loop :for f :in entries
               :for p = (or (and (logical-pathname-p f) f)
                            (let* ((u (ignore-errors (call-function merger f))))
                              ;; The first u avoids a cumbersome (truename u) error.
                              ;; At this point f should already be a truename,
                              ;; but isn't quite in CLISP, for it doesn't have :version :newest
                              (and u (equal (truename* u) (truename* f)) u)))
           :when p :collect p)
         :test 'pathname-equal)
        entries))

  (defun directory-files (directory &optional (pattern *wild-file-for-directory*))
    "Return a list of the files in a directory according to the PATTERN.
Subdirectories should NOT be returned.
  PATTERN defaults to a pattern carefully chosen based on the implementation;
override the default at your own risk.
  DIRECTORY-FILES tries NOT to resolve symlinks if the implementation permits this,
but the behavior in presence of symlinks is not portable. Use IOlib to handle such situations."
    (let ((dir (ensure-directory-pathname directory)))
      (when (logical-pathname-p dir)
        ;; Because of the filtering we do below,
        ;; logical pathnames have restrictions on wild patterns.
        ;; Not that the results are very portable when you use these patterns on physical pathnames.
        (when (wild-pathname-p dir)
          (parameter-error "~S: Invalid wild pattern in logical directory ~S"
                           'directory-files directory))
        (unless (member (pathname-directory pattern) '(() (:relative)) :test 'equal)
          (parameter-error "~S: Invalid file pattern ~S for logical directory ~S" 'directory-files pattern directory))
        (setf pattern (make-pathname-logical pattern (pathname-host dir))))
      (let* ((pat (merge-pathnames* pattern dir))
             (entries (ignore-errors (directory* pat))))
        (remove-if 'directory-pathname-p
                   (filter-logical-directory-results
                    directory entries
                    #'(lambda (f)
                        (make-pathname :defaults dir
                                       :name (make-pathname-component-logical (pathname-name f))
                                       :type (make-pathname-component-logical (pathname-type f))
                                       :version (make-pathname-component-logical (pathname-version f)))))))))

  (defun subdirectories (directory)
    "Given a DIRECTORY pathname designator, return a list of the subdirectories under it.
The behavior in presence of symlinks is not portable. Use IOlib to handle such situations."
    (let* ((directory (ensure-directory-pathname directory))
           #-(or abcl cormanlisp genera xcl)
           (wild (merge-pathnames*
                  #-(or abcl allegro cmucl lispworks sbcl scl xcl)
                  *wild-directory*
                  #+(or abcl allegro cmucl lispworks sbcl scl xcl) "*.*"
                  directory))
           (dirs
             #-(or abcl cormanlisp genera xcl)
             (ignore-errors
              (directory* wild . #.(or #+clozure '(:directories t :files nil)
                                       #+mcl '(:directories t))))
             #+(or abcl xcl) (system:list-directory directory)
             #+cormanlisp (cl::directory-subdirs directory)
             #+genera (handler-case (fs:directory-list directory) (fs:directory-not-found () nil)))
           #+(or abcl allegro cmucl genera lispworks sbcl scl xcl)
           (dirs (loop :for x :in dirs
                       :for d = #+(or abcl xcl) (extensions:probe-directory x)
                       #+allegro (excl:probe-directory x)
                       #+(or cmucl sbcl scl) (directory-pathname-p x)
                       #+genera (getf (cdr x) :directory)
                       #+lispworks (lw:file-directory-p x)
                       :when d :collect #+(or abcl allegro xcl) (ensure-directory-pathname d)
                         #+genera (ensure-directory-pathname (first x))
                       #+(or cmucl lispworks sbcl scl) x)))
      (filter-logical-directory-results
       directory dirs
       (let ((prefix (or (normalize-pathname-directory-component (pathname-directory directory))
                         '(:absolute)))) ; because allegro returns NIL for #p"FOO:"
         #'(lambda (d)
             (let ((dir (normalize-pathname-directory-component (pathname-directory d))))
               (and (consp dir) (consp (cdr dir))
                    (make-pathname
                     :defaults directory :name nil :type nil :version nil
                     :directory (append prefix (make-pathname-component-logical (last dir)))))))))))

  (defun collect-sub*directories (directory collectp recursep collector)
    "Given a DIRECTORY, when COLLECTP returns true when CALL-FUNCTION'ed with the directory,
call-function the COLLECTOR function designator on the directory,
and recurse each of its subdirectories on which the RECURSEP returns true when CALL-FUNCTION'ed with them.
This function will thus let you traverse a filesystem hierarchy,
superseding the functionality of CL-FAD:WALK-DIRECTORY.
The behavior in presence of symlinks is not portable. Use IOlib to handle such situations."
    (when (call-function collectp directory)
      (call-function collector directory)
      (dolist (subdir (subdirectories directory))
        (when (call-function recursep subdir)
          (collect-sub*directories subdir collectp recursep collector))))))

;;; Resolving symlinks somewhat
(with-upgradability ()
  (defun truenamize (pathname)
    "Resolve as much of a pathname as possible"
    (block nil
      (when (typep pathname '(or null logical-pathname)) (return pathname))
      (let ((p pathname))
        (unless (absolute-pathname-p p)
          (setf p (or (absolute-pathname-p (ensure-absolute-pathname p 'get-pathname-defaults nil))
                      (return p))))
        (when (logical-pathname-p p) (return p))
        (let ((found (probe-file* p :truename t)))
          (when found (return found)))
        (let* ((directory (normalize-pathname-directory-component (pathname-directory p)))
               (up-components (reverse (rest directory)))
               (down-components ()))
          (assert (eq :absolute (first directory)))
          (loop :while up-components :do
            (if-let (parent
                     (ignore-errors
                      (probe-file* (make-pathname :directory `(:absolute ,@(reverse up-components))
                                                  :name nil :type nil :version nil :defaults p))))
              (if-let (simplified
                       (ignore-errors
                        (merge-pathnames*
                         (make-pathname :directory `(:relative ,@down-components)
                                        :defaults p)
                         (ensure-directory-pathname parent))))
                (return simplified)))
            (push (pop up-components) down-components)
            :finally (return p))))))

  (defun resolve-symlinks (path)
    "Do a best effort at resolving symlinks in PATH, returning a partially or totally resolved PATH."
    #-allegro (truenamize path)
    #+allegro
    (if (physical-pathname-p path)
        (or (ignore-errors (excl:pathname-resolve-symbolic-links path)) path)
        path))

  (defvar *resolve-symlinks* t
    "Determine whether or not ASDF resolves symlinks when defining systems.
Defaults to T.")

  (defun resolve-symlinks* (path)
    "RESOLVE-SYMLINKS in PATH iff *RESOLVE-SYMLINKS* is T (the default)."
    (if *resolve-symlinks*
        (and path (resolve-symlinks path))
        path)))


;;; Check pathname constraints
(with-upgradability ()
  (defun ensure-pathname
      (pathname &key
                  on-error
                  defaults type dot-dot namestring
                  empty-is-nil
                  want-pathname
                  want-logical want-physical ensure-physical
                  want-relative want-absolute ensure-absolute ensure-subpath
                  want-non-wild want-wild wilden
                  want-file want-directory ensure-directory
                  want-existing ensure-directories-exist
                  truename resolve-symlinks truenamize
       &aux (p pathname)) ;; mutable working copy, preserve original
    "Coerces its argument into a PATHNAME,
optionally doing some transformations and checking specified constraints.

If the argument is NIL, then NIL is returned unless the WANT-PATHNAME constraint is specified.

If the argument is a STRING, it is first converted to a pathname via
PARSE-UNIX-NAMESTRING, PARSE-NAMESTRING or PARSE-NATIVE-NAMESTRING respectively
depending on the NAMESTRING argument being :UNIX, :LISP or :NATIVE respectively,
or else by using CALL-FUNCTION on the NAMESTRING argument;
if :UNIX is specified (or NIL, the default, which specifies the same thing),
then PARSE-UNIX-NAMESTRING it is called with the keywords
DEFAULTS TYPE DOT-DOT ENSURE-DIRECTORY WANT-RELATIVE, and
the result is optionally merged into the DEFAULTS if ENSURE-ABSOLUTE is true.

The pathname passed or resulting from parsing the string
is then subjected to all the checks and transformations below are run.

Each non-nil constraint argument can be one of the symbols T, ERROR, CERROR or IGNORE.
The boolean T is an alias for ERROR.
ERROR means that an error will be raised if the constraint is not satisfied.
CERROR means that an continuable error will be raised if the constraint is not satisfied.
IGNORE means just return NIL instead of the pathname.

The ON-ERROR argument, if not NIL, is a function designator (as per CALL-FUNCTION)
that will be called with the the following arguments:
a generic format string for ensure pathname, the pathname,
the keyword argument corresponding to the failed check or transformation,
a format string for the reason ENSURE-PATHNAME failed,
and a list with arguments to that format string.
If ON-ERROR is NIL, ERROR is used instead, which does the right thing.
You could also pass (CERROR \"CONTINUE DESPITE FAILED CHECK\").

The transformations and constraint checks are done in this order,
which is also the order in the lambda-list:

EMPTY-IS-NIL returns NIL if the argument is an empty string.
WANT-PATHNAME checks that pathname (after parsing if needed) is not null.
Otherwise, if the pathname is NIL, ensure-pathname returns NIL.
WANT-LOGICAL checks that pathname is a LOGICAL-PATHNAME
WANT-PHYSICAL checks that pathname is not a LOGICAL-PATHNAME
ENSURE-PHYSICAL ensures that pathname is physical via TRANSLATE-LOGICAL-PATHNAME
WANT-RELATIVE checks that pathname has a relative directory component
WANT-ABSOLUTE checks that pathname does have an absolute directory component
ENSURE-ABSOLUTE merges with the DEFAULTS, then checks again
that the result absolute is an absolute pathname indeed.
ENSURE-SUBPATH checks that the pathname is a subpath of the DEFAULTS.
WANT-FILE checks that pathname has a non-nil FILE component
WANT-DIRECTORY checks that pathname has nil FILE and TYPE components
ENSURE-DIRECTORY uses ENSURE-DIRECTORY-PATHNAME to interpret
any file and type components as being actually a last directory component.
WANT-NON-WILD checks that pathname is not a wild pathname
WANT-WILD checks that pathname is a wild pathname
WILDEN merges the pathname with **/*.*.* if it is not wild
WANT-EXISTING checks that a file (or directory) exists with that pathname.
ENSURE-DIRECTORIES-EXIST creates any parent directory with ENSURE-DIRECTORIES-EXIST.
TRUENAME replaces the pathname by its truename, or errors if not possible.
RESOLVE-SYMLINKS replaces the pathname by a variant with symlinks resolved by RESOLVE-SYMLINKS.
TRUENAMIZE uses TRUENAMIZE to resolve as many symlinks as possible."
    (block nil
      (flet ((report-error (keyword description &rest arguments)
               (call-function (or on-error 'error)
                              "Invalid pathname ~S: ~*~?"
                              pathname keyword description arguments)))
        (macrolet ((err (constraint &rest arguments)
                     `(report-error ',(intern* constraint :keyword) ,@arguments))
                   (check (constraint condition &rest arguments)
                     `(when ,constraint
                        (unless ,condition (err ,constraint ,@arguments))))
                   (transform (transform condition expr)
                     `(when ,transform
                        (,@(if condition `(when ,condition) '(progn))
                         (setf p ,expr)))))
          (etypecase p
            ((or null pathname))
            (string
             (when (and (emptyp p) empty-is-nil)
               (return-from ensure-pathname nil))
             (setf p (case namestring
                       ((:unix nil)
                        (parse-unix-namestring
                         p :defaults defaults :type type :dot-dot dot-dot
                           :ensure-directory ensure-directory :want-relative want-relative))
                       ((:native)
                        (parse-native-namestring p))
                       ((:lisp)
                        (parse-namestring p))
                       (t
                        (call-function namestring p))))))
          (etypecase p
            (pathname)
            (null
             (check want-pathname (pathnamep p) "Expected a pathname, not NIL")
             (return nil)))
          (check want-logical (logical-pathname-p p) "Expected a logical pathname")
          (check want-physical (physical-pathname-p p) "Expected a physical pathname")
          (transform ensure-physical () (physicalize-pathname p))
          (check ensure-physical (physical-pathname-p p) "Could not translate to a physical pathname")
          (check want-relative (relative-pathname-p p) "Expected a relative pathname")
          (check want-absolute (absolute-pathname-p p) "Expected an absolute pathname")
          (transform ensure-absolute (not (absolute-pathname-p p))
                     (ensure-absolute-pathname p defaults (list #'report-error :ensure-absolute "~@?")))
          (check ensure-absolute (absolute-pathname-p p)
                 "Could not make into an absolute pathname even after merging with ~S" defaults)
          (check ensure-subpath (absolute-pathname-p defaults)
                 "cannot be checked to be a subpath of non-absolute pathname ~S" defaults)
          (check ensure-subpath (subpathp p defaults) "is not a sub pathname of ~S" defaults)
          (check want-file (file-pathname-p p) "Expected a file pathname")
          (check want-directory (directory-pathname-p p) "Expected a directory pathname")
          (transform ensure-directory (not (directory-pathname-p p)) (ensure-directory-pathname p))
          (check want-non-wild (not (wild-pathname-p p)) "Expected a non-wildcard pathname")
          (check want-wild (wild-pathname-p p) "Expected a wildcard pathname")
          (transform wilden (not (wild-pathname-p p)) (wilden p))
          (when want-existing
            (let ((existing (probe-file* p :truename truename)))
              (if existing
                  (when truename
                    (return existing))
                  (err want-existing "Expected an existing pathname"))))
          (when ensure-directories-exist (ensure-directories-exist p))
          (when truename
            (let ((truename (truename* p)))
              (if truename
                  (return truename)
                  (err truename "Can't get a truename for pathname"))))
          (transform resolve-symlinks () (resolve-symlinks p))
          (transform truenamize () (truenamize p))
          p)))))


;;; Pathname defaults
(with-upgradability ()
  (defun get-pathname-defaults (&optional (defaults *default-pathname-defaults*))
    "Find the actual DEFAULTS to use for pathnames, including
resolving them with respect to GETCWD if the DEFAULTS were relative"
    (or (absolute-pathname-p defaults)
        (merge-pathnames* defaults (getcwd))))

  (defun call-with-current-directory (dir thunk)
    "call the THUNK in a context where the current directory was changed to DIR, if not NIL.
Note that this operation is usually NOT thread-safe."
    (if dir
        (let* ((dir (resolve-symlinks*
                     (get-pathname-defaults
                      (ensure-directory-pathname
                       dir))))
               (cwd (getcwd))
               (*default-pathname-defaults* dir))
          (chdir dir)
          (unwind-protect
               (funcall thunk)
            (chdir cwd)))
        (funcall thunk)))

  (defmacro with-current-directory ((&optional dir) &body body)
    "Call BODY while the POSIX current working directory is set to DIR"
    `(call-with-current-directory ,dir #'(lambda () ,@body))))


;;; Environment pathnames
(with-upgradability ()
  (defun inter-directory-separator ()
    "What character does the current OS conventionally uses to separate directories?"
    (os-cond ((os-unix-p) #\:) (t #\;)))

  (defun split-native-pathnames-string (string &rest constraints &key &allow-other-keys)
    "Given a string of pathnames specified in native OS syntax, separate them in a list,
check constraints and normalize each one as per ENSURE-PATHNAME,
where an empty string denotes NIL."
    (loop :for namestring :in (split-string string :separator (string (inter-directory-separator)))
          :collect (unless (emptyp namestring) (apply 'parse-native-namestring namestring constraints))))

  (defun getenv-pathname (x &rest constraints &key ensure-directory want-directory on-error &allow-other-keys)
    "Extract a pathname from a user-configured environment variable, as per native OS,
check constraints and normalize as per ENSURE-PATHNAME."
    ;; For backward compatibility with ASDF 2, want-directory implies ensure-directory
    (apply 'parse-native-namestring (getenvp x)
           :ensure-directory (or ensure-directory want-directory)
           :on-error (or on-error
                         `(error "In (~S ~S), invalid pathname ~*~S: ~*~?" getenv-pathname ,x))
           constraints))
  (defun getenv-pathnames (x &rest constraints &key on-error &allow-other-keys)
    "Extract a list of pathname from a user-configured environment variable, as per native OS,
check constraints and normalize each one as per ENSURE-PATHNAME.
       Any empty entries in the environment variable X will be returned as NILs."
    (unless (getf constraints :empty-is-nil t)
      (parameter-error "Cannot have EMPTY-IS-NIL false for ~S" 'getenv-pathnames))
    (apply 'split-native-pathnames-string (getenvp x)
           :on-error (or on-error
                         `(error "In (~S ~S), invalid pathname ~*~S: ~*~?" getenv-pathnames ,x))
           :empty-is-nil t
           constraints))
  (defun getenv-absolute-directory (x)
    "Extract an absolute directory pathname from a user-configured environment variable,
as per native OS"
    (getenv-pathname x :want-absolute t :ensure-directory t))
  (defun getenv-absolute-directories (x)
    "Extract a list of absolute directories from a user-configured environment variable,
as per native OS.  Any empty entries in the environment variable X will be returned as
NILs."
    (getenv-pathnames x :want-absolute t :ensure-directory t))

  (defun lisp-implementation-directory (&key truename)
    "Where are the system files of the current installation of the CL implementation?"
    (declare (ignorable truename))
    (let ((dir
            #+abcl extensions:*lisp-home*
            #+(or allegro clasp ecl mkcl) #p"SYS:"
            #+clisp custom:*lib-directory*
            #+clozure #p"ccl:"
            #+cmucl (ignore-errors (pathname-parent-directory-pathname (truename #p"modules:")))
            #+gcl system::*system-directory*
            #+lispworks lispworks:*lispworks-directory*
            #+sbcl (if-let (it (find-symbol* :sbcl-homedir-pathname :sb-int nil))
                     (funcall it)
                     (getenv-pathname "SBCL_HOME" :ensure-directory t))
            #+scl (ignore-errors (pathname-parent-directory-pathname (truename #p"file://modules/")))
            #+xcl ext:*xcl-home*))
      (if (and dir truename)
          (truename* dir)
          dir)))

  (defun lisp-implementation-pathname-p (pathname)
    "Is the PATHNAME under the current installation of the CL implementation?"
    ;; Other builtin systems are those under the implementation directory
    (and (when pathname
           (if-let (impdir (lisp-implementation-directory))
             (or (subpathp pathname impdir)
                 (when *resolve-symlinks*
                   (if-let (truename (truename* pathname))
                     (if-let (trueimpdir (truename* impdir))
                       (subpathp truename trueimpdir)))))))
         t)))


;;; Simple filesystem operations
(with-upgradability ()
  (defun ensure-all-directories-exist (pathnames)
    "Ensure that for every pathname in PATHNAMES, we ensure its directories exist"
    (dolist (pathname pathnames)
      (when pathname
        (ensure-directories-exist (physicalize-pathname pathname)))))

  (defun delete-file-if-exists (x)
    "Delete a file X if it already exists"
    (when x (handler-case (delete-file x) (file-error () nil))))

  (defun rename-file-overwriting-target (source target)
    "Rename a file, overwriting any previous file with the TARGET name,
in an atomic way if the implementation allows."
    (let ((source (ensure-pathname source :namestring :lisp :ensure-physical t :want-file t))
          (target (ensure-pathname target :namestring :lisp :ensure-physical t :want-file t)))
      #+clisp ;; in recent enough versions of CLISP, :if-exists :overwrite would make it atomic
      (progn (funcall 'require "syscalls")
             (symbol-call :posix :copy-file source target :method :rename))
      #+(and sbcl os-windows) (delete-file-if-exists target) ;; not atomic
      #-clisp
      (rename-file source target
                   #+(or clasp clozure ecl) :if-exists
                   #+clozure :rename-and-delete #+(or clasp ecl) t)))

  (defun delete-empty-directory (directory-pathname)
    "Delete an empty directory"
    #+(or abcl digitool gcl) (delete-file directory-pathname)
    #+allegro (excl:delete-directory directory-pathname)
    #+clisp (ext:delete-directory directory-pathname)
    #+clozure (ccl::delete-empty-directory directory-pathname)
    #+(or cmucl scl) (multiple-value-bind (ok errno)
                       (unix:unix-rmdir (native-namestring directory-pathname))
                     (unless ok
                       #+cmucl (error "Error number ~A when trying to delete directory ~A"
                                    errno directory-pathname)
                       #+scl (error "~@<Error deleting ~S: ~A~@:>"
                                    directory-pathname (unix:get-unix-error-msg errno))))
    #+cormanlisp (win32:delete-directory directory-pathname)
    #+(or clasp ecl) (si:rmdir directory-pathname)
    #+genera (fs:delete-directory directory-pathname)
    #+lispworks (lw:delete-directory directory-pathname)
    #+mkcl (mkcl:rmdir directory-pathname)
    #+sbcl #.(if-let (dd (find-symbol* :delete-directory :sb-ext nil))
               `(,dd directory-pathname) ;; requires SBCL 1.0.44 or later
               `(progn (require :sb-posix) (symbol-call :sb-posix :rmdir directory-pathname)))
    #+xcl (symbol-call :uiop :run-program `("rmdir" ,(native-namestring directory-pathname)))
    #-(or abcl allegro clasp clisp clozure cmucl cormanlisp digitool ecl gcl genera lispworks mkcl sbcl scl xcl)
    (not-implemented-error 'delete-empty-directory "(on your platform)")) ; genera

  (defun delete-directory-tree (directory-pathname &key (validate nil validatep) (if-does-not-exist :error))
    "Delete a directory including all its recursive contents, aka rm -rf.

To reduce the risk of infortunate mistakes, DIRECTORY-PATHNAME must be
a physical non-wildcard directory pathname (not namestring).

If the directory does not exist, the IF-DOES-NOT-EXIST argument specifies what happens:
if it is :ERROR (the default), an error is signaled, whereas if it is :IGNORE, nothing is done.

Furthermore, before any deletion is attempted, the DIRECTORY-PATHNAME must pass
the validation function designated (as per ENSURE-FUNCTION) by the VALIDATE keyword argument
which in practice is thus compulsory, and validates by returning a non-NIL result.
If you're suicidal or extremely confident, just use :VALIDATE T."
    (check-type if-does-not-exist (member :error :ignore))
    (cond
      ((not (and (pathnamep directory-pathname) (directory-pathname-p directory-pathname)
                 (physical-pathname-p directory-pathname) (not (wild-pathname-p directory-pathname))))
       (parameter-error "~S was asked to delete ~S but it is not a physical non-wildcard directory pathname"
              'delete-directory-tree directory-pathname))
      ((not validatep)
       (parameter-error "~S was asked to delete ~S but was not provided a validation predicate"
              'delete-directory-tree directory-pathname))
      ((not (call-function validate directory-pathname))
       (parameter-error "~S was asked to delete ~S but it is not valid ~@[according to ~S~]"
              'delete-directory-tree directory-pathname validate))
      ((not (directory-exists-p directory-pathname))
       (ecase if-does-not-exist
         (:error
          (error "~S was asked to delete ~S but the directory does not exist"
              'delete-directory-tree directory-pathname))
         (:ignore nil)))
      #-(or allegro cmucl clozure genera sbcl scl)
      ((os-unix-p) ;; On Unix, don't recursively walk the directory and delete everything in Lisp,
       ;; except on implementations where we can prevent DIRECTORY from following symlinks;
       ;; instead spawn a standard external program to do the dirty work.
       (symbol-call :uiop :run-program `("rm" "-rf" ,(native-namestring directory-pathname))))
      (t
       ;; On supported implementation, call supported system functions
       #+allegro (symbol-call :excl.osi :delete-directory-and-files
                              directory-pathname :if-does-not-exist if-does-not-exist)
       #+clozure (ccl:delete-directory directory-pathname)
       #+genera (fs:delete-directory directory-pathname :confirm nil)
       #+sbcl #.(if-let (dd (find-symbol* :delete-directory :sb-ext nil))
                  `(,dd directory-pathname :recursive t) ;; requires SBCL 1.0.44 or later
                  '(error "~S requires SBCL 1.0.44 or later" 'delete-directory-tree))
       ;; Outside Unix or on CMUCL and SCL that can avoid following symlinks,
       ;; do things the hard way.
       #-(or allegro clozure genera sbcl)
       (let ((sub*directories
               (while-collecting (c)
                 (collect-sub*directories directory-pathname t t #'c))))
             (dolist (d (nreverse sub*directories))
               (map () 'delete-file (directory-files d))
               (delete-empty-directory d)))))))
