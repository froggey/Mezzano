;;;; -------------------------------------------------------------------------
;;;; Portability layer around Common Lisp pathnames
;; This layer allows for portable manipulation of pathname objects themselves,
;; which all is necessary prior to any access the filesystem or environment.

(uiop/package:define-package :uiop/pathname
  (:nicknames :asdf/pathname) ;; deprecated. Used by ceramic
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/os)
  (:export
   ;; Making and merging pathnames, portably
   #:normalize-pathname-directory-component #:denormalize-pathname-directory-component
   #:merge-pathname-directory-components #:*unspecific-pathname-type* #:make-pathname*
   #:make-pathname-component-logical #:make-pathname-logical
   #:merge-pathnames*
   #:nil-pathname #:*nil-pathname* #:with-pathname-defaults
   ;; Predicates
   #:pathname-equal #:logical-pathname-p #:physical-pathname-p #:physicalize-pathname
   #:absolute-pathname-p #:relative-pathname-p #:hidden-pathname-p #:file-pathname-p
   ;; Directories
   #:pathname-directory-pathname #:pathname-parent-directory-pathname
   #:directory-pathname-p #:ensure-directory-pathname
   ;; Parsing filenames
   #:split-name-type #:parse-unix-namestring #:unix-namestring
   #:split-unix-namestring-directory-components
   ;; Absolute and relative pathnames
   #:subpathname #:subpathname*
   #:ensure-absolute-pathname
   #:pathname-root #:pathname-host-pathname
   #:subpathp #:enough-pathname #:with-enough-pathname #:call-with-enough-pathname
   ;; Checking constraints
   #:ensure-pathname ;; implemented in filesystem.lisp to accommodate for existence constraints
   ;; Wildcard pathnames
   #:*wild* #:*wild-file* #:*wild-file-for-directory* #:*wild-directory*
   #:*wild-inferiors* #:*wild-path* #:wilden
   ;; Translate a pathname
   #:relativize-directory-component #:relativize-pathname-directory
   #:directory-separator-for-host #:directorize-pathname-host-device
   #:translate-pathname*
   #:*output-translation-function*))
(in-package :uiop/pathname)

;;; Normalizing pathnames across implementations

(with-upgradability ()
  (defun normalize-pathname-directory-component (directory)
    "Convert the DIRECTORY component from a format usable by the underlying
implementation's MAKE-PATHNAME and other primitives to a CLHS-standard format
that is a list and not a string."
    (cond
      #-(or cmucl sbcl scl) ;; these implementations already normalize directory components.
      ((stringp directory) `(:absolute ,directory))
      ((or (null directory)
           (and (consp directory) (member (first directory) '(:absolute :relative))))
       directory)
      #+gcl
      ((consp directory)
       (cons :relative directory))
      (t
       (parameter-error (compatfmt "~@<~S: Unrecognized pathname directory component ~S~@:>")
                        'normalize-pathname-directory-component directory))))

  (defun denormalize-pathname-directory-component (directory-component)
    "Convert the DIRECTORY-COMPONENT from a CLHS-standard format to a format usable
by the underlying implementation's MAKE-PATHNAME and other primitives"
    directory-component)

  (defun merge-pathname-directory-components (specified defaults)
    "Helper for MERGE-PATHNAMES* that handles directory components"
    (let ((directory (normalize-pathname-directory-component specified)))
      (ecase (first directory)
        ((nil) defaults)
        (:absolute specified)
        (:relative
         (let ((defdir (normalize-pathname-directory-component defaults))
               (reldir (cdr directory)))
           (cond
             ((null defdir)
              directory)
             ((not (eq :back (first reldir)))
              (append defdir reldir))
             (t
              (loop :with defabs = (first defdir)
                    :with defrev = (reverse (rest defdir))
                    :while (and (eq :back (car reldir))
                                (or (and (eq :absolute defabs) (null defrev))
                                    (stringp (car defrev))))
                    :do (pop reldir) (pop defrev)
                    :finally (return (cons defabs (append (reverse defrev) reldir)))))))))))

  ;; Giving :unspecific as :type argument to make-pathname is not portable.
  ;; See CLHS make-pathname and 19.2.2.2.3.
  ;; This will be :unspecific if supported, or NIL if not.
  (defparameter *unspecific-pathname-type*
    #+(or abcl allegro clozure cmucl lispworks sbcl scl) :unspecific
    #+(or genera clasp clisp ecl mkcl gcl xcl #|These haven't been tested:|# cormanlisp mcl mezzano) nil
    "Unspecific type component to use with the underlying implementation's MAKE-PATHNAME")

  (defun make-pathname* (&rest keys &key directory host device name type version defaults
                                      #+scl &allow-other-keys)
    "Takes arguments like CL:MAKE-PATHNAME in the CLHS, and
   tries hard to make a pathname that will actually behave as documented,
   despite the peculiarities of each implementation. DEPRECATED: just use MAKE-PATHNAME."
    (declare (ignore host device directory name type version defaults))
    (apply 'make-pathname keys))

  (defun make-pathname-component-logical (x)
    "Make a pathname component suitable for use in a logical-pathname"
    (typecase x
      ((eql :unspecific) nil)
      #+clisp (string (string-upcase x))
      #+clisp (cons (mapcar 'make-pathname-component-logical x))
      (t x)))

  (defun make-pathname-logical (pathname host)
    "Take a PATHNAME's directory, name, type and version components,
and make a new pathname with corresponding components and specified logical HOST"
    (make-pathname
     :host host
     :directory (make-pathname-component-logical (pathname-directory pathname))
     :name (make-pathname-component-logical (pathname-name pathname))
     :type (make-pathname-component-logical (pathname-type pathname))
     :version (make-pathname-component-logical (pathname-version pathname))))

  (defun merge-pathnames* (specified &optional (defaults *default-pathname-defaults*))
    "MERGE-PATHNAMES* is like MERGE-PATHNAMES except that
if the SPECIFIED pathname does not have an absolute directory,
then the HOST and DEVICE both come from the DEFAULTS, whereas
if the SPECIFIED pathname does have an absolute directory,
then the HOST and DEVICE both come from the SPECIFIED pathname.
This is what users want on a modern Unix or Windows operating system,
unlike the MERGE-PATHNAMES behavior.
Also, if either argument is NIL, then the other argument is returned unmodified;
this is unlike MERGE-PATHNAMES which always merges with a pathname,
by default *DEFAULT-PATHNAME-DEFAULTS*, which cannot be NIL."
    (when (null specified) (return-from merge-pathnames* defaults))
    (when (null defaults) (return-from merge-pathnames* specified))
    #+scl
    (ext:resolve-pathname specified defaults)
    #-scl
    (let* ((specified (pathname specified))
           (defaults (pathname defaults))
           (directory (normalize-pathname-directory-component (pathname-directory specified)))
           (name (or (pathname-name specified) (pathname-name defaults)))
           (type (or (pathname-type specified) (pathname-type defaults)))
           (version (or (pathname-version specified) (pathname-version defaults))))
      (labels ((unspecific-handler (p)
                 (if (typep p 'logical-pathname) #'make-pathname-component-logical #'identity)))
        (multiple-value-bind (host device directory unspecific-handler)
            (ecase (first directory)
              ((:absolute)
               (values (pathname-host specified)
                       (pathname-device specified)
                       directory
                       (unspecific-handler specified)))
              ((nil :relative)
               (values (pathname-host defaults)
                       (pathname-device defaults)
                       (merge-pathname-directory-components directory (pathname-directory defaults))
                       (unspecific-handler defaults))))
          (make-pathname :host host :device device :directory directory
                         :name (funcall unspecific-handler name)
                         :type (funcall unspecific-handler type)
                         :version (funcall unspecific-handler version))))))

  (defun logical-pathname-p (x)
    "is X a logical-pathname?"
    (typep x 'logical-pathname))

  (defun physical-pathname-p (x)
    "is X a pathname that is not a logical-pathname?"
    (and (pathnamep x) (not (logical-pathname-p x))))

  (defun physicalize-pathname (x)
    "if X is a logical pathname, use translate-logical-pathname on it."
    ;; Ought to be the same as translate-logical-pathname, except the latter borks on CLISP
    (let ((p (when x (pathname x))))
      (if (logical-pathname-p p) (translate-logical-pathname p) p)))

  (defun nil-pathname (&optional (defaults *default-pathname-defaults*))
    "A pathname that is as neutral as possible for use as defaults
when merging, making or parsing pathnames"
    ;; 19.2.2.2.1 says a NIL host can mean a default host;
    ;; see also "valid physical pathname host" in the CLHS glossary, that suggests
    ;; strings and lists of strings or :unspecific
    ;; But CMUCL decides to die on NIL.
    ;; MCL has issues with make-pathname, nil and defaulting
    (declare (ignorable defaults))
    #.`(make-pathname :directory nil :name nil :type nil :version nil
                      :device (or #+(and mkcl os-unix) :unspecific)
                      :host (or #+cmucl lisp::*unix-host* #+(and mkcl os-unix) "localhost")
                      #+scl ,@'(:scheme nil :scheme-specific-part nil
                                :username nil :password nil :parameters nil :query nil :fragment nil)
                      ;; the default shouldn't matter, but we really want something physical
                      #-mcl ,@'(:defaults defaults)))

  (defvar *nil-pathname* (nil-pathname (physicalize-pathname (user-homedir-pathname)))
    "A pathname that is as neutral as possible for use as defaults
when merging, making or parsing pathnames")

  (defmacro with-pathname-defaults ((&optional defaults) &body body)
    "Execute BODY in a context where the *DEFAULT-PATHNAME-DEFAULTS* is as specified,
where leaving the defaults NIL or unspecified means a (NIL-PATHNAME), except
on ABCL, Genera and XCL, where it remains unchanged for it doubles as current-directory."
    `(let ((*default-pathname-defaults*
             ,(or defaults
                  #-(or abcl genera xcl) '*nil-pathname*
                  #+(or abcl genera xcl) '*default-pathname-defaults*)))
       ,@body)))


;;; Some pathname predicates
(with-upgradability ()
  (defun pathname-equal (p1 p2)
    "Are the two pathnames P1 and P2 reasonably equal in the paths they denote?"
    (when (stringp p1) (setf p1 (pathname p1)))
    (when (stringp p2) (setf p2 (pathname p2)))
    (flet ((normalize-component (x)
             (unless (member x '(nil :unspecific :newest (:relative)) :test 'equal)
               x)))
      (macrolet ((=? (&rest accessors)
                   (flet ((frob (x)
                            (reduce 'list (cons 'normalize-component accessors)
                                    :initial-value x :from-end t)))
                     `(equal ,(frob 'p1) ,(frob 'p2)))))
        (or (and (null p1) (null p2))
            (and (pathnamep p1) (pathnamep p2)
                 (and (=? pathname-host)
                      #-(and mkcl os-unix) (=? pathname-device)
                      (=? normalize-pathname-directory-component pathname-directory)
                      (=? pathname-name)
                      (=? pathname-type)
                      #-mkcl (=? pathname-version)))))))

  (defun absolute-pathname-p (pathspec)
    "If PATHSPEC is a pathname or namestring object that parses as a pathname
possessing an :ABSOLUTE directory component, return the (parsed) pathname.
Otherwise return NIL"
    (and pathspec
         (typep pathspec '(or null pathname string))
         (let ((pathname (pathname pathspec)))
           (and (eq :absolute (car (normalize-pathname-directory-component
                                    (pathname-directory pathname))))
                pathname))))

  (defun relative-pathname-p (pathspec)
    "If PATHSPEC is a pathname or namestring object that parses as a pathname
possessing a :RELATIVE or NIL directory component, return the (parsed) pathname.
Otherwise return NIL"
    (and pathspec
         (typep pathspec '(or null pathname string))
         (let* ((pathname (pathname pathspec))
                (directory (normalize-pathname-directory-component
                            (pathname-directory pathname))))
           (when (or (null directory) (eq :relative (car directory)))
             pathname))))

  (defun hidden-pathname-p (pathname)
    "Return a boolean that is true if the pathname is hidden as per Unix style,
i.e. its name starts with a dot."
    (and pathname (equal (first-char (pathname-name pathname)) #\.)))

  (defun file-pathname-p (pathname)
    "Does PATHNAME represent a file, i.e. has a non-null NAME component?

Accepts NIL, a string (converted through PARSE-NAMESTRING) or a PATHNAME.

Note that this does _not_ check to see that PATHNAME points to an
actually-existing file.

Returns the (parsed) PATHNAME when true"
    (when pathname
      (let ((pathname (pathname pathname)))
        (unless (and (member (pathname-name pathname) '(nil :unspecific "") :test 'equal)
                     (member (pathname-type pathname) '(nil :unspecific "") :test 'equal))
          pathname)))))


;;; Directory pathnames
(with-upgradability ()
  (defun pathname-directory-pathname (pathname)
    "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME, TYPE and VERSION components"
    (when pathname
      (make-pathname :name nil :type nil :version nil :defaults pathname)))

  (defun pathname-parent-directory-pathname (pathname)
    "Returns a new pathname that corresponds to the parent of the current pathname's directory,
i.e. removing one level of depth in the DIRECTORY component. e.g. if pathname is
Unix pathname /foo/bar/baz/file.type then return /foo/bar/"
    (when pathname
      (make-pathname :name nil :type nil :version nil
                     :directory (merge-pathname-directory-components
                                 '(:relative :back) (pathname-directory pathname))
                     :defaults pathname)))

  (defun directory-pathname-p (pathname)
    "Does PATHNAME represent a directory?

A directory-pathname is a pathname _without_ a filename. The three
ways that the filename components can be missing are for it to be NIL,
:UNSPECIFIC or the empty string.

Note that this does _not_ check to see that PATHNAME points to an
actually-existing directory."
    (when pathname
      ;; I tried using Allegro's excl:file-directory-p, but this cannot be done,
      ;; because it rejects apparently legal pathnames as
      ;; ill-formed. [2014/02/10:rpg]
      (let ((pathname (pathname pathname)))
        (flet ((check-one (x)
                 (member x '(nil :unspecific) :test 'equal)))
          (and (not (wild-pathname-p pathname))
               (check-one (pathname-name pathname))
               (check-one (pathname-type pathname))
               t)))))

  (defun ensure-directory-pathname (pathspec &optional (on-error 'error))
    "Converts the non-wild pathname designator PATHSPEC to directory form."
    (cond
      ((stringp pathspec)
       (ensure-directory-pathname (pathname pathspec)))
      ((not (pathnamep pathspec))
       (call-function on-error (compatfmt "~@<Invalid pathname designator ~S~@:>") pathspec))
      ((wild-pathname-p pathspec)
       (call-function on-error (compatfmt "~@<Can't reliably convert wild pathname ~3i~_~S~@:>") pathspec))
      ((directory-pathname-p pathspec)
       pathspec)
      (t
       (handler-case
           (make-pathname :directory (append (or (normalize-pathname-directory-component
                                                  (pathname-directory pathspec))
                                                 (list :relative))
                                             (list #-genera (file-namestring pathspec)
                                                   ;; On Genera's native filesystem (LMFS),
                                                   ;; directories have a type and version
                                                   ;; which must be ignored when converting
                                                   ;; to a directory pathname
                                                   #+genera (if (typep pathspec 'fs:lmfs-pathname)
                                                                (pathname-name pathspec)
                                                                (file-namestring pathspec))))
                          :name nil :type nil :version nil :defaults pathspec)
         (error (c) (call-function on-error (compatfmt "~@<error while trying to create a directory pathname for ~S: ~A~@:>") pathspec c)))))))


;;; Parsing filenames
(with-upgradability ()
  (declaim (ftype function ensure-pathname)) ; forward reference

  (defun split-unix-namestring-directory-components
      (unix-namestring &key ensure-directory dot-dot)
    "Splits the path string UNIX-NAMESTRING, returning four values:
A flag that is either :absolute or :relative, indicating
   how the rest of the values are to be interpreted.
A directory path --- a list of strings and keywords, suitable for
   use with MAKE-PATHNAME when prepended with the flag value.
   Directory components with an empty name or the name . are removed.
   Any directory named .. is read as DOT-DOT, or :BACK if it's NIL (not :UP).
A last-component, either a file-namestring including type extension,
   or NIL in the case of a directory pathname.
A flag that is true iff the unix-style-pathname was just
   a file-namestring without / path specification.
ENSURE-DIRECTORY forces the namestring to be interpreted as a directory pathname:
the third return value will be NIL, and final component of the namestring
will be treated as part of the directory path.

An empty string is thus read as meaning a pathname object with all fields nil.

Note that colon characters #\: will NOT be interpreted as host specification.
Absolute pathnames are only appropriate on Unix-style systems.

The intention of this function is to support structured component names,
e.g., \(:file \"foo/bar\"\), which will be unpacked to relative pathnames."
    (check-type unix-namestring string)
    (check-type dot-dot (member nil :back :up))
    (if (and (not (find #\/ unix-namestring)) (not ensure-directory)
             (plusp (length unix-namestring)))
        (values :relative () unix-namestring t)
        (let* ((components (split-string unix-namestring :separator "/"))
               (last-comp (car (last components))))
          (multiple-value-bind (relative components)
              (if (equal (first components) "")
                  (if (equal (first-char unix-namestring) #\/)
                      (values :absolute (cdr components))
                      (values :relative nil))
                  (values :relative components))
            (setf components (remove-if #'(lambda (x) (member x '("" ".") :test #'equal))
                                        components))
            (setf components (substitute (or dot-dot :back) ".." components :test #'equal))
            (cond
              ((equal last-comp "")
               (values relative components nil nil)) ; "" already removed from components
              (ensure-directory
               (values relative components nil nil))
              (t
               (values relative (butlast components) last-comp nil)))))))

  (defun split-name-type (filename)
    "Split a filename into two values NAME and TYPE that are returned.
We assume filename has no directory component.
The last . if any separates name and type from from type,
except that if there is only one . and it is in first position,
the whole filename is the NAME with an empty type.
NAME is always a string.
For an empty type, *UNSPECIFIC-PATHNAME-TYPE* is returned."
    (check-type filename string)
    (assert (plusp (length filename)))
    (destructuring-bind (name &optional (type *unspecific-pathname-type*))
        (split-string filename :max 2 :separator ".")
      (if (equal name "")
          (values filename *unspecific-pathname-type*)
          (values name type))))

  (defun parse-unix-namestring (name &rest keys &key type defaults dot-dot ensure-directory
                                &allow-other-keys)
    "Coerce NAME into a PATHNAME using standard Unix syntax.

Unix syntax is used whether or not the underlying system is Unix;
on such non-Unix systems it is reliably usable only for relative pathnames.
This function is especially useful to manipulate relative pathnames portably,
where it is of crucial to possess a portable pathname syntax independent of the underlying OS.
This is what PARSE-UNIX-NAMESTRING provides, and why we use it in ASDF.

When given a PATHNAME object, just return it untouched.
When given NIL, just return NIL.
When given a non-null SYMBOL, first downcase its name and treat it as a string.
When given a STRING, portably decompose it into a pathname as below.

#\\/ separates directory components.

The last #\\/-separated substring is interpreted as follows:
1- If TYPE is :DIRECTORY or ENSURE-DIRECTORY is true,
 the string is made the last directory component, and NAME and TYPE are NIL.
 if the string is empty, it's the empty pathname with all slots NIL.
2- If TYPE is NIL, the substring is a file-namestring, and its NAME and TYPE
 are separated by SPLIT-NAME-TYPE.
3- If TYPE is a string, it is the given TYPE, and the whole string is the NAME.

Directory components with an empty name or the name \".\" are removed.
Any directory named \"..\" is read as DOT-DOT,
which must be one of :BACK or :UP and defaults to :BACK.

HOST, DEVICE and VERSION components are taken from DEFAULTS,
which itself defaults to *NIL-PATHNAME*, also used if DEFAULTS is NIL.
No host or device can be specified in the string itself,
which makes it unsuitable for absolute pathnames outside Unix.

For relative pathnames, these components (and hence the defaults) won't matter
if you use MERGE-PATHNAMES* but will matter if you use MERGE-PATHNAMES,
which is an important reason to always use MERGE-PATHNAMES*.

Arbitrary keys are accepted, and the parse result is passed to ENSURE-PATHNAME
with those keys, removing TYPE DEFAULTS and DOT-DOT.
When you're manipulating pathnames that are supposed to make sense portably
even though the OS may not be Unixish, we recommend you use :WANT-RELATIVE T
to throw an error if the pathname is absolute"
    (block nil
      (check-type type (or null string (eql :directory)))
      (when ensure-directory
        (setf type :directory))
      (etypecase name
        ((or null pathname) (return name))
        (symbol
         (setf name (string-downcase name)))
        (string))
      (multiple-value-bind (relative path filename file-only)
          (split-unix-namestring-directory-components
           name :dot-dot dot-dot :ensure-directory (eq type :directory))
        (multiple-value-bind (name type)
            (cond
              ((or (eq type :directory) (null filename))
               (values nil nil))
              (type
               (values filename type))
              (t
               (split-name-type filename)))
          (apply 'ensure-pathname
                 (make-pathname
                  :directory (unless file-only (cons relative path))
                  :name name :type type
                  :defaults (or #-mcl defaults *nil-pathname*))
                 (remove-plist-keys '(:type :dot-dot :defaults) keys))))))

  (defun unix-namestring (pathname)
    "Given a non-wild PATHNAME, return a Unix-style namestring for it.
If the PATHNAME is NIL or a STRING, return it unchanged.

This only considers the DIRECTORY, NAME and TYPE components of the pathname.
This is a portable solution for representing relative pathnames,
But unless you are running on a Unix system, it is not a general solution
to representing native pathnames.

An error is signaled if the argument is not NULL, a STRING or a PATHNAME,
or if it is a PATHNAME but some of its components are not recognized."
    (etypecase pathname
      ((or null string) pathname)
      (pathname
       (with-output-to-string (s)
         (flet ((err () (parameter-error "~S: invalid unix-namestring ~S"
                                         'unix-namestring pathname)))
           (let* ((dir (normalize-pathname-directory-component (pathname-directory pathname)))
                  (name (pathname-name pathname))
                  (name (and (not (eq name :unspecific)) name))
                  (type (pathname-type pathname))
                  (type (and (not (eq type :unspecific)) type)))
             (cond
               ((member dir '(nil :unspecific)))
               ((eq dir '(:relative)) (princ "./" s))
               ((consp dir)
                (destructuring-bind (relabs &rest dirs) dir
                  (or (member relabs '(:relative :absolute)) (err))
                  (when (eq relabs :absolute) (princ #\/ s))
                  (loop :for x :in dirs :do
                    (cond
                      ((member x '(:back :up)) (princ "../" s))
                      ((equal x "") (err))
                      ;;((member x '("." "..") :test 'equal) (err))
                      ((stringp x) (format s "~A/" x))
                      (t (err))))))
               (t (err)))
             (cond
               (name
                (unless (and (stringp name) (or (null type) (stringp type))) (err))
                (format s "~A~@[.~A~]" name type))
               (t
                (or (null type) (err)))))))))))

;;; Absolute and relative pathnames
(with-upgradability ()
  (defun subpathname (pathname subpath &key type)
    "This function takes a PATHNAME and a SUBPATH and a TYPE.
If SUBPATH is already a PATHNAME object (not namestring),
and is an absolute pathname at that, it is returned unchanged;
otherwise, SUBPATH is turned into a relative pathname with given TYPE
as per PARSE-UNIX-NAMESTRING with :WANT-RELATIVE T :TYPE TYPE,
then it is merged with the PATHNAME-DIRECTORY-PATHNAME of PATHNAME."
    (or (and (pathnamep subpath) (absolute-pathname-p subpath))
        (merge-pathnames* (parse-unix-namestring subpath :type type :want-relative t)
                          (pathname-directory-pathname pathname))))

  (defun subpathname* (pathname subpath &key type)
    "returns NIL if the base pathname is NIL, otherwise like SUBPATHNAME."
    (and pathname
         (subpathname (ensure-directory-pathname pathname) subpath :type type)))

  (defun pathname-root (pathname)
    "return the root directory for the host and device of given PATHNAME"
    (make-pathname :directory '(:absolute)
                   :name nil :type nil :version nil
                   :defaults pathname ;; host device, and on scl, *some*
                   ;; scheme-specific parts: port username password, not others:
                   . #.(or #+scl '(:parameters nil :query nil :fragment nil))))

  (defun pathname-host-pathname (pathname)
    "return a pathname with the same host as given PATHNAME, and all other fields NIL"
    (make-pathname :directory nil
                   :name nil :type nil :version nil :device nil
                   :defaults pathname ;; host device, and on scl, *some*
                   ;; scheme-specific parts: port username password, not others:
                   . #.(or #+scl '(:parameters nil :query nil :fragment nil))))

  (defun ensure-absolute-pathname (path &optional defaults (on-error 'error))
    "Given a pathname designator PATH, return an absolute pathname as specified by PATH
considering the DEFAULTS, or, if not possible, use CALL-FUNCTION on the specified ON-ERROR behavior,
with a format control-string and other arguments as arguments"
    (cond
      ((absolute-pathname-p path))
      ((stringp path) (ensure-absolute-pathname (pathname path) defaults on-error))
      ((not (pathnamep path)) (call-function on-error "not a valid pathname designator ~S" path))
      ((let ((default-pathname (if (pathnamep defaults) defaults (call-function defaults))))
         (or (if (absolute-pathname-p default-pathname)
                 (absolute-pathname-p (merge-pathnames* path default-pathname))
                 (call-function on-error "Default pathname ~S is not an absolute pathname"
                                default-pathname))
             (call-function on-error "Failed to merge ~S with ~S into an absolute pathname"
                            path default-pathname))))
      (t (call-function on-error
                        "Cannot ensure ~S is evaluated as an absolute pathname with defaults ~S"
                        path defaults))))

  (defun subpathp (maybe-subpath base-pathname)
    "if MAYBE-SUBPATH is a pathname that is under BASE-PATHNAME, return a pathname object that
when used with MERGE-PATHNAMES* with defaults BASE-PATHNAME, returns MAYBE-SUBPATH."
    (and (pathnamep maybe-subpath) (pathnamep base-pathname)
         (absolute-pathname-p maybe-subpath) (absolute-pathname-p base-pathname)
         (directory-pathname-p base-pathname) (not (wild-pathname-p base-pathname))
         (pathname-equal (pathname-root maybe-subpath) (pathname-root base-pathname))
         (with-pathname-defaults (*nil-pathname*)
           (let ((enough (enough-namestring maybe-subpath base-pathname)))
             (and (relative-pathname-p enough) (pathname enough))))))

  (defun enough-pathname (maybe-subpath base-pathname)
    "if MAYBE-SUBPATH is a pathname that is under BASE-PATHNAME, return a pathname object that
when used with MERGE-PATHNAMES* with defaults BASE-PATHNAME, returns MAYBE-SUBPATH."
    (let ((sub (when maybe-subpath (pathname maybe-subpath)))
          (base (when base-pathname (ensure-absolute-pathname (pathname base-pathname)))))
      (or (and base (subpathp sub base)) sub)))

  (defun call-with-enough-pathname (maybe-subpath defaults-pathname thunk)
    "In a context where *DEFAULT-PATHNAME-DEFAULTS* is bound to DEFAULTS-PATHNAME (if not null,
or else to its current value), call THUNK with ENOUGH-PATHNAME for MAYBE-SUBPATH
given DEFAULTS-PATHNAME as a base pathname."
    (let ((enough (enough-pathname maybe-subpath defaults-pathname))
          (*default-pathname-defaults* (or defaults-pathname *default-pathname-defaults*)))
      (funcall thunk enough)))

  (defmacro with-enough-pathname ((pathname-var &key (pathname pathname-var)
                                                  (defaults *default-pathname-defaults*))
                                  &body body)
    "Shorthand syntax for CALL-WITH-ENOUGH-PATHNAME"
    `(call-with-enough-pathname ,pathname ,defaults #'(lambda (,pathname-var) ,@body))))


;;; Wildcard pathnames
(with-upgradability ()
  (defparameter *wild* (or #+cormanlisp "*" :wild)
    "Wild component for use with MAKE-PATHNAME")
  (defparameter *wild-directory-component* (or :wild)
    "Wild directory component for use with MAKE-PATHNAME")
  (defparameter *wild-inferiors-component* (or :wild-inferiors)
    "Wild-inferiors directory component for use with MAKE-PATHNAME")
  (defparameter *wild-file*
    (make-pathname :directory nil :name *wild* :type *wild*
                   :version (or #-(or allegro abcl xcl) *wild*))
    "A pathname object with wildcards for matching any file with TRANSLATE-PATHNAME")
  (defparameter *wild-file-for-directory*
    (make-pathname :directory nil :name *wild* :type (or #-(or clisp gcl) *wild*)
                   :version (or #-(or allegro abcl clisp gcl xcl) *wild*))
    "A pathname object with wildcards for matching any file with DIRECTORY")
  (defparameter *wild-directory*
    (make-pathname :directory `(:relative ,*wild-directory-component*)
                   :name nil :type nil :version nil)
    "A pathname object with wildcards for matching any subdirectory")
  (defparameter *wild-inferiors*
    (make-pathname :directory `(:relative ,*wild-inferiors-component*)
                   :name nil :type nil :version nil)
    "A pathname object with wildcards for matching any recursive subdirectory")
  (defparameter *wild-path*
    (merge-pathnames* *wild-file* *wild-inferiors*)
    "A pathname object with wildcards for matching any file in any recursive subdirectory")

  (defun wilden (path)
    "From a pathname, return a wildcard pathname matching any file in any subdirectory of given pathname's directory"
    (merge-pathnames* *wild-path* path)))


;;; Translate a pathname
(with-upgradability ()
  (defun relativize-directory-component (directory-component)
    "Given the DIRECTORY-COMPONENT of a pathname, return an otherwise similar relative directory component"
    (let ((directory (normalize-pathname-directory-component directory-component)))
      (cond
        ((stringp directory)
         (list :relative directory))
        ((eq (car directory) :absolute)
         (cons :relative (cdr directory)))
        (t
         directory))))

  (defun relativize-pathname-directory (pathspec)
    "Given a PATHNAME, return a relative pathname with otherwise the same components"
    (let ((p (pathname pathspec)))
      (make-pathname
       :directory (relativize-directory-component (pathname-directory p))
       :defaults p)))

  (defun directory-separator-for-host (&optional (pathname *default-pathname-defaults*))
    "Given a PATHNAME, return the character used to delimit directory names on this host and device."
    (let ((foo (make-pathname :directory '(:absolute "FOO") :defaults pathname)))
      (last-char (namestring foo))))

  #-scl
  (defun directorize-pathname-host-device (pathname)
    "Given a PATHNAME, return a pathname that has representations of its HOST and DEVICE components
added to its DIRECTORY component. This is useful for output translations."
    (os-cond
     ((os-unix-p)
      (when (physical-pathname-p pathname)
        (return-from directorize-pathname-host-device pathname))))
    (let* ((root (pathname-root pathname))
           (wild-root (wilden root))
           (absolute-pathname (merge-pathnames* pathname root))
           (separator (directory-separator-for-host root))
           (root-namestring (namestring root))
           (root-string
             (substitute-if #\/
                            #'(lambda (x) (or (eql x #\:)
                                              (eql x separator)))
                            root-namestring)))
      (multiple-value-bind (relative path filename)
          (split-unix-namestring-directory-components root-string :ensure-directory t)
        (declare (ignore relative filename))
        (let ((new-base (make-pathname :defaults root :directory `(:absolute ,@path))))
          (translate-pathname absolute-pathname wild-root (wilden new-base))))))

  #+scl
  (defun directorize-pathname-host-device (pathname)
    (let ((scheme (ext:pathname-scheme pathname))
          (host (pathname-host pathname))
          (port (ext:pathname-port pathname))
          (directory (pathname-directory pathname)))
      (flet ((specificp (x) (and x (not (eq x :unspecific)))))
        (if (or (specificp port)
                (and (specificp host) (plusp (length host)))
                (specificp scheme))
            (let ((prefix ""))
              (when (specificp port)
                (setf prefix (format nil ":~D" port)))
              (when (and (specificp host) (plusp (length host)))
                (setf prefix (strcat host prefix)))
              (setf prefix (strcat ":" prefix))
              (when (specificp scheme)
                (setf prefix (strcat scheme prefix)))
              (assert (and directory (eq (first directory) :absolute)))
              (make-pathname :directory `(:absolute ,prefix ,@(rest directory))
                             :defaults pathname)))
        pathname)))

  (defun* (translate-pathname*) (path absolute-source destination &optional root source)
    "A wrapper around TRANSLATE-PATHNAME to be used by the ASDF output-translations facility.
PATH is the pathname to be translated.
ABSOLUTE-SOURCE is an absolute pathname to use as source for translate-pathname,
DESTINATION is either a function, to be called with PATH and ABSOLUTE-SOURCE,
or a relative pathname, to be merged with ROOT and used as destination for translate-pathname
or an absolute pathname, to be used as destination for translate-pathname.
In that last case, if ROOT is non-NIL, PATH is first transformated by DIRECTORIZE-PATHNAME-HOST-DEVICE."
    (declare (ignore source))
    (cond
      ((functionp destination)
       (funcall destination path absolute-source))
      ((eq destination t)
       path)
      ((not (pathnamep destination))
       (parameter-error "~S: Invalid destination" 'translate-pathname*))
      ((not (absolute-pathname-p destination))
       (translate-pathname path absolute-source (merge-pathnames* destination root)))
      (root
       (translate-pathname (directorize-pathname-host-device path) absolute-source destination))
      (t
       (translate-pathname path absolute-source destination))))

  (defvar *output-translation-function* 'identity
    "Hook for output translations.

This function needs to be idempotent, so that actions can work
whether their inputs were translated or not,
which they will be if we are composing operations. e.g. if some
create-lisp-op creates a lisp file from some higher-level input,
you need to still be able to use compile-op on that lisp file."))
