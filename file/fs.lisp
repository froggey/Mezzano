;;;; Common Lisp file and file system support.

(defpackage :mezzano.file-system
  (:use :cl)
  (:export #:find-host
           #:list-all-hosts
           #:logical-host
           #:unknown-host
           #:host-name
           #:host-default-device
           #:host-pathname-class
           #:parse-namestring-using-host
           #:namestring-using-host
           #:make-host-name
           #:open-using-host
           #:probe-using-host
           #:directory-using-host
           #:ensure-directories-exist-using-host
           #:rename-file-using-host
           #:file-properties-using-host
           #:file-properties-using-stream
           #:file-properties
           #:set-file-properties-using-host
           #:set-file-properties-using-stream
           #:set-file-properties
           #:delete-file-using-host
           #:delete-directory
           #:delete-directory-using-host
           #:expunge-directory
           #:expunge-directory-using-host
           #:file-stream-pathname
           #:simple-file-error
           #:stream-truename
           #:truename-using-host
           #:no-namestring-error
           #:tmpdir-pathname
           #:file-system-host
           #:file-host-mount-mixin
           #:file-host-mount-args
           #:file-host-mount-state
           #:file-host-mount-device
           #:mount-host
           #:create-host
           #:register-block-device-host-type
           #:mount-block-device
           #:unmount-block-device
           #:unmounted-file-system-error))

(in-package :mezzano.file-system)

(define-condition file-error (error)
  ((pathname :initarg :pathname
             :reader file-error-pathname)))

(define-condition simple-file-error (file-error simple-error)
  ())

(define-condition no-namestring-error (simple-error)
  ((pathname :initarg :pathname
             :reader no-namestring-error-pathname)))

(defgeneric file-stream-pathname (stream))

(defvar *valid-hostname-characters* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-")

(defgeneric host-name (host))
(defgeneric host-default-device (host))

(defgeneric host-pathname-class (host)
  (:method (host) (find-class 'pathname)))

(defvar *host-alist* '())

(define-condition unknown-host (file-error)
  ((host :initarg :host :reader unknown-host-host))
  (:report (lambda (condition stream)
             (format stream "Unknown host ~S." (unknown-host-host condition)))))

(defun find-host (name &optional (errorp t))
  (typecase name
    ((or string symbol)
     (setf name (string-upcase (string name)))
     (if (zerop (length name))
         (pathname-host *default-pathname-defaults*)
         (or (second (assoc name *host-alist* :test 'string=))
             (when errorp
               (error 'unknown-host
                      :host name
                      :pathname (format nil "~A:" name))))))
    (file-system-host name)
    (t (error 'type-error
              :expected-type '(or string symbol file-system-host)
              :datum name))))

(defun (setf find-host) (new-value name &optional (errorp t))
  (declare (ignore errorp))
  (setf name (string-upcase (string name)))
  (assert (not (zerop (length name))))
  (check-type new-value (or null file-system-host))
  (cond ((null new-value)
         (setf *host-alist* (remove name *host-alist* :key 'first :test 'string=))
         NIL)
        (T
         (setf *host-alist*
               (list* (list name new-value)
                      (remove name *host-alist* :key 'first :test 'string=)))
         new-value)))

(defun list-all-hosts ()
  (mapcar #'second *host-alist*))

(defclass pathname ()
  ((%host :initarg :host :reader pathname-%host)
   (%device :initarg :device :reader pathname-%device)
   (%directory :initarg :directory :reader pathname-%directory)
   (%name :initarg :name :reader pathname-%name)
   (%type :initarg :type :reader pathname-%type)
   (%version :initarg :version :reader pathname-%version))
  (:default-initargs :device nil :directory nil :name nil :type nil :version nil))

(defun pathnamep (object)
  (typep object 'pathname))

;; This should really have a host associated with it...
(defvar *default-pathname-defaults* (make-instance 'pathname :host nil))

(defmethod initialize-instance :after ((instance pathname) &key)
  (assert (pathname-%host instance)))

(defun make-pathname (&key host
                        (device nil devicep)
                        (directory nil directoryp)
                        (name nil namep)
                        (type nil typep)
                        (version nil versionp)
                        defaults)
  (let* ((defaults (cond (defaults
                          (pathname defaults))
                         ((pathname-host *default-pathname-defaults*)
                          (make-instance (host-pathname-class (pathname-host *default-pathname-defaults*))
                                         :host (pathname-host *default-pathname-defaults*)))
                         (t
                          ;; During bootstrap, before any real filesystems are
                          ;; defined, the pathname in *D-P-D* is a hostless
                          ;; pathname. We want to avoid creating any more of
                          ;; these. So just use it as-is. This is fine, as all
                          ;; the other elements are NIL anyway.
                          *default-pathname-defaults*)))
         (host (if host
                   (find-host host)
                   (pathname-host defaults))))
    (make-instance (host-pathname-class host)
                   :host host
                   :device (if devicep device (pathname-device defaults))
                   :directory (if directoryp
                                  (if (eq directory :wild)
                                      '(:absolute :wild-inferiors)
                                      directory)
                                  (pathname-directory defaults))
                   :name (if namep name (pathname-name defaults))
                   :type (if typep type (pathname-type defaults))
                   :version (if versionp version (pathname-version defaults)))))

(defun pathname-host (pathname &key (case :local))
  (declare (ignore case))
  (pathname-%host (pathname pathname)))
(defun pathname-device (pathname &key (case :local))
  (declare (ignore case))
  (pathname-%device (pathname pathname)))
(defun pathname-directory (pathname &key (case :local))
  (declare (ignore case))
  (pathname-%directory (pathname pathname)))
(defun pathname-name (pathname &key (case :local))
  (declare (ignore case))
  (pathname-%name (pathname pathname)))
(defun pathname-type (pathname &key (case :local))
  (declare (ignore case))
  (pathname-%type (pathname pathname)))
(defun pathname-version (pathname &key (case :local))
  (declare (ignore case))
  (pathname-%version (pathname pathname)))

(defmethod make-load-form ((object pathname) &optional environment)
  (declare (ignore environment))
  `(let ((host (find-host ',(host-name (pathname-host object)) t)))
     (make-pathname :host host
                    :device ',(pathname-device object)
                    :directory ',(pathname-directory object)
                    :name ',(pathname-name object)
                    :type ',(pathname-type object)
                    :version ',(pathname-version object))))

(defun mezzano.internals::pathnames-equal (x y)
  (and (equal (pathname-host x) (pathname-host y))
       (equal (pathname-device x) (pathname-device y))
       (equal (pathname-directory x) (pathname-directory y))
       (equal (pathname-name x) (pathname-name y))
       (equal (pathname-type x) (pathname-type y))
       (or (and (null (pathname-version x))
                (eql (pathname-version y) :newest))
           (and (null (pathname-version y))
                (eql (pathname-version x) :newest))
           (equal (pathname-version x) (pathname-version y)))))

(defun mezzano.internals::hash-pathname (pathname depth)
  (let ((version (or (pathname-version pathname) :newest)))
    (logxor (mezzano.internals::sxhash-1 (host-name (pathname-host pathname)) depth)
            (mezzano.internals::sxhash-1 (pathname-device pathname) depth)
            (mezzano.internals::sxhash-1 (pathname-directory pathname) depth)
            (mezzano.internals::sxhash-1 (pathname-name pathname) depth)
            (mezzano.internals::sxhash-1 (pathname-type pathname) depth)
            (mezzano.internals::sxhash-1 version depth))))

(defun pathname-match-directory (p w)
  (let ((p-dir (pathname-directory p))
        (w-dir (pathname-directory w)))
    (labels ((match (p w)
               (cond
                 ;; :wild-inferiors matches the remaining directory levels
                 ((eql (first w) :wild-inferiors) t)
                 ((and (null p) (null w)) t)
                 ((or (null p) (null w)) nil)
                 ((eql (first w) :wild)
                  (match (rest p) (rest w)))
                 (t (and (string= (first p) (first w))
                         (match (rest p) (rest w)))))))
      (and (eql (first p-dir) (first w-dir))
           (match (rest p-dir) (rest w-dir))))))

(defun pathname-match-p (pathname wildcard)
  (let ((p (pathname pathname))
        (w (pathname wildcard)))
    (and (or (member (pathname-host w) '(nil :wild))
             (eql (pathname-host p) (pathname-host w)))
         (or (member (pathname-device w) '(nil :wild))
             (equal (pathname-device p) (pathname-device w)))
         (pathname-match-directory p w)
         (or (member (pathname-name w) '(nil :wild))
             (equal (pathname-name p) (pathname-name w)))
         (or (member (pathname-type w) '(nil :wild))
             (equal (pathname-type p) (pathname-type w)))
         (or (member (pathname-version w) '(nil :wild))
             (equal (pathname-version p) (pathname-version w))))))

(defgeneric namestring-using-host (path host))

(defun host-namestring (pathname)
  (host-name (pathname-host pathname)))

(defun namestring (pathname)
  (let ((p (pathname pathname)))
    (concatenate 'string
                 (string (host-name (pathname-host p)))
                 ":"
                 (namestring-using-host (pathname-host p) p))))

(defun file-namestring (pathname)
  (namestring-using-host (pathname-host pathname)
                         (make-pathname :host (pathname-host pathname)
                                        :device nil
                                        :directory nil
                                        :name (pathname-name pathname)
                                        :type (pathname-type pathname)
                                        :version (pathname-version pathname)
                                        :defaults pathname)))

(defun directory-namestring (pathname)
  (namestring-using-host (pathname-host pathname)
                         (make-pathname :host (pathname-host pathname)
                                        :device nil
                                        :directory (pathname-directory pathname)
                                        :name nil
                                        :type nil
                                        :version nil
                                        :defaults pathname)))

(defun enough-namestring (pathname &optional (defaults *default-pathname-defaults*))
  (if (eql (pathname-host pathname) (pathname-host defaults))
      (let ((p-dirs (pathname-directory pathname))
            (d-dirs (pathname-directory defaults)))
        (if (and (eq (car p-dirs) :absolute)
                 (eq (car d-dirs) :absolute))
            (do ((p-dir (cadr p-dirs) (cadr p-rest))
                 (d-dir (cadr d-dirs) (cadr d-rest))
                 (p-rest (cdr p-dirs) (cdr p-rest))
                 (d-rest (cdr d-dirs) (cdr d-rest)))
                ((or (null p-dir) (null d-dir) (not (equal p-dir d-dir)))
                 (cond ((null p-dir)
                        (if (null d-dir)
                            ;; directories match exactly
                            (file-namestring pathname)
                            ;; default directory has more entries than pathname
                            (namestring pathname)))
                       ((null d-dir)
                        (namestring-using-host (pathname-host pathname)
                                               (make-pathname
                                                :host (pathname-host pathname)
                                                :directory (cons :relative p-rest)
                                                :name (pathname-name pathname)
                                                :type (pathname-type pathname)
                                                :version (pathname-version pathname))))
                       ;; directory names differ
                       (t (namestring pathname)))))
            ;; pathnames are not absolute but have the same host
            (namestring-using-host (pathname-host pathname) pathname)))
      ;; hosts don't match
      (namestring pathname)))

(defmethod print-object ((object pathname) stream)
  (cond ((pathname-host object)
         (handler-case
             (format stream "#P~S" (namestring object))
           (no-namestring-error (c)
             (declare (ignore c))
             (print-unreadable-object (object stream :type t)
               (format stream "with no namestring ~S"
                       (list :host (pathname-host object)
                             :device (pathname-device object)
                             :directory (pathname-directory object)
                             :name (pathname-name object)
                             :type (pathname-type object)
                             :version (pathname-version object)))))))
        (t
         (print-unreadable-object (object stream :type t)
           (format stream "with no host ~S"
                   (list :host (pathname-host object)
                         :device (pathname-device object)
                         :directory (pathname-directory object)
                         :name (pathname-name object)
                         :type (pathname-type object)
                         :version (pathname-version object)))))))

(defun pathname (pathname)
  (typecase pathname
    (pathname pathname)
    (file-stream
     (pathname (file-stream-pathname pathname)))
    (synonym-stream
     (pathname (mezzano.internals::follow-synonym-stream pathname)))
    (string
     (parse-namestring pathname))
    (t
     (error 'type-error :datum pathname :expected-type 'pathname-designator))))

(defgeneric stream-truename (stream))

(defgeneric truename-using-host (host pathname))

(defmethod truename-using-host (host pathname)
  (or (probe-file pathname)
      (error 'simple-file-error
             :pathname pathname
             :format-control "No such file.")))

(defun truename (pathname)
  (cond
    ((typep pathname 'file-stream)
     (stream-truename pathname))
    (t
     (let ((p (translate-logical-pathname (merge-pathnames pathname))))
       (truename-using-host (pathname-host p) p)))))

(defun merge-pathnames (pathname &optional
                        (default-pathname *default-pathname-defaults*)
                        (default-version :newest))
  (setf default-pathname (pathname default-pathname))
  (let* ((pathname (let ((*default-pathname-defaults* default-pathname))
                     (pathname pathname)))
         (host (or (pathname-host pathname) (pathname-host default-pathname)))
         (device (pathname-device pathname))
         (directory (pathname-directory pathname))
         (name (or (pathname-name pathname) (pathname-name default-pathname)))
         (type (or (pathname-type pathname) (pathname-type default-pathname)))
         (version (or (pathname-version pathname)
                      (if (pathname-name pathname)
                          default-version
                          (pathname-version default-pathname)))))
    (when (and (not device)
               (pathname-host pathname)
               (not (pathname-device pathname)))
      (if (and (eql (pathname-host pathname) (pathname-host default-pathname)))
          (setf device (pathname-device default-pathname))
          (setf device (host-default-device host))))
    (cond ((and (pathname-directory default-pathname)
                (eql (first directory) :relative))
           (setf directory (append (pathname-directory default-pathname)
                                   (rest directory)))
           ;; remove :backs
           (let ((dir-type (car directory))
                 (dirs))
             (dolist (d (cdr directory))
               (if (eq d :back)
                   (pop dirs)
                   (push d dirs)))
             (setf directory (cons dir-type (nreverse dirs)))))
          ((null directory)
           (setf directory (pathname-directory default-pathname))))
    (make-pathname :host host
                   :device device
                   :directory directory
                   :name name
                   :type type
                   :version version)))

(defun wild-pathname-p (pathname &optional field-key)
  (ecase field-key
    ((nil) (or (eql (pathname-host pathname) :wild)
               (eql (pathname-device pathname) :wild)
               (eql (pathname-directory pathname) :wild)
               (and (listp (pathname-directory pathname))
                    (or (find :wild (cdr (pathname-directory pathname)))
                        (find :wild-inferiors (cdr (pathname-directory pathname)))))
               (eql (pathname-name pathname) :wild)
               (eql (pathname-type pathname) :wild)
               (eql (pathname-version pathname) :wild)))
    (:host (eql (pathname-host pathname) :wild))
    (:device (eql (pathname-device pathname) :wild))
    (:directory (or (eql (pathname-directory pathname) :wild)
                    (and (listp (pathname-directory pathname))
                         (or (find :wild (cdr (pathname-directory pathname)))
                             (find :wild-inferiors (cdr (pathname-directory pathname)))))))
    (:name (eql (pathname-name pathname) :wild))
    (:type (eql (pathname-type pathname) :wild))
    (:version (eql (pathname-version pathname) :wild))))

(defgeneric parse-namestring-using-host (host namestring junk-allowed))

(defun parse-namestring (thing &optional host (default-pathname *default-pathname-defaults*) &key (start 0) (end nil) junk-allowed)
  (loop
     (when (not (typep thing 'synonym-stream))
       (return))
     (setf thing (symbol-value (synonym-stream-symbol thing))))
  (when (typep thing 'file-stream)
    (setf thing (file-stream-pathname thing)))
  (etypecase thing
    (pathname
     (unless (or (eql host nil) (eql (pathname-host thing) host))
       (error 'simple-type-error
              :expected-type `(eql ,host)
              :datum (pathname-host thing)
              :format-control "Manifest host in pathname ~S does not match supplied host."
              :format-arguments (list thing)))
     thing)
    (string
     (setf thing (subseq thing start end))
     (multiple-value-bind (other-hostname rest-of-path)
         (extract-host-from-namestring thing)
       (when other-hostname
         ;; Namestring has an explicit host portion, do something with it.
         (let ((other-host (find-host other-hostname)))
           (cond ((and host (not (eql host other-host)))
                  (error 'simple-type-error
                         :expected-type `(eql ,host)
                         :datum other-host
                         :format-control "Manifest host in pathname ~S does not match supplied host."
                         :format-arguments (list thing)))
                 (t (setf host other-host)))))
       (setf default-pathname (pathname default-pathname))
       (parse-namestring-using-host (or host (pathname-host default-pathname))
                                    rest-of-path junk-allowed)))))

(defun valid-hostname-character-p (character)
  (find character *valid-hostname-characters*))

(defun extract-host-from-namestring (namestring)
  "Returns two values, the hostname extracted from NAMESTRING and the rest of the
path. If there is no host name, then NIL is returned as the first value and
NAMESTRING as the second."
  (do ((current 0 (1+ current))
       (hostname (make-array 50
                             :element-type 'character
                             :adjustable t
                             :fill-pointer 0)))
      ((>= current (length namestring))
       (values nil namestring))
    (let ((ch (char-upcase (char namestring current))))
      (when (eql ch #\:)
        ;; Reached end.
        (return (values hostname (subseq namestring (1+ current)))))
      (unless (valid-hostname-character-p ch)
        ;; Not a hostname.
        (return (values nil namestring)))
      (vector-push-extend ch hostname))))

(defun make-host-name (label &key replace-invalid-characters)
  "Convert a label into a valid host name if possible. Returns host-name and valid-p"
  (let ((host-name (string-upcase (string-right-trim '(#\Space #\Null) label))))
    (when replace-invalid-characters
      (setf host-name (substitute-if-not #\- #'valid-hostname-character-p host-name)))
    (when (/= (length host-name) 0)
      (loop
         for ch across host-name
         when (not (valid-hostname-character-p ch)) do
           (format t "Unable to make a host name from \"~A\", ~
                  ~C not a valid host name character~%" label ch)
           (return-from make-host-name (values NIL NIL))
         finally
           (return
             (loop
                with names = (mapcar #'first *host-alist*)
                with number = 2
                with result-name = host-name
                while (member result-name names :test 'string=) do
                  (setf result-name (format nil "~A-~D" host-name number))
                  (incf number)
                finally
                  (return (values result-name T))))))))

(defgeneric open-using-host (host pathname &key direction element-type if-exists if-does-not-exist external-format))

(defun open (filespec &key
             (direction :input)
             (element-type 'character)
             (if-exists nil if-exists-p)
             (if-does-not-exist nil if-does-not-exist-p)
             (external-format :default))
  (check-type direction (member :input :output :io :probe))
  (check-type if-exists (member :error :new-version :rename :rename-and-delete :overwrite :append :supersede nil))
  (check-type if-does-not-exist (member :error :create nil))
  (loop
     (restart-case
         (return
           (let* ((path (translate-logical-pathname (merge-pathnames filespec)))
                  (host (pathname-host path)))
             (when (wild-pathname-p path)
               (error 'simple-file-error
                      :pathname filespec
                      :format-control "Wild pathname specified."))
             (unless if-exists-p
               (setf if-exists (if (eql (pathname-version path) :newest)
                                   :new-version
                                   :error)))
             (unless if-does-not-exist-p
               (cond ((or (eql direction :input)
                          (eql if-exists :overwrite)
                          (eql if-exists :append))
                      (setf if-does-not-exist :error))
                     ((or (eql direction :output)
                          (eql direction :io))
                      (setf if-does-not-exist :create))
                     ((eql direction :probe)
                      (setf if-does-not-exist nil))))
             (open-using-host host path
                              :direction direction
                              :element-type element-type
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist
                              :external-format external-format)))
       (use-new-filespec (new-filespec)
         :report "Try opening a different path"
         :interactive (lambda ()
                        (format *debug-io* "Enter a new pathname (evaluated): ")
                        (finish-output *debug-io*)
                        (multiple-value-list (eval (read *debug-io*))))
         (setf filespec new-filespec)))))

(defgeneric probe-using-host (host pathname))

(defun probe-file (pathspec)
  (let* ((path (translate-logical-pathname (merge-pathnames pathspec)))
         (host (pathname-host path)))
    (when (wild-pathname-p path)
      (error 'simple-file-error
             :pathname pathspec
             :format-control "Wild pathname specified."))
    (probe-using-host host path)))

(defgeneric directory-using-host (host path &key))

(defun directory (pathspec &rest args &key &allow-other-keys)
  (let ((path (translate-logical-pathname (merge-pathnames pathspec))))
    (apply #'directory-using-host (pathname-host path) path args)))

(defun mixed-case-p (name)
  (not (every (lambda (ch)
                (or (eql ch #\*)
                    (eql ch #\-)
                    (digit-char-p ch)
                    (upper-case-p ch)))
              name)))

(defun case-correct-path-component (component from-host to-host)
  (cond ((and (typep from-host 'logical-host)
              (not (typep to-host 'logical-host))
              (stringp component)
              (not (mixed-case-p component)))
         (string-downcase component))
        ((and (not (typep from-host 'logical-host))
              (typep to-host 'logical-host)
              (stringp component)
              (not (mixed-case-p component)))
         (string-upcase component))
        (t component)))

(defun translate-one (source from to what)
  (cond ((member (funcall what to) '(nil :wild))
         (case-correct-path-component (funcall what source)
                                      (pathname-host from)
                                      (pathname-host to)))
        ((and (equal (funcall what source) (funcall what from))
              (not (member (funcall what to) '(nil :wild :unspecified))))
         (funcall what to))
        ((or (equal (funcall what source) (funcall what from))
             (eql (funcall what from) :wild))
         (case-correct-path-component (funcall what source)
                                      (pathname-host from)
                                      (pathname-host to)))
        (t
         (error "Source and from ~S don't match." what))))

(defun translate-directory (source from-wildcard to-wildcard)
  (let* ((s-d (pathname-directory source))
         (f-d (pathname-directory from-wildcard))
         (t-d (pathname-directory to-wildcard))
         (new-path (list (first t-d))))
    (when (null f-d)
      (return-from translate-directory source))
    (loop ;; Match leading parts of source/from.
       (cond ((eql (first f-d) :wild)
              (error ":WILD elements in from-wildcard directory not yet supported..."))
             ((eql (first f-d) :wild-inferiors)
              (assert (null (rest f-d)) (source from-wildcard to-wildcard)
                      ":WILD-INFERIORS must be the last directory entry... (FIXME)")
              (return))
             ((and (null s-d) (null f-d))
              (return))
             ((or (null s-d)
                  (null f-d)
                  (not (equal (first s-d) (first f-d))))
              (error "Directory entry mismatch. ~S ~S ~S ~S ~S~%"
                     (first s-d) (first f-d)
                     source from-wildcard to-wildcard)))
       (setf s-d (rest s-d)
             f-d (rest f-d)))
    ;; Merge SOURCE and TO. First component was done above.
    (do ((d (rest t-d) (cdr d)))
        ((or (null d)
             (eql (first d) :wild-inferiors))
         (cond ((null d)
                (assert (endp s-d) (s-d)
                        "To-wildcard directory portion exhausted with remaining source values.")
                (nreverse new-path))
               (t
                (assert (null (rest d))
                        (source from-wildcard to-wildcard)
                        ":WILD-INFERIORS must be the last directory entry... (FIXME)")
                (nconc (nreverse new-path)
                       (loop
                          for component in s-d
                          collect (case-correct-path-component component (pathname-host source) (pathname-host to-wildcard)))))))
      (push (first d) new-path))))

(defun translate-pathname (source from-wildcard to-wildcard &key)
  (make-pathname :host (pathname-host to-wildcard)
                 :device (cond ((typep source 'logical-pathname)
                                ;; Always favour the to-wildcard's device when
                                ;; translating from a logical pathname.
                                ;; Logical pathnames always have a device of
                                ;; :UNSPECIFIC, which would otherwise override
                                ;; any device specified in a translation.
                                (pathname-device to-wildcard))
                               (t
                                (translate-one source from-wildcard to-wildcard 'pathname-device)))
                 :name (translate-one source from-wildcard to-wildcard 'pathname-name)
                 :type (translate-one source from-wildcard to-wildcard 'pathname-type)
                 :version (translate-one source from-wildcard to-wildcard 'pathname-version)
                 :directory (translate-directory source from-wildcard to-wildcard)))

(defgeneric ensure-directories-exist-using-host (host pathname &key verbose))

(defun ensure-directories-exist (pathspec &rest keys &key verbose &allow-other-keys)
  (declare (ignore verbose))
  (let ((path (translate-logical-pathname (merge-pathnames pathspec))))
    (values pathspec
            (apply 'ensure-directories-exist-using-host
                   (pathname-host path)
                   path
                   keys))))

(defgeneric rename-file-using-host (host source dest))

(defun rename-file (filespec new-name)
  (let* ((source (translate-logical-pathname (merge-pathnames filespec)))
         (dest (translate-logical-pathname (merge-pathnames new-name source))))
    (assert (eql (pathname-host source) (pathname-host dest))
            (filespec new-name) "Cannot rename across hosts yet.")
    (rename-file-using-host (pathname-host source) source dest)
    (values dest source dest)))

(defgeneric file-properties-using-host (host pathname)
  (:documentation "Return file properties for the given pathname.
Should signal an error if the path does not exist."))
(defmethod file-properties-using-host (host pathname)
  '())

(defgeneric file-properties-using-stream (stream)
  (:documentation "Return file properties for the file associated with stream.
The default method falls back to calling FILE-PROPERTIES-USING-HOST with stream's truename,
but file systems may implement methods on this function to better support streams opened
in new-version/supersede/etc modes, where the underlying file does not fully exist."))
(defmethod file-properties-using-stream ((stream stream))
  ;; Fall back on the -host function.
  (let ((truename (truename stream)))
    (file-properties-using-host (pathname-host truename) truename)))

(defun file-properties (pathspec-or-stream)
  "Return a plist of properties associated with the specified file.
PATHSPEC-OR-STREAM should be either a pathname designator or a stream.
If it is a stream, then the properties associated with that stream's file will
be returned.
Exact behaviour and supported properties depend on the underlying file system.
Common properties include :LENGTH, :WRITE-DATE and :AUTHOR."
  (if (streamp pathspec-or-stream)
      (file-properties-using-stream pathspec-or-stream)
      (let ((path (translate-logical-pathname (merge-pathnames pathspec-or-stream))))
        (assert (not (wild-pathname-p path)))
        (file-properties-using-host (pathname-host path) path))))

(defgeneric set-file-properties-using-host (host pathname &key)
  (:documentation "Set properties associated with the specified path.
Methods should add keywords to indicate supported properties and allow
unknown properties to signal errors using the normal keyword checking mechanism."))
(defmethod set-file-properties-using-host (host pathname &key)
  (values))

(defgeneric set-file-properties-using-stream (stream &key)
  (:documentation "Like FILE-PROPERTIES-USING-STREAM.
Falls back on SET-FILE-PROPERTIES-USING-HOST by default."))
(defmethod set-file-properties-using-stream ((stream stream) &rest properties &key)
  ;; Fall back on the -host function.
  (let ((truename (truename stream)))
    (apply #'set-file-properties-using-host (pathname-host truename) truename properties)))

(defun set-file-properties (pathspec-or-stream &rest properties)
  "Sets properties associated with the file specified PATHSPEC-OR-STREAM.
The exact set of properties supported is file system dependent and underlying
implementations will signal an error on an attempt to set an unsupported property.
Users should pass :ALLOW-OTHER-KEYS T to avoid this, in which case the unsupported
properties will be ignored."
  (if (streamp pathspec-or-stream)
      (apply #'set-file-properties-using-stream pathspec-or-stream properties)
      (let ((path (translate-logical-pathname (merge-pathnames pathspec-or-stream))))
        (assert (not (wild-pathname-p path)))
        (apply #'set-file-properties-using-host (pathname-host path) path properties)))
  (values))

(defun file-write-date (pathspec)
  (getf (file-properties pathspec) :write-date nil))

(defun file-author (pathspec)
  (getf (file-properties pathspec) :author nil))

(defgeneric delete-file-using-host (host path &key))

(defun delete-file (filespec &rest args &key &allow-other-keys)
  (let ((path (translate-logical-pathname (merge-pathnames filespec))))
    (assert (not (wild-pathname-p path)))
    (apply #'delete-file-using-host (pathname-host path) path args))
  t)

(defgeneric delete-directory-using-host (host path &key recursive))

(defun delete-directory (pathspec &rest args &key &allow-other-keys)
  (let ((path (translate-logical-pathname (merge-pathnames pathspec))))
    (assert (not (wild-pathname-p path)))
    (apply #'delete-directory-using-host (pathname-host path) path args)))

(defgeneric expunge-directory-using-host (host path &key))

(defun expunge-directory (filespec &rest args &key &allow-other-keys)
  (let ((path (translate-logical-pathname (merge-pathnames filespec))))
    (assert (not (wild-pathname-p path)))
    (apply #'expunge-directory-using-host (pathname-host path) path args))
  t)

(defvar *home-directory* nil)

(defun user-homedir-pathname (&optional host)
  (if (not (member host '(nil :unspecific)))
      nil
      *home-directory*))

(defun tmpdir-pathname ()
  (let ((home (user-homedir-pathname)))
    ;; Construct a pathname with the same host as the homedir
    ;; then merge together to produce the full path on the right host.
    ;; Without this, the resulting pathname will take on the host
    ;; of *default-pathname-defaults*, which may differ from homedir's host.
    (merge-pathnames (make-pathname :host (pathname-host home)
                                    :directory '(:relative "tmp"))
                     home)))

;;; File System Host - base class that all file system hosts must superclass
;;;     Created so that find-host can identify file system host objects

(defclass file-system-host ()
  ())

;;; Mount Mixin - used to manage the mount state of block device hosts.

(defclass file-host-mount-mixin ()
  ((%mount-args    :initarg :mount-args   :accessor file-host-mount-args)
   (%mount-state   :initarg :mount-state  :accessor file-host-mount-state)
   (%mount-device  :initarg :mount-device :accessor file-host-mount-device))
  (:default-initargs :mount-args nil :mount-state :unmounted :mount-device NIL))

(defgeneric mount-host (host block-device))

(defgeneric create-host (class block-device name-alist))

;;; Code to maintain a list of file system host types that support block devices

(defvar *block-device-host-type-lock*
  (mezzano.supervisor:make-mutex "Lock for *block-device-host-types*"))
(defvar *block-device-host-types* NIL)

;;; Mapping between partition UUIDs and file system host names.  This
;;; is a system specific configuration and is typically set at the top
;;; of ipl.lisp

(defvar *filesystems-alist* NIL)

(defun register-block-device-host-type (host-type)
  (mezzano.supervisor:with-mutex (*block-device-host-type-lock*)
    (push host-type *block-device-host-types*))
  (dolist (block-device (mezzano.disk:all-block-devices))
    (create-host host-type block-device *filesystems-alist*))
  T)

(defun mount-block-device (block-device)
  ;; check if an existing host goes with this block device
  (loop
     for pair in *host-alist*
     for host = (cadr pair)
     when (and (typep host 'file-host-mount-mixin)
               (eq (file-host-mount-state host) :unmounted)) do
       (when (mount-host host block-device)
         (format t "mount-block-device: mounted ~A on ~A~%"
                 block-device (car pair))
         (return-from mount-block-device)))
  ;; No existing host found
  ;; if *filesystems* bound (usually in config.lisp) check list for
  ;; cold-boot hosts.
    (loop
       for host-class in *block-device-host-types*
       for name = (create-host host-class block-device *filesystems-alist*)
       when name do
         (format t "mount-block-device: mounted ~A on ~A~%" block-device name)
         (return-from mount-block-device))
  ;; No host in *filesystems-alist* found
  ;; TODO get uuid/host name alist from name space server and call
  ;; create-host for each host class, exit on success.
  (format t "mount-block-device: no host found for ~A~%" block-device))

(defun unmount-block-device (block-device)
  (loop
     for pair in *host-alist*
     for host = (cadr pair)
     when (and (typep host 'file-host-mount-mixin)
               (eq (file-host-mount-device host) block-device)) do
       (setf (file-host-mount-state host) :unmounted
             (file-host-mount-device host) NIL)
       (format t "unmount-block-device: unmounted ~A~%" (car pair))
       (return-from unmount-block-device))
  (format t "unmount-block-device: no host found for ~A~%" block-device))

;;; Logical pathnames.

(defclass logical-host (file-system-host)
  ((%name :initarg :name :reader host-name)
   (%translations :initform '() :accessor logical-host-translations)))

(defmethod print-object ((object logical-host) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (host-name object))))

(defclass logical-pathname (pathname)
  ())

(defmethod host-pathname-class ((host logical-host))
  (find-class 'logical-pathname))

(defmethod parse-namestring-using-host ((host logical-host) namestring junk-allowed)
  (assert (not junk-allowed) (junk-allowed) "Junk-allowed not implemented yet")
  (let ((relative :absolute)
        (directories '())
        (name nil)
        (type nil)
        (version nil)
        (offset 0))
    (flet ((consume-char (char)
             (when (and (< offset (length namestring))
                        (char= (char namestring offset) char))
               (incf offset)
               t))
           (consume-word ()
             (let ((chars (make-array 50
                                      :element-type 'character
                                      :fill-pointer 0
                                      :adjustable t))
                   (saw-escape nil))
               (loop
                  (when (>= offset (length namestring))
                    (when saw-escape
                      (error "Unexpected end of string after escape in logical pathname"))
                    (return))
                  (let ((ch (char namestring offset)))
                    (cond ((and (eql saw-escape :multiple)
                                (eql ch #\|))
                           ;; Leave multiple escape.
                           (setf saw-escape nil)
                           (incf offset))
                          ((and (eql saw-escape :multiple)
                                (eql ch #\\))
                           (setf saw-escape :multiple-single)
                           (incf offset))
                          (saw-escape
                           (vector-push-extend ch chars)
                           (case saw-escape
                             (:single
                              (setf saw-escape nil))
                             (:multiple-single
                              (setf saw-escape :multiple)))
                           (incf offset))
                          ((or (alphanumericp ch)
                               (eql ch #\-)
                               (eql ch #\*))
                           (vector-push-extend (char-upcase ch) chars)
                           (incf offset))
                          ((eql ch #\\)
                           (setf saw-escape :single)
                           (incf offset))
                          ((eql ch #\|)
                           (setf saw-escape :multiple)
                           (incf offset))
                          (t
                           (return)))))
               (cond ((string= chars "*")
                      :wild)
                     ((string= chars "**")
                      :wild-inferiors)
                     ((string= chars "")
                      nil)
                     (t chars)))))
      ;; [relative-directory-marker]
      (when (consume-char #\;)
        (setf relative :relative))
      ;; {directory directory-marker}* [name]
      (loop
         (let ((word (consume-word)))
           (when (not word)
             (return))
           (when (not (consume-char #\;))
             ;; This is the name portion.
             (when (eql word :wild-inferiors)
               (error "Unexpected wild-inferiors in name position."))
             (setf name word)
             (return))
           ;; Directory element.
           (push word directories)))
      ;; Possible type & version.
      (when (consume-char #\.)
        (let ((word (or (consume-word)
                        (error "Expected type after type-marker."))))
          (when (eql word :wild-inferiors)
            (error "Unexpected wild-inferiors in type position."))
          (setf type word)
          (when (consume-char #\.)
            (let ((word (or (consume-word)
                            (error "Expected version after version-marker."))))
              (when (eql word :wild-inferiors)
                (error "Unexpected wild-inferiors in version position."))
              (cond ((and (stringp word)
                          (every #'digit-char-p word))
                     (setf version (parse-integer word)))
                    ((and (stringp word)
                          (string= word "NEWEST"))
                     (setf version :newest))
                    (t (setf version word))))))))
    (when (not (eql offset (length namestring)))
      (error "Unexpected ~C in logical namestring." (char namestring offset)))
    (make-instance 'logical-pathname
                   :host host
                   :device :unspecific
                   :directory (list* relative (reverse directories))
                   :name name
                   :type type
                   :version version)))

(defmethod namestring-using-host ((host logical-host) (path logical-pathname))
  (with-output-to-string (namestring)
    (when (eql (first (pathname-directory path)) :relative)
      (write-char #\; namestring))
    (flet ((write-word (word)
             (cond ((mixed-case-p word)
                    (write-char #\| namestring)
                    (loop
                       for ch across word
                       do
                         (when (member ch '(#\\ #\|))
                           (write-char #\\ namestring))
                         (write-char ch namestring))
                    (write-char #\| namestring))
                   (t
                    (write-string word namestring)))))
      (dolist (dir (rest (pathname-directory path)))
        (cond ((eql dir :wild)
               (write-string "*" namestring))
              ((eql dir :wild-inferiors)
               (write-string "**" namestring))
              (t
               (write-word dir)))
        (write-char #\; namestring))
      (let ((name (pathname-name path))
            (type (pathname-type path))
            (version (pathname-version path)))
        (cond ((eql name :wild)
               (write-string "*" namestring))
              (name
               (write-word name)))
        (when type
          (write-char #\. namestring)
          (cond ((eql type :wild)
                 (write-string "*" namestring))
                (t
                 (write-word type)))
          (when version
            (write-char #\. namestring)
            (cond ((eql version :wild)
                   (write-string "*" namestring))
                  ((eql version :newest)
                   (write-string "NEWEST" namestring))
                  (t
                   (write type namestring)))))))))

(defmethod host-default-device ((host logical-host))
  nil)

(defun logical-pathname-translations (host)
  (let ((host (or (find-host host nil)
                  (error "Logical host ~S not yet defined." host))))
    (check-type host logical-host)
    (logical-host-translations host)))

(defun (setf logical-pathname-translations) (new-translations host)
  (let ((logical-host (find-host host nil)))
    (when (not logical-host)
      (check-type host string)
      (setf logical-host (make-instance 'logical-host :name (string-upcase host))
            (find-host host) logical-host))
    (check-type logical-host logical-host)
    (setf (logical-host-translations logical-host) new-translations)))

(defun translate-logical-pathname (pathname &key)
  (setf pathname (pathname pathname))
  (when (not (typep pathname 'logical-pathname))
    (return-from translate-logical-pathname pathname))
  (loop
     with host = (pathname-host pathname)
     for translation in (logical-pathname-translations host)
     for from-wildcard = (parse-namestring (first translation) host)
     for to-wildcard = (pathname (second translation))
     do
       (when (pathname-match-p pathname from-wildcard)
         (return (translate-logical-pathname
                  (translate-pathname pathname from-wildcard to-wildcard))))
     finally
       (error "No matching translation for logical pathname ~S." pathname)))

(defun logical-pathname (pathspec)
  (let ((pathname (pathname pathspec)))
    (check-type pathname logical-pathname)
    pathname))

;; Create the SYS logical host if it doesn't exist.
(when (not (find-host "SYS" nil))
  (setf (logical-pathname-translations "SYS") '()))
