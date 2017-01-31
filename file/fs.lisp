;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

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
           #:unparse-pathname
           #:unparse-pathname-file
           #:unparse-pathname-directory
           #:open-using-host
           #:directory-using-host
           #:ensure-directories-exist-using-host
           #:rename-file-using-host
           #:file-write-date-using-host
           #:delete-file-using-host
           #:expunge-directory-using-host
           #:file-stream-pathname
           #:simple-file-error
           #:stream-truename
           #:truename-using-host))

(in-package :mezzano.file-system)

(define-condition file-error (error)
  ((pathname :initarg :pathname
             :reader file-error-pathname)))

(define-condition simple-file-error (file-error simple-error)
  ())

(defgeneric file-stream-pathname (stream))

(defvar *valid-hostname-characters* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-")

(defgeneric host-name (host))
(defgeneric host-default-device (host))

(defgeneric host-pathname-class (host)
  (:method (host) (find-class 'pathname)))

(defvar *host-alist* '())

(define-condition unknown-host (error)
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
               (error 'unknown-host :host name)))))
    (t name)))

(defun (setf find-host) (new-value name &optional errorp)
  (setf name (string-upcase (string name)))
  (assert (not (zerop (length name))))
  (cond (new-value
         (setf *host-alist*
               (list* (list name new-value)
                      (remove name *host-alist* :key 'first :test 'string=))))
        (t (setf *host-alist* (remove name *host-alist* :key 'first :test 'string=)))))

(defun list-all-hosts ()
  (mapcar #'second *host-alist*))

(defclass pathname ()
  ((%host :initarg :host :accessor pathname-%host)
   (%device :initarg :device :accessor pathname-%device)
   (%directory :initarg :directory :accessor pathname-%directory)
   (%name :initarg :name :accessor pathname-%name)
   (%type :initarg :type :accessor pathname-%type)
   (%version :initarg :version :accessor pathname-%version))
  (:default-initargs :device nil :directory nil :name nil :type nil :version nil))

(defun pathnamep (object)
  (typep object 'pathname))

;; This should really have a host associated with it...
(defvar *default-pathname-defaults* (make-instance 'pathname :host nil))

(defun make-pathname (&key host
                        (device nil devicep)
                        (directory nil directoryp)
                        (name nil namep)
                        (type nil typep)
                        (version nil versionp)
                        defaults)
  (if defaults
      (setf defaults (pathname defaults))
      (setf defaults (make-instance (host-pathname-class (pathname-host *default-pathname-defaults*))
                                    :host (pathname-host *default-pathname-defaults*))))
  (make-instance (host-pathname-class (if host (find-host host) (pathname-host defaults)))
                 :host (if host (find-host host) (pathname-host defaults))
                 :device (if devicep device (pathname-device defaults))
                 :directory (if directoryp directory (pathname-directory defaults))
                 :name (if namep name (pathname-name defaults))
                 :type (if typep type (pathname-type defaults))
                 :version (if versionp version (pathname-version defaults))))

(defun pathname-host (pathname &key (case :local))
  (pathname-%host (pathname pathname)))
(defun pathname-device (pathname &key (case :local))
  (pathname-%device (pathname pathname)))
(defun pathname-directory (pathname &key (case :local))
  (pathname-%directory (pathname pathname)))
(defun pathname-name (pathname &key (case :local))
  (pathname-%name (pathname pathname)))
(defun pathname-type (pathname &key (case :local))
  (pathname-%type (pathname pathname)))
(defun pathname-version (pathname &key (case :local))
  (pathname-%version (pathname pathname)))

(defmethod make-load-form ((object pathname) &optional environment)
  `(let ((host (find-host ',(host-name (pathname-host object)) t)))
     (make-pathname :host host
                    :device ',(pathname-device object)
                    :directory ',(pathname-directory object)
                    :name ',(pathname-name object)
                    :type ',(pathname-type object)
                    :version ',(pathname-version object))))

(defun sys.int::pathnames-equal (x y)
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

(defun pathname-match-directory (p w)
  (let ((p-dir (pathname-directory p))
        (w-dir (pathname-directory w)))
    (labels ((match (p w)
               (cond
                 ((eql (first w) :wild-inferiors)
                  ;; Eat elements until a match is found or the end of the
                  ;; directory is reached.
                  (loop
                     (when (match p (rest w))
                       (return t))
                     (when (null p)
                       (return nil))
                     (pop p)))
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

(defgeneric unparse-pathname (path host))
(defgeneric unparse-pathname-file (pathname host))
(defgeneric unparse-pathname-directory (pathname host))

(defun host-namestring (pathname)
  (host-name (pathname-host pathname)))

(defun file-namestring (pathname)
  (unparse-pathname-file pathname (pathname-host pathname)))

(defun directory-namestring (pathname)
  (unparse-pathname-directory pathname (pathname-host pathname)))

(defun namestring (pathname)
  (concatenate 'string
               (string (host-name (pathname-host pathname)))
               ":"
               (unparse-pathname pathname (pathname-host pathname))))

(defun enough-namestring (pathname &optional (defaults *default-pathname-defaults*))
  (cond ((eql (pathname-host pathname) (pathname-host defaults))
         (unparse-pathname pathname (pathname-host pathname)))
        (t
         (namestring pathname))))

(defmethod print-object ((object pathname) stream)
  (cond ((pathname-host object)
         (format stream "#P~S" (namestring object)))
        (t (print-unreadable-object (object stream :type t)
             (format stream ":HOST ~S :DEVICE ~S :DIRECTORY ~S :NAME ~S :TYPE ~S :VERSION ~S"
                     (pathname-host object) (pathname-device object)
                     (pathname-directory object) (pathname-name object)
                     (pathname-type object) (pathname-version object))))))

(defun pathname (pathname)
  (cond ((pathnamep pathname)
         pathname)
        ((typep pathname 'file-stream)
         (pathname (file-stream-pathname pathname)))
        (t (parse-namestring pathname))))

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
                                   (rest directory))))
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
  (setf thing (sys.int::follow-synonym-stream thing))
  (check-type thing (or string pathname stream))
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

(defun probe-file (pathspec)
  (let ((stream (open pathspec :direction :probe)))
    (when stream
      (close stream)
      (stream-truename stream))))

(defgeneric directory-using-host (host path &key))

(defun directory (pathspec &rest args &key &allow-other-keys)
  (let ((path (translate-logical-pathname (merge-pathnames pathspec))))
    (apply #'directory-using-host (pathname-host path) path args)))

(defun case-correct-path-component (component from-host to-host)
  (cond ((and (typep from-host 'logical-host)
              (not (typep to-host 'logical-host))
              (stringp component))
         (string-downcase component))
        ((and (not (typep from-host 'logical-host))
              (typep to-host 'logical-host)
              (stringp component))
         (string-upcase component))
        (t component)))

(defun translate-one (source from to what)
  (cond ((member (funcall what to) '(nil :wild))
         (case-correct-path-component (funcall what source)
                                      (pathname-host from)
                                      (pathname-host to)))
        ((or (eql (funcall what source) (funcall what from))
             (eql (funcall what from) :wild))
         (case-correct-path-component (funcall what source)
                                      (pathname-host from)
                                      (pathname-host to)))
        (t (error "Source and from ~S don't match." what))))

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
                 :device (translate-one source from-wildcard to-wildcard 'pathname-device)
                 :name (translate-one source from-wildcard to-wildcard 'pathname-name)
                 :type (translate-one source from-wildcard to-wildcard 'pathname-type)
                 :version (translate-one source from-wildcard to-wildcard 'pathname-version)
                 :directory (translate-directory source from-wildcard to-wildcard)))

(defgeneric ensure-directories-exist-using-host (host pathname &key verbose))

(defun ensure-directories-exist (pathspec &rest keys &key verbose &allow-other-keys)
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

(defgeneric file-write-date-using-host (host path))

(defun file-write-date (pathspec)
  (let ((path (translate-logical-pathname (merge-pathnames pathspec))))
    (assert (not (wild-pathname-p path)))
    (file-write-date-using-host (pathname-host path) path)))

(defgeneric file-author-using-host (host path))

(defun file-author (pathspec)
  (let ((path (translate-logical-pathname (merge-pathnames pathspec))))
    (assert (not (wild-pathname-p path)))
    (file-author-using-host (pathname-host path) path)))

(defgeneric delete-file-using-host (host path &key))

(defun delete-file (filespec &rest args &key &allow-other-keys)
  (let ((path (translate-logical-pathname (merge-pathnames filespec))))
    (assert (not (wild-pathname-p path)))
    (apply #'delete-file-using-host (pathname-host path) path args)))

(defgeneric expunge-directory-using-host (host path &key))

(defun expunge-directory (filespec &rest args &key &allow-other-keys)
  (let ((path (translate-logical-pathname (merge-pathnames filespec))))
    (assert (not (wild-pathname-p path)))
    (apply #'expunge-directory-using-host (pathname-host path) path args)))

(defvar *home-directory* nil)

(defun user-homedir-pathname (&optional host)
  (if (not (member host '(nil :unspecific)))
      nil
      *home-directory*))

;;; Logical pathnames.

(defclass logical-host ()
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
                                      :adjustable t)))
               (loop
                  (when (>= offset (length namestring))
                    (return))
                  (let ((ch (char namestring offset)))
                    (cond ((or (alphanumericp ch)
                               (eql ch #\-)
                               (eql ch #\*))
                           (vector-push-extend (char-upcase ch) chars)
                           (incf offset))
                          (t (return)))))
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

(defmethod unparse-pathname ((path logical-pathname) (host logical-host))
  (with-output-to-string (namestring)
    (when (eql (first (pathname-directory path)) :relative)
      (write-char #\; namestring))
    (dolist (dir (rest (pathname-directory path)))
      (cond ((eql dir :wild)
             (write-string "*" namestring))
            ((eql dir :wild-inferiors)
             (write-string "**" namestring))
            (t
             (write-string dir namestring)))
      (write-char #\; namestring))
    (let ((name (pathname-name path))
          (type (pathname-type path))
          (version (pathname-version path)))
      (cond ((eql name :wild)
             (write-string "*" namestring))
            (name
             (write-string name namestring)))
      (when type
        (write-char #\. namestring)
        (cond ((eql type :wild)
               (write-string "*" namestring))
              (t
               (write-string type namestring)))
        (when version
          (write-char #\. namestring)
          (cond ((eql version :wild)
                 (write-string "*" namestring))
                ((eql version :newest)
                 (write-string "NEWEST" namestring))
                (t
                 (write type namestring))))))))

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
