;;;; bundle.lisp

(in-package #:ql-bundle)

;;; Bundling is taking a set of Quicklisp-provided systems and
;;; creating a directory structure and metadata in which those systems
;;; can be loaded without involving Quicklisp.
;;;
;;; This works for systems provided directly Quicklisp, or systems in
;;; the Quicklisp local-projects directories (if
;;; :include-local-projects is specified).

(defgeneric find-system (system bundle))
(defgeneric add-system (system bundle))
(defgeneric ensure-system (system bundle))

(defgeneric find-release (relase bundle))
(defgeneric add-release (release bundle))
(defgeneric ensure-release (release bundle))

(defgeneric write-loader-script (bundle stream))
(defgeneric write-system-index (bundle stream))

(defgeneric unpack-release (release target))
(defgeneric unpack-releases (bundle target))

(defgeneric write-bundle (bundle target))

(defvar *ignored-systems*
  (list "asdf")
  "Systems that might appear in depends-on lists in Quicklisp, but
  which can't be bundled.")

(defvar *bundle-progress-output*
  (make-synonym-stream '*trace-output*)
  "Informative output related to creating the bundle is sent to this
  stream.")

;;; Implementation

;;; Conditions

(define-condition bundle-error (error) ())

(define-condition object-not-found (bundle-error)
  ((name
    :initarg :name
    :reader object-not-found-name)
   (type
    :initarg :type
    :reader object-not-found-type))
  (:report
   (lambda (condition stream)
     (format stream "~A ~S not found"
             (object-not-found-type condition)
             (object-not-found-name condition))))
  (:default-initargs
   :type "Object"))

(define-condition system-not-found (object-not-found)
  ((name
    :reader system-not-found-system))
  (:default-initargs
   :type "System"))

(define-condition release-not-found (object-not-found)
  ()
  (:default-initargs
   :type "Release"))

(define-condition bundle-directory-exists (bundle-error)
  ((directory
    :initarg :directory
    :reader bundle-directory-exists-directory))
  (:report
   (lambda (condition stream)
     (format stream "Bundle directory ~A already exists"
             (bundle-directory-exists-directory condition)))))


(defun iso8601-time-stamp (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time time 0)
     (format nil "~4,'0D-~2,'0D-~2,'0DT~
                  ~2,'0D:~2,'0D:~2,'0DZ"
             year month day
             hour minute second)))


(defclass bundle ()
  ((requested-systems
    :initarg :requested-systems
    :reader requested-systems
    :documentation "Names of the systems requested directly for
    bundling.")
   (creation-time
    :initarg :creation-time
    :reader creation-time)
   (release-table
    :initarg :release-table
    :reader release-table)
   (system-table
    :initarg :system-table
    :reader system-table))
  (:default-initargs
   :requested-systems nil
   :creation-time (iso8601-time-stamp)
   :release-table (make-hash-table :test 'equalp)
   :system-table (make-hash-table :test 'equalp)))

(defmethod print-object ((bundle bundle) stream)
  (print-unreadable-object (bundle stream :type t)
    (format stream "~D release~:P, ~D system~:P"
            (hash-table-count (release-table bundle))
            (hash-table-count (system-table bundle)))))

(defmethod provided-releases ((bundle bundle))
  (let ((releases '()))
    (maphash (lambda (name release)
               (declare (ignore name))
               (push release releases))
             (release-table bundle))
    (sort releases 'string< :key 'name)))

(defmethod provided-systems ((bundle bundle))
  (sort (mapcan #'provided-systems (provided-releases bundle))
        'string<
        :key 'name))

(defmethod find-system (name (bundle bundle))
  (values (gethash name (system-table bundle))))

(defmethod add-system (name (bundle bundle))
  (let ((system (ql-dist:find-system name)))
    (unless system
      (error 'system-not-found
             :name name))
    (ensure-release (name (release system)) bundle)
    system))

(defmethod ensure-system (name (bundle bundle))
  (or (find-system name bundle)
      (add-system name bundle)))

(defmethod find-release (name (bundle bundle))
  (values (gethash name (release-table bundle))))

(defmethod add-release (name (bundle bundle))
  (let ((release (ql-dist:find-release name)))
    (unless release
      (error 'release-not-found
             :name name))
    (setf (gethash (name release) (release-table bundle)) release)
    (let ((system-table (system-table bundle)))
      (dolist (system (provided-systems release))
        (setf (gethash (name system) system-table) system)))
    release))

(defmethod ensure-release (name (bundle bundle))
  (or (find-release name bundle)
      (add-release name bundle)))


(defun add-systems-recursively (names bundle)
  (with-consistent-dists
    (labels ((add-one (name)
               (unless (member name *ignored-systems* :test 'equalp)
                 (let ((system
                        (restart-case
                            (ensure-system name bundle)
                          (omit ()
                            :report "Ignore this system and omit it from the bundle."))))
                   (when system
                     (dolist (required-system-name (required-systems system))
                       (add-one required-system-name)))))))
      (map nil #'add-one names)))
  bundle)


(defmethod unpack-release (release target)
  (let ((*default-pathname-defaults* (truename
                                      (ensure-directories-exist target)))
        (archive (ensure-local-archive-file release))
        (temp-tar (ensure-directories-exist
                   (ql-setup:qmerge "tmp/bundle.tar"))))
    (ql-gunzipper:gunzip archive temp-tar)
    (ql-minitar:unpack-tarball temp-tar :directory "software/")
    (delete-file temp-tar)
    release))

(defmethod unpack-releases ((bundle bundle) target)
  (dolist (release (provided-releases bundle))
    (unpack-release release target))
  bundle)

(defmethod write-system-index ((bundle bundle) stream)
  (dolist (release (provided-releases bundle))
    ;; Working with strings, here, intentionally not with pathnames
    (let ((prefix (concatenate 'string "software/" (prefix release))))
      (dolist (system-file (system-files release))
        (format stream "~A/~A~%" prefix system-file)))))

(defmethod write-loader-script ((bundle bundle) stream)
  (let ((template-lines
         (load-time-value
          (with-open-file (stream #. (merge-pathnames "bundle-template"
                                                      (or *compile-file-truename*
                                                          *load-truename*)))
            (loop for line = (read-line stream nil)
                  while line collect line)))))
    (dolist (line template-lines)
      (write-line line stream))))

(defun coerce-to-directory (pathname)
  ;; Cribbed from quicklisp-bootstrap/quicklisp.lisp
  (let ((name (file-namestring pathname)))
    (if (or (null name)
            (equal name ""))
        pathname
        (make-pathname :defaults pathname
                       :name nil
                       :type nil
                       :directory (append (pathname-directory pathname)
                                          (list name))))))

(defun bundle-metadata-plist (bundle)
  (list :creation-time (creation-time bundle)
        :requested-systems (requested-systems bundle)
        :lisp-info (list :machine-instance (machine-instance)
                         :machine-type (machine-type)
                         :machine-version (machine-version)
                         :lisp-implementation-type (lisp-implementation-type)
                         :lisp-implementation-version (lisp-implementation-version))
        :quicklisp-info (list :home (namestring ql:*quicklisp-home*)
                              :local-project-directories
                              (mapcar 'namestring ql:*local-project-directories*)
                              :dists
                              (loop for dist in (enabled-dists)
                                    collect (list :name (name dist)
                                                  :dist-url
                                                  (canonical-distinfo-url dist)
                                                  :version (version dist))))))

(defmethod write-bundle ((bundle bundle) target)
  (unpack-releases bundle target)
  (let ((index-file (merge-pathnames "system-index.txt" target))
        (loader-file (merge-pathnames "bundle.lisp" target))
        (local-projects (merge-pathnames "local-projects/" target))
        (metadata-file (merge-pathnames "bundle-info.sexp" target)))
    (ensure-directories-exist local-projects)
    (with-open-file (stream index-file :direction :output
                            :if-exists :supersede)
      (write-system-index bundle stream))
    (with-open-file (stream loader-file :direction :output
                            :if-exists :supersede)
      (write-loader-script bundle stream))
    (with-open-file (stream metadata-file :direction :output
                            :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*print-pretty* t))
          (prin1 (bundle-metadata-plist bundle) stream)
          (terpri stream))))
    (probe-file loader-file)))


(defun copy-file (from-file to-file)
  (with-open-file (from-stream from-file :element-type '(unsigned-byte 8)
                               :if-does-not-exist nil)
    (when from-stream
      (let ((buffer (make-array 10000 :element-type '(unsigned-byte 8))))
        (with-open-file (to-stream to-file
                                   :direction :output
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
          (loop
            (let ((end-index (read-sequence buffer from-stream)))
              (when (zerop end-index)
                (return to-file))
              (write-sequence buffer to-stream :end end-index))))))))

(defun copy-directory-tree (from-directory to-directory)
  ;; Use the truename here to ensure that relative pathnames match up
  ;; properly. For example, on SBCL, "~/foo/bar/" entries are not
  ;; relative to "/home/baz/foo/bar/" entries.
  (setf from-directory (truename from-directory))
  (map-directory-tree
   from-directory
   (lambda (from-pathname)
     (when (probe-file from-pathname)
       (let* ((relative (enough-namestring from-pathname from-directory))
              (relative-directory (pathname-directory relative))
              (to-pathname (merge-pathnames relative to-directory)))
         (unless (or (null relative-directory)
                     (eql (first relative-directory)
                          :relative))
           (error "Expected relative pathname to copy from ~A ~
                   - bad symlink? - ~S"
                  from-pathname
                  relative))
         (ensure-directories-exist to-pathname)
         (copy-file from-pathname to-pathname))))))

(defun copy-local-projects-directories (local-projects-directories
                                        to-directory)
  "Copy the local-projects directories to TO-DIRECTORY. Each one gets
  a distinct subdirectory."
  (loop for prefix from 0
        for prefix-directory = (make-pathname :defaults *quicklisp-home*
                                              :directory
                                              (list :relative
                                                    (format nil "~4,'0X" prefix)))
        for from-directory in local-projects-directories
        for real-to-directory = (merge-pathnames prefix-directory to-directory)
        do
        (format *bundle-progress-output*
                "~&; Copying ~A to bundle..." from-directory )
        (force-output *bundle-progress-output*)
        (ensure-directories-exist real-to-directory)
        (copy-directory-tree from-directory real-to-directory)
        (format *bundle-progress-output* "done.~%")
        (force-output *bundle-progress-output*)))


(defun ql:bundle-systems (system-names
                          &key include-local-projects to (overwrite t))
  "In the directory TO, construct a self-contained bundle of libraries
based on SYSTEM-NAMES. For each system named, and its recursive
required systems, unpack its release archive in TO/software/, and
write a system index, compatible with the output of
QL:WRITE-ASDF-MANIFEST-FILE, to TO/system-index.txt. Write a loader
script to TO/bundle.lisp that, when loaded via CL:LOAD, configures
ASDF to load systems from the bundle before any other system.

SYSTEM-NAMES must name systems provided directly by Quicklisp.

If INCLUDE-LOCAL-PROJECTS is true, each directory in
QL:*LOCAL-PROJECT-DIRECTORIES* is copied into the bundle and loaded
before any of the other bundled systems."
  (unless to
    (error "TO argument must be provided"))
  (let* ((bundle (make-instance 'bundle
                                :requested-systems system-names))
         (to (coerce-to-directory to))
         (software (merge-pathnames "software/" to)))
    (when (and (probe-directory to)
               (not overwrite))
      (cerror "Overwrite it"
              'bundle-directory-exists
              :directory to))
    (when (probe-directory software)
      (delete-directory-tree software))
    (add-systems-recursively system-names bundle)
    (let ((bundled-local-projects (merge-pathnames "bundled-local-projects/"
                                                   to)))
      (when include-local-projects
        (when (probe-directory bundled-local-projects)
          (delete-directory-tree bundled-local-projects))
        (copy-local-projects-directories ql:*local-project-directories*
                                         bundled-local-projects)
        (ensure-directories-exist bundled-local-projects)
        (ql::make-system-index bundled-local-projects)))
    (values (write-bundle bundle to)
            bundle)))
