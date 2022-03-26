;;;; impl-util.lisp

(in-package #:ql-impl-util)

(definterface call-with-quiet-compilation (fun)
  (:documentation
   "Call FUN with warnings, style-warnings, and other verbose messages
   suppressed.")
  (:implementation t
    (let ((*load-verbose* nil)
          (*compile-verbose* nil)
          (*load-print* nil)
          (*compile-print* nil))
      (handler-bind ((warning #'muffle-warning))
        (funcall fun)))))

(defimplementation (call-with-quiet-compilation :for sbcl :qualifier :around)
    (fun)
  (declare (ignore fun))
  (handler-bind ((ql-sbcl:compiler-note #'muffle-warning))
    (call-next-method)))

(defimplementation (call-with-quiet-compilation :for cmucl :qualifier :around)
    (fun)
  (declare (ignore fun))
  (let ((ql-cmucl:*gc-verbose* nil))
    (call-next-method)))

(definterface rename-directory (from to)
  (:implementation t
    (rename-file from to)
    (truename to))
  (:implementation cmucl
    (rename-file from (string-right-trim "/" (namestring to)))
    (truename to))
  (:implementation clisp
    (ql-clisp:rename-directory from to)
    (truename to)))

(definterface probe-directory (pathname)
  (:documentation "Return the truename of PATHNAME, if it exists and
  is a directory, or NIL otherwise.")
  (:implementation t
    (let ((directory (probe-file pathname)))
      (when directory
        ;; probe-file is specified to return the truename of the path,
        ;; but Allegro does not return the truename; truenamize it.
        (truename directory))))
  (:implementation clisp
    (let ((directory (ql-clisp:probe-pathname pathname)))
      (when (and directory (ql-clisp:probe-directory directory))
        directory))))

(definterface init-file-name ()
  (:documentation "Return the init file name for the current implementation.")
  (:implementation allegro
    ".clinit.cl")
  (:implementation abcl
    ".abclrc")
  (:implementation ccl
    #+windows
    "ccl-init.lisp"
    #-windows
    ".ccl-init.lisp")
  (:implementation clasp
    ".clasprc")
  (:implementation clisp
    ".clisprc.lisp")
  (:implementation ecl
    ".eclrc")
  (:implementation mezzano
    "init.lisp")
  (:implementation mkcl
    ".mkclrc")
  (:implementation lispworks
    ".lispworks")
  (:implementation sbcl
    ".sbclrc")
  (:implementation cmucl
    ".cmucl-init.lisp")
  (:implementation scl
    ".scl-init.lisp")
  )

(defun init-file-name-for (&optional implementation-designator)
  (let* ((class-name (find-symbol (string-upcase implementation-designator)
                                  'ql-impl))
         (class (find-class class-name nil)))
    (when class
      (let ((*implementation* (make-instance class)))
        (init-file-name)))))

(defun quicklisp-init-file-form ()
  "Return a form suitable for describing the location of the quicklisp
  init file. If the file is available relative to the home directory,
  returns a form that merges with the home directory instead of
  specifying an absolute file."
  (let* ((init-file (ql-setup:qmerge "setup.lisp"))
         (enough (enough-namestring init-file (user-homedir-pathname))))
    (cond ((equal (pathname enough) (pathname init-file))
           ;; The init-file is somewhere outside of the home directory
           (pathname enough))
          (t
           `(merge-pathnames ,enough (user-homedir-pathname))))))

(defun write-init-forms (stream &key (indentation 0))
  (format stream "~%~v@T;;; The following lines added by ql:add-to-init-file:~%"
          indentation)
  (format stream "~v@T#-quicklisp~%" indentation)
  (let ((*print-case* :downcase))
    (format stream "~v@T(let ((quicklisp-init ~S))~%"
            indentation
            (quicklisp-init-file-form)))
  (format stream "~v@T  (when (probe-file quicklisp-init)~%" indentation)
  (format stream "~v@T    (load quicklisp-init)))~%~%" indentation))

(defun suitable-lisp-init-file (implementation)
  "Return the name of IMPLEMENTATION's init file. If IMPLEMENTAION is
a string or pathname, return its merged pathname instead."
  (typecase implementation
    ((or string pathname)
     (merge-pathnames implementation))
    ((or null (eql t))
     (init-file-name))
    (t
     (init-file-name-for implementation))))

(defun add-to-init-file (&optional implementation-or-file)
  "Add forms to the Lisp implementation's init file that will load
quicklisp at CL startup."
  (let ((init-file (suitable-lisp-init-file implementation-or-file)))
    (unless init-file
      (error "Don't know how to add to init file for your implementation."))
    (setf init-file (merge-pathnames init-file (user-homedir-pathname)))
    (format *query-io* "~&I will append the following lines to ~S:~%"
            init-file)
    (write-init-forms *query-io* :indentation 2)
    (when (ql-util:press-enter-to-continue)
      (with-open-file (stream init-file
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :append)
        (write-init-forms stream)))
    init-file))



;;;
;;; Native namestrings.
;;;

(definterface native-namestring (pathname)
  (:documentation "In Clozure CL, #\\.s in pathname-names are escaped
  in namestrings with #\\> on Windows and #\\\\ elsewhere. This can
  cause a problem when using CL:NAMESTRING to store pathname data that
  might be used by other implementations. NATIVE-NAMESTRING is
  intended to provide a namestring that can be parsed as a same-enough
  object on multiple implementations.")
  (:implementation t
    (namestring pathname))
  (:implementation ccl
    (ql-ccl:native-translated-namestring pathname))
  (:implementation sbcl
    (ql-sbcl:native-namestring pathname)))


;;;
;;; Directory write date
;;;

(definterface directory-write-date (pathname)
  (:documentation "Return the write-date of the directory designated
  by PATHNAME as a universal time, like file-write-date.")
  (:implementation t
    (file-write-date pathname))
  (:implementation clisp
    (nth-value 2 (ql-clisp:probe-pathname pathname))))


;;;
;;; Deleting a directory tree
;;;

(defvar *wild-entry*
  (make-pathname :name :wild :type :wild :version :wild))

(defvar *wild-relative*
  (make-pathname :directory '(:relative :wild)))

(definterface directoryp (entry)
  (:documentation "Return true if ENTRY refers to a directory.")
  (:implementation t
    (not (or (pathname-name entry)
             (pathname-type entry))))
  (:implementation allegro
    (ql-allegro:file-directory-p entry))
  (:implementation lispworks
    (ql-lispworks:file-directory-p entry)))

(definterface directory-entries (directory)
  (:documentation "Return all directory entries of DIRECTORY as a
  list, or NIL if there are no directory entries. Excludes the \".\"
  and \"..\" entries.")
  (:implementation allegro
    (directory directory
               #+allegro :directories-are-files
               #+allegro nil
               #+allegro :follow-symbolic-links
               #+allegro nil))
  (:implementation abcl
    (directory (merge-pathnames *wild-entry* directory)
               #+abcl :resolve-symlinks #+abcl nil))
  (:implementation ccl
    (directory (merge-pathnames *wild-entry* directory)
               #+ccl :directories #+ccl t
               #+ccl :follow-links #+ccl nil))
  (:implementation clasp
    (nconc
     (directory (merge-pathnames *wild-entry* directory)
                #+clasp :resolve-symlinks #+clasp nil)
     (directory (merge-pathnames *wild-relative* directory)
                #+clasp :resolve-symlinks #+clasp nil)))
  (:implementation clisp
    ;; :full gives pathnames as well as truenames, BUT: it returns a
    ;; singleton pathname, not a list, on dead symlinks.
    (remove nil
            (mapcar (lambda (entry) (and (listp entry) (first entry)))
                    (nconc (directory (merge-pathnames *wild-entry* directory)
                                      #+clisp :full #+clisp t
                                      #+clisp :if-does-not-exist #+clisp :keep)
                           (directory (merge-pathnames *wild-relative* directory)
                                      #+clisp :full #+clisp t
                                      #+clisp :if-does-not-exist #+clisp :keep)))))
  (:implementation cmucl
    (directory (merge-pathnames *wild-entry* directory)
               #+cmucl :truenamep #+cmucl nil))
  (:implementation scl
    (directory (merge-pathnames *wild-entry* directory)
               #+scl :truenamep #+scl nil))
  (:implementation lispworks
    (directory (merge-pathnames *wild-entry* directory)
               #+lispworks :directories #+lispworks t
               #+lispworks :link-transparency #+lispworks nil))
  (:implementation ecl
    (nconc
     (directory (merge-pathnames *wild-entry* directory)
                #+ecl :resolve-symlinks #+ecl nil)
     (directory (merge-pathnames *wild-relative* directory)
                #+ecl :resolve-symlinks #+ecl nil)))
  (:implementation mezzano
    (directory (merge-pathnames *wild-entry* directory)))
  (:implementation mkcl
    (setf directory (truename directory))
    (nconc
     (directory (merge-pathnames *wild-entry* directory))
     (directory (merge-pathnames *wild-relative* directory))))
  (:implementation sbcl
    (directory (merge-pathnames *wild-entry* directory)
               #+sbcl :resolve-symlinks #+sbcl nil)))

(defimplementation (directory-entries :qualifier :around) (directory)
  ;; Don't return any entries when called with a non-directory
  ;; argument
  (if (directoryp directory)
      (call-next-method)
      (warn "directory-entries - not a directory -- ~S" directory)))

(definterface delete-directory (entry)
  (:documentation "Delete the directory ENTRY. Might signal an error
  if it is not an empty directory.")
  (:implementation t
    (delete-file entry))
  (:implementation allegro
    (ql-allegro:delete-directory entry))
  (:implementation ccl
    (ql-ccl:delete-directory entry))
  (:implementation clasp
    (ql-clasp:rmdir entry))
  (:implementation clisp
    (ql-clisp:delete-directory entry))
  (:implementation cmucl
    (ql-cmucl:unix-rmdir (namestring entry)))
  (:implementation scl
    (ql-scl:unix-rmdir (ql-scl:unix-namestring entry)))
  (:implementation ecl
    (ql-ecl:rmdir entry))
  (:implementation mkcl
    (ql-mkcl:rmdir entry))
  (:implementation lispworks
    (ql-lispworks:delete-directory entry))
  (:implementation sbcl
    (ql-sbcl:rmdir entry)))

(defimplementation (delete-directory :qualifier :around) (directory)
  ;; Don't delete non-directories with delete-directory
  (if (directoryp directory)
      (call-next-method)
      (error "delete-directory - not a directory -- ~A" directory)))

(definterface delete-directory-tree (pathname)
  (:documentation "Delete the directory tree rooted at PATHNAME.")
  (:implementation t
    (let ((directories-to-process (list (truename pathname)))
          (directories-to-delete '()))
      (loop
        (unless directories-to-process
          (return))
        (let* ((current (pop directories-to-process))
               (entries (directory-entries current)))
          (push current directories-to-delete)
          (dolist (entry entries)
            (if (directoryp entry)
                (push entry directories-to-process)
                (delete-file entry)))))
      (map nil 'delete-directory directories-to-delete)))
  (:implementation allegro
    (ql-allegro:delete-directory-and-files pathname))
  (:implementation ccl
    (ql-ccl:delete-directory pathname)))

(defimplementation (delete-directory-tree :qualifier :around) (pathname)
  (if (directoryp pathname)
      (call-next-method)
      (progn
        (warn "delete-directory-tree - not a directory, ~
               deleting anyway -- ~s" pathname)
        (delete-file pathname))))

(defun map-directory-tree (directory fun)
  "Call FUN for every file in directory and all its subdirectories,
recursively. Uses the truename of directory as a starting point. Does
not follow symlinks, but, on some implementations, DOES include
potentially dead symlinks."
  (let ((directories-to-process (list (truename directory))))
    (loop
      (unless directories-to-process
        (return))
      (let* ((current (pop directories-to-process))
             (entries (directory-entries current)))
        (dolist (entry entries)
          (if (directoryp entry)
              (push entry directories-to-process)
              (funcall fun entry)))))))

