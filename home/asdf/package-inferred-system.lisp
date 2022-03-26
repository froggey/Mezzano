;;;; -------------------------------------------------------------------------
;;;; Package systems in the style of quick-build or faslpath

(uiop:define-package :asdf/package-inferred-system
  (:recycle :asdf/package-inferred-system :asdf/package-system :asdf)
  (:use :uiop/common-lisp :uiop
        :asdf/upgrade :asdf/session
        :asdf/component :asdf/system :asdf/system-registry :asdf/lisp-action
        :asdf/parse-defsystem)
  (:export
   #:package-inferred-system #:sysdef-package-inferred-system-search
   #:package-system ;; backward compatibility only. To be removed.
   #:register-system-packages
   #:*defpackage-forms* #:*package-inferred-systems* #:package-inferred-system-missing-package-error))
(in-package :asdf/package-inferred-system)

(with-upgradability ()
  ;; The names of the recognized defpackage forms.
  (defparameter *defpackage-forms* '(defpackage define-package))

  (defun initial-package-inferred-systems-table ()
    ;; Mark all existing packages are preloaded.
    (let ((h (make-hash-table :test 'equal)))
      (dolist (p (list-all-packages))
        (dolist (n (package-names p))
          (setf (gethash n h) t)))
      h))

  ;; Mapping from package names to systems that provide them.
  (defvar *package-inferred-systems* (initial-package-inferred-systems-table))

  (defclass package-inferred-system (system)
    ()
    (:documentation "Class for primary systems for which secondary systems are automatically
in the one-file, one-file, one-system style: system names are mapped to files under the primary
system's system-source-directory, dependencies are inferred from the first defpackage form in
every such file"))

  ;; DEPRECATED. For backward compatibility only. To be removed in an upcoming release:
  (defclass package-system (package-inferred-system) ())

  ;; Is a given form recognizable as a defpackage form?
  (defun defpackage-form-p (form)
    (and (consp form)
         (member (car form) *defpackage-forms*)))

  ;; Find the first defpackage form in a stream, if any
  (defun stream-defpackage-form (stream)
    (loop :for form = (read stream nil nil) :while form
          :when (defpackage-form-p form) :return form))

  (defun file-defpackage-form (file)
    "Return the first DEFPACKAGE form in FILE."
    (with-input-file (f file)
      (stream-defpackage-form f)))

  (define-condition package-inferred-system-missing-package-error (system-definition-error)
    ((system :initarg :system :reader error-system)
     (pathname :initarg :pathname :reader error-pathname))
    (:report (lambda (c s)
               (format s (compatfmt "~@<No package form found while ~
                                     trying to define package-inferred-system ~A from file ~A~>")
                       (error-system c) (error-pathname c)))))

  (defun package-dependencies (defpackage-form)
    "Return a list of packages depended on by the package
defined in DEFPACKAGE-FORM.  A package is depended upon if
the DEFPACKAGE-FORM uses it or imports a symbol from it."
    (assert (defpackage-form-p defpackage-form))
    (remove-duplicates
     (while-collecting (dep)
       (loop* :for (option . arguments) :in (cddr defpackage-form) :do
              (ecase option
                ((:use :mix :reexport :use-reexport :mix-reexport)
                 (dolist (p arguments) (dep (string p))))
                ((:import-from :shadowing-import-from)
                 (dep (string (first arguments))))
                #+sbcl
                ((:local-nicknames)
                 (loop* :for (local-nickname actual-package-name) :in arguments :do
                      (dep (string actual-package-name))))
                ((:nicknames :documentation :shadow :export :intern :unintern :recycle)))))
     :from-end t :test 'equal))

  (defun package-designator-name (package)
    "Normalize a package designator to a string"
    (etypecase package
      (package (package-name package))
      (string package)
      (symbol (string package))))

  (defun register-system-packages (system packages)
    "Register SYSTEM as providing PACKAGES."
    (let ((name (or (eq system t) (coerce-name system))))
      (dolist (p (ensure-list packages))
        (setf (gethash (package-designator-name p) *package-inferred-systems*) name))))

  (defun package-name-system (package-name)
    "Return the name of the SYSTEM providing PACKAGE-NAME, if such exists,
otherwise return a default system name computed from PACKAGE-NAME."
    (check-type package-name string)
    (or (gethash package-name *package-inferred-systems*)
        (string-downcase package-name)))

  ;; Given a file in package-inferred-system style, find its dependencies
  (defun package-inferred-system-file-dependencies (file &optional system)
    (if-let (defpackage-form (file-defpackage-form file))
      (remove t (mapcar 'package-name-system (package-dependencies defpackage-form)))
      (error 'package-inferred-system-missing-package-error :system system :pathname file)))

  ;; Given package-inferred-system object, check whether its specification matches
  ;; the provided parameters
  (defun same-package-inferred-system-p (system name directory subpath around-compile dependencies)
    (and (eq (type-of system) 'package-inferred-system)
         (equal (component-name system) name)
         (pathname-equal directory (component-pathname system))
         (equal dependencies (component-sideway-dependencies system))
         (equal around-compile (around-compile-hook system))
         (let ((children (component-children system)))
           (and (length=n-p children 1)
                (let ((child (first children)))
                  (and (eq (type-of child) 'cl-source-file)
                       (equal (component-name child) "lisp")
                       (and (slot-boundp child 'relative-pathname)
                            (equal (slot-value child 'relative-pathname) subpath))))))))

  ;; sysdef search function to push into *system-definition-search-functions*
  (defun sysdef-package-inferred-system-search (system)
    (let ((primary (primary-system-name system)))
      (unless (equal primary system)
        (let ((top (find-system primary nil)))
          (when (typep top 'package-inferred-system)
            (if-let (dir (component-pathname top))
              (let* ((sub (subseq system (1+ (length primary))))
                     (f (probe-file* (subpathname dir sub :type "lisp")
                                     :truename *resolve-symlinks*)))
                (when (file-pathname-p f)
                  (let ((dependencies (package-inferred-system-file-dependencies f system))
                        (previous (registered-system system))
                        (around-compile (around-compile-hook top)))
                    (if (same-package-inferred-system-p previous system dir sub around-compile dependencies)
                        previous
                        (eval `(defsystem ,system
                                 :class package-inferred-system
                                 :source-file ,(system-source-file top)
                                 :pathname ,dir
                                 :depends-on ,dependencies
                                 :around-compile ,around-compile
                                 :components ((cl-source-file "lisp" :pathname ,sub)))))))))))))))

(with-upgradability ()
  (pushnew 'sysdef-package-inferred-system-search *system-definition-search-functions*)
  (setf *system-definition-search-functions*
        (remove (find-symbol* :sysdef-package-system-search :asdf/package-system nil)
                *system-definition-search-functions*)))
