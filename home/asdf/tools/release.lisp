(in-package :asdf-tools)

;;; Getting a list of source files in a system

(defun enough-namestring! (base pathname)
  (let ((e (enough-namestring base pathname)))
    (assert (relative-pathname-p e))
    e))

(defun enough-namestrings (base pathnames)
  (loop :with b = (ensure-pathname base :want-absolute t :want-directory t)
        :for p :in pathnames
        :collect (enough-namestring! p b)))

(defun system-source-files (system &key monolithic)
  (let ((system (find-system system)))
    (enough-namestrings
     (system-source-directory system)
     (input-files (if monolithic
		      'monolithic-concatenate-source-op
		      'concatenate-source-op)
		  system))))


;;; Making release tarballs for asdf, asdf/defsystem, uiop.

(defun tarname (name) (strcat name ".tar.gz"))

(defun make-tarball-under-build (name base files)
  (check-type name string)
  (ensure-pathname base :want-absolute t :want-existing t :want-directory t)
  (dolist (f files)
    (check-type f string))
  (let* ((base
           (ensure-pathname
            base
            :want-absolute t :want-directory t
            :want-existing t :truename t))
         (destination
           (ensure-pathname
            name
            :defaults (build-dir)
            :want-relative t :ensure-absolute t
            :ensure-subpath t :ensure-directory t))
         (tarball
           (ensure-pathname
            (tarname name)
            :defaults (build-dir)
            :want-relative t :ensure-absolute t
            :ensure-subpath t :want-file t
            :ensure-directories-exist t)))
    (assert (< 6 (length (pathname-directory destination))))
    (when (probe-file* destination)
      (error "Destination ~S already exists, not taking chances - you can delete it yourself."
             destination))
    (ensure-directories-exist destination)
    (run* `(cp "-pHux" --parents ,@files ,destination) :directory base :show t)
    (run* `(tar "zcf" ,tarball "-C" ,(build-dir)
                ;; TODO: Have better autodetection for which tar is being used,
                ;; and fall back to no option if not recognized.
                ;; #+linux (* :owner root :group root) ;; assume GNU tar on Linux.
                ;; #+darwin (* :uid 0 :gid 0) ;; assume BSD tar on Darwin.
               (,name /)) :show t)
    (delete-directory-tree destination :validate #'(lambda (x) (equal x destination)))
    (success)))

(defun uiop-files ()
  "list files in uiop"
  (let ((*asdf-version* "3")) ;; prevent check-not-old-asdf-system from hiding uiop.asd.
    (list* "README.md" "uiop.asd" "asdf-driver.asd" "contrib/debug.lisp"
           (system-source-files "uiop"))))
(defun uiop-name ()
  (format nil "uiop-~A" (version-from-file)))
(deftestcmd make-uiop-tarball ()
  (make-tarball-under-build (uiop-name) (uiop-dir) (uiop-files)))

(defun asdf-defsystem-files ()
  "list files in asdf/defsystem"
  (list* "build/asdf.lisp" ;; for bootstrap purposes
         "asdf.asd" "version.lisp-expr" "header.lisp"
         (system-source-files "asdf/defsystem")))
(defun asdf-defsystem-name ()
  (format nil "asdf-defsystem-~A" (version-from-file)))
(deftestcmd make-asdf-defsystem-tarball ()
  (build-asdf)
  (make-tarball-under-build (asdf-defsystem-name) (asdf-dir) (asdf-defsystem-files)))

(defun asdf-all-name ()
  (strcat "asdf-" (version-from-file)))
(defun asdf-all-files ()
  (remove-if #'(lambda (x) (string-prefix-p "ext/" x))
             (with-asdf-dir () (run/lines '(git ls-files)))))
(deftestcmd make-asdf-all-tarball ()
  (build-asdf)
  (make-tarball-under-build (asdf-all-name) (asdf-dir) (asdf-all-files)))

(defun asdf-lisp-name ()
  (format nil "asdf-~A.lisp" (version-from-file)))
(deftestcmd make-asdf-lisp ()
  (build-asdf)
  (concatenate-files (list (pn "build/asdf.lisp"))
                     (pn "build/" (asdf-lisp-name))))

(deftestcmd make-archive ()
  "build tarballs for release"
  (make-uiop-tarball)
  (make-asdf-defsystem-tarball)
  (make-asdf-all-tarball)
  (make-asdf-lisp))


;;; Publishing tarballs onto the public repository

(defvar *clnet* "common-lisp.net")
(defvar *clnet-asdf-public* "/project/asdf/public_html/")
(defun public-path (x) (strcat *clnet-asdf-public* x))

(deftestcmd publish-archive ()
  "publish tarballs to the website"
  (let ((tarballs (mapcar 'tarname (list (uiop-name) (asdf-defsystem-name) (asdf-all-name)))))
    (run* `(rsync "--times" "--chmod=a+rX,ug+w"
                  ,@tarballs ,(asdf-lisp-name) (,*clnet* ":" ,(public-path "archives/")))
          :show t :directory (pn "build/")))
  (format t "~&To download the tarballs, point your browser at:~%
        http://common-lisp.net/project/asdf/archives/
~%"))

(deftestcmd link-archive ()
  "symlink new tarballs on the website"
  (run* (format nil "ln -sf ~S ~S ; ln -sf ~S ~S ; ln -sf ~S ~S ; ln -sf ~S ~S"
                (tarname (uiop-name))
                (public-path "archives/uiop.tar.gz")
                (tarname (asdf-defsystem-name))
                (public-path "archives/asdf-defsystem.tar.gz")
                (tarname (asdf-all-name))
                (public-path "archives/asdf.tar.gz")
                (asdf-lisp-name)
                (public-path "archives/asdf.lisp"))
        :show t :host *clnet*))

(deftestcmd make-and-publish-archive ()
  "make and publish tarballs"
  (make-archive)
  (publish-archive)
  (link-archive))

(defalias archive make-and-publish-archive)
(defalias install make-and-publish-archive)

(defun get-debian-packager-keyid ()
  (or (getenv "DEBSIGN_KEYID")
      (error "Please export variable DEBSIGN_KEYID to be the 8-hex hash of your GnuPG secret key")))

;;; Making a debian package
(deftestcmd debian-package ((release "release"))
  "build a debian package"
  (let* ((debian-version (debian-version-from-file release))
         (version (version-from-file release))
         (has-ext-p (probe-file* (pn "ext/inferior-shell/inferior-shell.asd")))
         (branch (get-git-branch))
         (keyid (get-debian-packager-keyid)))
    (unless (equal version (parse-debian-version debian-version))
      (error "Debian version ~A doesn't match asdf version ~A" debian-version version))
    (git-all-committed-p)
    (clean)
    (ext-clear)
    (git '(checkout release))
    (format t "building package version ~A~%" (debian-version-from-file))
    (run* `(git-buildpackage
            ;; --git-ignore-new ;; for testing purpose
            ;; Override of the signing key, don't extract the name from the Changelog:
            (--git-builder="debuild -k0x",keyid" -i -I")
            (--git-debian-branch= ,release)
            (--git-upstream-tag="%(version)s")
            ;;--git-upstream-tree=tag ;; if the changelog says 3.1.2, looks at that tag
            ;;(--git-upstream-branch= ,version) ;; if the changelog says 3.1.2, looks at that tag
            --git-tag --git-retag
            ;; --git-no-pristine-tar
            --git-force-create
            --git-ignore-branch)
          :directory (pn) :show t)
    (git `(checkout ,branch))
    (when has-ext-p (ext-reset))
    (success)))

(defun debian-architecture ()
  (run/ss `(dpkg --print-architecture)))

(deftestcmd publish-debian-package (release)
  "publish a debian package"
  (let ((changes (strcat "cl-asdf_" (debian-version-without-epoch (debian-version-from-file release))
                         "_" (debian-architecture) ".changes")))
    (run* `(dput mentors ,(pn "../" changes)))))

(deftestcmd release (new-version lisps scripts systems)
  "all steps to release the code (NOT YET IMPLEMENTED)"
  (break) ;; for each function, offer to do it or not (?)
  (with-asdf-dir ()
    (let ((log (newlogfile "release" "all"))
          (releasep (= (length (parse-version new-version)) 3)))
      (when releasep
        (let ((debian-version (debian-version-from-file)))
          (unless (equal new-version (parse-debian-version debian-version))
            (error "You're trying to release version ~A but the debian/changelog wasn't properly updated"
                   new-version)))
        (when (nth-value 1 (run '(dpkg-parsechangelog) :output nil :error-output :lines))
          (error "Malformed debian/changelog entry")))
      scripts ;; TODO: needs to be passed as argument!
      (git-all-committed-p)
      (test-all-no-stop) ;; TODO: NEED ARGUMENTS!
      (test-load-systems lisps systems)
      (bump new-version)
      (when releasep
        (and
         (debian-package)
         (publish-debian-package)
         (merge-master-into-release)))
      ;; SUCCESS! now publish more widely
      (%push)
      (archive)
      (website)
      (when releasep
        (log! log t "Don't forget to send a debian mentors request!"))
      (log! log "Don't forget to send announcement to asdf-announce, asdf-devel, etc.")
      (log! log "Don't forget to move all fixed bugs from Fix Committed -> Fix Released on launchpad"))))
