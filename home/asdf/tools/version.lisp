(in-package :asdf-tools)

;;; Extracting version information

(defparameter *version-tag-glob* "[0-9][.][0-9]*") ;; NB: it's a glob, not regex

(defun version-from-tag (&optional commit)
  ;; run-program issue: :output :line closes the fd, which causes the program to die in error.
  (first (nth-value 2 (git `(describe --tags --match ,*version-tag-glob* ,commit) :output :lines))))

(defun version-from-file (&optional commit)
  (if commit
      (nth-value 2 (git `(show (,commit":version.lisp-expr")) :output :form))
      (safe-read-file-form (pn "version.lisp-expr"))))

(defun debian-version-from-file (&optional commit)
  (match (if commit
             ;; run-program issue: :output :line closes the fd, which causes the program to die in error.
             (first (nth-value 2 (git `(show (,commit":debian/changelog"))
                                      :output :lines :error-output nil)))
             (read-file-line (pn "debian/changelog")))
    ((ppcre "^[^(]*\\(([-0-9.:-]+)\\)" ver) ver)))

(defun parse-debian-version (&optional (debian-version (debian-version-from-file)))
  (cl-ppcre:register-groups-bind (epoch ver rel)
      ("^(?:([0-9]+):)?([0-9.]+)(?:-([0-9]+))$" debian-version)
    ;; NB: (A) we return version first, not epoch, because it's the primary result!
    ;; (B) epoch = nil is semantically same as epoch = 0
    ;; (C) rel = nil is for debian-native packages, e.g. base-passwd or cowbuilder
    (values ver epoch rel)))

(defun debian-version-string (ver epoch rel)
  (format nil "~@[~D:~]~A~@[-~A~]" epoch ver rel))

(defun debian-version-without-epoch (version-string)
  (multiple-value-bind (ver epoch rel) (parse-debian-version version-string)
    (declare (ignore epoch))
    (debian-version-string ver nil rel)))

;;; Bumping the version of ASDF

(defparameter *versioned-files*
  '(("version.lisp-expr" "\"" "\"")
    ("uiop/version.lisp" "(defparameter *uiop-version* \"" "\")")
    ("asdf.asd" "  :version \"" "\" ;; to be automatically updated by make bump-version")
    ("header.lisp" "This is ASDF " ": Another System Definition Facility.")
    ("upgrade.lisp" "   (asdf-version \"" "\")")
    ("doc/asdf.texinfo" "Manual for Version " ""))) ;; NB: two occurrences with this pattern.


(defparameter *old-version* :default)
(defparameter *new-version* :default)

(defun compute-next-version (v)
  (let ((pv (parse-version v 'error)))
    (assert (first pv))
    (assert (second pv))
    (unless (third pv) (appendf pv (list 0)))
    (unless (fourth pv) (appendf pv (list 0)))
    (incf (car (last pv)))
    (unparse-version pv)))

(defun versions-from-args (&optional v1 v2)
  (labels ((check (old new)
             (parse-version old 'error)
             (parse-version new 'error)
             (values old new)))
    (cond
      ((and v1 v2) (check v1 v2))
      (v1 (check (version-from-file) v1))
      ((not (eq *new-version* :default)) *new-version*) ;; Ugly passing of argument from Makefile.
      (t (let ((old (version-from-file)))
           (check old (compute-next-version old)))))))

(deftype byte-vector () '(array (unsigned-byte 8) (*)))

(defun maybe-replace-file (file transformer
                           &key (reader 'read-file-string)
                             (writer nil) (comparator 'equalp)
                             (external-format *utf-8-external-format*))
  (format t "Transforming file ~A... " (file-namestring file))
  (let* ((old-contents (funcall reader file))
         (new-contents (funcall transformer old-contents)))
    (if (funcall comparator old-contents new-contents)
        (format t "no changes needed!~%")
        (let ((written-contents
                (if writer
                    (with-output (s ())
                      (funcall writer s new-contents))
                    new-contents)))
          (check-type written-contents (or string (byte-vector)))
          (clobber-file-with-vector file written-contents :external-format external-format)
          (format t "done.~%"))))
  (success))

(defun version-transformer (new-version file prefix suffix &optional dont-warn)
  (let* ((qprefix (cl-ppcre:quote-meta-chars prefix))
         (versionrx "([0-9]+(\\.[0-9]+)+)")
         (qsuffix (cl-ppcre:quote-meta-chars suffix))
         (regex (strcat "(" qprefix ")(" versionrx ")(" qsuffix ")"))
         (replacement
           (constantly (strcat prefix new-version suffix))))
    (lambda (text)
      (multiple-value-bind (new-text foundp)
          (cl-ppcre:regex-replace-all regex text replacement)
        (unless (or foundp dont-warn)
          (warn "Missing version in ~A" (file-namestring file)))
        (values new-text foundp)))))

(defun transform-file (new-version file prefix suffix)
  (maybe-replace-file (pn file) (version-transformer new-version file prefix suffix)))

(defun transform-files (new-version)
  (loop :for f :in *versioned-files* :do (apply 'transform-file new-version f))
  (success))

(defun test-transform-file (new-version file prefix suffix)
  (let ((lines (read-file-lines (pn file))))
    (dolist (l lines (progn (warn "Couldn't find a match in ~A" file) nil))
      (multiple-value-bind (new-text foundp)
          (funcall (version-transformer new-version file prefix suffix t) l)
        (when foundp
          (format t "Found a match:~%  ==> ~A~%Replacing with~%  ==> ~A~%~%"
                  l new-text)
          (return (success)))))))

(defun test-transform (new-version)
  (apply 'test-transform-file new-version (first *versioned-files*)))

(defun bump-version (&optional v1 v2)
  "bump asdf version, do not commit"
  (with-asdf-dir ()
    (multiple-value-bind (old-version new-version)
        (versions-from-args v1 v2)
      (format t "Bumping ASDF version from ~A to ~A~%" old-version new-version)
      (transform-files new-version)
      (println "Rebuilding ASDF with bumped version")
      (build-asdf)
      new-version)))

(defun bump (&optional v1 v2)
  "bump asdf version, then commit and tag"
  (let ((v (bump-version v1 v2)))
    (git `(commit -a -m ("Bump version to ",v)))
    (git `(tag ,v))
    v))

