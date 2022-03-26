;;;; client.lisp

(in-package #:quicklisp-client)

(defvar *quickload-verbose* nil
  "When NIL, show terse output when quickloading a system. Otherwise,
  show normal compile and load output.")

(defvar *quickload-prompt* nil
  "When NIL, quickload systems without prompting for enter to
  continue, otherwise proceed directly without user intervention.")

(defvar *quickload-explain* t)

(define-condition system-not-quickloadable (error)
  ((system
    :initarg :system
    :reader not-quickloadable-system)))

(defun maybe-silence (silent stream)
  (or (and silent (make-broadcast-stream)) stream))

(defgeneric quickload (systems &key verbose silent prompt explain &allow-other-keys)
  (:documentation
   "Load SYSTEMS the quicklisp way. SYSTEMS is a designator for a list
   of things to be loaded.")
  (:method (systems &key
            (prompt *quickload-prompt*)
            (silent nil)
            (verbose *quickload-verbose*) &allow-other-keys)
    (let ((*standard-output* (maybe-silence silent *standard-output*))
          (*trace-output*    (maybe-silence silent *trace-output*)))
      (unless (consp systems)
        (setf systems (list systems)))
      (dolist (thing systems systems)
        (flet ((ql ()
                 (autoload-system-and-dependencies thing :prompt prompt)))
          (if verbose
              (ql)
              (call-with-quiet-compilation #'ql)))))))

(defmethod quickload :around (systems &key verbose prompt explain
                                      &allow-other-keys)
  (declare (ignorable systems verbose prompt explain))
  (with-consistent-dists
    (call-next-method)))

(defun system-list ()
  (provided-systems t))

(defun update-dist (dist &key (prompt t))
  (when (stringp dist)
    (setf dist (find-dist dist)))
  (let ((new (available-update dist)))
    (cond (new
           (show-update-report dist new)
           (when (or (not prompt) (press-enter-to-continue))
             (update-in-place dist new)))
          ((not (subscribedp dist))
           (format t "~&You are not subscribed to ~S."
                   (name dist)))
          (t
           (format t "~&You already have the latest version of ~S: ~A.~%"
                   (name dist)
                   (version dist))))))

(defun update-all-dists (&key (prompt t))
  (let ((dists (remove-if-not 'subscribedp (all-dists))))
    (format t "~&~D dist~:P to check.~%" (length dists))
    (dolist (old dists)
      (with-simple-restart (skip "Skip update of dist ~S" (name old))
        (update-dist old :prompt prompt)))))

(defun available-dist-versions (name)
  (available-versions (find-dist-or-lose name)))

(defun help ()
  "For help with Quicklisp, see http://www.quicklisp.org/beta/")

(defun uninstall (system-name)
  (let ((system (find-system system-name)))
    (cond (system
           (ql-dist:uninstall system))
          (t
           (warn "Unknown system ~S" system-name)
           nil))))

(defun uninstall-dist (name)
  (let ((dist (find-dist name)))
    (when dist
      (ql-dist:uninstall dist))))

(defun write-asdf-manifest-file (output-file &key (if-exists :rename-and-delete)
                                               exclude-local-projects)
  "Write a list of system file pathnames to OUTPUT-FILE, one per line,
in order of descending QL-DIST:PREFERENCE."
  (when (or (eql output-file nil)
            (eql output-file t))
    (setf output-file (qmerge "manifest.txt")))
  (with-open-file (stream output-file
                          :direction :output
                          :if-exists if-exists)
    (unless exclude-local-projects
      (register-local-projects)
      (dolist (system-file (list-local-projects))
        (let* ((enough (enough-namestring system-file output-file))
               (native (native-namestring enough)))
          (write-line native stream))))
    (with-consistent-dists
      (let ((systems (provided-systems t))
            (already-seen (make-hash-table :test 'equal)))
        (dolist (system (sort systems #'>
                              :key #'preference))
          ;; FIXME: find-asdf-system-file does another find-system
          ;; behind the scenes. Bogus. Should be a better way to go
          ;; from system object to system file.
          (let* ((system-file (find-asdf-system-file (name system)))
                 (enough (and system-file (enough-namestring system-file
                                                             output-file)))
                 (native (and enough (native-namestring enough))))
            (when (and native (not (gethash native already-seen)))
              (setf (gethash native already-seen) native)
              (format stream "~A~%" native)))))))
  (probe-file output-file))

(defun where-is-system (name)
  "Return the pathname to the source directory of ASDF system with the
given NAME, or NIL if no system by that name can be found known."
  (let ((system (asdf:find-system name nil)))
    (when system
      (asdf:system-source-directory system))))
