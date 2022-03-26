;;;; -------------------------------------------------------------------------
;;;; Session

(uiop/package:define-package :asdf/session
  (:recycle :asdf/session :asdf/cache :asdf/component
            :asdf/action :asdf/find-system :asdf/plan :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade)
  (:export
   #:get-file-stamp #:compute-file-stamp #:register-file-stamp
   #:asdf-cache #:set-asdf-cache-entry #:unset-asdf-cache-entry #:consult-asdf-cache
   #:do-asdf-cache #:normalize-namestring
   #:call-with-asdf-session #:with-asdf-session
   #:*asdf-session* #:*asdf-session-class* #:session #:toplevel-asdf-session
   #:session-cache #:forcing #:asdf-upgraded-p
   #:visited-actions #:visiting-action-set #:visiting-action-list
   #:total-action-count #:planned-action-count #:planned-output-action-count
   #:clear-configuration-and-retry #:retry
   #:operate-level
   ;; conditions
   #:system-definition-error ;; top level, moved here because this is the earliest place for it.
   #:formatted-system-definition-error #:format-control #:format-arguments #:sysdef-error))
(in-package :asdf/session)


(with-upgradability ()
  ;; The session variable.
  ;; NIL when outside a session.
  (defvar *asdf-session* nil)
  (defparameter* *asdf-session-class* 'session
    "The default class for sessions")

  (defclass session ()
    (;; The ASDF session cache is used to memoize some computations.
     ;; It is instrumental in achieving:
     ;; * Consistency in the view of the world relied on by ASDF within a given session.
     ;;   Inconsistencies in file stamps, system definitions, etc., could cause infinite loops
     ;;   (a.k.a. stack overflows) and other erratic behavior.
     ;; * Speed and reliability of ASDF, with fewer side-effects from access to the filesystem, and
     ;;   no expensive recomputations of transitive dependencies for input-files or output-files.
     ;; * Testability of ASDF with the ability to fake timestamps without actually touching files.
     (ancestor
      :initform nil :initarg :ancestor :reader session-ancestor
      :documentation "Top level session that this is part of")
     (session-cache
      :initform (make-hash-table :test 'equal) :initarg :session-cache :reader session-cache
      :documentation "Memoize expensive computations")
     (operate-level
      :initform 0 :initarg :operate-level :accessor session-operate-level
      :documentation "Number of nested calls to operate we're under (for toplevel session only)")
     ;; shouldn't the below be superseded by the session-wide caching of action-status
     ;; for (load-op "asdf") ?
     (asdf-upgraded-p
      :initform nil :initarg :asdf-upgraded-p :accessor asdf-upgraded-p
      :documentation "Was ASDF already upgraded in this session - only valid for toplevel-asdf-session.")
     (forcing
      :initform nil :initarg :forcing :accessor forcing
      :documentation "Forcing parameters for the session")
     ;; Table that to actions already visited while walking the dependencies associates status
     (visited-actions :initform (make-hash-table :test 'equal) :accessor visited-actions)
     ;; Actions that depend on those being currently walked through, to detect circularities
     (visiting-action-set ;; as a set
      :initform (make-hash-table :test 'equal) :accessor visiting-action-set)
     (visiting-action-list :initform () :accessor visiting-action-list) ;; as a list
     ;; Counts of total actions in plan
     (total-action-count :initform 0 :accessor total-action-count)
     ;; Count of actions that need to be performed
     (planned-action-count :initform 0 :accessor planned-action-count)
     ;; Count of actions that need to be performed that have a non-empty list of output-files.
     (planned-output-action-count :initform 0 :accessor planned-output-action-count))
    (:documentation "An ASDF session with a cache to memoize some computations"))

  (defun toplevel-asdf-session ()
    (when *asdf-session* (or (session-ancestor *asdf-session*) *asdf-session*)))

  (defun operate-level ()
    (session-operate-level (toplevel-asdf-session)))

  (defun (setf operate-level) (new-level)
    (setf (session-operate-level (toplevel-asdf-session)) new-level))

  (defun asdf-cache ()
    (session-cache *asdf-session*))

  ;; Set a session cache entry for KEY to a list of values VALUE-LIST, when inside a session.
  ;; Return those values.
  (defun set-asdf-cache-entry (key value-list)
    (values-list (if *asdf-session*
                     (setf (gethash key (asdf-cache)) value-list)
                     value-list)))

  ;; Unset the session cache entry for KEY, when inside a session.
  (defun unset-asdf-cache-entry (key)
    (when *asdf-session*
      (remhash key (session-cache *asdf-session*))))

  ;; Consult the session cache entry for KEY if present and in a session;
  ;; if not present, compute it by calling the THUNK,
  ;; and set the session cache entry accordingly, if in a session.
  ;; Return the values from the cache and/or the thunk computation.
  (defun consult-asdf-cache (key &optional thunk)
    (if *asdf-session*
        (multiple-value-bind (results foundp) (gethash key (session-cache *asdf-session*))
          (if foundp
              (values-list results)
              (set-asdf-cache-entry key (multiple-value-list (call-function thunk)))))
        (call-function thunk)))

  ;; Syntactic sugar for consult-asdf-cache
  (defmacro do-asdf-cache (key &body body)
    `(consult-asdf-cache ,key #'(lambda () ,@body)))

  ;; Compute inside a ASDF session with a cache.
  ;; First, make sure an ASDF session is underway, by binding the session cache variable
  ;; to a new hash-table if it's currently null (or even if it isn't, if OVERRIDE is true).
  ;; Second, if a new session was started, establish restarts for retrying the overall computation.
  ;; Finally, consult the cache if a KEY was specified with the THUNK as a fallback when the cache
  ;; entry isn't found, or just call the THUNK if no KEY was specified.
  (defun call-with-asdf-session (thunk &key override key override-cache override-forcing)
    (let ((fun (if key #'(lambda () (consult-asdf-cache key thunk)) thunk)))
      (if (and (not override) *asdf-session*)
          (funcall fun)
          (loop
            (restart-case
                (let ((*asdf-session*
                       (apply 'make-instance *asdf-session-class*
                              (when *asdf-session*
                                `(:ancestor ,(toplevel-asdf-session)
                                  ,@(unless override-forcing
                                      `(:forcing ,(forcing *asdf-session*)))
                                  ,@(unless override-cache
                                      `(:session-cache ,(session-cache *asdf-session*))))))))
                  (return (funcall fun)))
              (retry ()
                :report (lambda (s)
                          (format s (compatfmt "~@<Retry ASDF operation.~@:>"))))
              (clear-configuration-and-retry ()
                :report (lambda (s)
                          (format s (compatfmt "~@<Retry ASDF operation after resetting the configuration.~@:>")))
                (clrhash (session-cache *asdf-session*))
                (clear-configuration)))))))

  ;; Syntactic sugar for call-with-asdf-session
  (defmacro with-asdf-session ((&key key override override-cache override-forcing) &body body)
    `(call-with-asdf-session
      #'(lambda () ,@body)
      :override ,override :key ,key
      :override-cache ,override-cache :override-forcing ,override-forcing))


  ;;; Define specific accessor for file (date) stamp.

  ;; Normalize a namestring for use as a key in the session cache.
  (defun normalize-namestring (pathname)
    (let ((resolved (resolve-symlinks*
                     (ensure-absolute-pathname
                      (physicalize-pathname pathname)
                      'get-pathname-defaults))))
      (with-pathname-defaults () (namestring resolved))))

  ;; Compute the file stamp for a normalized namestring
  (defun compute-file-stamp (normalized-namestring)
    (with-pathname-defaults ()
      (or (safe-file-write-date normalized-namestring) t)))

  ;; Override the time STAMP associated to a given FILE in the session cache.
  ;; If no STAMP is specified, recompute a new one from the filesystem.
  (defun register-file-stamp (file &optional (stamp nil stampp))
    (let* ((namestring (normalize-namestring file))
           (stamp (if stampp stamp (compute-file-stamp namestring))))
      (set-asdf-cache-entry `(get-file-stamp ,namestring) (list stamp))))

  ;; Get or compute a memoized stamp for given FILE from the session cache.
  (defun get-file-stamp (file)
    (when file
      (let ((namestring (normalize-namestring file)))
        (do-asdf-cache `(get-file-stamp ,namestring) (compute-file-stamp namestring)))))


  ;;; Conditions

  (define-condition system-definition-error (error) ()
    ;; [this use of :report should be redundant, but unfortunately it's not.
    ;; cmucl's lisp::output-instance prefers the kernel:slot-class-print-function
    ;; over print-object; this is always conditions::%print-condition for
    ;; condition objects, which in turn does inheritance of :report options at
    ;; run-time.  fortunately, inheritance means we only need this kludge here in
    ;; order to fix all conditions that build on it.  -- rgr, 28-Jul-02.]
    #+cmucl (:report print-object))

  (define-condition formatted-system-definition-error (system-definition-error)
    ((format-control :initarg :format-control :reader format-control)
     (format-arguments :initarg :format-arguments :reader format-arguments))
    (:report (lambda (c s)
               (apply 'format s (format-control c) (format-arguments c)))))

  (defun sysdef-error (format &rest arguments)
    (error 'formatted-system-definition-error :format-control
           format :format-arguments arguments)))
