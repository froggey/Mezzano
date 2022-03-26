;;;; -------------------------------------------------------------------------
;;;; Forcing

(uiop/package:define-package :asdf/forcing
  (:recycle :asdf/forcing :asdf/plan :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session
        :asdf/component :asdf/operation :asdf/system :asdf/system-registry)
  (:export
   #:forcing #:make-forcing #:forced #:forced-not #:performable-p
   #:normalize-forced-systems #:normalize-forced-not-systems
   #:action-forced-p #:action-forced-not-p))
(in-package :asdf/forcing)

;;;; Forcing
(with-upgradability ()
  (defclass forcing ()
    (;; Can plans using this forcing be PERFORMed? A plan that has different force and force-not
     ;; settings than the session can only be used for read-only queries that do not cause the
     ;; status of any action to be raised.
     (performable-p :initform nil :initarg :performable-p :reader performable-p)
     ;; Parameters
     (parameters :initform nil :initarg :parameters :reader parameters)
     ;; Table of systems specified via :force arguments
     (forced :initarg :forced :reader forced)
     ;; Table of systems specified via :force-not argument (and/or immutable)
     (forced-not :initarg :forced-not :reader forced-not)))

  (defgeneric action-forced-p (forcing operation component)
    (:documentation "Is this action forced to happen in this plan?"))
  (defgeneric action-forced-not-p (forcing operation component)
    (:documentation "Is this action forced to not happen in this plan?
Takes precedence over action-forced-p."))

  (defun normalize-forced-systems (force system)
    "Given a SYSTEM on which operate is called and the specified FORCE argument,
extract a hash-set of systems that are forced, or a predicate on system names,
or NIL if none are forced, or :ALL if all are."
    (etypecase force
      ((or (member nil :all) hash-table function) force)
      (cons (list-to-hash-set (mapcar #'coerce-name force)))
      ((eql t) (when system (list-to-hash-set (list (coerce-name system)))))))

  (defun normalize-forced-not-systems (force-not system)
    "Given a SYSTEM on which operate is called, the specified FORCE-NOT argument,
and the set of IMMUTABLE systems, extract a hash-set of systems that are effectively forced-not,
or predicate on system names, or NIL if none are forced, or :ALL if all are."
    (let ((requested
            (etypecase force-not
              ((or (member nil :all) hash-table function) force-not)
              (cons (list-to-hash-set (mapcar #'coerce-name force-not)))
              ((eql t) (if system (let ((name (coerce-name system)))
                                    #'(lambda (x) (not (equal x name))))
                           :all)))))
      (if (and *immutable-systems* requested)
          #'(lambda (x) (or (call-function requested x)
                            (call-function *immutable-systems* x)))
          (or *immutable-systems* requested))))

  ;; TODO: shouldn't we be looking up the primary system name, rather than the system name?
  (defun action-override-p (forcing operation component override-accessor)
    "Given a plan, an action, and a function that given the plan accesses a set of overrides,
i.e. force or force-not, see if the override applies to the current action."
    (declare (ignore operation))
    (call-function (funcall override-accessor forcing)
                   (coerce-name (component-system (find-component () component)))))

  (defmethod action-forced-p (forcing operation component)
    (and
     ;; Did the user ask us to re-perform the action?
     (action-override-p forcing operation component 'forced)
     ;; You really can't force a builtin system and :all doesn't apply to it.
     (not (builtin-system-p (component-system component)))))

  (defmethod action-forced-not-p (forcing operation component)
    ;; Did the user ask us to not re-perform the action?
    ;; NB: force-not takes precedence over force, as it should
    (action-override-p forcing operation component 'forced-not))

  ;; Null forcing means no forcing either way
  (defmethod action-forced-p ((forcing null) (operation operation) (component component))
    nil)
  (defmethod action-forced-not-p ((forcing null) (operation operation) (component component))
    nil)

  (defun or-function (fun1 fun2)
    (cond
      ((or (null fun2) (eq fun1 :all)) fun1)
      ((or (null fun1) (eq fun2 :all)) fun2)
      (t #'(lambda (x) (or (call-function fun1 x) (call-function fun2 x))))))

  (defun make-forcing (&key performable-p system
                         (force nil force-p) (force-not nil force-not-p) &allow-other-keys)
    (let* ((session-forcing (when *asdf-session* (forcing *asdf-session*)))
           (system (and system (coerce-name system)))
           (forced (normalize-forced-systems force system))
           (forced-not (normalize-forced-not-systems force-not system))
           (parameters `(,@(when force `(:force ,force))
                         ,@(when force-not `(:force-not ,force-not))
                         ,@(when (or (eq force t) (eq force-not t)) `(:system ,system))
                         ,@(when performable-p `(:performable-p t))))
           forcing)
      (cond
        ((not session-forcing)
         (setf forcing (make-instance 'forcing
                                      :performable-p performable-p :parameters parameters
                                      :forced forced :forced-not forced-not))
         (when (and performable-p *asdf-session*)
           (setf (forcing *asdf-session*) forcing)))
        (performable-p
         (when (and (not (equal parameters (parameters session-forcing)))
                    (or force-p force-not-p))
           (parameter-error "~*~S and ~S arguments not allowed in a nested call to ~3:*~S ~
unless identically to toplevel"
                            (find-symbol* :operate :asdf) :force :force-not))
         (setf forcing session-forcing))
        (t
         (setf forcing (make-instance 'forcing
                           ;; Combine force and force-not with values from the toplevel-plan
                           :parameters `(,@parameters :on-top-of ,(parameters session-forcing))
                           :forced (or-function (forced session-forcing) forced)
                           :forced-not (or-function (forced-not session-forcing) forced-not)))))
      forcing))

  (defmethod print-object ((forcing forcing) stream)
    (print-unreadable-object (forcing stream :type t)
      (format stream "~{~S~^ ~}" (parameters forcing))))

  ;; During upgrade, the *asdf-session* may legitimately be NIL, so we must handle that case.
  (defmethod forcing ((x null))
    (if-let (session (toplevel-asdf-session))
      (forcing session)
      (make-forcing :performable-p t)))

  ;; When performing a plan that is a list of actions, use the toplevel asdf sesssion forcing.
  (defmethod forcing ((x cons)) (forcing (toplevel-asdf-session))))
