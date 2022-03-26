;;;; -------------------------------------------------------------------------
;;;; Plan

(uiop/package:define-package :asdf/plan
  ;; asdf/action below is needed for required-components, traverse-action and traverse-sub-actions
  ;; that used to live there before 3.2.0.
  (:recycle :asdf/plan :asdf/action :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session
        :asdf/component :asdf/operation :asdf/action :asdf/lisp-action
        :asdf/system :asdf/system-registry :asdf/find-component :asdf/forcing)
  (:export
   #:plan #:plan-traversal #:sequential-plan #:*plan-class*
   #:action-status #:status-stamp #:status-index #:status-done-p #:status-keep-p #:status-need-p
   #:action-already-done-p
   #:+status-good+ #:+status-todo+ #:+status-void+
   #:system-out-of-date #:action-up-to-date-p
   #:circular-dependency #:circular-dependency-actions
   #:needed-in-image-p
   #:map-direct-dependencies #:reduce-direct-dependencies #:direct-dependencies
   #:compute-action-stamp #:traverse-action #:record-dependency
   #:make-plan #:plan-actions #:plan-actions-r #:perform-plan #:mark-as-done
   #:required-components #:filtered-sequential-plan
   #:plan-component-type #:plan-keep-operation #:plan-keep-component))
(in-package :asdf/plan)

;;;; Generic plan traversal class
(with-upgradability ()
  (defclass plan () ()
    (:documentation "Base class for a plan based on which ASDF can build a system"))
  (defclass plan-traversal (plan)
    (;; The forcing parameters for this plan. Also indicates whether the plan is performable,
     ;; in which case the forcing is the same as for the entire session.
     (forcing :initform (forcing (toplevel-asdf-session)) :initarg :forcing :reader forcing))
    (:documentation "Base class for plans that simply traverse dependencies"))
  ;; Sequential plans (the default)
  (defclass sequential-plan (plan-traversal)
    ((actions-r :initform nil :accessor plan-actions-r))
    (:documentation "Simplest, default plan class, accumulating a sequence of actions"))

  (defgeneric plan-actions (plan)
    (:documentation "Extract from a plan a list of actions to perform in sequence"))
  (defmethod plan-actions ((plan list))
    plan)
  (defmethod plan-actions ((plan sequential-plan))
    (reverse (plan-actions-r plan)))

  (defgeneric record-dependency (plan operation component)
    (:documentation "Record an action as a dependency in the current plan"))

  ;; No need to record a dependency to build a full graph, just accumulate nodes in order.
  (defmethod record-dependency ((plan sequential-plan) (o operation) (c component))
    (values)))

(when-upgrading (:version "3.3.0")
  (defmethod initialize-instance :after ((plan plan-traversal) &key &allow-other-keys)))


;;;; Planned action status
(with-upgradability ()
  (defclass action-status ()
    ((bits
      :type fixnum :initarg :bits :reader status-bits
      :documentation "bitmap describing the status of the action.")
     (stamp
      :type (or integer boolean) :initarg :stamp :reader status-stamp
      :documentation "STAMP associated with the ACTION if it has been completed already in some
previous session or image, T if it was done and builtin the image, or NIL if it needs to be done.")
     (level
      :type fixnum :initarg :level :initform 0 :reader status-level
      :documentation "the highest (operate-level) at which the action was needed")
     (index
      :type (or integer null) :initarg :index :initform nil :reader status-index
      :documentation "INDEX associated with the ACTION in the current session,
or NIL if no the status is considered outside of a specific plan."))
    (:documentation "Status of an action in a plan"))

  ;; STAMP   KEEP-P DONE-P NEED-P     symbol bitmap  previously   currently
  ;; not-nil   T      T      T     =>  GOOD     7    up-to-date   done (e.g. file previously loaded)
  ;; not-nil   T      T     NIL    =>  HERE     6    up-to-date   unplanned yet done
  ;; not-nil   T     NIL     T     =>  REDO     5    up-to-date   planned (e.g. file to load)
  ;; not-nil   T     NIL    NIL    =>  SKIP     4    up-to-date   unplanned (e.g. file compiled)
  ;; not-nil  NIL     T      T     =>  DONE     3    out-of-date  done
  ;; not-nil  NIL     T     NIL    =>  WHAT     2    out-of-date  unplanned yet done(?)
  ;;  NIL     NIL    NIL     T     =>  TODO     1    out-of-date  planned
  ;;  NIL     NIL    NIL    NIL    =>  VOID     0    out-of-date  unplanned
  ;;
  ;; Note that a VOID status cannot happen as part of a transitive dependency of a wanted node
  ;; while traversing a node with TRAVERSE-ACTION; it can only happen while checking whether an
  ;; action is up-to-date with ACTION-UP-TO-DATE-P.
  ;;
  ;; When calling TRAVERSE-ACTION, the +need-bit+ is set,
  ;; unless the action is up-to-date and not needed-in-image (HERE, SKIP).
  ;; When PERFORMing an action, the +done-bit+ is set.
  ;; When the +need-bit+ is set but not the +done-bit+, the level slot indicates which level of
  ;; OPERATE it was last marked needed for; if it happens to be needed at a higher-level, then
  ;; its urgency (and that of its transitive dependencies) must be escalated so that it will be
  ;; done before the end of this level of operate.
  ;;
  ;; Also, when no ACTION-STATUS is associated to an action yet, NIL serves as a bottom value.
  ;;
  (defparameter +keep-bit+ 4)
  (defparameter +done-bit+ 2)
  (defparameter +need-bit+ 1)
  (defparameter +good-bits+ 7)
  (defparameter +todo-bits+ 1)
  (defparameter +void-bits+ 0)

  (defparameter +status-good+
    (make-instance 'action-status :bits +good-bits+ :stamp t))
  (defparameter +status-todo+
    (make-instance 'action-status :bits +todo-bits+ :stamp nil))
  (defparameter +status-void+
    (make-instance 'action-status :bits +void-bits+ :stamp nil)))

(with-upgradability ()
  (defun make-action-status (&key bits stamp (level 0) index)
    (check-type bits (integer 0 7))
    (check-type stamp (or integer boolean))
    (check-type level (integer 0 #.most-positive-fixnum))
    (check-type index (or integer null))
    (assert (eq (null stamp) (zerop (logand bits #.(logior +keep-bit+ +done-bit+)))) ()
            "Bad action-status :bits ~S :stamp ~S" bits stamp)
    (block nil
      (when (and (null index) (zerop level))
        (case bits
          (#.+void-bits+ (return +status-void+))
          (#.+todo-bits+ (return +status-todo+))
          (#.+good-bits+ (when (eq stamp t) (return +status-good+)))))
      (make-instance 'action-status :bits bits :stamp stamp :level level :index index)))

  (defun status-keep-p (status)
    (plusp (logand (status-bits status) #.+keep-bit+)))
  (defun status-done-p (status)
    (plusp (logand (status-bits status) #.+done-bit+)))
  (defun status-need-p (status)
    (plusp (logand (status-bits status) #.+need-bit+)))

  (defun merge-action-status (status1 status2) ;; status-and
    "Return the earliest status later than both status1 and status2"
    (make-action-status
     :bits (logand (status-bits status1) (status-bits status2))
     :stamp (latest-timestamp (status-stamp status1) (status-stamp status2))
     :level (min (status-level status1) (status-level status2))
     :index (or (status-index status1) (status-index status2))))

  (defun mark-status-needed (status &optional (level (operate-level))) ;; limited status-or
    "Return the same status but with the need bit set, for the given level"
    (if (and (status-need-p status)
             (>= (status-level status) level))
        status
        (make-action-status
         :bits (logior (status-bits status) +need-bit+)
         :level (max level (status-level status))
         :stamp (status-stamp status)
         :index (status-index status))))

  (defmethod print-object ((status action-status) stream)
    (print-unreadable-object (status stream :type t)
      (with-slots (bits stamp level index) status
        (format stream "~{~S~^ ~}" `(:bits ,bits :stamp ,stamp :level ,level :index ,index)))))

  (defgeneric action-status (plan operation component)
    (:documentation "Returns the ACTION-STATUS associated to the action of OPERATION on COMPONENT
in the PLAN, or NIL if the action wasn't visited yet as part of the PLAN."))

  (defgeneric (setf action-status) (new-status plan operation component)
    (:documentation "Sets the ACTION-STATUS associated to
the action of OPERATION on COMPONENT in the PLAN"))

  (defmethod action-status ((plan null) (o operation) (c component))
    (multiple-value-bind (stamp done-p) (component-operation-time o c)
      (if done-p
          (make-action-status :bits #.+keep-bit+ :stamp stamp)
          +status-void+)))

  (defmethod (setf action-status) (new-status (plan null) (o operation) (c component))
    (let ((times (component-operation-times c)))
      (if (status-done-p new-status)
          (setf (gethash o times) (status-stamp new-status))
          (remhash o times)))
    new-status)

  ;; Handle FORCED-NOT: it makes an action return its current timestamp as status
  (defmethod action-status ((p plan) (o operation) (c component))
    ;; TODO: should we instead test something like:
    ;; (action-forced-not-p plan operation (primary-system component))
    (or (gethash (make-action o c) (visited-actions *asdf-session*))
        (when (action-forced-not-p (forcing p) o c)
          (let ((status (action-status nil o c)))
            (setf (gethash (make-action o c) (visited-actions *asdf-session*))
                  (make-action-status
                   :bits +good-bits+
                   :stamp (or (and status (status-stamp status)) t)
                   :index (incf (total-action-count *asdf-session*))))))))

  (defmethod (setf action-status) (new-status (p plan) (o operation) (c component))
    (setf (gethash (make-action o c) (visited-actions *asdf-session*)) new-status))

  (defmethod (setf action-status) :after
      (new-status (p sequential-plan) (o operation) (c component))
    (unless (status-done-p new-status)
      (push (make-action o c) (plan-actions-r p)))))


;;;; Is the action needed in this image?
(with-upgradability ()
  (defgeneric needed-in-image-p (operation component)
    (:documentation "Is the action of OPERATION on COMPONENT needed in the current image
to be meaningful, or could it just as well have been done in another Lisp image?"))

  (defmethod needed-in-image-p ((o operation) (c component))
    ;; We presume that actions that modify the filesystem don't need be run
    ;; in the current image if they have already been done in another,
    ;; and can be run in another process (e.g. a fork),
    ;; whereas those that don't are meant to side-effect the current image and can't.
    (not (output-files o c))))


;;;; Visiting dependencies of an action and computing action stamps
(with-upgradability ()
  (defun* (map-direct-dependencies) (operation component fun)
    "Call FUN on all the valid dependencies of the given action in the given plan"
    (loop* :for (dep-o-spec . dep-c-specs) :in (component-depends-on operation component)
      :for dep-o = (find-operation operation dep-o-spec)
      :when dep-o
      :do (loop :for dep-c-spec :in dep-c-specs
            :for dep-c = (and dep-c-spec (resolve-dependency-spec component dep-c-spec))
            :when (action-valid-p dep-o dep-c)
            :do (funcall fun dep-o dep-c))))

  (defun* (reduce-direct-dependencies) (operation component combinator seed)
    "Reduce the direct dependencies to a value computed by iteratively calling COMBINATOR
for each dependency action on the dependency's operation and component and an accumulator
initialized with SEED."
    (map-direct-dependencies
     operation component
     #'(lambda (dep-o dep-c) (setf seed (funcall combinator dep-o dep-c seed))))
    seed)

  (defun* (direct-dependencies) (operation component)
    "Compute a list of the direct dependencies of the action within the plan"
    (reverse (reduce-direct-dependencies operation component #'acons nil)))

  ;; In a distant future, get-file-stamp, component-operation-time and latest-stamp
  ;; shall also be parametrized by the plan, or by a second model object,
  ;; so they need not refer to the state of the filesystem,
  ;; and the stamps could be cryptographic checksums rather than timestamps.
  ;; Such a change remarkably would only affect COMPUTE-ACTION-STAMP.
  (define-condition dependency-not-done (warning)
    ((op
      :initarg :op)
     (component
      :initarg :component)
     (dep-op
      :initarg :dep-op)
     (dep-component
      :initarg :dep-component)
     (plan
      :initarg :plan
      :initform nil))
    (:report (lambda (condition stream)
               (with-slots (op component dep-op dep-component plan) condition
                 (format stream "Computing just-done stamp ~@[in plan ~S~] for action ~S, but dependency ~S wasn't done yet!"
                         plan
                         (action-path (make-action op component))
                         (action-path (make-action dep-op dep-component)))))))

  (defmethod compute-action-stamp (plan (o operation) (c component) &key just-done)
    ;; Given an action, figure out at what time in the past it has been done,
    ;; or if it has just been done, return the time that it has.
    ;; Returns two values:
    ;; 1- the TIMESTAMP of the action if it has already been done and is up to date,
    ;;   or NIL is either hasn't been done or is out of date.
    ;;   (An ASDF extension could use a cryptographic digest instead.)
    ;; 2- the DONE-IN-IMAGE-P boolean flag that is T if the action has already been done
    ;;   in the current image, or NIL if it hasn't.
    ;; Note that if e.g. LOAD-OP only depends on up-to-date files, but
    ;; hasn't been done in the current image yet, then it can have a non-NIL timestamp,
    ;; yet a NIL done-in-image-p flag: we can predict what timestamp it will have once loaded,
    ;; i.e. that of the input-files.
    ;; If just-done is NIL, these values return are the notional fields of
    ;; a KEEP, REDO or TODO status (VOID is possible, but probably an error).
    ;; If just-done is T, they are the notional fields of DONE status
    ;; (or, if something went wrong, TODO).
    (nest
     (block ())
     (let* ((dep-status ; collect timestamp from dependencies (or T if forced or out-of-date)
             (reduce-direct-dependencies
              o c
              #'(lambda (do dc status)
                  ;; out-of-date dependency: don't bother looking further
                  (let ((action-status (action-status plan do dc)))
                    (cond
                      ((and action-status (or (status-keep-p action-status)
                                              (and just-done (status-stamp action-status))))
                       (merge-action-status action-status status))
                      (just-done
                       ;; It's OK to lose some ASDF action stamps during self-upgrade
                       (unless (equal "asdf" (primary-system-name dc))
                         (warn 'dependency-not-done
                               :plan plan
                               :op o :component c
                               :dep-op do :dep-component dc))
                       status)
                      (t
                       (return (values nil nil))))))
              +status-good+))
            (dep-stamp (status-stamp dep-status))))
     (let* (;; collect timestamps from inputs, and exit early if any is missing
            (in-files (input-files o c))
            (in-stamps (mapcar #'get-file-stamp in-files))
            (missing-in (loop :for f :in in-files :for s :in in-stamps :unless s :collect f))
            (latest-in (timestamps-latest (cons dep-stamp in-stamps))))
       (when (and missing-in (not just-done)) (return (values nil nil))))
     (let* (;; collect timestamps from outputs, and exit early if any is missing
            (out-files (remove-if 'null (output-files o c)))
            (out-stamps (mapcar (if just-done 'register-file-stamp 'get-file-stamp) out-files))
            (missing-out (loop :for f :in out-files :for s :in out-stamps :unless s :collect f))
            (earliest-out (timestamps-earliest out-stamps)))
       (when (and missing-out (not just-done)) (return (values nil nil))))
     (let (;; Time stamps from the files at hand, and whether any is missing
           (all-present (not (or missing-in missing-out)))
           ;; Has any input changed since we last generated the files?
           ;; Note that we use timestamp<= instead of timestamp< to play nice with generated files.
           ;; Any race condition is intrinsic to the limited timestamp resolution.
           (up-to-date-p (timestamp<= latest-in earliest-out))
           ;; If everything is up to date, the latest of inputs and outputs is our stamp
           (done-stamp (timestamps-latest (cons latest-in out-stamps))))
       ;; Warn if some files are missing:
       ;; either our model is wrong or some other process is messing with our files.
       (when (and just-done (not all-present))
         ;; Shouldn't that be an error instead?
         (warn "~A completed without ~:[~*~;~*its input file~:p~2:*~{ ~S~}~*~]~
                ~:[~; or ~]~:[~*~;~*its output file~:p~2:*~{ ~S~}~*~]"
               (action-description o c)
               missing-in (length missing-in) (and missing-in missing-out)
               missing-out (length missing-out))))
     (let (;; There are three kinds of actions:
           (out-op (and out-files t)) ; those that create files on the filesystem
           ;;(image-op (and in-files (null out-files))) ; those that load stuff into the image
           ;;(null-op (and (null out-files) (null in-files))) ; placeholders that do nothing
           ))
     (if (or just-done ;; The done-stamp is valid: if we're just done, or
             (and all-present ;; if all filesystem effects are up-to-date
                  up-to-date-p
                  (operation-done-p o c) ;; and there's no invalidating reason.
                  (not (action-forced-p (forcing (or plan *asdf-session*)) o c))))
         (values done-stamp ;; return the hard-earned timestamp
                 (or just-done
                     out-op ;; A file-creating op is done when all files are up to date.
                     ;; An image-effecting operation is done when
                     (and (status-done-p dep-status) ;; all the dependencies were done, and
                          (multiple-value-bind (perform-stamp perform-done-p)
                              (component-operation-time o c)
                            (and perform-done-p ;; the op was actually run,
                                 (equal perform-stamp done-stamp)))))) ;; with a matching stamp.
         ;; done-stamp invalid: return a timestamp in an indefinite future, action not done yet
         (values nil nil)))))


;;;; The four different actual traversals:
;; * TRAVERSE-ACTION o c T: Ensure all dependencies are either up-to-date in-image, or planned
;; * TRAVERSE-ACTION o c NIL: Ensure all dependencies are up-to-date or planned, in-image or not
;; * ACTION-UP-TO-DATE-P: Check whether some (defsystem-depends-on ?) dependencies are up to date
;; * COLLECT-ACTION-DEPENDENCIES: Get the dependencies (filtered), don't change any status
(with-upgradability ()

  ;; Compute the action status for a newly visited action.
  (defun compute-action-status (plan operation component need-p)
    (multiple-value-bind (stamp done-p)
        (compute-action-stamp plan operation component)
      (assert (or stamp (not done-p)))
      (make-action-status
       :bits (logior (if stamp #.+keep-bit+ 0)
                     (if done-p #.+done-bit+ 0)
                     (if need-p #.+need-bit+ 0))
       :stamp stamp
       :level (operate-level)
       :index (incf (total-action-count *asdf-session*)))))

  ;; TRAVERSE-ACTION, in the context of a given PLAN object that accumulates dependency data,
  ;; visits the action defined by its OPERATION and COMPONENT arguments,
  ;; and all its transitive dependencies (unless already visited),
  ;; in the context of the action being (or not) NEEDED-IN-IMAGE-P,
  ;; i.e. needs to be done in the current image vs merely have been done in a previous image.
  ;;
  ;; TRAVERSE-ACTION updates the VISITED-ACTIONS entries for the action and for all its
  ;; transitive dependencies (that haven't been sufficiently visited so far).
  ;; It does not return any usable value.
  ;;
  ;; Note that for an XCVB-like plan with one-image-per-file-outputting-action,
  ;; the below method would be insufficient, since it assumes a single image
  ;; to traverse each node at most twice; non-niip actions would be traversed only once,
  ;; but niip nodes could be traversed once per image, i.e. once plus once per non-niip action.

  (defun traverse-action (plan operation component needed-in-image-p)
    (block nil
      (unless (action-valid-p operation component) (return))
      ;; Record the dependency. This hook is needed by POIU, which tracks a full dependency graph,
      ;; instead of just a dependency order as in vanilla ASDF.
      ;; TODO: It is also needed to detect OPERATE-in-PERFORM.
      (record-dependency plan operation component)
      (while-visiting-action (operation component) ; maintain context, handle circularity.
        ;; needed-in-image distinguishes b/w things that must happen in the
        ;; current image and those things that simply need to have been done in a previous one.
        (let* ((aniip (needed-in-image-p operation component)) ; action-specific needed-in-image
               ;; effective niip: meaningful for the action and required by the plan as traversed
               (eniip (and aniip needed-in-image-p))
               ;; status: have we traversed that action previously, and if so what was its status?
               (status (action-status plan operation component))
               (level (operate-level)))
          (when (and status
                     (or (status-done-p status) ;; all done
                         (and (status-need-p status) (<= level (status-level status))) ;; already visited
                         (and (status-keep-p status) (not eniip)))) ;; up-to-date and not eniip
            (return)) ; Already visited with sufficient need-in-image level!
          (labels ((visit-action (niip) ; We may visit the action twice, once with niip NIL, then T
                     (map-direct-dependencies ; recursively traverse dependencies
                      operation component #'(lambda (o c) (traverse-action plan o c niip)))
                     ;; AFTER dependencies have been traversed, compute action stamp
                     (let* ((status (if status
                                        (mark-status-needed status level)
                                        (compute-action-status plan operation component t)))
                            (out-of-date-p (not (status-keep-p status)))
                            (to-perform-p (or out-of-date-p (and niip (not (status-done-p status))))))
                       (cond ; it needs be done if it's out of date or needed in image but absent
                         ((and out-of-date-p (not niip)) ; if we need to do it,
                          (visit-action t)) ; then we need to do it *in the (current) image*!
                         (t
                          (setf (action-status plan operation component) status)
                          (when (status-done-p status)
                            (setf (component-operation-time operation component)
                                  (status-stamp status)))
                          (when to-perform-p ; if it needs to be added to the plan, count it
                            (incf (planned-action-count *asdf-session*))
                            (unless aniip ; if it's output-producing, count it
                              (incf (planned-output-action-count *asdf-session*)))))))))
            (visit-action eniip)))))) ; visit the action

  ;; NB: This is not an error, not a warning, but a normal expected condition,
  ;; to be to signaled by FIND-SYSTEM when it detects an out-of-date system,
  ;; *before* it tries to replace it with a new definition.
  (define-condition system-out-of-date (condition)
    ((name :initarg :name :reader component-name))
    (:documentation "condition signaled when a system is detected as being out of date")
    (:report (lambda (c s)
               (format s "system ~A is out of date" (component-name c)))))

  (defun action-up-to-date-p (plan operation component)
    "Check whether an action was up-to-date at the beginning of the session.
Update the VISITED-ACTIONS table with the known status, but don't add anything to the PLAN."
    (block nil
      (unless (action-valid-p operation component) (return t))
      (while-visiting-action (operation component) ; maintain context, handle circularity.
        ;; Do NOT record the dependency: it might be out of date.
        (let ((status (or (action-status plan operation component)
                          (setf (action-status plan operation component)
                                (let ((dependencies-up-to-date-p
                                       (handler-case
                                           (block nil
                                             (map-direct-dependencies
                                              operation component
                                              #'(lambda (o c)
                                                  (unless (action-up-to-date-p plan o c)
                                                    (return nil))))
                                             t)
                                         (system-out-of-date () nil))))
                                  (if dependencies-up-to-date-p
                                      (compute-action-status plan operation component nil)
                                      +status-void+))))))
          (and (status-keep-p status) (status-stamp status)))))))


;;;; Incidental traversals

;;; Making a FILTERED-SEQUENTIAL-PLAN can be used to, e.g., all of the source
;;; files required by a bundling operation.
(with-upgradability ()
  (defclass filtered-sequential-plan (sequential-plan)
    ((component-type :initform t :initarg :component-type :reader plan-component-type)
     (keep-operation :initform t :initarg :keep-operation :reader plan-keep-operation)
     (keep-component :initform t :initarg :keep-component :reader plan-keep-component))
    (:documentation "A variant of SEQUENTIAL-PLAN that only records a subset of actions."))

  (defmethod initialize-instance :after ((plan filtered-sequential-plan)
                                         &key system other-systems)
    ;; Ignore force and force-not, rely on other-systems:
    ;; force traversal of what we're interested in, i.e. current system or also others;
    ;; force-not traversal of what we're not interested in, i.e. other systems unless other-systems.
    (setf (slot-value plan 'forcing)
          (make-forcing :system system :force :all :force-not (if other-systems nil t))))

  (defmethod plan-actions ((plan filtered-sequential-plan))
    (with-slots (keep-operation keep-component) plan
      (loop :for action :in (call-next-method)
        :as o = (action-operation action)
        :as c = (action-component action)
        :when (and (typep o keep-operation) (typep c keep-component))
        :collect (make-action o c))))

  (defun collect-action-dependencies (plan operation component)
    (when (action-valid-p operation component)
      (while-visiting-action (operation component) ; maintain context, handle circularity.
        (let ((action (make-action operation component)))
          (unless (nth-value 1 (gethash action (visited-actions *asdf-session*)))
            (setf (gethash action (visited-actions *asdf-session*)) nil)
            (when (and (typep component (plan-component-type plan))
                       (not (action-forced-not-p (forcing plan) operation component)))
              (map-direct-dependencies operation component
                                       #'(lambda (o c) (collect-action-dependencies plan o c)))
              (push action (plan-actions-r plan))))))))

  (defgeneric collect-dependencies (operation component &key &allow-other-keys)
    (:documentation "Given an action, build a plan for all of its dependencies."))
  (define-convenience-action-methods collect-dependencies (operation component &key))
  (defmethod collect-dependencies ((operation operation) (component component)
                                   &rest keys &key &allow-other-keys)
    (let ((plan (apply 'make-instance 'filtered-sequential-plan
                       :system (component-system component) keys)))
      (loop :for action :in (direct-dependencies operation component)
        :do (collect-action-dependencies plan (action-operation action) (action-component action)))
      (plan-actions plan)))

  (defun* (required-components) (system &rest keys &key (goal-operation 'load-op) &allow-other-keys)
    "Given a SYSTEM and a GOAL-OPERATION (default LOAD-OP), traverse the dependencies and
return a list of the components involved in building the desired action."
    (with-asdf-session (:override t)
      (remove-duplicates
       (mapcar 'action-component
               (apply 'collect-dependencies goal-operation system
                      (remove-plist-key :goal-operation keys)))
       :from-end t))))


;;;; High-level interface: make-plan, perform-plan
(with-upgradability ()
  (defgeneric make-plan (plan-class operation component &key &allow-other-keys)
    (:documentation "Generate and return a plan for performing OPERATION on COMPONENT."))
  (define-convenience-action-methods make-plan (plan-class operation component &key))

  (defgeneric mark-as-done (plan-class operation component)
    (:documentation "Mark an action as done in a plan, after performing it."))
  (define-convenience-action-methods mark-as-done (plan-class operation component))

  (defgeneric perform-plan (plan &key)
    (:documentation "Actually perform a plan and build the requested actions"))

  (defparameter* *plan-class* 'sequential-plan
    "The default plan class to use when building with ASDF")

  (defmethod make-plan (plan-class (o operation) (c component) &rest keys &key &allow-other-keys)
    (with-asdf-session ()
      (let ((plan (apply 'make-instance (or plan-class *plan-class*) keys)))
        (traverse-action plan o c t)
        plan)))

  (defmethod perform-plan :around ((plan t) &key)
    (assert (performable-p (forcing plan)) () "plan not performable")
    (let ((*package* *package*)
          (*readtable* *readtable*))
      (with-compilation-unit () ;; backward-compatibility.
        (call-next-method))))   ;; Going forward, see deferred-warning support in lisp-build.

  (defun action-already-done-p (plan operation component)
    (if-let (status (action-status plan operation component))
      (status-done-p status)))

  (defmethod perform-plan ((plan t) &key)
    (loop :for action :in (plan-actions plan)
      :as o = (action-operation action)
      :as c = (action-component action) :do
      (unless (action-already-done-p plan o c)
        (perform-with-restarts o c)
        (mark-as-done plan o c))))

  (defmethod mark-as-done ((plan plan) (o operation) (c component))
    (let ((plan-status (action-status plan o c))
          (perform-status (action-status nil o c)))
      (assert (and (status-stamp perform-status) (status-keep-p perform-status)) ()
              "Just performed ~A but failed to mark it done" (action-description o c))
      (setf (action-status plan o c)
            (make-action-status
             :bits (logior (status-bits plan-status) +done-bit+)
             :stamp (status-stamp perform-status)
             :level (status-level plan-status)
             :index (status-index plan-status))))))
