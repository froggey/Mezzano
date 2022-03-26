(defpackage :asdf/contrib/plan (:use :cl)) ;; dummy, for use as package-inferred-system

(in-package :asdf/plan)

;; NB: Whenever compute-action-stamp is changed, this file must be updated
;; to insert the let-and-DBG statements in the current version of the function.
;; So if you find this file not working, it might be out of date.

(uiop:uiop-debug)

(progn
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
                         (warn "Computing just-done stamp in plan ~S for action ~S, but dependency ~S wasn't done yet!"
                               plan
                               (action-path (make-action o c))
                               (action-path (make-action do dc))))
                       status)
                      (t
                       (DBG "compute-action-stamp: forced by out of date dependency" (action-path (make-action o c)) (action-path (make-action do dc)) action-status)
                       (return (values nil nil))))))
              +status-good+))
            (dep-stamp (status-stamp dep-status)))
       ;; DBG
       (when (null dep-stamp)
         (let* ((action-path (action-path (cons o c)))
                (dep-statuses (loop for action in (direct-dependencies o c) for (o . c) = action collect
                                    (list (action-path action) (action-status plan o c)))))
           (DBG :nds action-path dep-statuses))))
     (let* (;; collect timestamps from inputs, and exit early if any is missing
            (in-files (input-files o c))
            (in-stamps (mapcar #'get-file-stamp in-files))
            (missing-in (loop :for f :in in-files :for s :in in-stamps :unless s :collect f))
            (latest-in (timestamps-latest (cons dep-stamp in-stamps))))
       (when (and missing-in (not just-done))
         (DBG "compute-action-stamp: missing inputs" (cons o c) missing-in)
         (return (values nil nil))))
     (let* (;; collect timestamps from outputs, and exit early if any is missing
            (out-files (remove-if 'null (output-files o c)))
            (out-stamps (mapcar (if just-done 'register-file-stamp 'get-file-stamp) out-files))
            (missing-out (loop :for f :in out-files :for s :in out-stamps :unless s :collect f))
            (earliest-out (timestamps-earliest out-stamps)))
       (when (and missing-out (not just-done))
         (DBG "compute-action-stamp: missing outputs" (cons o c) missing-out)
         (return (values nil nil))))
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
     ;; Status of the action as previously performed in the image
     (progn ;; DBG
       (multiple-value-bind (perform-stamp perform-done-p)
           (if just-done
               (values done-stamp t)
               (component-operation-time o c))
         (let* ((forced (action-forced-p (forcing (or plan *asdf-session*)) o c))
                (already-done-p (and all-present up-to-date-p (operation-done-p o c)
                                     (not forced)))
                (matching-stamp-p (and perform-done-p (eql perform-stamp done-stamp)))
                (action-path (action-path (cons o c))))
           (DBG "compute-action-stamp"
                action-path
                in-files
                in-stamps
                dep-stamp
                out-files
                out-stamps
                latest-in
                earliest-out
                up-to-date-p
                perform-stamp
                perform-done-p
                done-stamp
                all-present
                forced
                already-done-p
                just-done
                out-op
                matching-stamp-p))))
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
