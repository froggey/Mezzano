(in-package #:sys.newc)

(defgeneric optimize-form (form use-map &optional substitutions))

(defun optimize-application (form use-map &optional substitutions)
  (let* ((used-vars '())
         (fn (first form))
         ;; Optimize arguments first.
         (args (mapcar (lambda (a)
                         (multiple-value-bind (result used)
                             (optimize-form a use-map substitutions)
                           (setf used-vars (append used used-vars))
                           result))
                       (rest form))))
    (typecase fn
      (constant
       (case (constant-value fn)
         ((funcall)
          ;; (funcall cont fn args...) -> (fn cont args...)
          (optimize-application (list* (second args) (first args) (cddr args))
                                use-map
                                substitutions))
         ((%invoke-continuation)
          ;; (%invoke-continuation (clambda ...) ...) -> ((clambda ...) ...)
          (if (and (typep (first args) 'closure)
                   (getf (plist (first args)) 'continuation))
              (optimize-application args use-map substitutions)
              (values (list* fn args) used-vars)))
         ((%tagbody)
          ;; (%tagbody cont (lambda (exit) body)) ->
          ;; ((clambda (exit) body) cont))
          (cond ((= (length args) 2)
                 (optimize-application (list (second args) (first args))
                                       use-map
                                       substitutions))
                (t (values (list* fn args) used-vars))))
         (t (values (list* fn args) used-vars))))
      (closure
       (assert (= (length args)
                  (length (closure-required-params fn))))
       (let ((paired-args (pairlis (closure-required-params fn) args)))
         ;; Substitute lexicals, constants and used-once closures.
         (multiple-value-bind (result used)
             (optimize-application (closure-body fn)
                                   use-map
                                   (append (remove-if (lambda (x)
                                                        (and (typep (cdr x) 'closure)
                                                             (not (= (length (gethash (car x) use-map)) 1))))
                                                      paired-args)
                                           substitutions))
           ;; Eliminate unused arguments.
           (let* ((used-args (remove-if-not (lambda (x) (member x used)) paired-args :key 'car))
                  (new-args (mapcar 'cdr used-args))
                  (new-params (mapcar 'car used-args)))
             (values (if (null new-args)
                         ;; ((lambda () body)) -> body.
                         (multiple-value-bind (r2 u2)
                             (optimize-application result use-map substitutions)
                           (setf used-args (union u2 used-args))
                           r2)
                         ;; Closures used in bindings are always treated as continuations.
                         (list* (! `(clambda
                                      ,new-params ,result)) new-args))
                     (append used-vars used))))))
      (t ;; Something else.
       (multiple-value-bind (result used)
           (optimize-form fn use-map substitutions)
         (values (list* result args)
                 (append used-vars used)))))))

(defmethod optimize-form ((form closure) use-map &optional substitutions)
  ;; (lambda () (foo)) => foo
  (cond ((and (null (closure-required-params form))
              (null (closure-arguments form)))
         (closure-function form))
        (t (multiple-value-bind (result used)
               (optimize-application (closure-body form) use-map substitutions)
             (values (make-instance 'closure
                                    :name (closure-name form)
                                    :required-params (closure-required-params form)
                                    :body result
                                    :plist (list 'continuation (getf (plist form) 'continuation)))
                     (remove-if (lambda (x) (member x (closure-required-params form)))
                                used))))))

(defmethod optimize-form ((form constant) use-map &optional substitutions)
  (declare (ignore substitutions use-map))
  (values form '()))

(defmethod optimize-form ((form lexical) use-map &optional substitutions)
  ;; Substitute lexicals.
  (let ((replacement (assoc form substitutions)))
    (if replacement
        (optimize-form (cdr replacement) use-map substitutions)
        (values form (list form)))))

;; Build a hash-table mapping variables and closures to their uses.
(defun use-map (closure)
  (let ((table (make-hash-table)))
    (build-use-map closure table)
    table))

(defun build-use-map (closure table)
  (dolist (x (closure-body closure))
    (push closure (gethash x table))
    (when (typep x 'closure)
      (build-use-map x table))))

;; Build a hash-table mapping variables to their definitions.
(defun def-map (closure)
  (let ((table (make-hash-table)))
    (build-def-map closure table)
    table))

(defun build-def-map (closure table)
  (dolist (var (closure-required-params closure))
    (setf (gethash var table) closure))
  (dolist (x (closure-body closure))
    (when (typep x 'closure)
      (build-def-map x table))))

(defun filter-map (fn map)
  (let ((results '()))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (funcall fn k map) (push k results)))
             map)
    results))

;; Replace one form with another, by rebuilding the tree.
(defgeneric substitute-form (form target replacement))

(defmethod substitute-form :around (form target replacement)
  (if (eql form target)
      replacement
      (call-next-method)))

(defmethod substitute-form ((form closure) target replacement)
  (make-instance 'closure
                 :name (closure-name form)
                 :required-args (closure-required-params form)
                 :body (let ((form (closure-body form)))
                         (list* (substitute-form (first form)
                                                 target replacement)
                                (mapcar (lambda (f)
                                          (substitute-form f target replacement))
                                        (rest form))))))

(defmethod substitute-form ((form lexical) target replacement)
  (declare (ignore target replacement))
  form)

(defmethod substitute-form ((form constant) target replacement)
  (declare (ignore target replacement))
  form)

(defun track-one-continuation (fn arguments info)
  (check-type fn (and closure (satisfies continuationp)))
  (mapc (lambda (p a)
          (pushnew a (gethash p info))
          (when (typep a 'lexical)
            (setf (gethash p info) (union (gethash p info)
                                          (gethash a info)))))
        (closure-required-params fn)
        arguments))

(defun variable-value-tracking* (thing info)
  (when (typep thing 'closure)
    (when (not (continuationp thing))
      (pushnew :continuation (gethash (first (closure-required-params thing)) info))
      (dolist (p (rest (closure-required-params thing)))
        (pushnew :argument (gethash p info))))
    (let ((fn (closure-function thing)))
      (cond
        ((typep fn 'closure)
         (track-one-continuation fn (closure-arguments thing) info))
        ((and (typep fn 'constant)
              (eql (constant-value fn) '%invoke-continuation))
         (check-type (first (closure-arguments thing)) lexical)
         (dolist (cont (gethash (first (closure-arguments thing)) info))
           (unless (or (eql cont :continuation)
                       (typep cont 'lexical))
             (track-one-continuation cont (rest (closure-arguments thing)) info))))
        (t ;; Call to the outside.
         (etypecase (first (closure-arguments thing))
           (lexical
            (dolist (cont (gethash (first (closure-arguments thing)) info))
              (unless (or (eql cont :continuation)
                          (typep cont 'lexical))
                (dolist (param (closure-required-params cont))
                  (pushnew :argument (gethash param info))))))
           (closure
            (dolist (param (closure-required-params (first (closure-arguments thing))))
              (pushnew :argument (gethash param info))))))))
    (dolist (x (closure-body thing))
      (variable-value-tracking* x info))))

;; FIXME: Should iterate until no change.
(defun variable-value-tracking (closure &optional (info (make-hash-table)))
  "Compute potential values for every variable."
  (variable-value-tracking* closure info)
  info)

(defun inverse-variable-value-tracking (variable-track)
  (let ((result (make-hash-table)))
    (maphash (lambda (k v)
               (dolist (v* v)
                 (pushnew k (gethash v* result))))
             variable-track)
    result))

(defgeneric dynamic-contour-analysis* (info thing outer))

(defmethod dynamic-contour-analysis* (info (thing closure) outer)
  (when (not (continuationp thing))
    (setf outer thing))
  (dynamic-contour-analysis* info (closure-function thing) outer)
  (cond ((not (typep (closure-function thing) 'closure))
         (let (args)
           (cond ((and (typep (closure-function thing) 'constant)
                       (eql (constant-value (closure-function thing)) '%if))
                  ;; %IF takes two continuations.
                  (dynamic-contour-analysis* info (first (closure-arguments thing)) outer)
                  (dynamic-contour-analysis* info (second (closure-arguments thing)) outer)
                  (setf args (cddr (closure-arguments thing))))
                 (t (dynamic-contour-analysis* info (first (closure-arguments thing)) outer)
                    (setf args (cdr (closure-arguments thing)))))
           (dolist (x args)
             (cond ((and (typep x 'closure) (continuationp x))
                    (dynamic-contour-analysis* info x x))
                   (t (dynamic-contour-analysis* info x outer))))))
        (t (dolist (x (closure-arguments thing))
             (cond ((and (typep x 'closure) (continuationp x))
                    (dynamic-contour-analysis* info x x))
                   (t (dynamic-contour-analysis* info x outer)))))))

(defmethod dynamic-contour-analysis* (info (thing lexical) outer)
  (pushnew outer (gethash thing info)))

(defmethod dynamic-contour-analysis* (info (thing constant) outer))

(defun dynamic-contour-analysis (closure &optional (info (make-hash-table)))
  (dynamic-contour-analysis* info closure nil)
  (let* ((lexical-track (variable-value-tracking closure))
         (inverse-lexical-track (inverse-variable-value-tracking lexical-track)))
    ;; Now do some kind of union over each variable,
    ;; replacing continuations with closures.
    ;; I don't know what I'm doing. :D
    ;; Mash until no change.
    (flet ((frob (prev)
             "Perform one step of contour resolution."
             (let ((result (make-hash-table)))
               (maphash (lambda (k v)
                          (dolist (v* v)
                            (cond ((continuationp v*)
                                   (setf (gethash k result) (union (gethash k result)
                                                                   (mapcan (lambda (var)
                                                                             (copy-list (gethash var info)))
                                                                           (gethash v* inverse-lexical-track)))))
                                  (t (pushnew v* (gethash k result))))))
                        prev)
               result))
           (done-crunching? (old new)
             "Compare two hash-tables, test if the contents are identical."
             (maphash (lambda (old-k old-v)
                        (let ((new-v (gethash old-k new)))
                          (unless (and (every (lambda (o) (member o new-v)) old-v)
                                       (every (lambda (n) (member n old-v)) new-v))
                            (return-from done-crunching? nil))))
                      old)
             t))
      (do* ((prev info new)
            (new (frob prev) (frob prev)))
           ((done-crunching? prev new)
            (setf info new))))
    ;; Purge the result table of any non-closures and continuations.
    (maphash (lambda (k v)
               (declare (ignore v))
               (setf (gethash k info) (remove-if-not (lambda (x)
                                                       (and (typep x 'closure)
                                                            (not (continuationp x))))
                                                     (gethash k info))))
             info)
    info))

(defun variable-in-dynamic-contourp (variable contour contour-map)
  "Test if VARIABLE is only used in CONTOUR, or not used at all."
  (every (lambda (x) (eql x contour)) (gethash variable contour-map)))

(defgeneric lower-block (form contour-map))

(defun lower-block-application (form contour-map)
  (cond ((and (typep (first form) 'constant)
              (eql (constant-value (first form)) '%block)
              (= (length (rest form)) 2)
              (and (typep (third form) 'closure)
                   (not (continuationp (third form))))
              (= (length (closure-required-params (third form))) 1)
              (variable-in-dynamic-contourp (first (closure-required-params (third form)))
                                            (closure-required-params (third form))
                                            contour-map))
         (made-a-change)
         (list (lower-block (third form) contour-map)
               (lower-block (second form) contour-map)))
        (t ;; Dunno.
         (mapcar (lambda (f) (lower-block f contour-map)) form))))

(defmethod lower-block ((form closure) contour-map)
  (make-instance 'closure
                 :name (closure-name form)
                 :required-params (closure-required-params form)
                 :body (lower-block-application (closure-body form)
                                    contour-map)
                 :plist (plist form)))

(defmethod lower-block ((form lexical) contour-map)
  (declare (ignore contour-map))
  form)

(defmethod lower-block ((form constant) contour-map)
  (declare (ignore contour-map))
  form)

(defgeneric lower-tagbody (form contour-map))

(defun lower-tagbody-application (form contour-map)
  (cond ((and (typep (first form) 'constant)
              (eql (constant-value (first form)) '%tagbody)
              (> (length form) 2) ; (%tagbody cont form1 ...)
              (every (lambda (closure)
                       (every (lambda (name)
                                (variable-in-dynamic-contourp name
                                                              closure
                                                              contour-map))
                              (rest (closure-required-params closure))))
                     (cddr form)))
         (made-a-change)
         (let ((cont-name (make-instance 'lexical :name (gensym "cont"))))
           (flet ((lower-one (closure)
                    (let ((closure-names (mapcar (lambda (x)
                                                   (make-instance 'lexical :name (genname x)))
                                                 (rest (closure-required-params closure))))
                          (thunk-var (make-instance 'lexical :name (gensym "var"))))
                      (! `(clambda ,closure-names
                            ((clambda ,(closure-required-params closure)
                               ,(lower-tagbody-application (closure-body closure) contour-map))
                             (clambda (,thunk-var)
                               (%invoke-continuation ,cont-name nil))
                             ,@(mapcar (lambda (name)
                                         (! `(clambda (,(make-instance 'lexical))
                                               (%invoke-continuation ,name ,@closure-names))))
                                       closure-names)))))))
             (list (! `(clambda (,cont-name)
                         ,(mapcar #'lower-one (cddr form))))
                   (lower-tagbody (second form) contour-map)))))
        (t ;; Dunno.
         (mapcar (lambda (f) (lower-tagbody f contour-map)) form))))

(defmethod lower-tagbody ((form closure) contour-map)
  (make-instance 'closure
                 :name (closure-name form)
                 :required-params (closure-required-params form)
                 :body (lower-tagbody-application (closure-body form)
                                    contour-map)
                 :plist (plist form)))

(defmethod lower-tagbody ((form lexical) contour-map)
  (declare (ignore contour-map))
  form)

(defmethod lower-tagbody ((form constant) contour-map)
  (declare (ignore contour-map))
  form)
