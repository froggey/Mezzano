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
         (t (values (list* fn args) used-vars))))
      (closure
       (multiple-value-bind (result used)
           (optimize-binding fn args use-map substitutions)
         (values result (append used used-vars))))
      (t ;; Something else.
       (multiple-value-bind (result used)
           (optimize-form fn use-map substitutions)
         (values (list* result args)
                 (append used-vars used)))))))

(defun optimize-binding (closure args use-map substitutions)
  (let ((n-args (length args))
        (req (closure-required-params* closure))
        (opt (closure-optional-params closure))
        (rest (closure-rest-param closure))
        (keys-enabled (closure-keyword-params-enabled closure))
        (keys (closure-keyword-params closure)))
    (assert (>= n-args (length req)) (closure args) "Too few arguments in binding.")
    (unless (or rest keys-enabled)
      (assert (<= n-args (+ (length req) (length opt))) (closure args) "Too many arguments in binding."))
    (let ((paired-args '())
          (remaining-args args))
      (dolist (r req)
        (push (cons r (pop remaining-args)) paired-args))
      (dolist (o opt)
        (cond (remaining-args
               ;; Value.
               (push (cons (first o) (pop remaining-args)) paired-args)
               ;; Suppliedp.
               (push (cons (second o) (make-instance 'constant :value 't)) paired-args))
              (t ;; Value.
               (push (cons (first o) (make-instance 'constant :value 'nil)) paired-args)
               ;; Suppliedp.
               (push (cons (second o) (make-instance 'constant :value 'nil)) paired-args))))
      (when keys-enabled
        (assert (evenp (length remaining-args)))
        (do ((k-arg remaining-args (cddr k-arg)))
            ((null k-arg))
          ;; dealing with this is tricky...
          (unless (typep (car k-arg) 'constant)
            (error "TODO: binding keyword argument with non-constant keyword thing."))
          (unless (closure-allow-other-keywords closure)
            (unless (assoc (constant-value (car k-arg)) keys)
              (error "Unknown keyword ~S~%" (constant-value (car k-arg))))))
        (dolist (k keys)
          (do ((k-arg remaining-args (cddr k-arg)))
              ((null k-arg)
               ;; Not found.
               (push (cons (second k) (make-instance 'constant :value 'nil)) paired-args)
               (push (cons (third k) (make-instance 'constant :value 'nil)) paired-args))
            (when (eql (constant-value (car k-arg)) (first k))
              ;; Found.
              (push (cons (second k) (cadr k-arg)) paired-args)
              (push (cons (third k) (make-instance 'constant :value 't)) paired-args)
              (return)))))
      (when rest
        (when remaining-args
          (error "TODO: binding rest with args."))
        (push (cons rest (make-instance 'constant :value 'nil)) paired-args))
      ;; Substitute lexicals, constants and used-once closures.
      (multiple-value-bind (result used)
          (optimize-application (closure-body closure)
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
                      (list* (! `(clambda ,new-params ,result)) new-args))
                  used))))))

(defmethod optimize-form ((form closure) use-map &optional substitutions)
  ;; (lambda () (foo)) => foo
  (cond ((and (null (closure-required-params* form))
              (null (closure-optional-params form))
              (not (closure-rest-param form))
              (not (closure-keyword-params-enabled form))
              (null (closure-arguments form)))
         (closure-function form))
        (t (multiple-value-bind (result used)
               (optimize-application (closure-body form) use-map substitutions)
             (values (copy-closure-with-new-body form
                                                 result)
                     (remove-if (lambda (x)
                                  (or (member x (closure-required-params* form))
                                      (member x (mapcar 'first (closure-optional-params form)))
                                      (member x (mapcar 'second (closure-optional-params form)))
                                      (eql x (closure-rest-param form))
                                      (member x (mapcar 'second (closure-keyword-params form)))
                                      (member x (mapcar 'third (closure-keyword-params form)))))
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
#+nil(defun def-map (closure)
  (let ((table (make-hash-table)))
    (build-def-map closure table)
    table))

#+nil(defun build-def-map (closure table)
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

#+nil(progn
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
  form))

(defun track-one-continuation (fn arguments info)
  (check-type fn (and closure (satisfies continuationp)))
  (check-required-params-only fn)
  (mapc (lambda (p a)
          (pushnew a (gethash p info))
          (when (typep a 'lexical)
            (setf (gethash p info) (union (gethash p info)
                                          (gethash a info)))))
        (closure-required-params* fn)
        arguments))

(defun mark-closure-arguments (closure info first-is-continuation)
  (cond (first-is-continuation
         (pushnew :continuation (gethash (first (closure-required-params* closure)) info))
         (dolist (p (rest (closure-required-params* closure)))
           (pushnew :argument (gethash p info))))
        (t (dolist (p (closure-required-params* closure))
             (pushnew :argument (gethash p info)))))
  (dolist (o (closure-optional-params closure))
    (pushnew :argument (gethash (first o) info))
    (pushnew :argument (gethash (second o) info)))
  (when (closure-rest-param closure)
    (pushnew :argument (gethash (closure-rest-param closure) info)))
  (dolist (k (closure-keyword-params closure))
    (pushnew :argument (gethash (second k) info))
    (pushnew :argument (gethash (third k) info))))

(defun variable-value-tracking* (thing info)
  (when (typep thing 'closure)
    (when (not (continuationp thing))
      (mark-closure-arguments thing info t))
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
                (mark-closure-arguments cont info nil))))
           (closure
            (mark-closure-arguments (first (closure-arguments thing)) info nil))))))
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
              (= (length (closure-required-params* (third form))) 1)
              (null (closure-optional-params (third form)))
              (not (closure-rest-param (third form)))
              (not (closure-keyword-params-enabled (third form)))
              (variable-in-dynamic-contourp (first (closure-required-params* (third form)))
                                            (third form)
                                            contour-map))
         (made-a-change)
         (list (lower-block (third form) contour-map)
               (lower-block (second form) contour-map)))
        (t ;; Dunno.
         (mapcar (lambda (f) (lower-block f contour-map)) form))))

(defmethod lower-block ((form closure) contour-map)
  (copy-closure-with-new-body form
                              (lower-block-application (closure-body form)
                                                       contour-map)))

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
                       (and (every (lambda (name)
                                     (variable-in-dynamic-contourp name
                                                                   closure
                                                                   contour-map))
                                   (rest (closure-required-params* closure)))
                            (null (closure-optional-params closure))
                            (not (closure-rest-param closure))
                            (not (closure-keyword-params-enabled closure))))
                     (cddr form)))
         (made-a-change)
         (let ((cont-name (make-instance 'lexical :name (gensym "cont"))))
           (flet ((lower-one (closure)
                    (let ((closure-names (mapcar (lambda (x)
                                                   (make-instance 'lexical :name (genname x)))
                                                 (rest (closure-required-params* closure))))
                          (thunk-var (make-instance 'lexical :name (gensym "var"))))
                      (! `(clambda ,closure-names
                            ((clambda ,(closure-required-params* closure)
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
  (copy-closure-with-new-body form
                              (lower-tagbody-application (closure-body form)
                                                         contour-map)))

(defmethod lower-tagbody ((form lexical) contour-map)
  (declare (ignore contour-map))
  form)

(defmethod lower-tagbody ((form constant) contour-map)
  (declare (ignore contour-map))
  form)
