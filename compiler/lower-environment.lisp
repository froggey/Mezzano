;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Lower non-local lexical variable accesses so they refer directly
;;;; to environment objects.
;;;; This is done in two passes.
;;;; Pass 1 discovers escaping variables & assigns them slots
;;;; in their environment vector. Determines the extent of every lambda.
;;;; Pass 2 links each environment vector together and actually
;;;; rewrites the code.
;;;; Vectors are created at LAMBDA and TAGBODY nodes.

(in-package :sys.c)

(defvar *environment-chain*)
(defvar *environment-layout*)
(defvar *environment-layout-dx*)
(defvar *active-environment-vector*)
(defvar *allow-dx-environment*)
(defvar *environment-allocation-mode* nil)
(defvar *free-variables*)
(defvar *environment*)
(defvar *lambda-parents*)
(defvar *current-closure-set*)

(defun lower-environment (lambda)
  (let ((*environment-layout* (make-hash-table))
        (*environment-layout-dx* (make-hash-table))
        (*allow-dx-environment* 't)
        (*current-lambda* nil)
        (*lambda-parents* (make-hash-table))
        (*free-variables* (compute-free-variable-sets lambda))
        (*current-closure-set* '()))
    (compute-environment-layout lambda)
    (let ((*environment* '()))
      (lower-env-form lambda))))

(defun quoted-form-p (form)
  (typep form 'ast-quote))

(defgeneric compute-environment-layout (form))

(defmethod compute-environment-layout ((form ast-block))
  "BLOCK defines one variable."
  (maybe-add-environment-variable (info form))
  (compute-environment-layout (body form)))

(defmethod compute-environment-layout ((form ast-function))
  nil)

(defmethod compute-environment-layout ((form ast-go))
  (compute-environment-layout (info form)))

(defmethod compute-environment-layout ((form ast-if))
  (compute-environment-layout (test form))
  (compute-environment-layout (if-then form))
  (compute-environment-layout (if-else form)))

(defmethod compute-environment-layout ((form ast-let))
  (dolist (binding (bindings form))
    (maybe-add-environment-variable (first binding))
    (compute-environment-layout (second binding)))
  (compute-environment-layout (body form)))

(defmethod compute-environment-layout ((form ast-multiple-value-bind))
  (dolist (binding (bindings form))
    (maybe-add-environment-variable binding))
  (compute-environment-layout (value-form form))
  (compute-environment-layout (body form)))

(defmethod compute-environment-layout ((form ast-multiple-value-call))
  (compute-environment-layout (function-form form))
  (compute-environment-layout (value-form form)))

(defmethod compute-environment-layout ((form ast-multiple-value-prog1))
  (compute-environment-layout (value-form form))
  (compute-environment-layout (body form)))

(defmethod compute-environment-layout ((form ast-progn))
  (mapc #'compute-environment-layout (forms form)))

(defmethod compute-environment-layout ((form ast-quote))
  nil)

(defmethod compute-environment-layout ((form ast-return-from))
  (compute-environment-layout (value form))
  (compute-environment-layout (info form)))

(defmethod compute-environment-layout ((form ast-setq))
  (compute-environment-layout (value form)))

(defmethod compute-environment-layout ((form ast-tagbody))
  "TAGBODY defines a single variable in the enclosing environment and each group
of statements opens a new contour."
  (maybe-add-environment-variable (info form))
  (let ((env-is-dx t))
    (let ((*active-environment-vector* (info form))
          (*allow-dx-environment* t)
          (*current-closure-set* '()))
      (loop
         for (go-tag stmt) in (statements form) do
           (unless (finalize-environment-layout *active-environment-vector*)
             (setf env-is-dx nil))
           (setf *active-environment-vector* go-tag
                 *allow-dx-environment* t
                 *current-closure-set* '())
           (compute-environment-layout stmt))
      (unless (finalize-environment-layout *active-environment-vector*)
        (setf env-is-dx nil)))
    (unless env-is-dx
      (setf *allow-dx-environment* nil))))

(defmethod compute-environment-layout ((form ast-the))
  (compute-environment-layout (value form)))

(defmethod compute-environment-layout ((form ast-unwind-protect))
  (compute-environment-layout (protected-form form))
  (when (and (lambda-information-p (cleanup-function form))
             (not (getf (lambda-information-plist (cleanup-function form)) 'extent)))
    (setf (getf (lambda-information-plist (cleanup-function form)) 'extent) :dynamic))
  (compute-environment-layout (cleanup-function form)))

(defmethod compute-environment-layout ((form ast-call))
  (cond ((and (eql (name form) 'mezzano.runtime::%funcall)
              (lambda-information-p (first (arguments form))))
         (unless (getf (lambda-information-plist (first (arguments form))) 'extent)
           (setf (getf (lambda-information-plist (first (arguments form))) 'extent) :dynamic))
         (compute-lambda-environment-layout (first (arguments form)))
         (mapc #'compute-environment-layout (rest (arguments form))))
        (t (mapc #'compute-environment-layout (arguments form)))))

(defmethod compute-environment-layout ((form ast-jump-table))
  (compute-environment-layout (value form))
  (mapc #'compute-environment-layout (targets form)))

(defmethod compute-environment-layout ((form lexical-variable))
  nil)

(defmethod compute-environment-layout ((form lambda-information))
  (setf (getf (lambda-information-plist form) 'dynamic-extent) :indefinite)
  (compute-lambda-environment-layout form))

(defun maybe-add-environment-variable (variable)
  (when (and (not (typep variable 'special-variable))
             (not (localp variable)))
    (push variable (gethash *active-environment-vector* *environment-layout*))))

(defun lambda-is-dynamic-extent-p (lambda)
  (or (eql (getf (lambda-information-plist lambda) 'extent) :dynamic)
      (getf (lambda-information-plist lambda) 'declared-dynamic-extent)))

(defun lambda-tree-is-dynamic-extent-p (lambda end)
  (cond ((eql lambda end)
         t)
        (t
         (and (lambda-is-dynamic-extent-p lambda)
              (lambda-tree-is-dynamic-extent-p (gethash lambda *lambda-parents*) end)))))

(defun finalize-environment-layout (env)
  ;; Inner environments must be DX, and every variable (including the parent
  ;; backlink) in this environment must only be accessed by DX lambdas.
  (when (and *allow-dx-environment*
             (not *perform-tce*)
             (every (lambda (var)
                      (every (lambda (l)
                               (or (eql (lexical-variable-definition-point var) l)
                                   (lambda-tree-is-dynamic-extent-p l *current-lambda*)))
                             (lexical-variable-used-in var)))
                    (gethash env *environment-layout*))
             (every #'lambda-is-dynamic-extent-p *current-closure-set*))
    (setf (gethash env *environment-layout-dx*) t)
    t))

(defun check-simple-lambda-parameters (lambda)
  "Ensure that lambda only has simple parameters.
Keyword arguments, non-constant init-forms and special variables are disallowed."
  (loop for arg in (lambda-information-required-args lambda) do
       (assert (lexical-variable-p arg)))
  (loop for (arg init-form suppliedp) in (lambda-information-optional-args lambda) do
       (assert (lexical-variable-p arg))
       (assert (quoted-form-p init-form))
       (when suppliedp
         (assert (lexical-variable-p suppliedp))))
  (when (lambda-information-rest-arg lambda)
    (assert (lexical-variable-p (lambda-information-rest-arg lambda))))
  (when (lambda-information-fref-arg lambda)
    (assert (lexical-variable-p (lambda-information-fref-arg lambda))))
  (when (lambda-information-closure-arg lambda)
    (assert (lexical-variable-p (lambda-information-closure-arg lambda))))
  (when (lambda-information-count-arg lambda)
    (assert (lexical-variable-p (lambda-information-count-arg lambda))))
  (assert (not (lambda-information-enable-keys lambda))))

(defun compute-lambda-environment-layout (lambda)
  (let ((env-is-dx nil))
    (setf (gethash lambda *lambda-parents*) *current-lambda*)
    (let ((*active-environment-vector* lambda)
          (*allow-dx-environment* t)
          (*current-lambda* lambda)
          (*current-closure-set* '()))
      (assert (null (lambda-information-environment-arg lambda)))
      (check-simple-lambda-parameters lambda)
      (dolist (arg (lambda-information-required-args lambda))
        (maybe-add-environment-variable arg))
      (dolist (arg (lambda-information-optional-args lambda))
        (maybe-add-environment-variable (first arg))
        (when (third arg)
          (maybe-add-environment-variable (third arg))))
      (when (lambda-information-rest-arg lambda)
        (maybe-add-environment-variable (lambda-information-rest-arg lambda)))
      (when (lambda-information-fref-arg lambda)
        (maybe-add-environment-variable (lambda-information-fref-arg lambda)))
      (when (lambda-information-closure-arg lambda)
        (maybe-add-environment-variable (lambda-information-closure-arg lambda)))
      (when (lambda-information-count-arg lambda)
        (maybe-add-environment-variable (lambda-information-count-arg lambda)))
      (compute-environment-layout (lambda-information-body lambda))
      (setf env-is-dx (finalize-environment-layout lambda)))
    (when (gethash lambda *free-variables*)
      (push lambda *current-closure-set*))
    (unless env-is-dx
      (setf *allow-dx-environment* nil))))

(defun compute-free-variable-sets (lambda)
  (let ((*free-variables* (make-hash-table)))
    (compute-free-variable-sets-1 lambda)
    *free-variables*))

(defgeneric compute-free-variable-sets-1 (form))

(defun compute-free-variable-sets-form-list (forms)
  (reduce 'union (mapcar #'compute-free-variable-sets-1 forms) :initial-value '()))

(defmethod compute-free-variable-sets-1 ((form ast-block))
  (remove (info form) (compute-free-variable-sets-1 (body form))))

(defmethod compute-free-variable-sets-1 ((form ast-function))
  '())

(defmethod compute-free-variable-sets-1 ((form ast-go))
  (compute-free-variable-sets-1 (info form)))

(defmethod compute-free-variable-sets-1 ((form ast-if))
  (union (compute-free-variable-sets-1 (test form))
         (union (compute-free-variable-sets-1 (if-then form))
                (compute-free-variable-sets-1 (if-else form)))))

(defmethod compute-free-variable-sets-1 ((form ast-let))
  (let ((vars (union (compute-free-variable-sets-form-list (mapcar #'second (bindings form)))
                     (compute-free-variable-sets-1 (body form))))
        (defs (mapcar #'first (bindings form))))
    (set-difference vars defs)))

(defmethod compute-free-variable-sets-1 ((form ast-multiple-value-bind))
  (let ((vars (union (compute-free-variable-sets-1 (value-form form))
                     (compute-free-variable-sets-1 (body form))))
        (defs (bindings form)))
    (set-difference vars defs)))

(defmethod compute-free-variable-sets-1 ((form ast-multiple-value-call))
  (union (compute-free-variable-sets-1 (function-form form))
         (compute-free-variable-sets-1 (value-form form))))

(defmethod compute-free-variable-sets-1 ((form ast-multiple-value-prog1))
  (union (compute-free-variable-sets-1 (value-form form))
         (compute-free-variable-sets-1 (body form))))

(defmethod compute-free-variable-sets-1 ((form ast-progn))
  (compute-free-variable-sets-form-list (forms form)))

(defmethod compute-free-variable-sets-1 ((form ast-quote))
  '())

(defmethod compute-free-variable-sets-1 ((form ast-return-from))
  (union (compute-free-variable-sets-1 (value form))
         (compute-free-variable-sets-1 (info form))))

(defmethod compute-free-variable-sets-1 ((form ast-setq))
  (union (list (setq-variable form))
         (compute-free-variable-sets-1 (value form))))

(defmethod compute-free-variable-sets-1 ((form ast-tagbody))
  (remove (info form)
          (compute-free-variable-sets-form-list (mapcar #'second (statements form)))))

(defmethod compute-free-variable-sets-1 ((form ast-the))
  (compute-free-variable-sets-1 (value form)))

(defmethod compute-free-variable-sets-1 ((form ast-unwind-protect))
  (union (compute-free-variable-sets-1 (protected-form form))
         (compute-free-variable-sets-1 (cleanup-function form))))

(defmethod compute-free-variable-sets-1 ((form ast-call))
  (compute-free-variable-sets-form-list (arguments form)))

(defmethod compute-free-variable-sets-1 ((form ast-jump-table))
  (union (compute-free-variable-sets-1 (value form))
         (compute-free-variable-sets-form-list (targets form))))

(defmethod compute-free-variable-sets-1 ((form lexical-variable))
  (list form))

(defmethod compute-free-variable-sets-1 ((form lambda-information))
  (let* ((initforms (append (mapcar #'second (lambda-information-optional-args form))
                            (mapcar #'second (lambda-information-key-args form))))
         (defs (append (lambda-information-required-args form)
                       (mapcar #'first (lambda-information-optional-args form))
                       (mapcar #'third (lambda-information-optional-args form))
                       (mapcar #'second (mapcar #'first (lambda-information-key-args form)))
                       (mapcar #'third (lambda-information-key-args form))
                       (when (lambda-information-rest-arg form)
                         (list (lambda-information-rest-arg form)))
                       (when (lambda-information-environment-arg form)
                         (list (lambda-information-environment-arg form)))
                       (when (lambda-information-fref-arg form)
                         (list (lambda-information-fref-arg form)))
                       (when (lambda-information-closure-arg form)
                         (list (lambda-information-closure-arg form)))
                       (when (lambda-information-count-arg form)
                         (list (lambda-information-count-arg form)))))
         (vars (set-difference (union (compute-free-variable-sets-form-list initforms)
                                      (compute-free-variable-sets-1 (lambda-information-body form)))
                               defs)))
    (setf (gethash form *free-variables*) vars)
    vars))

(defgeneric lower-env-form (form))

(defmethod lower-env-form ((form ast-block))
  (ast `(block ,(info form)
          (progn
            ,@(when (not (localp (info form)))
                (let ((env-var (second (first *environment-chain*)))
                      (env-offset (1+ (position (info form) (gethash (first *environment*) *environment-layout*)))))
                  (setf (block-information-env-var (info form)) env-var
                        (block-information-env-offset (info form)) env-offset)
                  (list
                   `(call (setf sys.int::%object-ref-t) ,(info form) ,env-var (quote ,env-offset)))))
            ,(lower-env-form (body form))))
       form))

(defmethod lower-env-form ((form ast-function))
  form)

(defmethod lower-env-form ((form ast-go))
  (setf (info form) (lower-env-form (info form)))
  form)

(defmethod lower-env-form ((form ast-if))
  (setf (test form) (lower-env-form (test form))
        (if-then form) (lower-env-form (if-then form))
        (if-else form) (lower-env-form (if-else form)))
  form)

(defmethod lower-env-form ((form ast-let))
  (setf (bindings form)
        (loop
           for (variable init-form) in (bindings form)
           collect (list variable (if (or (typep variable 'special-variable)
                                          (localp variable))
                                      (lower-env-form init-form)
                                      (ast `(call (setf sys.int::%object-ref-t)
                                                  ,(lower-env-form init-form)
                                                  ,(second (first *environment-chain*))
                                                  (quote ,(1+ (position variable (gethash (first *environment*) *environment-layout*)))))
                                           form)))))
  (setf (body form) (lower-env-form (body form)))
  form)

(defmethod lower-env-form ((form ast-multiple-value-bind))
  (ast `(multiple-value-bind ,(bindings form)
            ,(lower-env-form (value-form form))
          (progn
            ,@(mapcan (lambda (var)
                        (when (and (not (typep var 'special-variable))
                                   (not (localp var)))
                          (list `(call (setf sys.int::%object-ref-t)
                                       ,var
                                       ,(second (first *environment-chain*))
                                       (quote ,(1+ (position var (gethash (first *environment*) *environment-layout*))))))))
                      (bindings form))
            ,(lower-env-form (body form))))
       form))

(defmethod lower-env-form ((form ast-multiple-value-call))
  (ast `(multiple-value-call
            ,(lower-env-form (function-form form))
          ,(lower-env-form (value-form form)))
       form))

(defmethod lower-env-form ((form ast-multiple-value-prog1))
  (ast `(multiple-value-prog1
            ,(lower-env-form (value-form form))
          ,(lower-env-form (body form)))
       form))

(defmethod lower-env-form ((form ast-progn))
  (ast `(progn ,@(mapcar #'lower-env-form (forms form)))
       form))

(defmethod lower-env-form ((form ast-quote))
  form)

(defmethod lower-env-form ((form ast-return-from))
  (setf (value form) (lower-env-form (value form))
        (info form) (lower-env-form (info form)))
  form)

(defmethod lower-env-form ((form ast-setq))
  (cond ((localp (setq-variable form))
         (setf (value form) (lower-env-form (value form)))
         form)
        (t (dolist (e *environment*
                    (error "Can't find variable ~S in environment." (setq-variable form)))
             (let* ((layout (gethash e *environment-layout*))
                    (offset (position (setq-variable form) layout)))
               (when offset
                 (return (ast `(call (setf sys.int::%object-ref-t)
                                     ,(lower-env-form (value form))
                                     ,(get-env-vector e)
                                     (quote ,(1+ offset)))
                              form))))))))

(defmethod lower-env-form ((form ast-tagbody))
  (let* ((possible-env-vector-heads (list* (info form)
                                           (mapcar #'first (statements form))))
         (env-vector-heads (remove-if (lambda (x) (endp (gethash x *environment-layout*)))
                                      possible-env-vector-heads))
         (new-envs (loop for i in env-vector-heads
                      collect (list i
                                    (make-instance 'lexical-variable
                                                   :inherit form
                                                   :name (gensym "Environment")
                                                   :definition-point *current-lambda*
                                                   :plist (list 'hide-from-debug-info t))
                                    (gethash i *environment-layout*)))))
    (labels ((frob-outer ()
               (let* ((new-statements (frob-inner (info form)))
                      (old-entry (first (first new-statements))))
                 ;; Avoid adding new statements if it's not needed.
                 (if (or (not (localp (info form)))
                         (assoc (info form) new-envs))
                     (ast `(tagbody ,(info form)
                              (entry (progn
                                       ;; Save the tagbody info.
                                       ,@(when (not (localp (info form)))
                                               (let ((env-var (second (first *environment-chain*)))
                                                     (env-offset (1+ (position (info form) (gethash (first *environment*) *environment-layout*)))))
                                                 (setf (tagbody-information-env-var (info form)) env-var
                                                       (tagbody-information-env-offset (info form)) env-offset)
                                                 (list `(call (setf sys.int::%object-ref-t)
                                                              ,(info form)
                                                              ,env-var
                                                              (quote ,env-offset)))))
                                       ,@(let ((info (assoc (info form) new-envs)))
                                              (when info
                                                (if *environment*
                                                    (list `(setq ,(second info) ,(generate-make-environment (info form) (1+ (length (third info)))))
                                                          `(call (setf sys.int::%object-ref-t)
                                                                 ,(second (first *environment-chain*))
                                                                 ,(second info)
                                                                 (quote 0)))
                                                    (list `(setq ,(second info)
                                                                 ,(generate-make-environment (info form) (1+ (length (third info)))))))))
                                       (go ,old-entry ,(info form))))
                              ,@new-statements)
                          form)
                     (ast `(tagbody ,(info form)
                              ,@new-statements)
                          form))))
             (frob-inner (current-env)
               (loop
                  for (go-tag stmt) in (statements form)
                  collect (let ((info (assoc go-tag new-envs)))
                            (setf current-env go-tag)
                            (list go-tag
                                  (ast `(progn
                                          ,@(when info
                                              (list `(setq ,(second info)
                                                           ,(generate-make-environment current-env (1+ (length (third info)))))))
                                          ,@(when (and info *environment*)
                                              (list `(call (setf sys.int::%object-ref-t)
                                                           ,(second (first *environment-chain*))
                                                           ,(second info)
                                                           (quote 0))))
                                          ,(if info
                                               (let ((*environment-chain* (list* (list current-env (second info))
                                                                                 *environment-chain*))
                                                     (*environment* (list* current-env *environment*)))
                                                 (lower-env-form stmt))
                                               (lower-env-form stmt)))
                                       stmt))))))
      (if (endp new-envs)
          (frob-outer)
          (ast `(let ,(loop
                         for (stmt env layout) in new-envs
                         collect (list env (ast `(quote nil))))
                  ,(frob-outer))
               form)))))

(defmethod lower-env-form ((form ast-the))
  (setf (value form) (lower-env-form (value form)))
  form)

(defmethod lower-env-form ((form ast-unwind-protect))
  (ast `(unwind-protect
             ,(lower-env-form (protected-form form))
          ,(lower-env-form (cleanup-function form)))
       form))

(defmethod lower-env-form ((form ast-call))
  (ast `(call
         ,(name form)
         ,@(mapcar #'lower-env-form (arguments form)))
       form))

(defmethod lower-env-form ((form ast-jump-table))
  (ast `(jump-table
         ,(lower-env-form (value form))
         ,@(mapcar #'lower-env-form (targets form)))
       form))

(defmethod lower-env-form ((form lexical-variable))
  (if (localp form)
      form
      (dolist (e *environment*
               (error "Can't find variable ~S in environment." form))
        (let* ((layout (gethash e *environment-layout*))
               (offset (position form layout)))
          (when offset
            (return (ast `(call sys.int::%object-ref-t
                                ,(get-env-vector e)
                                (quote ,(1+ offset)))
                         form)))))))

(defun lower-env-lambda (lambda)
  (let ((*environment-chain* '())
        (*environment* *environment*)
        (local-env (gethash lambda *environment-layout*))
        (*current-lambda* lambda)
        (*environment-allocation-mode* (let* ((declares (getf (lambda-information-plist lambda) :declares))
                                              (mode (assoc 'sys.c::closure-allocation declares)))
                                         (if (and mode (cdr mode))
                                             (second mode)
                                             *environment-allocation-mode*))))
    (when *environment*
      ;; The entry environment vector.
      (let ((env (make-instance 'lexical-variable
                                :inherit lambda
                                :name (gensym "Environment")
                                :definition-point lambda
                                :plist (list 'hide-from-debug-info t))))
        (setf (lambda-information-environment-arg lambda) env)
        (push (list (first *environment*) env) *environment-chain*)))
    (cond ((not (endp local-env))
           ;; Environment is present, rewrite body with a new vector.
           (let ((new-env (make-instance 'lexical-variable
                                         :inherit lambda
                                         :name (gensym "Environment")
                                         :definition-point lambda
                                         :plist (list 'hide-from-debug-info t))))
             (flet ((set-var (var)
                      `(call (setf sys.int::%object-ref-t)
                             ,var
                             ,new-env
                             (quote ,(1+ (position var local-env))))))
               (push (list lambda new-env) *environment-chain*)
               (push lambda *environment*)
               (setf (lambda-information-environment-layout lambda) (compute-environment-layout-debug-info))
               (setf (lambda-information-body lambda)
                     (ast `(let ((,new-env ,(generate-make-environment lambda (1+ (length local-env)))))
                             (progn
                               ,@(when (rest *environment-chain*)
                                       (list `(call (setf sys.int::%object-ref-t)
                                                    ,(second (second *environment-chain*))
                                                    ,new-env
                                                    (quote 0))))
                               ,@(mapcar (lambda (arg)
                                           (set-var arg))
                                         (remove-if #'localp (lambda-information-required-args lambda)))
                               ,@(mapcar (lambda (arg)
                                           (set-var (first arg)))
                                         (remove-if #'localp (lambda-information-optional-args lambda)
                                                    :key #'first))
                               ,@(mapcar (lambda (arg)
                                           (set-var (third arg)))
                                         (remove-if #'(lambda (x) (or (null x) (localp x)))
                                                    (lambda-information-optional-args lambda)
                                                    :key #'third))
                               ,@(when (and (lambda-information-rest-arg lambda)
                                            (not (localp (lambda-information-rest-arg lambda))))
                                       (list (set-var (lambda-information-rest-arg lambda))))
                               ,@(when (and (lambda-information-fref-arg lambda)
                                            (not (localp (lambda-information-fref-arg lambda))))
                                       (list (set-var (lambda-information-fref-arg lambda))))
                               ,@(when (and (lambda-information-closure-arg lambda)
                                            (not (localp (lambda-information-closure-arg lambda))))
                                       (list (set-var (lambda-information-closure-arg lambda))))
                               ,@(when (and (lambda-information-count-arg lambda)
                                            (not (localp (lambda-information-count-arg lambda))))
                                       (list (set-var (lambda-information-count-arg lambda))))
                               ,(lower-env-form (lambda-information-body lambda))))
                          lambda)))))
          (t (setf (lambda-information-environment-layout lambda) (compute-environment-layout-debug-info))
             (setf (lambda-information-body lambda) (lower-env-form (lambda-information-body lambda)))))
    lambda))

(defmethod lower-env-form ((form lambda-information))
  (cond ((or (not *environment-chain*)
             (endp (gethash form *free-variables*)))
         (let ((*environment* '()))
           (lower-env-lambda form)))
        ((and (getf (lambda-information-plist form) 'declared-dynamic-extent)
              (not *perform-tce*))
         (ast `(call sys.c::make-dx-closure
                     ,(lower-env-lambda form)
                     ,(second (first *environment-chain*)))
              form))
        (*environment-allocation-mode*
         (ast `(call sys.int::make-closure
                     ,(lower-env-lambda form)
                     ,(second (first *environment-chain*))
                     (quote ,*environment-allocation-mode*))
              form))
        (t (ast `(call sys.int::make-closure
                       ,(lower-env-lambda form)
                       ,(second (first *environment-chain*)))
                form))))

(defvar *environment-chain* nil
  "The directly accessible environment vectors in this function.")

(defun compute-environment-layout-debug-info ()
  (when *environment*
    (list (second (first *environment-chain*))
          (mapcar (lambda (env)
                    (mapcar (lambda (x)
                              (if (or (tagbody-information-p x)
                                      (block-information-p x))
                                  nil
                                  (lexical-variable-name x)))
                            (gethash env *environment-layout*)))
                  *environment*))))

(defun generate-make-environment (lambda size)
  (ast (cond ((gethash lambda *environment-layout-dx*)
              ;; DX allocation.
              `(call sys.c::make-dx-simple-vector (quote ,size)))
             (*environment-allocation-mode*
              ;; Allocation in an explicit area.
              `(call sys.int::make-simple-vector (quote ,size) (quote ,*environment-allocation-mode*)))
             (t ;; General allocation.
              `(call sys.int::make-simple-vector (quote ,size))))
       lambda))

(defun get-env-vector (vector-id)
  (let ((chain (assoc vector-id *environment-chain*)))
    (when chain
      (return-from get-env-vector
        (second chain))))
  ;; Not in the chain, walk the rest of the environment.
  (do ((e *environment* (cdr e))
       (c *environment-chain* (cdr c)))
      ((null (cdr c))
       (let ((result (second (car c))))
         (dolist (env (cdr e)
                  (error "Can't find environment for ~S?" vector-id))
           (setf result (ast `(call sys.int::%object-ref-t ,result (quote 0))))
           (when (eql env vector-id)
             (return result)))))))

;;; Locate a variable in the environment.
(defun find-var (var env chain)
  (assert chain (var env chain) "No environment chain?")
  (assert env (var env chain) "No environment?")
  (cond ((member var (first env))
         (values (first chain) 0 (position var (first env))))
        ((rest chain)
         (find-var var (rest env) (rest chain)))
        (t ;; Walk the environment using the current chain as a root.
         (let ((depth 0))
           (dolist (e (rest env)
                    (error "~S not found in environment?" var))
             (incf depth)
             (when (member var e)
               (return (values (first chain) depth
                               (position var e)))))))))
