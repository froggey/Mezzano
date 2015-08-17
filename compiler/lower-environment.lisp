;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
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

(defun lower-environment (lambda)
  (let ((*environment-layout* (make-hash-table))
        (*environment-layout-dx* (make-hash-table))
        (*allow-dx-environment* 't))
    (compute-environment-layout lambda)
    (let ((*free-variables* (compute-free-variable-sets lambda))
          (*environment* '()))
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
          (*allow-dx-environment* t))
      (dolist (stmt (statements form))
        (cond ((go-tag-p stmt)
               (unless (finalize-environment-layout *active-environment-vector*)
                 (setf env-is-dx nil))
               (setf *active-environment-vector* stmt
                     *allow-dx-environment* t))
              (t (compute-environment-layout stmt))))
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
  (cond ((and (eql (name form) 'funcall)
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
  (when (and (not (symbolp variable))
             (not (localp variable)))
    (push variable (gethash *active-environment-vector* *environment-layout*))))

(defun finalize-environment-layout (env)
  ;; Inner environments must be DX, and every variable in this environment
  ;; must only be accessed by DX lambdas.
  (when (and *allow-dx-environment*
             (every (lambda (var)
                      (every (lambda (l)
                               (or (eql (lexical-variable-definition-point var) l)
                                   (eql (getf (lambda-information-plist l) 'extent) :dynamic)
                                   (getf (lambda-information-plist l) 'declared-dynamic-extent)))
                             (lexical-variable-used-in var)))
                    (gethash env *environment-layout*)))
    (setf (gethash env *environment-layout-dx*) t)
    t))

(defun compute-lambda-environment-layout (lambda)
  (let ((env-is-dx nil))
    (let ((*active-environment-vector* lambda)
          (*allow-dx-environment* t))
      (assert (null (lambda-information-environment-arg lambda)))
      ;; Special variables are not supported here, nor are keywords or non-trivial &OPTIONAL init-forms.
      (assert (every (lambda (arg)
                       (lexical-variable-p arg))
                     (lambda-information-required-args lambda)))
      (assert (every (lambda (arg)
                       (and (lexical-variable-p (first arg))
                            (quoted-form-p (second arg))
                            (or (null (third arg))
                                (lexical-variable-p (first arg)))))
                     (lambda-information-optional-args lambda)))
      (assert (or (null (lambda-information-rest-arg lambda))
                  (lexical-variable-p (lambda-information-rest-arg lambda))))
      (assert (not (lambda-information-enable-keys lambda)))
      (dolist (arg (lambda-information-required-args lambda))
        (maybe-add-environment-variable arg))
      (dolist (arg (lambda-information-optional-args lambda))
        (maybe-add-environment-variable (first arg))
        (when (third arg)
          (maybe-add-environment-variable (third arg))))
      (when (lambda-information-rest-arg lambda)
        (maybe-add-environment-variable (lambda-information-rest-arg lambda)))
      (compute-environment-layout (lambda-information-body lambda))
      (setf env-is-dx (finalize-environment-layout lambda)))
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
          (compute-free-variable-sets-form-list (remove-if #'go-tag-p (statements form)))))

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
                         (list (lambda-information-environment-arg form)))))
         (vars (set-difference (union (compute-free-variable-sets-form-list initforms)
                                      (compute-free-variable-sets-1 (lambda-information-body form)))
                               defs)))
    (setf (gethash form *free-variables*) vars)
    vars))

(defgeneric lower-env-form (form))

(defmethod lower-env-form ((form ast-block))
  (make-instance 'ast-block
                 :info (info form)
                 :body (make-instance 'ast-progn
                                      :forms (append (when (not (localp (info form)))
                                                       (let ((env-var (second (first *environment-chain*)))
                                                             (env-offset (1+ (position (info form) (gethash (first *environment*) *environment-layout*)))))
                                                         (setf (block-information-env-var (info form)) env-var
                                                               (block-information-env-offset (info form)) env-offset)
                                                         (list (make-instance 'ast-call
                                                                              :name '(setf sys.int::%object-ref-t)
                                                                              :arguments (list (info form)
                                                                                               env-var
                                                                                               (make-instance 'ast-quote
                                                                                                              :value env-offset))))))
                                                     (list (lower-env-form (body form)))))))

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
           collect (list variable (if (or (symbolp variable)
                                          (localp variable))
                                      (lower-env-form init-form)
                                      (make-instance 'ast-call
                                                     :name '(setf sys.int::%object-ref-t)
                                                     :arguments (list (lower-env-form init-form)
                                                                      (second (first *environment-chain*))
                                                                      (make-instance 'ast-quote
                                                                                     :value (1+ (position variable (gethash (first *environment*) *environment-layout*))))))))))
  (setf (body form) (lower-env-form (body form)))
  form)

(defmethod lower-env-form ((form ast-multiple-value-bind))
  (make-instance 'ast-multiple-value-bind
                 :bindings (bindings form)
                 :value-form (lower-env-form (value-form form))
                 :body (make-instance 'ast-progn
                                      :forms (append (mapcan (lambda (var)
                                                               (when (and (not (symbolp var))
                                                                          (not (localp var)))
                                                                 (list (make-instance 'ast-call
                                                                                      :name '(setf sys.int::%object-ref-t)
                                                                                      :arguments (list var
                                                                                                       (second (first *environment-chain*))
                                                                                                       (make-instance 'ast-quote
                                                                                                                      :value (1+ (position var (gethash (first *environment*) *environment-layout*)))))))))
                                                             (bindings form))
                                                     (list (lower-env-form (body form)))))))

(defmethod lower-env-form ((form ast-multiple-value-call))
  (make-instance 'ast-multiple-value-call
                 :function-form (lower-env-form (function-form form))
                 :value-form (lower-env-form (value-form form))))

(defmethod lower-env-form ((form ast-multiple-value-prog1))
  (make-instance 'ast-multiple-value-prog1
                 :value-form (lower-env-form (value-form form))
                 :body (lower-env-form (body form))))

(defmethod lower-env-form ((form ast-progn))
  (make-instance 'ast-progn
                 :forms (mapcar #'lower-env-form (forms form))))

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
                 (return (make-instance 'ast-call
                                        :name '(setf sys.int::%object-ref-t)
                                        :arguments (list (lower-env-form (value form))
                                                         (get-env-vector e)
                                                         (make-instance 'ast-quote :value (1+ offset)))))))))))

(defmethod lower-env-form ((form ast-tagbody))
  (let* ((possible-env-vector-heads (list* (info form)
                                           (remove-if-not #'go-tag-p (statements form))))
         (env-vector-heads (remove-if (lambda (x) (endp (gethash x *environment-layout*)))
                                      possible-env-vector-heads))
         (new-envs (loop for i in env-vector-heads
                      collect (list i
                                    (make-instance 'lexical-variable
                                                   :name (gensym "Environment")
                                                   :definition-point *current-lambda*)
                                    (gethash i *environment-layout*)))))
    (labels ((frob-outer ()
               (make-instance 'ast-tagbody
                              :info (info form)
                              :statements (append
                                           ;; Save the tagbody info.
                                           (when (not (localp (info form)))
                                             (let ((env-var (second (first *environment-chain*)))
                                                   (env-offset (1+ (position (info form) (gethash (first *environment*) *environment-layout*)))))
                                               (setf (tagbody-information-env-var (info form)) env-var
                                                     (tagbody-information-env-offset (info form)) env-offset)
                                               (list (make-instance 'ast-call
                                                                    :name '(setf sys.int::%object-ref-t)
                                                                    :arguments (list (info form)
                                                                                     env-var
                                                                                     (make-instance 'ast-quote :value env-offset))))))
                                           (let ((info (assoc (info form) new-envs)))
                                             (when info
                                               (if *environment*
                                                   (list (make-instance 'ast-setq
                                                                        :variable (second info)
                                                                        :value (generate-make-environment (info form) (1+ (length (third info)))))
                                                         (make-instance 'ast-call
                                                                        :name '(setf sys.int::%object-ref-t)
                                                                        :arguments (list (second (first *environment-chain*))
                                                                                         (second info)
                                                                                         (make-instance 'ast-quote :value '0))))
                                                   (list (make-instance 'ast-setq
                                                                        :variable (second info)
                                                                        :value (generate-make-environment (info form) (1+ (length (third info)))))))))
                                           (frob-inner (info form)))))
             (frob-inner (current-env)
               (loop for stmt in (statements form)
                  append (cond ((go-tag-p stmt)
                                (setf current-env stmt)
                                (let ((info (assoc current-env new-envs)))
                                  (append (list stmt)
                                          (when info
                                            (list (make-instance 'ast-setq
                                                                 :variable (second info)
                                                                 :value (generate-make-environment current-env (1+ (length (third info)))))))
                                          (when (and info *environment*)
                                            (list (make-instance 'ast-call
                                                                 :name '(setf sys.int::%object-ref-t)
                                                                 :arguments (list (second (first *environment-chain*))
                                                                                  (second info)
                                                                                  (make-instance 'ast-quote :value '0))))))))
                                (t (let ((info (assoc current-env new-envs)))
                                     (if info
                                         (let ((*environment-chain* (list* (list current-env (second info))
                                                                           *environment-chain*))
                                               (*environment* (list* current-env *environment*)))
                                           (list (lower-env-form stmt)))
                                         (list (lower-env-form stmt)))))))))
      (if (endp new-envs)
          (frob-outer)
          (make-instance 'ast-let
                         :bindings (loop
                                      for (stmt env layout) in new-envs
                                      collect (list env (make-instance 'ast-quote :value 'nil)))
                         :body (frob-outer))))))

(defmethod lower-env-form ((form ast-the))
  (setf (value form) (lower-env-form (value form)))
  form)

(defmethod lower-env-form ((form ast-unwind-protect))
  (make-instance 'ast-unwind-protect
                 :protected-form (lower-env-form (protected-form form))
                 :cleanup-function (lower-env-form (cleanup-function form))))

(defmethod lower-env-form ((form ast-call))
  (make-instance 'ast-call
                 :name (name form)
                 :arguments (mapcar #'lower-env-form (arguments form))))

(defmethod lower-env-form ((form ast-jump-table))
  (make-instance 'ast-jump-table
                 :value (lower-env-form (value form))
                 :targets (mapcar #'lower-env-form (targets form))))

(defmethod lower-env-form ((form lexical-variable))
  (if (localp form)
      form
      (dolist (e *environment*
               (error "Can't find variable ~S in environment." form))
        (let* ((layout (gethash e *environment-layout*))
               (offset (position form layout)))
          (when offset
            (return (make-instance 'ast-call
                                   :name 'sys.int::%object-ref-t
                                   :arguments (list (get-env-vector e)
                                                    (make-instance 'ast-quote :value (1+ offset))))))))))

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
                                :name (gensym "Environment")
                                :definition-point lambda)))
        (setf (lambda-information-environment-arg lambda) env)
        (push (list (first *environment*) env) *environment-chain*)))
    (cond ((not (endp local-env))
           ;; Environment is present, rewrite body with a new vector.
           (let ((new-env (make-instance 'lexical-variable
                                         :name (gensym "Environment")
                                         :definition-point lambda)))
             (push (list lambda new-env) *environment-chain*)
             (push lambda *environment*)
             (setf (lambda-information-environment-layout lambda) (compute-environment-layout-debug-info))
             (setf (lambda-information-body lambda)
                   (make-instance 'ast-let
                                  :bindings (list (list new-env (generate-make-environment lambda (1+ (length local-env)))))
                                  :body (make-instance 'ast-progn
                                                       :forms (append (when (rest *environment-chain*)
                                                                        (list (make-instance 'ast-call
                                                                                             :name '(setf sys.int::%object-ref-t)
                                                                                             :arguments (list (second (second *environment-chain*))
                                                                                                              new-env
                                                                                                              (make-instance 'ast-quote :value '0)))))
                                                                      (mapcar (lambda (arg)
                                                                                (make-instance 'ast-call
                                                                                               :name '(setf sys.int::%object-ref-t)
                                                                                               :arguments (list arg
                                                                                                                new-env
                                                                                                                (make-instance 'ast-quote
                                                                                                                               :value (1+ (position arg local-env))))))
                                                                              (remove-if #'localp (lambda-information-required-args lambda)))
                                                                      (mapcar (lambda (arg)
                                                                                (make-instance 'ast-call
                                                                                               :name '(setf sys.int::%object-ref-t)
                                                                                               :arguments (list (first arg)
                                                                                                                new-env
                                                                                                                (make-instance 'ast-quote
                                                                                                                               :value (1+ (position (first arg) local-env))))))
                                                                              (remove-if #'localp (lambda-information-optional-args lambda)
                                                                                         :key #'first))
                                                                      (mapcar (lambda (arg)
                                                                                (make-instance 'ast-call
                                                                                               :name '(setf sys.int::%object-ref-t)
                                                                                               :arguments (list (third arg)
                                                                                                                new-env
                                                                                                                (make-instance 'ast-quote
                                                                                                                               :value (1+ (position (third arg) local-env))))))
                                                                              (remove-if #'(lambda (x) (or (null x) (localp x)))
                                                                                         (lambda-information-optional-args lambda)
                                                                                         :key #'third))
                                                                      (when (and (lambda-information-rest-arg lambda)
                                                                                 (not (localp (lambda-information-rest-arg lambda))))
                                                                        (list (make-instance 'ast-call
                                                                                             :name '(setf sys.int::%object-ref-t)
                                                                                             :arguments (list (lambda-information-rest-arg lambda)
                                                                                                              new-env
                                                                                                              (make-instance 'ast-quote
                                                                                                                             :value (1+ (position (lambda-information-rest-arg lambda) local-env)))))))
                                                                      (list (lower-env-form (lambda-information-body lambda)))))))))
          (t (setf (lambda-information-environment-layout lambda) (compute-environment-layout-debug-info))
             (setf (lambda-information-body lambda) (lower-env-form (lambda-information-body lambda)))))
    lambda))

(defmethod lower-env-form ((form lambda-information))
  (cond ((or (not *environment-chain*)
             (endp (gethash form *free-variables*)))
         (let ((*environment* '()))
           (lower-env-lambda form)))
        ((getf (lambda-information-plist form) 'declared-dynamic-extent)
         (make-instance 'ast-call
                        :name 'sys.c::make-dx-closure
                        :arguments (list (lower-env-lambda form)
                                         (second (first *environment-chain*)))))
        (*environment-allocation-mode*
         (make-instance 'ast-call
                        :name 'sys.int::make-closure
                        :arguments (list (lower-env-lambda form)
                                         (second (first *environment-chain*))
                                         (make-instance 'ast-quote :value *environment-allocation-mode*))))
        (t (make-instance 'ast-call
                          :name 'sys.int::make-closure
                          :arguments (list (lower-env-lambda form)
                                           (second (first *environment-chain*)))))))

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
  (cond ((gethash lambda *environment-layout-dx*)
         ;; DX allocation.
         (make-instance 'ast-call
                        :name 'sys.c::make-dx-simple-vector
                        :arguments (list (make-instance 'ast-quote :value size))))
        (*environment-allocation-mode*
         ;; Allocation in an explicit area.
         (make-instance 'ast-call
                        :name 'sys.int::make-simple-vector
                        :arguments (list (make-instance 'ast-quote :value size)
                                         (make-instance 'ast-quote :value *environment-allocation-mode*))))
        ;; General allocation.
        (t (make-instance 'ast-call
                          :name 'sys.int::make-simple-vector
                          :arguments (list (make-instance 'ast-quote :value size))))))

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
           (setf result (make-instance 'ast-call
                                       :name 'sys.int::%object-ref-t
                                       :arguments (list result (make-instance 'ast-quote :value '0))))
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
