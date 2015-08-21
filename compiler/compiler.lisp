;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

(defvar *should-inline-functions* t)

(defvar *jump-table-size-min* 4)
(defvar *jump-table-size-max* 64)

(defvar *load-time-value-hook*)

(defun parse-declares (forms)
  "Extract any leading declare forms.
Returns 2 values:
The body, with the declare forms removed.
A list of any declaration-specifiers."
  (do ((declares '())
       (itr forms (cdr itr)))
      ((or (null itr)
	   ;; Stop when (car itr) is not a declare form.
	   (not (and (consp (car itr))
		     (eq 'declare (caar itr)))))
       (values itr (nreverse declares)))
    ;; Dump the bodies of each declare form into a single list.
    (dolist (decl (cdar itr))
      (push decl declares))))

(declaim (special *environment*))

(defun compile-lambda (lambda &optional env)
  (codegen-lambda (compile-lambda-1 lambda env)))

;; Parse lambda and optimize, but do not do codegen.
(defun compile-lambda-1 (lambda &optional env)
  (let ((*environment* (cdr env)))
    (detect-uses
     (simplify
      (detect-uses
       ;; Make the dynamic environment explicit.
       (lower-special-bindings
        ;; Run a final simplify pass to kill off any useless bindings.
        (detect-uses
         (simplify
          (detect-uses
           ;; Lower closed-over variables.
           (lower-environment
            (detect-uses
             (lower-arguments
              (detect-uses
               (run-optimizers
                (pass1-lambda lambda (car env))))))))))))))))

(defun eval-load-time-value (form read-only-p)
  (declare (ignore read-only-p))
  `(quote ,(eval form)))

(defun compile (name &optional definition)
  (unless definition
    (setf definition (or (when (symbolp name) (macro-function name))
                         (fdefinition name))))
  (when (functionp definition)
    (when (compiled-function-p definition)
      (return-from compile
        (values definition nil nil)))
    (multiple-value-bind (lambda-expression env)
        (function-lambda-expression definition)
      (when (null lambda-expression)
        (error "No source information available for ~S." definition))
      (when env
        (error "TODO: cannot compile functions defined outside the null lexical environment."))
      (setf definition lambda-expression)))
  (multiple-value-bind (fn warnings-p errors-p)
      (let ((*load-time-value-hook* 'eval-load-time-value))
        (compile-lambda definition))
    (cond (name
           (if (and (symbolp name) (macro-function name))
               (setf (macro-function name) fn)
               (setf (fdefinition name) fn))
           (values name warnings-p errors-p))
          (t (values fn warnings-p errors-p)))))

(defvar *current-lambda* nil
  "A lambda-information struct for the lambda currently being translated.")
(defvar *change-count* nil
  "Number of changes made by the optimizer passes.")

(defun change-made ()
  (when (and (boundp '*change-count*)
             *change-count*)
    (incf *change-count*)))

(defun run-optimizers (form)
  (dotimes (i 20 (progn (warn 'sys.int::simple-style-warning
			      :format-control "Possible optimizer infinite loop."
			      :format-arguments '())
			form))
    (let ((*change-count* 0))
      ;; Must be run before lift.
      (setf form (inline-functions (detect-uses form)))
      (setf form (lambda-lift (detect-uses form)))
      ;; Key arg conversion must be performed after lambda-lifting, so as not to
      ;; complicate the lift code.
      (setf form (lower-keyword-arguments form))
      (setf form (constprop (detect-uses form)))
      (setf form (simplify (detect-uses form)))
      (setf form (kill-temporaries (detect-uses form)))
      (setf form (value-aware-lowering (detect-uses form)))
      (setf form (simplify-control-flow (detect-uses form)))
      (detect-uses form)
      (when (eql *change-count* 0)
	(return form)))))

(defclass lambda-information ()
  ((%name :initarg :name :accessor lambda-information-name)
   (%docstring :initarg :docstring :accessor lambda-information-docstring)
   (%lambda-list :initarg :lambda-list :accessor lambda-information-lambda-list)
   (%body :initarg :body :accessor lambda-information-body)
   (%required-args :initarg :required-args :accessor lambda-information-required-args)
   (%optional-args :initarg :optional-args :accessor lambda-information-optional-args)
   (%rest-arg :initarg :rest-arg :accessor lambda-information-rest-arg)
   (%enable-keys :initarg :enable-keys :accessor lambda-information-enable-keys)
   (%key-args :initarg :key-args :accessor lambda-information-key-args)
   (%allow-other-keys :initarg :allow-other-keys :accessor lambda-information-allow-other-keys)
   (%environment-arg :initarg :environment-arg :accessor lambda-information-environment-arg)
   (%environment-layout :initarg :environment-layout :accessor lambda-information-environment-layout)
   (%plist :initarg :plist :accessor lambda-information-plist))
  (:default-initargs :name nil
                     :docstring nil
                     :lambda-list '()
                     :body nil
                     :required-args '()
                     :optional-args '()
                     :rest-arg nil
                     :enable-keys nil
                     :key-args '()
                     :allow-other-keys '()
                     :environment-arg nil
                     :environment-layout nil
                     :plist '()))

(defun lambda-information-p (object)
  (typep object 'lambda-information))

;;; A lexical-variable represents a "renamed" variable, and stores definition information.
(defclass lexical-variable ()
  ((%name :initarg :name :accessor lexical-variable-name)
   (%definition-point :initarg :definition-point :accessor lexical-variable-definition-point)
   (%ignore :initarg :ignore :accessor lexical-variable-ignore)
   (%dynamic-extent :initarg :dynamic-extent :accessor lexical-variable-dynamic-extent)
   (%use-count :initarg :use-count :accessor lexical-variable-use-count)
   (%write-count :initarg :write-count :accessor lexical-variable-write-count)
   (%used-in :initarg :used-in :accessor lexical-variable-used-in)
   (%plist :initarg :plist :accessor lexical-variable-plist))
  (:default-initargs :name nil
                     :definition-point nil
                     :ignore nil
                     :dynamic-extent nil
                     :use-count 0
                     :write-count 0
                     :used-in '()
                     :plist '()))

(defun lexical-variable-p (object)
  (typep object 'lexical-variable))

(defmethod print-object ((object lexical-variable) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (lexical-variable-name object))))

;;; A special variable, only used in bindings.
(defclass special-variable ()
  ((%name :initarg :name :accessor name)))

(defmethod print-object ((object special-variable) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (name object))))

(defclass block-information (lexical-variable)
  ((%return-mode :initarg :return-mode :accessor block-information-return-mode)
   (%count :initarg :count :accessor block-information-count)
   (%env-var :initarg :env-var :accessor block-information-env-var)
   (%env-offset :initarg :env-offset :accessor block-information-env-offset))
  (:default-initargs :return-mode nil
                     :count nil
                     :env-var nil
                     :env-offset nil))

(defun block-information-p (object)
  (typep object 'block-information))

(defclass tagbody-information (lexical-variable)
  ((%go-tags :initarg :go-tags :accessor tagbody-information-go-tags)
   (%env-var :initarg :env-var :accessor tagbody-information-env-var)
   (%env-offset :initarg :env-offset :accessor tagbody-information-env-offset))
  (:default-initargs :go-tags '()
                     :env-var nil
                     :env-offset nil))

(defun tagbody-information-p (object)
  (typep object 'tagbody-information))

(defclass go-tag ()
  ((%name :initarg :name :accessor go-tag-name)
   (%tagbody :initarg :tagbody :accessor go-tag-tagbody)
   (%use-count :initarg :use-count :accessor go-tag-use-count)
   (%used-in :initarg :used-in :accessor go-tag-used-in))
  (:default-initargs :name nil
                     :tagbody nil
                     :use-count 0
                     :used-in '()))

(defun go-tag-p (object)
  (typep object 'go-tag))

(defclass ast-block ()
  ((%info :initarg :info :accessor info)
   (%body :initarg :body :accessor body)))

(defclass ast-function ()
  ((%name :initarg :name :accessor name)))

(defmethod print-object ((object ast-function) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (name object))))

(defclass ast-go ()
  ((%target :initarg :target :accessor target)
   (%info :initarg :info :accessor info)))

(defclass ast-if ()
  ((%test :initarg :test :accessor test)
   (%then :initarg :then :accessor if-then)
   (%else :initarg :else :accessor if-else)))

(defclass ast-let ()
  ;; BINDINGS is a list of (variable init-form), where
  ;; variable is either a LEXICAL-VARIABLE or a SPECIAL-VARIABLE.
  ;; Init-forms are evaluated in list order.
  ;; Lexical bindings occur immediately
  ;; Special bindings occur once all init-forms have been evaulated.
  ((%bindings :initarg :bindings :accessor bindings)
   (%body :initarg :body :accessor body)))

(defclass ast-multiple-value-bind ()
  ;; BINDING is a list of variables, which can be either LEXICAL-VARIABLEs
  ;; or SPECIAL-VARIABLES.
  ;; Bindings occur after VALUE-FORM has been evaulated.
  ((%bindings :initarg :bindings :accessor bindings)
   (%value-form :initarg :value-form :accessor value-form)
   (%body :initarg :body :accessor body)))

(defclass ast-multiple-value-call ()
  ((%function-form :initarg :function-form :accessor function-form)
   (%value-form :initarg :value-form :accessor value-form)))

(defclass ast-multiple-value-prog1 ()
  ((%value-form :initarg :value-form :accessor value-form)
   (%body :initarg :body :accessor body)))

(defclass ast-progn ()
  ((%forms :initarg :forms :accessor forms)))

(defclass ast-quote ()
  ((%value :initarg :value :accessor value)))

(defmethod print-object ((object ast-quote) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (value object))))

(defclass ast-return-from ()
  ((%target :initarg :target :accessor target)
   (%value :initarg :value :accessor value)
   (%info :initarg :info :accessor info)))

(defclass ast-setq ()
  ((%variable :initarg :variable :accessor setq-variable)
   (%value :initarg :value :accessor value)))

(defclass ast-tagbody ()
  ((%info :initarg :info :accessor info)
   ;; A list of (go-tag form).
   ;; Form that do not end in a control transfer will cause the
   ;; tagbody to return.
   ;; Only the first statement is directly reachable, other
   ;; statements can only be reached via GO forms.
   (%statements :initarg :statements :accessor statements)))

(defclass ast-the ()
  ((%the-type :initarg :type :accessor the-type)
   (%value :initarg :value :accessor value)))

(defclass ast-unwind-protect ()
  ((%protected-form :initarg :protected-form :accessor protected-form)
   (%cleanup-function :initarg :cleanup-function :accessor cleanup-function)))

(defclass ast-call ()
  ((%name :initarg :name :accessor name)
   (%arguments :initarg :arguments :accessor arguments)))

(defclass ast-jump-table ()
  ((%value :initarg :value :accessor value)
   (%targets :initarg :targets :accessor targets)))

(defmethod print-object ((object go-tag) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (go-tag-name object))))

(defvar *replacements*)

(defun copy-form (form)
  "Completely copy a form, incrementing use-counts."
  (let ((*replacements* '()))
    (copy-form-1 form)))

(defgeneric copy-form-1 (form))

(defmethod copy-form-1 :around (form)
  (let ((*replacements* *replacements*))
    (call-next-method)))

(defun copy-form-fix (thing)
  (let ((r (assoc thing *replacements*)))
    (if r (cdr r) thing)))

(defun copy-variable (var)
  (if (lexical-variable-p var)
      (let ((new (make-instance (if (block-information-p var)
                                    'block-information
                                    'lexical-variable)
                                :name (lexical-variable-name var)
                                :ignore (lexical-variable-ignore var)
                                :dynamic-extent (lexical-variable-dynamic-extent var))))
        (setf (lexical-variable-definition-point new) (copy-form-fix (lexical-variable-definition-point var))
              (lexical-variable-use-count new) 0
              (lexical-variable-write-count new) 0
              (lexical-variable-used-in new) '())
        (push (cons var new) *replacements*)
        new)
      var))

(defmethod copy-form-1 ((form ast-block))
  (make-instance 'ast-block
                 :info (copy-variable (info form))
                 :body (copy-form-1 (body form))))

(defmethod copy-form-1 ((form ast-go))
  (let ((tag (copy-form-fix (target form))))
    (incf (go-tag-use-count tag))
    (pushnew *current-lambda* (go-tag-used-in tag))
    (incf (lexical-variable-use-count (go-tag-tagbody tag)))
    (pushnew *current-lambda* (lexical-variable-used-in (go-tag-tagbody tag)))
    (make-instance 'ast-go
                   :target tag
                   :info (copy-form-1 (info form)))))

(defmethod copy-form-1 ((form ast-function))
  form)

(defmethod copy-form-1 ((form ast-if))
  (make-instance 'ast-if
                 :test (copy-form-1 (test form))
                 :then (copy-form-1 (if-then form))
                 :else (copy-form-1 (if-else form))))

(defmethod copy-form-1 ((form ast-let))
  ;; So that labels works correctly, this must create the variables and then
  ;; copy init-forms.
  (loop
     for (variable init-form) in (bindings form)
     do (copy-variable variable))
  (make-instance 'ast-let
                 :bindings (loop
                              for (variable init-form) in (bindings form)
                              collect (list (copy-form-fix variable)
                                            (copy-form-1 init-form)))
                 :body (copy-form-1 (body form))))

(defmethod copy-form-1 ((form ast-multiple-value-bind))
  (make-instance 'ast-multiple-value-bind
                 :bindings (mapcar #'copy-variable (bindings form))
                 :value-form (copy-form-1 (value-form form))
                 :body (copy-form-1 (body form))))

(defmethod copy-form-1 ((form ast-multiple-value-call))
  (make-instance 'ast-multiple-value-call
                 :function-form (copy-form-1 (function-form form))
                 :value-form (copy-form-1 (value-form form))))

(defmethod copy-form-1 ((form ast-multiple-value-prog1))
  (make-instance 'ast-multiple-value-prog1
                 :value-form (copy-form-1 (value-form form))
                 :body (copy-form-1 (body form))))

(defmethod copy-form-1 ((form ast-progn))
  (make-instance 'ast-progn
                 :forms (loop
                           for form in (forms form)
                           collect (copy-form-1 form))))

(defmethod copy-form-1 ((form ast-quote))
  form)

(defmethod copy-form-1 ((form ast-return-from))
  (let ((var (copy-form-fix (target form))))
    (incf (lexical-variable-use-count var))
    (pushnew *current-lambda* (lexical-variable-used-in var))
    (make-instance 'ast-return-from
                   :target var
                   :value (copy-form-1 (value form))
                   :info (copy-form-1 (info form)))))

(defmethod copy-form-1 ((form ast-setq))
  (let ((var (copy-form-fix (setq-variable form))))
    (incf (lexical-variable-use-count var))
    (incf (lexical-variable-write-count var))
    (pushnew *current-lambda* (lexical-variable-used-in var))
    (make-instance 'ast-setq
                   :variable var
                   :value (copy-form-1 (value form)))))

(defmethod copy-form-1 ((form ast-tagbody))
  (let ((info (make-instance 'tagbody-information
                             :definition-point (copy-form-fix (lexical-variable-definition-point (info form))))))
    (push (cons (info form) info) *replacements*)
    (dolist (tag (tagbody-information-go-tags (info form)))
      (let ((new-tag (make-instance 'go-tag
                                    :name (go-tag-name tag)
                                    :tagbody info)))
        (push new-tag (tagbody-information-go-tags info))
        (push (cons tag new-tag) *replacements*)))
    (make-instance 'ast-tagbody
                   :info info
                   :statements (loop
                                  for (go-tag statement) in (statements form)
                                  collect (list (copy-form-fix go-tag)
                                                (copy-form-1 statement))))))

(defmethod copy-form-1 ((form ast-the))
  (make-instance 'ast-the
                 :type (the-type form)
                 :value (copy-form-1 (value form))))

(defmethod copy-form-1 ((form ast-unwind-protect))
  (make-instance 'ast-unwind-protect
                 :protected-form (copy-form-1 (protected-form form))
                 :cleanup-function (copy-form-1 (cleanup-function form))))

(defmethod copy-form-1 ((form ast-call))
  (make-instance 'ast-call
                 :name (name form)
                 :arguments (loop
                               for arg in (arguments form)
                               collect (copy-form-1 arg))))

(defmethod copy-form-1 ((form ast-jump-table))
  (make-instance 'ast-jump-table
                 :value (copy-form-1 (value form))
                 :targets (loop
                             for targ in (targets form)
                             collect (copy-form-1 targ))))

(defmethod copy-form-1 ((form lexical-variable))
  (let ((var (copy-form-fix form)))
    (incf (lexical-variable-use-count var))
    (pushnew *current-lambda* (lexical-variable-used-in var))
    var))

(defmethod copy-form-1 ((form lambda-information))
  (let* ((info (make-instance 'lambda-information
                              :name (lambda-information-name form)
                              :docstring (lambda-information-docstring form)
                              :lambda-list (lambda-information-lambda-list form)
                              :enable-keys (lambda-information-enable-keys form)
                              :allow-other-keys (lambda-information-allow-other-keys form)
                              :plist (copy-list (lambda-information-plist form))))
         (*current-lambda* info))
    (incf (getf (lambda-information-plist info) 'copy-count 0))
    (push (cons form info) *replacements*)
    (setf (lambda-information-required-args info)
          (mapcar #'copy-variable (lambda-information-required-args form)))
    (setf (lambda-information-optional-args info)
          (mapcar (lambda (x)
                    (list (copy-variable (first x))
                          (copy-form-1 (second x))
                          (when (third x)
                            (copy-variable (third x)))))
                  (lambda-information-optional-args form)))
    (when (lambda-information-rest-arg form)
      (setf (lambda-information-rest-arg info)
            (copy-variable (lambda-information-rest-arg form))))
    (setf (lambda-information-key-args info)
          (mapcar (lambda (x)
                    (list (list (first (first x))
                                (copy-variable (second (first x))))
                          (copy-form-1 (second x))
                          (when (third x)
                            (copy-variable (third x)))))
                  (lambda-information-key-args form)))
    (when (lambda-information-environment-arg form)
      (setf (lambda-information-environment-arg info)
            (copy-variable (lambda-information-environment-arg form))))
    (setf (lambda-information-body info)
          (copy-form-1 (lambda-information-body form)))
    info))

(defun reset-var (var)
  (when (lexical-variable-p var)
    (setf (lexical-variable-used-in var) '()
          (lexical-variable-use-count var) 0
          (lexical-variable-write-count var) 0)))

(defun detect-uses (form)
  "Walk form, refreshing variable use counts & locations."
  (detect-uses-1 form)
  form)

(defgeneric detect-uses-1 (form))

(defmethod detect-uses-1 ((form ast-block))
  (reset-var (info form))
  (detect-uses-1 (body form)))

(defmethod detect-uses-1 ((form ast-function)))

(defmethod detect-uses-1 ((form ast-go))
  (assert (or (not (tagbody-information-p (info form)))
              (eql (go-tag-tagbody (target form)) (info form))))
  (detect-uses-1 (info form))
  (incf (go-tag-use-count (target form)))
  (pushnew *current-lambda* (go-tag-used-in (target form)))
  (incf (lexical-variable-use-count (go-tag-tagbody (target form)))))

(defmethod detect-uses-1 ((form ast-if))
  (detect-uses-1 (test form))
  (detect-uses-1 (if-then form))
  (detect-uses-1 (if-else form)))

(defmethod detect-uses-1 ((form ast-let))
  (loop
     for (variable init-form) in (bindings form)
     do (reset-var variable))
  (loop
     for (variable init-form) in (bindings form)
     do (detect-uses-1 init-form))
  (detect-uses-1 (body form)))

(defmethod detect-uses-1 ((form ast-multiple-value-bind))
  (mapc #'reset-var (bindings form))
  (detect-uses-1 (value-form form))
  (detect-uses-1 (body form)))

(defmethod detect-uses-1 ((form ast-multiple-value-call))
  (detect-uses-1 (function-form form))
  (detect-uses-1 (value-form form)))

(defmethod detect-uses-1 ((form ast-multiple-value-prog1))
  (detect-uses-1 (value-form form))
  (detect-uses-1 (body form)))

(defmethod detect-uses-1 ((form ast-progn))
  (mapc #'detect-uses-1 (forms form)))

(defmethod detect-uses-1 ((form ast-quote)))

(defmethod detect-uses-1 ((form ast-return-from))
  (incf (lexical-variable-use-count (target form)))
  (detect-uses-1 (value form))
  (detect-uses-1 (info form)))

(defmethod detect-uses-1 ((form ast-setq))
  (let ((var (setq-variable form)))
    (pushnew *current-lambda* (lexical-variable-used-in var))
    (incf (lexical-variable-use-count var))
    (incf (lexical-variable-write-count var))
    (detect-uses-1 (value form))))

(defmethod detect-uses-1 ((form ast-tagbody))
  (reset-var (info form))
  (dolist (tag (tagbody-information-go-tags (info form)))
    (setf (go-tag-use-count tag) 0
          (go-tag-used-in tag) '()))
  (let ((first-go-tag (first (first (statements form)))))
    ;; First go tag/statement is always reachable. It's the entry point.
    (incf (go-tag-use-count first-go-tag))
    (push *current-lambda* (go-tag-used-in first-go-tag)))
  (loop
     for (go-tag statement) in (statements form)
     do (detect-uses-1 statement)))

(defmethod detect-uses-1 ((form ast-the))
  (detect-uses-1 (value form)))

(defmethod detect-uses-1 ((form ast-unwind-protect))
  (detect-uses-1 (protected-form form))
  (detect-uses-1 (cleanup-function form)))

(defmethod detect-uses-1 ((form ast-call))
  (mapc #'detect-uses-1 (arguments form)))

(defmethod detect-uses-1 ((form ast-jump-table))
  (detect-uses-1 (value form))
  (mapc #'detect-uses-1 (targets form)))

(defmethod detect-uses-1 ((form lexical-variable))
  (pushnew *current-lambda* (lexical-variable-used-in form))
  (incf (lexical-variable-use-count form)))

(defmethod detect-uses-1 ((form lambda-information))
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-required-args form))
      (reset-var arg))
    (dolist (arg (lambda-information-optional-args form))
      (reset-var (first arg))
      (detect-uses-1 (second arg))
      (when (third arg)
        (reset-var (third arg))))
    (when (lambda-information-rest-arg form)
      (reset-var (lambda-information-rest-arg form)))
    (dolist (arg (lambda-information-key-args form))
      (reset-var (second (first arg)))
      (detect-uses-1 (second arg))
      (when (third arg)
        (reset-var (third arg))))
    (when (lambda-information-environment-arg form)
      (reset-var (lambda-information-environment-arg form)))
    (detect-uses-1 (lambda-information-body form))))

(defun variable-name (var)
  (etypecase var
    (special-variable
     (name var))
    (lexical-variable
     (lexical-variable-name var))))

(defun lower-key-arguments* (body rest keys allow-other-keys)
  (let* ((values (mapcar (lambda (x)
                           (make-instance 'lexical-variable
                                          :name (gensym (string (variable-name (cadar x))))
                                          :definition-point *current-lambda*))
                         keys))
         (suppliedp (mapcar (lambda (x)
                              (make-instance 'lexical-variable
                                             :name (if (third x)
                                                       (gensym (string (variable-name (third x))))
                                                       (gensym))
                                             :definition-point *current-lambda*))
                            keys))
         (itr (make-instance 'lexical-variable
                             :name (gensym)
                             :definition-point *current-lambda*))
         (current-keyword (make-instance 'lexical-variable
                                         :name (gensym)
                                         :definition-point *current-lambda*)))
    (labels ((create-key-test-list (key-args values suppliedp)
               (cond (key-args
                      `(if (call eql ,current-keyword (quote ,(caar (first key-args))))
                           (if ,(first suppliedp)
                               (quote nil)
                               (progn
                                 (setq ,(first suppliedp) 't)
                                 (setq ,(first values) (call cadr ,itr))))
                           ,(create-key-test-list (rest key-args) (rest values) (rest suppliedp))))
                     (allow-other-keys
                      '(quote nil))
                     (t
                      `(call error
                             'sys.int::simple-program-error
                             ':format-control '"Unknown &KEY argument ~S. Expected one of ~S."
                             ':format-arguments (call list ,current-keyword (quote ,(mapcar 'caar keys)))))))
             (create-key-let-body (key-args values suppliedp)
               (cond (key-args
                      `(let ((,(second (first (first key-args)))
                              (if ,(first suppliedp)
                                  ,(first values)
                                  ,(second (first key-args)))))
                         ,(if (third (first key-args))
                              `(let ((,(third (first key-args)) ,(first suppliedp)))
                                 ,(create-key-let-body (rest key-args) (rest values) (rest suppliedp)))
                              (create-key-let-body (rest key-args) (rest values) (rest suppliedp)))))
                     (t body))))
      (ast `(let (,@(mapcar (lambda (x) (list x '(quote nil))) values)
                  ,@(mapcar (lambda (x) (list x '(quote nil))) suppliedp)
                    (,itr ,(etypecase rest
                             (special-variable
                              `(call symbol-value (quote ,rest)))
                             (lexical-variable
                              rest))))
              (progn
                (tagbody tb
                   (entry
                    (go test-tag tb))
                   (head-tag
                    (progn
                      (if (call null (call cdr ,itr))
                          (call error
                                'sys.int::simple-program-error
                                ':format-control '"Odd number of &KEY arguments.")
                          (quote nil))
                      (let ((,current-keyword (call car ,itr)))
                        ,(create-key-test-list keys values suppliedp))
                      (setq ,itr (call cddr ,itr))
                      (go test-tag tb)))
                   (test-tag
                    (if ,itr
                        (go head-tag tb)
                        (quote nil))))
                ,(create-key-let-body keys values suppliedp)))))))

(defun lower-keyword-arguments (form)
  (lower-keyword-arguments-1 form)
  form)

(defgeneric lower-keyword-arguments-1 (form))

(defmethod lower-keyword-arguments-1 ((form ast-block))
  (lower-keyword-arguments-1 (body form)))

(defmethod lower-keyword-arguments-1 ((form ast-function)))

(defmethod lower-keyword-arguments-1 ((form ast-go))
  (lower-keyword-arguments-1 (info form)))

(defmethod lower-keyword-arguments-1 ((form ast-if))
  (lower-keyword-arguments-1 (test form))
  (lower-keyword-arguments-1 (if-then form))
  (lower-keyword-arguments-1 (if-else form)))

(defmethod lower-keyword-arguments-1 ((form ast-let))
  (loop
     for (variable init-form) in (bindings form)
     do (lower-keyword-arguments-1 init-form))
  (lower-keyword-arguments-1 (body form)))

(defmethod lower-keyword-arguments-1 ((form ast-multiple-value-bind))
  (lower-keyword-arguments-1 (value-form form))
  (lower-keyword-arguments-1 (body form)))

(defmethod lower-keyword-arguments-1 ((form ast-multiple-value-call))
  (lower-keyword-arguments-1 (function-form form))
  (lower-keyword-arguments-1 (value-form form)))

(defmethod lower-keyword-arguments-1 ((form ast-multiple-value-prog1))
  (lower-keyword-arguments-1 (value-form form))
  (lower-keyword-arguments-1 (body form)))

(defmethod lower-keyword-arguments-1 ((form ast-progn))
  (mapc #'lower-keyword-arguments-1 (forms form)))

(defmethod lower-keyword-arguments-1 ((form ast-quote)))

(defmethod lower-keyword-arguments-1 ((form ast-return-from))
  (lower-keyword-arguments-1 (value form))
  (lower-keyword-arguments-1 (info form)))

(defmethod lower-keyword-arguments-1 ((form ast-setq))
  (lower-keyword-arguments-1 (value form)))

(defmethod lower-keyword-arguments-1 ((form ast-tagbody))
  (loop
     for (go-tag statement) in (statements form)
     do (lower-keyword-arguments-1 statement)))

(defmethod lower-keyword-arguments-1 ((form ast-the))
  (lower-keyword-arguments-1 (value form)))

(defmethod lower-keyword-arguments-1 ((form ast-unwind-protect))
  (lower-keyword-arguments-1 (protected-form form))
  (lower-keyword-arguments-1 (cleanup-function form)))

(defmethod lower-keyword-arguments-1 ((form ast-call))
  (mapc #'lower-keyword-arguments-1 (arguments form)))

(defmethod lower-keyword-arguments-1 ((form ast-jump-table))
  (lower-keyword-arguments-1 (value form))
  (mapc #'lower-keyword-arguments-1 (targets form)))

(defmethod lower-keyword-arguments-1 ((form lexical-variable)))

(defmethod lower-keyword-arguments-1 ((form lambda-information))
  (let ((*current-lambda* form))
    (when (lambda-information-enable-keys form)
      (unless (lambda-information-rest-arg form)
        ;; Add in a &REST arg and make it dynamic-extent.
        (setf (lambda-information-rest-arg form)
              (make-instance 'lexical-variable
                             :name (gensym "REST")
                             :definition-point *current-lambda*
                             :ignore :maybe
                             :dynamic-extent t)))
      (setf (lambda-information-body form)
            (lower-key-arguments* (lambda-information-body form)
                                  (lambda-information-rest-arg form)
                                  (lambda-information-key-args form)
                                  (lambda-information-allow-other-keys form)))
      ;; Remove the old keyword arguments.
      (setf (lambda-information-enable-keys form) nil
            (lambda-information-key-args form) '()
            (lambda-information-allow-other-keys form) nil)
      (incf *change-count*))
    (dolist (arg (lambda-information-optional-args form))
      (lower-keyword-arguments-1 (second arg)))
    (lower-keyword-arguments-1 (lambda-information-body form))))

(defun lower-arguments (form)
  "Simplify lambda lists so that no lambda argument is special and
so that no &OPTIONAL argument has a non-constant init-form.
Must be run after keywords have been lowered."
  (lower-arguments-1 form)
  form)

(defgeneric lower-arguments-1 (form))

(defmethod lower-arguments-1 ((form ast-block))
  (lower-arguments-1 (body form)))

(defmethod lower-arguments-1 ((form ast-function)))

(defmethod lower-arguments-1 ((form ast-go))
  (lower-arguments-1 (info form)))

(defmethod lower-arguments-1 ((form ast-if))
  (lower-arguments-1 (test form))
  (lower-arguments-1 (if-then form))
  (lower-arguments-1 (if-else form)))

(defmethod lower-arguments-1 ((form ast-let))
  (loop
     for (variable init-form) in (bindings form)
     do (lower-arguments-1 init-form))
  (lower-arguments-1 (body form)))

(defmethod lower-arguments-1 ((form ast-multiple-value-bind))
  (lower-arguments-1 (value-form form))
  (lower-arguments-1 (body form)))

(defmethod lower-arguments-1 ((form ast-multiple-value-call))
  (lower-arguments-1 (function-form form))
  (lower-arguments-1 (value-form form)))

(defmethod lower-arguments-1 ((form ast-multiple-value-prog1))
  (lower-arguments-1 (value-form form))
  (lower-arguments-1 (body form)))

(defmethod lower-arguments-1 ((form ast-progn))
  (mapc #'lower-arguments-1 (forms form)))

(defmethod lower-arguments-1 ((form ast-quote)))

(defmethod lower-arguments-1 ((form ast-return-from))
  (lower-arguments-1 (value form))
  (lower-arguments-1 (info form)))

(defmethod lower-arguments-1 ((form ast-setq))
  (lower-arguments-1 (value form)))

(defmethod lower-arguments-1 ((form ast-tagbody))
  (loop
     for (go-tag statement) in (statements form)
     do (lower-arguments-1 statement)))

(defmethod lower-arguments-1 ((form ast-the))
  (lower-arguments-1 (value form)))

(defmethod lower-arguments-1 ((form ast-unwind-protect))
  (lower-arguments-1 (protected-form form))
  (lower-arguments-1 (cleanup-function form)))

(defmethod lower-arguments-1 ((form ast-call))
  (mapc #'lower-arguments-1 (arguments form)))

(defmethod lower-arguments-1 ((form ast-jump-table))
  (lower-arguments-1 (value form))
  (mapc #'lower-arguments-1 (targets form)))

(defmethod lower-arguments-1 ((form lexical-variable)))

(defmethod lower-arguments-1 ((form lambda-information))
  (flet ((new-var (name)
           (make-instance 'lexical-variable
                          :name (gensym name)
                          :definition-point *current-lambda*)))
    (let* ((*current-lambda* form)
           (extra-bindings '()))
      (when (lambda-information-enable-keys form)
        (error "Keyword arguments not lowered!"))
      ;; Eliminate special required arguments.
      (setf (lambda-information-required-args form)
            (loop
               for arg in (lambda-information-required-args form)
               collect (etypecase arg
                         (special-variable
                          (let ((temp (new-var (string (name arg)))))
                            (push (list arg temp) extra-bindings)
                            temp))
                         (lexical-variable
                          arg))))
      ;; Eliminate special optional arguments & non-constant init-forms.
      (setf (lambda-information-optional-args form)
            (loop
               for (arg init-form suppliedp) in (lambda-information-optional-args form)
               collect (let* ((new-suppliedp (etypecase suppliedp
                                               (null
                                                (new-var (format nil "~S-suppliedp" arg)))
                                               (special-variable
                                                (new-var (string (name suppliedp))))
                                               (lexical-variable
                                                suppliedp)))
                              (trivial-init-form (typep init-form 'ast-quote))
                              (new-arg (etypecase arg
                                         (special-variable
                                          (new-var (string (name arg))))
                                         (lexical-variable
                                          (if (not trivial-init-form)
                                              (new-var (string (lexical-variable-name arg)))
                                              arg))))
                              (new-init-form (if trivial-init-form
                                                 init-form
                                                 (ast '(quote nil)))))
                         (when (or (not trivial-init-form)
                                   (typep arg 'special-variable))
                           (push (list arg (ast `(if ,new-suppliedp
                                                     ,new-arg
                                                     ,init-form)))
                                 extra-bindings))
                         (when (and (not (null suppliedp))
                                    (typep suppliedp 'special-variable))
                           (push (list suppliedp new-suppliedp)
                                 extra-bindings))
                         (list new-arg new-init-form new-suppliedp))))
      ;; And eliminate special rest args.
      (when (and (lambda-information-rest-arg form)
                 (typep (lambda-information-rest-arg form) 'special-variable))
        (let ((new-rest (new-var (string (name (lambda-information-rest-arg form))))))
          (push (list (lambda-information-rest-arg form) new-rest)
                extra-bindings)
          (setf (lambda-information-rest-arg form) new-rest)))
      (when extra-bindings
        ;; Bindings were added.
        (setf (lambda-information-body form)
              (ast `(let ,(reverse extra-bindings)
                      ,(lambda-information-body form)))))
      (lower-arguments-1 (lambda-information-body form)))))

(defun unparse-compiler-form (form)
  (etypecase form
    (ast-block
     `(block ,(info form)
        ,(unparse-compiler-form (body form))))
    (ast-function
     `(function ,(name form)))
    (ast-go
     `(go ,(target form) ,(unparse-compiler-form (info form))))
    (ast-if
     `(if ,(unparse-compiler-form (test form))
          ,(unparse-compiler-form (if-then form))
          ,(unparse-compiler-form (if-else form))))
    (ast-let
     `(let ,(loop
               for (variable init-form) in (bindings form)
               collect (list variable #+nil(if (lexical-variable-p variable)
                                 (lexical-variable-name variable)
                                 variable)
                             (unparse-compiler-form init-form)))
        ,(unparse-compiler-form (body form))))
    (ast-multiple-value-bind
     `(multiple-value-bind ,(mapcar #'unparse-compiler-form (bindings form))
          ,(unparse-compiler-form (value-form form))
        ,(unparse-compiler-form (body form))))
    (ast-multiple-value-call
     `(multiple-value-call ,(function-form form)
        ,(value-form form)))
    (ast-multiple-value-prog1
     `(multiple-value-prog1
          ,(unparse-compiler-form (value-form form))
        ,(unparse-compiler-form (body form))))
    (ast-progn
     `(progn ,@(mapcar #'unparse-compiler-form (forms form))))
    (ast-quote `',(value form))
    (ast-return-from
     `(return-from ,(lexical-variable-name (target form))
        ,(unparse-compiler-form (value form))
        ,(unparse-compiler-form (info form))))
    (ast-setq
     (let ((var (setq-variable form)))
       `(setq ,(if (lexical-variable-p var)
                   (lexical-variable-name var)
                   var)
              ,(unparse-compiler-form (value form)))))
    (ast-tagbody
     `(tagbody
         ,(info form)
         ,@(loop
              for (go-tag form) in (statements form)
              collect go-tag
              collect (unparse-compiler-form form))))
    (ast-the
     `(the ,(the-type form) ,(unparse-compiler-form (value form))))
    (ast-unwind-protect
     `(unwind-protect
           ,(unparse-compiler-form (protected-form form))
        (funcall ,(unparse-compiler-form (cleanup-function form)))))
    (ast-call
     (list* (name form) (mapcar #'unparse-compiler-form (arguments form))))
    (ast-jump-table
     `(sys.int::%jump-table ,(unparse-compiler-form (value form))
                            ,@(mapcar #'unparse-compiler-form (targets form))))
    (lexical-variable form #+nil(lexical-variable-name form))
    (lambda-information
     `(lambda ,(append (when (lambda-information-environment-arg form)
                         `(&env (unparse-compiler-form (lambda-information-environment-arg form))))
                       (loop
                          for arg in (lambda-information-required-args form)
                          collect (unparse-compiler-form arg))
                       (list '&optional)
                       (loop
                          for (arg init-form suppliedp) in (lambda-information-optional-args form)
                          collect (list (unparse-compiler-form arg) (unparse-compiler-form init-form) (unparse-compiler-form suppliedp)))
                       (when (lambda-information-rest-arg form)
                         `(&rest ,(unparse-compiler-form (lambda-information-rest-arg form))))
                       (when (lambda-information-enable-keys form)
                         `(&key
                           ,@(loop
                                for ((keyword arg) init-form suppliedp) in (lambda-information-key-args form)
                                collect (list (list keyword (unparse-compiler-form arg)) (unparse-compiler-form init-form) (unparse-compiler-form suppliedp)))
                           ,@(when (lambda-information-allow-other-keys form)
                                   '(&allow-other-keys)))))
        ,(unparse-compiler-form (lambda-information-body form))))))
