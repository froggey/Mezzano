;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

;;; AST objects.

(defclass ast-node ()
  ((%optimize-qualities :initarg :optimize :accessor ast-optimize))
  (:default-initargs :optimize '()))

(defmethod initialize-instance :after ((instance ast-node) &key inherit &allow-other-keys)
  (when inherit
    (loop
       for (quality value) on (ast-optimize inherit) by #'cddr
       do (setf (getf (ast-optimize instance) quality)
                (max value
                     (getf (ast-optimize instance) quality 0))))))

(defclass lambda-information (ast-node)
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
   (%fref-arg :initarg :fref-arg :accessor lambda-information-fref-arg)
   (%closure-arg :initarg :closure-arg :accessor lambda-information-closure-arg)
   (%count-arg :initarg :count-arg :accessor lambda-information-count-arg)
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
                     :fref-arg nil
                     :closure-arg nil
                     :count-arg nil
                     :plist '()))

(defun lambda-information-p (object)
  (typep object 'lambda-information))

;;; A lexical-variable represents a "renamed" variable, and stores definition information.
(defclass lexical-variable (ast-node)
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

(defmethod name ((object lexical-variable))
  (lexical-variable-name object))

(defun lexical-variable-p (object)
  (typep object 'lexical-variable))

(defmethod print-object ((object lexical-variable) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (lexical-variable-name object))))

(defun localp (var)
  (or (null (lexical-variable-used-in var))
      (and (null (cdr (lexical-variable-used-in var)))
           (eq (car (lexical-variable-used-in var)) (lexical-variable-definition-point var)))))

;;; A special variable, only used in bindings.
(defclass special-variable (ast-node)
  ((%name :initarg :name :accessor name)
   (%implicitly-declared :initarg :implicitly-declared :accessor special-variable-implicitly-declared))
  (:default-initargs :implicitly-declared nil))

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

(defclass go-tag (ast-node)
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

(defclass ast-block (ast-node)
  ((%info :initarg :info :accessor info :accessor ast-info)
   (%body :initarg :body :accessor body :accessor ast-body)))

(defclass ast-function (ast-node)
  ((%name :initarg :name :accessor name :accessor ast-name)))

(defmethod print-object ((object ast-function) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (name object))))

(defclass ast-go (ast-node)
  ((%target :initarg :target :accessor target :accessor ast-target)
   (%info :initarg :info :accessor info :accessor ast-info)))

(defclass ast-if (ast-node)
  ((%test :initarg :test :accessor test :accessor ast-test)
   (%then :initarg :then :accessor if-then :accessor ast-if-then)
   (%else :initarg :else :accessor if-else :accessor ast-if-else)))

(defclass ast-let (ast-node)
  ;; BINDINGS is a list of (variable init-form), where
  ;; variable is either a LEXICAL-VARIABLE or a SPECIAL-VARIABLE.
  ;; Init-forms are evaluated in list order.
  ;; Lexical bindings occur immediately
  ;; Special bindings occur once all init-forms have been evaulated.
  ((%bindings :initarg :bindings :accessor bindings :accessor ast-bindings)
   (%body :initarg :body :accessor body :accessor ast-body)))

(defclass ast-multiple-value-bind (ast-node)
  ;; BINDING is a list of variables, which can be either LEXICAL-VARIABLEs
  ;; or SPECIAL-VARIABLES.
  ;; Bindings occur after VALUE-FORM has been evaulated.
  ((%bindings :initarg :bindings :accessor bindings :accessor ast-bindings)
   (%value-form :initarg :value-form :accessor value-form :accessor ast-value-form)
   (%body :initarg :body :accessor body :accessor ast-body)))

(defclass ast-multiple-value-call (ast-node)
  ((%function-form :initarg :function-form :accessor function-form :accessor ast-function-form)
   (%value-form :initarg :value-form :accessor value-form :accessor ast-value-form)))

(defclass ast-multiple-value-prog1 (ast-node)
  ((%value-form :initarg :value-form :accessor value-form :accessor ast-value-form)
   (%body :initarg :body :accessor body :accessor ast-body)))

(defclass ast-progn (ast-node)
  ((%forms :initarg :forms :accessor forms :accessor ast-forms)))

(defclass ast-quote (ast-node)
  ((%value :initarg :value :accessor value :accessor ast-value)))

(defmethod print-object ((object ast-quote) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (value object))))

(defclass ast-return-from (ast-node)
  ((%target :initarg :target :accessor target :accessor ast-target)
   (%value :initarg :value :accessor value :accessor ast-value)
   (%info :initarg :info :accessor info :accessor ast-info)))

(defclass ast-setq (ast-node)
  ((%variable :initarg :variable :accessor setq-variable :accessor ast-setq-variable)
   (%value :initarg :value :accessor value :accessor ast-value)))

(defclass ast-tagbody (ast-node)
  ((%info :initarg :info :accessor info :accessor ast-info)
   ;; A list of (go-tag form).
   ;; Form that do not end in a control transfer will cause the
   ;; tagbody to return.
   ;; Only the first statement is directly reachable, other
   ;; statements can only be reached via GO forms.
   (%statements :initarg :statements :accessor statements :accessor ast-statements)))

(defclass ast-the (ast-node)
  ((%the-type :initarg :type :accessor the-type :accessor ast-the-type)
   (%value :initarg :value :accessor value :accessor ast-value)))

(defmethod print-object ((instance ast-the) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S" (ast-the-type instance))))

(defclass ast-unwind-protect (ast-node)
  ((%protected-form :initarg :protected-form :accessor protected-form :accessor ast-protected-form)
   (%cleanup-function :initarg :cleanup-function :accessor cleanup-function :accessor ast-cleanup-function)))

(defclass ast-call (ast-node)
  ((%name :initarg :name :accessor name :accessor ast-name)
   (%arguments :initarg :arguments :accessor arguments :accessor ast-arguments)))

(defmethod print-object ((object ast-call) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (name object))))

(defclass ast-jump-table (ast-node)
  ((%value :initarg :value :accessor value :accessor ast-value)
   (%targets :initarg :targets :accessor targets :accessor ast-targets)))

(defmethod print-object ((object go-tag) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (go-tag-name object))))

;;; Deep copying ASTs.

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
                                :inherit var
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
                 :inherit form
                 :info (copy-variable (info form))
                 :body (copy-form-1 (body form))))

(defmethod copy-form-1 ((form ast-go))
  (let ((tag (copy-form-fix (target form))))
    (incf (go-tag-use-count tag))
    (pushnew *current-lambda* (go-tag-used-in tag))
    (incf (lexical-variable-use-count (go-tag-tagbody tag)))
    (pushnew *current-lambda* (lexical-variable-used-in (go-tag-tagbody tag)))
    (make-instance 'ast-go
                   :inherit form
                   :target tag
                   :info (copy-form-1 (info form)))))

(defmethod copy-form-1 ((form ast-function))
  form)

(defmethod copy-form-1 ((form ast-if))
  (make-instance 'ast-if
                 :inherit form
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
                 :inherit form
                 :bindings (loop
                              for (variable init-form) in (bindings form)
                              collect (list (copy-form-fix variable)
                                            (copy-form-1 init-form)))
                 :body (copy-form-1 (body form))))

(defmethod copy-form-1 ((form ast-multiple-value-bind))
  (make-instance 'ast-multiple-value-bind
                 :inherit form
                 :bindings (mapcar #'copy-variable (bindings form))
                 :value-form (copy-form-1 (value-form form))
                 :body (copy-form-1 (body form))))

(defmethod copy-form-1 ((form ast-multiple-value-call))
  (make-instance 'ast-multiple-value-call
                 :inherit form
                 :function-form (copy-form-1 (function-form form))
                 :value-form (copy-form-1 (value-form form))))

(defmethod copy-form-1 ((form ast-multiple-value-prog1))
  (make-instance 'ast-multiple-value-prog1
                 :inherit form
                 :value-form (copy-form-1 (value-form form))
                 :body (copy-form-1 (body form))))

(defmethod copy-form-1 ((form ast-progn))
  (make-instance 'ast-progn
                 :inherit form
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
                   :inherit form
                   :target var
                   :value (copy-form-1 (value form))
                   :info (copy-form-1 (info form)))))

(defmethod copy-form-1 ((form ast-setq))
  (let ((var (copy-form-fix (setq-variable form))))
    (incf (lexical-variable-use-count var))
    (incf (lexical-variable-write-count var))
    (pushnew *current-lambda* (lexical-variable-used-in var))
    (make-instance 'ast-setq
                   :inherit form
                   :variable var
                   :value (copy-form-1 (value form)))))

(defmethod copy-form-1 ((form ast-tagbody))
  (let ((info (make-instance 'tagbody-information
                             :inherit (info form)
                             :definition-point (copy-form-fix (lexical-variable-definition-point (info form))))))
    (push (cons (info form) info) *replacements*)
    (dolist (tag (tagbody-information-go-tags (info form)))
      (let ((new-tag (make-instance 'go-tag
                                    :inherit tag
                                    :name (go-tag-name tag)
                                    :tagbody info)))
        (push new-tag (tagbody-information-go-tags info))
        (push (cons tag new-tag) *replacements*)))
    (make-instance 'ast-tagbody
                   :inherit form
                   :info info
                   :statements (loop
                                  for (go-tag statement) in (statements form)
                                  collect (list (copy-form-fix go-tag)
                                                (copy-form-1 statement))))))

(defmethod copy-form-1 ((form ast-the))
  (make-instance 'ast-the
                 :inherit form
                 :type (the-type form)
                 :value (copy-form-1 (value form))))

(defmethod copy-form-1 ((form ast-unwind-protect))
  (make-instance 'ast-unwind-protect
                 :inherit form
                 :protected-form (copy-form-1 (protected-form form))
                 :cleanup-function (copy-form-1 (cleanup-function form))))

(defmethod copy-form-1 ((form ast-call))
  (make-instance 'ast-call
                 :inherit form
                 :name (name form)
                 :arguments (loop
                               for arg in (arguments form)
                               collect (copy-form-1 arg))))

(defmethod copy-form-1 ((form ast-jump-table))
  (make-instance 'ast-jump-table
                 :inherit form
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
                              :inherit form
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
    (when (lambda-information-fref-arg form)
      (setf (lambda-information-fref-arg info)
            (copy-variable (lambda-information-fref-arg form))))
    (when (lambda-information-closure-arg form)
      (setf (lambda-information-closure-arg info)
            (copy-variable (lambda-information-closure-arg form))))
    (when (lambda-information-count-arg form)
      (setf (lambda-information-count-arg info)
            (copy-variable (lambda-information-count-arg form))))
    (setf (lambda-information-body info)
          (copy-form-1 (lambda-information-body form)))
    info))

;;; Counting variable uses.

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
    (when (lambda-information-fref-arg form)
      (reset-var (lambda-information-fref-arg form)))
    (when (lambda-information-closure-arg form)
      (reset-var (lambda-information-closure-arg form)))
    (when (lambda-information-count-arg form)
      (reset-var (lambda-information-count-arg form)))
    (detect-uses-1 (lambda-information-body form))))

;;; Pretty-printing.

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
     `(multiple-value-call ,(unparse-compiler-form (function-form form))
        ,(unparse-compiler-form (value-form form))))
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
                         `(&env ,(unparse-compiler-form (lambda-information-environment-arg form))))
                       (loop
                          for arg in (lambda-information-required-args form)
                          collect (unparse-compiler-form arg))
                       (list '&optional)
                       (loop
                          for (arg init-form suppliedp) in (lambda-information-optional-args form)
                          collect (list (unparse-compiler-form arg)
                                        (unparse-compiler-form init-form)
                                        (when suppliedp
                                          (unparse-compiler-form suppliedp))))
                       (when (lambda-information-rest-arg form)
                         `(&rest ,(unparse-compiler-form (lambda-information-rest-arg form))))
                       (when (lambda-information-enable-keys form)
                         `(&key
                           ,@(loop
                                for ((keyword arg) init-form suppliedp) in (lambda-information-key-args form)
                                collect (list (list keyword (unparse-compiler-form arg))
                                              (unparse-compiler-form init-form)
                                              (when suppliedp
                                                (unparse-compiler-form suppliedp))))
                           ,@(when (lambda-information-allow-other-keys form)
                                   '(&allow-other-keys))))
                       (when (lambda-information-fref-arg form)
                         `(sys.int::&fref ,(unparse-compiler-form (lambda-information-fref-arg form))))
                       (when (lambda-information-closure-arg form)
                         `(sys.int::&closure ,(unparse-compiler-form (lambda-information-closure-arg form))))
                       (when (lambda-information-count-arg form)
                         `(sys.int::&count ,(unparse-compiler-form (lambda-information-count-arg form)))))
        ,(unparse-compiler-form (lambda-information-body form))))))
