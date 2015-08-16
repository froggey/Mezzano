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
     (simp-form
      (detect-uses
       ;; Make the dynamic environment explicit.
       (lsb-lambda
        ;; Run a final simplify pass to kill off any useless bindings.
        (detect-uses
         (simp-form
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
      (setf form (il-form (detect-uses form)))
      (setf form (ll-form (detect-uses form)))
      ;; Key arg conversion must be performed after lambda-lifting, so as not to
      ;; complicate the lift code.
      (setf form (lower-keyword-arguments form))
      (setf form (constprop (detect-uses form)))
      (setf form (simp-form (detect-uses form)))
      (setf form (kt-form (detect-uses form)))
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
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (lexical-variable-name object))))

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

(defclass ast-function ()
  ((%name :initarg :name :accessor name)))

(defclass ast-if ()
  ((%test :initarg :test :accessor test)
   (%then :initarg :then :accessor if-then)
   (%else :initarg :else :accessor if-else)))

(defclass ast-multiple-value-bind ()
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

(defclass ast-setq ()
  ((%variable :initarg :variable :accessor setq-variable)
   (%value :initarg :value :accessor value)))

(defclass ast-the ()
  ((%the-type :initarg :type :accessor the-type)
   (%value :initarg :value :accessor value)))

(defclass ast-call ()
  ((%name :initarg :name :accessor name)
   (%arguments :initarg :arguments :accessor arguments)))

(defmethod print-object ((object go-tag) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (go-tag-name object))))

(defun flush-form (form)
  "Kill off a form and reduce any use-counts it was holding."
  (flet ((implicit-progn (forms)
	   (dolist (i forms)
	     (flush-form i))))
    (etypecase form
      (cons (ecase (first form)
	      ((block) (implicit-progn (cddr form)))
	      ((go)
               (decf (go-tag-use-count (second form)))
               (decf (lexical-variable-use-count (go-tag-tagbody (second form)))))
	      ((let)
	       (dolist (b (second form))
		 (flush-form (second b)))
	       (implicit-progn (cddr form)))
	      ((return-from)
	       (decf (lexical-variable-use-count (second form)))
	       (flush-form (third form))
	       (flush-form (fourth form)))
	      ((tagbody)
	       (dolist (i (cddr form))
		 (unless (go-tag-p i)
		   (flush-form i))))
	      ((unwind-protect) (implicit-progn (cdr form)))
              ((sys.int::%jump-table) (implicit-progn (cdr form)))))
      (ast-function)
      (ast-if
       (flush-form (test form))
       (flush-form (if-then form))
       (flush-form (if-else form)))
      (ast-multiple-value-bind
       (flush-form (value-form form))
       (flush-form (body form)))
      (ast-multiple-value-call
       (flush-form (function-form form))
       (flush-form (value-form form)))
      (ast-multiple-value-prog1
       (flush-form (value-form form))
       (flush-form (body form)))
      (ast-progn (implicit-progn (forms form)))
      (ast-quote)
      (ast-setq
       (decf (lexical-variable-use-count (setq-variable form)))
       (decf (lexical-variable-write-count (setq-variable form)))
       (flush-form (value form)))
      (ast-the
       (flush-form (value form)))
      (ast-call
       (mapc #'flush-form (arguments form)))
      (lexical-variable
       (decf (lexical-variable-use-count form)))
      (lambda-information
       (dolist (arg (lambda-information-optional-args form))
         (flush-form (second arg)))
       (dolist (arg (lambda-information-key-args form))
         (flush-form (second arg)))
       (flush-form (lambda-information-body form))))))

(defun copy-form (form &optional replacements)
  "Completely copy a form, incrementing use-counts."
  (labels ((fix (thing)
	     (let ((r (assoc thing replacements)))
	       (if r (cdr r) thing)))
	   (implicit-progn (forms)
	     (mapcar (lambda (x) (copy-form x replacements)) forms))
	   (copy-variable (var)
	     (if (lexical-variable-p var)
		 (let ((new (make-instance (if (block-information-p var)
                                               'block-information
                                               'lexical-variable)
                                     :name (lexical-variable-name var)
                                     :ignore (lexical-variable-ignore var)
                                     :dynamic-extent (lexical-variable-dynamic-extent var))))
		   (setf (lexical-variable-definition-point new) (fix (lexical-variable-definition-point var))
			 (lexical-variable-use-count new) 0
			 (lexical-variable-write-count new) 0
			 (lexical-variable-used-in new) '())
		   (push (cons var new) replacements)
		   new)
		 var)))
    (etypecase form
      (cons (ecase (first form)
	      ((block)
	       `(block ,(copy-variable (second form)) ,@(implicit-progn (cddr form))))
	      ((go)
	       (let ((tag (fix (second form))))
		 (incf (go-tag-use-count tag))
		 (pushnew *current-lambda* (go-tag-used-in tag))
		 (incf (lexical-variable-use-count (go-tag-tagbody tag)))
		 (pushnew *current-lambda* (lexical-variable-used-in (go-tag-tagbody tag)))
		 `(go ,tag ,(go-tag-tagbody tag))))
	      ((let)
	       ;; So that labels works correctly, this must create the variables and then
	       ;; copy init-forms.
	       (dolist (b (second form))
		 (copy-variable (first b)))
	       `(let ,(mapcar (lambda (b)
				(list (fix (first b))
				      (copy-form (second b) replacements)))
			      (second form))
		  ,@(implicit-progn (cddr form))))
	      ((return-from)
	       (let ((var (fix (second form))))
		 (incf (lexical-variable-use-count var))
		 (pushnew *current-lambda* (lexical-variable-used-in var))
		 `(return-from ,var ,(copy-form (third form) replacements) ,(copy-form (fourth form) replacements))))
	      ((tagbody)
	       (let ((info (make-instance 'tagbody-information
                                          :definition-point (fix (lexical-variable-definition-point (second form))))))
		 (push (cons (second form) info) replacements)
		 (dolist (tag (tagbody-information-go-tags (second form)))
		   (let ((new-tag (make-instance 'go-tag
                                                 :name (go-tag-name tag)
                                                 :tagbody info)))
		     (push new-tag (tagbody-information-go-tags info))
		     (push (cons tag new-tag) replacements)))
		 `(tagbody ,info
		     ,@(mapcar (lambda (x)
				 (if (go-tag-p x)
				     (fix x)
				     (copy-form x replacements)))
			       (cddr form)))))
	      ((unwind-protect) `(unwind-protect ,@(implicit-progn (cdr form))))
              ((sys.int::%jump-table) `(sys.int::%jump-table ,@(implicit-progn (cdr form))))))
      (ast-function form)
      (ast-if
       (make-instance 'ast-if
                      :test (copy-form (test form) replacements)
                      :then (copy-form (if-then form) replacements)
                      :else (copy-form (if-else form) replacements)))
      (ast-multiple-value-bind
       (make-instance 'ast-multiple-value-bind
                      :bindings (mapcar #'copy-variable (bindings form))
                      :value-form (copy-form (value-form form) replacements)
                      :body (copy-form (body form) replacements)))
      (ast-multiple-value-call
       (make-instance 'ast-multiple-value-call
                      :function-form (copy-form (function-form form) replacements)
                      :value-form (copy-form (value-form form) replacements)))
      (ast-multiple-value-prog1
       (make-instance 'ast-multiple-value-prog1
                      :value-form (copy-form (value-form form) replacements)
                      :body (copy-form (body form) replacements)))
      (ast-progn
       (make-instance 'ast-progn
                      :forms (implicit-progn (forms form))))
      (ast-quote form)
      (ast-setq
       (let ((var (fix (setq-variable form))))
         (incf (lexical-variable-use-count var))
         (incf (lexical-variable-write-count var))
         (pushnew *current-lambda* (lexical-variable-used-in var))
         (make-instance 'ast-setq
                        :variable var
                        :value (copy-form (value form) replacements))))
      (ast-the
       (make-instance 'ast-the
                      :type (the-type form)
                      :value (copy-form (value form) replacements)))
      (ast-call
       (make-instance 'ast-call
                      :name (name form)
                      :arguments (loop
                                    for arg in (arguments form)
                                    collect (copy-form arg replacements))))
      (lexical-variable
       (let ((var (fix form)))
	 (incf (lexical-variable-use-count var))
	 (pushnew *current-lambda* (lexical-variable-used-in var))
	 var))
      (lambda-information
       (let* ((info (make-instance 'lambda-information
                                   :name (lambda-information-name form)
                                   :docstring (lambda-information-docstring form)
                                   :lambda-list (lambda-information-lambda-list form)
                                   :enable-keys (lambda-information-enable-keys form)
                                   :allow-other-keys (lambda-information-allow-other-keys form)
                                   :plist (copy-list (lambda-information-plist form))))
	      (*current-lambda* info))
         (incf (getf (lambda-information-plist info) 'copy-count 0))
	 (push (cons form info) replacements)
	 (setf (lambda-information-required-args info)
	       (mapcar #'copy-variable (lambda-information-required-args form)))
	 (setf (lambda-information-optional-args info)
	       (mapcar (lambda (x)
			 (list (copy-variable (first x))
			       (copy-form (second x) replacements)
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
			       (copy-form (second x) replacements)
			       (when (third x)
				 (copy-variable (third x)))))
		       (lambda-information-key-args form)))
         (when (lambda-information-environment-arg form)
	   (setf (lambda-information-environment-arg info)
		 (copy-variable (lambda-information-environment-arg form))))
	 (setf (lambda-information-body info)
	       (copy-form (lambda-information-body form) replacements))
	 info)))))

(defun detect-uses (form)
  "Walk form, refreshing variable use counts & locations."
  (flet ((implicit-progn (forms)
	   (dolist (i forms)
	     (detect-uses i)))
	 (reset-var (var)
	   (when (lexical-variable-p var)
	     (setf (lexical-variable-used-in var) '()
		   (lexical-variable-use-count var) 0
		   (lexical-variable-write-count var) 0))))
    (etypecase form
      (cons (ecase (first form)
	      ((block)
	       (reset-var (second form))
	       (implicit-progn (cddr form)))
	      ((go)
               (assert (or (not (tagbody-information-p (third form)))
                           (eql (go-tag-tagbody (second form)) (third form))))
               (detect-uses (third form))
	       (incf (go-tag-use-count (second form)))
	       (pushnew *current-lambda* (go-tag-used-in (second form)))
               (incf (lexical-variable-use-count (go-tag-tagbody (second form)))))
	      ((let)
	       (dolist (b (second form))
		 (reset-var (first b)))
	       (dolist (b (second form))
		 (detect-uses (second b)))
	       (implicit-progn (cddr form)))
	      ((return-from)
	       (incf (lexical-variable-use-count (second form)))
	       (detect-uses (third form))
	       (detect-uses (fourth form)))
	      ((tagbody)
               (reset-var (second form))
	       (dolist (tag (tagbody-information-go-tags (second form)))
		 (setf (go-tag-use-count tag) 0
		       (go-tag-used-in tag) '()))
	       (dolist (i (cddr form))
		 (unless (go-tag-p i)
		   (detect-uses i))))
	      ((unwind-protect) (implicit-progn (cdr form)))
              ((sys.int::%jump-table) (implicit-progn (cdr form)))))
      (ast-function)
      (ast-if
       (detect-uses (test form))
       (detect-uses (if-then form))
       (detect-uses (if-else form)))
      (ast-multiple-value-bind
       (mapc #'reset-var (bindings form))
       (detect-uses (value-form form))
       (detect-uses (body form)))
      (ast-multiple-value-call
       (detect-uses (function-form form))
       (detect-uses (value-form form)))
      (ast-multiple-value-prog1
       (detect-uses (value-form form))
       (detect-uses (body form)))
      (ast-progn
       (implicit-progn (forms form)))
      (ast-quote)
      (ast-setq
       (let ((var (setq-variable form)))
         (pushnew *current-lambda* (lexical-variable-used-in var))
         (incf (lexical-variable-use-count var))
         (incf (lexical-variable-write-count var))
         (detect-uses (value form))))
      (ast-the
       (detect-uses (value form)))
      (ast-call
       (dolist (arg (arguments form))
         (detect-uses arg)))
      (lexical-variable
       (pushnew *current-lambda* (lexical-variable-used-in form))
       (incf (lexical-variable-use-count form)))
      (lambda-information
       (let ((*current-lambda* form))
         (dolist (arg (lambda-information-required-args form))
           (reset-var arg))
         (dolist (arg (lambda-information-optional-args form))
           (reset-var (first arg))
           (detect-uses (second arg))
           (when (third arg)
             (reset-var (third arg))))
         (when (lambda-information-rest-arg form)
           (reset-var (lambda-information-rest-arg form)))
         (dolist (arg (lambda-information-key-args form))
           (reset-var (second (first arg)))
           (detect-uses (second arg))
           (when (third arg)
             (reset-var (third arg))))
         (when (lambda-information-environment-arg form)
           (reset-var (lambda-information-environment-arg form)))
         (detect-uses (lambda-information-body form))))))
  form)

(defun variable-name (var)
  (if (symbolp var)
      var
      (lexical-variable-name var)))

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
                                         :definition-point *current-lambda*))
         (tb (make-instance 'tagbody-information :definition-point *current-lambda*))
         (head-tag (make-instance 'go-tag
                                  :name (gensym "HEAD")
                                  :tagbody tb))
         (test-tag (make-instance 'go-tag
                                  :name (gensym "TEST")
                                  :tagbody tb)))
    (push head-tag (tagbody-information-go-tags tb))
    (push test-tag (tagbody-information-go-tags tb))
    (labels ((create-key-test-list (key-args values suppliedp)
               (cond (key-args
                      (make-instance 'ast-if
                                     :test (make-instance 'ast-call
                                                          :name 'eql
                                                          :arguments (list current-keyword
                                                                           (make-instance 'ast-quote :value (caar (first key-args)))))
                                     :then (make-instance 'ast-if
                                                          :test (first suppliedp)
                                                          :then (make-instance 'ast-quote :value nil)
                                                          :else (make-instance 'ast-progn
                                                                               :forms (list (make-instance 'ast-setq
                                                                                                           :variable (first suppliedp)
                                                                                                           :value (make-instance 'ast-quote :value 't))
                                                                                            (make-instance 'ast-setq
                                                                                                           :variable (first values)
                                                                                                           :value (make-instance 'ast-call
                                                                                                                                 :name 'cadr
                                                                                                                                 :arguments (list itr))))))
                                     :else (create-key-test-list (rest key-args) (rest values) (rest suppliedp))))
                     (allow-other-keys
                      (make-instance 'ast-quote :value nil))
                     (t (make-instance 'ast-call
                                       :name 'error
                                       :arguments (list (make-instance 'ast-quote :value 'sys.int::simple-program-error)
                                                        (make-instance 'ast-quote :value ':format-control)
                                                        (make-instance 'ast-quote :value '"Unknown &KEY argument ~S. Expected one of ~S.")
                                                        (make-instance 'ast-quote :value ':format-arguments)
                                                        (make-instance 'ast-call
                                                                       :name 'list
                                                                       :arguments (list current-keyword
                                                                                        (make-instance 'ast-quote :value (mapcar 'caar keys)))))))))
             (create-key-let-body (key-args values suppliedp)
               (cond (key-args
                      `(let ((,(second (first (first key-args)))
                              ,(make-instance 'ast-if
                                              :test (first suppliedp)
                                              :then (first values)
                                              :else (second (first key-args)))))
                         ,(if (third (first key-args))
                              `(let ((,(third (first key-args)) ,(first suppliedp)))
                                 ,(create-key-let-body (rest key-args) (rest values) (rest suppliedp)))
                              (create-key-let-body (rest key-args) (rest values) (rest suppliedp)))))
                     (t body))))
      `(let ,(append (mapcar (lambda (x) (list x (make-instance 'ast-quote :value nil))) values)
                     (mapcar (lambda (x) (list x (make-instance 'ast-quote :value nil))) suppliedp)
                     (list (list itr (if (symbolp rest)
                                         (make-instance 'ast-call
                                                        :name 'symbol-value
                                                        :arguments (list (make-instance 'ast-quote
                                                                                        :value rest)))
                                         rest))))
         (tagbody ,tb
            (go ,test-tag ,(go-tag-tagbody test-tag))
            ,head-tag
            ,(make-instance 'ast-if
                            :test (make-instance 'ast-call
                                                 :name 'null
                                                 :arguments (list (make-instance 'ast-call
                                                                                 :name 'cdr
                                                                                 :arguments (list itr))))
                            :then (make-instance 'ast-call
                                                 :name 'error
                                                 :arguments (list (make-instance 'ast-quote :value 'sys.int::simple-program-error)
                                                                  (make-instance 'ast-quote :value ':format-control)
                                                                  (make-instance 'ast-quote :value '"Odd number of &KEY arguments.")))
                            :else (make-instance 'ast-quote :value nil))
            (let ((,current-keyword ,(make-instance 'ast-call
                                                    :name 'car
                                                    :arguments (list itr))))
              ,(create-key-test-list keys values suppliedp))
            ,(make-instance 'ast-setq
                            :variable itr
                            :value (make-instance 'ast-call
                                                  :name 'cddr
                                                  :arguments (list itr)))
            ,test-tag
            ,(make-instance 'ast-if
                            :test itr
                            :then `(go ,head-tag ,(go-tag-tagbody head-tag))
                            :else (make-instance 'ast-quote :value nil)))
         ,(create-key-let-body keys values suppliedp)))))

(defun lower-keyword-arguments (form)
  "Walk form, lowering keyword arguments to simple lisp code."
  (flet ((implicit-progn (forms)
	   (dolist (i forms)
	     (lower-keyword-arguments i))))
    (etypecase form
      (cons (ecase (first form)
	      ((block) (implicit-progn (cddr form)))
	      ((go))
	      ((let)
	       (dolist (b (second form))
		 (lower-keyword-arguments (second b)))
	       (implicit-progn (cddr form)))
	      ((return-from)
               (lower-keyword-arguments (third form))
               (lower-keyword-arguments (fourth form)))
	      ((tagbody)
	       (dolist (i (cddr form))
		 (unless (go-tag-p i)
		   (lower-keyword-arguments i))))
	      ((unwind-protect) (implicit-progn (cdr form)))
	      ((sys.int::%jump-table) (implicit-progn (cdr form)))))
      (ast-function)
      (ast-if
       (lower-keyword-arguments (test form))
       (lower-keyword-arguments (if-then form))
       (lower-keyword-arguments (if-else form)))
      (ast-multiple-value-bind
       (lower-keyword-arguments (value-form form))
       (lower-keyword-arguments (body form)))
      (ast-multiple-value-call
       (lower-keyword-arguments (function-form form))
       (lower-keyword-arguments (value-form form)))
      (ast-multiple-value-prog1
       (lower-keyword-arguments (value-form form))
       (lower-keyword-arguments (body form)))
      (ast-progn
       (implicit-progn (forms form)))
      (ast-quote)
      (ast-setq (lower-keyword-arguments (value form)))
      (ast-the (lower-keyword-arguments (value form)))
      (ast-call
       (dolist (arg (arguments form))
         (lower-keyword-arguments arg)))
      (lexical-variable)
      (lambda-information
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
           (lower-keyword-arguments (second arg)))
         (lower-keyword-arguments (lambda-information-body form))))))
  form)

(defun lower-arguments (form)
  "Simplify lambda lists so that no lambda argument is special and
so that no &OPTIONAL argument has a non-constant init-form.
Must be run after keywords have been lowered."
  (flet ((implicit-progn (forms)
	   (dolist (i forms)
	     (lower-arguments i)))
         (new-var (name)
           (make-instance 'lexical-variable
                          :name (gensym name)
                          :definition-point *current-lambda*)))
    (etypecase form
      (cons (ecase (first form)
	      ((block) (implicit-progn (cddr form)))
	      ((go))
	      ((let)
	       (dolist (b (second form))
		 (lower-arguments (second b)))
	       (implicit-progn (cddr form)))
	      ((return-from)
               (lower-arguments (third form))
               (lower-arguments (fourth form)))
	      ((tagbody)
	       (dolist (i (cddr form))
		 (unless (go-tag-p i)
		   (lower-arguments i))))
	      ((unwind-protect) (implicit-progn (cdr form)))
	      ((sys.int::%jump-table) (implicit-progn (cdr form)))))
      (ast-function)
      (ast-if
       (lower-arguments (test form))
       (lower-arguments (if-then form))
       (lower-arguments (if-else form)))
      (ast-multiple-value-bind
       (lower-arguments (value-form form))
       (lower-arguments (body form)))
      (ast-multiple-value-call
       (lower-arguments (function-form form))
       (lower-arguments (value-form form)))
      (ast-multiple-value-prog1
       (lower-arguments (value-form form))
       (lower-arguments (body form)))
      (ast-progn
       (implicit-progn (forms form)))
      (ast-quote)
      (ast-setq (lower-arguments (value form)))
      (ast-the (lower-arguments (value form)))
      (ast-call
       (dolist (arg (arguments form))
         (lower-arguments arg)))
      (lexical-variable)
      (lambda-information
       (let* ((*current-lambda* form)
              (extra-bindings '()))
         (when (lambda-information-enable-keys form)
           (error "Keyword arguments not lowered!"))
         ;; Eliminate special required arguments.
         (setf (lambda-information-required-args form)
               (loop for arg in (lambda-information-required-args form)
                  collect (if (symbolp arg)
                              (let ((temp (new-var (string arg))))
                                (push (list arg temp) extra-bindings)
                                temp)
                              arg)))
         ;; Eliminate special optional arguments & non-constant init-forms.
         (setf (lambda-information-optional-args form)
               (loop for (arg init-form suppliedp) in (lambda-information-optional-args form)
                  collect (let* ((new-suppliedp (cond ((null suppliedp)
                                                       (new-var (format nil "~S-suppliedp" arg)))
                                                      ((symbolp suppliedp)
                                                       (new-var (string suppliedp)))
                                                      (t suppliedp)))
                                 (trivial-init-form (typep init-form 'ast-quote))
                                 (new-arg (cond ((symbolp arg)
                                                 (new-var (string arg)))
                                                ((not trivial-init-form)
                                                 (new-var (string (lexical-variable-name arg))))
                                                (t arg)))
                                 (new-init-form (if trivial-init-form
                                                    init-form
                                                    (make-instance 'ast-quote :value 'nil))))
                            (when (or (not trivial-init-form)
                                      (symbolp arg))
                              (push (list arg (make-instance 'ast-if
                                                             :test new-suppliedp
                                                             :then new-arg
                                                             :else init-form))
                                    extra-bindings))
                            (when (and (not (null suppliedp))
                                       (symbolp suppliedp))
                              (push (list suppliedp new-suppliedp)
                                    extra-bindings))
                            (list new-arg new-init-form new-suppliedp))))
         ;; And eliminate special rest args.
         (when (and (lambda-information-rest-arg form)
                    (symbolp (lambda-information-rest-arg form)))
           (let ((new-rest (new-var (string (lambda-information-rest-arg form)))))
             (push (list (lambda-information-rest-arg form) new-rest)
                   extra-bindings)
             (setf (lambda-information-rest-arg form) new-rest)))
         (when extra-bindings
           ;; Bindings were added.
           (setf (lambda-information-body form)
                 `(let ,(reverse extra-bindings)
                    ,(lambda-information-body form))))
         (lower-arguments (lambda-information-body form))))))
  form)

(defun unparse-compiler-form (form)
  (flet ((implicit-progn (forms) (mapcar 'unparse-compiler-form forms)))
    (etypecase form
      (cons (case (first form)
	      ((block)
               `(block ,(lexical-variable-name (second form))
                  ,@(implicit-progn (cddr form))))
	      ((go)
               `(go ,(go-tag-name (second form))))
	      ((let)
               `(let ,(mapcar (lambda (b) (list (if (lexical-variable-p (first b))
                                                    (lexical-variable-name (first b))
                                                    (first b))
                                                (unparse-compiler-form (second b))))
                              (second form))
                  ,@(implicit-progn (cddr form))))
	      ((return-from)
               `(return-from ,(unparse-compiler-form (second form))
                  ,(unparse-compiler-form (third form))))
	      ((tagbody)
               `(tagbody ,@(mapcar (lambda (x)
                                     (if (go-tag-p x)
                                         (go-tag-name x)
                                         (unparse-compiler-form x)))
                                   (cddr form))))
	      ((unwind-protect)
               `(unwind-protect ,@(implicit-progn (cdr form))))
              ((sys.int::%jump-table)
               `(sys.int::%jump-table ,@(implicit-progn (cdr form))))
              (t `(:invalid ,form))))
      (ast-function
       `(function ,(name form)))
      (ast-if
       `(if ,(unparse-compiler-form (test form))
            ,(unparse-compiler-form (if-then form))
            ,(unparse-compiler-form (if-else form))))
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
       `(progn ,@(implicit-progn (forms form))))
      (ast-quote `',(value form))
      (ast-setq
       (let ((var (setq-variable form)))
         `(setq ,(if (lexical-variable-p var)
                     (lexical-variable-name var)
                     var)
                ,(unparse-compiler-form (value form)))))
      (ast-the
       `(the ,(the-type form) ,(unparse-compiler-form (value form))))
      (ast-call
       (list* (name form) (mapcar #'unparse-compiler-form (arguments form))))
      (lexical-variable (lexical-variable-name form))
      (lambda-information
       `(lambda ????
          ,(unparse-compiler-form (lambda-information-body form)))))))
