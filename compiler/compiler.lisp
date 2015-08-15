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

(defmethod print-object ((object go-tag) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (go-tag-name object))))

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

(defun flush-form (form)
  "Kill off a form and reduce any use-counts it was holding."
  (flet ((implicit-progn (forms)
	   (dolist (i forms)
	     (flush-form i))))
    (etypecase form
      (cons (case (first form)
	      ((block) (implicit-progn (cddr form)))
	      ((go)
               (decf (go-tag-use-count (second form)))
               (decf (lexical-variable-use-count (go-tag-tagbody (second form)))))
	      ((if) (implicit-progn (cdr form)))
	      ((let)
	       (dolist (b (second form))
		 (flush-form (second b)))
	       (implicit-progn (cddr form)))
	      ((multiple-value-bind) (implicit-progn (cddr form)))
	      ((multiple-value-call) (implicit-progn (cdr form)))
	      ((multiple-value-prog1) (implicit-progn (cdr form)))
	      ((progn) (implicit-progn (cdr form)))
	      ((function quote))
	      ((return-from)
	       (decf (lexical-variable-use-count (second form)))
	       (flush-form (third form))
	       (flush-form (fourth form)))
	      ((setq)
	       (decf (lexical-variable-use-count (second form)))
	       (decf (lexical-variable-write-count (second form)))
	       (flush-form (third form)))
	      ((tagbody)
	       (dolist (i (cddr form))
		 (unless (go-tag-p i)
		   (flush-form i))))
	      ((the) (flush-form (third form)))
	      ((unwind-protect) (implicit-progn (cdr form)))
	    (t (implicit-progn (cdr form)))))
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
      (cons (case (first form)
	      ((block)
	       `(block ,(copy-variable (second form)) ,@(implicit-progn (cddr form))))
	      ((go)
	       (let ((tag (fix (second form))))
		 (incf (go-tag-use-count tag))
		 (pushnew *current-lambda* (go-tag-used-in tag))
		 (incf (lexical-variable-use-count (go-tag-tagbody tag)))
		 (pushnew *current-lambda* (lexical-variable-used-in (go-tag-tagbody tag)))
		 `(go ,tag ,(go-tag-tagbody tag))))
	      ((if) `(if ,@(implicit-progn (cdr form))))
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
	      ((multiple-value-bind)
               `(multiple-value-bind ,(mapcar #'copy-variable (second form))
                    ,@(implicit-progn (cddr form))))
	      ((multiple-value-call) `(multiple-value-call ,@(implicit-progn (cdr form))))
	      ((multiple-value-prog1) `(multiple-value-prog1 ,@(implicit-progn (cdr form))))
	      ((progn) `(progn ,@(implicit-progn (cdr form))))
	      ((function quote) form)
	      ((return-from)
	       (let ((var (fix (second form))))
		 (incf (lexical-variable-use-count var))
		 (pushnew *current-lambda* (lexical-variable-used-in var))
		 `(return-from ,var ,(copy-form (third form) replacements) ,(copy-form (fourth form) replacements))))
	      ((setq)
	       (let ((var (fix (second form))))
		 (incf (lexical-variable-use-count var))
		 (incf (lexical-variable-write-count var))
		 (pushnew *current-lambda* (lexical-variable-used-in var))
		 `(setq ,var ,(copy-form (third form) replacements))))
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
	      ((the) `(the ,(second form) ,(copy-form (third form) replacements)))
	      ((unwind-protect) `(unwind-protect ,@(implicit-progn (cdr form))))
	      (t (list* (first form) (implicit-progn (cdr form))))))
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
      (cons (case (first form)
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
	      ((if) (implicit-progn (cdr form)))
	      ((let)
	       (dolist (b (second form))
		 (reset-var (first b)))
	       (dolist (b (second form))
		 (detect-uses (second b)))
	       (implicit-progn (cddr form)))
	      ((multiple-value-bind)
               (dolist (v (second form))
                 (reset-var v))
               (implicit-progn (cddr form)))
	      ((multiple-value-call) (implicit-progn (cdr form)))
	      ((multiple-value-prog1) (implicit-progn (cdr form)))
	      ((progn) (implicit-progn (cdr form)))
	      ((function quote))
	      ((return-from)
	       (incf (lexical-variable-use-count (second form)))
	       (detect-uses (third form))
	       (detect-uses (fourth form)))
	      ((setq)
	       (pushnew *current-lambda* (lexical-variable-used-in (second form)))
	       (incf (lexical-variable-use-count (second form)))
	       (incf (lexical-variable-write-count (second form)))
	       (detect-uses (third form)))
	      ((tagbody)
               (reset-var (second form))
	       (dolist (tag (tagbody-information-go-tags (second form)))
		 (setf (go-tag-use-count tag) 0
		       (go-tag-used-in tag) '()))
	       (dolist (i (cddr form))
		 (unless (go-tag-p i)
		   (detect-uses i))))
	      ((the) (detect-uses (third form)))
	      ((unwind-protect) (implicit-progn (cdr form)))
	    (t (implicit-progn (cdr form)))))
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
                      `(if (eql ,current-keyword ',(caar (first key-args)))
                           (if ,(first suppliedp)
                               'nil
                               (progn (setq ,(first suppliedp) 't)
                                      (setq ,(first values) (cadr ,itr))))
                           ,(create-key-test-list (rest key-args) (rest values) (rest suppliedp))))
                     (allow-other-keys
                      ''nil)
                     (t `(error 'sys.int::simple-program-error
                                ':format-control '"Unknown &KEY argument ~S. Expected one of ~S."
                                ':format-arguments (list ,current-keyword ',(mapcar 'caar keys))))))
             (create-key-let-body (key-args values suppliedp)
               (cond (key-args
                      `(let ((,(second (first (first key-args))) (if ,(first suppliedp)
                                                                     ,(first values)
                                                                     ,(second (first key-args)))))
                         ,(if (third (first key-args))
                              `(let ((,(third (first key-args)) ,(first suppliedp)))
                                 ,(create-key-let-body (rest key-args) (rest values) (rest suppliedp)))
                              (create-key-let-body (rest key-args) (rest values) (rest suppliedp)))))
                     (t body))))
      `(let ,(append (mapcar (lambda (x) (list x ''nil)) values)
                     (mapcar (lambda (x) (list x ''nil)) suppliedp)
                     (list (list itr (if (symbolp rest) `(symbol-value ,rest) rest))))
         (tagbody ,tb
            (go ,test-tag ,(go-tag-tagbody test-tag))
            ,head-tag
            (if (null (cdr ,itr))
                (error 'sys.int::simple-program-error
                       ':format-control '"Odd number of &KEY arguments.")
                'nil)
            (let ((,current-keyword (car ,itr)))
              ,(create-key-test-list keys values suppliedp))
            (setq ,itr (cddr ,itr))
            ,test-tag
            (if ,itr
                (go ,head-tag ,(go-tag-tagbody head-tag))
                'nil))
         ,(create-key-let-body keys values suppliedp)))))

(defun lower-keyword-arguments (form)
  "Walk form, lowering keyword arguments to simple lisp code."
  (flet ((implicit-progn (forms)
	   (dolist (i forms)
	     (lower-keyword-arguments i))))
    (etypecase form
      (cons (case (first form)
	      ((block) (implicit-progn (cddr form)))
	      ((go))
	      ((if) (implicit-progn (cdr form)))
	      ((let)
	       (dolist (b (second form))
		 (lower-keyword-arguments (second b)))
	       (implicit-progn (cddr form)))
	      ((multiple-value-bind) (implicit-progn (cddr form)))
	      ((multiple-value-call) (implicit-progn (cdr form)))
	      ((multiple-value-prog1) (implicit-progn (cdr form)))
	      ((progn) (implicit-progn (cdr form)))
	      ((function quote))
	      ((return-from)
               (lower-keyword-arguments (third form))
               (lower-keyword-arguments (fourth form)))
	      ((setq) (lower-keyword-arguments (third form)))
	      ((tagbody)
	       (dolist (i (cddr form))
		 (unless (go-tag-p i)
		   (lower-keyword-arguments i))))
	      ((the) (lower-keyword-arguments (third form)))
	      ((unwind-protect) (implicit-progn (cdr form)))
              (t (implicit-progn (cdr form)))))
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
      (cons (case (first form)
	      ((block) (implicit-progn (cddr form)))
	      ((go))
	      ((if) (implicit-progn (cdr form)))
	      ((let)
	       (dolist (b (second form))
		 (lower-arguments (second b)))
	       (implicit-progn (cddr form)))
	      ((multiple-value-bind) (implicit-progn (cddr form)))
	      ((multiple-value-call) (implicit-progn (cdr form)))
	      ((multiple-value-prog1) (implicit-progn (cdr form)))
	      ((progn) (implicit-progn (cdr form)))
	      ((function quote))
	      ((return-from)
               (lower-arguments (third form))
               (lower-arguments (fourth form)))
	      ((setq) (lower-arguments (third form)))
	      ((tagbody)
	       (dolist (i (cddr form))
		 (unless (go-tag-p i)
		   (lower-arguments i))))
	      ((the) (lower-arguments (third form)))
	      ((unwind-protect) (implicit-progn (cdr form)))
              (t (implicit-progn (cdr form)))))
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
                               (trivial-init-form (and (listp init-form)
                                                       (= (length init-form) 2)
                                                       (eql (first init-form) 'quote)))
                               (new-arg (cond ((symbolp arg)
                                               (new-var (string arg)))
                                              ((not trivial-init-form)
                                               (new-var (string (lexical-variable-name arg))))
                                              (t arg)))
                               (new-init-form (if trivial-init-form
                                                  init-form
                                                  ''nil)))
                          (when (or (not trivial-init-form)
                                    (symbolp arg))
                            (push (list arg `(if ,new-suppliedp
                                                 ,new-arg
                                                 ,init-form))
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
	      ((if)
               `(if ,@(implicit-progn (rest form))))
	      ((let)
               `(let ,(mapcar (lambda (b) (list (if (lexical-variable-p (first b))
                                                    (lexical-variable-name (first b))
                                                    (first b))
                                                (unparse-compiler-form (second b))))
                              (second form))
                  ,@(implicit-progn (cddr form))))
	      ((multiple-value-bind)
               `(multiple-value-bind ,@(implicit-progn (cddr form))))
	      ((multiple-value-call)
               `(multiple-value-call ,@(implicit-progn (cdr form))))
	      ((multiple-value-prog1)
               `(multiple-value-prog1 ,@(implicit-progn (cdr form))))
	      ((progn)
               `(progn ,@(implicit-progn (cdr form))))
	      ((function quote) form)
	      ((return-from)
               `(return-from ,(unparse-compiler-form (second form))
                  ,(unparse-compiler-form (third form))))
	      ((setq)
               `(setq ,(if (lexical-variable-p (second form))
                           (lexical-variable-name (second form))
                           (second form))
                      ,(unparse-compiler-form (third form))))
	      ((tagbody)
               `(tagbody ,@(mapcar (lambda (x)
                                     (if (go-tag-p x)
                                         (go-tag-name x)
                                         (unparse-compiler-form x)))
                                   (cddr form))))
	      ((the)
               `(the ,(second form) ,(unparse-compiler-form (third form))))
	      ((unwind-protect)
               `(unwind-protect ,@(implicit-progn (cdr form))))
              (t (list* (first form) (implicit-progn (cdr form))))))
      (lexical-variable (lexical-variable-name form))
      (lambda-information
       `(lambda ????
          ,(lambda-information-body form))))))
