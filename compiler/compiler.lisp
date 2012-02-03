(defpackage #:system.compiler
  (:nicknames #:sys.c)
  (:use #:cl #:system))

(in-package #:system.compiler)

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

;(defun compile (name &optional definition)
;  (when name
;    (error "TODO: Compiling named functions."))
;  (unless definition
;    (error "Compiling nothing!"))
;  (pass1-lambda definition nil))

(defun compile-lambda (lambda &optional env)
  (codegen-lambda (detect-uses (run-optimizers (pass1-lambda lambda env)))))

(defvar *current-lambda* nil
  "A lambda-information struct for the lambda currently being translated.")
(defvar *change-count* nil
  "Number of changes made by the optimizer passes.")

(defstruct lambda-information
  name
  docstring
  lambda-list
  body
  required-args
  optional-args
  rest-arg
  enable-keys
  key-args
  allow-other-keys)

;;; A lexical-variable represents a "renamed" variable, and stores definition information.
(defstruct lexical-variable
  name
  definition-point
  ignore
  dynamic-extent
  (use-count 0)
  (write-count 0)
  used-in)

(defstruct tagbody-information
  definition-point
  go-tags)

(defstruct go-tag
  name
  tagbody
  (use-count 0)
  used-in)

(defun run-optimizers (form)
  (dotimes (i 5 (progn (warn 'simple-style-warning
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
      (setf form (cp-form (detect-uses form)))
      (setf form (simp-form (detect-uses form)))
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
	      ((go) (decf (go-tag-use-count (second form))))
	      ((if) (implicit-progn (cdr form)))
	      ((let)
	       (dolist (b (second form))
		 (flush-form (second b)))
	       (implicit-progn (cddr form)))
	      ((load-time-value) (error "TODO: load-time-value"))
	      ((multiple-value-bind) (implicit-progn (cddr form)))
	      ((multiple-value-call) (implicit-progn (cdr form)))
	      ((multiple-value-prog1) (implicit-progn (cdr form)))
	      ((progn) (implicit-progn (cdr form)))
	      ((progv) (implicit-progn (cdr form)))
	      ((quote))
	      ((return-from)
	       (decf (lexical-variable-use-count (second form)))
	       (flush-form (third form)))
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
     (implicit-progn (lambda-information-body form))))))

(defun copy-form (form &optional replacements)
  "Completely copy a form, incrementing use-counts."
  (labels ((fix (thing)
	     (let ((r (assoc thing replacements)))
	       (if r (cdr r) thing)))
	   (implicit-progn (forms)
	     (mapcar (lambda (x) (copy-form x replacements)) forms))
	   (copy-variable (var)
	     (if (lexical-variable-p var)
		 (let ((new (make-lexical-variable :name (lexical-variable-name var)
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
		 `(go ,tag)))
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
	      ((load-time-value) (error "TODO: load-time-value"))
	      ((multiple-value-bind)
               `(multiple-value-bind ,(mapcar #'copy-variable (second form))
                    ,@(implicit-progn (cddr form))))
	      ((multiple-value-call) `(multiple-value-call ,@(implicit-progn (cdr form))))
	      ((multiple-value-prog1) `(multiple-value-prog1 ,@(implicit-progn (cdr form))))
	      ((progn) `(progn ,@(implicit-progn (cdr form))))
	      ((progv) `(progv ,@(implicit-progn (cdr form))))
	      ((quote) form)
	      ((return-from)
	       (let ((var (fix (second form))))
		 (incf (lexical-variable-use-count var))
		 (pushnew *current-lambda* (lexical-variable-used-in var))
		 `(return-from ,var ,(copy-form (third form) replacements))))
	      ((setq)
	       (let ((var (fix (second form))))
		 (incf (lexical-variable-use-count var))
		 (incf (lexical-variable-write-count var))
		 (pushnew *current-lambda* (lexical-variable-used-in var))
		 `(setq ,var ,(copy-form (third form) replacements))))
	      ((tagbody)
	       (let ((info (make-tagbody-information :definition-point (fix (tagbody-information-definition-point (second form))))))
		 (push (cons (second form) info) replacements)
		 (dolist (tag (tagbody-information-go-tags (second form)))
		   (let ((new-tag (make-go-tag :name (go-tag-name tag))))
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
       (let* ((info (make-lambda-information :name (lambda-information-name form)
					     :docstring (lambda-information-docstring form)
					     :lambda-list (lambda-information-lambda-list form)
                                             :enable-keys (lambda-information-enable-keys form)
                                             :allow-other-keys (lambda-information-allow-other-keys form)))
	      (*current-lambda* info))
	 (push (cons form info) replacements)
	 (setf (lambda-information-required-args info)
	       (mapcar #'copy-variable (lambda-information-required-args form)))
	 (setf (lambda-information-optional-args info)
	       (mapcar (lambda (x)
			 (list (copy-variable (first x))
			       (copy-form (second x))
			       (when (third x)
				 (copy-form (third x)))))
		       (lambda-information-optional-args form)))
	 (when (lambda-information-rest-arg form)
	   (setf (lambda-information-rest-arg info)
		 (copy-variable (lambda-information-rest-arg form))))
         (setf (lambda-information-key-args info)
	       (mapcar (lambda (x)
			 (list (copy-variable (second (first x)))
			       (copy-form (second x))
			       (when (third x)
				 (copy-form (third x)))))
		       (lambda-information-key-args form)))
	 (setf (lambda-information-body info)
	       (implicit-progn (lambda-information-body form)))
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
	       (incf (go-tag-use-count (second form)))
	       (pushnew *current-lambda* (go-tag-used-in (second form))))
	      ((if) (implicit-progn (cdr form)))
	      ((let)
	       (dolist (b (second form))
		 (reset-var (first b)))
	       (dolist (b (second form))
		 (detect-uses (second b)))
	       (implicit-progn (cddr form)))
	      ((load-time-value) (error "TODO: load-time-value"))
	      ((multiple-value-bind)
               (dolist (v (second form))
                 (reset-var v))
               (implicit-progn (cddr form)))
	      ((multiple-value-call) (implicit-progn (cdr form)))
	      ((multiple-value-prog1) (implicit-progn (cdr form)))
	      ((progn) (implicit-progn (cdr form)))
	      ((progv) (implicit-progn (cdr form)))
	      ((quote))
	      ((return-from)
	       (pushnew *current-lambda* (lexical-variable-used-in (second form)))
	       (incf (lexical-variable-use-count (second form)))
	       (detect-uses (third form)))
	      ((setq)
	       (pushnew *current-lambda* (lexical-variable-used-in (second form)))
	       (incf (lexical-variable-use-count (second form)))
	       (incf (lexical-variable-write-count (second form)))
	       (detect-uses (third form)))
	      ((tagbody)
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
       (implicit-progn (lambda-information-body form))))))
  form)

(defun variable-name (var)
  (if (symbolp var)
      var
      (lexical-variable-name var)))

(defun lower-key-arguments* (body rest keys allow-other-keys)
  (let* ((values (mapcar (lambda (x)
                           (make-lexical-variable :name (gensym (string (variable-name (cadar x))))
                                                  :definition-point *current-lambda*))
                         keys))
         (suppliedp (mapcar (lambda (x)
                              (make-lexical-variable :name (if (third x)
                                                               (gensym (string (variable-name (third x))))
                                                               (gensym))
                                                     :definition-point *current-lambda*))
                            keys))
         (itr (make-lexical-variable :name (gensym)
                                     :definition-point *current-lambda*))
         (current-keyword (make-lexical-variable :name (gensym)
                                                 :definition-point *current-lambda*))
         (tb (make-tagbody-information :definition-point *current-lambda*))
         (head-tag (make-go-tag :name (gensym "HEAD") :tagbody tb))
         (test-tag (make-go-tag :name (gensym "TEST") :tagbody tb)))
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
                     (t `(error '"Unknown &KEY argument ~S. Expected one of ~S."
                                ,current-keyword
                                ',(mapcar 'caar keys)))))
             (create-key-let-body (key-args values suppliedp)
               (cond (key-args
                      `(let ((,(second (first (first key-args))) (if ,(first suppliedp)
                                                                     ,(first values)
                                                                     ,(second (first key-args)))))
                         ,(create-key-let-body (rest key-args) (rest values) (rest suppliedp))))
                     (t `(progn ,@body)))))
      `((let ,(append (mapcar (lambda (x) (list x ''nil)) values)
                      (mapcar (lambda (x) (list x ''nil)) suppliedp)
                      (list (list itr (if (symbolp rest) `(symbol-value ,rest) rest))))
          (tagbody ,tb
             (go ,test-tag)
             ,head-tag
             (if (null (cdr ,itr))
                 (error '"Odd number of &KEY arguments.")
                 'nil)
             (let ((,current-keyword (car ,itr)))
               ,(create-key-test-list keys values suppliedp))
             (setq ,itr (cddr ,itr))
             ,test-tag
             (if ,itr
                 (go ,head-tag)
                 'nil))
          ,(create-key-let-body keys values suppliedp))))))

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
	      ((load-time-value) (error "TODO: load-time-value"))
	      ((multiple-value-bind) (implicit-progn (cddr form)))
	      ((multiple-value-call) (implicit-progn (cdr form)))
	      ((multiple-value-prog1) (implicit-progn (cdr form)))
	      ((progn) (implicit-progn (cdr form)))
	      ((progv) (implicit-progn (cdr form)))
	      ((quote))
	      ((return-from) (lower-keyword-arguments (third form)))
	      ((setq) (lower-keyword-arguments (third form)))
	      ((tagbody)
	       (dolist (i (cddr form))
		 (unless (go-tag-p i)
		   (lower-keyword-arguments i))))
	      ((the) (lower-keyword-arguments (third form)))
	      ((unwind-protect) (lower-keyword-arguments (cdr form)))
	    (t (implicit-progn (cdr form)))))
    (lexical-variable)
    (lambda-information
     (let ((*current-lambda* form))
       (when (lambda-information-enable-keys form)
         (unless (lambda-information-rest-arg form)
           ;; Add in a &REST arg and make it dynamic-extent.
           (setf (lambda-information-rest-arg form)
                 (make-lexical-variable :name (gensym "REST")
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
       (implicit-progn (lambda-information-body form))))))
  form)
