;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

(defun parse-lambda (lambda)
  "Parse a lambda expression, extracting the body, lambda-list, declare expressions, any name declared with lambda-name and a docstring."
  (when (or (not (eq (first lambda) 'lambda))
	    (not (rest lambda)))
    (error "Lambda expression does not begin with LAMBDA."))
  (do ((lambda-list (second lambda))
       (declares '())
       (name nil)
       (docstring nil)
       (body (cddr lambda) (cdr body)))
      ((or (null body)
	   ;; A string at the end of forms must always be treated
	   ;; as a body form, not a docstring.
	   (and (stringp (car body))
		(null (cdr body)))
	   ;; Stop when (car body) is not a string and is not a declare form.
	   (and (not (stringp (car body)))
		(not (and (consp (car body))
			  (eq 'declare (caar body))))))
       (values body lambda-list (nreverse declares) name docstring))
    (if (stringp (car body))
	(unless docstring
	  (setf docstring (car body)))
	;; Dump the bodies of each declare form into one list and scan
	;; for a lambda-name declaration.
	(dolist (decl (cdar body))
	  (push decl declares)
	  (when (and (eq (car decl) 'system:lambda-name)
		     (cdr decl)
		     (not name))
	    (setf name (cadr decl)))))))

(defun parse-let-binding (binding)
  (etypecase binding
    (symbol (values binding nil))
    (cons (destructuring-bind (name &optional init-form)
	      binding
	    (values name init-form)))))

(defun pick-variables (type declares)
  "Extract a list of variables from DECLARES."
  (let ((vars '()))
    (dolist (decl declares)
      (when (eq (first decl) type)
	(setf vars (union vars (rest decl)))))
    vars))

;;; Currently forwards: SPECIAL, IGNORE, IGNORABLE and DYNAMIC-EXTENT.
(defun forward-declares (variables declares)
  (labels ((test (x) (find x variables)))
    (let ((special (pick-variables 'special declares))
	  (ignore (pick-variables 'ignore declares))
	  (ignorable (pick-variables 'ignorable declares))
	  (dynamic-extent (pick-variables 'dynamic-extent declares)))
      `(declare (special ,@(remove-if-not #'test special))
		(ignore ,@(remove-if-not #'test ignore))
		(ignorable ,@(remove-if-not #'test ignorable))
		(dynamic-extent ,@(remove-if-not #'test dynamic-extent))))))

(defun lower-aux-arguments (body aux declares)
  `(let* ,aux
     ,(forward-declares (mapcar 'car aux) declares)
     ,@body))

(defun declared-as-p (what name declares)
  (dolist (decl declares nil)
    (when (and (eql what (first decl))
	       (find name (rest decl)))
      (return t))))

(defun check-variable-usage (variable)
  "Warn if VARIABLE was not used and not declared IGNORE or IGNORABLE."
  (when (and (not (lexical-variable-ignore variable))
	     (= (lexical-variable-use-count variable) 0))
    (warn 'sys.int::simple-style-warning
	  :format-control "Unused variable ~S."
	  :format-arguments (list (lexical-variable-name variable)))))

(defun make-variable (name declares)
  (if (or (sys.int::variable-information name)
	  (declared-as-p 'special name declares))
      name
      (make-lexical-variable :name name
			     :definition-point *current-lambda*
			     :ignore (cond ((declared-as-p 'ignore name declares) t)
					   ((declared-as-p 'ignorable name declares) :maybe))
			     :dynamic-extent (declared-as-p 'dynamic-extent name declares))))

(defun pass1-lambda (lambda env)
  "Perform macroexpansion, alpha-conversion, and canonicalization on LAMBDA."
  (multiple-value-bind (body lambda-list declares name docstring)
      (parse-lambda lambda)
    (multiple-value-bind (required optional rest enable-keys keys allow-other-keys aux)
	(sys.int::parse-ordinary-lambda-list lambda-list)
      (let* ((info (make-lambda-information :name name
					    :docstring docstring
					    :lambda-list lambda-list
                                            :enable-keys enable-keys
                                            :allow-other-keys allow-other-keys
                                            :plist (list :declares declares)))
	     (*current-lambda* info)
	     (bindings (list :bindings))
	     (env (list* bindings env)))
	;; Lower &AUX arguments so they don't have to be dealt with.
	(when aux
	  (setf body (list (lower-aux-arguments body aux declares))))
	;; Add required, optional and rest arguments to the environment & lambda.
	(labels ((add-var (name)
		   (let ((var (make-variable name declares)))
		     (push (cons name var) (cdr bindings))
		     var)))
	  (setf (lambda-information-required-args info) (mapcar #'add-var required))
	  (setf (lambda-information-optional-args info)
		(mapcar (lambda (opt)
			  (let ((var (first opt))
				(init-form (pass1-form (second opt) env))
				(suppliedp (third opt)))
			    (list (add-var var) init-form (when suppliedp
                                                            (add-var suppliedp)))))
			optional))
	  (when rest
	    (setf (lambda-information-rest-arg info) (add-var rest)))
          (setf (lambda-information-key-args info)
                (mapcar (lambda (key)
                          (let ((keyword (first (first key)))
                                (var (second (first key)))
                                (init-form (pass1-form (second key) env))
                                (suppliedp (third key)))
                            (list (list keyword (add-var var))
                                  init-form
                                  (when suppliedp (add-var suppliedp)))))
                        keys)))
	;; Add special variables to the environment.
	;; NOTE: Does not quite work properly with &aux arguments.
	;; Special declarations will apply inside their init-forms.
	(let* ((lambda-variables (append required
					 (mapcar #'car optional)
					 (delete 'nil (mapcar 'caddr optional))
					 (when rest
					   (list rest))
					 (mapcar #'cadar keys)
					 (delete 'nil (mapcar 'caddr keys))
					 (mapcar #'car aux)))
	       (specials (remove-if (lambda (x) (find x lambda-variables))
				    (pick-variables 'special declares)))
	       (env (cons (list* :bindings (mapcar (lambda (x) (cons x x)) specials)) env)))
	  (setf (lambda-information-body info) (pass1-implicit-progn body env)))
	;; Perform (un)used-variable warnings.
	(dolist (var (lambda-information-required-args info))
	  (when (lexical-variable-p var)
	    (check-variable-usage var)))
	(dolist (opt (lambda-information-optional-args info))
	  (when (lexical-variable-p (first opt))
	    (check-variable-usage (first opt)))
	  (when (lexical-variable-p (third opt))
	    (check-variable-usage (third opt))))
	(when (lexical-variable-p (lambda-information-rest-arg info))
	  (check-variable-usage (lambda-information-rest-arg info)))
        (dolist (opt (lambda-information-key-args info))
	  (when (lexical-variable-p (second (first opt)))
	    (check-variable-usage (second (first opt))))
	  (when (lexical-variable-p (third opt))
	    (check-variable-usage (third opt))))
	info))))

(defun pass1-implicit-progn (forms env)
  (mapcar (lambda (x) (pass1-form x env)) forms))

(defun find-variable (symbol env &optional allow-symbol-macros)
  "Find SYMBOL in ENV, returning SYMBOL if it's special or if it's not found."
  ;; FIXME: Should check for symbols defined with DEFINE-SYMBOL-MACRO.
  (dolist (e env symbol)
    (when (eq (first e) :bindings)
      (let ((v (assoc symbol (rest e))))
	(when v
	  (return (cdr v)))))
    (when (eql (first e) :symbol-macros)
      (let ((v (assoc symbol (rest e))))
        (when v
          (unless allow-symbol-macros
            (error "Symbol-macro not allowed here."))
          (return v))))))

(defun find-function (name env)
  "Find the function named by NAME in ENV, returning NAME if not found."
  (dolist (e env name)
    (when (eq (first e) :functions)
      (let ((v (assoc name (rest e) :test #'equal)))
	(when v
	  (return (cdr v)))))))

(defun expand-constant-variable (symbol)
  "Expand a constant variable, returning the quoted value or returning NIL if the variable is not a constant."
  (when (eql (sys.int::variable-information symbol) :constant)
    (ignore-errors `(quote ,(symbol-value symbol)))))

(defun compiler-macroexpand-1 (form env)
  "Expand one level of macros and compiler macros."
  ;; Detect (funcall #'name ..) and call the appropriate compiler macro.
  (let* ((name (if (and (eq (first form) 'funcall)
			(consp (second form))
			(eq (first (second form)) 'function))
		   (second (second form))
		   (first form)))
	 (fn (when (or (symbolp name)
		       (and (consp name)
			    (eq (first name) 'setf)))
	       (compiler-macro-function name env))))
    (when fn
      (let ((expansion (funcall *macroexpand-hook* fn form env)))
        (when (not (eq expansion form))
          (return-from compiler-macroexpand-1
            (values expansion t))))))
  (let ((fn (macro-function (first form) env)))
    (if fn
	(values (funcall *macroexpand-hook* fn form env) t)
	(values form nil))))

(defun pass1-form (form env)
  (cond
    ((symbolp form)
     ;; Expand symbol macros.
     (multiple-value-bind (expansion expandedp)
         (macroexpand-1 form env)
       (if expandedp
           (pass1-form expansion env)
           (let ((var (find-variable form env)))
             (if (symbolp var)
                 ;; Replace constants with their quoted values.
                 (or (expand-constant-variable form)
                     ;; And replace special variable with calls to symbol-value.
                     (pass1-form `(symbol-value ',var) env))
                 (progn
                   (when (eq (lexical-variable-ignore var) 't)
                     (warn 'sys.int::simple-style-warning
                           :format-control "Reading ignored variable ~S."
                           :format-arguments (list (lexical-variable-name var))))
                   (incf (lexical-variable-use-count var))
                   (pushnew *current-lambda* (lexical-variable-used-in var))
                   var))))))
    ;; Self-evaluating forms are quoted.
    ((not (consp form))
     `(quote ,form))
    ;; ((lambda ...) ...) is converted to (funcall #'(lambda ...) ...)
    ((and (consp (first form))
	  (eq (first (first form)) 'lambda))
     (pass1-form `(funcall (function ,(first form)) ,@(rest form)) env))
    (t (case (first form)
	 ;; Check for special forms before macroexpansion.
	 ((block) (pass1-block form env))
	 ((catch) (pass1-catch form env))
	 ((eval-when) (pass1-eval-when form env))
	 ((flet) (pass1-flet form env))
	 ((function) (pass1-function form env))
	 ((go) (pass1-go form env))
	 ((if) (pass1-if form env))
	 ((labels) (pass1-labels form env))
	 ((let) (pass1-let form env))
	 ((let*) (pass1-let* form env))
	 ((load-time-value) (pass1-load-time-value form env))
	 ((locally) (pass1-locally form env))
	 ((macrolet) (pass1-macrolet form env))
	 ((multiple-value-call) (pass1-multiple-value-call form env))
	 ((multiple-value-prog1) (pass1-multiple-value-prog1 form env))
	 ((progn) (pass1-progn form env))
	 ((progv) (pass1-progv form env))
	 ((quote) (pass1-quote form env))
	 ((return-from) (pass1-return-from form env))
	 ((setq) (pass1-setq form env))
	 ((symbol-macrolet) (pass1-symbol-macrolet form env))
	 ((tagbody) (pass1-tagbody form env))
	 ((the) (pass1-the form env))
	 ((throw) (pass1-throw form env))
	 ((unwind-protect) (pass1-unwind-protect form env))
	 (t (multiple-value-bind (expansion expanded-p)
		(compiler-macroexpand-1 form env)
	      (if expanded-p
		  (pass1-form expansion env)
		  (pass1-function-form form env))))))))

(defun function-name-p (thing)
  (ignore-errors
    (or (symbolp thing)
        (typep thing '(cons (eql setf) (cons symbol null))))))

(defun pass1-function-form (form env)
  (let ((fn (find-function (first form) env))
	(args (pass1-implicit-progn (rest form) env)))
    (cond ((lexical-variable-p fn)
           ;; Lexical function.
	   (list* 'funcall fn args))
	  (t ;; Top-level function.
           (list* fn args)))))

(defun pass1-block (form env)
  (destructuring-bind (name &body forms) (cdr form)
    (let ((var (make-block-information :name name
				       :definition-point *current-lambda*)))
      `(block ,var ,@(pass1-implicit-progn forms (cons (list :block name var) env))))))

(defun pass1-catch (form env)
  (destructuring-bind (tag &body body) (cdr form)
    (pass1-form `(sys.int::%catch ,tag
                                  (flet ((%%catch-body%% () (progn ,@body)))
                                    (declare (dynamic-extent (function %%catch-body%%)))
                                    (function %%catch-body%%)))
                env)))

(defun pass1-eval-when (form env)
  (destructuring-bind (situations &body forms) (cdr form)
    (multiple-value-bind (compile load eval)
	(sys.int::parse-eval-when-situation situations)
      (declare (ignore compile load))
      (if eval
	  (pass1-form `(progn ,@forms) env)
	  ''nil))))

(defun frob-flet-function (fn)
  "Turn an FLET/LABELS function definition into a symbolic name, a lexical variable and a lambda expression."
  (multiple-value-bind (body declares)
      (parse-declares (cddr fn))
    ;; FIXME: docstring permitted here.
    (let* ((name (first fn))
	   (var (make-lexical-variable :name name
				       :definition-point *current-lambda*)))
      (values name var `(lambda ,(second fn)
                          (declare (system:lambda-name ,name)
                                   ,@declares)
                          (block ,(if (consp name)
                                      (second name)
                                      name)
                            ,@body))))))

(defun function-declared-dynamic-extent-p (name declares)
  "Look for a (dynamic-extent (function NAME)) declaration for NAME in DECLARES."
  (dolist (dec declares)
    (when (eql (first dec) 'dynamic-extent)
      (dolist (item (rest dec))
        (when (and (typep item '(cons (eql function)
                                 (cons t null)))
                   (equal (second item) name))
          (return-from function-declared-dynamic-extent-p t))))))

(defun pass1-flet (form env)
  (destructuring-bind (functions &body forms) (cdr form)
    (multiple-value-bind (body declares)
	(parse-declares forms)
      (let ((bindings (list :functions)))
	`(let ,(mapcar (lambda (x)
			 (multiple-value-bind (sym var lambda)
			     (frob-flet-function x)
			   (push (cons sym var) (cdr bindings))
                           (let ((lambda (pass1-lambda lambda env)))
                             (when (function-declared-dynamic-extent-p sym declares)
                               (setf (getf (lambda-information-plist lambda) 'declared-dynamic-extent) t))
                             (list var lambda))))
		       functions)
	   ;; TODO: special vars.
	   ,@(pass1-implicit-progn body (cons bindings env)))))))

(defun pass1-function (form env)
  (destructuring-bind (name) (cdr form)
    (if (and (consp name)
	     (eq (first name) 'lambda))
	(pass1-lambda name env)
	(let* ((var (find-function name env)))
	  (cond ((lexical-variable-p var)
                 ;; Lexical function.
                 (incf (lexical-variable-use-count var))
                 (pushnew *current-lambda* (lexical-variable-used-in var))
                 var)
                (t ;; Top-level function.
                 `(function ,name)))))))

(defun pass1-go (form env)
  (destructuring-bind (tag) (cdr form)
    (dolist (e env (error "GO refers to unknown tag ~S." tag))
      (when (eq (car e) :tagbody)
	(let ((x (assoc tag (cddr e))))
	  (when x
	    (incf (go-tag-use-count (cdr x)))
	    (pushnew *current-lambda* (go-tag-used-in (cdr x)))
	    (return `(go ,(cdr x) ,(go-tag-tagbody (cdr x))))))))))

(defun pass1-if (form env)
  (destructuring-bind (test then &optional else) (cdr form)
    `(if ,(pass1-form test env)
	 ,(pass1-form then env)
	 ,(pass1-form else env))))

(defun pass1-labels (form env)
  (destructuring-bind (functions &body forms) (cdr form)
    (multiple-value-bind (body declares)
	(parse-declares forms)
      ;; Generate variables & lambda expressions for each function.
      (let* ((raw-bindings (mapcar (lambda (x)
				     (multiple-value-list (frob-flet-function x)))
				   functions))
	     (env (cons (list* :functions (mapcar (lambda (x)
						    (cons (first x) (second x)))
						  raw-bindings))
			env)))
	`(let ,(mapcar (lambda (x)
                         (let ((lambda (pass1-lambda (third x) env)))
                           (when (function-declared-dynamic-extent-p (first x) declares)
                             (setf (getf (lambda-information-plist lambda) 'declared-dynamic-extent) t))
                           (list (second x) lambda)))
		       raw-bindings)
	   ;; TODO: declares
	   ,@(pass1-implicit-progn body env))))))

(defun pass1-let (form env)
  (destructuring-bind (bindings &body forms) (cdr form)
    (multiple-value-bind (body declares)
	(parse-declares forms)
      (let* ((variables (mapcar (lambda (binding)
				  (multiple-value-bind (name init-form)
				      (parse-let-binding binding)
				    (let ((var (make-variable name declares)))
					(list name init-form var))))
				bindings)))
	`(let ,(mapcar (lambda (b)
			 (list (third b) (pass1-form (second b) env)))
		       variables)
	   ,@(pass1-implicit-progn body (cons (append (list :bindings)
						      (mapcar (lambda (b)
								(cons (first b) (third b)))
							      variables)
						      (mapcar (lambda (x) (cons x x))
							      (remove-if (lambda (x) (find x variables :key #'first))
									 (pick-variables 'special declares))))
					      env)))))))

(defun pass1-let* (form env)
  (destructuring-bind (bindings &body forms) (cdr form)
    (multiple-value-bind (body declares)
	(parse-declares forms)
      (let* ((result (cons ''nil nil))
	     (inner result)
	     (var-names '()))
	(dolist (b bindings)
	  (multiple-value-bind (name init-form)
	      (parse-let-binding b)
	    (push name var-names)
	    (let ((var (make-variable name declares)))
	      (setf (car inner) (list 'let `((,var ,(pass1-form init-form env))) ''nil)
		    inner (cddar inner)
		    env (cons (list :bindings (cons name var)) env)))))
	(setf (car inner) (pass1-form `(progn ,@body) (cons (list* :bindings (mapcar (lambda (x) (cons x x))
										     (remove-if (lambda (x) (find x var-names))
												(pick-variables 'special declares))))
							    env)))
	(car result)))))

;;; Not really sure how to do this one.
;;;(defun pass1-load-time-value (form env)
;;;  (declare (ignore env))
;;;  (destructuring-bind (form &optional read-only-p) (cdr form)
;;;    `(load-time-value ,(pass1-form form nil) ,read-only-p)))

(defun pass1-locally-body (forms env)
  (multiple-value-bind (body declares)
      (parse-declares forms)
    (let ((env (cons (list :bindings) env)))
      (dolist (s (pick-variables 'special declares))
	(push (cons s s) (rest (first env))))
      (pass1-form `(progn ,@body) env))))

(defun pass1-locally (form env)
  (pass1-locally-body (cdr form) env))

(defun hack-macrolet-definition (def)
  "Turn a MACROLET function definition into a name and expansion function."
  (destructuring-bind (name lambda-list &body forms) def
    ;; FIXME: docstring permitted here.
    (let ((whole (gensym "WHOLE"))
          (env (gensym "ENV")))
      (multiple-value-bind (body declares)
          (parse-declares forms)
        (cons name (eval `(lambda (,whole ,env)
                            (declare (ignorable ,whole ,env)
                                     (system:lambda-name ,name))
                            (destructuring-bind ,lambda-list (cdr ,whole)
                              (declare ,@declares)
                              (block ,name ,@body)))))))))

(defun pass1-macrolet (form env)
  (destructuring-bind (definitions &body body) (cdr form)
    (let ((env (list* (list* :macros (mapcar 'hack-macrolet-definition definitions)) env)))
      (pass1-locally-body body env))))

(defun pass1-multiple-value-call (form env)
  (destructuring-bind (function-form &rest forms) (cdr form)
    ;; Simplify M-V-CALL based on the number of forms.
    (case (length forms)
      (0 ; No forms, convert to funcall.
       `(funcall ,(pass1-form function-form env)))
      (1 ; One form, transform as-is.
       `(multiple-value-call ,(pass1-form function-form env)
          ,(pass1-form (first forms) env)))
      (t ; Many forms, simplify.
       (pass1-form `(apply ,function-form
                           (append ,@(loop for f in forms
                                        collect `(multiple-value-call #'list ,f))))
                   env)))))

(defun pass1-multiple-value-prog1 (form env)
  (destructuring-bind (first-form &body forms) (cdr form)
    (if forms
	`(multiple-value-prog1 ,(pass1-form first-form env)
	   ,@(pass1-implicit-progn forms env))
	(pass1-form first-form env))))

;;; Never generate empty PROGNs and avoid generating PROGNs with just one form.
(defun pass1-progn (form env)
  (cond ((null (cdr form))
	 ''nil)
	((null (cddr form))
	 (pass1-form (cadr form) env))
	(t `(progn ,@(pass1-implicit-progn (rest form) env)))))

;; Turn PROGV into a call to %PROGV.
(defun pass1-progv (form env)
  (destructuring-bind (symbols values &body forms) (cdr form)
    (pass1-form `(sys.int::%progv ,symbols ,values
                                  #'(lambda () (progn ,@forms)))
                env)))

(defun pass1-quote (form env)
  (declare (ignore env))
  (destructuring-bind (thing) (cdr form)
    (declare (ignore thing))
    form))

(defun pass1-return-from (form env)
  (destructuring-bind (name &optional result) (cdr form)
    (dolist (e env (error "RETURN-FROM refers to unknown block ~S." name))
      (when (and (eq (first e) :block)
		 (eq (second e) name))
	(incf (lexical-variable-use-count (third e)))
	(pushnew *current-lambda* (lexical-variable-used-in (third e)))
	(return `(return-from ,(third e) ,(pass1-form result env) ,(third e)))))))

(defun pass1-setq (form env)
  (do ((i (cdr form) (cddr i))
       (forms '()))
      ((endp i)
       (cond ((null forms)
	      ''nil)
	     ((null (rest forms))
	      (first forms))
	     (t `(progn ,@(nreverse forms)))))
    (when (null (cdr i))
      (error "Odd number of arguments to SETQ."))
    (let ((var (find-variable (first i) env t))
	  (val (second i)))
      (etypecase var
        (symbol
         (push (pass1-form `(funcall #'(setf symbol-value) ,val ',var) env) forms))
        (lexical-variable
         (when (eq (lexical-variable-ignore var) 't)
           (warn 'sys.int::simple-style-warning
                 :format-control "Writing ignored variable ~S."
                 :format-arguments (list (lexical-variable-name var))))
         (incf (lexical-variable-use-count var))
         (incf (lexical-variable-write-count var))
         (pushnew *current-lambda* (lexical-variable-used-in var))
         (push `(setq ,var ,(pass1-form val env)) forms))
        (cons
         ;; Symbol macro.
         (push (pass1-form `(setf ,(second var) ,val) env) forms))))))

(defun pass1-symbol-macrolet (form env)
  (destructuring-bind (definitions &body body) (cdr form)
    (assert (every (lambda (def)
                     (and (= (list-length def) 2)
                          (symbolp (first def))))
                   definitions)
            ()
            "Bad SYMBOL-MACROLET definition.")
    (let ((env (list* (list* :symbol-macros definitions) env)))
      (pass1-locally-body body env))))

(defun pass1-tagbody (form env)
  (let* ((tb (make-tagbody-information :name (gensym "TAGBODY") :definition-point *current-lambda*))
	 (env (cons (list* :tagbody tb (mapcan (lambda (stmt)
						 (when (or (symbolp stmt) (integerp stmt))
						   (let ((tag (make-go-tag :name stmt :tagbody tb)))
						     (push tag (tagbody-information-go-tags tb))
						     (list (cons stmt tag)))))
					       (rest form)))
		    env)))
    `(tagbody ,tb
	,@(mapcar (lambda (stmt)
		    (if (or (symbolp stmt) (integerp stmt))
			(cdr (assoc stmt (cddr (first env))))
			(pass1-form stmt env)))
		  (rest form)))))

(defun pass1-the (form env)
  (destructuring-bind (value-type form) (cdr form)
    `(the ,value-type ,(pass1-form form env))))

(defun pass1-throw (form env)
  (destructuring-bind (tag result) (cdr form)
    (pass1-form `(sys.int::%throw ,tag (multiple-value-list ,result)) env)))

;;; Translate (unwind-protect form . cleanup-forms) to
;;; (unwind-protect form (lambda () . cleanup-forms)).
(defun pass1-unwind-protect (form env)
  (destructuring-bind (protected-form &body cleanup-forms) (cdr form)
    (if cleanup-forms
        (list 'unwind-protect
              (pass1-form protected-form env)
              (pass1-lambda `(lambda ()
                               (progn ,@cleanup-forms))
                            env))
	(pass1-form protected-form env))))
