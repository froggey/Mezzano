;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

(defun parse-lambda (lambda)
  "Parse a lambda expression, extracting the body, lambda-list, declare expressions, any name declared with lambda-name and a docstring."
  (when (or (not (eq (first lambda) 'lambda))
            (not (rest lambda)))
    (error-program-error "Lambda expression does not begin with LAMBDA."))
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
          (when (and (eql (car decl) 'sys.int::lambda-name)
                     (cdr decl)
                     (not name))
            (setf name (cadr decl)))))))

(defun parse-let-binding (binding)
  (etypecase binding
    (symbol (values binding nil))
    (cons (destructuring-bind (name &optional init-form)
              binding
            (check-type name symbol)
            (values name init-form)))))

(defun pick-variables (type declares)
  "Extract a list of variables from DECLARES."
  (let ((vars '()))
    (dolist (decl declares)
      (when (eq (first decl) type)
        (setf vars (union vars (rest decl)))))
    vars))

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
      (make-instance 'special-variable :name name)
      (make-instance 'lexical-variable
                     :name name
                     :definition-point *current-lambda*
                     :ignore (cond ((declared-as-p 'ignore name declares) t)
                                   ((declared-as-p 'ignorable name declares) :maybe))
                     :dynamic-extent (declared-as-p 'dynamic-extent name declares))))

(defun pass1-lambda (lambda env)
  "Perform macroexpansion, alpha-conversion, and canonicalization on LAMBDA."
  (multiple-value-bind (body lambda-list declares name docstring)
      (parse-lambda lambda)
    (multiple-value-bind (required optional rest enable-keys keys allow-other-keys aux fref-arg closure-arg count-arg)
        (sys.int::parse-ordinary-lambda-list lambda-list)
      (let* ((optimize-env (extend-environment env :declarations declares))
             (info (make-instance 'lambda-information
                                  :name (or name
                                            `(lambda :in ,(or (and (boundp '*current-lambda*)
                                                                   *current-lambda*
                                                                   (lambda-information-name *current-lambda*))
                                                              (when *compile-file-pathname*
                                                                (princ-to-string *compile-file-pathname*)))))
                                  :docstring docstring
                                  :lambda-list lambda-list
                                  :enable-keys enable-keys
                                  :allow-other-keys allow-other-keys
                                  :optimize (optimize-qualities-in-environment optimize-env)
                                  :plist (list :declares declares)))
             (*current-lambda* info)
             (aux-bindings '()))
        ;; Add required, optional and rest arguments to the environment & lambda.
        (labels ((add-var (name)
                   (let ((var (make-variable name declares)))
                     (setf env (extend-environment env :variables (list (list name var))))
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
                        keys))
          (setf aux-bindings (mapcar (lambda (binding)
                                       (let ((var (first binding))
                                             (init-form (pass1-form (second binding) env)))
                                         (list (add-var var) init-form)))
                                     aux))
          (when fref-arg
            (setf (lambda-information-fref-arg info) (add-var fref-arg)))
          (when closure-arg
            (setf (lambda-information-closure-arg info) (add-var closure-arg)))
          (when count-arg
            (setf (lambda-information-count-arg info) (add-var count-arg))))
        ;; Add declarations to the environment.
        (let* ((env (extend-environment env
                                        :variables (mapcar (lambda (x)
                                                             (list x (make-instance 'special-variable :name x)))
                                                           (pick-variables 'special declares))
                                        :declarations declares))
               ;; Process lambda body.
               (body-ast (pass1-form `(progn ,@body) env))
               ;; Actually bind &AUX variables, now that the body has been processed.
               (body-with-aux (if aux-bindings
                                  (ast `(let ,aux-bindings ,body-ast))
                                  body-ast)))
          (setf (lambda-information-body info) body-with-aux))
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
        (when (lexical-variable-p (lambda-information-fref-arg info))
          (check-variable-usage (lambda-information-fref-arg info)))
        (when (lexical-variable-p (lambda-information-closure-arg info))
          (check-variable-usage (lambda-information-closure-arg info)))
        (when (lexical-variable-p (lambda-information-count-arg info))
          (check-variable-usage (lambda-information-count-arg info)))
        info))))

(defun pass1-implicit-progn (forms env)
  (mapcar (lambda (x) (pass1-form x env)) forms))

(defun find-variable (symbol env &optional allow-symbol-macros)
  "Find SYMBOL in ENV, returning a SPECIAL-VARIABLE if it's special or if it's not found."
  (let ((var (lookup-variable-in-environment symbol env)))
    (cond ((typep var 'symbol-macro)
           (when (not allow-symbol-macros)
             (error-program-error "Symbol-macro not allowed here."))
           (list symbol (symbol-macro-expansion var)))
          (t
           (when (and (typep var 'special-variable)
                      (special-variable-implicitly-declared var))
             (warn 'sys.int::simple-style-warning
                   :format-control "Undefined variable ~S."
                   :format-arguments (list (name var))))
           var))))

(defun find-function (name env)
  "Find the function named by NAME in ENV, returning NAME if not found."
  (let ((result (lookup-function-in-environment name env)))
    (cond ((typep result 'top-level-function)
           name)
          (t
           result))))

(defun expand-constant-variable (symbol)
  "Expand a constant variable, returning the quoted value or returning NIL if the variable is not a constant."
  (when (eql (sys.int::variable-information symbol) :constant)
    (ignore-errors (make-instance 'ast-quote :value (symbol-value symbol)))))

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
    (when (and fn
               (not (eql (inline-info-in-environment name env) 'notinline)))
      (let ((expansion (funcall *macroexpand-hook* fn form env)))
        (when (not (eq expansion form))
          (return-from compiler-macroexpand-1
            (values expansion t))))))
  (let ((fn (macro-function (first form) env)))
    (if fn
        (values (funcall *macroexpand-hook* fn form env) t)
        (values form nil))))

(defun pass1-variable-inner (form env)
  (let ((var (find-variable form env)))
    (etypecase var
      (special-variable
       ;; Replace constants with their quoted values.
       (or (expand-constant-variable form)
           ;; And replace special variable with calls to symbol-value.
           (pass1-form `(,(special-variable-access-function var) ',(name var)) env)))
      (lexical-variable
       (when (eq (lexical-variable-ignore var) 't)
         (warn 'sys.int::simple-style-warning
               :format-control "Reading ignored variable ~S."
               :format-arguments (list (lexical-variable-name var))))
       (incf (lexical-variable-use-count var))
       (pushnew *current-lambda* (lexical-variable-used-in var))
       var))))

(defun pass1-variable (form env)
  (let* ((converted (pass1-variable-inner form env))
         (declared-type (lookup-variable-declared-type-in-environment form env)))
    (if (eql declared-type 't)
        converted
        (make-instance 'ast-the
                       :optimize (optimize-qualities-in-environment env)
                       :type (sys.int::typeexpand declared-type env)
                       :value converted))))

(defun pass1-form (form env)
  (cond
    ((symbolp form)
     ;; Expand symbol macros.
     (multiple-value-bind (expansion expandedp)
         (macroexpand-1 form env)
       (if expandedp
           (pass1-form (let ((declared-type (lookup-variable-declared-type-in-environment form env)))
                         (if (eql declared-type 't)
                             expansion
                             `(the ,declared-type ,expansion)))
                       env)
           (pass1-variable form env))))
    ;; Self-evaluating forms are quoted.
    ((not (consp form))
     (make-instance 'ast-quote
                    :optimize (optimize-qualities-in-environment env)
                    :value form))
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
         ((sys.int::%jump-table) (pass1-jump-table form env))
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
  (assert (symbolp (first form)))
  (let ((fn (find-function (first form) env))
        (args (pass1-implicit-progn (rest form) env)))
    (cond ((lexical-variable-p fn)
           ;; Lexical function.
           (make-instance 'ast-call
                          :optimize (optimize-qualities-in-environment env)
                          :name 'funcall
                          :arguments (list* fn args)))
          (t ;; Top-level function.
           (make-instance 'ast-call
                          :optimize (optimize-qualities-in-environment env)
                          :name fn
                          :arguments args)))))

(defun pass1-block (form env)
  (destructuring-bind (name &body forms) (cdr form)
    (check-type name symbol)
    (let* ((var (make-instance 'block-information
                               :optimize (optimize-qualities-in-environment env)
                               :name name
                               :definition-point *current-lambda*))
           (env (extend-environment env
                                    :blocks (list (list name var)))))
      (make-instance 'ast-block
                     :optimize (optimize-qualities-in-environment env)
                     :info var
                     :body (pass1-form `(progn ,@forms) env)))))

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
          (pass1-form ''nil env)))))

(defun frob-flet-function (fn)
  "Turn an FLET/LABELS function definition into a symbolic name, a lexical variable and a lambda expression."
  (multiple-value-bind (body declares)
      (parse-declares (cddr fn))
    ;; FIXME: docstring permitted here.
    (let* ((name (first fn))
           (var (make-instance 'lexical-variable
                               :name name
                               :definition-point *current-lambda*)))
      (values name var `(lambda ,(second fn)
                          (declare (sys.int::lambda-name (flet ,name :in ,(lambda-information-name *current-lambda*)))
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

(defun function-declared-notinline-p (name declares)
  "Look for a (notinline NAME) declaration for NAME in DECLARES."
  (dolist (dec declares)
    (when (eql (first dec) 'notinline)
      (dolist (item (rest dec))
        (when (equal item name)
          (return-from function-declared-notinline-p t))))))

(defun pass1-flet (form env)
  (destructuring-bind (functions &body forms) (cdr form)
    (multiple-value-bind (body declares)
        (parse-declares forms)
      (let ((bindings '())
            (env (extend-environment env
                                     :declarations (remove-if-not (lambda (x) (eql x 'optimize))
                                                                  declares
                                                                  :key #'first))))
        (make-instance 'ast-let
                       :optimize (optimize-qualities-in-environment env)
                       :bindings (mapcar (lambda (x)
                                           (multiple-value-bind (sym var lambda)
                                               (frob-flet-function x)
                                             (push (list sym var) bindings)
                                             (let ((lambda (pass1-lambda lambda env)))
                                               (when (function-declared-dynamic-extent-p sym declares)
                                                 (setf (getf (lambda-information-plist lambda) 'declared-dynamic-extent) t))
                                               (when (function-declared-notinline-p sym declares)
                                                 (setf (getf (lambda-information-plist lambda) 'notinline) t))
                                               (list var lambda))))
                                         functions)
                       :body (pass1-form `(progn ,@body)
                                         (extend-environment env
                                                             :functions bindings
                                                             :declarations declares)))))))

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
                 (make-instance 'ast-function
                                :optimize (optimize-qualities-in-environment env)
                                :name name)))))))

(defun pass1-go (form env)
  (destructuring-bind (tag-name) (cdr form)
    (check-type tag-name (or symbol integer))
    (let ((tag (or (lookup-go-tag-in-environment tag-name env)
                   (error-program-error "GO refers to unknown tag ~S." tag-name))))
      (incf (go-tag-use-count tag))
      (pushnew *current-lambda* (go-tag-used-in tag))
      (make-instance 'ast-go
                     :optimize (optimize-qualities-in-environment env)
                     :target tag
                     :info (go-tag-tagbody tag)))))

(defun pass1-if (form env)
  (destructuring-bind (test then &optional else) (cdr form)
    (make-instance 'ast-if
                   :optimize (optimize-qualities-in-environment env)
                   :test (pass1-form test env)
                   :then (pass1-form then env)
                   :else (pass1-form else env))))

(defun pass1-labels (form env)
  (destructuring-bind (functions &body forms) (cdr form)
    (multiple-value-bind (body declares)
        (parse-declares forms)
      ;; Generate variables & lambda expressions for each function.
      (let* ((raw-bindings (mapcar (lambda (x)
                                     (multiple-value-list (frob-flet-function x)))
                                   functions))
             (env (extend-environment env
                                      :functions (mapcar (lambda (x)
                                                           (list (first x) (second x)))
                                                         raw-bindings)
                                      :declarations (remove-if-not (lambda (x) (eql x 'optimize))
                                                                   declares
                                                                   :key #'first))))
        (make-instance 'ast-let
                       :optimize (optimize-qualities-in-environment env)
                       :bindings (mapcar (lambda (x)
                                           (let ((lambda (pass1-lambda (third x) env)))
                                             (when (function-declared-dynamic-extent-p (first x) declares)
                                               (setf (getf (lambda-information-plist lambda) 'declared-dynamic-extent) t))
                                             (when (function-declared-notinline-p (first x) declares)
                                               (setf (getf (lambda-information-plist lambda) 'notinline) t))
                                             (list (second x) lambda)))
                                         raw-bindings)
                       :body (pass1-form `(progn ,@body)
                                         (extend-environment env :declarations declares)))))))

(defun check-variable-bindable (var)
  (when (and (typep var 'special-variable)
             (not (member (sys.int::symbol-mode (name var))
                          '(nil :special))))
    (error-program-error "Attemting to bind ~S variable ~S." (sys.int::symbol-mode (name var)) (name var))))

(defun filter-let-type-declarations (declares names)
  (let ((result '()))
    (loop
       for dec in declares
       when (eql (first dec) 'type)
       do (push `(type ,(second dec) ,@(remove-if (lambda (x) (member x names)) (cddr dec))) result)
       when (non-type-type-declaration-p (first dec))
       do (push `(type ,(first dec) ,@(remove-if (lambda (x) (member x names)) (rest dec))) result))
    (reverse result)))

(defun pass1-let (form env)
  (destructuring-bind (bindings &body forms) (cdr form)
    (multiple-value-bind (body declares)
        (parse-declares forms)
      (let* ((names (loop for binding in bindings collect (parse-let-binding binding)))
             (env (extend-environment env
                                      :declarations (append
                                                     (remove-if-not (lambda (x) (eql x 'optimize))
                                                                    declares
                                                                    :key #'first)
                                                     (filter-let-type-declarations declares names))))
             (variables (mapcar (lambda (binding)
                                  (multiple-value-bind (name init-form)
                                      (parse-let-binding binding)
                                    (let ((var (make-variable name declares)))
                                      (check-variable-bindable var)
                                      (list name init-form var))))
                                bindings)))
        (make-instance 'ast-let
                       :optimize (optimize-qualities-in-environment env)
                       :bindings (mapcar (lambda (b)
                                           (list (third b) (pass1-form (wrap-initform-with-the (second b) (third b) declares) env)))
                                         variables)
                       :body (pass1-form `(progn ,@body)
                                         (extend-environment env
                                                             :variables (mapcar (lambda (b)
                                                                                  (list (first b) (third b)))
                                                                                variables)
                                                             :declarations declares)))))))

(defun find-type-declaration (symbol declares)
  (dolist (dec declares 't)
    (when (and (eql (first dec) 'type)
               (member symbol (cddr dec)))
      (return (second dec)))))

(defun wrap-initform-with-the (initform var declares)
  (etypecase var
    (lexical-variable
     (let ((declared-type (find-type-declaration (name var) declares)))
       (if (eql declared-type 't)
           initform
           `(the ,declared-type ,initform))))
    (special-variable
     (let ((declared-type `(and ,(find-type-declaration (name var) declares)
                                ,(mezzano.runtime::symbol-type (name var)))))
       `(the ,declared-type ,initform)))))

(defun non-type-type-declaration-p (declaration)
  "Test if DECLARATION should be treated as a type declaration."
  (and (symbolp declaration)
       (or (get declaration 'sys.int::type-expander)
           (get declaration 'sys.int::type-symbol))))

(defun fake-type-declarations-for (declares name)
  (dolist (dec declares '())
    (cond ((and (eql (first dec) 'type)
               (member name (cddr dec)))
           (return `((type ,(second dec) ,name))))
          ((and (non-type-type-declaration-p (first dec))
                (member name (rest dec)))
           (return `((type ,(first dec) ,name)))))))

(defun pass1-let* (form env)
  (destructuring-bind (bindings &body forms) (cdr form)
    (multiple-value-bind (body declares)
        (parse-declares forms)
      (let* ((env (extend-environment env
                                      :declarations (remove-if-not (lambda (x) (eql x 'optimize))
                                                                   declares
                                                                   :key #'first)))
             (result (make-instance 'ast-let
                                    :optimize (optimize-qualities-in-environment env)
                                    :bindings '()
                                    :body (make-instance 'ast-quote
                                                         :optimize (optimize-qualities-in-environment env)
                                                         :value 'nil)))
             (inner result)
             (var-names '()))
        (dolist (b bindings)
          (multiple-value-bind (name init-form)
              (parse-let-binding b)
            (push name var-names)
            (let ((var (make-variable name declares)))
              (check-variable-bindable var)
              (setf (body inner) (make-instance 'ast-let
                                                :optimize (optimize-qualities-in-environment env)
                                                :bindings (list (list var (pass1-form (wrap-initform-with-the init-form var declares) env)))
                                                :body (make-instance 'ast-quote :value 'nil))
                    inner (body inner)
                    env (extend-environment env
                                            :variables (list (list name var))
                                            :declarations (fake-type-declarations-for declares name))))))
        (setf (body inner) (pass1-form `(progn ,@body)
                                       (extend-environment env :declarations declares)))
        (body result)))))

(defun pass1-load-time-value (form env)
  (declare (ignore env))
  (destructuring-bind (form &optional read-only-p) (cdr form)
    (pass1-form (funcall *load-time-value-hook* form read-only-p) '())))

(defun pass1-locally-body (forms env)
  (multiple-value-bind (body declares)
      (parse-declares forms)
    (pass1-form `(progn ,@body) (extend-environment env :declarations declares))))

(defun pass1-locally (form env)
  (pass1-locally-body (cdr form) env))

(defun hack-macrolet-definition (def lexenv)
  "Turn a MACROLET function definition into a name and expansion function."
  (destructuring-bind (name lambda-list &body forms) def
    ;; FIXME: docstring permitted here.
    (let ((whole (gensym "WHOLE")))
      (multiple-value-bind (lambda-list env)
          (sys.int::fix-lambda-list-environment lambda-list)
        (when (not env)
          (setf env (gensym "ENV")))
        (multiple-value-bind (body declares)
            (parse-declares forms)
          (list name
                (sys.int::eval-in-lexenv
                 `(lambda (,whole ,env)
                    (declare (ignorable ,whole ,env)
                             (sys.int::lambda-name (macrolet ,name)))
                    (destructuring-bind ,lambda-list (cdr ,whole)
                      (declare ,@declares)
                      (block ,name ,@body)))
                 (environment-macro-definitions-only lexenv))))))))

(defun pass1-macrolet (form env)
  (destructuring-bind (definitions &body body) (cdr form)
    (let* ((macro-functions (loop
                               for def in definitions
                               collect (hack-macrolet-definition def env)))
           (env (extend-environment env :functions macro-functions)))
      (pass1-locally-body body env))))

(defun pass1-multiple-value-call (form env)
  (destructuring-bind (function-form &rest forms) (cdr form)
    ;; Simplify M-V-CALL based on the number of forms.
    (case (length forms)
      (0 ; No forms, convert to funcall.
       (pass1-form `(funcall ,function-form) env))
      (1 ; One form, transform as-is.
       (make-instance 'ast-multiple-value-call
                      :optimize (optimize-qualities-in-environment env)
                      :function-form (pass1-form function-form env)
                      :value-form (pass1-form (first forms) env)))
      (t ; Many forms, simplify.
       (pass1-form `(apply ,function-form
                           (append ,@(loop for f in forms
                                        collect `(multiple-value-call #'list ,f))))
                   env)))))

(defun pass1-multiple-value-prog1 (form env)
  (destructuring-bind (first-form &body forms) (cdr form)
    (if forms
        (make-instance 'ast-multiple-value-prog1
                       :optimize (optimize-qualities-in-environment env)
                       :value-form (pass1-form first-form env)
                       :body (pass1-form `(progn ,@forms) env))
        (pass1-form first-form env))))

;;; Never generate empty PROGNs and avoid generating PROGNs with just one form.
(defun pass1-progn (form env)
  (cond ((null (cdr form))
         (pass1-form ''nil env))
        ((null (cddr form))
         (pass1-form (cadr form) env))
        (t (make-instance 'ast-progn
                          :optimize (optimize-qualities-in-environment env)
                          :forms (pass1-implicit-progn (rest form) env)))))

;; Turn PROGV into a call to %PROGV.
(defun pass1-progv (form env)
  (destructuring-bind (symbols values &body forms) (cdr form)
    (pass1-form `(sys.int::%progv ,symbols ,values
                                  #'(lambda () (progn ,@forms)))
                env)))

(defun pass1-quote (form env)
  (destructuring-bind (thing) (cdr form)
    (make-instance 'ast-quote
                   :optimize (optimize-qualities-in-environment env)
                   :value thing)))

(defun pass1-return-from (form env)
  (destructuring-bind (name &optional result) (cdr form)
    (check-type name symbol)
    (let ((tag (or (lookup-block-in-environment name env)
                   (error-program-error "RETURN-FROM refers to unknown block ~S." name))))
      (incf (lexical-variable-use-count tag))
      (pushnew *current-lambda* (lexical-variable-used-in tag))
      (make-instance 'ast-return-from
                     :optimize (optimize-qualities-in-environment env)
                     :target tag
                     :value (pass1-form result env)
                     :info tag))))

(defun special-variable-access-function (special-variable)
  (cond ((eql (sys.int::symbol-mode (name special-variable)) :global)
         'sys.int::symbol-global-value)
        (t
         'symbol-value)))

(defun pass1-setq (form env)
  (do ((i (cdr form) (cddr i))
       (forms '()))
      ((endp i)
       (cond ((null forms)
              (pass1-form ''nil env))
             ((null (rest forms))
              (first forms))
             (t (make-instance 'ast-progn
                               :optimize (optimize-qualities-in-environment env)
                               :forms (nreverse forms)))))
    (when (null (cdr i))
      (error-program-error "Odd number of arguments to SETQ."))
    (let* ((var (find-variable (first i) env t))
           (val (second i))
           (declared-type (lookup-variable-declared-type-in-environment (first i) env))
           (wrapped-value (if (eql declared-type 't)
                              val
                              `(the ,declared-type ,val))))
      (etypecase var
        (special-variable
         (push (pass1-form `(funcall #'(setf ,(special-variable-access-function var))
                                     ,wrapped-value
                                     ',(name var))
                           env) forms))
        (lexical-variable
         (when (eq (lexical-variable-ignore var) 't)
           (warn 'sys.int::simple-style-warning
                 :format-control "Writing ignored variable ~S."
                 :format-arguments (list (lexical-variable-name var))))
         (incf (lexical-variable-use-count var))
         (incf (lexical-variable-write-count var))
         (pushnew *current-lambda* (lexical-variable-used-in var))
         (push (make-instance 'ast-setq
                              :optimize (optimize-qualities-in-environment env)
                              :variable var
                              :value (pass1-form wrapped-value env))
               forms))
        (cons
         ;; Symbol macro.
         (push (pass1-form `(setf ,(second var) ,wrapped-value) env) forms))))))

(defun pass1-symbol-macrolet (form env)
  (destructuring-bind (definitions &body body) (cdr form)
    (assert (every (lambda (def)
                     (and (= (list-length def) 2)
                          (symbolp (first def))))
                   definitions)
            ()
            "Bad SYMBOL-MACROLET definition.")
    (multiple-value-bind (body declares)
        (parse-declares body)
      (let* ((defs (loop
                      for (name expansion) in definitions
                      do
                        (when (declared-as-p 'special name declares)
                          (error-program-error "Symbol macro ~S declared special." name))
                        (when (eql (sys.int::variable-information name) :special)
                          (error-program-error "Attempt to bind special variable ~S as a symbol-macro." name))
                        (when (eql (sys.int::variable-information name) :constant)
                          (error-program-error "Attempt to bind constant ~S as a symbol-macro." name))
                      collect (list name
                                    (make-instance 'symbol-macro
                                                   :optimize (optimize-qualities-in-environment env)
                                                   :name name
                                                   :expansion expansion)))))
        (pass1-form `(progn ,@body) (extend-environment env
                                                        :variables defs
                                                        :declarations declares))))))

;; Turn a list of statements (mixed go tags and forms) into
;; a list of (go-tag form). If there is no initial go tag, then one will
;; be created. GO forms will be inserted to replicate normal fall-through behaviour.
(defun parse-tagbody-body (statements)
  (let ((current-tag nil)
        (accumulated-forms '())
        (result '()))
    (cond ((typep (first statements) '(or symbol integer))
           (setf current-tag (pop statements)))
          (t
           ;; Generate an entry tag.
           (setf current-tag (gensym "TAGBODY-ENTRY"))))
    (dolist (statement statements)
      (etypecase statement
        ((or symbol integer)
         ;; Finish the current tag and switch to the next.
         (push `(go ,statement) accumulated-forms)
         (push (list current-tag `(progn ,@(reverse accumulated-forms))) result)
         (setf current-tag statement
               accumulated-forms '()))
        (cons
         (push statement accumulated-forms))))
    ;; Finish up.
    (push (list current-tag `(progn ,@(reverse accumulated-forms))) result)
    (reverse result)))

(defun pass1-tagbody (form env)
  (let* ((tb (make-instance 'tagbody-information
                            :optimize (optimize-qualities-in-environment env)
                            :name (gensym "TAGBODY")
                            :definition-point *current-lambda*))
         (parsed-body (parse-tagbody-body (rest form)))
         (go-tags (loop
                     for (name form) in parsed-body
                     collect (let ((tag (make-instance 'go-tag
                                                       :optimize (optimize-qualities-in-environment env)
                                                       :name name
                                                       :tagbody tb)))
                               (push tag (tagbody-information-go-tags tb))
                               tag)))
         (env (extend-environment env
                                  :go-tags (loop
                                              for (name form) in parsed-body
                                              for go-tag in go-tags
                                              collect (list name go-tag)))))
    (make-instance 'ast-tagbody
                   :optimize (optimize-qualities-in-environment env)
                   :info tb
                   :statements (loop
                                  for (name form) in parsed-body
                                  for go-tag in go-tags
                                  collect (list go-tag (pass1-form form env))))))

(defun pass1-the (form env)
  (destructuring-bind (value-type form) (cdr form)
    (make-instance 'ast-the
                   :optimize (optimize-qualities-in-environment env)
                   :type (sys.int::typeexpand value-type env)
                   :value (pass1-form form env))))

(defun pass1-throw (form env)
  (destructuring-bind (tag result) (cdr form)
    (let ((tag-sym (gensym "TAG"))
          (result-sym (gensym "RESULT")))
      ;; Why not expand THROW to (M-V-CALL #'%THROW (values tag) result)?
      ;; Because M-V-CALL with multiple arguments isn't natively supported by the compiler
      ;; and involves calls to APPEND & MULTIPLE-VALUE-LIST.
      ;; Do some ridiculous gymanstics to put the result values into a list with dynamic extent.
      (pass1-form
       `(let ((,tag-sym ,tag))
          (flet ((%%throw-trampoline (&rest ,result-sym)
                   (declare (dynamic-extent ,result-sym))
                   ;; And do more to prevent this from being turned into a tail call.
                   (values (sys.int::%throw ,tag-sym ,result-sym))))
            (declare (dynamic-extent #'%%throw-trampoline))
            (multiple-value-call #'%%throw-trampoline ,result)))
       env))))

;;; Translate (unwind-protect form . cleanup-forms) to
;;; (unwind-protect form (lambda () . cleanup-forms)).
(defun pass1-unwind-protect (form env)
  (destructuring-bind (protected-form &body cleanup-forms) (cdr form)
    (if cleanup-forms
        (make-instance 'ast-unwind-protect
                       :optimize (optimize-qualities-in-environment env)
                       :protected-form (pass1-form protected-form env)
                       :cleanup-function (pass1-lambda `(lambda ()
                                                          (declare (sys.int::lambda-name (unwind-protect-cleanup :in ,(lambda-information-name *current-lambda*))))
                                                          (progn ,@cleanup-forms))
                                                       env))
        (pass1-form protected-form env))))

(defun pass1-jump-table (form env)
  (destructuring-bind (test-form &body forms) (cdr form)
    (make-instance 'ast-jump-table
                   :optimize (optimize-qualities-in-environment env)
                   :value (pass1-form test-form env)
                   :targets (pass1-implicit-progn forms env))))
