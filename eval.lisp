(defpackage #:system.eval
  (:nicknames #:sys.eval)
  (:use #:cl))

(in-package #:sys.eval)

(defparameter *special-forms* (make-hash-table))

(defmacro defspecial (name lambda-list &body body)
  (let ((form-sym (gensym))
        (env-sym nil))
    (if (eql (first lambda-list) '&environment)
        (setf env-sym (second lambda-list)
              lambda-list (cddr lambda-list))
        (setf env-sym (gensym)))
  `(setf (gethash ',name *special-forms*)
         (lambda (,form-sym ,env-sym)
           (declare (ignorable ,form-sym ,env-sym)
                    (system:lambda-name (special-form ,name)))
           (block ,name
             (destructuring-bind ,lambda-list (cdr ,form-sym)
               ,@body))))))

(defun find-variable (symbol env)
  "Locate SYMBOL in ENV. Returns a binding list or the symbol if there was no lexical binding."
  (dolist (e env symbol)
    (when (and (eql (first e) :special) (member symbol (rest e)))
      (return symbol))
    (when (and (eql (first e) :binding) (eql (second e) symbol))
      (return e))))

(defun find-function (name env)
  (dolist (e env (restart-case (fdefinition name)
                   (use-value (v)
                     :interactive (lambda ()
                                    (format t "Enter a new value (evaluated): ")
                                    (list (eval (read))))
                     :report (lambda (s) (format s "Input a value to be used in place of ~S." `(fdefinition ',name)))
                     v)
                   (store-value (v)
                     :interactive (lambda ()
                                    (format t "Enter a new value (evaluated): ")
                                    (list (eval (read))))
                     :report (lambda (s) (format s "Input a new value for ~S." `(fdefinition ',name)))
                     (setf (fdefinition name) v))))
    (when (eql (first e) :functions)
      (let ((fn (assoc name (rest e))))
	(when fn
	  (return (cdr fn)))))))

(defclass interpreted-function ()
  ((name :initarg :name :reader interpreted-function-name)
   (lambda :initarg :lambda :reader interpreted-function-lambda)
   (env :initarg :env :reader interpreted-function-environment))
  (:metaclass sys.clos:funcallable-standard-class)
  (:default-initargs :name nil))

(defmethod sys.int::funcallable-instance-lambda-expression ((function interpreted-function))
  (values (interpreted-function-lambda function)
          (interpreted-function-environment function)
          (interpreted-function-name function)))

(defmethod print-object ((object interpreted-function) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (interpreted-function-name object)
      (write (interpreted-function-name object) :stream stream))))

(defun eval-lambda (lambda outer-env)
  (let ((lambda-list (second lambda))
        (forms (cddr lambda))
        (name 'interpreted-function))
    (multiple-value-bind (body declares docstring)
	(sys.int::parse-declares forms :permit-docstring t)
      (declare (ignore docstring))
      (dolist (i declares)
        (when (eql (first i) 'system:lambda-name)
          (setf name (second i))))
      (multiple-value-bind (required optional rest enable-keys keys allow-other-keys aux)
	  (sys.int::parse-ordinary-lambda-list lambda-list)
        (flet ((interpret-function (&rest args)
                 (let ((env outer-env))
                   (dolist (arg required)
                     (when (null args)
                       (error "Too few arguments to function ~S lambda-list ~S" name lambda-list))
                     (push (list :binding arg (pop args)) env))
                   (dolist (arg optional)
                     (cond (args
                            (push (list :binding (first arg) (pop args)) env)
                            (when (third arg)
                              (push (list :binding (third arg) t) env)))
                           (t
                            (push (list :binding (first arg) (eval-in-lexenv (second arg) env)) env)
                            (when (third arg)
                              (push (list :binding (third arg) nil) env)))))
                   (if rest
                       (push (list :binding rest args) env)
                       (when (and (not enable-keys) args)
                         (error "Too many arguments to function ~S lambda-list ~S" name lambda-list)))
                   (when enable-keys
                     (when (oddp (length args))
                       (error "Odd number of &KEY arguments."))
                     (unless allow-other-keys
                       (do ((i args (cddr i)))
                           ((null i))
                         (unless (member (car i) keys)
                           (error "Unknown &KEY argument ~S." (car i)))))
                     (dolist (key keys)
                       (let ((arg (getf args (caar key) args)))
                         (cond ((eql arg args)
                                (push (list :binding (cadar key) (eval-in-lexenv (second key) env)) env)
                                (when (third key)
                                  (push (list :binding (third key) nil) env)))
                               (t (push (list :binding (cadar key) arg) env)
                                  (when (third key)
                                    (push (list :binding (third key) t) env)))))))
                   (dolist (arg aux)
                     (push (list :binding (first arg) (eval-in-lexenv (second arg) env)) env))
                   (eval-locally-body declares body env))))
          (let ((x (make-instance 'interpreted-function
                                  :name name
                                  :lambda lambda
                                  :env outer-env)))
            (sys.clos:set-funcallable-instance-function x #'interpret-function)
            x))))))

(defun eval-progn-body (forms env)
  (do ((itr forms (cdr itr)))
      ((null (cdr itr))
       (eval-in-lexenv (car itr) env))
    (eval-in-lexenv (car itr) env)))

(defun eval-locally-body (declares body env)
  "Collect all special declarations and add them to the environment."
  (dolist (dec declares)
    (when (eql (car dec) 'special)
      (dolist (v (cdr dec))
	(push (list :special v) env))))
  (eval-progn-body body env))

(defun frob-flet-function (definition env)
  (destructuring-bind (name lambda-list &body body) definition
    (values name
            ;; FIXME: create a named block.
            (eval-lambda `(lambda ,lambda-list
                            ,@body)
                         env))))

(defspecial block (&environment env name &body body)
  (let ((env (cons (list :block name #+nil(lambda (values)
                                       (return-from block (values-list values))))
                   env)))
    (eval-progn-body body env)))

(defspecial eval-when (&environment env situation &body body)
  (multiple-value-bind (compile load eval)
      (sys.int::parse-eval-when-situation situation)
    (when eval
      (eval-progn-body body env))))

(defspecial flet (&environment env definitions &body forms)
  (let ((functions (mapcar (lambda (def)
                             (multiple-value-bind (name fn)
                                 (frob-flet-function def env)
                               (cons name fn)))
                           definitions)))
    (multiple-value-bind (body declares)
        (sys.int::parse-declares forms)
      (eval-locally-body declares body (cons (list* :functions functions) env)))))

(defspecial function (&environment env name)
  (if (sys.int::lambda-expression-p name)
      (eval-lambda name env)
      (find-function name env)))

(defspecial if (&environment env test then &optional else)
  (if (eval-in-lexenv test env)
      (eval-in-lexenv then env)
      (eval-in-lexenv else env)))

(defspecial labels (&environment env definitions &body forms)
  (let* ((env (cons (list :functions) env))
         (functions (mapcar (lambda (def)
                              (multiple-value-bind (name fn)
                                  (frob-flet-function def env)
                                (cons name fn)))
                            definitions)))
    (setf (rest (first env)) functions)
    (multiple-value-bind (body declares)
        (sys.int::parse-declares forms)
      (eval-locally-body declares body env))))

(defspecial let (&environment env bindings &body forms)
  (multiple-value-bind (body declares)
      (sys.int::parse-declares forms)
    (let ((special-variables '())
          (special-values '())
          (special-declares (apply 'append
                                   (mapcar #'rest
                                           (remove-if-not (lambda (dec) (eql (first dec) 'special))
                                                          declares))))
          (new-env env))
      (dolist (b bindings)
        (multiple-value-bind (name init-form)
            (sys.int::parse-let-binding b)
          (let ((value (eval-in-lexenv init-form env)))
            (ecase (sys.int::symbol-mode name)
              ((nil :symbol-macro)
               (cond ((member name special-declares)
                      (push name special-variables)
                      (push value special-values))
                     (t (push (list :binding name value) new-env))))
              (:special
               (push name special-variables)
               (push value special-values))
              (:constant (error "Cannot bind over constant ~S." name))))))
      (when special-variables
        (error "TODO: special-variables"))
      #+nil(progv special-variables special-values
             (eval-locally-body declares body new-env))
      (eval-locally-body declares body new-env))))

(defspecial let* (&environment env bindings &body forms)
  (multiple-value-bind (body declares)
      (sys.int::parse-declares forms)
    (let ((special-declares (apply 'append
                                   (mapcar #'rest
                                           (remove-if-not (lambda (dec) (eql (first dec) 'special))
                                                          declares)))))
      (labels ((bind-one (bindings)
                 (if bindings
                     (multiple-value-bind (name init-form)
                         (sys.int::parse-let-binding (first bindings))
                       (let ((value (eval-in-lexenv init-form env)))
                         (ecase (sys.int::symbol-mode name)
                           ((nil :symbol-macro)
                            (cond ((member name special-declares)
                                   (progv (list name) (list value)
                                     (bind-one (rest bindings))))
                                  (t (push (list :binding name value) env)
                                     (bind-one (rest bindings)))))
                           (:special
                            (progv (list name) (list value)
                              (bind-one (rest bindings))))
                           (:constant (error "Cannot bind over constant ~S." name)))))
                     (eval-locally-body declares body env))))
        (bind-one bindings)))))

(defspecial multiple-value-call (&environment env function-form &rest forms)
  (apply (eval-in-lexenv function-form env)
         (mapcan (lambda (f)
                   (multiple-value-list (eval-in-lexenv f env)))
                 forms)))

(defspecial progn (&environment env &body forms)
  (eval-progn-body forms env))

(defspecial quote (object)
  object)

(defspecial return-from (&environment env name &optional value)
  (dolist (e env (error "No block named ~S." name))
    (when (and (eql (first e) :block)
               (eql (second e) name))
      (funcall (third e) (multiple-value-list (eval-in-lexenv value env))))))

;;; TODO: Expand symbol macros.
(defspecial setq (&environment env symbol value)
  (let ((var (find-variable symbol env)))
    (if (symbolp var)
        (setf (symbol-value var) (eval-in-lexenv value env))
        (setf (third var) (eval-in-lexenv value env)))))

(defspecial the (&environment env type form)
  (declare (ignore type))
  (eval-in-lexenv form env))

(defspecial unwind-protect (&environment env protected-form &body cleanup-forms)
  (unwind-protect
       (eval-in-lexenv protected-form env)
    (eval-progn-body cleanup-forms env)))

(defspecial catch (&environment env tag &body body)
  (catch (eval-in-lexenv tag env)
    (eval-progn-body body env)))

(defspecial throw (&environment env tag result)
  (throw (eval-in-lexenv tag env)
    (eval-in-lexenv result env)))

(defun frob-macrolet-definition (def)
  (destructuring-bind (name lambda-list &body body) def
    (let ((whole (gensym "WHOLE"))
          (env (gensym "ENV")))
      (multiple-value-bind (new-lambda-list env-binding)
          (sys.int::fix-lambda-list-environment lambda-list)
        (cons name
              (eval `#'(lambda (,whole ,env)
                         (declare (system:lambda-name (macrolet ,name))
                                  (ignorable ,whole ,env))
                         ,(sys.int::expand-destructuring-lambda-list new-lambda-list name body
                                                                     whole `(cdr ,whole)
                                                                     (when env-binding
                                                                       (list `(,env-binding ,env)))))))))))

(defspecial macrolet (&environment env definitions &body body)
  (multiple-value-bind (body declares)
      (sys.int::parse-declares body)
    (eval-locally-body declares body
                       (cons (list* :macros
                                    (mapcar 'frob-macrolet-definition definitions))
                             env))))

(defun eval-symbol (form env)
  "3.1.2.1.1  Symbols as forms"
  (let ((var (find-variable form env)))
    (if (symbolp var)
        (restart-case (symbol-value var)
          (use-value (v)
            :interactive (lambda ()
                           (format t "Enter a new value (evaluated): ")
                           (list (eval (read))))
            :report (lambda (s) (format s "Input a value to be used in place of ~S." var))
            v)
          (store-value (v)
            :interactive (lambda ()
                           (format t "Enter a new value (evaluated): ")
                           (list (eval (read))))
            :report (lambda (s) (format s "Input a new value for ~S." var))
            (setf (symbol-value var) v)))
        (third var))))

(defun eval-cons (form env)
  "3.1.2.1.2  Conses as forms"
  (let ((fn (gethash (first form) *special-forms*)))
    (cond (fn (funcall fn form env))
          ((macro-function (first form) env)
           (eval-in-lexenv (macroexpand-1 form env) env))
          (t (apply (if (sys.int::lambda-expression-p (first form))
                        (eval-lambda (first form) env)
                        (find-function (first form) env))
                    (mapcar (lambda (f) (eval-in-lexenv f env))
                            (rest form)))))))

(defun eval-in-lexenv (form &optional env)
  "3.1.2.1  Form evaluation"
  (cond ((symbolp form)
	 (eval-symbol form env))
	((consp form)
	 (eval-cons form env))
	;; 3.1.2.1.3  Self evaluating objects
	(t form)))

(defun eval (form)
  (eval-in-lexenv form nil))
