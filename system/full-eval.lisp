;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; The full evaluator, interprets all forms.

(defpackage :mezzano.full-eval
  (:export #:eval-in-lexenv)
  (:use :cl))

(in-package :mezzano.full-eval)

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
                    (sys.int::lambda-name (special-form ,name)))
           (block ,name
             (destructuring-bind ,lambda-list (cdr ,form-sym)
               ,@body))))))

(defun find-variable (symbol env)
  "Locate SYMBOL in ENV. Returns a binding list or the symbol if there was no lexical binding."
  (sys.c::lookup-variable-in-environment symbol env))

(defun find-function (name env)
  (let ((local-fn (sys.c::lookup-function-in-environment name env)))
    (cond ((functionp local-fn)
           ;; Macro function.
           (error "~S names a macro." name))
          ((typep local-fn 'lexical-variable)
           (variable-value local-fn))
          (t (restart-case (fdefinition name)
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
                     (setf (fdefinition name) v)))))))

(defclass interpreted-function ()
  ((name :initarg :name :reader interpreted-function-name)
   (lambda :initarg :lambda :reader interpreted-function-lambda)
   (env :initarg :env :reader interpreted-function-environment)
   (filename :initarg :filename :reader interpreted-function-filename)
   (tlf :initarg :tlf :reader interpreted-function-tlf))
  (:metaclass mezzano.clos:funcallable-standard-class)
  (:default-initargs :name nil :filename nil :tlf nil))

(defmethod sys.int::funcallable-instance-lambda-expression ((function interpreted-function))
  (values (interpreted-function-lambda function)
          (interpreted-function-environment function)
          (interpreted-function-name function)))

(defmethod sys.int::funcallable-instance-debug-info ((function interpreted-function))
  (list :debug-info
        (interpreted-function-name function)
        nil
        nil
        (interpreted-function-filename function)
        (interpreted-function-tlf function)
        (second (interpreted-function-lambda function))
        nil))

(defmethod sys.int::funcallable-instance-compiled-function-p ((function interpreted-function))
  nil)

(defmethod print-object ((object interpreted-function) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (interpreted-function-name object)
      (write (interpreted-function-name object) :stream stream))))

(defclass lexical-variable ()
  ((name :initarg :name :reader variable-name)
   (value :initarg :value :accessor variable-value))
  (:default-initargs :value nil))

(defun eval-lambda (lambda outer-env)
  (let ((lambda-list (second lambda))
        (forms (cddr lambda))
        (name 'interpreted-function))
    (multiple-value-bind (body declares docstring)
        (sys.int::parse-declares forms :permit-docstring t)
      (declare (ignore docstring))
      (dolist (i declares)
        (when (eql (first i) 'sys.int::lambda-name)
          (setf name (second i))))
      (multiple-value-bind (required optional rest enable-keys keys allow-other-keys aux)
          (sys.int::parse-ordinary-lambda-list lambda-list)
        (flet ((interpret-function (&rest args)
                 (let ((env outer-env))
                   (flet ((bind-one (name value)
                            (setf env
                                  (sys.c::extend-environment
                                   env
                                   :variables (list (list name (make-instance 'lexical-variable :name name :value value)))))))
                     (dolist (arg required)
                       (when (null args)
                         (error "Too few arguments to function ~S lambda-list ~S" name lambda-list))
                       (bind-one arg (pop args)))
                     (dolist (arg optional)
                       (cond (args
                              (bind-one (first arg) (pop args))
                              (when (third arg)
                                (bind-one (third arg) t)))
                             (t
                              (bind-one (first arg) (eval-in-lexenv (second arg) env))
                              (when (third arg)
                                (bind-one (third arg) nil)))))
                     (if rest
                         (bind-one rest args)
                         (when (and (not enable-keys) args)
                           (error "Too many arguments to function ~S lambda-list ~S" name lambda-list)))
                     (when enable-keys
                       (when (oddp (length args))
                         (error "Odd number of &KEY arguments."))
                       (unless allow-other-keys
                         (do ((i args (cddr i)))
                             ((null i))
                           (unless (member (car i) keys :key 'caar)
                             (error "Unknown &KEY argument ~S." (car i)))))
                       (dolist (key keys)
                         (let ((arg (getf args (caar key) args)))
                           (cond ((eql arg args)
                                  (bind-one (cadar key) (eval-in-lexenv (second key) env))
                                  (when (third key)
                                    (bind-one (third key) nil)))
                                 (t
                                  (bind-one (cadar key) arg)
                                  (when (third key)
                                    (bind-one (third key) t)))))))
                     (dolist (arg aux)
                       (bind-one (first arg) (eval-in-lexenv (second arg) env)))
                     (eval-locally-body declares body env)))))
          (let ((x (make-instance 'interpreted-function
                                  :name name
                                  :lambda lambda
                                  :env outer-env
                                  :filename (ignore-errors (namestring *load-truename*))
                                  :tlf sys.int::*top-level-form-number*)))
            (mezzano.clos:set-funcallable-instance-function x #'interpret-function)
            x))))))

(defun eval-progn-body (forms env)
  (do ((itr forms (cdr itr)))
      ((null (cdr itr))
       (eval-in-lexenv (car itr) env))
    (eval-in-lexenv (car itr) env)))

(defun eval-locally-body (declares body env)
  "Collect all special declarations and add them to the environment."
  (eval-progn-body body (sys.c::extend-environment env :declarations declares)))

(defun frob-flet-function (definition env)
  (destructuring-bind (name lambda-list &body body) definition
    (multiple-value-bind (body-forms declares docstring)
        (sys.int::parse-declares body :permit-docstring t)
      (values name
              (eval-lambda `(lambda ,lambda-list
                              (declare ,@declares)
                              ,docstring
                              (block ,(if (symbolp name)
                                          name
                                          (second name))
                                ,@body-forms))
                         env)))))

(defspecial block (&environment env name &body body)
  (let ((env (sys.c::extend-environment
              env
              :blocks (list (list name (lambda (values)
                                         (return-from block (values-list values))))))))
    (eval-progn-body body env)))

(defspecial return-from (&environment env name &optional value)
  (let ((target (or (sys.c::lookup-block-in-environment name env)
                    (error "No block named ~S." name))))
    (funcall target (multiple-value-list (eval-in-lexenv value env)))))

(defspecial eval-when (&environment env situation &body body)
  (multiple-value-bind (compile load eval)
      (sys.int::parse-eval-when-situation situation)
    (when eval
      (eval-progn-body body env))))

(defspecial flet (&environment env definitions &body forms)
  (multiple-value-bind (body declares)
      (sys.int::parse-declares forms)
    (let* ((functions (mapcar (lambda (def)
                                (multiple-value-bind (name fn)
                                    (frob-flet-function def env)
                                  (list name (make-instance
                                              'lexical-variable
                                              :name name
                                              :value fn))))
                              definitions))
           (env (sys.c::extend-environment
                 env
                 :functions functions
                 :declarations declares)))
      (eval-progn-body body env))))

(defspecial function (&environment env name)
  (if (sys.int::lambda-expression-p name)
      (eval-lambda name env)
      (find-function name env)))

(defspecial if (&environment env test then &optional else)
  (if (eval-in-lexenv test env)
      (eval-in-lexenv then env)
      (eval-in-lexenv else env)))

(defspecial labels (&environment env definitions &body forms)
  (multiple-value-bind (body declares)
      (sys.int::parse-declares forms)
    (let* ((functions (mapcar (lambda (def)
                                (let ((name (first def)))
                                  (list name (make-instance
                                              'lexical-variable
                                              :name name
                                              :value nil))))
                              definitions))
           (env (sys.c::extend-environment
                 env
                 :functions functions
                 :declarations declares)))
      (loop
         for def in definitions
         for (name var) in functions
         do (multiple-value-bind (name fn)
                (frob-flet-function def env)
              (setf (variable-value var) fn)))
      (eval-progn-body body env))))

(defspecial locally (&environment env &body forms)
  (multiple-value-bind (body declares)
      (sys.int::parse-declares forms)
    (eval-locally-body declares body env)))

(defun make-variable (name declares)
  (if (or (sys.int::variable-information name)
          (sys.c::declared-as-p 'special name declares))
      (make-instance 'sys.c::special-variable :name name)
      (make-instance 'lexical-variable
                     :name name)))

(defspecial let (&environment env bindings &body forms)
  (multiple-value-bind (body declares)
      (sys.int::parse-declares forms)
    (let* ((names-and-values (mapcar (lambda (binding)
                                       (multiple-value-bind (name init-form)
                                           (sys.int::parse-let-binding binding)
                                         (list name
                                               (make-variable name declares)
                                               (eval-in-lexenv init-form env))))
                                     bindings))
           (env (sys.c::extend-environment env
                                           :variables (loop for (name var init-form) in names-and-values
                                                         collect (list name var))
                                           :declarations declares))
           (special-variables '())
           (special-values '()))
      (loop for (name var init-value) in names-and-values do
           (etypecase var
             (sys.c::special-variable
              (push name special-variables)
              (push init-value special-values))
             (lexical-variable
              (setf (variable-value var) init-value))))
      (progv special-variables special-values
        (eval-progn-body body env)))))

(defspecial let* (&environment env bindings &body forms)
  (multiple-value-bind (body declares)
      (sys.int::parse-declares forms)
    (labels ((bind-one (bindings env)
               (if bindings
                   (multiple-value-bind (name init-form)
                       (sys.int::parse-let-binding (first bindings))
                     (let ((value (eval-in-lexenv init-form env)))
                       (ecase (sys.int::symbol-mode name)
                         ((nil :symbol-macro)
                          (cond ((sys.c::declared-as-p 'special name declares)
                                 (progv (list name) (list value)
                                   (bind-one (rest bindings)
                                             (sys.c::extend-environment env :variables (list (list name (make-instance 'sys.c::special-variable :name name)))))))
                                (t
                                 (let ((var (make-instance 'lexical-variable :name name :value value)))
                                   (bind-one (rest bindings)
                                             (sys.c::extend-environment env :variables (list (list name var))))))))
                         (:special
                          (progv (list name) (list value)
                            (bind-one (rest bindings)
                                      (sys.c::extend-environment env :variables (list (list name (make-instance 'sys.c::special-variable :name name)))))))
                         (:constant (error "Cannot bind over constant ~S." name)))))
                   (eval-progn-body body (sys.c::extend-environment env :declarations declares)))))
      (bind-one bindings env))))

(defspecial multiple-value-call (&environment env function-form &rest forms)
  (apply (eval-in-lexenv function-form env)
         (mapcan (lambda (f)
                   (multiple-value-list (eval-in-lexenv f env)))
                 forms)))

(defspecial multiple-value-prog1 (&environment env first-form &body forms)
  (multiple-value-prog1 (eval-in-lexenv first-form env)
    (eval-progn-body forms env)))

(defspecial progn (&environment env &body forms)
  (eval-progn-body forms env))

(defspecial quote (object)
  object)

(defspecial setq (&environment env &rest pairs)
  (when (oddp (length pairs))
    (error "Odd number of arguments to SETQ."))
  (when pairs
    (flet ((set-one (symbol value)
             (let ((var (find-variable symbol env)))
               (etypecase var
                 (sys.c::special-variable
                  (setf (symbol-value symbol) (eval-in-lexenv value env)))
                 (sys.c::symbol-macro
                  (eval-in-lexenv `(setf ,symbol ,value) env))
                 (lexical-variable
                  (setf (variable-value var) (eval-in-lexenv value env)))))))
    (do ((i pairs (cddr i)))
        ((null (cddr i))
         (set-one (first i) (second i)))
      (set-one (first i) (second i))))))

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
                         (declare (sys.int::lambda-name (macrolet ,name))
                                  (ignorable ,whole ,env))
                         ,(sys.int::expand-destructuring-lambda-list new-lambda-list name body
                                                                     whole `(cdr ,whole)
                                                                     (when env-binding
                                                                       (list `(,env-binding ,env)))
                                                                     :permit-docstring t))))))))

(defspecial macrolet (&environment env definitions &body body)
  (multiple-value-bind (body declares)
      (sys.int::parse-declares body)
    (eval-locally-body declares body
                       (sys.int::make-macrolet-env definitions env))))

(defspecial symbol-macrolet (&environment env definitions &body body)
  (multiple-value-bind (body declares)
      (sys.int::parse-declares body)
    (eval-locally-body declares body
                       (sys.int::make-symbol-macrolet-env definitions env))))

(defspecial tagbody (&environment env &body body)
  (let ((current body))
    (tagbody
       (setf env (sys.c::extend-environment
                  env
                  :go-tags (loop for sublist on body
                                when (typep (first sublist) '(or symbol integer))
                                collect (list (first sublist)
                                              (let ((place sublist))
                                                (lambda () (setf current place) (go loop)))))))
     loop
       (cond
         ((null current)
          (go end))
         ((not (typep (first current) '(or symbol integer)))
          (eval-in-lexenv (first current) env)))
       (setf current (rest current))
       (go loop)
     end)))

(defspecial go (&environment env tag)
  (check-type tag (or symbol integer))
  (funcall (or (sys.c::lookup-go-tag-in-environment tag env)
               (error "No GO-tag named ~S." tag))))

(defspecial progv (&environment env symbols values &body body)
  (progv (eval-in-lexenv symbols env) (eval-in-lexenv values env)
    (eval-progn-body body env)))

(defspecial load-time-value (&environment env form &optional read-only-p)
  (declare (ignore read-only-p))
  (eval form))

(defun eval-symbol (form env)
  "3.1.2.1.1  Symbols as forms"
  (let ((var (find-variable form env)))
    (etypecase var
      (sys.c::special-variable
       (restart-case (symbol-value form)
         (use-value (v)
           :interactive (lambda ()
                          (format t "Enter a new value (evaluated): ")
                          (list (eval (read))))
           :report (lambda (s) (format s "Input a value to be used in place of ~S." form))
           v)
         (store-value (v)
           :interactive (lambda ()
                          (format t "Enter a new value (evaluated): ")
                          (list (eval (read))))
           :report (lambda (s) (format s "Input a new value for ~S." form))
           (setf (symbol-value form) v))))
      (sys.c::symbol-macro
       (eval-in-lexenv (sys.c::symbol-macro-expansion var) env))
      (lexical-variable
       (variable-value var)))))

(defun eval-cons (form env)
  "3.1.2.1.2  Conses as forms"
  (let ((fn (gethash (first form) *special-forms*)))
    (cond (fn (funcall fn form env))
          ((and (listp (first form))
                (eql (first (first form)) 'lambda))
           ;; Lambda form.
           (eval-in-lexenv `(funcall #',(first form) ,@(rest form)) env))
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
