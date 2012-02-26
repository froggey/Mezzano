(in-package #:system.internals)

(defun expand-macrolet-function (function)
  (destructuring-bind (name lambda-list &body body) function
    (let ((whole (gensym "WHOLE"))
          (env (gensym "ENV")))
      (multiple-value-bind (new-lambda-list env-binding)
          (fix-lambda-list-environment lambda-list)
        `(lambda (,whole ,env)
           (declare (lambda-name (macrolet ,name))
                    (ignorable ,whole ,env))
           ,(expand-destructuring-lambda-list new-lambda-list name body
                                              whole `(cdr ,whole)
                                              (when env-binding
                                                (list `(,env-binding ,env)))))))))

;; TODO: Create macrofunctions correctly, instead of using the host's destructuring-bind
(defun make-macrolet-env (defs env)
  "Return a new environment containing the macro definitions."
  (dolist (d defs)
    (push (list :macro (first d) (eval (expand-macrolet-function d))) env))
  env)

(defun make-symbol-macrolet-env (defs env)
  (dolist (d defs)
    (destructuring-bind (symbol expansion) d
      (push (list :symbol-macro symbol (lambda (whole env)
                                         (declare (ignore whole env))
                                         expansion))
            env)))
  env)

(defun handle-top-level-implicit-progn (forms load-fn eval-fn mode env)
  (dolist (f forms)
    (handle-top-level-form f load-fn eval-fn mode env)))

(defun handle-top-level-lms-body (forms load-fn eval-fn mode env)
  "Common code for handling the body of LOCALLY, MACROLET and SYMBOL-MACROLET forms at the top-level."
  (multiple-value-bind (body declares)
      (parse-declares forms)
    (dolist (dec declares)
      (when (eql 'special (first dec))
        (push (list* :special (rest dec)) env)))
    (handle-top-level-implicit-progn body load-fn eval-fn mode env)))

(defun handle-top-level-form (form load-fn eval-fn &optional (mode :not-compile-time) env)
  "Handle top-level forms. If the form should be evaluated at compile-time
then it is evaluated using EVAL-FN, if it should be loaded then
it is passed to LOAD-FN.
LOAD-FN is must be a function designator for a two-argument function. The
first argument is the form to load and the second is the environment.
NOTE: Non-compound forms (after macro-expansion) are ignored."
  ;; 3.2.3.1 Processing of Top Level Forms
  ;; 2. If the form is a macro form, its macro expansion is computed and processed
  ;;    as a top level form in the same processing mode.
  (let ((expansion (macroexpand form env)))
    ;; Symbol-macros, etc will have been expanded by this point
    ;; Normal symbols and self-evaluating objects will not have side-effects and
    ;; can be ignored.
    (when (consp expansion)
      (case (first expansion)
	;; 3. If the form is a progn form, each of its body forms is sequentially
	;;    processed as a top level form in the same processing mode.
	((progn) (handle-top-level-implicit-progn (rest expansion) load-fn eval-fn mode env))
	;; 4. If the form is a locally, macrolet, or symbol-macrolet, compile-file
	;;    establishes the appropriate bindings and processes the body forms as
	;;    top level forms with those bindings in effect in the same processing mode.
	((locally)
         (handle-top-level-lms-body (rest expansion) load-fn eval-fn mode env))
	((macrolet)
         (destructuring-bind (definitions &body body) (rest expansion)
           (handle-top-level-lms-body body load-fn eval-fn mode (make-macrolet-env definitions env))))
	((symbol-macrolet)
         (destructuring-bind (definitions &body body) (rest expansion)
           (handle-top-level-lms-body body load-fn eval-fn mode (make-symbol-macrolet-env definitions env))))
	;; 5. If the form is an eval-when form, it is handled according to figure 3-7.
	((eval-when)
         (destructuring-bind (situation &body body) (rest expansion)
           (multiple-value-bind (compile load eval)
               (parse-eval-when-situation situation)
             ;; Figure 3-7. EVAL-WHEN processing
             (cond
               ;; Process as compile-time-too.
               ((or (and compile load)
                    (and (not compile) load eval (eql mode :compile-time-too)))
                (handle-top-level-implicit-progn body load-fn eval-fn :compile-time-too env))
               ;; Process as not-compile-time.
               ((or (and (not compile) load eval (eql mode :not-compile-time))
                    (and (not compile) load (not eval)))
                (handle-top-level-implicit-progn body load-fn eval-fn :not-compile-time env))
               ;; Evaluate.
               ((or (and compile (not load))
                    (and (not compile) (not load) eval (eql mode :compile-time-too)))
                (dolist (form body)
                  (funcall eval-fn `(progn ,@body) env)))
               ;; Discard.
               ((or (and (not compile) (not load) eval (eql mode :not-compile-time))
                    (and (not compile) (not load) (not eval)))
                nil)
               (t (error "Impossible!"))))))
	  ;; 6. Otherwise, the form is a top level form that is not one of the
	  ;;    special cases. In compile-time-too mode, the compiler first
	  ;;    evaluates the form in the evaluation environment and then minimally
	  ;;    compiles it. In not-compile-time mode, the form is simply minimally
	  ;;    compiled. All subforms are treated as non-top-level forms.
	  (t (when (eql mode :compile-time-too)
	       (funcall eval-fn expansion env))
	     (funcall load-fn expansion env))))))
