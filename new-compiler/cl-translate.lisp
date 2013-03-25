;;;; Translate CL into IR1.

(in-package #:sys.newc)

(defvar *special-form-translators* (make-hash-table :test 'eq))
(defvar *print-ir-like-cl* t
  "Set to true if the IR printer should use ()' instead of {}? when printing.")

(defclass constant ()
  ((value :initarg :value :reader constant-value)
   (plist :initform '() :initarg :plist :accessor plist))
  (:documentation "A constant value."))

(defclass lexical ()
  ((name :initarg :name :reader lexical-name)
   (plist :initform '() :initarg :plist :accessor plist))
  (:documentation "A lexical variable.")
  (:default-initargs :name (gensym "var")))

(defclass closure ()
  ((name :initarg :name :reader closure-name)
   (required-params :initarg :required-params :reader closure-required-params*)
   (optional-params :initarg :optional-params :reader closure-optional-params)
   (rest-param :initarg :rest-param :reader closure-rest-param)
   (keyword-params :initarg :keyword-params :reader closure-keyword-params)
   (keyword-params-enabled :initarg :keyword-params-enabled :reader closure-keyword-params-enabled)
   (allow-other-keywords :initarg :allow-other-keywords :reader closure-allow-other-keywords)
   ;; Body of the closure, a function application.
   (body :initarg :body :reader closure-body)
   (plist :initform '() :initarg :plist :accessor plist))
  (:documentation "A closure.")
  (:default-initargs :name nil
    :optional-params '()
    :rest-param nil
    :keyword-params-enabled nil
    :keyword-params '()
    :allow-other-keywords nil))

(defun closure-function (closure)
  (first (closure-body closure)))
(defun closure-arguments (closure)
  (rest (closure-body closure)))

(defmethod print-object ((o constant) stream)
  (if *print-pretty*
      (format stream "~A~S"
              (if *print-ir-like-cl* #\' #\?)
              (constant-value o))
      (call-next-method)))

(defmethod print-object ((o lexical) stream)
  (if (and *print-pretty*
           (lexical-name o))
      (write (lexical-name o) :stream stream :escape nil :readably nil)
      (print-unreadable-object (o stream :type t :identity t))))

(defmethod print-object ((o closure) stream)
  (if *print-pretty*
      (let ((form (list (if (getf (plist o) 'continuation) 'clambda 'lambda)
                        (append (closure-required-params* o)
                            (when (closure-optional-params o)
                              `(&optional . ,(closure-optional-params o)))
                            (when (closure-rest-param o)
                              `(&rest ,(closure-rest-param o)))
                            (when (closure-keyword-params-enabled o)
                              `(&key
                                ,@(closure-keyword-params o)
                                ,@(when (closure-allow-other-keywords o)
                                    '(&allow-other-keys)))))
                        (closure-body o))))
        (pprint-logical-block (stream
                               form
                               :prefix (if *print-ir-like-cl* "(" "{")
                               :suffix (if *print-ir-like-cl* ")" "}"))
          (write (first form) :stream stream)
          (write-char #\Space stream)
          (pprint-newline :miser stream)
          (write (second form) :stream stream)
          (pprint-indent :block 1 stream)
          (write-char #\Space stream)
          (pprint-newline :linear stream)
          (write-char (if *print-ir-like-cl* #\( #\{) stream)
          (pprint-fill stream (third form) nil)
          (write-char (if *print-ir-like-cl* #\) #\}) stream)))
      (call-next-method)))

(defmacro defspecial (name (lambda-list continuation environment) &body body)
  "Define a CL special form translator."
  (let ((symbol (intern (format nil "!SPECIAL-FORM-~A" name)))
        (form-sym (gensym "FORM")))
    `(progn (defun ,symbol (,form-sym ,continuation ,environment)
              (declare (ignorable ,continuation ,environment))
              (destructuring-bind ,lambda-list (cdr ,form-sym)
                ,@body))
            (setf (gethash ',name *special-form-translators*) ',symbol))))

(defun ! (code)
  "Convert shorthand to proper IR objects."
  (typecase code
    (cons
     (assert (member (first code) '(lambda clambda)))
     (assert (listp (third code)))
     (make-instance 'closure
                    :required-params (second code)
                    :body (mapcar '! (third code))
                    :plist (list 'continuation (eql (first code) 'clambda))))
    (symbol (make-instance 'constant :value code))
    (t code)))

(defun lambda-expression-p (thing)
  (and (consp thing)
       (eql (first thing) 'lambda)))

(defun go-tag-p (statement)
  (or (integerp statement)
      (symbolp statement)))

(defun find-variable (symbol env)
  (dolist (e env nil)
    (when (eql (first e) :bindings)
      (let ((x (assoc symbol (rest e))))
        (when x
          (return x))))))

(defun translate (form cont env)
  (typecase form
    (symbol (translate-symbol form cont env))
    (cons (translate-cons form cont env))
    (t (translate `',form cont env))))

(defun translate-symbol (form cont env)
  (let ((info (find-variable form env)))
    (if info
        (list (make-instance 'constant :value '%invoke-continuation)
              cont (cdr info))
        (translate (or (sys.c::expand-constant-variable form)
                       `(symbol-value ',form))
                   cont env))))

(defun translate-cons (form cont env)
  (let ((special-fn (gethash (first form) *special-form-translators*)))
    (cond (special-fn
           (funcall special-fn form cont env))
          ((lambda-expression-p (first form))
           (translate-arguments (rest form)
                                (translate-lambda (first form) env)
                                (list cont)
                                env))
          (t (multiple-value-bind (expansion expandedp)
                 (sys.c::compiler-macroexpand-1 form env)
               (if expandedp
                   (translate expansion cont env)
                   (translate-arguments (rest form)
                                        (make-instance 'constant :value (first form))
                                        (list cont)
                                        env)))))))

(defun translate-arguments (args function accum env)
  (if args
      (let ((arg (make-instance 'lexical :name (gensym "arg"))))
        (translate (first args)
                   (! `(clambda (,arg)
                         ,(translate-arguments (rest args)
                                               function
                                               (cons arg accum)
                                               env)))
                   env))
      (list* function (nreverse accum))))

(defun translate-progn (forms cont env)
  "Translate a PROGN-like list of FORMS."
  (cond ((null forms) (translate ''nil cont env))
        ((null (rest forms))
         (translate (first forms) cont env))
        (t (let ((v (make-instance 'lexical :name (gensym "progn"))))
             (translate (first forms)
                        (! `(clambda (,v)
                              ,(translate-progn (rest forms) cont env)))
                        env)))))

(defun translate-lambda (lambda env)
  (multiple-value-bind (body lambda-list declares name docstring)
      (sys.c::parse-lambda lambda)
    (multiple-value-bind (required optional rest enable-keys keys allow-other-keys aux)
	(sys.int::parse-ordinary-lambda-list lambda-list)
      (let ((lambda-cont (make-instance 'lexical :name (gensym "cont")))
            (required-args (mapcar (lambda (x) (make-instance 'lexical :name x))
                                   required))
            (optional-args (mapcar (lambda (x) (make-instance 'lexical :name (gensym (format nil "%~A" (first x)))))
                                   optional))
            (optional-supplied-args (mapcar (lambda (x)
                                              (make-instance 'lexical
                                                             :name (gensym (format nil "~A-suppliedp" (first x)))))
                                            optional))
            (rest-arg (when rest (make-instance 'lexical :name rest)))
            (key-args (mapcar (lambda (x) (make-instance 'lexical :name (gensym (format nil "%~A" (second (first x))))))
                              keys))
            (key-supplied-args (mapcar (lambda (x)
                                         (make-instance 'lexical
                                                             :name (gensym (format nil "~A-suppliedp" (second (first x))))))
                                       keys)))
        (labels ((frob-optional (opt args suppliedp env)
                   (cond (opt
                          (let ((true-optional (make-instance 'lexical
                                                              :name (first (car opt))))
                                (cont (make-instance 'lexical :name (gensym "cont"))))
                            (list (! `(clambda (,cont)
                                        (%if (clambda () (%invoke-continuation ,cont ,(car args)))
                                             ,(make-instance 'closure
                                                             :required-params '()
                                                             :body (translate (second (car opt))
                                                                              cont
                                                                              env)
                                                             :plist '(continuation t))
                                             ,(car suppliedp))))
                                  (make-instance 'closure
                                                 :required-params (list true-optional)
                                                 :body (frob-optional (cdr opt) (cdr args) (cdr suppliedp)
                                                                      (append
                                                                       `((:bindings (,(first (car opt)) . ,true-optional)))
                                                                       (when (third (car opt))
                                                                         `((:bindings (,(third (car opt)) . ,(car suppliedp)))))
                                                                       env))
                                                 :plist '(continuation t)))))
                         (t (frob-rest env))))
                 (frob-rest (env)
                   (when rest
                     (push (list :bindings (cons rest rest-arg)) env))
                   (if enable-keys
                       (frob-keys keys key-args key-supplied-args env)
                       (frob-aux aux env)))
                 (frob-keys (keys args suppliedp env)
                   (cond (keys
                          (let ((true-key (make-instance 'lexical
                                                         :name (second (first (car keys)))))
                                (cont (make-instance 'lexical :name (gensym "cont"))))
                            (list (! `(clambda (,cont)
                                        (%if (clambda () (%invoke-continuation ,cont ,(car args)))
                                             ,(make-instance 'closure
                                                             :required-params '()
                                                             :body (translate (second (car keys))
                                                                              cont
                                                                              env)
                                                             :plist '(continuation t))
                                             ,(car suppliedp))))
                                  (make-instance 'closure
                                                 :required-params (list true-key)
                                                 :body (frob-keys (cdr keys) (cdr args) (cdr suppliedp)
                                                                  (append
                                                                   `((:bindings (,(second (first (car keys))) . ,true-key)))
                                                                   (when (third (car keys))
                                                                     `((:bindings (,(third (car keys)) . ,(car suppliedp)))))
                                                                   env))
                                                 :plist '(continuation t)))))
                         (t (frob-aux aux env))))
                 (frob-aux (aux env)
                   (cond (aux
                          (let ((aux-var (make-instance 'lexical :name (first (car aux)))))
                            (translate (second (car aux))
                                       (make-instance 'closure
                                                      :required-params (list aux-var)
                                                      :body (frob-aux (cdr aux)
                                                                      (list* (list :bindings (cons (first (car aux)) aux-var)) env))
                                                      :plist '(continuation t))
                                       env)))
                         (t (finish env))))
                 (finish (env)
                   (translate-progn body
                                    lambda-cont
                                    env)))
          (make-instance 'closure
                         :name name
                         :required-params (list* lambda-cont required-args)
                         :optional-params (mapcar 'list optional-args optional-supplied-args)
                         :rest-param rest-arg
                         :keyword-params-enabled enable-keys
                         :keyword-params (mapcar (lambda (k lexical suppliedp)
                                                   (list (first (first k)) lexical suppliedp))
                                                 keys
                                                 key-args
                                                 key-supplied-args)
                         :allow-other-keywords allow-other-keys
                         :body (frob-optional optional optional-args optional-supplied-args
                                              (list* `(:bindings ,@(pairlis required required-args))
                                                     env))))))))

(defspecial if ((test-form then-form &optional (else-form ''nil)) cont env)
  (let ((test (make-instance 'lexical :name (gensym "test")))
        (cont-arg (make-instance 'lexical :name (gensym "cont"))))
    (translate test-form
               (! `(clambda (,test)
                     ((clambda (,cont-arg)
                        (%if (clambda () ,(translate then-form cont-arg env))
                             (clambda () ,(translate else-form cont-arg env))
                             ,test))
                      ,cont)))
               env)))

(defspecial progn ((&rest forms) cont env)
  (translate-progn forms cont env))

(defspecial quote ((object) cont env)
  (list (make-instance 'constant :value '%invoke-continuation)
        cont (make-instance 'constant :value object)))

(defspecial function ((name) cont env)
  (if (lambda-expression-p name)
      (list cont (translate-lambda name env))
      (translate `(fdefinition ',name) cont env)))

(defspecial multiple-value-call ((function-form &rest forms) cont env)
  (translate `(%multiple-value-call ,function-form
                                    ,@(mapcar (lambda (f) `#'(lambda () ,f))
                                              forms))
             cont env))

(defspecial multiple-value-prog1 ((first-form &rest forms) cont env)
  (translate `(%multiple-value-prog1 #'(lambda () ,first-form)
                                     #'(lambda () ,@forms))
             cont env))

(defspecial block ((name &body body) original-cont env)
  (let ((cont (make-instance 'lexical :name (gensym "block-cont"))))
    (list (make-instance 'constant :value '%block)
          original-cont
          (! `(lambda (,cont)
                ,(translate `(progn ,@body)
                            cont
                            (list* (list :block name cont)
                                   env)))))))

(defspecial return-from ((name &optional (result ''nil)) cont env)
  (dolist (e env (error "RETURN-FROM refers to unknown block ~S." name))
    (when (and (eql (first e) :block)
               (eql (second e) name))
      (return (translate result (third e) env)))))

(defspecial unwind-protect ((protected-form &rest cleanup-forms) cont env)
  (translate `(%unwind-protect #'(lambda () ,protected-form)
                               #'(lambda () ,@cleanup-forms))
             cont env))

(defspecial eval-when ((situations &body forms) cont env)
  (multiple-value-bind (compile load eval)
      (sys.int::parse-eval-when-situation situations)
    (declare (ignore compile load))
    (if eval
        (translate `(progn ,@forms) cont env)
        (translate ''nil cont env))))

(defspecial catch ((tag &body body) cont env)
  (translate `(%catch ,tag #'(lambda () ,@body)) cont env))

(defspecial throw ((tag result-form) cont env)
  (translate `(%throw ,tag #'(lambda () ,result-form)) cont env))

(defspecial let ((bindings &body body) cont env)
  (let ((variables '())
        (values '()))
    (dolist (b bindings)
      (multiple-value-bind (name init-form)
          (sys.c::parse-let-binding b)
        (push name variables)
        (push init-form values)))
    (translate `((lambda ,(nreverse variables)
                   ,@body)
                 ,@(nreverse values))
               cont env)))

(defspecial let* ((bindings &body body) cont env)
  (if bindings
      (translate `(let (,(first bindings))
                    (let* ,(rest bindings)
                      ,@body))
                 cont env)
      (translate `(progn ,@body) cont env)))

(defspecial progv ((symbols values &body body) cont env)
  (translate `(%progv ,symbols ,values #'(lambda () ,@body)) cont env))

(defspecial setq ((&rest pairs) cont env)
  ;; (setq) -> 'nil
  ;; (setq x) -> error
  ;; (setq s1 v1) -> (%setq s1 v1)
  ;; (setq s1 v1 s2 v2 ... sn vn) -> (progn (%setq s1 v1)
  ;;                                        (setq s2 v2 ... sn vn))
  (cond ((null pairs)
         (translate ''nil cont env))
        ((null (cdr pairs))
         (error "Odd number of arguments to SETQ."))
        ((null (cddr pairs))
         (translate `(%setq ,(first pairs) ,(second pairs)) cont env))
        (t (translate `(progn (%setq ,(first pairs) ,(second pairs))
                              (setq ,@(cddr pairs)))
                      cont env))))

(defspecial %setq ((variable value) cont env)
  ;; TODO: check for symbol-macro here.
  (let* ((val (make-instance 'lexical :name (gensym "setq-value")))
         (info (find-variable variable env)))
    (cond
      (info
       (setf (getf (plist (cdr info)) 'is-set) t)
       (translate value
                  (! `(clambda (,val)
                        (%setq ,cont ,(cdr info) ,val)))
                  env))
      (t (translate `(funcall #'(setf symbol-value) ,value ',variable)
                    cont env)))))

(defspecial the ((value-type form) cont env)
  (declare (ignore value-type))
  (translate form cont env))

(defspecial tagbody ((&rest statements) cont env)
  (let ((go-tags (remove-if-not 'go-tag-p statements)))
    (list* (make-instance 'constant :value '%tagbody)
           cont
           (do ((i statements (cdr i))
                (forms '())
                (lambdas '()))
               ((null i)
                (let ((tag-mapping (mapcar (lambda (tag)
                                                 (cons tag
                                                       (make-instance 'lexical
                                                                      :name (gensym (format nil "~A-" tag)))))
                                               go-tags))
                      (exit-cont (make-instance 'lexical :name (gensym "tagbody-exit"))))
                  (push (! `(lambda (,exit-cont ,@(mapcar 'cdr tag-mapping))
                              ,(translate `(progn ,@(nreverse forms))
                                          exit-cont
                                          (list* `(:tagbody ,@tag-mapping) env))))
                        lambdas))
                (nreverse lambdas))
             (cond ((go-tag-p (car i))
                    (let ((tag-mapping (mapcar (lambda (tag)
                                                 (cons tag
                                                       (make-instance 'lexical
                                                                      :name (gensym (format nil "~A-" tag)))))
                                               go-tags)))
                      (push (! `(lambda (,(make-instance 'lexical :name (gensym "cont")) ,@(mapcar 'cdr tag-mapping))
                                  ,(translate `(progn ,@(nreverse forms))
                                              (let ((loop (make-instance 'lexical))
                                                    (loop2 (make-instance 'lexical)))
                                                (! `(clambda (,(make-instance 'lexical))
                                                      (,(cdr (assoc (car i) tag-mapping))
                                                        (clambda (,(make-instance 'lexical))
                                                          ((clambda (,loop) (%invoke-continuation ,loop ,loop))
                                                           (clambda (,loop2) (%invoke-continuation ,loop2 ,loop2))))))))
                                              (list* `(:tagbody ,@tag-mapping) env))))
                            lambdas)
                      (setf forms '())))
                   (t (push (car i) forms)))))))

(defspecial go ((tag) cont env)
  (check-type tag (satisfies go-tag-p) "a go tag")
  (dolist (e env (error "GO refers to unknown tag ~S." tag))
    (when (eql (first e) :tagbody)
      (let ((x (assoc tag (rest e))))
        (when x
          (return (list (cdr x)
                        (let ((loop (make-instance 'lexical))
                              (loop2 (make-instance 'lexical)))
                          (! `(clambda (,(make-instance 'lexical))
                                ((clambda (,loop) (%invoke-continuation ,loop ,loop))
                                 (clambda (,loop2) (%invoke-continuation ,loop2 ,loop2)))))))))))))

#+(or)(
((flet) (pass1-flet form env))
((labels) (pass1-labels form env))
((load-time-value) (pass1-load-time-value form env))
((locally) (pass1-locally form env))
((macrolet) (pass1-macrolet form env))
((symbol-macrolet) (pass1-symbol-macrolet form env))
)

(defvar *change-count* nil)

(defun made-a-change ()
  (when *change-count*
    (incf *change-count*)))

(defun bash-with-optimizers (form)
  "Repeatedly run the optimizers on form, returning the optimized form and the number of changes made."
  (let ((total-changes 0))
    (loop
       (let ((*change-count* 0))
         (setf form (optimize-form form (use-map form)))
         (setf form (if-to-select form))
         (setf form (tricky-if (simple-optimize-if form)))
         (multiple-value-bind (new-form target-ifs)
             (hoist-if-branches form)
           (setf form new-form)
           (dolist (target target-ifs)
             (setf form (apply 'replace-if-closure form target))))
         (setf form (optimize-form form (use-map form)))
         (setf form (lower-tagbody form (dynamic-contour-analysis form)))
         (setf form (optimize-form form (use-map form)))
         (setf form (lower-block form (dynamic-contour-analysis form)))
         (when (zerop *change-count*)
           (return))
         (format t "Made ~D changes this iteration.~%" *change-count*)
         (incf total-changes *change-count*)))
    (values (optimize-form form (use-map form)) total-changes)))

(defun translate-and-optimize (lambda)
  (let* ((*gensym-counter* 0)
         (form (convert-assignments (translate-lambda lambda nil))))
    (bash-with-optimizers form)))

(defun check-required-params-only (closure)
  "Ensure that CLOSURE only has required parameters."
  (assert (and (null (closure-optional-params closure))
               (not (closure-rest-param closure))
               (not (closure-keyword-params-enabled closure)))
          (closure)
          "Closure must not have non-required parameters."))
