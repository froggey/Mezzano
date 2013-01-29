;;;; Translate CL into IR1.

(in-package #:sys.newc)

(defvar *special-form-translators* (make-hash-table :test 'eq))
(defvar *print-ir-like-cl* t
  "Set to true if the IR printer should use ()' instead of {}? when printing.")

(defclass constant ()
  ((value :initarg :value :reader constant-value)
   (plist :initform '() :accessor plist))
  (:documentation "A constant value."))

(defclass lexical ()
  ((name :initarg :name :reader lexical-name)
   (plist :initform '() :accessor plist))
  (:documentation "A lexical variable.")
  (:default-initargs :name (gensym "var")))

(defclass closure ()
  ((name :initarg :name :reader closure-name)
   (required-params :initarg :required-params :reader closure-required-params)
   ;; Body of the closure, a function application.
   (body :initarg :body :reader closure-body)
   (plist :initform '() :accessor plist))
  (:documentation "A closure.")
  (:default-initargs :name nil))

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
      (format stream "~A~:<~;LAMBDA ~:S~1I ~_~A~/CL:PPRINT-FILL/~A~;~:>~A"
              (if *print-ir-like-cl* #\( #\{)
              (list (closure-required-params o)
                    (if *print-ir-like-cl* #\( #\{)
                    (closure-body o)
                    (if *print-ir-like-cl* #\) #\}))
              (if *print-ir-like-cl* #\) #\}))
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
     (assert (eql (first code) 'lambda))
     (assert (listp (third code)))
     (make-instance 'closure
                    :required-params (second code)
                    :body (mapcar '! (third code))))
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
        (list cont (cdr info))
        (translate `(symbol-value ',form) cont env))))

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
                   (! `(lambda (,arg)
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
                        (! `(lambda (,v)
                              ,(translate-progn (rest forms) cont env)))
                        env)))))

(defun translate-lambda (lambda env)
  (multiple-value-bind (body lambda-list declares name docstring)
      (sys.c::parse-lambda lambda)
    (multiple-value-bind (required optional rest enable-keys keys allow-other-keys aux)
	(sys.int::parse-ordinary-lambda-list lambda-list)
      (when (or optional rest enable-keys aux)
        (error "TODO: nontrivial lambda-lists."))
      (let ((lambda-cont (make-instance 'lexical :name (gensym "cont")))
            (required-args (mapcar (lambda (x) (make-instance 'lexical :name x))
                                   required)))
        (make-instance 'closure
                       :name name
                       :required-params (list* lambda-cont required-args)
                       :body (translate-progn body
                                              lambda-cont
                                              (list* `(:bindings ,@(pairlis required required-args)) env)))))))

(defspecial if ((test-form then-form &optional (else-form ''nil)) cont env)
  (let ((test (make-instance 'lexical :name (gensym "test")))
        (cont-arg (make-instance 'lexical :name (gensym "cont"))))
    (translate test-form
               (! `(lambda (,test)
                     ((lambda (,cont-arg)
                        (%if (lambda () ,(translate then-form cont-arg env))
                             (lambda () ,(translate else-form cont-arg env))
                             ,test))
                      ,cont)))
               env)))

(defspecial progn ((&rest forms) cont env)
  (translate-progn forms cont env))

(defspecial quote ((object) cont env)
  (list cont (make-instance 'constant :value object)))

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
                  (! `(lambda (,val)
                        (%setq ,cont ,(cdr info) ,val)))
                  env))
      (t (translate `(funcall #'(setf symbol-value) ,value ',variable)
                    cont env)))))

(defspecial the ((value-type form) cont env)
  (declare (ignore value-type))
  (translate form cont env))

#+(or)(
((flet) (pass1-flet form env))
((go) (pass1-go form env))
((labels) (pass1-labels form env))
((load-time-value) (pass1-load-time-value form env))
((locally) (pass1-locally form env))
((macrolet) (pass1-macrolet form env))
((symbol-macrolet) (pass1-symbol-macrolet form env))
((tagbody) (pass1-tagbody form env))
)

(defun translate-and-optimize (lambda)
  (let ((form (convert-assignments (translate-lambda lambda nil))))
    (setf form (optimize-form form (use-map form)))
    (loop
       (setf form (tricky-if (simple-optimize-if form)))
       (multiple-value-bind (new-form target-ifs)
           (hoist-if-branches form)
         (setf form new-form)
         (when (endp target-ifs)
           (return))
         (dolist (target target-ifs)
           (setf form (apply 'replace-if-closure form target))))
       (setf form (optimize-form form (use-map form))))
    (optimize-form form (use-map form))))
