;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; CLOS User Interface Macros.

(in-package :mezzano.clos)

;;; DEFCLASS.

(defvar *defclass-slot-names*)
(defvar *defclass-options*)

(defmacro defclass (name direct-superclasses direct-slots &rest options)
  (check-type name (and symbol (not null)))
  (dolist (superclass-name direct-superclasses)
    (check-type superclass-name (and symbol (not null))))
  (let ((*defclass-slot-names* '())
        (*defclass-options* '()))
    `(ensure-class ',name
                   :direct-superclasses ',direct-superclasses
                   :direct-slots (list ,@(mapcar #'canonicalize-defclass-direct-slot direct-slots))
                   ,@(mapcan #'canonicalize-defclass-option options))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun check-duplicate-slot-option (slot-name slot-options option)
  (do ((i slot-options (cddr i))
       (saw-option nil))
      ((endp i))
    (when (eql (first i) option)
      (cond (saw-option
             (error 'sys.int::simple-program-error
                    :format-control "Duplicate slot option ~S for slot ~S."
                    :format-arguments (list option slot-name)))
            (t
             (setf saw-option t))))))

(defun check-plist (plist)
  (do ((i plist (cddr i)))
      ((null i))
    (when (not (and (consp i)
                    (symbolp (first i))
                    (consp (rest i))))
      (error 'sys.int::simple-program-error
               :format-control "Malformed plist ~S."
               :format-arguments (list plist)))))

(defun canonicalize-defclass-direct-slot (direct-slot)
  (check-type direct-slot (or (and symbol (not null))
                              cons))
  (cond ((symbolp direct-slot)
         (when (member direct-slot *defclass-slot-names*)
           (error 'sys.int::simple-program-error
               :format-control "Duplicate slot named ~S."
               :format-arguments (list direct-slot)))
         (push direct-slot *defclass-slot-names*)
         `'(:name ,direct-slot))
        (t
         (let ((name (first direct-slot))
               (initform '())
               (initfunction nil)
               (initargs '())
               (readers '())
               (writers '())
               (documentation nil)
               (others '()))
           (check-type name (and symbol (not null)))
           (when (member name *defclass-slot-names*)
             (error 'sys.int::simple-program-error
                    :format-control "Duplicate slot named ~S."
                    :format-arguments (list name)))
           (push name *defclass-slot-names*)
           (check-plist (rest direct-slot))
           (check-duplicate-slot-option name (rest direct-slot) :initform)
           (check-duplicate-slot-option name (rest direct-slot) :allocation)
           (check-duplicate-slot-option name (rest direct-slot) :documentation)
           (check-duplicate-slot-option name (rest direct-slot) :type)
           (do ((i (rest direct-slot) (cddr i)))
               ((endp i))
             (let ((sym (first i))
                   (val (second i)))
               (case sym
                 (:initform
                  (setf initform val
                        initfunction `#'(lambda () ,val)))
                 (:initarg
                  (push val initargs))
                 (:reader
                  (push val readers))
                 (:writer
                  (push val writers))
                 (:accessor
                  (push val readers)
                  (push `(setf ,val) writers))
                 (:documentation
                  (setf documentation val))
                 (t
                  (let ((existing (assoc sym others)))
                    (when (not existing)
                      (setf existing (cons sym '()))
                      (push existing others))
                    (push val (cdr existing)))))))
           `(list :name ',name
                  ,@(when initfunction
                          `(:initform ',initform
                                      :initfunction ,initfunction))
                  ,@(when initargs
                          `(:initargs ',(reverse initargs)))
                  ,@(when readers
                          `(:readers ',(reverse readers)))
                  ,@(when writers
                          `(:writers ',(reverse writers)))
                  ,@(when documentation
                          `(:documentation ',documentation))
                  ,@(loop for (sym . values) in (reverse others)
                       collect `',sym
                       collect (if (rest values)
                                   `',(reverse values)
                                   `',(first values))))))))

(defun canonicalize-defclass-option (option)
  (check-type (first option) symbol)
  (when (member (first option) *defclass-options*)
    (error 'sys.int::simple-program-error
           :format-control "Duplicate class option ~S."
           :format-arguments (list (first option))))
  (push (first option) *defclass-options*)
  (case (first option)
    (:default-initargs
     `(:direct-default-initargs ,(canonicalize-defclass-default-initargs (rest option))))
    (:metaclass
     `(:metaclass ',(second option)))
    (:documentation
     `(:documentation ',(second option)))
    (t
     `(',(first option) ',(rest option)))))

(defun canonicalize-defclass-default-initargs (initargs)
  (check-plist initargs)
  (let ((seen-initargs '()))
    (loop
       for (initarg form) on initargs by #'cddr
       do
         (when (member initarg seen-initargs)
           (error 'sys.int::simple-program-error
                  :format-control "Duplicate default initarg option ~S."
                  :format-arguments (list initarg)))
         (push initarg seen-initargs)))
  `(list ,@(loop
              for (initarg form) on initargs by #'cddr
              collect `(list ',initarg ',form #'(lambda () ,form)))))

)

;;; DEFGENERIC

(defmacro defgeneric (function-name lambda-list &rest options-and-methods)
  (let ((methods (remove :method options-and-methods
                         :key #'first :test-not #'eql))
        (options (remove :method options-and-methods
                         :key #'first))
        (gf (gensym "GF")))
    `(let ((,gf (ensure-generic-function
                 ',function-name
                 :lambda-list ',lambda-list
                 ,@(loop
                      for opt in options
                      append (canonicalize-defgeneric-option opt)))))
       ,@(loop
            for meth in methods
            collect (expand-defgeneric-method function-name meth))
       ,gf)))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun expand-defgeneric-method (name def)
  `(defmethod ,name ,(second def) ,@(cddr def)))

(defun canonicalize-defgeneric-option (option)
  (case (first option)
    ((:documentation
      :generic-function-class
      :method-class)
     `(,(first option) ',(second option)))
    (declare
     ;; FIXME: Declarations must be accumulated.
     ;; FIXME: Some declarations are invalid in DEFGENERIC, and unknown ones
     ;; must be warned about.
     `(:declarations ',(rest option)))
    (:method-combination
     `(:method-combination
       (resolve-method-combination ',(second option) ,@(cddr option))))
    (:argument-precedence-order
     `(:argument-precedence-order ',(rest option)))
    (t
     ;; AMOP doesn't specify what happens to unknown/unsupported options.
     ;; CL says that an implementation must signal an error.
     (error 'sys.int::simple-program-error
              :format-control "Unsupported DEFGENERIC option ~S"
              :format-arguments (list (first option))))))

)

;;; DEFMETHOD.

(defmacro defmethod (function-name &rest args)
  (multiple-value-bind (qualifiers lambda-list specializers function)
        (parse-defmethod function-name args)
    `(defmethod-1 ',function-name
       :lambda-list ',lambda-list
       :qualifiers ',qualifiers
       :specializers ,(canonicalize-specializers specializers)
       :function #',function)))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun canonicalize-specializers (specializers)
  `(list ,@(mapcar #'canonicalize-specializer specializers)))

(defun canonicalize-specializer (specializer)
  (cond ((and (listp specializer)
              (eql (length specializer) 2)
              (eql (first specializer) 'eql))
         `(intern-eql-specializer ,(second specializer)))
        ((symbolp specializer)
         `(find-class ',specializer))
        (t (error "Bad method specializer ~S." specializer))))

(defun parse-defmethod (fn-spec args)
  (let ((qualifiers ())
        (specialized-lambda-list nil)
        (body ())
        (parse-state :qualifiers))
    (dolist (arg args)
       (ecase parse-state
         (:qualifiers
           (if (and (atom arg) (not (null arg)))
               (push-on-end arg qualifiers)
               (progn (setq specialized-lambda-list arg)
                      (setq parse-state :body))))
         (:body (push-on-end arg body))))
    (let ((lambda-list (extract-lambda-list specialized-lambda-list))
          (specializers (extract-specializer-names specialized-lambda-list)))
      (values qualifiers
              lambda-list
              specializers
              (compute-method-lambda lambda-list qualifiers specializers body fn-spec)))))

(defun compute-method-lambda (lambda-list qualifiers specializers body fn-spec)
  (multiple-value-bind (forms declares docstring)
      (sys.int::parse-declares body :permit-docstring t)
    (let* ((form (list* 'block
                        (if (consp fn-spec)
                            (cadr fn-spec)
                            fn-spec)
                        forms))
           (ll (analyze-lambda-list lambda-list))
           (req-args (loop
                        for name in (getf ll :required-names)
                        collect (gensym (string name))))
           (optional-args-p (not (endp (getf ll :optional-args))))
           (rest-arg (if (or optional-args-p
                             (member '&rest lambda-list)
                             (member '&key lambda-list))
                         (gensym "REST")
                         nil))
           (incoming-lambda-list (append req-args
                                         (if rest-arg
                                             `(&rest ,rest-arg)
                                             `()))))
      `(lambda (method next-emfun)
         (lambda ,incoming-lambda-list
           (declare (sys.int::lambda-name (defmethod ,fn-spec ,@qualifiers ,specializers)))
           ,@(when docstring (list docstring))
           (flet ((call-next-method (&rest cnm-args)
                    (if cnm-args
                        (if next-emfun
                            (apply next-emfun cnm-args)
                            (apply #'invoke-no-next-method method cnm-args))
                        (if next-emfun
                            ,(cond (rest-arg
                                    `(apply next-emfun ,@req-args ,rest-arg))
                                   (t
                                    `(funcall next-emfun ,@req-args)))
                            ,(cond (rest-arg
                                    `(apply #'invoke-no-next-method method ,@req-args ,rest-arg))
                                   (t
                                    `(funcall #'invoke-no-next-method method ,@req-args))))))
                  (next-method-p ()
                    (not (null next-emfun))))
             ,(cond (rest-arg
                     `(apply (lambda ,(kludge-arglist lambda-list)
                               (declare (ignorable ,@(getf (analyze-lambda-list lambda-list) :required-names))
                                        ,@declares)
                               ,form)
                             ,@req-args
                             ,rest-arg))
                    (t
                     `(funcall (lambda ,(kludge-arglist lambda-list)
                                 (declare (ignorable ,@(getf (analyze-lambda-list lambda-list) :required-names))
                                          ,@declares)
                                 ,form)
                               ,@req-args)))))))))

;;; N.B. The function kludge-arglist is used to pave over the differences
;;; between argument keyword compatibility for regular functions versus
;;; generic functions.
;;; TODO: This needs to examine the generic function's lambda list to
;;; determine if &allow-other-keys must be added.

(defun kludge-arglist (lambda-list)
  (let* ((plist (analyze-lambda-list lambda-list))
         (requireds (getf plist ':required-names))
         (rv (getf plist ':rest-var))
         (ks (getf plist ':key-args))
         (opts (getf plist ':optional-args))
         (auxs (getf plist ':auxiliary-args)))
    `(,@requireds
      ,@(if opts `(&optional ,@opts) ())
      ,@(if rv `(&rest ,rv) ())
      ,@(if (member '&key lambda-list)
            `(&key ,@ks &allow-other-keys)
            '())
      ,@(if auxs `(&aux ,@auxs) ()))))

;;; Several tedious functions for analyzing lambda lists

(defun extract-lambda-list (specialized-lambda-list)
  (let* ((plist (analyze-lambda-list specialized-lambda-list))
         (requireds (getf plist ':required-names))
         (rv (getf plist ':rest-var))
         (ks (getf plist ':key-args))
         (aok (getf plist ':allow-other-keys))
         (opts (getf plist ':optional-args))
         (auxs (getf plist ':auxiliary-args)))
    `(,@requireds
      ,@(if opts `(&optional ,@opts) ())
      ,@(if rv `(&rest ,rv) ())
      ,@(if (member '&key specialized-lambda-list)
            `(&key ,@ks)
            ())
      ,@(if aok '(&allow-other-keys) ())
      ,@(if auxs `(&aux ,@auxs) ()))))

(defun extract-specializer-names (specialized-lambda-list)
  (let ((plist (analyze-lambda-list specialized-lambda-list)))
    (getf plist ':specializers)))

;;; Note: Also used by Closette proper for method dispatch.

(defun analyze-lambda-list (lambda-list)
  (labels ((make-keyword (symbol)
              (intern (symbol-name symbol)
                      (find-package 'keyword)))
           (get-keyword-from-arg (arg)
              (if (listp arg)
                  (if (listp (car arg))
                      (caar arg)
                      (make-keyword (car arg)))
                  (make-keyword arg))))
    (let ((keys ())           ; Just the keywords
          (key-args ())       ; Keywords argument specs
          (required-names ()) ; Just the variable names
          (required-args ())  ; Variable names & specializers
          (specializers ())   ; Just the specializers
          (rest-var nil)
          (optionals ())
          (auxs ())
          (allow-other-keys nil)
          (state :parsing-required))
      (dolist (arg lambda-list)
        (if (member arg lambda-list-keywords)
          (ecase arg
            (&optional
              (setq state :parsing-optional))
            (&rest
              (setq state :parsing-rest))
            (&key
              (setq state :parsing-key))
            (&allow-other-keys
              (setq allow-other-keys 't))
            (&aux
              (setq state :parsing-aux)))
          (case state
            (:parsing-required
             (push-on-end arg required-args)
             (if (listp arg)
                 (progn (push-on-end (car arg) required-names)
                        (push-on-end (cadr arg) specializers))
                 (progn (push-on-end arg required-names)
                        (push-on-end 't specializers))))
            (:parsing-optional (push-on-end arg optionals))
            (:parsing-rest (setq rest-var arg))
            (:parsing-key
             (push-on-end (get-keyword-from-arg arg) keys)
             (push-on-end arg key-args))
            (:parsing-aux (push-on-end arg auxs)))))
      (list  :required-names required-names
             :required-args required-args
             :specializers specializers
             :rest-var rest-var
             :keywords keys
             :key-args key-args
             :auxiliary-args auxs
             :optional-args optionals
             :allow-other-keys allow-other-keys))))

)
