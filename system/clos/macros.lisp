;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; CLOS User Interface Macros.

(in-package :system.closette)

;;; DEFCLASS.

(defmacro defclass (name direct-superclasses direct-slots &rest options)
  (check-type name (and symbol (not null)))
  (dolist (superclass-name direct-superclasses)
    (check-type superclass-name (and symbol (not null))))
  `(ensure-class ',name
                 :direct-superclasses ',direct-superclasses
                 :direct-slots (list ,@(mapcar #'canonicalize-defclass-direct-slot direct-slots))
                 ,@(mapcan #'canonicalize-defclass-option options)))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun canonicalize-defclass-direct-slot (direct-slot)
  (check-type direct-slot (or (and symbol (not null))
                              cons))
  (if (symbolp direct-slot)
      `'(:name ,direct-slot)
      (let ((name (first direct-slot))
            (initform '())
            (initfunction nil)
            (initargs '())
            (readers '())
            (writers '())
            (documentation nil)
            (others '()))
        (check-type name (and symbol (not null)))
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
                                  `',(first values)))))))

(defun canonicalize-defclass-option (option)
  (check-type (first option) symbol)
  (case (first option)
    (:default-initargs
     `(:direct-default-initargs ,(canonicalize-defclass-default-initargs (rest option))))
    (:metaclass
     `(:metaclass ',(second option)))
    (:documentation
     `(:documentation ',(second option)))
    (t
     `(,(first option) ',(rest option)))))

(defun canonicalize-defclass-default-initargs (initargs)
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
    ((:argument-precedence-order
      :documentation
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
    (let ((form (list* 'block
                       (if (consp fn-spec)
                           (cadr fn-spec)
                           fn-spec)
                       forms)))
      `(lambda (args next-emfun)
         (declare (system:lambda-name (defmethod ,fn-spec ,@qualifiers ,specializers)))
         ,@(when docstring (list docstring))
         (flet ((call-next-method (&rest cnm-args)
                  (if (null next-emfun)
                      (error "No next method.")
                      (funcall next-emfun (or cnm-args args))))
                (next-method-p ()
                  (not (null next-emfun))))
           (apply (lambda ,(kludge-arglist lambda-list)
                    (declare ,@declares)
                    ,form)
                  args))))))

;;; N.B. The function kludge-arglist is used to pave over the differences
;;; between argument keyword compatibility for regular functions versus
;;; generic functions.

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
      &key
      ,@ks
      &allow-other-keys
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
      ,@(if (or ks aok) `(&key ,@ks) ())
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
