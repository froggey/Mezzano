;;;; Basic Common Lisp macros

(in-package :mezzano.internals)

(defmacro lambda (lambda-list &body body)
  `#'(lambda ,lambda-list ,@body))

(defmacro return (&optional result)
  `(return-from nil ,result))

(defmacro when (test &body body)
  `(if ,test (progn ,@body)))

(defmacro unless (test &body body)
  `(if ,test 'nil (progn ,@body)))

(defmacro or (&rest forms)
  (if forms
      (if (rest forms)
          (let ((sym (gensym)))
            `(let ((,sym ,(first forms)))
               (if ,sym ,sym (or ,@(rest forms)))))
          ;; Preserve non-toplevelness.
          `(the t ,(first forms)))
      'nil))

(defmacro and (&rest forms)
  (if forms
      (if (rest forms)
          `(if ,(first forms)
               (and ,@(rest forms))
               'nil)
          ;; Preserve non-toplevelness.
          `(the t ,(first forms)))
      't))

(defmacro cond (&body clauses)
  (when clauses
    (let ((c (first clauses)))
      (unless (consp c)
        (error "COND clause is not a list: ~S." c))
      (if (rest c)
          `(if ,(first c)
               (progn ,@(rest c))
               (cond ,@(rest clauses)))
          `(or ,(first c)
               (cond ,@(rest clauses)))))))

(defmacro psetq (&rest pairs)
  ;; Make sure all variables are symbols, then hand off to PSETF.
  (loop
     for var in pairs by #'cddr
     do (check-type var symbol))
  `(psetf ,@pairs))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun expand-do (varlist end-test result-forms body let-form set-form)
  (multiple-value-bind (body-forms declares)
      (parse-declares body)
    (let ((loop-head (gensym "HEAD")))
      (labels ((hack-vars (list)
                 (when list
                   (cons (let* ((vardef (car list))
                                (name (if (consp vardef)
                                          (car vardef)
                                          vardef)))
                           (unless (symbolp name)
                             (error "DO step variable is not a symbol: ~S." name))
                           (list name (if (consp vardef)
                                          (car (cdr vardef))
                                          'nil)))
                         (hack-vars (cdr list)))))
               (set-vars (list)
                 (when list
                   (if (and (consp (car list)) (cdr (cdr (car list))))
                       (let ((name (car (car list)))
                             (step-form (car (cdr (cdr (car list))))))
                         (when (cdr (cdr (cdr (car list))))
                           (error "Invalid form in DO variable list: ~S." (car list)))
                         (list* name step-form
                                (set-vars (cdr list))))
                       (set-vars (cdr list))))))
        `(block nil
           (,let-form ,(hack-vars varlist)
              (declare ,@declares)
              (tagbody
                 ,loop-head
                 (if ,end-test (return-from nil (progn ,@result-forms)))
                 (tagbody ,@body-forms)
                 (,set-form ,@(set-vars varlist))
                 (go ,loop-head))))))))
)

(defmacro do (varlist end &body body)
  (expand-do varlist (car end) (cdr end) body 'let 'psetq))

(defmacro do* (varlist end &body body)
  (expand-do varlist (car end) (cdr end) body 'let* 'setq))

(defmacro dolist ((var list-form &optional result-form) &body body)
  (multiple-value-bind (body-forms declares)
      (parse-declares body)
    (let ((itr (gensym "ITERATOR"))
          (head (gensym "HEAD")))
      `(block nil
         (let ((,itr ,list-form))
           (tagbody ,head
              (if (null ,itr)
                  (return (let ((,var nil))
                            (declare ,@declares
                                     (ignorable ,var))
                            ,result-form)))
              (let ((,var (car ,itr)))
                (declare ,@declares)
                (tagbody ,@body-forms))
              (setq ,itr (cdr ,itr))
              (go ,head)))))))

(defmacro dotimes ((var count-form &optional result-form) &body body)
  (let ((count-val (gensym "COUNT")))
    `(do ((,count-val (let ((,count-val ,count-form))
                        (check-type ,count-val integer)
                        ,count-val))
          (,var 0 (1+ ,var)))
         ((>= ,var ,count-val) ,result-form)
       ,@body)))

(defmacro multiple-value-bind (vars values-form &body body)
  (let ((ignore (gensym "IGNORE")))
    `(multiple-value-call #'(lambda (&optional ,@vars &rest ,ignore)
                              (declare (ignore ,ignore))
                              ,@body)
       ,values-form)))

(defmacro multiple-value-list (value-form)
  `(multiple-value-call #'list ,value-form))

(defmacro nth-value (n form)
  (cond ((eql n 0)
         `(values ,form))
        ((and (integerp n)
              (<= 0 n 32)) ; Don't generate a zillion temporaries for large N.
         (let ((temps (loop
                         repeat n
                         collect (gensym)))
               (value (gensym "VALUE")))
           `(multiple-value-bind (,@temps ,value)
                ,form
              (declare (ignore ,@temps))
              ,value)))
        (t
         (let ((n-value (gensym "N"))
               (values (gensym "VALUES")))
           `(let ((,n-value ,n))
              (flet ((nth-value-closure (&rest ,values)
                       (declare (dynamic-extent ,values))
                       (nth ,n-value ,values)))
                (declare (dynamic-extent #'nth-value-closure))
                (multiple-value-call #'nth-value-closure ,form)))))))

(defmacro case (keyform &body cases)
  (let ((test-key (gensym "CASE-KEY")))
    `(let ((,test-key ,keyform))
       (declare (ignorable ,test-key))
       (cond
         ,@(mapcar (lambda (clause)
                     (declare (type cons clause))
                     (let ((keys (car clause))
                           ;; Empty body should evaluate to nil.
                           (body (or (cdr clause) '(nil))))
                       (cond
                         ((or (eq keys 't)
                              (eq keys 'otherwise))
                          `(t ,@body))
                         ((listp keys)
                          `((or ,@(mapcar (lambda (key)
                                            `(eql ',key ,test-key))
                                          keys))
                            ,@body))
                         (t `((eql ',keys ,test-key) ,@body)))))
                   cases)))))

;;; Generate a jump table for all-integer key sets.
(define-compiler-macro case (&whole whole keyform &body cases)
;;(defun case-cm (whole keyform &rest cases)
  (let ((keys (loop for (keys . forms) in cases
                 when (listp keys) append keys
                 else collect keys))
        (default-form '()))
    (setf default-form (rest (find-if (lambda (x) (member x '(t otherwise))) cases
                                      :key #'first)))
    (setf keys (remove-if (lambda (x) (member x '(t otherwise))) keys))
    (setf cases (remove-if (lambda (x) (member x '(t otherwise))) cases
                           :key #'first))
    (if (and keys
             (every #'integerp keys))
        (let* ((unique-keys (remove-duplicates keys))
               (n-keys (length unique-keys))
               (min-key (apply #'min unique-keys))
               (max-key (apply #'max unique-keys))
               (range (- (1+ max-key) min-key)))
          (if (and (>= n-keys mezzano.compiler::*jump-table-size-min*)
                   (< range mezzano.compiler::*jump-table-size-max*))
              (let ((default-label (gensym "case-default"))
                    (block-name (gensym "case-block"))
                    (key-sym (gensym "case-key"))
                    (key-labels nil)
                    (form-and-labels nil))
                (loop for (keys . forms) in cases do
                     (let ((form-sym (gensym (format nil "ecase-~S" keys))))
                       (push (list form-sym `(return-from ,block-name (progn ,@forms))) form-and-labels)
                       (dolist (key (if (listp keys) keys (list keys)))
                         (unless (assoc key key-labels)
                           (push (list key form-sym) key-labels)))))
                `(block ,block-name
                   (let ((,key-sym ,keyform))
                     (tagbody
                        (if (and (,(if (typep range '(signed-byte 61))
                                       'fixnump
                                       'integerp)
                                   ,key-sym)
                                 (<= ',min-key ,key-sym)
                                 (<= ,key-sym ',max-key))
                            (%jump-table (- ,key-sym ',min-key)
                                         ,@(loop for i below range
                                              collect (let ((label (assoc (+ i min-key) key-labels)))
                                                        (if label
                                                            `(go ,(second label))
                                                            `(go ,default-label)))))
                            (go ,default-label))
                        ,default-label
                        (return-from ,block-name
                          (progn ,@default-form))
                        ,@(apply #'append form-and-labels)))))
              whole))
        whole)))

(defmacro ecase (keyform &body cases)
  (let ((test-key (gensym "CASE-KEY"))
        (all-keys '()))
    `(let ((,test-key ,keyform))
       (declare (ignorable ,test-key))
       (cond
         ,@(mapcar (lambda (clause)
                     (declare (type cons clause))
                     (let ((keys (car clause))
                           ;; Empty body should evaluate to nil.
                           (body (or (cdr clause) '(nil))))
                       (cond
                         ((listp keys)
                          `((or ,@(mapcar (lambda (key)
                                            (push key all-keys)
                                            `(eql ',key ,test-key))
                                          keys))
                            ,@body))
                         (t (push keys all-keys)
                            `((eql ',keys ,test-key) ,@body)))))
                   cases)
         (t (error 'simple-type-error
                   :expected-type '(member ,@all-keys)
                   :datum ,test-key
                   :format-control "~S fell through ECASE form"
                   :format-arguments (list ,test-key)))))))

;;; Generate a jump table for all-integer key sets.
(define-compiler-macro ecase (&whole whole keyform &body cases)
  (let ((keys (loop for (keys . forms) in cases
                 when (listp keys) append keys
                 else collect keys)))
    (if (and keys
             (every #'integerp keys))
        (let* ((unique-keys (remove-duplicates keys))
               (n-keys (length unique-keys))
               (min-key (apply #'min unique-keys))
               (max-key (apply #'max unique-keys))
               (range (- (1+ max-key) min-key)))
          (if (and (>= n-keys mezzano.compiler::*jump-table-size-min*)
                   (< range mezzano.compiler::*jump-table-size-max*))
              (let ((default-label (gensym "ecase-default"))
                    (block-name (gensym "ecase-block"))
                    (key-sym (gensym "ecase-key"))
                    (key-labels nil)
                    (form-and-labels nil))
                (loop for (keys . forms) in cases do
                     (let ((form-sym (gensym (format nil "ecase-~S" keys))))
                       (push (list form-sym `(return-from ,block-name (progn ,@forms))) form-and-labels)
                       (dolist (key (if (listp keys) keys (list keys)))
                         (unless (assoc key key-labels)
                           (push (list key form-sym) key-labels)))))
                `(block ,block-name
                   (let ((,key-sym ,keyform))
                     (tagbody
                        (if (and (,(if (typep range '(signed-byte 61))
                                       'fixnump
                                       'integerp)
                                   ,key-sym)
                                 (<= ',min-key ,key-sym)
                                 (<= ,key-sym ',max-key))
                            (%jump-table (- ,key-sym ',min-key)
                                         ,@(loop for i below range
                                              collect (let ((label (assoc (+ i min-key) key-labels)))
                                                        (if label
                                                            `(go ,(second label))
                                                            `(go ,default-label)))))
                            (go ,default-label))
                        ,default-label
                        (error 'simple-type-error
                               :expected-type '(member ,@keys)
                               :datum ,key-sym
                               :format-control "~S fell through ECASE form"
                               :format-arguments (list ,key-sym))
                        ,@(apply #'append form-and-labels)))))
              whole))
        whole)))

(defun ccase-error (keyplace key all-keys)
  (restart-case
      (error 'simple-type-error
             :expected-type `(member ,@all-keys)
             :datum key
             :format-control "~S fell through CCASE form"
             :format-arguments (list key))
    (store-value (new-key)
      :interactive (lambda ()
                     (format t "Enter a new value (evaluated): ")
                     (list (eval (read))))
      :report (lambda (s)
                (format s "Input a new value for ~S." keyplace))
      new-key)))

(defmacro ccase (keyplace &rest cases)
  (let ((block (gensym "CCASE-BLOCK"))
        (key (gensym "CCASE-KEY"))
        (loop (gensym "CCASE-LOOP"))
        (all-keys '()))
    `(block ,block
       (let ((,key ,keyplace))
         (declare (ignorable ,key))
         (tagbody
            ,loop
            (cond
              ,@(loop
                   for (keys . body) in cases
                   ;; Collect keys.
                   do (if (listp keys)
                          (setf all-keys (union all-keys keys))
                          (pushnew keys all-keys))
                   collect
                     `((member ,key ',(if (listp keys)
                                          keys
                                          (list keys)))
                       ;; Empty body should evaluate to nil.
                       (return-from ,block
                         (progn ,@(or body '(nil))))))
              (t
               (setf ,key (ccase-error ',keyplace ,key ',all-keys)
                     ,keyplace ,key)
               (go ,loop))))))))

(defmacro typecase (keyform &rest cases)
  (let ((test-key (gensym "CASE-KEY")))
    `(let ((,test-key ,keyform))
       (declare (ignorable ,test-key))
       (cond
         ,@(mapcar (lambda (clause)
                     (declare (type cons clause))
                     (let ((keys (car clause))
                           (body (cdr clause)))
                       (cond
                         ((or (eql keys 't)
                              (eql keys 'otherwise))
                          `(t nil ,@body))
                         (t `((typep ,test-key ',keys) nil ,@body)))))
                   cases)))))

(defmacro etypecase (keyform &rest cases)
  (let ((test-key (gensym "CASE-KEY")))
    `(let ((,test-key ,keyform))
       (declare (ignorable ,test-key))
       (cond
         ,@(mapcar (lambda (clause)
                     (declare (type cons clause))
                     (let ((key (car clause))
                           (body (cdr clause)))
                       `((typep ,test-key ',key) nil ,@body)))
                   cases)
         (t (error 'simple-type-error
                   :expected-type '(or ,@(mapcar #'first cases))
                   :datum ,test-key
                   :format-control "~S fell through ETYPECASE form"
                   :format-arguments (list ,test-key)))))))

(defun ctypecase-error (keyplace key types)
  (restart-case
      (error 'simple-type-error
             :expected-type `(or ,@types)
             :datum key
             :format-control "~S fell through CTYPECASE form"
             :format-arguments (list key))
    (store-value (new-key)
      :interactive (lambda ()
                     (format t "Enter a new value (evaluated): ")
                     (list (eval (read))))
      :report (lambda (s)
                (format s "Input a new value for ~S." keyplace))
      new-key)))

(defmacro ctypecase (keyplace &rest cases)
  (let ((block (gensym "CTYPECASE-BLOCK"))
        (key (gensym "CCASE-KEY"))
        (loop (gensym "CCASE-LOOP"))
        (all-keys (mapcar #'first cases)))
    `(block ,block
       (let ((,key ,keyplace))
         (declare (ignorable ,key))
         (tagbody
            ,loop
            (cond
              ,@(loop
                   for (type . body) in cases
                   collect
                     `((typep ,key ',type)
                       ;; Empty body should evaluate to nil.
                       (return-from ,block
                         (progn ,@(or body '(nil))))))
              (t
               (setf ,key (ctypecase-error ',keyplace ,key ',all-keys)
                     ,keyplace ,key)
               (go ,loop))))))))

(defmacro declaim (&rest declaration-specifiers)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (lambda (dec) `(proclaim ',dec)) declaration-specifiers)))

(defmacro prog1 (first-form &rest forms)
  "Evaluate FIRST-FORM, then FORMS in order; returning the value of FIRST-FORM."
  (let ((sym (gensym)))
    `(let ((,sym ,first-form))
       (progn ,@forms)
       ,sym)))

(defmacro prog2 (first-form second-form &rest forms)
  "Evaluate FIRST-FORM, SECOND-FORM, then FORMS in order; returning the value of SECOND-FORM."
  `(prog1 (progn ,first-form ,second-form) ,@forms))

(defmacro declaim (&rest declaration-specifiers)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (lambda (x) `(proclaim ',x)) declaration-specifiers)))

(defmacro current-source-location (&optional name)
  `(lambda () (declare (lambda-name ,(or name 'source-location)))))

;;; DEFVAR.
(defmacro defvar (name &optional (initial-value nil initial-valuep) (docstring nil docstringp))
  (when docstringp
    (check-type docstring string))
  `(progn
     (declaim (special ,name))
     ,@(when initial-valuep
         `((unless (boundp ',name)
             (setq ,name ,initial-value))))
     ,@(when docstringp
         `((set-variable-docstring ',name ',docstring)))
     (set-variable-source-location ',name (current-source-location (defvar ,name)) 'defvar)
     ',name))

;;; DEFPARAMETER.
(defmacro defparameter (name initial-value &optional (docstring nil docstringp))
  (when docstringp
    (check-type docstring string))
  `(progn
     (declaim (special ,name))
     (setq ,name ,initial-value)
     ,@(when docstringp
         `((set-variable-docstring ',name ',docstring)))
     (set-variable-source-location ',name (current-source-location (defparameter ,name)) 'defparameter)
     ',name))

(defmacro defconstant (name initial-value &optional (docstring nil docstringp))
  (when docstringp
    (check-type docstring string))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%defconstant ',name ,initial-value
                   (current-source-location (defconstant ,name))
                   ,@(when docstring `(',docstring)))))

(defmacro define-symbol-macro (symbol expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%define-symbol-macro ',symbol ',expansion (current-source-location (define-symbol-macro ,symbol)))))

(defmacro defglobal (name &optional (initial-value nil initial-valuep) (docstring nil docstringp))
  (when docstringp
    (check-type docstring string))
  `(progn
     (declaim (global ,name))
     ,@(when initial-valuep
         `((unless (boundp ',name)
             (setq ,name ,initial-value))))
     ,@(when docstringp
         `((set-variable-docstring ',name ',docstring)))
     (set-variable-source-location ',name (current-source-location (defglobal ,name)) 'defglobal)
     ',name))

(defmacro defun (&environment env name lambda-list &body body)
  (let ((base-name (if (consp name)
                       (second name)
                       name)))
    (multiple-value-bind (body-forms declares docstring)
        (parse-declares body :permit-docstring t)
      (let ((the-lambda `(lambda ,lambda-list
                           (declare ,@declares
                                    (lambda-name ,name))
                           ,docstring
                           (block ,base-name ,@body-forms))))
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             ;; Don't emit source information if there's an environment.
             ;; Currently inlining a DEFUN defined in a macrolet doesn't work.
             (%compiler-defun ',name ',(if env 'nil the-lambda)))
           (%defun ',name ,the-lambda ',docstring)
           ',name)))))

;; Like DEFUN, but behave as though the function is defined in the empty
;; environment for the purposes of inlining.
;; This is to work around DEFUN not being able to inline functions
;; that are wrapped in a MACROLET.
(defmacro defun* (name lambda-list &body body)
  (macroexpand-1 `(defun ,name ,lambda-list ,@body) nil))

(defmacro prog (variables &body body)
  (multiple-value-bind (body-forms declares)
      (parse-declares body)
    `(block nil
       (let ,variables
         (declare ,@declares)
         (tagbody ,@body-forms)))))

(defmacro prog* (variables &body body)
  (multiple-value-bind (body-forms declares)
      (parse-declares body)
    `(block nil
       (let* ,variables
         (declare ,@declares)
         (tagbody ,@body-forms)))))

(defmacro multiple-value-setq (vars form)
  (dolist (v vars)
    (check-type v symbol))
  ;; Always return the primary value of FORM.
  ;; (SETF VALUES) will return the variables as values.
  (if vars
      `(values (setf (values ,@vars) ,form))
      `(values ,form)))

(defmacro unwind-protect-unwind-only (protected-form &body cleanup-forms)
  "Like UNWIND-PROTECT, but CLEANUP-FORMS are not executed if a normal return occurs."
  (let ((abnormal-return (gensym "ABNORMAL-RETURN")))
    `(let ((,abnormal-return t))
       (unwind-protect
            (multiple-value-prog1
                ,protected-form
              (setf ,abnormal-return nil))
         (when ,abnormal-return
           ,@cleanup-forms)))))
