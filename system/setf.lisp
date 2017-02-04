;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; setf.lisp

(in-package :sys.int)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun get-setf-expansion (place &optional environment)
  (assert (not (and (consp place) (eql (first place) 'values))) (place)
          "SETF VALUES not supported here.")
  (if (consp place)
      (let ((expander (and (symbolp (car place))
                           (get (car place) 'setf-expander)))
            (update-fn (and (symbolp (car place))
                            (get (car place) 'setf-update-fn))))
        (cond
          (expander
           ;; Invoke the exansion function.
           (funcall expander place environment))
          (update-fn
           (let ((vars '())
                 (vals '())
                 (store-sym (gensym)))
             (dolist (arg (cdr place))
               (setf vars (cons (gensym) vars)
                     vals (cons arg vals)))
             (setf vars (nreverse vars)
                   vals (nreverse vals))
             (values vars vals (list store-sym)
                     (append (list update-fn)
                             vars
                             (list store-sym))
                     (list* (car place) vars))))
          (t (multiple-value-bind (expansion expanded-p)
                 (macroexpand-1 place environment)
               (if expanded-p
                   ;; Expand one level of macros.
                   (get-setf-expansion expansion environment)
                   ;; Generate an expansion for a function call form place.
                   (let ((vars '())
                         (vals '())
                         (store-sym (gensym)))
                     (dolist (arg (cdr place))
                       (setf vars (cons (gensym) vars)
                             vals (cons arg vals)))
                     (setf vars (nreverse vars)
                           vals (nreverse vals))
                     (values vars vals (list store-sym)
                             `(funcall #'(setf ,(car place)) ,store-sym ,@vars)
                             (list* (car place) vars))))))))
      (multiple-value-bind (expansion expanded-p)
          (macroexpand-1 place environment)
        (if expanded-p
            ;; Expand symbol macros.
            (get-setf-expansion expansion environment)
            ;; Generate an expansion for a variable name place.
            (let ((store-sym (gensym "STORE")))
              (values '() '() (list store-sym)
                      `(setq ,place ,store-sym)
                      place))))))

(defun expand-setf-values (place value env)
  "Expand a (setf (values foo...) bar) place."
  ;; TODO: Evaluation order & env.
  (let ((tmp-values (loop for i below (length (rest place))
                         collect (gensym "VALUE"))))
    `(multiple-value-bind ,tmp-values ,value
       (setf ,@(loop
                  for p in (rest place)
                  for v in tmp-values
                  collect p
                  collect v))
       ;; Evaluate to all values.
       (values ,@tmp-values))))

(defun expand-setf (place value env)
  "Expand a normal setf place."
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (declare (ignore getter))
    `(let* ,(mapcar #'list vars vals)
       ,(if (or (null stores) (cdr stores))
            `(multiple-value-bind ,stores
                 ,value
               (progn ,setter))
            `(let ((,(car stores) ,value))
               (progn ,setter))))))
)

(defmacro setf (&environment env &rest forms)
  (when forms
    (do* ((i forms (cddr i))
          (code (cons 'progn nil))
          (tail code (cdr tail)))
         ((endp i)
          (if (cddr code)
              code
              (cadr code)))
      (when (endp (cdr i))
        (error "Odd number of forms supplied to SETF."))
      (let ((place (car i))
            (value (cadr i)))
        (setf (cdr tail) (cons (cond ((and (consp place) (eql (first place) 'values))
                                      (expand-setf-values place value env))
                                     (t (expand-setf place value env)))
                               nil))))))

(defmacro push (&environment env item place)
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (let ((item-sym (gensym)))
      `(let* ((,item-sym ,item)
              ,@(mapcar #'list vars vals)
              (,(car stores) (cons ,item-sym ,getter)))
         ,setter))))

(defmacro pop (&environment env place)
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (let ((item-sym (gensym)))
      `(let* (,@(mapcar #'list vars vals)
              (,item-sym ,getter)
              (,(car stores) (cdr ,item-sym)))
         (prog1 (car ,item-sym) ,setter)))))

(defmacro pushnew (&environment env item place &key key test test-not)
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (let ((item-sym (gensym))
          (list-sym (gensym))
          (key-sym (gensym)))
      `(let* ((,item-sym ,item)
              ,@(mapcar #'list vars vals)
              (,list-sym ,getter)
              (,key-sym ,(or key '#'identity)))
         (if (member (funcall ,key-sym ,item-sym) ,list-sym
                     :key ,key-sym
                     ,@(when test (list :test test))
                     ,@(when test-not (list :test-not test-not)))
             ,list-sym
             (let ((,(car stores) (cons ,item-sym ,list-sym)))
               ,setter))))))

(defmacro define-modify-macro (name lambda-list function &optional documentation)
  (multiple-value-bind (required optional rest enable-keys keys allow-other-keys aux)
      (parse-ordinary-lambda-list lambda-list)
    (when (or enable-keys keys allow-other-keys aux)
      (error "&KEYS and &AUX not permitted in define-modify-macro lambda list"))
    (let ((reference (gensym "PLACE"))
          (env (gensym)))
      `(defmacro ,name (&environment ,env ,reference ,@lambda-list)
         ,documentation
         (multiple-value-bind (dummies vals newvals setter getter)
             (get-setf-expansion ,reference ,env)
           (when (cdr newvals)
             (error "Can't expand this"))
           `(let* (,@(mapcar #'list dummies vals)
                   (,(car newvals)
                    ,(list* ',function getter
                            ,@required
                            ,@(mapcar #'car optional)
                            ,@(if rest
                                  (list rest)
                                  (list '())))))
              ,setter))))))

(defmacro define-setf-expander (access-fn lambda-list &body body)
  (let ((whole (gensym "WHOLE"))
        (env (gensym "ENV")))
    (multiple-value-bind (new-lambda-list env-binding)
        (fix-lambda-list-environment lambda-list)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (%define-setf-expander ',access-fn
                                (lambda (,whole ,env)
                                  (declare (lambda-name (setf-expander ,access-fn))
                                           (ignorable ,whole ,env))
                                  ,(expand-destructuring-lambda-list new-lambda-list access-fn body
                                                                     whole `(cdr ,whole)
                                                                     (when env-binding
                                                                       (list `(,env-binding ,env)))
                                                                     :permit-docstring t)))
         ',access-fn))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %define-setf-expander (access-fn expander)
  (setf (get access-fn 'setf-expander) expander
        (get access-fn 'setf-update-fn) nil))
)

(define-modify-macro incf (&optional (delta 1)) +)
(define-modify-macro decf (&optional (delta 1)) -)

;; FIXME...
(defmacro psetf (&rest pairs)
  (when pairs
    (when (null (cdr pairs))
      (error "Odd number of arguments to PSETF"))
    (let ((value (gensym)))
      `(let ((,value ,(cadr pairs)))
         (psetf ,@(cddr pairs))
         (setf ,(car pairs) ,value)
         nil))))

;; FIXME...
(defmacro rotatef (&rest places)
  (when places
    (let ((results '()))
      (dolist (x places)
        (push x results)
        (push x results))
      (push (first places) results)
      (setf results (nreverse results))
      (setf (first results) 'psetf)
      `(progn ,results 'nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %defsetf-short-form (access-fn update-fn documentation)
  (declare (ignore documentation))
  (setf (get access-fn 'setf-expander) nil
        (get access-fn 'setf-update-fn) update-fn)
  access-fn)
)

(defmacro defsetf (access-fn &rest args)
  (cond ((listp (first args))
         `(defsetf-long ,access-fn ,@args))
        (t
         `(defsetf-short ,access-fn ,@args))))

(defmacro defsetf-short (access-fn update-fn &optional documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%defsetf-short-form ',access-fn ',update-fn ',documentation)))

(defmacro defsetf-long (access-fn lambda-list store-variables &body body)
  (when (member '&aux lambda-list)
    (error "&AUX not permitted in DEFSETF lambda list ~S." lambda-list))
  (multiple-value-bind (new-lambda-list env-binding)
      (fix-lambda-list-environment lambda-list)
    (let ((subforms (gensym "SUBFORMS"))
          (env (or env-binding (gensym "ENV"))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (%defsetf-long-form ',access-fn
                             ',(length store-variables)
                             (lambda (,subforms ,env ,@store-variables)
                               (declare (lambda-name (defsetf ,access-fn))
                                        (ignorable ,env))
                               (block ,access-fn
                                 (apply (lambda ,new-lambda-list
                                          ,@body)
                                        ,subforms))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %defsetf-long-form (access-fn store-variable-count expansion-fn)
  (flet ((expand (whole env)
           ;; Generate gensyms for every parameter, and for every store-variable.
           (let* ((param-syms (loop for f in (rest whole) collect (gensym "PARAM")))
                  (store-syms (loop repeat store-variable-count collect (gensym "STORE")))
                  ;; Get the expansion.
                  (expansion (apply expansion-fn param-syms env store-syms)))
             (values param-syms
                     (rest whole)
                     store-syms
                     expansion
                     `(,access-fn ,@param-syms)))))
    (setf (get access-fn 'setf-expander) #'expand
          (get access-fn 'setf-update-fn) nil))
  access-fn)
)

(defmacro shiftf (&rest args)
  (assert args)
  (let ((syms (mapcar (lambda (x)
                        (declare (ignore x))
                        (gensym))
                      args)))
    `(let ,(mapcar #'list syms args)
       ,@(mapcar (lambda (x y) (list 'setf x y))
                 (subseq args 0 (1- (length args)))
                 (rest syms))
       ,(first syms))))

;; Just for now.
(define-setf-expander the (value-type form &environment env)
  (get-setf-expansion form env))

(define-setf-expander apply (function &rest arguments &environment env)
  ;; 5.1.2.5 APPLY Forms as Places.
  ;; FUNCTION must look like #'SYMBOL.
  (unless (and (listp function)
               (= (list-length function) 2)
               (eql (first function) 'function)
               (symbolp (second function)))
    (error "Bad FUNCTION in (SETF APPLY) place."))
  (let ((real-function (second function))
        (store (gensym))
        (vars (loop repeat (length arguments) collect (gensym))))
    (values vars
            arguments
            (list store)
            `(apply #'(setf ,real-function) ,store ,@vars)
            `(apply #',real-function ,@vars))))
