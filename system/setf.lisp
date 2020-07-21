;;;; setf.lisp

(in-package :mezzano.internals)

(eval-when (:compile-toplevel :load-toplevel :execute)

;; This is also initialized by cold-start.
;; Initializing here is just for the sake of the cold generator.
(defvar *setf-expanders* (make-hash-table))

(defun setf-expander-function (symbol)
  (check-type symbol symbol)
  (gethash symbol *setf-expanders*))

(defun (setf setf-expander-function) (value symbol)
  (check-type symbol symbol)
  (check-type value (or function null))
  (if value
      (setf (gethash symbol *setf-expanders*) value)
      (remhash symbol *setf-expanders*))
  value)

(defun expand-setf-function-call (place environment)
  ;; Generate an expansion for a function call form place.
  (loop
     with store-sym = (gensym)
     for arg in (rest place)
     for var = (gensym)
     when (constantp arg environment)
     collect arg into call-vars
     else
     collect var into call-vars
     and collect var into vars
     and collect arg into vals
     finally
       (return (values vars vals (list store-sym)
                       `(funcall #'(setf ,(car place)) ,store-sym ,@call-vars)
                       (list* (car place) call-vars)))))

(defun get-setf-expansion (place &optional environment)
  (if (consp place)
      (let ((expander (and (symbolp (car place))
                           (setf-expander-function (car place)))))
        (cond
          (expander
           ;; Invoke the exansion function.
           (funcall expander place environment))
          (t (multiple-value-bind (expansion expanded-p)
                 (macroexpand-1 place environment)
               (if expanded-p
                   ;; Expand one level of macros.
                   (get-setf-expansion expansion environment)
                   (expand-setf-function-call expansion environment))))))
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
        (setf (cdr tail) (cons (expand-setf place value env) nil))))))

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
    ;; Use MAKE-SYMBOL instead of GENSYM to avoid producing a name like PLACE1234.
    ;; This is visible in the lambda list and is shown by Slime.
    (let ((reference (make-symbol "PLACE"))
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
                                                                     :permit-docstring t))
                                ',(nth-value 2 (parse-declares body :permit-docstring t)))
         ',access-fn))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %define-setf-expander (access-fn expander documentation)
  (set-setf-docstring access-fn documentation)
  (setf (setf-expander-function access-fn) expander))
)

(define-modify-macro incf (&optional (delta 1)) +)
(define-modify-macro decf (&optional (delta 1)) -)

(defmacro psetf (&environment env &rest pairs)
  ;; Evaluate all subforms before performing assignments.
  (labels ((frob (remaining)
             (when (endp remaining)
               (return-from frob))
             (when (endp (rest remaining))
               (error "Odd number of arguments to PSETF"))
             (let ((place (first remaining))
                   (newvalue (second remaining)))
               (multiple-value-bind (vars vals stores setter getter)
                   (get-setf-expansion place env)
                 (declare (ignore getter))
                 `(let ,(mapcar 'list vars vals)
                    (multiple-value-bind ,stores
                        ,newvalue
                      ,(frob (cddr remaining))
                      ,setter))))))
    `(progn ,(frob pairs) nil)))

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

(defmacro defsetf (access-fn &rest args)
  (cond ((listp (first args))
         `(defsetf-long ,access-fn ,@args))
        (t
         `(defsetf-short ,access-fn ,@args))))

(defmacro defsetf-short (access-fn update-fn &optional documentation)
  (check-type documentation (or null string))
  (let ((args (gensym "ARGS"))
        (value (gensym "VALUE")))
    `(defsetf-long ,access-fn (&rest ,args) (,value)
       ,@(when documentation (list documentation))
       `(,',update-fn ,@,args ,,value))))

(defmacro defsetf-long (access-fn lambda-list store-variables &body body)
  (when (member '&aux lambda-list)
    (error "&AUX not permitted in DEFSETF lambda list ~S." lambda-list))
  (multiple-value-bind (new-lambda-list env-binding)
      (fix-lambda-list-environment lambda-list)
    (let ((subforms (gensym "SUBFORMS"))
          (env (or env-binding (gensym "ENV")))
          (docstring (nth-value 2 (parse-declares body :permit-docstring t))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (%defsetf-long-form ',access-fn
                             ',(length store-variables)
                             (lambda (,subforms ,env ,@store-variables)
                               (declare (lambda-name (defsetf ,access-fn))
                                        (ignorable ,env))
                               (block ,access-fn
                                 (apply (lambda ,new-lambda-list
                                          ,@body)
                                        ,subforms)))
                             ',docstring)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %defsetf-long-form (access-fn store-variable-count expansion-fn documentation)
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
    (%define-setf-expander access-fn #'expand documentation))
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

(define-setf-expander the (value-type form &environment env)
  (multiple-value-bind (temp-vars temp-forms store-vars store-form read-form)
      (get-setf-expansion form env)
    (let ((the-store-vars (loop for v in store-vars collect (gensym))))
      (values temp-vars
              temp-forms
              the-store-vars
              `(multiple-value-bind ,store-vars
                   (the ,value-type (values ,@the-store-vars))
                 nil
                 ,store-form)
              `(the ,value-type ,read-form)))))

(define-setf-expander apply (function &rest arguments)
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

(define-setf-expander values (&rest places &environment env)
  (let ((temp-vars '())
        (temp-forms '())
        (store-vars '())
        (store-forms '())
        (read-forms '()))
    (loop
       for place in places
       do
         (multiple-value-bind (place-temp-vars
                               place-temp-forms
                               place-store-vars
                               place-store-form
                               place-read-form)
             (get-setf-expansion place env)
           (setf temp-vars (append temp-vars place-temp-vars)
                 temp-forms (append temp-forms place-temp-forms))
           (dolist (inner-value (rest place-store-vars))
             (push inner-value temp-vars)
             (push nil temp-forms))
           (setf store-vars (append store-vars (list (first (or place-store-vars
                                                                (list (gensym)))))))
           (setf store-forms (append store-forms (list place-store-form)))
           (setf read-forms (append read-forms (list place-read-form)))))
    (values temp-vars
            temp-forms
            store-vars
            `(values ,@store-forms)
            `(values ,@read-forms))))
