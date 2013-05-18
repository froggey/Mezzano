;;;; setf.lisp

(in-package :sys.int)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun get-setf-expansion (place &optional environment)
  (if (consp place)
      (let ((expander (and (symbolp (car place))
			   (get (car place) 'setf-expander))))
	(if expander
	    ;; Invoke the exansion function.
	    (funcall expander place environment)
	    (multiple-value-bind (expansion expanded-p)
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
			    (list* (car place) vars)))))))
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
	(multiple-value-bind (vars vals stores setter getter)
	    (get-setf-expansion place env)
	  (declare (ignore getter))
	  (funcall #'(setf cdr)
		   (cons
		    `(let ,(mapcar #'list vars vals)
		       ,(if (or (null stores) (cdr stores))
			    `(multiple-value-bind ,stores
				 ,value
			       (progn ,setter))
			    `(let ((,(car stores) ,value))
			       (progn ,setter))))
		    nil)
		   tail))))))

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
	  (list-sym (gensym)))
      `(let* ((,item-sym ,item)
	      ,@(mapcar #'list vars vals)
	      (,list-sym ,getter)
	      (,(car stores) (cons ,item-sym ,list-sym)))
	 (unless (member ,item-sym ,list-sym
                         ,@(when key (list :key key))
                         ,@(when test (list :test test))
                         ,@(when test-not (list :test-not test-not)))
	   ,setter)))))

(defmacro define-modify-macro (name lambda-list function &optional documentation)
  (multiple-value-bind (required optional rest enable-keys keys allow-other-keys aux)
      (parse-ordinary-lambda-list lambda-list)
    (when (or enable-keys keys allow-other-keys aux)
      (error "&KEYS and &AUX not permitted in define-modify-macro lambda list"))
    (let ((reference (gensym)) (env (gensym)))
      `(defmacro ,name (&environment ,env ,reference ,@lambda-list)
	 ,documentation
	 (multiple-value-bind (dummies vals newvals setter getter)
	     (get-setf-expansion ,reference ,env)
	   (when (cdr newvals)
	     (error "Can't expand this"))
	   `(let* (,@(mapcar #'list dummies vals) (,(car newvals)
						   ,(list* ',function getter
                                                           ,@required
                                                           ,@(mapcar #'car optional)
                                                           ,@(when rest (list rest)))))
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
                                                                       (list `(,env-binding ,env))))))
	 ',access-fn))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %define-setf-expander (access-fn expander)
  (setf (get access-fn 'setf-expander) expander))
)

(define-modify-macro incf (&optional (delta 1)) +)
(define-modify-macro decf (&optional (delta 1)) -)

(defun %putf (plist indicator value)
  (do ((i plist (cddr i)))
      ((null i)
       (list* indicator value plist))
    (when (eql (car i) indicator)
      (setf (cadr i) value)
      (return plist))))

(define-setf-expander getf (place indicator &optional default &environment env)
  (multiple-value-bind (temps vals stores
                              store-form access-form)
      (get-setf-expansion place env);Get setf expansion for place.
    (let ((indicator-temp (gensym))
          (store (gensym))     ;Temp var for byte to store.
          (stemp (first stores))) ;Temp var for int to store.
      (when (cdr stores) (error "Can't expand this."))
      ;; Return the setf expansion for LDB as five values.
      (values (list* indicator-temp temps)       ;Temporary variables.
              (list* indicator vals)     ;Value forms.
              (list store)             ;Store variables.
              `(let ((,stemp (%putf ,access-form ,indicator-temp ,store)))
                 ,default
                 ,store-form
                 ,store)               ;Storing form.
              `(getf ,access-form ,indicator-temp ,default))))) ;Accessing form.

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
