;;;; setf.lisp

(in-package "SYSTEM.INTERNALS")

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
		      (setq vars (cons (gensym) vars)
			    vals (cons arg vals)))
		    (setq vars (nreverse vars)
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

(defmacro pushnew (&environment env item place)
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (let ((item-sym (gensym))
	  (list-sym (gensym)))
      `(let* ((,item-sym ,item)
	      ,@(mapcar #'list vars vals)
	      (,list-sym ,getter)
	      (,(car stores) (cons ,item-sym ,list-sym)))
	 (unless (member ,item-sym ,list-sym)
	   ,setter)))))
