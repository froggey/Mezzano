;;;; This file contains code required to bootstrap the system and will load in other required files.
;;;; When bringing up a fresh bootstrap environment, this should be the first file loaded.
;;;; *DEFAULT-PATHNAME-DEFAULTS* should point at the bootstrap directory in the host Lisp.

(/show0 "Hello, Lisp World!~%")

;;;; Early bootstrap version of DEFUN and DEFMACRO.
;;;; DECLARE & docstrings are not supported, and defmacro
;;;; only does one level of destructuring. &ENVIRONMENT must be at the start
;;;; of the lambda list.

;;; DEFUN.
(funcall #'(setf macro-function)
	 #'(lambda (whole env)
	     (declare (lambda-name early-defun)
		      (ignore env))
	     (let* ((name (car (cdr whole)))
		    (base-name (if (consp name)
				   (car (cdr name))
				   name))
		    (lambda-list (car (cdr (cdr whole))))
		    (body (cdr (cdr (cdr whole)))))
	       (list 'progn
		     (list 'funcall '#'(setf fdefinition)
			   (list 'function (list 'lambda lambda-list
						 (list 'declare (list 'lambda-name name))
						 (list* 'block base-name body)))
			   (list 'quote name))
		     (list 'quote name))))
	 'defun)

;;;; DEFMACRO.
(defun bootstrap-expand-defmacro (lambda-list name body whole)
  (labels ((required-portion (ll current)
	     (if ll
		 (if (eql (car ll) '&optional)
		     (optional-portion (cdr ll) current)
		     (if (eql (car ll) '&rest)
			 (rest-portion (cdr ll) current)
			 (if (eql (car ll) '&body)
			     (rest-portion (cdr ll) current)
			     (cons (list (car ll) (list 'car current))
				   (required-portion (cdr ll) (list 'cdr current))))))))
	   (optional-portion (ll current)
	     (if ll
		 (if (eql (car ll) '&rest)
		     (rest-portion (cdr ll) current)
		     (if (eql (car ll) '&body)
			 (rest-portion (cdr ll) current)
			 ;; TODO: init-forms & supplied-p here.
			 (cons (list (car ll) (list 'if current
						   (list 'car current)))
			   (optional-portion (cdr ll) (list 'cdr current)))))))
	   (rest-portion (ll current)
	     (list (list (car ll) current))))
    (let ((whole-var nil))
      (if (eq '&whole (car lambda-list))
	  (setq whole-var (car (cdr lambda-list))
		lambda-list (cdr (cdr lambda-list))))
      (let ((bindings (required-portion lambda-list (list 'cdr whole))))
	(list 'let (if whole-var
		       (cons (list whole-var whole)
			     bindings)
		       bindings)
	      (list* 'block name body))))))

(defun bootstrap-defmacro (whole env)
  (let ((name (car (cdr whole)))
	(lambda-list (car (cdr (cdr whole))))
	(body (cdr (cdr (cdr whole))))
	(whole (gensym "WHOLE"))
	(env nil))
    (if (eq '&environment (car lambda-list))
	(setq env (car (cdr lambda-list))
	      lambda-list (cdr (cdr lambda-list)))
	(setq env (gensym "ENV")))
    (list 'progn
	  (list 'funcall '#'(setf macro-function)
		(list 'function (list 'lambda (list whole env)
				      (list 'declare
					    (list 'lambda-name (list 'macro-function name))
					    (list 'ignorable whole env))
				      (bootstrap-expand-defmacro lambda-list name body whole)))
		(list 'quote name))
	  (list 'quote name))))

(funcall #'(setf macro-function) #'bootstrap-defmacro 'defmacro)

;;; A bunch of handy CL macros.

(defmacro lambda (lambda-list &body body)
  (list 'function (list* 'lambda lambda-list body)))

(defmacro return (&optional result)
  (list 'return-from 'nil result))

(defmacro when (test &body body)
  (list 'if test (list* 'progn body)))

(defmacro unless (test &body body)
  (list 'if test 'nil (list* 'progn body)))

(defmacro or (&rest forms)
  (if forms
      (let ((sym (gensym)))
	(list 'let (list (list sym (car forms)))
	      (list 'if sym
		    sym
		    (list* 'or (cdr forms)))))
      'nil))

(defmacro and (&rest forms)
  (if forms
      (if (cdr forms)
	  (list 'if (car forms)
		(list* 'and (cdr forms))
		'nil)
	  (list 'let '() (car forms)))
      't))

(defmacro cond (&body clauses)
  (when clauses
    (unless (consp (car clauses))
      (error "COND clause is not a list: ~S." (car clauses)))
    (if (cdr (car clauses))
	(list 'if (car (car clauses))
	      (list* 'progn (cdr (car clauses)))
	      (list* 'cond (cdr clauses)))
	(list 'or (car (car clauses))
	      (list* 'cond (cdr clauses))))))

(defmacro psetq (&rest pairs)
  (when pairs
    (when (null (cdr pairs))
      (error "Odd number of arguments to psetq."))
    (let ((value (gensym "VALUE")))
      (list 'let (list (list value (car (cdr pairs))))
	    (list* 'psetq (cdr (cdr pairs)))
	    (list 'setq (car pairs) value)
	    'nil))))

(defun expand-do (varlist end-test result-forms body let-form set-form)
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
    (list 'block 'nil
	  (list let-form (hack-vars varlist)
		(list 'tagbody
		      loop-head
		      (list 'if end-test
			    (list 'return-from 'nil (list* 'progn result-forms)))
		      (list* 'tagbody body)
		      (list* set-form (set-vars varlist))
		      (list 'go loop-head)))))))

(defmacro do (varlist end &body body)
  (expand-do varlist (car end) (cdr end) body 'let 'psetq))

(defmacro do* (varlist end &body body)
  (expand-do varlist (car end) (cdr end) body 'let* 'setq))

(defmacro dolist (info &body body)
  (let ((var (car info))
	(list (car (cdr info)))
	(result (car (cdr (cdr info))))
	(itr (gensym "ITERATOR")))
    (list* 'do*
	   (list (list itr list (list 'cdr itr))
		 (list var (list 'car itr) (list 'car itr)))
	   (list (list 'null itr) result)
	   body)))

(defmacro dotimes (info &body body)
  (let ((var (car info))
	(count (car (cdr info)))
	(result (car (cdr (cdr info))))
	(count-val (gensym "COUNT")))
    (list* 'do
	   (list (list count-val count)
		 (list var 0 (list '1+ var)))
	   (list (list '>= var count-val) result)
	   body)))

;;; Limited SETF that evaluates its arguments in the wrong order.
(defun %setf (place value)
  (if (symbolp place)
      (list 'setq place value)
      (list* 'funcall
	     (list 'function (list 'setf (car place)))
	     value
	     (cdr place))))

(defmacro setf (&rest forms)
  (do* ((itr forms (cdr (cdr itr)))
	(code (cons 'progn nil))
	(tail code (cdr tail)))
       ((null itr)
	code)
    (funcall #'(setf cdr) (cons (%setf (car itr) (car (cdr itr))) nil) tail)))

(defmacro declaim (&rest declaration-specifiers)
  (labels ((frob (decs)
             (when decs
               (cons (list 'proclaim (list 'quote (car decs)))
                     (frob (cdr decs))))))
    (cons 'eval-when
          (cons '(:compile-toplevel :load-toplevel :execute)
                (frob declaration-specifiers)))))

;;; DEFVAR.
(defmacro defvar (name initial-value)
  (list 'progn
	(list 'declaim (list 'special name))
	(list 'if (list 'not (list 'boundp (list 'quote name)))
	      (list 'setq name initial-value))
	(list 'quote name)))

;;; DEFPARAMETER.
(defmacro defparameter (name initial-value)
  (list 'progn
	(list 'declaim (list 'special name))
	(list 'setq name initial-value)
	(list 'quote name)))

(defmacro defconstant (name initial-value &optional docstring)
  (list 'eval-when '(:compile-toplevel :load-toplevel :execute)
	(list 'declaim (list 'special name))
	(list 'setq name initial-value)
        (list 'declaim (list 'constant name))
	(list 'quote name)))

;; NOTE: incomplete.
(defun equal (x y)
  (cond
    ((eql x y))
    ((stringp x)
     (and (stringp y)
          (string= x y)))
    ((consp x)
     (and (consp y)
	  (equal (car x) (car y))
	  (equal (cdr x) (cdr y))))))

;; A no-op until the package system exists.
(defmacro in-package (name)
  nil)

;; Compiler macros are important for making stuff go fast.
;; Get them running ASAP.
(defmacro define-compiler-macro (name lambda-list &body body)
  (let ((whole (gensym "WHOLE"))
	(env nil))
    (if (eq '&environment (car lambda-list))
	(setq env (car (cdr lambda-list))
	      lambda-list (cdr (cdr lambda-list)))
	(setq env (gensym "ENV")))
    (list 'progn
	  (list 'funcall '#'(setf compiler-macro-function)
		(list 'function (list 'lambda (list whole env)
				      (list 'declare
					    (list 'lambda-name (list 'compiler-macro-function name))
					    (list 'ignorable whole env))
				      (bootstrap-expand-defmacro lambda-list name body whole)))
		(list 'quote name))
	  (list 'quote name))))

(declaim (inline identity))
(defun identity (thing)
  thing)

(declaim (inline complement))
(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))

;;;; Start pulling in basic parts of the system.
(load "early-cons.lisp")
(load "primitive-sequence.lisp")
(load "backquote.lisp")

(load "cons-compiler-macros.lisp")

(defmacro multiple-value-bind (vars values-form &body body)
  (let ((ignore (gensym "IGNORE")))
    `(multiple-value-call #'(lambda (&optional ,@vars &rest ,ignore)
			      (declare (ignore ,ignore))
			      ,@body)
       ,values-form)))

;; TODO: Symbol macros, macroexpand-hook.
(defun macroexpand-1 (form &optional env)
  (if (consp form)
      (let ((mf (macro-function (car form) env)))
	(if mf
	    (values (funcall mf form env) t)
	    (values form nil)))
      (values form nil)))

(defun macroexpand (form &optional env)
  (let ((did-expand nil))
    (do () (nil)
       (multiple-value-bind (expansion expanded-p)
           (macroexpand-1 form env)
         (if expanded-p
             (setf form expansion
                   did-expand t)
             (return (values form did-expand)))))))

(defmacro prog1 (first-form &rest forms)
  (let ((sym (gensym)))
    `(let ((,sym ,first-form))
       (progn ,@forms)
       ,sym)))

(load "setf.lisp")
(load "defmacro.lisp")
(load "basic-macros.lisp")
(load "data-types.lisp")

(defun typep (object type-specifier)
  (ecase type-specifier
    (null (null object))
    (cons (consp object))
    (symbol (symbolp object))
    (list (listp object))))

(load "parse.lisp")
(load "setf-full.lisp")

(defun %defstruct (structure-type)
  (setf (get (structure-name structure-type) 'structure-type) structure-type))

(load "defstruct.lisp")

(defmacro defun (name lambda-list &body body)
  (let ((base-name (if (consp name)
		       (second name)
		       name))
	(declares '()))
    (do ()
	((not (and body
		   (or (and (consp (first body))
			    (eql (first (first body)) 'declare))
		       (and (stringp (first body))
			    (rest body))))))
      (if (consp (first body))
	  (push (pop body) declares)
	  (pop body)))
    `(progn
       (funcall #'(setf fdefinition)
		#'(lambda ,lambda-list
		    ,@(nreverse declares)
		    (declare (lambda-name ,name))
		    (block ,base-name ,@body))
		',name)
       ',name)))

(defun function-symbol (name)
  "Convert a function name to a symbol."
  (cond ((symbolp name) name)
	((and (consp name)
	      (= (list-length name) 2)
	      (eql (first name) 'setf)
	      (symbolp (second name)))
	 (let ((sym (get (second name) 'setf-symbol)))
	   (unless sym
	     (setf sym (make-symbol (symbol-name (second name)))
		   (get (second name) 'setf-symbol) sym))
	   sym))
	(t (error "Invalid function name ~S." name))))

(defun fdefinition (name)
  (symbol-function (function-symbol name)))

(defun (setf fdefinition) (value name)
  (setf (symbol-function (function-symbol name)) value))

(defun compiler-macro-function (name &optional environment)
  (get (function-symbol name) 'compiler-macro-function))

(defun (setf compiler-macro-function) (value name &optional environment)
  (setf (get (function-symbol name) 'compiler-macro-function) value))

(defmacro ignore-errors (&body forms)
  `(progn ,@forms))

(defmacro restart-case (expression &body clauses)
  `(the t ,expression))

(defmacro with-simple-restart ((restart-name format-string &rest format-arguments) &body body)
  `(progn ,@body))

(defun equalp (x y)
  (error "TODO: Equalp."))

(defmacro loop (&body body)
  (let ((head (gensym)))
    `(block nil
       (tagbody ,head
	  (progn ,@body)
	  (go ,head)))))

(defmacro assert (test-form &optional places datum &rest arguments)
  `(unless ,test-form
     (error ,datum ,@arguments)))

(defun warn (datum &rest arguments)
  (if (stringp datum)
      (apply #'format t datum arguments)
      (format t "Warning: ~S ~S~%" datum arguments)))

;; FIXME: This actually depends on CLOS before it's loaded.
(load "type.lisp")
(load "array.lisp")
(load "sequence.lisp")
(load "hash-table.lisp")

;;; Package system loaded here!
(load "packages.lisp")

(setf (symbol-function '%load) #'load)
(defun load (pathname)
  (let ((*package* *package*))
    (%load pathname)))

(load "string.lisp")
(load "reader.lisp")

(defun load (filespec)
  (let ((*package* *package*)
	(file (open filespec)))
    (unwind-protect
	 (do ((form (read file nil file)
		    (read file nil file)))
	     ((eql form file)
	      t)
	   (format t "; Loading ~S~%" form)
	   (eval form))
      (close file))))

(defconstant lambda-list-keywords '(&allow-other-keys
                                    &aux
                                    &body
                                    &environment
                                    &key
                                    &optional
                                    &rest
                                    &whole))

(defun equalp (x y)
  (typecase x
    (character (and (characterp y)
                    (char-equal x y)))
    (number (and (numberp y)
                 (= x y)))
    (cons (and (consp y)
               (equalp (car x) (car y))
               (equalp (cdr x) (cdr y))))
    (vector (and (vectorp y)
                 (eql (length x) (length y))
                 (dotimes (i (length x) t)
                   (when (not (equalp (aref x i) (aref y i)))
                     (return nil)))))
    (array (and (arrayp y)
                (equalp (array-dimensions x) (array-dimensions y))
                (dotimes (i (array-total-size x) t)
                  (when (not (equalp (row-major-aref x i) (row-major-aref y i)))
                    (return nil)))))
    ;; TODO: structures and hash-tables.
    (t (eq x y))))

(defmacro print-unreadable-object ((object stream &rest keys &key type identity) &body body)
  `(%print-unreadable-object ,(when body `(lambda () (progn ,@body))) ,object ,stream ,@keys))

;; Object system.
(load "../closette.lisp")

;; Reload type.lisp.
(load "type.lisp")

;; Conditions and error handling.
(load "condition.lisp")
(load "restarts.lisp")
(load "error.lisp")

;; Compiler.
(load "../lap.lisp")
(load "../lap-x86.lisp")
(load "../compiler/compiler.lisp")
(load "../compiler/pass1.lisp")
(load "../compiler/constprop.lisp")
(load "../compiler/simplify.lisp")
(load "../compiler/lift.lisp")
(load "../compiler/inline.lisp")
(load "../compiler/codegen.lisp")
(load "../compiler/builtins.lisp")

;; Misc stuff.
(load "numbers.lisp")
(load "file-compiler.lisp")
