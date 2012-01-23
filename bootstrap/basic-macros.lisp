;;;; This file contains final versions of basic CL macros.
;;;; It should be loaded during bootstrap after the full defmacro
;;;; is installed.

(in-package "SYSTEM.INTERNALS")

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

;;; TODO: Complete psetq. Has to work with setf & symbol-macros.

;;; TODO: DO/DO* with declare support.

(defmacro dolist ((var list-form &optional result-form) &body body)
  (let ((itr (gensym "ITERATOR")))
    `(do* ((,itr (the list ,list-form) (cdr ,itr))
	   (,var (car ,itr) (car ,itr)))
	  ((null ,itr) ,result-form)
       ,@body)))

(defmacro dotimes ((var count-form &optional result-form) &body body)
  (let ((count-val (gensym "COUNT")))
    `(do ((,count-val (the integer ,count-form))
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

(defmacro case (keyform &body cases)
  (let ((test-key (gensym "CASE-KEY")))
    `(let ((,test-key ,keyform))
       (declare (ignoreable ,test-key))
       (cond
	 ,@(mapcar (lambda (clause)
		     (declare (type cons clause))
		     (let ((keys (car clause))
			   (body (cdr clause)))
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

(defmacro ecase (keyform &body cases)
  (let ((test-key (gensym "CASE-KEY"))
	(all-keys '()))
    `(let ((,test-key ,keyform))
       (declare (ignoreable ,test-key))
       (cond
	 ,@(mapcar (lambda (clause)
		     (declare (type cons clause))
		     (let ((keys (car clause))
			   (body (cdr clause)))
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
	 (t (error "~S fell through ECASE expression. Wanted one of ~S" ,test-key ',all-keys))))))

(defmacro typecase (keyform &rest cases)
  (let ((test-key (gensym "CASE-KEY")))
    `(let ((,test-key ,keyform))
       (declare (ignoreable ,test-key))
       (cond
	 ,@(mapcar (lambda (clause)
		     (declare (type cons clause))
		     (let ((keys (car clause))
			   (body (cdr clause)))
		       (cond
			 ((or (eql keys 't)
			      (eql keys 'otherwise))
			  `(t ,@body))
			 ((listp keys)
			  `((or ,@(mapcar (lambda (key)
					    `(typep ,test-key ',key))
					  keys))
			    ,@body))
			 (t `((typep ,test-key ',keys) ,@body)))))
		   cases)))))

(defmacro etypecase (keyform &rest cases)
  (let ((test-key (gensym "CASE-KEY")))
    `(let ((,test-key ,keyform))
       (declare (ignoreable ,test-key))
       (cond
	 ,@(mapcar (lambda (clause)
		     (declare (type cons clause))
		     (let ((keys (car clause))
			   (body (cdr clause)))
		       (cond
			 ((listp keys)
			  `((or ,@(mapcar (lambda (key)
					    `(typep ,test-key ',key))
					  keys))
			    ,@body))
			 (t `((typep ,test-key ',keys) ,@body)))))
		   cases)
	 (t (error "~S fell through ETYPECASE expression." ,test-key))))))

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
  (let ((sym (gensym)))
    `(progn
       ,first-form
       (let ((,sym ,second-form))
	 (progn ,@forms)
	 ,sym))))
