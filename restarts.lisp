(in-package :sys.int)

(defparameter *active-restarts* nil)

(defstruct (restart
	     (:constructor make-restart (name function &key interactive-function report-function test-function)))
  name
  function
  interactive-function
  report-function
  test-function)

(defun report-restart (restart stream)
  (let ((report-fn (restart-report-function restart)))
    (if report-fn
	(funcall report-fn stream)
	(restart-name restart))))

(defun test-restart (restart condition)
  (let ((test-fn (restart-test-function restart)))
    (if test-fn
	(funcall test-fn condition)
	t)))

(defun compute-restarts (&optional condition)
  (do* ((i *active-restarts* (cdr i))
	(list (cons nil nil))
	(tail list))
       ((null i) (cdr list))
    (dolist (restart (car i))
      (when (test-restart restart condition)
	(rplacd tail (cons restart nil))
	(setf tail (cdr tail))))))

(defun find-restart (identifier &optional condition)
  (declare (type (or symbol restart) identifier))
  (if (symbolp identifier)
      (dolist (restarts *active-restarts*)
	(dolist (r restarts)
	  (when (and (eql identifier (restart-name r))
		     (test-restart r condition))
	    (return-from find-restart r))))
      identifier))

(defun find-restart-or-die (identifier &optional condition)
  (or (find-restart identifier condition)
      (error "No applicable restart ~S" identifier)))

(defun invoke-restart-interactively (restart)
  (let* ((restart (find-restart-or-die restart))
	 (interactive-function (restart-interactive-function restart))
	 (arguments (if interactive-function
			(funcall interactive-function)
			nil)))
    (apply #'invoke-restart restart arguments)))

(defun invoke-restart (restart &rest arguments)
  (apply (restart-function (find-restart-or-die restart)) arguments))

(defmacro restart-bind (clauses &rest forms)
  `(let ((*active-restarts* (cons (list ,@(mapcar (lambda (clause)
						    (let ((name (car clause)))
						      `(make-restart ',name ,@(cdr clause))))
						  clauses))
				  *active-restarts*)))
     ,@forms))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun handle-restart-case-clause (clause block-name arguments)
  (let ((name (car clause))
	(lambda-list (cadr clause))
	(forms (cddr clause))
	interactive report test
	(label (gensym)))
    (do () ((null forms))
      (case (car forms)
	(:interactive
	 (when interactive
	   (error "Duplicate interactive clause"))
	 (setf interactive `(function ,(cadr forms))
	       forms (cddr forms)))
	(:report
	 (when report
	   (error "Duplicate report clause"))
	 (setf report (if (stringp (cadr forms))
			  `(lambda (stream) (write-string stream ,(cadr forms)))
			  `(function ,(cadr forms)))
	       forms (cddr forms)))
	(:test
	 (when test
	   (error "Duplicate test clause"))
	 (setf test `(function ,(cadr forms))
	       forms (cddr forms)))
	(t (return))))
    (values `(,name #'(lambda (&rest temp)
			(setq ,arguments (copy-list temp))
			(go ,label))
		    ,@(when interactive `(:interactive-function ,interactive))
		    ,@(when report `(:report-function ,report))
		    ,@(when test `(:test-function ,test)))
	    label
	    `(return-from ,block-name
	       (apply #'(lambda ,lambda-list ,@forms) ,arguments)))))

)

(defmacro restart-case (restartable-form &rest clauses)
  (let ((block-name (gensym)) (arguments (gensym))
	(restart-bindings nil) (restart-bodies nil))
    (dolist (clause clauses)
      (multiple-value-bind (binding label body)
	  (handle-restart-case-clause clause block-name arguments)
	(push binding restart-bindings)
	(push body restart-bodies)
	(push label restart-bodies)))
    `(block ,block-name
       (let ((,arguments nil))
	 (tagbody
	    (restart-bind ,restart-bindings
	      (return-from ,block-name ,restartable-form))
	    ,@restart-bodies)))))

(defmacro with-simple-restart ((name format-control &rest format-arguments) &body forms)
  `(restart-case (progn ,@forms)
     (,name ()
       :report (lambda (stream)
		 (format stream ,format-control ,@format-arguments))
       (values nil t))))

(defun abort (&optional condition)
  (invoke-restart (find-restart 'abort condition)))

(defun continue (&optional condition)
  (let ((r (find-restart 'continue condition)))
    (when r
      (invoke-restart r))))

(defun muffle-warning (&optional condition)
  (invoke-restart (find-restart 'muffle-warning condition)))

(defun store-value (value &optional condition)
  (let ((r (find-restart 'store-value condition)))
    (when r
      (invoke-restart r value))))

(defun use-value (value &optional condition)
  (let ((r (find-restart 'use-value condition)))
    (when r
      (invoke-restart r value))))

(defmethod print-object ((object restart) stream)
  (cond (*print-escape*
         (print-unreadable-object (object stream :type t :identity t)
           (write (restart-name object) :stream stream)))
        (t (report-restart object stream))))
