(in-package #:sys.int)

(defvar *scheduler-stack-group* nil)
(defvar *current-process* nil)
(defvar *active-processes* nil)

(defclass base-process ()
  ((name :initarg :name
	 :reader process-name)
   (run-reasons :initarg :run-reasons
		:accessor process-run-reasons)
   (arrest-reasons :initarg :arrest-reasons
		   :accessor process-arrest-reasons)
   (wait-function :initarg :wait-function
		  :accessor process-wait-function)
   (wait-argument-list :initarg :wait-argument-list
		       :accessor process-wait-argument-list)
   (whostate :initarg :whostate
	     :accessor process-whostate)
   (initial-form :initarg :initial-form
		 :accessor process-initial-form))
  (:default-initargs :run-reasons '() :arrest-reasons '()
                     :wait-function (constantly 'nil)
                     :wait-argument-list '()
                     :whostate '()
                     :initial-form nil))

(defmethod print-object ((object base-process) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (process-name object))))

(defclass process (base-process)
  ((stack-group :initarg :stack-group
		:accessor process-stack-group)
   (initial-stack-group :reader process-initial-stack-group))
  (:default-initargs :initform nil))

(defclass simple-process (base-process)
  ())

(defgeneric process-run-reason (process object)
  (:documentation "Add OBJECT to PROCESS's run reasons."))
(defgeneric process-revoke-run-reason (process object)
  (:documentation "Remove OBJECT from PROCESS's run reasons."))

(defgeneric process-arrest-reason (process object)
  (:documentation "Add OBJECT to PROCESS's arrest reasons."))
(defgeneric process-revoke-arrest-reason (process object)
  (:documentation "Remove OBJECT from PROCESS's arrest reasons."))

(defgeneric process-runnable-p (process)
  (:documentation "Returns true when the process has at least one run reason and no arrest reasons."))

(defgeneric process-preset (process function &rest arguments)
  (:documentation "Sets the process's initial function and arguments, then resets the process."))

(defgeneric process-reset (process)
  (:documentation "Dump the process's current state. When it next runs, it will invoke the initial function."))

(defmethod process-preset ((process base-process) function &rest arguments)
  (setf (process-initial-form process) (cons function arguments))
  (process-reset process))

(defmethod process-reset :before ((process base-process))
  (setf (process-wait-function process) nil
	(process-wait-argument-list process) nil))

(defmethod process-reset ((process simple-process))
  nil)

(defmethod process-reset ((process process))
  (setf (process-stack-group process) (process-initial-stack-group process))
  (stack-group-preset (process-initial-stack-group process)
                      (lambda ()
                        (apply (first (process-initial-form process))
                               (rest (process-initial-form process)))
                        (process-disable process)
                        (stack-group-invoke *scheduler-stack-group*))))

(defmethod process-run-reason ((process base-process) object)
  (pushnew object (process-run-reasons process))
  (process-consider-runnability process))

(defmethod process-revoke-run-reason ((process base-process) object)
  (setf (process-run-reasons process) (remove object (process-run-reasons process)))
  (process-consider-runnability process))

(defmethod process-arrest-reason ((process base-process) object)
  (pushnew object (process-arrest-reasons process))
  (process-consider-runnability process))

(defmethod process-revoke-arrest-reason ((process base-process) object)
  (setf (process-run-reasons process) (remove object (process-arrest-reasons process)))
  (process-consider-runnability process))

(defun process-wait (reason function &rest arguments)
  (declare (dynamic-extent arguments))
  (if (and *current-process*
	   *scheduler-stack-group*
	   (not (eql (stack-group-state *scheduler-stack-group*) :active)))
      (progn
	(setf (process-whostate *current-process*) reason
	      (process-wait-function *current-process*) function
	      (process-wait-argument-list *current-process*) arguments)
	(stack-group-invoke *scheduler-stack-group*))
      (do ()
	  ((apply function arguments))
	(%hlt))))

;; todo, Fix this...
(defun process-wait-with-timeout (reason timeout function &rest arguments)
  (declare (dynamic-extent arguments))
  (process-wait reason (lambda ()
			 (if (<= (decf timeout) 0)
			     t
			     (apply function arguments))))
  (<= timeout 0))

(defmethod initialize-instance :after ((instance simple-process))
  (process-consider-runnability instance))

(defmethod initialize-instance :after ((instance process) &key name stack-group)
  (unless stack-group
    (setf (slot-value instance 'stack-group) (make-stack-group name)))
  (setf (slot-value instance 'initial-stack-group) (slot-value instance 'stack-group))
  (process-consider-runnability instance))

(defun process-consider-runnability (process)
  (cond ((or (process-arrest-reasons process)
	     (null (process-run-reasons process)))
	 (setf *active-processes* (remove process *active-processes*)))
	(t (pushnew process *active-processes*))))

(defun process-enable (process)
  (process-disable process)
  (process-run-reason process :enable))

(defun process-reset-and-enable (process)
  (process-reset process)
  (process-enable process))

(defun process-disable (process)
  (setf (process-run-reasons process) nil
	(process-arrest-reasons process) nil)
  (process-consider-runnability process))

(defun get-next-process (current)
  (dolist (proc (member current *active-processes*))
    (let ((wait-fn (process-wait-function proc))
          (wait-args (process-wait-argument-list proc)))
      (when (or (null wait-fn) (apply wait-fn wait-args))
        (setf (process-wait-function proc) nil
              (process-wait-argument-list proc) nil)
        (return-from get-next-process proc))))
  (dolist (proc *active-processes*)
    (let ((wait-fn (process-wait-function proc))
          (wait-args (process-wait-argument-list proc)))
      (when (or (null wait-fn) (apply wait-fn wait-args))
        (setf (process-wait-function proc) nil
              (process-wait-argument-list proc) nil)
        (return-from get-next-process proc)))))

;;; FIXME: Running for so long with interrupts disabled is lame.
;;; Probably need to speed this up somehow.
(defun process-scheduler ()
  (let ((*terminal-io* *terminal-io*)
        (*standard-output* *standard-output*)
        (*standard-input* *standard-input*)
        (*debug-io* *debug-io*)
        (*query-io* *query-io*)
        (*error-output* *error-output*))
    (loop
       (%cli)
       (let ((next-process (get-next-process *current-process*)))
         (setf *current-process* next-process)
         (cond (next-process
                (if (typep next-process 'simple-process)
                    (with-simple-restart (abort "Return from ~S." next-process)
                      (%sti)
                      (apply (car (process-initial-form next-process))
                             (cdr (process-initial-form next-process))))
                    (progn
                      (stack-group-resume (process-stack-group next-process) nil)
                      (setf (process-stack-group next-process) (stack-group-resumer *scheduler-stack-group*)))))
               (t (%stihlt)))))))

(defmacro with-process ((name function &rest arguments) &body body)
  (let ((x (gensym)))
    `(let ((,x (make-instance 'sys.int::process :name ,name)))
       (unwind-protect (progn
                         (sys.int::process-preset ,x ,function ,@arguments)
                         (sys.int::process-enable ,x)
                         ,@body)
         (sys.int::process-disable ,x)))))

(setf *scheduler-stack-group* (make-stack-group "scheduler" :safe nil))
(stack-group-preset *scheduler-stack-group* #'process-scheduler)
(setf *current-process* (make-instance 'process
                                       :name "initial-process"
                                       :stack-group (current-stack-group)
                                       :run-reasons '(:initial)
                                       :whostate "RUN"))
