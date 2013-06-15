(in-package :sys.int)

(defvar *scheduler-stack-group* nil)
(defvar *current-process* nil)
(defvar *active-processes* nil)

(defstruct (process
             (:constructor %make-process))
  name
  (run-reasons '())
  (arrest-reasons '())
  (wait-function (constantly 'nil))
  (wait-argument-list '())
  (whostate '())
  (initial-form nil)
  stack-group
  initial-stack-group)

(defmethod print-object ((object process) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (process-name object))))

(defun process-preset (process function &rest arguments)
  (setf (process-initial-form process) (cons function arguments))
  (process-reset process))

(defun process-reset (process)
  (setf (process-wait-function process) nil
	(process-wait-argument-list process) nil)
  (setf (process-stack-group process) (process-initial-stack-group process))
  (stack-group-preset (process-initial-stack-group process)
                      (lambda ()
                        (with-simple-restart (abort "Terminate process ~S." (process-name process))
                          (apply (first (process-initial-form process))
                                 (rest (process-initial-form process))))
                        (process-disable process)
                        (stack-group-invoke *scheduler-stack-group*))))

(defun process-run-reason (process object)
  (pushnew object (process-run-reasons process))
  (process-consider-runnability process))

(defun process-revoke-run-reason (process object)
  (setf (process-run-reasons process) (remove object (process-run-reasons process)))
  (process-consider-runnability process))

(defun process-arrest-reason (process object)
  (pushnew object (process-arrest-reasons process))
  (process-consider-runnability process))

(defun process-revoke-arrest-reason (process object)
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
                (stack-group-resume (process-stack-group next-process) nil)
                (setf (process-stack-group next-process) (stack-group-resumer *scheduler-stack-group*)))
               (t (%stihlt)))))))

(defun make-process (name &key stack-group
                            control-stack-size
                            data-stack-size
                            binding-stack-size
                            run-reasons
                            arrest-reasons
                            whostate)
  (unless stack-group
    (setf stack-group (make-stack-group name
                                        :control-stack-size control-stack-size
                                        :data-stack-size data-stack-size
                                        :binding-stack-size binding-stack-size)))
  (let ((process (%make-process :name name
                                :stack-group stack-group
                                :initial-stack-group stack-group
                                :run-reasons run-reasons
                                :arrest-reasons arrest-reasons
                                :whostate whostate)))
    (process-consider-runnability process)
    process))

(defmacro with-process ((name function &rest arguments) &body body)
  (let ((x (gensym)))
    `(let ((,x (make-process ,name)))
       (unwind-protect (progn
                         (sys.int::process-preset ,x ,function ,@arguments)
                         (sys.int::process-enable ,x)
                         ,@body)
         (sys.int::process-disable ,x)))))

(setf *scheduler-stack-group* (make-stack-group "scheduler"
                                                :safe nil
                                                :interruptable nil))
(stack-group-preset *scheduler-stack-group* #'process-scheduler)
(setf *current-process* (make-process "Initial Process"
                                      :stack-group (current-stack-group)
                                      :run-reasons '(:initial)
                                      :whostate "RUN"))
