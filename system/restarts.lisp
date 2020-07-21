;;;; Restarts

(in-package :mezzano.internals)

(defparameter *active-restarts* nil)
(defvar *restart-associations* '())

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

(defun restart-associated-with-condition-p (restart condition)
  (or (not condition)
      (loop
         with restart-associated-p = nil
         for (test-condition . test-restarts) in *restart-associations*
         do
           (when (member restart test-restarts)
             (setf restart-associated-p t)
             (when (eql test-condition condition)
               (return t)))
         finally (return (not restart-associated-p)))))

(defun compute-restarts (&optional condition)
  (let ((restarts '()))
    (dolist (restart-cluster *active-restarts*)
      (dolist (restart restart-cluster)
        (when (and (restart-associated-with-condition-p restart condition)
                   (test-restart restart condition))
          (push restart restarts))))
    (reverse restarts)))

(defun find-restart (identifier &optional condition)
  (check-type identifier (or symbol restart))
  (if (symbolp identifier)
      (dolist (restarts *active-restarts*)
        (dolist (r restarts)
          (when (and (eql identifier (restart-name r))
                     (and (restart-associated-with-condition-p r condition)
                          (test-restart r condition)))
            (return-from find-restart r))))
      identifier))

(defun find-restart-or-die (identifier &optional condition)
  (or (find-restart identifier condition)
      (error 'bad-restart-error
             :identifier identifier
             :condition condition)))

(defun invoke-restart-interactively (restart)
  (let* ((restart (find-restart-or-die restart))
         (interactive-function (restart-interactive-function restart))
         (arguments (if interactive-function
                        (funcall interactive-function)
                        nil)))
    (apply #'invoke-restart restart arguments)))

(defun invoke-restart (restart &rest arguments)
  (apply (restart-function (find-restart-or-die restart)) arguments))

(defmacro with-condition-restarts (condition-form restarts-form &body body)
  `(call-with-condition-restarts ,condition-form ,restarts-form
                                 (lambda () (progn ,@body))))

(defun call-with-condition-restarts (condition restarts fn)
  (mezzano.delimited-continuations:with-continuation-barrier ('with-condition-restarts)
    (let ((*restart-associations* (list* (list* condition restarts)
                                         *restart-associations*)))
      (funcall fn))))

(defun %restart-bind (clauses thunk)
  (if *active-restarts*
      ;; There is a barrier here as this would capture the entire stack of handlers
      ;; not just the ones inside the continuation.
      ;; This should switch to a mechanism similar to %CATCH, but that is more
      ;; difficult to handle because of the visibility requirements when calling
      ;; handlers.
      (mezzano.delimited-continuations:with-continuation-barrier ('restart-bind)
        (let ((*active-restarts* (cons clauses *active-restarts*)))
          (funcall thunk)))
      (let ((*active-restarts* (cons clauses *active-restarts*)))
        (funcall thunk))))

(defmacro restart-bind (clauses &rest forms)
  `(%restart-bind (list ,@(loop
                             for (name . arguments) in clauses
                             collect `(make-restart ',name ,@arguments)))
                  (lambda () (progn ,@forms))))

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
                          `(lambda (stream) (write-string ,(cadr forms) stream))
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

(defmacro restart-case (&environment env restartable-form &rest clauses)
  (let ((block-name (gensym))
        (arguments (gensym))
        (restart-bindings '())
        (restart-bodies '()))
    (dolist (clause clauses)
      (multiple-value-bind (binding label body)
          (handle-restart-case-clause clause block-name arguments)
        (push binding restart-bindings)
        (push label restart-bodies)
        (push body restart-bodies)))
    (let ((expanded-restartable-form (macroexpand restartable-form env)))
      (cond ((and (listp expanded-restartable-form)
                  (member (first expanded-restartable-form)
                          '(signal error cerror warn)))
             (let ((condition (gensym "CONDITION")))
               ;; TODO: Do this without the calls to FIND-RESTART.
               ;; This'll require open-coding the RESTART-BIND.
               `(let ((,condition (coerce-to-condition ',(ecase (first expanded-restartable-form)
                                                           ((signal) 'simple-condition)
                                                           ((error cerror) 'simple-error)
                                                           ((warn) 'simple-warning))
                                                       ,(second expanded-restartable-form)
                                                       (list ,@(cddr expanded-restartable-form)))))
                  (restart-case
                      (with-condition-restarts ,condition
                          (list ,@(loop
                                     for clause in clauses
                                     collect `(find-restart ',(first clause))))
                        (,(first expanded-restartable-form) ,condition))
                    ,@clauses))))
            (t
             `(block ,block-name
                (let ((,arguments nil))
                  (tagbody
                     (restart-bind ,(reverse restart-bindings)
                       (return-from ,block-name ,restartable-form))
                     ,@(reverse restart-bodies)))))))))

(defmacro with-simple-restart ((name format-control &rest format-arguments) &body forms)
  `(restart-case (progn ,@forms)
     (,name ()
       :report (lambda (stream)
                 (format stream ,format-control ,@format-arguments))
       (values nil t))))

(defun abort (&optional condition)
  (invoke-restart (find-restart-or-die 'abort condition)))

(defun continue (&optional condition)
  (let ((r (find-restart 'continue condition)))
    (when r
      (invoke-restart r))))

(defun muffle-warning (&optional condition)
  (invoke-restart (find-restart-or-die 'muffle-warning condition)))

(defun store-value (value &optional condition)
  (let ((r (find-restart 'store-value condition)))
    (when r
      (invoke-restart r value))))

(defun use-value (value &optional condition)
  (let ((r (find-restart 'use-value condition)))
    (when r
      (invoke-restart r value))))
