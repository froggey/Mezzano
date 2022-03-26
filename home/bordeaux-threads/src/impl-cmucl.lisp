;;;; -*- indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

(deftype thread ()
  'mp::process)

;;; Thread Creation

(defun start-multiprocessing ()
  (mp::startup-idle-and-top-level-loops))

(defun %make-thread (function name)
  #+#.(cl:if (cl:find-symbol (cl:string '#:process-join) :mp) '(and) '(or))
  (mp:make-process function :name name)
  #-#.(cl:if (cl:find-symbol (cl:string '#:process-join) :mp) '(and) '(or))
  (mp:make-process (lambda ()
                     (let ((return-values
                             (multiple-value-list (funcall function))))
                       (setf (getf (mp:process-property-list mp:*current-process*)
                                   'return-values)
                             return-values)
                       (values-list return-values)))
                   :name name))

(defun current-thread ()
  mp:*current-process*)

(defmethod threadp (object)
  (mp:processp object))

(defun thread-name (thread)
  (mp:process-name thread))

;;; Resource contention: locks and recursive locks

(deftype lock () 'mp:error-check-lock)

(deftype recursive-lock () 'mp:recursive-lock)

(defun lock-p (object)
  (typep object 'mp:error-check-lock))

(defun recursive-lock-p (object)
  (typep object 'mp:recursive-lock))

(defun make-lock (&optional name)
  (mp:make-lock (or name "Anonymous lock")
                :kind :error-check))

(defun acquire-lock (lock &optional (wait-p t))
  (if wait-p
      (mp::lock-wait lock "Lock wait")
      (mp::lock-wait-with-timeout lock "Lock wait" 0)))

(defun release-lock (lock)
  (setf (mp::lock-process lock) nil))

(defmacro with-lock-held ((place) &body body)
  `(mp:with-lock-held (,place "Lock wait") ,@body))

(defun make-recursive-lock (&optional name)
  (mp:make-lock (or name "Anonymous recursive lock")
                :kind :recursive))

(defun acquire-recursive-lock (lock &optional (wait-p t))
  (acquire-lock lock))

(defun release-recursive-lock (lock)
  (release-lock lock))

(defmacro with-recursive-lock-held ((place &key timeout) &body body)
  `(mp:with-lock-held (,place "Lock Wait" :timeout ,timeout) ,@body))

;;; Note that the locks _are_ recursive, but not "balanced", and only
;;; checked if they are being held by the same process by with-lock-held.
;;; The default with-lock-held in bordeaux-mp.lisp sort of works, in that
;;; it will wait for recursive locks by the same process as well.

;;; Resource contention: condition variables

;;; There's some stuff in x86-vm.lisp that might be worth investigating
;;; whether to build on. There's also process-wait and friends.

(defstruct condition-var
  "CMUCL doesn't have conditions, so we need to create our own type."
  name
  lock
  active)

(defun make-condition-variable (&key name)
  (make-condition-var :lock (make-lock)
                      :name (or name "Anonymous condition variable")))

(defun condition-wait (condition-variable lock &key timeout)
  (signal-error-if-condition-wait-timeout timeout)
  (check-type condition-variable condition-var)
  (with-lock-held ((condition-var-lock condition-variable))
    (setf (condition-var-active condition-variable) nil))
  (release-lock lock)
  (mp:process-wait "Condition Wait"
                   #'(lambda () (condition-var-active condition-variable)))
  (acquire-lock lock)
  t)

(define-condition-wait-compiler-macro)

(defun condition-notify (condition-variable)
  (check-type condition-variable condition-var)
  (with-lock-held ((condition-var-lock condition-variable))
    (setf (condition-var-active condition-variable) t))
  (thread-yield))

(defun thread-yield ()
  (mp:process-yield))

;;; Timeouts

(defmacro with-timeout ((timeout) &body body)
  (once-only (timeout)
    `(mp:with-timeout (,timeout (error 'timeout :length ,timeout))
       ,@body)))

;;; Introspection/debugging

(defun all-threads ()
  (mp:all-processes))

(defun interrupt-thread (thread function &rest args)
  (flet ((apply-function ()
           (if args
               (lambda () (apply function args))
               function)))
    (declare (dynamic-extent #'apply-function))
    (mp:process-interrupt thread (apply-function))))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (mp:destroy-process thread))

(defun thread-alive-p (thread)
  (mp:process-active-p thread))

(defun join-thread (thread)
  #+#.(cl:if (cl:find-symbol (cl:string '#:process-join) :mp) '(and) '(or))
  (mp:process-join thread)
  #-#.(cl:if (cl:find-symbol (cl:string '#:process-join) :mp) '(and) '(or))
  (progn
    (mp:process-wait (format nil "Waiting for thread ~A to complete" thread)
                     (lambda () (not (mp:process-alive-p thread))))
    (let ((return-values
            (getf (mp:process-property-list thread) 'return-values)))
      (values-list return-values))))

(mark-supported)
