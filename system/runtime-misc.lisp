;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(fmakunbound 'funcallable-instance-lambda-expression)
(defgeneric funcallable-instance-lambda-expression (function)
  (:method ((function function))
    (declare (ignore function))
    (values nil t nil)))

(fmakunbound 'funcallable-instance-debug-info)
(defgeneric funcallable-instance-debug-info (function)
  (:method ((function function))
    (declare (ignore function))
    nil))

(fmakunbound 'funcallable-instance-compiled-function-p)
(defgeneric funcallable-instance-compiled-function-p (function)
  (:method ((function function))
    (declare (ignore function))
    (compiled-function-p (funcallable-std-instance-function function))))

(defgeneric make-load-form (object &optional environment))

(defun raise-undefined-function (invoked-through &rest args)
  (setf invoked-through (function-reference-name invoked-through))
  ;; Allow restarting.
  (restart-case (error 'undefined-function :name invoked-through)
    (use-value (v)
      :interactive (lambda ()
                     (format t "Enter a new value (evaluated): ")
                     (list (eval (read))))
      :report (lambda (s) (format s "Input a value to be used in place of ~S." `(fdefinition ',invoked-through)))
      (apply v args))))

(defmethod print-object ((object structure-object) stream)
  (write-string "#S" stream)
  (let ((contents (list (type-of object)))
        (type (%struct-slot object 0)))
    (write (list* (type-of object)
                  (loop
                     for i from 1
                     for slot in (structure-slots type)
                     collect (intern (symbol-name (first slot)) "KEYWORD")
                     collect (%struct-slot object i)))
           :stream stream)))

(defmethod print-object ((object hash-table) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defmethod print-object ((object mezzano.supervisor:thread) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (mezzano.supervisor:thread-name object))))

(defmethod print-object ((object mezzano.supervisor::nic) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~:(~A~) ~/mezzano.network.ethernet:format-mac-address/"
            (type-of (mezzano.supervisor::nic-device object))
            (mezzano.supervisor:nic-mac object))))

(defmethod print-object ((object mezzano.supervisor::disk) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "Disk")
    (when (typep (mezzano.supervisor::disk-device object) 'mezzano.supervisor::partition)
      (format stream " Partition on ~S" (mezzano.supervisor::partition-disk (mezzano.supervisor::disk-device object))))))

(defmethod print-object ((object package) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~:(~S~)" (package-shortest-name object))))

(defmethod print-object ((o structure-definition) stream)
  (print-unreadable-object (o stream :identity t :type t)
    (write (structure-name o) :stream stream)))

(defmethod print-object ((o function-reference) stream)
  (print-unreadable-object (o stream :identity t :type t)
    (write (function-reference-name o) :stream stream)))
