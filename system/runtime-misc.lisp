;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
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

(defun raise-undefined-function (&rest args &fref invoked-through)
  (setf invoked-through (function-reference-name invoked-through))
  ;; Allow restarting.
  ;; FIXME: Restarting doesn't actually work, as args are lost by the undefined function thunk.
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
                     collect (intern (symbol-name (structure-slot-name slot)) "KEYWORD")
                     collect (%struct-slot object i)))
           :stream stream)))

(defmethod print-object ((object hash-table) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defmethod print-object ((object mezzano.supervisor:thread) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (mezzano.supervisor:thread-name object))))

(defmethod print-object ((object mezzano.supervisor::disk) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "Disk")
    (when (typep (mezzano.supervisor::disk-device object) 'mezzano.supervisor::partition)
      (format stream " Partition on ~S" (mezzano.supervisor::partition-disk (mezzano.supervisor::disk-device object))))))

(defmethod print-object ((object mezzano.supervisor::wait-queue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (mezzano.supervisor::wait-queue-name object))))

(defmethod print-object ((object mezzano.supervisor::mutex) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (mezzano.supervisor::mutex-name object))
    (let ((owner (mezzano.supervisor::mutex-owner object)))
      (if owner
          (format stream " held by ~S" owner)
          (format stream " unlocked")))))

(defmethod print-object ((object mezzano.supervisor::condition-variable) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (mezzano.supervisor::condition-variable-name object))))

(defmethod print-object ((object package) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~:(~S~)" (package-shortest-name object))))

(defmethod print-object ((o structure-definition) stream)
  (print-unreadable-object (o stream :identity t :type t)
    (write (structure-name o) :stream stream)))

(defmethod print-object ((o function-reference) stream)
  (print-unreadable-object (o stream :identity t :type t)
    (write (function-reference-name o) :stream stream)))

(defmethod print-object ((o weak-pointer) stream)
  (print-unreadable-object (o stream :identity t :type t)
    (multiple-value-bind (value livep)
        (weak-pointer-value o)
      (if livep
          (format stream "pointing to ~S" value)
          (format stream "dead")))))

(defmethod print-object ((o byte) stream)
  (print-unreadable-object (o stream :type t)
    (format stream ":Size ~D :Position ~D"
            (byte-size o) (byte-position o))))

(defmethod print-object ((object mezzano.supervisor::pci-device) stream)
  (print-unreadable-object (object stream :type t)
    (multiple-value-bind (bus device function)
        (mezzano.supervisor:pci-device-location object)
      (format stream "~2,'0X:~2,'0X:~X~A"
              bus device function
              (if (eql (mezzano.supervisor::pci-device-boot-id object) mezzano.supervisor::*boot-id*)
                  ""
                  " (stale)")))))

(defun snapshot-and-exit ()
  "Terminate the current thread and take a snapshot.
To be run this in the basic repl after ipl completes."
  (mezzano.supervisor:make-thread (lambda ()
                                    (sleep 3)
                                    (sys.int::gc)
                                    (mezzano.supervisor:snapshot)))
  (throw 'mezzano.supervisor::terminate-thread nil))

(defun cal (path &key force)
  "Compile and load PATH.
The file will only be recompiled if the source is newer than the output file, or if FORCE is true."
  (let ((compiled (compile-file-pathname path)))
    (when (or force
              (not (probe-file compiled))
              (<= (file-write-date compiled) (file-write-date path)))
      (format t "; Compiling ~S~%" path)
      (ignore-errors (delete-file compiled))
      (compile-file path))
    (format t "; Loading ~S~%" compiled)
    (load compiled)))

(defun copy-file (filespec new-name &optional (element-type 'character) (source-external-format :default) (destination-external-format :default) (buffer-size (* 1 1024 1024)))
  (let* ((source (merge-pathnames filespec))
         (dest (merge-pathnames new-name source))
         (buf (make-array buffer-size :element-type element-type)))
    (with-open-file (s source :element-type element-type :external-format source-external-format)
      (with-open-file (d dest :direction :output :element-type element-type :external-format destination-external-format)
        (loop
           (let ((n-elements-read (read-sequence buf s)))
             (write-sequence buf d :end n-elements-read)
             (when (< n-elements-read buffer-size)
               (return))))))
    dest))

;; FIXME: Should be a weak hash table.
(defvar *symbol-macro-expansions* (make-hash-table))

(defun symbol-macro-p (symbol)
  (check-type symbol symbol)
  (multiple-value-bind (expansion presentp)
      (gethash symbol *symbol-macro-expansions*)
    (declare (ignore expansion))
    presentp))

(defun symbol-macroexpand-1 (symbol)
  (check-type symbol symbol)
  (multiple-value-bind (expansion presentp)
      (gethash symbol *symbol-macro-expansions*)
    (cond (presentp
           (values expansion t))
          (t
           (values symbol nil)))))

(defun %define-symbol-macro (name expansion)
  (check-type name symbol)
  (when (not (member (symbol-mode name) '(nil :symbol-macro)))
    (cerror "Redefine as a symbol-macro" "Symbol ~S already defined as a ~A" name (symbol-mode name)))
  (setf (symbol-mode name) :symbol-macro)
  (setf (gethash name *symbol-macro-expansions*) expansion)
  name)

(define-condition defconstant-uneql (error)
  ((name :initarg :name :reader defconstant-uneql-name)
   (old-value :initarg :old-value :reader defconstant-uneql-old-value)
   (new-value :initarg :new-value :reader defconstant-uneql-new-value))
  (:report
   (lambda (condition stream)
     (format stream
             "~@<The constant ~S is being redefined (from ~S to ~S)~@:>"
             (defconstant-uneql-name condition)
             (defconstant-uneql-old-value condition)
             (defconstant-uneql-new-value condition)))))
