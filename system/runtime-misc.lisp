;;;; Miscellaneous runtime functions
;;;;
;;;; This is loaded late, once most of the system is working, so can
;;;; declaim types and define generic functions.

(in-package :mezzano.internals)

;; Undefine the following functions if they're not generic functions.
;; They have early non-generic definitions that these generics overwrite.
;; Undefining them beforehand prevents problems with redefinition.

(when (and (fboundp 'funcallable-instance-lambda-expression)
           (not (typep (fdefinition 'funcallable-instance-lambda-expression)
                       'standard-generic-function)))
  (fmakunbound 'funcallable-instance-lambda-expression))
(defgeneric funcallable-instance-lambda-expression (function)
  (:method ((function function))
    (declare (ignore function))
    (values nil t nil)))

(when (and (fboundp 'funcallable-instance-debug-info)
           (not (typep (fdefinition 'funcallable-instance-debug-info)
                       'standard-generic-function)))
  (fmakunbound 'funcallable-instance-debug-info))
(defgeneric funcallable-instance-debug-info (function)
  (:method ((function function))
    (declare (ignore function))
    nil))

(when (and (fboundp 'funcallable-instance-compiled-function-p)
           (not (typep (fdefinition 'funcallable-instance-compiled-function-p)
                       'standard-generic-function)))
  (fmakunbound 'funcallable-instance-compiled-function-p))
(defgeneric funcallable-instance-compiled-function-p (function)
  (:method ((function function))
    (declare (ignore function))
    nil))

(defmethod print-object ((object structure-object) stream)
  (write-string "#S" stream)
  (let ((class (class-of object)))
    (write (list* (class-name class)
                  (loop
                     for slot in (mezzano.clos:class-slots class)
                     for slot-name = (mezzano.clos:slot-definition-name slot)
                     collect (intern (symbol-name slot-name) "KEYWORD")
                     collect (if (mezzano.clos:structure-slot-definition-fixed-vector slot)
                                 (let ((vec (make-array (mezzano.clos:structure-slot-definition-fixed-vector slot))))
                                   (dotimes (i (mezzano.clos:structure-slot-definition-fixed-vector slot))
                                     (setf (aref vec i) (%struct-vector-slot object class slot-name i)))
                                   vec)
                                 (%struct-slot object class slot-name))))
           :stream stream)))

(defmethod print-object ((object layout) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (class-name (layout-class object)))
    (when (layout-obsolete object)
      (format stream " [obsolete]"))))

(defmethod print-object ((object hash-table) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream ":Test ~S :Count ~S"
            (hash-table-test object)
            (hash-table-count object))
    (when (hash-table-weakness object)
      (format stream " :Weakness ~S" (hash-table-weakness object)))
    (when (hash-table-synchronized object)
      (format stream " :Synchronized ~S" (hash-table-synchronized object)))
    (when (hash-table-enforce-gc-invariant-keys object)
      (format stream " :Enforce-Gc-Invariant-Keys ~S"
              (hash-table-enforce-gc-invariant-keys object)))))

(defmethod print-object ((object mezzano.supervisor:thread) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S"
            (mezzano.supervisor:thread-name object)
            (mezzano.supervisor:thread-state object))))

(defmethod print-object ((object mezzano.supervisor::disk) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "Disk")
    (when (typep (mezzano.supervisor::disk-device object) 'mezzano.supervisor::partition)
      (format stream " Partition on ~S" (mezzano.supervisor::partition-disk (mezzano.supervisor::disk-device object))))))

(defmethod print-object ((object mezzano.supervisor::wait-queue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (mezzano.supervisor::wait-queue-name object))))

(defmethod print-object ((object mezzano.supervisor:mutex) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (mezzano.supervisor:mutex-name object))
    (let ((owner (mezzano.supervisor::mutex-owner object)))
      (if owner
          (format stream " held by ~S" owner)
          (format stream " unlocked")))))

(defmethod print-object ((object mezzano.supervisor:condition-variable) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (mezzano.supervisor:condition-variable-name object))))

(defmethod print-object ((object mezzano.supervisor:rw-lock) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S :State ~X" (mezzano.supervisor:rw-lock-name object)
            (mezzano.supervisor::rw-lock-state object))))

(defmethod print-object ((object package) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~:(~S~)" (package-shortest-name object))))

(defmethod print-object ((o structure-definition) stream)
  (print-unreadable-object (o stream :identity t :type t)
    (write (structure-definition-name o) :stream stream)))

(defmethod print-object ((o function-reference) stream)
  (print-unreadable-object (o stream :identity t :type t)
    (write (function-reference-name o) :stream stream)))

(defmethod print-object ((o mezzano.runtime::symbol-value-cell) stream)
  (print-unreadable-object (o stream :identity t :type t)
    (format stream "~A~A"
            (mezzano.runtime::symbol-value-cell-symbol o)
            (if (mezzano.runtime::symbol-global-value-cell-p o)
                " [global]"
                ""))))

(defmethod print-object ((o weak-pointer) stream)
  (print-unreadable-object (o stream :identity t :type t)
    (multiple-value-bind (key value livep)
        (weak-pointer-pair o)
      (cond (livep
             (if (eql key value)
                 (format stream "pointing to ~S" value)
                 (format stream "with key ~S pointing to ~S" key value)))
            (t
             (format stream "dead"))))
    (format stream " with weakness ~:(~S~)" (weak-pointer-weakness o))))

(defmethod print-object ((o weak-pointer-vector) stream)
  (print-unreadable-object (o stream :identity t :type t)
    (let ((entries (loop
                      for i below (weak-pointer-vector-length o)
                      collect (multiple-value-bind (key value livep)
                                  (weak-pointer-vector-pair o i)
                                (if livep
                                    (if (and key (eql key value))
                                        key
                                        (cons key value))
                                    nil)))))
      (format stream "~:S" entries))))

(defmethod print-object ((o byte) stream)
  (print-unreadable-object (o stream :type t)
    (format stream ":Size ~D :Position ~D"
            (byte-size o) (byte-position o))))

(defmethod print-object ((object mezzano.supervisor.pci:pci-device) stream)
  (print-unreadable-object (object stream :type t)
    (multiple-value-bind (bus device function)
        (mezzano.supervisor.pci:pci-device-location object)
      (format stream "~2,'0X:~2,'0X:~X~A"
              bus device function
              (if (eql (mezzano.supervisor.pci:pci-device-boot-id object)
                       (mezzano.supervisor:current-boot-id))
                  ""
                  " (stale)")))))

(defmethod print-object ((object mezzano.supervisor::cpu) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D ~S" (mezzano.supervisor::cpu-apic-id object) (mezzano.supervisor::cpu-state object))))

(defmethod print-object ((object mezzano.delimited-continuations:delimited-continuation) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if (mezzano.delimited-continuations:resumable-p object)
        (format stream "resumable")
        (format stream "consumed"))))

(defmethod print-object ((object bit-vector) stream)
  (if *print-array*
      (write-bit-vector object stream)
      (call-next-method)))

(defmethod print-object ((object vector) stream)
  (if *print-array*
      (write-vector object stream)
      (call-next-method)))

(defmethod print-object ((object array) stream)
  (cond (*print-array*
         (write-char #\# stream)
         (write (array-rank object) :stream stream :base 10 :radix nil)
         (write-char #\A stream)
         (labels ((print-level (dims index)
                    (cond ((null dims)
                           (write (row-major-aref object index) :stream stream)
                           (1+ index))
                          (t
                           (write-char #\( stream)
                           (dotimes (i (first dims))
                             (setf index (print-level (rest dims) index))
                             (when (not (eql i (1- (first dims))))
                               (write-char #\Space stream)))
                           (write-char #\) stream)
                           index))))
           (print-level (array-dimensions object) 0)))
        (t
         (call-next-method))))

(defmethod print-object ((object mezzano.supervisor::irq) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~A" (mezzano.supervisor::irq-platform-number object))))

(defmethod print-object ((object mezzano.supervisor:watcher) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (let ((name (mezzano.supervisor:watcher-name object)))
      (if name
          (format stream "~A ~:A" name (mezzano.supervisor:watcher-objects object))
          (format stream "~:A" (mezzano.supervisor:watcher-objects object))))))

(defmethod print-object ((o instance-header) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "for ~S" (mezzano.runtime::%unpack-instance-header o))))

(defun snapshot-and-exit ()
  "Terminate the current thread and take a snapshot.
To be run this in the basic repl after ipl completes."
  (mezzano.supervisor:make-thread (lambda ()
                                    (sleep 3)
                                    (sys.int::gc :full t)
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

(defvar *symbol-macro-expansions* (make-hash-table :synchronized t :weakness :key))

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

(defun %define-symbol-macro (name expansion source-location)
  (check-type name symbol)
  (when (not (member (symbol-mode name) '(nil :symbol-macro)))
    (cerror "Redefine as a symbol-macro" "Symbol ~S already defined as a ~A" name (symbol-mode name)))
  (setf (symbol-mode name) :symbol-macro)
  (setf (gethash name *symbol-macro-expansions*) expansion)
  (set-variable-source-location name source-location 'define-symbol-macro)
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

(deftype radix () '(integer 2 36))
(declaim (type radix *read-base*))
(declaim (type (member short-float single-float double-float long-float)
               *read-default-float-format*))
(declaim (type radix *print-base*))
(declaim (type (member :upcase :downcase :capitalize) *print-case*))
(declaim (type (or null (integer 0))
               *print-length*
               *print-level*
               *print-lines*
               *print-miser-width*
               *print-right-margin*))
