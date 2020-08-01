;;;; ERROR and other error related functions/macros.

(in-package :mezzano.internals)

(declaim (special *error-output* *debug-io*))
(defparameter *infinite-error-protect* 0)
(defparameter *debugger-hook* nil)

(define-condition error (serious-condition)
  ())

(define-condition simple-error (simple-condition error)
  ())

(define-condition arithmetic-error (error)
  ((operation :initarg :operation
              :reader arithmetic-error-operation)
   (operands :initarg :operands
             :initarg nil
             :reader arithmetic-error-operands)))

(define-condition array-dimension-error (error)
  ((array :initarg :array
          :reader array-dimension-error-array)
   (axis :initarg :axis
         :reader array-dimension-error-axis)))

(define-condition cell-error (error)
  ((name :initarg :name
         :reader cell-error-name)))

(defmethod print-object ((c cell-error) s)
  (if (slot-boundp c 'name)
      (print-unreadable-object (c s :type t)
        (write (cell-error-name c) :stream s))
      (call-next-method)))

(define-condition control-error (error)
  ())

(define-condition bad-catch-tag-error (control-error)
  ((tag :initarg :tag
        :reader bad-catch-tag-error-tag))
  (:report (lambda (condition stream)
             (format stream "Throw to unknown catch-tag ~S." (bad-catch-tag-error-tag condition)))))

(define-condition bad-restart-error (control-error)
  ((identifier :initarg :identifier
               :reader bad-restart-error-identifier)
   (condition :initarg :condition
              :reader bad-restart-error-condition))
  (:report (lambda (condition stream)
             (if (bad-restart-error-condition condition)
                 (format stream "No applicable restart named ~S associated with ~S"
                         (bad-restart-error-identifier condition)
                         (bad-restart-error-condition condition))
                 (format stream "No applicable restart named ~S"
                         (bad-restart-error-identifier condition))))))

(define-condition unavailable-go-tag-error (control-error)
  ((tag :initarg :tag
        :reader unavailable-go-tag-error-tag))
  (:report (lambda (condition stream)
             (format stream "GO to out-of-scope go-tag ~S." (unavailable-go-tag-error-tag condition)))))

(define-condition unavailable-block-error (control-error)
  ((tag :initarg :tag
        :reader unavailable-block-error-tag))
  (:report (lambda (condition stream)
             (format stream "RETURN-FROM to out-of-scope block ~S." (unavailable-block-error-tag condition)))))

(define-condition division-by-zero (arithmetic-error) ())
(define-condition floating-point-inexact (arithmetic-error) ())
(define-condition floating-point-invalid-operation (arithmetic-error) ())
(define-condition floating-point-overflow (arithmetic-error) ())
(define-condition floating-point-underflow (arithmetic-error) ())
;; Extension.
(define-condition mezzano.extensions:floating-point-denormal-operand (arithmetic-error) ())

(define-condition invalid-index-error (error)
  ((array :initarg :array
          :reader invalid-index-error-array)
   (axis :initarg :axis
         :initform nil
         :reader invalid-index-error-axis)
   (datum :initarg :datum
          :reader invalid-index-error-datum))
  (:report (lambda (condition stream)
             (if (invalid-index-error-axis condition)
                 (format stream "Invalid index ~S for array ~S. Must be non-negative and less than ~S."
                         (invalid-index-error-datum condition)
                         (invalid-index-error-array condition)
                         (array-dimension (invalid-index-error-array condition)
                                          (invalid-index-error-axis condition)))
                 (format stream "Invalid index ~S for array ~S."
                         (invalid-index-error-datum condition)
                         (invalid-index-error-array condition))))))

(define-condition package-error (error)
  ((package :initarg :package
            :reader package-error-package)))

(define-condition simple-package-error (simple-condition package-error)
  ())

(define-condition parse-error (error)
  ())

(define-condition simple-parse-error (simple-condition parse-error)
  ())

(define-condition print-not-readable (error)
  ((object :initarg :object
           :reader print-not-readable-object)))

(define-condition program-error (error)
  ())

(define-condition invalid-argument-error (program-error)
  ((function :initarg :function
             :initform '()
             :reader invalid-argument-error-function)
   (arguments :initarg :arguments
              :initform '()
              :reader invalid-argument-error-arguments))
  (:report (lambda (condition stream)
             (format stream "Function ~S called with invalid arguments ~:S."
                     (invalid-argument-error-function condition)
                     (invalid-argument-error-arguments condition)))))

(define-condition simple-program-error (program-error simple-error)
  ())

(define-condition stream-error (error)
  ((stream :initarg :stream
           :reader stream-error-stream)))

(define-condition end-of-file (stream-error)
  ())

(define-condition reader-error (parse-error stream-error)
  ())

(define-condition simple-reader-error (simple-condition reader-error)
  ())

(define-condition type-error (error)
  ((datum :initarg :datum
          :reader type-error-datum)
   (expected-type :initarg :expected-type
                  :reader type-error-expected-type))
  (:report (lambda (condition stream)
             (format stream "Type error. ~S is not of type ~S."
                     (type-error-datum condition)
                     (type-error-expected-type condition)))))

(define-condition simple-type-error (simple-condition type-error)
  ())

(define-condition unknown-package-error (type-error package-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No package named ~S"
                     (type-error-datum condition)))))

(define-condition unbound-variable (cell-error)
  ()
  (:report (lambda (condition stream)
             (format stream "The variable ~S is unbound." (cell-error-name condition)))))

(define-condition undefined-function (cell-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Undefined function ~S." (cell-error-name condition)))))

(define-condition call-undefined-function (undefined-function)
  ((arguments :initarg :arguments
              :initform '()
              :reader call-undefined-function-arguments))
  (:report (lambda (condition stream)
             (format stream "Undefined function ~S called with arguments ~:S."
                     (cell-error-name condition)
                     (call-undefined-function-arguments condition)))))

(define-condition unbound-slot (cell-error)
  ((instance :initarg :instance
             :reader unbound-slot-instance))
  (:report (lambda (condition stream)
             (format stream "The slot ~S is unbound in the object ~S."
                     (cell-error-name condition)
                     (unbound-slot-instance condition)))))

(define-condition warning ()
  ())

(define-condition simple-warning (simple-condition warning)
  ())

(define-condition style-warning (warning)
  ())

(define-condition simple-style-warning (simple-condition style-warning)
  ())

(define-condition storage-condition (serious-condition) ())
(define-condition simple-storage-condition (simple-condition storage-condition) ())

(define-condition invalid-macro-lambda-list (simple-error)
  ((lambda-list :initarg :lambda-list
                :reader invalid-macro-lambda-list-lambda-list)))

(define-condition mutex-error (simple-error)
  ((mutex :initarg :mutex
          :reader mutex-error-mutex)))

(define-condition unknown-type-specifier-error (error)
  ((type-specifier :initarg :type-specifier
                   :reader unknown-type-specifier-error-type-specifier))
  (:report (lambda (condition stream)
             (format stream "~S is not a known type-specifier"
                     (unknown-type-specifier-error-type-specifier condition)))))

(defun error (datum &rest arguments)
  (mezzano.supervisor::check-supervisor-error datum arguments)
  (let ((condition datum))
    (let ((*infinite-error-protect* (1+ *infinite-error-protect*)))
      (cond ((<= *infinite-error-protect* 10)
             (setf condition (coerce-to-condition 'simple-error datum arguments))
             (signal condition))
            (t ;; gone into infinite-error-protect.
             (setf *print-safe* t)
             (dolist (x arguments)
               (write-char #\Space *debug-io*)
               (typecase x
                 (string (write-string x *debug-io*))
                 (symbol (cond ((and (symbol-package x) (packagep (symbol-package x)))
                                (write-string (package-name (symbol-package x)) *debug-io*)
                                (write-string "::" *debug-io*))
                               (t (write-string "#:" *debug-io*)))
                         (write-string (symbol-name x) *debug-io*))
                 (fixnum (write x :stream *debug-io*))
                 (t (write-string "#<unknown>" *debug-io*)))))))
    (invoke-debugger condition)))

(defun cerror (continue-format-control datum &rest arguments)
  (mezzano.supervisor::check-supervisor-error datum arguments)
  (let ((condition (if (typep datum 'condition)
                       datum
                       (coerce-to-condition 'simple-error datum arguments))))
    (restart-case
        (with-condition-restarts condition (list (find-restart 'continue))
          (signal condition)
          (invoke-debugger condition))
      (continue ()
        :report (lambda (stream)
                  (apply #'format stream continue-format-control arguments))))))

(define-condition assert-error (simple-error)
  ((test-form :initarg :test-form
              :reader assert-error-test-form)
   (intermediate-values :initarg :intermediate-values
                        :reader assert-error-intermediate-values))
  (:report (lambda (condition stream)
             (apply #'format stream
                    (simple-condition-format-control condition)
                    (simple-condition-format-arguments condition))
             (loop
                for (value form) in (assert-error-intermediate-values condition)
                do
                  (format stream "~&  ~S := ~S" form value)))))

(defun assert-error (test-form intermediate-values datum &rest arguments)
  (let ((condition (cond ((or (stringp datum)
                              (functionp datum))
                          (make-condition 'assert-error
                                          :format-control datum
                                          :format-arguments arguments
                                          :test-form test-form
                                          :intermediate-values intermediate-values))
                         (datum
                          (coerce-to-condition 'simple-error datum arguments))
                         (t
                          (make-condition 'assert-error
                                          :format-control "Assertion failed: ~S"
                                          :format-arguments (list test-form)
                                          :test-form test-form
                                          :intermediate-values intermediate-values)))))
    (cerror "Retry assertion." condition)))

(defun assert-prompt (place value)
  (format *debug-io* "~&Current value of ~S is ~S~%" place value)
  (format *debug-io* "~&Enter a new value for ~S.~%> " place)
  (eval (read *debug-io*)))

(defmacro assert (&environment env test-form &optional places datum-form &rest argument-forms)
  (if (and (consp test-form)
           (symbolp (first test-form))
           (not (macro-function (first test-form) env))
           (not (special-operator-p (first test-form))))
      ;; Evaluate arguments and include their values.
      (let ((test-arguments '())
            (intermediate-symbols '())
            (intermediate-forms '())
            (retry-tag (gensym)))
        (dolist (arg (rest test-form))
          (cond ((or (and (consp arg)
                          (not (eql (first arg) 'quote)))
                     (and (symbolp arg)
                          (not (constantp arg env))))
                 (let ((tmp (gensym)))
                   (push tmp test-arguments)
                   (push tmp intermediate-symbols)
                   (push arg intermediate-forms)))
                (t
                 (push arg test-arguments))))
        (setf test-arguments (reverse test-arguments)
              intermediate-symbols (reverse intermediate-symbols)
              intermediate-forms (reverse intermediate-forms))
        `(tagbody
            ,retry-tag
            (let ,(loop
                     for sym in intermediate-symbols
                     for form in intermediate-forms
                     collect (list sym form))
              (when (not (,(first test-form) ,@test-arguments))
                (assert-error
                 ',test-form (list ,@(loop
                                        for sym in intermediate-symbols
                                        for form in intermediate-forms
                                        collect `(list ,sym ',form)))
                 ,datum-form
                 ,@argument-forms)
                ,@(mapcar (lambda (place)
                            `(setf ,place (assert-prompt ',place ,place)))
                          places)
                (go ,retry-tag)))))
      (let ((retry-tag (gensym))
            (value (gensym)))
        `(tagbody
            ,retry-tag
            (let ((,value ,test-form))
              (when (not ,value)
                (assert-error ',test-form (list (list ,value ',test-form))
                              ,datum-form ,@argument-forms)
                ,@(mapcar (lambda (place)
                            `(setf ,place (assert-prompt ',place ,place)))
                          places)
                (go ,retry-tag)))))))

(defun break (&optional (format-control "Break") &rest format-arguments)
  (with-simple-restart (continue "Return from BREAK.")
    (let ((*debugger-hook* nil))
      (invoke-debugger
       (make-condition 'simple-condition
                       :format-control format-control
                       :format-arguments format-arguments))))
  nil)

(defun warn (datum &rest arguments)
  (let ((condition (coerce-to-condition 'simple-warning datum arguments)))
    (check-type condition warning)
    (restart-case (signal condition)
      (muffle-warning ()
        :report "Ignore this warning."
        (return-from warn nil)))
    (if (typep condition 'style-warning)
        (format *error-output* "~&Style-Warning: ~A~%" condition)
        (format *error-output* "~&Warning: ~A~%" condition))
    nil))

(defun invoke-debugger (condition)
  (when *debugger-hook*
    (let ((old-hook *debugger-hook*)
          (*debugger-hook* nil))
      (funcall old-hook condition old-hook)))
  (enter-debugger condition))

(defun undefined-function-called (args fref)
  (let ((name (function-reference-name fref)))
    (restart-case
        (error 'call-undefined-function :name name :arguments args)
      (use-value (&rest values)
        :interactive (lambda ()
                       (format *query-io* "Enter a value to return (evaluated): ")
                       (finish-output *query-io*)
                       (multiple-value-list (eval (read *query-io*))))
        :report "Supply a value to use instead."
        (values-list values))
      (continue ()
        :report "Retry calling the function."
        (apply name args))
      (specify-function (fn)
        :interactive (lambda ()
                       (format *query-io* "Enter a function to call (evaluated): ")
                       (finish-output *query-io*)
                       (list (eval (read *query-io*))))
        :report "Supply a function to call with these arguments instead."
        (apply fn args)))))

(defun make-deferred-undefined-function (fref)
  (lambda (&rest args)
    (undefined-function-called args fref)))

;;; Calls to these functions are generated by the compiler to
;;; signal errors.

;; This function must be a normal non-closure compiled function, or
;; the invoked-through value will be lost.
(defun raise-undefined-function (&rest args &closure invoked-through)
  (undefined-function-called args invoked-through))

(defun raise-unbound-error (symbol)
  (error 'unbound-variable :name symbol))

(defun raise-type-error (datum expected-type)
  (error 'type-error :datum datum :expected-type expected-type))

(defun raise-invalid-argument-error (&rest args &closure function)
  ;; FIXME: The wronged function tail-calls here, causing it to not show
  ;; up in backtraces. Fixing this probably involves a non-trivial rework
  ;; of the invalid args mechanism.
  ;; This is currently worked around with a hack in FUNCTION-FROM-FRAME.
  (restart-case
      (error 'invalid-argument-error :function function :arguments args)
    (specify-arguments (new-arguments)
      :interactive (lambda ()
                     (format *query-io* "Enter a list of arguments to call ~S with (evaluated): " function)
                     (finish-output *query-io*)
                     (list (eval (read *query-io*))))
      :report "Supply new arguments."
      (apply function new-arguments))
    (use-value (&rest values)
      :interactive (lambda ()
                     (format *query-io* "Enter a value to return (evaluated): ")
                     (finish-output *query-io*)
                     (multiple-value-list (eval (read *query-io*))))
      :report "Supply a value to use instead."
      (values-list values))))

(defun raise-stack-alignment-error ()
  (error "Stack was misaligned."))

(defun raise-bounds-error (array index)
  (error "Index ~D out of bounds for array ~S." index array))

(defun raise-bounds-range-error (array index range)
  (error "Index ~D out of bounds for array ~S. Should be non-negative and less than ~S."
         index array
         (- (array-dimension array 0) range)))

(defun raise-complex-bounds-error (array index dim axis)
  (error "Subscript ~S is invalid for array ~S axis ~D, should be non-negative and less than ~S."
         index array axis dim))

(defun raise-bad-go-tag (name)
  (error 'unavailable-go-tag-error :tag name))

(defun raise-bad-block (name)
  (error 'unavailable-block-error :tag name))

(define-condition stack-overflow (storage-condition)
  ()
  (:report "Stack overflow"))

(defun raise-stack-overflow ()
  (error 'stack-overflow))

(define-condition memory-fault-error (error)
  ((address :initarg :address
            :reader memory-fault-error-address))
  (:report (lambda (condition stream)
             (format stream "Unhandled memory fault on address #x~8,'0X"
                     (memory-fault-error-address condition)))))

(define-condition mezzano.supervisor:dma-buffer-expired (memory-fault-error)
  ()
  (:report (lambda (condition stream)
             (format stream "DMA buffer at address #x~8,'0X has expired"
                     (memory-fault-error-address condition)))))

(defun raise-memory-fault (address)
  (cond ((<= #x0000204000000000 address (1- #x0000208000000000))
         ;; TODO: Track the dma-buffer associated with each address and
         ;; include it in the condition.
         (error 'mezzano.supervisor:dma-buffer-expired :address address))
        (t
         (error 'memory-fault-error :address address))))

(in-package :mezzano.delimited-continuations)

(define-condition consumed-continuation-resumed (control-error)
  ((continuation :initarg :continuation
                 :reader consumed-continuation-resumed-continuation)
   (arguments :initarg :arguments
              :initform '()
              :reader consumed-continuation-resumed-arguments))
  (:report (lambda (condition stream)
             (format stream "Attempted to resume consumed continuation ~S with arguments ~:S."
                     (consumed-continuation-resumed-continuation condition)
                     (consumed-continuation-resumed-arguments condition)))))

(define-condition barrier-present (control-error)
  ((tag :initarg :tag
        :reader barrier-present-tag)
   (barrier :initarg :barrier
        :reader barrier-present-barrier))
  (:report (lambda (condition stream)
             (format stream "Cannot abort to prompt ~S, blocked by barrier ~S."
                     (barrier-present-tag condition)
                     (barrier-present-barrier condition)))))

(define-condition unknown-prompt-tag (control-error)
  ((tag :initarg :tag
        :reader unknown-prompt-tag-tag))
  (:report (lambda (condition stream)
             (format stream "Unknown prompt tag ~S."
                     (unknown-prompt-tag-tag condition)))))

(in-package :mezzano.clos)

(define-condition unknown-class (cell-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No class named ~S"
                     (cell-error-name condition)))))
