(in-package :sys.c)

(defclass symbol-macro ()
  ((%name :initarg :name :accessor name)
   (%expansion :initarg :expansion :accessor symbol-macro-expansion)))

(defclass top-level-function ()
  ((%name :initarg :name :accessor name)))

(defun function-name-p (object)
  (or (symbolp object)
      (and (consp object)
           (consp (rest object))
           (null (cddr object))
           (member (first object) '(setf sys.int::cas))
           (symbolp (second object)))))

(defgeneric lookup-variable-in-environment (symbol environment))
(defgeneric lookup-function-in-environment (name environment))
(defgeneric inline-info-in-environment (name environment))
(defgeneric lookup-block-in-environment (tag environment))
(defgeneric lookup-go-tag-in-environment (tag environment))
(defgeneric environment-macro-definitions-only (environment))
(defgeneric compiler-macro-function-in-environment (name environment))
(defgeneric macro-function-in-environment (symbol environment))
(defgeneric lookup-variable-declared-type-in-environment (symbol environment))
(defgeneric optimize-qualities-in-environment (environment))

;;; Lexical environments.

(defclass lexical-environment ()
  ((%variables :initform '())
   (%functions :initform '())
   (%blocks :initform '())
   (%go-tags :initform '())
   (%inline-decls :initform '())
   (%variable-type-decls :initform '())
   (%optimize :initform '())))

(defun extend-environment (environment &key
                                         variables
                                         functions
                                         blocks
                                         go-tags
                                         declarations)
  (assert (every (lambda (v) (symbolp (first v))) variables))
  (assert (every (lambda (v) (function-name-p (first v))) functions))
  (assert (every (lambda (v) (symbolp (first v))) blocks))
  (assert (every (lambda (v) (or (symbolp (first v))
                                 (integerp (first v))))
                 go-tags))
  (check-type environment (or null lexical-environment))
  (when (and (endp variables)
             (endp functions)
             (endp blocks)
             (endp go-tags)
             (endp declarations))
    (return-from extend-environment environment))
  (when (null environment)
    (setf environment (make-instance 'lexical-environment)))
  (let ((sub (make-instance 'lexical-environment)))
    (setf (slot-value sub '%variables) (append variables
                                               (loop
                                                  for (what . names) in declarations
                                                  when (eql what 'special)
                                                  append (mapcar (lambda (x)
                                                                   (list x (make-instance 'special-variable :name x)))
                                                                 (remove-if (lambda (x) (member x variables :key #'first))
                                                                            names)))
                                               (slot-value environment '%variables)))
    (setf (slot-value sub '%functions) (append functions
                                               (slot-value environment '%functions)))
    (setf (slot-value sub '%blocks) (append blocks
                                               (slot-value environment '%blocks)))
    (setf (slot-value sub '%go-tags) (append go-tags
                                             (slot-value environment '%go-tags)))
    (let ((new-decls '()))
      (flet ((add-decl (name what)
               (let ((existing (assoc name new-decls :test #'equal)))
                 (cond (existing
                        (when (not (eql (second existing) what))
                          (error "~S ~S declaration conflicts with ~S declaration."
                                 what name (second existing))))
                       (t
                        (push (list name what) new-decls))))))
        (loop
           for (what . names) in declarations
           when (member what '(inline notinline))
           do (loop for name in names do (add-decl name what))))
      (dolist (func functions)
        (when (not (assoc (first func) new-decls :test #'equal))
          (push (list (first func) nil) new-decls)))
      (setf (slot-value sub '%inline-decls) (append new-decls (slot-value environment '%inline-decls))))
    (let ((new-decls '()))
      (flet ((add-decl (name type)
               (let* ((new-var (assoc name variables))
                      (actual-var (or (and new-var (second new-var))
                                      (lookup-variable-in-environment name environment)))
                      (real-type (etypecase actual-var
                                   ((or lexical-variable
                                        symbol-macro)
                                    (if new-var
                                        type
                                        (let ((old-type (lookup-variable-declared-type-in-environment name environment)))
                                          (if (eql old-type 't)
                                              type
                                              `(and ,type ,old-type)))))
                                   (special-variable
                                    (let ((old-type (mezzano.runtime::symbol-type name)))
                                      (if (eql old-type 't)
                                          type
                                          `(and ,type ,old-type)))))))
                 (when (assoc name new-decls :key #'name)
                   (error "Multiple type declarations for ~S." name))
                 (push (list actual-var real-type) new-decls))))
        (loop
           for (what type . names) in declarations
           ;; TODO: Pick up (declare (fixnum ...)) and similar.
           when (eql what 'type)
           do (loop for name in names do (add-decl name type)))
        (loop
           for (name var) in variables
           when (and (typep var 'lexical-variable)
                     (not (assoc var new-decls)))
           do (push (list var 't) new-decls))
        (loop
           for (var type) in (slot-value environment '%variable-type-decls)
           when (not (assoc var new-decls))
           do (push (list var type) new-decls))
        (setf (slot-value sub '%variable-type-decls) new-decls)))
    (let ((optimize-settings (optimize-qualities-in-environment environment)))
      (loop
         for (what . qualities) in declarations
         when (eql what 'optimize)
         do
           (dolist (quality qualities)
             (destructuring-bind (quality value)
                 (if (symbolp quality)
                     `(,quality 3)
                     quality)
               (check-type quality (member compilation-speed debug safety space speed))
               (check-type value (member 0 1 2 3))
               (let ((current (getf optimize-settings quality 0)))
                 (setf (getf optimize-settings quality) (max value current))))))
      (setf (slot-value sub '%optimize) optimize-settings))
    sub))

(defmethod lookup-variable-in-environment (symbol (environment lexical-environment))
  (check-type symbol symbol)
  (let ((data (assoc symbol (slot-value environment '%variables))))
    (if data
        (second data)
        (lookup-variable-in-environment symbol nil))))

(defmethod lookup-function-in-environment (name (environment lexical-environment))
  (let ((data (assoc name (slot-value environment '%functions)
                     :test #'equal)))
    (if data
        (second data)
        (lookup-function-in-environment name nil))))

(defmethod inline-info-in-environment (name (environment lexical-environment))
  (let ((data (assoc name (slot-value environment '%inline-decls)
                     :test #'equal)))
    (cond (data
           (if (typep (lookup-function-in-environment name environment)
                      'top-level-function)
               (values (second data)
                       (nth-value 1 (function-inline-info name)))
               (values (second data)
                       nil)))
          (t
           (inline-info-in-environment name nil)))))

(defmethod lookup-block-in-environment (tag (environment lexical-environment))
  (let ((data (assoc tag (slot-value environment '%blocks))))
    (if data
        (second data)
        (lookup-block-in-environment tag nil))))

(defmethod lookup-go-tag-in-environment (tag (environment lexical-environment))
  (let ((data (assoc tag (slot-value environment '%go-tags))))
    (if data
        (second data)
        (lookup-go-tag-in-environment tag nil))))

(defmethod environment-macro-definitions-only ((environment lexical-environment))
  (extend-environment
   nil
   :variables (remove-if-not (lambda (x) (typep (second x) 'symbol-macro))
                             (slot-value environment '%variables))
   :functions (remove-if-not (lambda (x) (typep (second x) 'function))
                             (slot-value environment '%functions))))

(defmethod compiler-macro-function-in-environment (name (environment lexical-environment))
  (cond ((find name (slot-value environment '%functions) :test #'equal :key #'first)
         nil)
        (t
         (compiler-macro-function-in-environment name nil))))

(defmethod macro-function-in-environment (symbol (environment lexical-environment))
  (let ((data (assoc symbol (slot-value environment '%functions)
                     :test #'equal)))
    (cond (data
           (when (functionp (second data))
             (second data)))
          (t
           (macro-function-in-environment symbol nil)))))

(defmethod lookup-variable-declared-type-in-environment (symbol (environment lexical-environment))
  (let ((ty (assoc symbol (slot-value environment '%variable-type-decls)
                   :key #'name)))
    (if ty
        (second ty)
        (lookup-variable-declared-type-in-environment symbol nil))))

(defmethod optimize-qualities-in-environment ((environment lexical-environment))
  (slot-value environment '%optimize))
