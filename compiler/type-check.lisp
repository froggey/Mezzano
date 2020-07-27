;;;; A pass to insert type checks around THE forms.

(in-package :mezzano.compiler)

(defgeneric insert-type-checks-1 (form value-context))

(defun insert-type-checks (form target)
  (declare (ignore target))
  (insert-type-checks-1 form :single))

(defmethod insert-type-checks-1 ((form lexical-variable) value-context)
  form)

(defmethod insert-type-checks-1 ((form lambda-information) value-context)
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (insert-type-checks-1 (second arg) :single)))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (insert-type-checks-1 (second arg) :single)))
    (setf (lambda-information-body form)
          (insert-type-checks-1 (lambda-information-body form) :tail)))
  form)

(defmethod insert-type-checks-1 ((form ast-call) value-context)
  (setf (arguments form)
        (loop
           for arg in (arguments form)
           collect (insert-type-checks-1 arg :single)))
  form)

(defmethod insert-type-checks-1 ((form ast-block) value-context)
  (setf (block-information-return-mode (info form)) value-context)
  (setf (body form) (insert-type-checks-1 (body form) value-context))
  form)

(defmethod insert-type-checks-1 ((form ast-function) value-context)
  form)

(defmethod insert-type-checks-1 ((form ast-go) value-context)
  (setf (info form) (insert-type-checks-1 (info form) :single))
  form)

(defmethod insert-type-checks-1 ((form ast-if) value-context)
  (setf (test form) (insert-type-checks-1 (test form) :single)
        (if-then form) (insert-type-checks-1 (if-then form) value-context)
        (if-else form) (insert-type-checks-1 (if-else form) value-context))
  form)

(defmethod insert-type-checks-1 ((form ast-let) value-context)
  (setf (bindings form) (loop
                           for (var initform) in (bindings form)
                           collect (list var (insert-type-checks-1 initform :single))))
  (setf (body form) (insert-type-checks-1 (body form) value-context))
  form)

(defmethod insert-type-checks-1 ((form ast-multiple-value-bind) value-context)
  (setf (value-form form) (insert-type-checks-1 (value-form form) :multiple)
        (body form) (insert-type-checks-1 (body form) value-context))
  form)

(defmethod insert-type-checks-1 ((form ast-multiple-value-call) value-context)
  (setf (function-form form) (insert-type-checks-1 (function-form form) :single)
        (value-form form) (insert-type-checks-1 (value-form form) :multiple))
  form)

(defmethod insert-type-checks-1 ((form ast-multiple-value-prog1) value-context)
  (setf (value-form form) (insert-type-checks-1 (value-form form) value-context)
        (body form) (insert-type-checks-1 (body form) :effect))
  form)

(defmethod insert-type-checks-1 ((form ast-progn) value-context)
  (setf (forms form) (loop
                        for (subform . rest) on (forms form)
                        collect (insert-type-checks-1 subform
                                                      (if rest
                                                          :effect
                                                          value-context))))
  form)

(defmethod insert-type-checks-1 ((form ast-quote) value-context)
  form)

(defmethod insert-type-checks-1 ((form ast-return-from) value-context)
  (setf (value form) (insert-type-checks-1 (value form)
                                           (block-information-return-mode (ast-target form)))
        (info form) (insert-type-checks-1 (info form) :single))
  form)

(defmethod insert-type-checks-1 ((form ast-setq) value-context)
  (setf (value form) (insert-type-checks-1 (value form) value-context))
  form)

(defmethod insert-type-checks-1 ((form ast-tagbody) value-context)
  (setf (statements form)
        (loop
           for (go-tag statement) in (statements form)
           collect (list go-tag (insert-type-checks-1 statement :effect))))
  form)

(defun parse-values-type (type)
  (cond ((not (typep type '(cons (eql values) t)))
         ;; Simple non-values type.
         (values '() (list type) 't 't))
        ((not (or (find '&optional (rest type))
                  (find '&rest (rest type))
                  (find '&allow-other-keys (rest type))))
         ;; Simple values type.
         (values '() (rest type) 't 't))
        (t
         ;; Complicated values type.
         (let ((required-types '())
               (optional-types '())
               (rest-type 'null)
               (allow-other-keys nil)
               (current (rest type)))
           ;; Collecting required types.
           (loop
              (when (or (endp current)
                        (member (first current) '(&optional &rest &allow-other-keys)))
                (return))
              (push (pop current) required-types))
           (when (eql (first current) '&optional)
             (pop current)
             ;; Collecting optional types.
             (loop
                (when (eql (first current) '&optional)
                  (error "Invalid VALUES type ~S. &OPTIONAL specified multiple times." type))
                (when (or (endp current)
                          (member (first current) '(&rest &allow-other-keys)))
                  (return))
                (push (pop current) optional-types)))
           (when (eql (first current) '&rest)
             (pop current)
             (when (endp current)
               (error "Invalid VALUES type ~S. &REST not followed by typespec."
                      type))
             (when (eql (first current) '&allow-other-keys)
               (error "Invalid VALUES type ~S. &REST not followed by typespec, saw &ALLOW-OTHER-KEYS."
                      type))
             (setf rest-type (pop current)))
           (when (eql (first current) '&allow-other-keys)
             (pop current)
             (setf allow-other-keys t))
           (when (not (endp current))
             (error "Invalid VALUES type ~S. Junk at end." type))
           (values (reverse required-types)
                   (reverse optional-types)
                   rest-type
                   allow-other-keys)))))

(defun simplify-complicated-function-type (type &optional environment)
  "Reduce complicated function types like (FUNCTION (T T) BOOLEAN) to just FUNCTION.
Descends into inner AND/OR/NOT types."
  (labels ((frob (type)
             (let ((expanded (sys.int::typeexpand type environment)))
               ;; Try to keep the original type intact as much as possible.
               (cond ((not (consp expanded))
                      (values type nil))
                     ((eql (first expanded) 'function)
                      (values 'function t))
                     ((member (first expanded) '(and or not))
                      (let* ((did-change nil)
                             (new (loop
                                     for ty in (rest expanded)
                                     collect (multiple-value-bind (new-ty differentp)
                                                 (frob ty)
                                               (when differentp
                                                 (setf did-change t))
                                               new-ty))))
                        (if did-change
                            (values (list* (first expanded) new) t)
                            (values type nil))))
                     (t
                      (values type nil))))))
    (values (frob type))))

(defmethod insert-type-checks-1 ((form ast-the) value-context)
  ;; There are a few possible cases:
  ;; 1) Not compiling at safety 3. Don't do anything.
  ;; 2) A type of (VALUES). Don't do anything.
  ;; 3) Single/effect value context with a single-value type X or (VALUES X).
  ;;    Bind the value to a variable and test it.
  ;; 4) Single/effect value context with a multiple-value type or
  ;;    multiple/tail value context. Save values, inspect them.
  ;; TODO: What does &ALLOW-OTHER-KEYS mean?
  (multiple-value-bind (required-typespecs optional-typespecs rest-typespec)
      (parse-values-type (ast-the-type form))
    (setf (value form)
          (cond ((or (not (eql (optimize-quality form 'safety) 3))
                     ;; (VALUES T...)
                     (and (endp required-typespecs)
                          (every (lambda (x) (eql x 't)) optional-typespecs)
                          (eql rest-typespec 't)))
                 (insert-type-checks-1 (value form) value-context))
                ((and (endp required-typespecs)
                      (eql (length optional-typespecs) 1)
                      (eql rest-typespec 't)
                      (or (member value-context '(:effect :single))
                          ;; Lexical variable obviously only generates one value.
                          (typep (unwrap-the (value form)) 'lexical-variable)))
                 ;; Single value case.
                 (ast `(let ((val ,(insert-type-checks-1 (value form) :single)))
                         (progn
                           (if (source-fragment (typep val ',(simplify-complicated-function-type (first optional-typespecs))))
                               'nil
                               (progn
                                 (call sys.int::raise-type-error val ',(ast-the-type form))
                                 (call sys.int::%%unreachable)))
                           val))
                      form))
                (t
                 ;; Complicated case, many types.
                 ;; This preserves all values passed through the THE.
                 ;; TODO: Make this more efficient. Save values a la M-V-P1
                 ;; then inspect the saved values.
                 (let ((req-values (loop
                                      for ty in required-typespecs
                                      collect (gensym)))
                       (opt-values (loop
                                      for ty in optional-typespecs
                                      collect (gensym)))
                       (rest-value (gensym)))
                   (insert-type-checks-1
                    (ast `(multiple-value-call
                              (source-fragment
                               (lambda (,@req-values &optional ,@opt-values &rest ,rest-value sys.int::&count n-values)
                                 (declare (dynamic-extent ,rest-value)
                                          (sys.int::lambda-name (the ,(ast-the-type form))))
                                 ,@(loop
                                      for ty in required-typespecs
                                      for var in req-values
                                      collect `(the ,ty ,var))
                                 ,@(loop
                                      for ty in optional-typespecs
                                      for var in opt-values
                                      collect `(the ,ty ,var))
                                 (the ,rest-typespec ,rest-value)
                                 ;; Avoid consing...
                                 (let* ((opt-list (list* ,@opt-values ,rest-value))
                                        (req-list (list* ,@req-values opt-list)))
                                   (declare (dynamic-extent opt-list req-list))
                                   ;; Slice the list down to the values actually returned.
                                   ,@(when opt-values
                                       ;; Only needed when there are optional values (I think?)
                                       `((when (< n-values ,(+ (length req-values) (length opt-values)))
                                           (let* ((scratch (cons nil req-list))
                                                  (last (nthcdr n-values scratch)))
                                             (declare (dynamic-extent scratch))
                                             (setf (cdr last) nil
                                                   req-list (cdr scratch))))))
                                   (values-list req-list))))
                            ,(value form))
                         form)
                    value-context))))))
  form)

(defmethod insert-type-checks-1 ((form ast-unwind-protect) value-context)
  (setf (protected-form form) (insert-type-checks-1 (protected-form form) value-context))
  (setf (cleanup-function form) (insert-type-checks-1 (cleanup-function form) :effect))
  form)

(defmethod insert-type-checks-1 ((form ast-jump-table) value-context)
  (setf (value form) (insert-type-checks-1 (value form) :single))
  (setf (targets form) (loop
                          for target in (targets form)
                          collect (insert-type-checks-1 target value-context)))
  form)
