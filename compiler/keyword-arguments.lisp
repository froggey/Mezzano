;;;; Lowering pass implementing &KEY parameters.

(in-package :mezzano.compiler)

(defun variable-name (var)
  (etypecase var
    (special-variable
     (name var))
    (lexical-variable
     (lexical-variable-name var))))

(defun emit-even-keyword-check (form count)
  ;; Test even number of keyword arguments.
  (let ((n-ordinary-arguments (+ (length (lambda-information-required-args form))
                                 (length (lambda-information-optional-args form)))))
    `(if (call eq (call %fast-fixnum-logand
                        ,(etypecase count
                           (special-variable
                            `(call symbol-value (quote ,count)))
                           (lexical-variable
                            count))
                        '1)
               '0)
         ,(if (oddp n-ordinary-arguments)
              '(call error
                'sys.int::simple-program-error
                ':format-control '"Odd number of &KEY arguments.")
              '(quote nil))
         ,(if (oddp n-ordinary-arguments)
              '(quote nil)
              '(call error
                'sys.int::simple-program-error
                ':format-control '"Odd number of &KEY arguments.")))))

(defun emit-key-correctness-check (keys rest)
  ;; Check that all keywords provided are keywords accepted by
  ;; this function, unless :A-O-K is supplied and true.
  (let ((valid-keywords (loop
                           for ((keyword var) init-form suppliedp) in keys
                           collect keyword)))
    ;; The list evenness was checked earlier, so it is safe to use
    ;; the raw %car/%cdr functions.
    `(let ((aok (quote nil)))
       (progn
         ;; Look for :ALLOW-OTHER-KEYS.
         (let ((aok-itr ,rest))
           (tagbody aok-tb
              (aok-entry
               (go aok-test aok-tb))
              (aok-head
               (progn
                 (if (call eq (call mezzano.runtime::%car aok-itr) (quote :allow-other-keys))
                     (progn
                       (setq aok (call mezzano.runtime::%car (call mezzano.runtime::%cdr aok-itr)))
                       (go aok-out aok-tb))
                     (quote nil))
                 (setq aok-itr (call mezzano.runtime::%cdr (call mezzano.runtime::%cdr aok-itr)))
                 (go aok-test aok-tb)))
              (aok-test
               (if aok-itr
                   (go aok-head aok-tb)
                   (go aok-out aok-tb)))
              (aok-out
               (quote nil))))
         (if aok
             (quote nil)
             ;; And keyword checking, if AOK was nil.
             (let ((chk-itr ,rest))
               (tagbody chk-tb
                  (chk-entry
                   (go chk-test chk-tb))
                  (chk-head
                   (progn
                     (let ((current-keyword (call mezzano.runtime::%car chk-itr)))
                       (if (call member current-keyword (quote (:allow-other-keys ,@valid-keywords)))
                           (quote nil)
                           (call error
                                 'sys.int::simple-program-error
                                 ':format-control '"Unknown &KEY argument ~S. Expected one of ~S."
                                 ':format-arguments (call list current-keyword (quote ,valid-keywords)))))
                     (setq chk-itr (call mezzano.runtime::%cdr (call mezzano.runtime::%cdr chk-itr)))
                     (go chk-test chk-tb)))
                  (chk-test
                   (if chk-itr
                       (go chk-head chk-tb)
                       (go chk-out chk-tb)))
                  (chk-out
                   (quote nil)))))))))

(defun emit-keyword-arg (rest keyword value suppliedp)
  ;; Perform processing for a single keyword argument.
  ;; Walk the list and look for it.
  `(let ((itr ,rest))
     (tagbody tb
        (entry
         (go test tb))
        (head
         (progn
           (if (call eq (call mezzano.runtime::%car itr) (quote ,keyword))
               (progn
                 (setq ,value (call mezzano.runtime::%car (call mezzano.runtime::%cdr itr)))
                 (setq ,suppliedp (quote t))
                 (go out tb))
               (quote nil))
           (setq itr (call mezzano.runtime::%cdr (call mezzano.runtime::%cdr itr)))
           (go test tb)))
        (test
         (if itr
             (go head tb)
             (go out tb)))
        (out
         (quote nil)))))

(defun lower-key-arguments* (form body original-rest keys allow-other-keys count)
  (let* ((values (mapcar (lambda (x)
                           (make-instance 'lexical-variable
                                          :inherit form
                                          :name (gensym (string (variable-name (cadar x))))
                                          :definition-point *current-lambda*))
                         keys))
         (suppliedp (mapcar (lambda (x)
                              (make-instance 'lexical-variable
                                             :inherit form
                                             :name (if (third x)
                                                       (gensym (string (variable-name (third x))))
                                                       (gensym))
                                             :definition-point *current-lambda*))
                            keys))
         (rest (if (lexical-variable-p original-rest)
                   original-rest
                   (make-instance 'lexical-variable
                                  :inherit form
                                  :name (gensym "REST")
                                  :definition-point *current-lambda*))))
    (labels ((bind-variable (name value)
               (list name
                     (wrap-type-check name (ast value form))))
             (create-key-let-body (key-args values suppliedp)
               (cond (key-args
                      `(let (,(bind-variable (second (first (first key-args)))
                                             `(if ,(first suppliedp)
                                                  ,(first values)
                                                  ,(second (first key-args)))))
                         ,(if (third (first key-args))
                              `(let (,(bind-variable (third (first key-args)) (first suppliedp)))
                                 ,(create-key-let-body (rest key-args) (rest values) (rest suppliedp)))
                              (create-key-let-body (rest key-args) (rest values) (rest suppliedp)))))
                     (t body))))
      (ast `(let (,@(mapcar (lambda (x) (list x '(quote nil))) values)
                  ,@(mapcar (lambda (x) (list x '(quote nil))) suppliedp)
                  ,@(unless (lexical-variable-p original-rest)
                      (list (list rest `(call symbol-value (quote ,original-rest))))))
              (progn
                ;; No processing at all if the list is empty.
                ;; This is needed to get the list length evenness check
                ;; right when optional arguments are not supplied.
                (if ,rest
                    (progn
                      ,(emit-even-keyword-check form count)
                      ,@(unless allow-other-keys
                          (list (emit-key-correctness-check keys rest)))
                      ,@(loop
                           for ((keyword original-value) init-form original-suppliedp) in keys
                           for value in values
                           for suppliedp in suppliedp
                           collect (emit-keyword-arg rest keyword value suppliedp)))
                    (quote nil))
                ,(create-key-let-body keys values suppliedp)))
           form))))

(defun lower-keyword-arguments (form architecture)
  (declare (ignore architecture))
  (lower-keyword-arguments-1 form)
  form)

(defgeneric lower-keyword-arguments-1 (form))

(defmethod lower-keyword-arguments-1 ((form ast-block))
  (lower-keyword-arguments-1 (body form)))

(defmethod lower-keyword-arguments-1 ((form ast-function)))

(defmethod lower-keyword-arguments-1 ((form ast-go))
  (lower-keyword-arguments-1 (info form)))

(defmethod lower-keyword-arguments-1 ((form ast-if))
  (lower-keyword-arguments-1 (test form))
  (lower-keyword-arguments-1 (if-then form))
  (lower-keyword-arguments-1 (if-else form)))

(defmethod lower-keyword-arguments-1 ((form ast-let))
  (loop
     for (variable init-form) in (bindings form)
     do (lower-keyword-arguments-1 init-form))
  (lower-keyword-arguments-1 (body form)))

(defmethod lower-keyword-arguments-1 ((form ast-multiple-value-bind))
  (lower-keyword-arguments-1 (value-form form))
  (lower-keyword-arguments-1 (body form)))

(defmethod lower-keyword-arguments-1 ((form ast-multiple-value-call))
  (lower-keyword-arguments-1 (function-form form))
  (lower-keyword-arguments-1 (value-form form)))

(defmethod lower-keyword-arguments-1 ((form ast-multiple-value-prog1))
  (lower-keyword-arguments-1 (value-form form))
  (lower-keyword-arguments-1 (body form)))

(defmethod lower-keyword-arguments-1 ((form ast-progn))
  (mapc #'lower-keyword-arguments-1 (forms form)))

(defmethod lower-keyword-arguments-1 ((form ast-quote)))

(defmethod lower-keyword-arguments-1 ((form ast-return-from))
  (lower-keyword-arguments-1 (value form))
  (lower-keyword-arguments-1 (info form)))

(defmethod lower-keyword-arguments-1 ((form ast-setq))
  (lower-keyword-arguments-1 (value form)))

(defmethod lower-keyword-arguments-1 ((form ast-tagbody))
  (loop
     for (go-tag statement) in (statements form)
     do (lower-keyword-arguments-1 statement)))

(defmethod lower-keyword-arguments-1 ((form ast-the))
  (lower-keyword-arguments-1 (value form)))

(defmethod lower-keyword-arguments-1 ((form ast-unwind-protect))
  (lower-keyword-arguments-1 (protected-form form))
  (lower-keyword-arguments-1 (cleanup-function form)))

(defmethod lower-keyword-arguments-1 ((form ast-call))
  (mapc #'lower-keyword-arguments-1 (arguments form)))

(defmethod lower-keyword-arguments-1 ((form ast-jump-table))
  (lower-keyword-arguments-1 (value form))
  (mapc #'lower-keyword-arguments-1 (targets form)))

(defmethod lower-keyword-arguments-1 ((form lexical-variable)))

(defmethod lower-keyword-arguments-1 ((form lambda-information))
  (let ((*current-lambda* form))
    (when (lambda-information-enable-keys form)
      ;; TODO: If &REST or &COUNT are special, they should be lowered to
      ;; lexicals and those lexicals used for the &KEY processing.
      (unless (lambda-information-count-arg form)
        ;; Add in a &COUNT arg.
        (setf (lambda-information-count-arg form)
              (make-instance 'lexical-variable
                             :inherit form
                             :name (gensym "COUNT")
                             :definition-point *current-lambda*
                             :ignore :maybe)))
      (unless (lambda-information-rest-arg form)
        ;; Add in a &REST arg and make it dynamic-extent.
        (setf (lambda-information-rest-arg form)
              (make-instance 'lexical-variable
                             :inherit form
                             :name (gensym "REST")
                             :definition-point *current-lambda*
                             :ignore :maybe
                             :dynamic-extent t)))
      (setf (lambda-information-body form)
            (lower-key-arguments* form
                                  (lambda-information-body form)
                                  (lambda-information-rest-arg form)
                                  (lambda-information-key-args form)
                                  (lambda-information-allow-other-keys form)
                                  (lambda-information-count-arg form)))
      ;; Remove the old keyword arguments.
      (setf (lambda-information-enable-keys form) nil
            (lambda-information-key-args form) '()
            (lambda-information-allow-other-keys form) nil)
      (change-made))
    (dolist (arg (lambda-information-optional-args form))
      (lower-keyword-arguments-1 (second arg)))
    (lower-keyword-arguments-1 (lambda-information-body form))))
