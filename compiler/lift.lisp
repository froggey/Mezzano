;;; This is the compiler pass that performs lambda-lifting.

(in-package :sys.c)

(defun ll-form (form)
  (etypecase form
    (cons (case (first form)
	    ((block) (ll-block form))
	    ((go) (ll-go form))
	    ((if) (ll-if form))
	    ((let) (ll-let form))
	    ((load-time-value) (ll-load-time-value form))
	    ((multiple-value-bind) (ll-multiple-value-bind form))
	    ((multiple-value-call) (ll-multiple-value-call form))
	    ((multiple-value-prog1) (ll-multiple-value-prog1 form))
	    ((progn) (ll-progn form))
	    ((progv) (ll-progv form))
	    ((quote) (ll-quote form))
	    ((return-from) (ll-return-from form))
	    ((setq) (ll-setq form))
	    ((tagbody) (ll-tagbody form))
	    ((the) (ll-the form))
	    ((unwind-protect) (ll-unwind-protect form))
	    (t (ll-function-form form))))
    (lexical-variable (ll-variable form))
    (lambda-information (ll-lambda form))))

(defun ll-implicit-progn (x)
  (do ((i x (cdr i)))
      ((endp i))
    (setf (car i) (ll-form (car i)))))

(defun ll-block (form)
  (unless (eql (lexical-variable-definition-point (second form)) *current-lambda*)
    (incf *change-count*)
    ;; Update the definition point.
    (setf (lexical-variable-definition-point (second form)) *current-lambda*))
  (ll-implicit-progn (cddr form))
  form)

(defun ll-go (form)
  form)

(defun ll-if (form)
  (setf (second form) (ll-form (second form))
	(third form) (ll-form (third form))
	(fourth form) (ll-form (fourth form)))
  form)

(defun ll-let (form)
  ;; Patch up definition points after a lambda has been lifted.
  (dolist (binding (second form))
    (when (and (lexical-variable-p (first binding))
	       (not (eql (lexical-variable-definition-point (first binding)) *current-lambda*)))
      (incf *change-count*)
      (setf (lexical-variable-definition-point (first binding)) *current-lambda*))
    (setf (second binding) (ll-form (second binding))))
  (ll-implicit-progn (cddr form))
  form)

;;;(defun ll-load-time-value (form))

(defun ll-multiple-value-bind (form)
  ;; Patch up definition points after a lambda has been lifted.
  (dolist (var (second form))
    (when (and (lexical-variable-p var)
	       (not (eql (lexical-variable-definition-point var) *current-lambda*)))
      (incf *change-count*)
      (setf (lexical-variable-definition-point var) *current-lambda*)))
  (ll-implicit-progn (cddr form))
  form)

;; Reduce (multiple-value-call #'(lambda (&optional ... &rest unused) ...) value-form)
;; back down to (multiple-value-bind (vars...) value-form body...).
(defun ll-multiple-value-call (form)
  (cond ((and (lambda-information-p (second form))
              (null (lambda-information-required-args (second form)))
              (lambda-information-rest-arg (second form))
              (not (lambda-information-enable-keys (second form)))
              (every (lambda (x)
                       (and (equal ''nil (second x)) ; An init-form of NIL.
                            (eql (third x) nil)))    ; No suppliedp arg.
                     (lambda-information-optional-args (second form)))
              (lexical-variable-p (lambda-information-rest-arg (second form)))
              (zerop (lexical-variable-use-count (lambda-information-rest-arg (second form))))
              (= (length form) 3))
         (incf *change-count*)
         ;; Variable definition points will be fixed up by LL-MULTIPLE-VALUE-BIND.
         (ll-form `(multiple-value-bind ,(mapcar 'first (lambda-information-optional-args (second form)))
                       ,(third form)
                     ,@(lambda-information-body (second form)))))
        (t (ll-implicit-progn (cdr form))
           form)))

(defun ll-multiple-value-prog1 (form)
  (ll-implicit-progn (cdr form))
  form)

(defun ll-progn (form)
  (ll-implicit-progn (cdr form))
  form)

(defun ll-progv (form)
  (ll-implicit-progn (cdr form))
  form)

(defun ll-quote (form)
  form)

(defun ll-return-from (form)
  (setf (third form) (ll-form (third form)))
  form)

(defun ll-setq (form)
  (setf (third form) (ll-form (third form)))
  form)

(defun ll-tagbody (form)
  (unless (eq (tagbody-information-definition-point (second form)) *current-lambda*)
    (incf *change-count*)
    (setf (tagbody-information-definition-point (second form)) *current-lambda*))
  (do ((i (cddr form) (cdr i)))
      ((endp i))
    (unless (go-tag-p (car i))
      (setf (car i) (ll-form (car i)))))
  form)

(defun ll-the (form)
  (setf (third form) (ll-form (third form)))
  form)

(defun ll-unwind-protect (form)
  (ll-implicit-progn (cdr form))
  form)

;; NOTE: Only support required and keyword args.
;; Doesn't support fuzzy allow-other-keys matching or suppliedp variables.
(defun arguments-match-lambda-list (lambda arg-list)
  (let ((arg-count (length arg-list))
        (req-count (length (lambda-information-required-args lambda))))
    (cond ((lambda-information-enable-keys lambda)
           (let ((keywords (mapcar 'caar (lambda-information-key-args lambda))))
             (when (and (>= arg-count req-count)
                        (evenp (- arg-count req-count)))
               (do ((i (nthcdr req-count arg-list) (cddr i)))
                   ((null i)
                    t)
                 (when (third (car i))
                   (return nil))
                 (unless (and (listp (car i))
                              (= (length (car i)) 2)
                              (eql (first (car i)) 'quote)
                              (member (second (car i)) keywords))
                   (return nil))))))
          (t (= arg-count req-count)))))

(defun lift-lambda (lambda arg-list)
  (let ((name (lambda-information-name lambda))
	(required-args (lambda-information-required-args lambda))
	(optional-args (lambda-information-optional-args lambda))
	(rest-arg (lambda-information-rest-arg lambda))
        (key-args (lambda-information-key-args lambda)))
    ;; Attempt to match the argument list with the function's lambda list.
    (when (or optional-args rest-arg)
      ;; Bail out.
      (warn 'sys.int::simple-style-warning
	    :format-control "Cannot inline ~S yet."
	    :format-arguments (list name))
      (return-from lift-lambda))
    (unless (arguments-match-lambda-list lambda arg-list)
      ;; Bail out.
      (warn 'simple-warning
	    :format-control "Not inlining ~S, arguments do not match."
	    :format-arguments (list name))
      (return-from lift-lambda))
    (incf *change-count*)
    ;; Fix argument definition points.
    (dolist (arg required-args)
      (when (lexical-variable-p arg)
	(setf (lexical-variable-definition-point arg) *current-lambda*)))
    #+nil(dolist (arg optional-args)
      (when (lexical-variable-p arg)
	(setf (lexical-variable-definition-point arg) *current-lambda*)))
    #+nil(when (lexical-variable-p rest-arg)
      (setf (lexical-variable-definition-point rest-arg) *current-lambda*))
    (dolist (arg key-args)
      (when (lexical-variable-p (second (first arg)))
	(setf (lexical-variable-definition-point (second (first arg))) *current-lambda*))
      (when (lexical-variable-p (third arg))
	(setf (lexical-variable-definition-point (third arg)) *current-lambda*)))
    (let* ((argument-vars (mapcar (lambda (x)
                                    (declare (ignore x))
                                    (make-lexical-variable :name (gensym)
                                                           :definition-point *current-lambda*
                                                           :ignore :maybe))
                                  arg-list))
           (key-pairs (nthcdr (length required-args) argument-vars)))
      (labels ((build-key-bindings (keys)
                 (cond (keys
                        (do ((p 0 (+ p 2))
                             (i (nthcdr (length required-args) arg-list) (cddr i)))
                            ((null i)
                             ;; Not provided, use the initform.
                             `(let ((,(cadar (first keys)) ,(second (first keys))))
                                ,(build-key-bindings (rest keys))))
                          (when (eql (second (car i)) (caar (first keys)))
                            ;; Keywords match, use this argument.
                            (return `(let ((,(cadar (first keys)) ,(nth (1+ p) key-pairs)))
                                       ,(build-key-bindings (rest keys)))))))
                       (t `(progn ,@(mapcar #'ll-form (lambda-information-body lambda)))))))
        ;; Evaluate arguments.
        `(let ,(mapcar #'list argument-vars arg-list)
           ;; Bind required arguments.
           (let ,(mapcar #'list required-args argument-vars)
             ,(build-key-bindings key-args)))))))

(defun ll-function-form (form)
  (ll-implicit-progn (cdr form))
  (if *should-inline-functions*
      (or (and (eq (first form) 'funcall)
               (lambda-information-p (second form))
               (lift-lambda (second form) (cddr form)))
          ;; Couldn't lift.
          form)
      form))

(defun ll-variable (form)
  form)

(defun ll-lambda (form)
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (ll-form (second arg))))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (ll-form (second arg))))
    (ll-implicit-progn (lambda-information-body form)))
  form)
