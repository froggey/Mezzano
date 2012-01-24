;;; This is the compiler pass that performs lambda-lifting.

(in-package #:system.compiler)

(defun ll-form (form)
  (etypecase form
    (cons (case (first form)
	    ((block) (ll-block form))
	    ((go) (ll-go form))
	    ((if) (ll-if form))
	    ((let) (ll-let form))
	    ((load-time-value) (ll-load-time-value form))
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
  (dolist (binding (second form))
    (when (and (lexical-variable-p (first binding))
	       (not (eql (lexical-variable-definition-point (first binding)) *current-lambda*)))
      (incf *change-count*)
      (setf (lexical-variable-definition-point (first binding)) *current-lambda*))
    (setf (second binding) (ll-form (second binding))))
  (ll-implicit-progn (cddr form))
  form)

;;;(defun ll-load-time-value (form))

;;; TODO: Do lifting here as well. (single value functions, quote)
(defun ll-multiple-value-call (form)
  (ll-implicit-progn (cdr form))
  form)

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

(defun lift-lambda (lambda arg-list)
  (let ((name (lambda-information-name lambda))
	(required-args (lambda-information-required-args lambda))
	(optional-args (lambda-information-optional-args lambda))
	(rest-arg (lambda-information-rest-arg lambda)))
    ;; Attempt to match the argument list with the function's lambda list.
    (when (or optional-args rest-arg)
      ;; Bail out.
      (warn 'simple-style-warning
	    :format-control "Cannot inline ~S yet."
	    :format-arguments (list name))
      (return-from lift-lambda))
    (when (/= (length arg-list) (length required-args))
      ;; Bail out.
      (warn 'simple-warning
	    :format-control "Not inlining ~S, called with ~S arguments but wanted ~S."
	    :format-arguments (list name (length arg-list) (length required-args)))
      (return-from lift-lambda))
    (incf *change-count*)
    ;; Fix argument definition points.
    (dolist (arg required-args)
      (when (lexical-variable-p arg)
	(setf (lexical-variable-definition-point arg) *current-lambda*)))
    (dolist (arg optional-args)
      (when (lexical-variable-p arg)
	(setf (lexical-variable-definition-point arg) *current-lambda*)))
    (when (lexical-variable-p rest-arg)
      (setf (lexical-variable-definition-point rest-arg) *current-lambda*))
    `(let ,(mapcar #'list required-args arg-list)
       ,@(mapcar #'ll-form (lambda-information-body lambda)))))

(defun ll-function-form (form)
  (ll-implicit-progn (cdr form))
  (or (and (eq (first form) 'funcall)
	   (lambda-information-p (second form))
	   (lift-lambda (second form) (cddr form)))
      ;; Couldn't lift.
      (progn (ll-implicit-progn (cdr form))
	     form)))

(defun ll-variable (form)
  form)

(defun ll-lambda (form)
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (ll-form (second arg))))
    (ll-implicit-progn (lambda-information-body form)))
  form)
