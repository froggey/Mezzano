;;;; setf-full.lisp
;;;; Contains forms that require the parsing code.

(in-package "SYSTEM.INTERNALS")

(defmacro define-modify-macro (name lambda-list function &optional documentation)
  (multiple-value-bind (required optional rest enable-keys keys allow-other-keys aux)
      (parse-ordinary-lambda-list lambda-list)
    (when (or enable-keys keys allow-other-keys aux)
      (error "&KEYS and &AUX not permitted in define-modify-macro lambda list"))
    (let ((reference (gensym)) (env (gensym)))
      `(defmacro ,name (&environment ,env ,reference ,@lambda-list)
	 ,documentation
	 (multiple-value-bind (dummies vals newvals setter getter)
	     (get-setf-expansion ,reference ,env)
	   (when (cdr newvals)
	     (error "Can't expand this"))
	   `(let* (,@(mapcar #'list dummies vals) (,(car newvals)
						   ,(list ',function getter
							  ,@required
							  ,@(mapcar #'car optional)
							  ,@(when rest (list rest)))))
	      ,setter))))))

(define-modify-macro incf (&optional (delta 1)) +)
(define-modify-macro decf (&optional (delta 1)) -)
