;;;; Generate CL(ish) code from the CPS IR.

(in-package #:sys.newc)

(defun genname (x)
  (gensym (format nil "~A" x)))

(defun continuationp (closure)
  (getf (plist closure) 'continuation))

(defun emit-arg (val env)
  (etypecase val
    (closure
     (assert (not (continuationp val)))
     (emit-closure val env))
    (lexical
     (cdr (assoc val env)))
    (constant
     (list 'quote (constant-value val)))))

(defun emit-continuation (cont env)
  (etypecase cont
    (closure
     (assert (continuationp cont))
     (let* ((param-symbols (mapcar 'genname (closure-required-params cont)))
            (env (nconc (mapcar 'cons (closure-required-params cont) param-symbols) env)))
       `(lambda ,param-symbols
          ,(emit-application cont env))))
    (lexical
     (cdr (assoc cont env)))))

(defun emit-tagbody (closure env)
  ;; All arguments to %tagbody must be non-continuation
  ;; closures, except the first and there must be at
  ;; least two arguments.
  (assert (and (>= (length (closure-arguments closure)) 2)
               (every (lambda (x) (and (typep x 'closure)
                                       (not (continuationp x))))
                      (rest (closure-arguments closure)))))
  (let* ((last-lambda (car (last (closure-arguments closure))))
         (go-tag-params (rest (closure-required-params last-lambda)))
         (go-tags (mapcar 'genname go-tag-params))
         (go-tag-continuations (mapcar (lambda (name)
                                         `(lambda (&rest args)
                                            (declare (ignore args))
                                            (go ,name)))
                                       go-tags))
         (exit-tag (gensym "exit")))
    `(tagbody ,@(mapcan (lambda (closure next-tag)
                          (let* ((params (closure-required-params closure))
                                 (param-symbols (mapcar 'genname params))
                                 (new-env (nconc (mapcar 'cons params param-symbols) env)))
                            (list `(let ,(mapcar 'list param-symbols go-tag-continuations)
                                     ,(emit-application closure new-env))
                                  next-tag)))
                        (rest (closure-arguments closure))
                        go-tags)
        ,(let* ((params (closure-required-params last-lambda))
                (param-symbols (mapcar 'genname params))
                (new-env (nconc (mapcar 'cons params param-symbols) env)))
               `(let ,(mapcar 'list param-symbols (list* `(lambda (&rest args)
                                                            (declare (ignore args))
                                                            (go ,exit-tag))
                                                         go-tag-continuations))
                  ,(emit-application last-lambda new-env)))
        ,exit-tag)))

(defun emit-application (closure env)
  (etypecase (closure-function closure)
    (closure
     (cond ((continuationp (closure-function closure))
            ;; Simple binding.
            (let* ((param-symbols (mapcar 'genname (closure-required-params (closure-function closure)))))
              `(let ,(mapcar (lambda (p val)
                               (list p (emit-arg val env)))
                             param-symbols
                             (closure-arguments closure))
                 ,(emit-application (closure-function closure)
                                    (nconc (mapcar 'cons (closure-required-params (closure-function closure)) param-symbols) env)))))
           (t (error "TODO: non-binding closure application."))))
    (lexical
     ;; First argument is the continuation.
     `(funcall ,(emit-continuation (first (closure-arguments closure)) env)
               (funcall ,(cdr (assoc (closure-function closure) env))
                        ,@(mapcar (lambda (arg) (emit-arg arg env))
                                  (rest (closure-arguments closure))))))
    (constant
     (case (constant-value (closure-function closure))
       ((%if)
        ;; Special, because it has two continuations.
        `(if ,(emit-arg (third (closure-arguments closure)) env)
             (funcall ,(emit-continuation (first (closure-arguments closure)) env))
             (funcall ,(emit-continuation (second (closure-arguments closure)) env))))
       ((%block)
        ;; Block lifetimes are handled by CL's block form, no need for anything special.
        `(funcall ,(emit-continuation (first (closure-arguments closure)) env)
                  (funcall ,(emit-arg (second (closure-arguments closure)) env))))
       ((%tagbody)
        (emit-tagbody closure env))
       ((%invoke-continuation)
        ;; Not very special, could be implemented as
        ;; (defun %invoke-continuation (&rest args) (values-list args))
        ;; Just elide it.
        `(funcall ,(emit-continuation (first (closure-arguments closure)) env)
                  ,@(mapcar (lambda (arg) (emit-arg arg env))
                            (rest (closure-arguments closure)))))
       ((%make-cell)
        (destructuring-bind (cont value) (closure-arguments closure)
          (assert (and (typep cont 'closure)
                       (continuationp cont)
                       (= (length (closure-required-params cont)) 1)))
          (let ((sym (genname (first (closure-required-params cont)))))
            `(let ((,sym ,(emit-arg value env)))
               ,(emit-application cont (list* (cons (first (closure-required-params cont)) sym)
                                              env))))))
       ((%contents)
        (destructuring-bind (cont cell) (closure-arguments closure)
          (check-type cell lexical)
          `(funcall ,(emit-continuation cont env) ,(cdr (assoc cell env)))))
       ((%set-contents)
        (destructuring-bind (cont cell new-value) (closure-arguments closure)
          (check-type cell lexical)
          `(funcall ,(emit-continuation cont env) (setq ,(cdr (assoc cell env)) ,(emit-arg new-value env)))))
       (t ;; First argument is the continuation.
        `(funcall ,(emit-continuation (first (closure-arguments closure)) env)
                  (funcall ',(constant-value (closure-function closure))
                           ,@(mapcar (lambda (arg) (emit-arg arg env))
                                     (rest (closure-arguments closure))))))))))

(defun emit-closure (closure env)
  (check-type closure (and closure (not (satisfies continuationp))))
  (let* ((param-symbols (mapcar 'genname (closure-required-params closure)))
         (env (nconc (mapcar 'cons (closure-required-params closure) param-symbols) env))
         (block-name (gensym)))
    `(lambda ,(rest param-symbols)
       (block ,block-name
         (let ((,(first param-symbols) (lambda (&rest args) (return-from ,block-name (values-list args)))))
           ,(emit-application closure env))))))

(defun emit-cl (closure)
  (let ((code (emit-closure closure '())))
    (if *prettify-emitted-code*
        (prettify-emitted-code code)
        code)))

(defvar *prettify-emitted-code* nil)

(defun prettify-emitted-code (code)
  (typecase code
    (cons
     (case (first code)
       ((funcall)
        (cond ((and (= (length code) 2)
                    (consp (second code))
                    (eql (first (second code)) 'lambda)
                    (endp (second (second code))))
               ;; (funcall (lambda () ...)) => ...
               (prettify-emitted-code `(progn ,@(cddr (second code)))))
              ((and (listp (second code))
                    (= (length (second code)) 2)
                    (eql (first (second code)) 'quote)
                    (symbolp (second (second code))))
               ;; (funcall 'symbol ...) => (symbol ...)
               (list* (second (second code))
                      (mapcar 'prettify-emitted-code (cddr code))))
              (t (mapcar 'prettify-emitted-code code))))
       ((quote) code)
       ((let)
        ;; Collapse nested LETs.
        (let ((inner (prettify-emitted-code `(progn ,@(cddr code))))
              (bindings (mapcar (lambda (b)
                                  (list (first b) (prettify-emitted-code (second b))))
                                (second code))))
          (when (and (consp inner)
                     (member (first inner) '(let let*)))
            (setf bindings (append bindings (second inner))
                  inner `(progn ,@(cddr inner))))
          ;; Empty LETs will become PROGNs.
          (if (endp bindings)
              (prettify-emitted-code inner)
              `(let* ,bindings ,(prettify-emitted-code inner)))))
       ((progn)
        (if (null (cddr code))
            (prettify-emitted-code (second code))
            `(progn ,@(mapcar 'prettify-emitted-code (cdr code)))))
       (t (mapcar 'prettify-emitted-code code))))
    (t code)))
