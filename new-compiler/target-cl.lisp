;;;; Generate CL(ish) code from the CPS IR.

(in-package #:sys.newc)

(defun genname (x)
  (gensym (format nil "~A" x)))

(defun continuationp (closure)
  (getf (plist closure) 'continuation))

(defvar *emited-tagbody-things*)
(defvar *current-closure*)
(defvar *use-map*)
(defvar *contour-map*)
(defvar *variable-value-map*)
(defvar *value-variable-map*)

(defun emit-arg (val env)
  (etypecase val
    (closure
     (if (continuationp val)
         (emit-continuation val env)
         (emit-closure val env)))
    (lexical
     (second (assoc val env)))
    (constant
     (list 'quote (constant-value val)))))

(defun emit-continuation (cont env)
  (etypecase cont
    (closure
     (assert (continuationp cont))
     (let* ((required-symbols (mapcar 'genname (closure-required-params* cont)))
            (optional-symbols (mapcar 'genname (mapcar 'first (closure-optional-params cont))))
            (optional-suppliedp-symbols (mapcar 'genname (mapcar 'second (closure-optional-params cont))))
            (rest-symbol (when (closure-rest-param cont) (genname (closure-rest-param cont))))
            (key-symbols (mapcar 'genname (mapcar 'second (closure-keyword-params cont))))
            (key-suppliedp-symbols (mapcar 'genname (mapcar 'third (closure-keyword-params cont))))
            (env (nconc (mapcar 'list (closure-required-params* cont) required-symbols)
                        (mapcar 'list (mapcar 'first (closure-optional-params cont)) optional-symbols)
                        (mapcar 'list (mapcar 'second (closure-optional-params cont)) optional-suppliedp-symbols)
                        (when rest-symbol
                          (list (list (closure-rest-param cont) rest-symbol)))
                        (mapcar 'list (mapcar 'second (closure-keyword-params cont)) key-symbols)
                        (mapcar 'list (mapcar 'third (closure-keyword-params cont)) key-suppliedp-symbols)
                        env)))
       (mapc (lambda (param sym)
               (setf (get sym 'use-count) (length (gethash param *use-map*))))
             (closure-required-params* cont)
             required-symbols)
       `(lambda (,@required-symbols
                 ,@(when optional-symbols
                     (list* '&optional
                            (mapcar (lambda (sym suppliedp) (list sym nil suppliedp))
                                    optional-symbols
                                    optional-suppliedp-symbols)))
                 ,@(when rest-symbol
                     (list '&rest rest-symbol))
                 ,@(when (closure-keyword-params-enabled cont)
                     `(&key
                       ,@(mapcar (lambda (keyword sym suppliedp)
                                   `((,keyword ,sym) nil ,suppliedp))
                                 (mapcar 'first (closure-keyword-params cont))
                                 key-symbols
                                 key-suppliedp-symbols)
                       ,@(when (closure-allow-other-keywords cont)
                           (list '&allow-other-keys)))))
          ,(emit-application cont env))))
    (lexical
     (second (assoc cont env)))))

(defun emit-tagbody (closure env)
  ;; All arguments to %tagbody must be non-continuation
  ;; closures, except the first and there must be at
  ;; least two arguments.
  (assert (and (>= (length (closure-arguments closure)) 2)
               (every (lambda (x) (and (typep x 'closure)
                                       (not (continuationp x))))
                      (rest (closure-arguments closure)))))
  (let* ((last-lambda (car (last (closure-arguments closure))))
         (go-tag-params (rest (closure-required-params* last-lambda)))
         (go-tags (mapcar 'genname go-tag-params))
         (go-tag-continuations (mapcar (lambda (name)
                                         `(lambda (&rest args)
                                            (declare (ignore args))
                                            (go ,name)))
                                       go-tags))
         (exit-tag (gensym "exit")))
    `(tagbody ,@(mapcan (lambda (closure next-tag)
                          (let* ((params (rest (closure-required-params* closure)))
                                 (param-symbols (mapcar 'genname params))
                                 (new-env (nconc (mapcar (lambda (p ps tag)
                                                           (list p ps :go tag))
                                                         params
                                                         param-symbols
                                                         go-tags) env)))
                            (list `(let ,(mapcar 'list param-symbols go-tag-continuations)
                                     ,(emit-application closure new-env))
                                  next-tag)))
                        (rest (closure-arguments closure))
                        go-tags)
        ,(let* ((params (closure-required-params* last-lambda))
                (param-symbols (mapcar 'genname params))
                (new-env (nconc (mapcar (lambda (p ps tag)
                                          (list p ps :go tag))
                                        params
                                        param-symbols
                                        (list* exit-tag go-tags))
                                env)))
               `(let ,(mapcar 'list param-symbols (list* `(lambda (&optional arg)
                                                            (declare (ignore arg))
                                                            (go ,exit-tag))
                                                         go-tag-continuations))
                  ,(emit-application last-lambda new-env)))
        ,exit-tag)))

(defun emit-continuation-invocation (cont args env)
  (let* ((info (assoc cont env))
         (possible-values (remove-if (lambda (x) (typep x 'lexical))
                                     (gethash cont *variable-value-map*)))
         (global-info (assoc (first possible-values) *emited-tagbody-things*)))
    (cond ((and (typep cont 'lexical)
                info
                (eql (third info) :go))
           `(progn ,@args (go ,(fourth info))))
          ((and (typep cont 'lexical)
                info
                (eql (third info) :return-from))
           `(return-from ,(fourth info) (prog1 ,(first args) ,@(rest args))))
          ((and (typep cont 'lexical)
                global-info
                (eql (length possible-values) 1))
           `(progn ,@args (go ,(second global-info))))
          (t `(funcall ,(emit-continuation cont env) ,@args)))))

(defun should-emit-bound-closure-in-tagbody (val bound-in)
  (and (typep val 'closure)
       (continuationp val)
       (every (lambda (param)
                (or (zerop (length (gethash param *use-map*)))
                    (let ((possible-values (remove-if (lambda (x) (typep x 'lexical))
                                                      (gethash param *variable-value-map*))))
                      (and (eql (length possible-values) 1)
                           (or (eql (first possible-values) val)
                               (and (typep (first possible-values) 'closure)
                                    (continuationp (first possible-values)))
                               (member (first possible-values) (closure-arguments bound-in)))))))
              (closure-required-params* val))
       (null (closure-optional-params val))
       (not (closure-rest-param val))
       (not (closure-keyword-params-enabled val))
       (every (lambda (var)
                (let ((contours (gethash var *contour-map*)))
                  (and (eql (length contours) 1)
                       (eql (first contours) *current-closure*))))
              (gethash val *value-variable-map*))))

(defun emit-application (closure env)
  (etypecase (closure-function closure)
    (closure
     (cond ((continuationp (closure-function closure))
            (check-required-params-only (closure-function closure))
            ;; This isn't particularly flexible. Would be nice to be able
            ;; to mix & match closures & values.
            (cond ((and (closure-arguments closure)
                        (every (lambda (arg)
                                 (should-emit-bound-closure-in-tagbody arg closure))
                               (closure-arguments closure)))
                   ;; All arguments are closures, generate a tagbody here.
                   (let* ((go-tags (mapcar 'genname (closure-required-params* (closure-function closure))))
                          ;; (code param-symbol param-object)
                          (local-tags (mapcar 'list
                                              (closure-arguments closure)
                                              go-tags
                                              (closure-required-params* (closure-function closure)))))
                     (setf *emited-tagbody-things* (append local-tags *emited-tagbody-things*))
                   `(tagbody
                       (let ,(mapcar (lambda (tag) (list (second tag) `(lambda () (go ,(second tag)))))
                                     local-tags)
                         ,(emit-application (closure-function closure)
                                            (nconc (mapcar (lambda (param param-sym)
                                                             (list param param-sym :go param-sym))
                                                           (closure-required-params* (closure-function closure))
                                                           go-tags)
                                                   env)))
                       ,@(mapcan (lambda (tag)
                                   (list (second tag)
                                         (emit-application (first tag) env)))
                                 local-tags))))
                  (t ;; Simple binding.
                   (let* ((param-symbols (mapcar 'genname (closure-required-params* (closure-function closure)))))
                     (mapc (lambda (param sym)
                             (setf (get sym 'use-count) (length (gethash param *use-map*))))
                           (closure-required-params* (closure-function closure))
                           param-symbols)
                     `(let ,(mapcar (lambda (p val)
                                      (list p (emit-arg val env)))
                                    param-symbols
                                    (closure-arguments closure))
                        ,(emit-application (closure-function closure)
                                           (nconc (mapcar (lambda (p-obj p)
                                                            (list p-obj p))
                                                          (closure-required-params* (closure-function closure))
                                                          param-symbols)
                                                  env)))))))
           (t (error "TODO: non-binding closure application."))))
    (lexical
     ;; First argument is the continuation.
     ;; Function might be a go-tag. If so, generate a GO and elide the continuation.
     (let ((info (assoc (closure-function closure) env)))
       (cond ((eql (third info) :go)
              `(go ,(fourth info)))
             (t `(funcall ,(emit-continuation (first (closure-arguments closure)) env)
                          (funcall ,(second (assoc (closure-function closure) env))
                                   ,@(mapcar (lambda (arg) (emit-arg arg env))
                                             (rest (closure-arguments closure)))))))))
    (constant
     (case (constant-value (closure-function closure))
       ((%if)
        ;; Special, because it has two continuations.
        `(if ,(emit-arg (third (closure-arguments closure)) env)
             ,(emit-continuation-invocation (first (closure-arguments closure)) '() env)
             ,(emit-continuation-invocation (second (closure-arguments closure)) '() env)))
       ((%select)
        (destructuring-bind (cont then else test) (closure-arguments closure)
          (emit-continuation-invocation cont
                                        (list `(if ,(emit-arg test env)
                                                   ,(emit-arg then env)
                                                   ,(emit-arg else env)))
                                        env)))
       ((%block)
        ;; Block lifetimes are handled by CL's block form, no need for anything special.
        (emit-continuation-invocation (first (closure-arguments closure))
                                      (list `(funcall ,(emit-arg (second (closure-arguments closure)) env)))
                                      env))
       ((%tagbody)
        (emit-tagbody closure env))
       ((%invoke-continuation)
        ;; Not very special, could be implemented as
        ;; (defun %invoke-continuation (&rest args) (values-list args))
        ;; Just elide it.
        (emit-continuation-invocation (first (closure-arguments closure))
                                      (mapcar (lambda (arg) (emit-arg arg env))
                                              (rest (closure-arguments closure)))
                                      env))
       ((%make-cell)
        (destructuring-bind (cont value) (closure-arguments closure)
          (assert (and (typep cont 'closure)
                       (continuationp cont)
                       (= (length (closure-required-params* cont)) 1)))
          (check-required-params-only cont)
          (let ((sym (genname (first (closure-required-params* cont)))))
            `(let ((,sym ,(emit-arg value env)))
               ,(emit-application cont
                                  (list* (list (first (closure-required-params* cont)) sym)
                                         env))))))
       ((%contents)
        (destructuring-bind (cont cell) (closure-arguments closure)
          (check-type cell lexical)
          (emit-continuation-invocation cont
                                        (list (second (assoc cell env)))
                                        env)))
       ((%set-contents)
        (destructuring-bind (cont cell new-value) (closure-arguments closure)
          (check-type cell lexical)
          (emit-continuation-invocation cont
                                        (list `(setq ,(second (assoc cell env))
                                                     ,(emit-arg new-value env)))
                                        env)))
       (t ;; First argument is the continuation.
        (emit-continuation-invocation (first (closure-arguments closure))
                                      (list `(funcall ',(constant-value (closure-function closure))
                                                      ,@(mapcar (lambda (arg) (emit-arg arg env))
                                                                (rest (closure-arguments closure)))))
                                      env))))))

(defun emit-closure (closure env)
  (check-type closure (and closure (not (satisfies continuationp))))
  (let* ((required-symbols (mapcar 'genname (closure-required-params* closure)))
         (optional-symbols (mapcar 'genname (mapcar 'first (closure-optional-params closure))))
         (optional-suppliedp-symbols (mapcar 'genname (mapcar 'second (closure-optional-params closure))))
         (rest-symbol (when (closure-rest-param closure) (genname (closure-rest-param closure))))
         (key-symbols (mapcar 'genname (mapcar 'second (closure-keyword-params closure))))
         (key-suppliedp-symbols (mapcar 'genname (mapcar 'third (closure-keyword-params closure))))
         (block-name (gensym))
         (env (nconc (mapcar 'list (rest (closure-required-params* closure)) (rest required-symbols))
                     (mapcar 'list (mapcar 'first (closure-optional-params closure)) optional-symbols)
                     (mapcar 'list (mapcar 'second (closure-optional-params closure)) optional-suppliedp-symbols)
                     (when rest-symbol
                       (list (list (closure-rest-param closure) rest-symbol)))
                     (mapcar 'list (mapcar 'second (closure-keyword-params closure)) key-symbols)
                     (mapcar 'list (mapcar 'third (closure-keyword-params closure)) key-suppliedp-symbols)
                     (list (list (first (closure-required-params* closure))
                                 (first required-symbols)
                                 :return-from
                                 block-name))
                     env))
         ; this in closure only!
         (*emited-tagbody-things* '())
         (*current-closure* closure))
    `(lambda (,@(rest required-symbols)
              ,@(when optional-symbols
                  (list* '&optional
                         (mapcar (lambda (sym suppliedp) (list sym nil suppliedp))
                                 optional-symbols
                                 optional-suppliedp-symbols)))
              ,@(when rest-symbol
                  (list '&rest rest-symbol))
              ,@(when (closure-keyword-params-enabled closure)
                      `(&key
                        ,@(mapcar (lambda (keyword sym suppliedp)
                                    `((,keyword ,sym) nil ,suppliedp))
                                  (mapcar 'first (closure-keyword-params closure))
                                  key-symbols
                                  key-suppliedp-symbols)
                        ,@(when (closure-allow-other-keywords closure)
                                (list '&allow-other-keys)))))
       (block ,block-name
         (let ((,(first required-symbols) (lambda (&optional arg) (return-from ,block-name arg))))
           ,(emit-application closure env))))))

(defvar *prettify-emitted-code* t)

(defun emit-cl (closure)
  (let* ((*use-map* (use-map closure))
         (*contour-map* (dynamic-contour-analysis closure))
         (*variable-value-map* (variable-value-tracking closure))
         (*value-variable-map* (inverse-variable-value-tracking *variable-value-map*))
         (code (emit-closure closure '())))
    (if *prettify-emitted-code*
        (prettify-emitted-code code)
        code)))

(defun forward-expression (code variable form)
  (when (and (consp code)
             (symbolp (first code)))
    (case (first code)
      ((progn)
       (forward-expression (second code) variable form))
      ((if)
       (cond ((eql (second code) variable)
              (setf (second code) form)
              t)
             (t (forward-expression (second code) variable form))))
      ((setq)
       (cond ((eql (third code) variable)
              (setf (third code) form)
              t)
             (t (forward-expression (third code) variable form))))
      ((return-from)
       (cond ((eql (third code) variable)
              (setf (third code) form)
              t)
             (t (forward-expression (third code) variable form))))
      ((block catch eval-when flet function
        go labels let let* load-time-value locally
        macrolet multiple-value-call multiple-value-prog1
        progv quote symbol-macrolet tagbody the throw
        unwind-protect)
       nil)
      (t (do ((arg (cdr code) (cdr arg)))
             ((null arg) nil)
           (cond ((symbolp (car arg))
                  (when (eql (car arg) variable)
                    (setf (car arg) form)
                    (return t)))
                 ((consp (car arg))
                  (unless (member (first (car arg)) '(function lambda quote))
                    (return (forward-expression (car arg) variable form))))))))))

(defun prettify-emitted-code (code)
  (typecase code
    (cons
     (case (first code)
       ((funcall)
        (cond ((and (consp (second code))
                    (eql (first (second code)) 'lambda))
               ;; (funcall (lambda ...) ...) => ((lambda ...) ...)
               (prettify-emitted-code (rest code)))
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
        ;; LET variables with no uses are pushed into the body.
        (when (and (= (length (second code)) 1)
                   (eql (get (first (first (second code))) 'use-count) 0))
          (setf code `(let ()
                        ,(second (first (second code)))
                        ,@(cddr code))))
        ;; Collapse nested LETs.
        (let ((inner (prettify-emitted-code `(progn ,@(cddr code)))))
          ;; Attempt to push used-once variables into the next expression.
          (when (and (= (length (second code)) 1)
                     (eql (get (first (first (second code))) 'use-count) 1))
            (when (forward-expression inner
                                      (first (first (second code)))
                                      (second (first (second code))))
              ;; Expression was forwarded, drop it.
              (return-from prettify-emitted-code inner)))
          (let ((bindings (mapcar (lambda (b)
                                    (list (first b) (prettify-emitted-code (second b))))
                                  (second code))))
            (when (and (consp inner)
                       (member (first inner) '(let let*)))
              (setf bindings (append bindings (second inner))
                    inner `(progn ,@(cddr inner))))
            ;; Empty LETs will become PROGNs.
            (if (endp bindings)
                (prettify-emitted-code inner)
                `(let* ,bindings ,(prettify-emitted-code inner))))))
       ((progn)
        (if (null (cddr code))
            (prettify-emitted-code (second code))
            `(progn ,@(mapcar 'prettify-emitted-code (cdr code)))))
       (t (cond ((and (consp (first code))
                      (eql (first (first code)) 'lambda)
                      (eql (length (second (first code)))
                           (length (rest code)))
                      (notany (lambda (x) (member x lambda-list-keywords))
                              (second (first code))))
                 ;; Transform LAMBDA into LET.
                 ;; FIXME: Don't do this in the presense of non-required parameters.
                 (prettify-emitted-code `(let ,(mapcar 'list (second (first code)) (rest code))
                                           (progn ,@(cddr (first code))))))
                (t (mapcar 'prettify-emitted-code code))))))
    (t code)))
