;;;; Pattern-based form rewriter.

(in-package #:sys.newc)

(defun match-form (form original bindings use-map)
  (labels ((frob (form template)
             (etypecase template
               (symbol
                (let ((info (assoc template bindings)))
                  (cond (info
                         (eql (cdr info) form))
                        (t (push (cons template form) bindings)
                           t))))
               (cons
                (cond ((member (first template) '(lambda clambda))
                       ;; Scanning closure.
                       (when (typep form 'closure)
                         (ecase (first template)
                           ((lambda)
                            (when (getf (plist form) 'continuation)
                              (return-from frob nil)))
                           ((clambda)
                            (unless (getf (plist form) 'continuation)
                              (return-from frob nil))))
                         ;; Match formals list
                         (cond ((symbolp (second template))
                                (let ((info (assoc (second template) bindings)))
                                  (cond (info
                                         (when (not (eql (cdr info) (closure-required-params form)))
                                           (return-from frob nil)))
                                        (t (push (cons (second template)
                                                       (closure-required-params form))
                                                 bindings)))))
                               (t (when (not (eql (length (closure-required-params form))
                                                  (length (second template))))
                                    (return-from frob nil))
                                  (mapc (lambda (p tmplt)
                                          (when (symbolp tmplt) (setf tmplt (list tmplt)))
                                          (destructuring-bind (name &key uses) tmplt
                                            (when (not (or (not uses)
                                                           (eql (length (gethash p use-map)) uses)))
                                              (return-from frob nil))
                                            (let ((info (assoc name bindings)))
                                              (cond (info
                                                     (when (not (eql (cdr info) p))
                                                       (return-from frob nil)))
                                                    (t (push (cons name p) bindings))))))
                                        (closure-required-params form)
                                        (second template))))
                         ;; Match body.
                         (cond ((symbolp (third template))
                                (let ((info (assoc (third template) bindings)))
                                  (cond (info
                                         (when (not (eql (cdr info) (closure-body form)))
                                           (return-from frob nil)))
                                        (t (push (cons (third template)
                                                       (closure-body form))
                                                 bindings)))))
                               (t (when (not (frob form (third template)))
                                    (return-from frob nil))))
                         t))
                      ((eql (first template) 'quote)
                       (and (typep form 'constant)
                            (eql (second template) (constant-value form))))
                      (t ;; Scanning application.
                       (when (and (typep form 'closure)
                                  (eql (length (closure-body form))
                                       (length template)))
                         (every #'frob (closure-body form) template))))))))
    (values (frob form original)
            bindings)))

(defun perform-rewrite (replacement bindings subrewriter)
  "Fill REPLACEMENT with BINDINGS."
  (labels ((sym (name)
             (let ((x (assoc name bindings)))
               (cond (x (cdr x))
                     (t (let ((var (make-instance 'lexical :name (gensym (string name)))))
                          (push (cons name var) bindings)
                          var)))))
           (frob (r)
             (etypecase r
               (cons
                (cond ((member (first r) '(clambda lambda))
                       (let ((result (make-instance 'closure
                                                    :required-params (if (symbolp (second r))
                                                                         (sym (second r))
                                                                         (mapcar #'sym (second r)))
                                                    :body (if (symbolp (third r))
                                                              (sym (third r))
                                                              (frob (third r)))
                                                    :plist (list 'continuation (eql (first r) 'clambda)))))
                         (if (symbolp (third r))
                             (funcall subrewriter result)
                             result)))
                      ((eql (first r) 'quote)
                       (make-instance 'constant :value (second r)))
                      (t (mapcar #'frob r))))
               (symbol (funcall subrewriter (sym r))))))
    (values (frob replacement)
            bindings)))

(defun eval-rewrite-result (result bindings)
  (typecase result
      (symbol
       (let ((info (assoc result bindings)))
         (when (null info)
           (error "Unknown binding ~S." result))
         (cdr info)))
      (cons (apply (first result)
                   (mapcar (lambda (x) (eval-rewrite-result x bindings))
                           (rest result))))
      (t result)))

(defun rewrite-form (form inputs original replacement result)
  (let ((use-map (if (typep form 'closure)
                     (use-map form)
                     (make-hash-table)))
        (accumulated-results '()))
    (labels ((frob (form)
               (multiple-value-bind (matchp bindings)
                   (match-form form original inputs use-map)
                 (cond (matchp
                        ;; Form matches, rewrite it.
                        (multiple-value-bind (new-form new-bindings)
                            (perform-rewrite replacement bindings #'frob)
                          (when result
                            (push (eval-rewrite-result result new-bindings) accumulated-results))
                          (if (and (listp replacement)
                                   (not (member (first replacement) '(lambda clambda))))
                              (make-instance 'closure
                                             :name (closure-name form)
                                             :required-params (closure-required-params form)
                                             :body new-form
                                             :plist (plist form))
                              new-form)))
                       (t ;; No match, recurse.
                        (etypecase form
                          (closure
                           (make-instance 'closure
                                          :name (closure-name form)
                                          :required-params (closure-required-params form)
                                          :body (mapcar #'frob (closure-body form))
                                          :plist (plist form)))
                          ((or lexical constant)
                           form)))))))
      (values (frob form)
              accumulated-results))))

(defmacro define-rewrite-rule (name inputs original replacement &optional result)
  (let ((form (gensym "FORM")))
    `(defun ,name (,form ,@inputs)
       (rewrite-form ,form (pairlis ',inputs (list ,@inputs)) ',original ',replacement ',result))))
