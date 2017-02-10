;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Lowering pass implementing &KEY parameters.

(in-package :sys.c)

(defun variable-name (var)
  (etypecase var
    (special-variable
     (name var))
    (lexical-variable
     (lexical-variable-name var))))

;; Rewrites:
;; (lambda (&key (x x-init) (y nil y-p) z) ...)
;; into something like:
;; (lambda (&rest args)
;;   (let ((x-val nil)
;;         (x-suppliedp nil)
;;         (y-val nil)
;;         (y-suppliedp nil)
;;         (z-val nil)
;;         (z-suppliedp nil)
;;         (allow-other-keys nil)
;;         (itr args))
;;     (tagbody
;;        (go TEST)
;;      HEAD
;;        (when (eql (car itr1) :allow-other-keys)
;;          (setf allow-other-keys (car (cdr itr)))
;;          (go OUT))
;;        (setq itr (cddr itr))
;;      TEST
;;        (when itr
;;          (go HEAD))
;;      OUT)
;;     (setf itr args)
;;     (tagbody
;;        (go TEST)
;;      HEAD
;;        (if (null (cdr itr))
;;            (error "Odd number of keys"))
;;        (let ((kw (car itr)))
;;          (cond ((eql kw :x)
;;                 (unless x-suppliedp
;;                   (setq x-suppliedp t
;;                         x-val (car (cdr itr)))))
;;                ((eql kw :y)
;;                 (unless y-suppliedp
;;                   (setq y-suppliedp t
;;                         y-val (car (cdr itr)))))
;;                ((eql kw :z)
;;                 (unless z-suppliedp
;;                   (setq z-suppliedp t
;;                         z-val (car (cdr itr)))))
;;                ((eql kw :allow-other-keys))
;;                (t
;;                 (unless allow-other-keys
;;                   (error "Unknown keyword ~S" kw)))))
;;        (setq itr (cddr itr))
;;      TEST
;;        (when itr
;;          (go HEAD)))
;;     (let* ((x (if x-suppliedp
;;                   x-val
;;                   x-init))
;;            (y (if y-suppliedp
;;                   y-val
;;                   nil))
;;            (y-p y-suppliedp)
;;            (z (if z-suppliedp
;;                   z-val
;;                   nil)))
;;       ...)))
(defun lower-key-arguments* (form body rest keys allow-other-keys)
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
         (aok (make-instance 'lexical-variable
                             :inherit form
                             :name (gensym "ALLOW-OTHER-KEYS")
                             :definition-point *current-lambda*))
         (aok-itr (make-instance 'lexical-variable
                                 :inherit form
                                 :name (gensym)
                                 :definition-point *current-lambda*))
         (itr (make-instance 'lexical-variable
                             :inherit form
                             :name (gensym)
                             :definition-point *current-lambda*))
         (current-keyword (make-instance 'lexical-variable
                                         :inherit form
                                         :name (gensym)
                                         :definition-point *current-lambda*)))
    (labels ((create-key-test-list (key-args values suppliedp)
               (cond (key-args
                      `(if (call eql ,current-keyword (quote ,(caar (first key-args))))
                           (if ,(first suppliedp)
                               (quote nil)
                               (progn
                                 (setq ,(first suppliedp) 't)
                                 (setq ,(first values) (call cadr ,itr))))
                           ,(create-key-test-list (rest key-args) (rest values) (rest suppliedp))))
                     (allow-other-keys
                      '(quote nil))
                     (t
                      `(if ,aok
                           (quote nil)
                           (if (call eql ,current-keyword (quote :allow-other-keys))
                               (quote nil)
                               (call error
                                     'sys.int::simple-program-error
                                     ':format-control '"Unknown &KEY argument ~S. Expected one of ~S."
                                     ':format-arguments (call list ,current-keyword (quote ,(mapcar 'caar keys)))))))))
             (create-key-let-body (key-args values suppliedp)
               (cond (key-args
                      `(let ((,(second (first (first key-args)))
                              (if ,(first suppliedp)
                                  ,(first values)
                                  ,(second (first key-args)))))
                         ,(if (third (first key-args))
                              `(let ((,(third (first key-args)) ,(first suppliedp)))
                                 ,(create-key-let-body (rest key-args) (rest values) (rest suppliedp)))
                              (create-key-let-body (rest key-args) (rest values) (rest suppliedp)))))
                     (t body))))
      (ast `(let (,@(mapcar (lambda (x) (list x '(quote nil))) values)
                  ,@(mapcar (lambda (x) (list x '(quote nil))) suppliedp)
                    (,aok (quote ,(if allow-other-keys t nil)))
                    (,itr ,(etypecase rest
                             (special-variable
                              `(call symbol-value (quote ,rest)))
                             (lexical-variable
                              rest))))
              (progn
                ,@(when (not allow-other-keys)
                    `((tagbody aok-tb
                         (aok-entry
                          (go aok-test aok-tb))
                         (aok-head
                          (progn
                            (if (call eql (call car ,itr) (quote :allow-other-keys))
                                (progn
                                  (setq ,aok (call car (call cdr ,itr)))
                                  (go aok-out aok-tb))
                                (quote nil))
                            (setq ,itr (call cddr ,itr))
                            (go aok-test aok-tb)))
                         (aok-test
                          (if ,itr
                              (go aok-head aok-tb)
                              (go aok-out aok-tb)))
                         (aok-out
                          (quote nil)))
                      (setq ,itr ,(etypecase rest
                                    (special-variable
                                     `(call symbol-value (quote ,rest)))
                                    (lexical-variable
                                     rest)))))
                (tagbody tb
                   (entry
                    (go test-tag tb))
                   (head-tag
                    (progn
                      (if (call null (call cdr ,itr))
                          (call error
                                'sys.int::simple-program-error
                                ':format-control '"Odd number of &KEY arguments.")
                          (quote nil))
                      (let ((,current-keyword (call car ,itr)))
                        ,(create-key-test-list keys values suppliedp))
                      (setq ,itr (call cddr ,itr))
                      (go test-tag tb)))
                   (test-tag
                    (if ,itr
                        (go head-tag tb)
                        (quote nil))))
                ,(create-key-let-body keys values suppliedp)))
           form))))

(defun lower-keyword-arguments (form)
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
                                  (lambda-information-allow-other-keys form)))
      ;; Remove the old keyword arguments.
      (setf (lambda-information-enable-keys form) nil
            (lambda-information-key-args form) '()
            (lambda-information-allow-other-keys form) nil)
      (incf *change-count*))
    (dolist (arg (lambda-information-optional-args form))
      (lower-keyword-arguments-1 (second arg)))
    (lower-keyword-arguments-1 (lambda-information-body form))))
