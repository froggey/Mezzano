(in-package #:sys.newc)

(defun candidate-ifp (form flow-map)
  (let* ((outer-1 (gethash form flow-map))
         (outer-2 (gethash outer-1 flow-map)))
    (and
     ;; Must be (%IF then else test).
     (typep form 'application)
     (eql (application-function form) '%if)
     (= (length (application-arguments form)) 3)
     ;; Enclosed in a one-argument closure.
     (typep outer-1 'closure)
     (= (length (closure-required-args outer-1)) 1)
     ;; And the argument of the closure must be the test.
     (eql (first (closure-required-args outer-1))
          (third (application-arguments form)))
     ;; The closure must be an argument of a one-argument closure application.
     ;; one-argument rule is because I'm lazy.
     (typep outer-2 'application)
     (not (eql (application-function outer-2) outer-1))
     (typep (application-function outer-2) 'closure)
     (= (length (application-arguments outer-2)) 1))))

(defun hoist-candidate-if (form candidate flow-map)
  ;; ((LAMBDA (K) ...) (LAMBDA (X) (%IF foo bar X))) =>
  ;; ((LAMBDA (true false)
  ;;    ((LAMBDA (K) ...) (LAMBDA (X) (%IF true false X))))
  ;;  foo bar)
  (let* ((replace-this (gethash (gethash candidate flow-map) flow-map))
         (args (application-arguments candidate))
         (then-arg (first args))
         (else-arg (second args))
         (then-var (make-instance 'lexical :name (gensym "if-then")))
         (else-var (make-instance 'lexical :name (gensym "if-else")))
         (test-var (make-instance 'lexical :name (gensym "if-test"))))
    (values (substitute-form form replace-this
                             (! `((lambda (,then-var ,else-var)
                                    (,(application-function replace-this)
                                      (lambda (,test-var)
                                        (%if ,then-var ,else-var ,test-var))))
                                  ,then-arg
                                  ,else-arg)))
            then-var
            else-var
            (first (closure-required-args (application-function replace-this))))))

;; (closure-var x) => (%IF then-var else-var x)
;; (FOO ... closure-var ...) =>
;; (FOO ... (lambda (t) (%IF then-var else-var t)) ...)
(defgeneric replace-if-closure (form closure-var then-var else-var))

(defmethod replace-if-closure ((form application) closure-var then-var else-var)
  (cond ((and (eql (application-function form) closure-var)
              (= (length (application-arguments form)) 1))
         (! `(%IF ,then-var ,else-var ,(first (application-arguments form)))))
        (t (make-instance 'application
                          :function (cond ((symbolp (application-function form))
                                           (application-function form))
                                          (t (replace-if-closure (application-function form)
                                                                 closure-var then-var else-var)))
                          :arguments (mapcar (lambda (f)
                                               (cond ((eql f closure-var)
                                                      (let ((test (make-instance 'lexical :name (gensym "if-test"))))
                                                        (! `(lambda (,test) (%if ,then-var ,else-var ,test)))))
                                                     (t (replace-if-closure f closure-var then-var else-var))))
                                             (application-arguments form))))))

(defmethod replace-if-closure ((form closure) closure-var then-var else-var)
  (make-instance 'closure
                 :name (closure-name form)
                 :required-args (closure-required-args form)
                 :body (replace-if-closure (closure-body form)
                                           closure-var then-var else-var)))

(defmethod replace-if-closure ((form lexical) closure-var then-var else-var)
  (declare (ignore closure-var then-var else-var))
  form)

(defmethod replace-if-closure ((form constant) closure-var then-var else-var)
  (declare (ignore closure-var then-var else-var))
  form)

;; Replace %IF applications with calls to their appropriate branch when the
;; test is known to be true or false.
(defgeneric simple-optimize-if (form &optional known-truths))

(defmethod simple-optimize-if ((form application) &optional known-truths)
  (cond ((and (eql (application-function form) '%IF)
              (= (length (application-arguments form)) 3)
              (or (typep (third (application-arguments form)) 'constant)
                  (typep (third (application-arguments form)) 'closure)
                  (and (typep (third (application-arguments form)) 'lexical)
                       (assoc (third (application-arguments form)) known-truths))))
         ;; Known test result.
         (cond ((or (typep (third (application-arguments form)) 'closure)
                    (and (typep (third (application-arguments form)) 'constant)
                         (not (null (constant-value (third (application-arguments form))))))
                    (and (typep (third (application-arguments form)) 'lexical)
                         (cdr (assoc (third (application-arguments form)) known-truths))))
                ;; Result is true.
                (! `(,(simple-optimize-if (first (application-arguments form))
                                          known-truths))))
               (t ;; Result is false.
                (! `(,(simple-optimize-if (second (application-arguments form))
                                          known-truths))))))
        ((and (eql (application-function form) '%IF)
              (= (length (application-arguments form)) 3)
              (typep (third (application-arguments form)) 'lexical))
         ;; Unknown test result, but inner IFs can assume true/false based on the path.
         (! `(%IF ,(simple-optimize-if (first (application-arguments form))
                                       (list* (cons (third (application-arguments form)) t) known-truths))
                  ,(simple-optimize-if (second (application-arguments form))
                                       (list* (cons (third (application-arguments form)) nil) known-truths))
                  ,(third (application-arguments form)))))
        (t ;; Dunno.
         (make-instance 'application
                        :function (cond ((symbolp (application-function form))
                                         (application-function form))
                                        (t (simple-optimize-if (application-function form)
                                                               known-truths)))
                        :arguments (mapcar (lambda (f)
                                             (simple-optimize-if f known-truths))
                                           (application-arguments form))))))

(defmethod simple-optimize-if ((form closure) &optional known-truths)
  (make-instance 'closure
                 :name (closure-name form)
                 :required-args (closure-required-args form)
                 :body (simple-optimize-if (closure-body form)
                                           known-truths)))

(defmethod simple-optimize-if ((form lexical) &optional known-truths)
  (let ((info (assoc form known-truths)))
    (if info
        (make-instance 'constant :value (cdr info))
        form)))

(defmethod simple-optimize-if ((form constant) &optional known-truths)
  (declare (ignore known-truths))
  form)
