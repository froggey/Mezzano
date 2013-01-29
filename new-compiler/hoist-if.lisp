(in-package #:sys.newc)

(defun candidate-ifp (closure use-map)
  (and
   ;; Must be (LAMBDA (test) ('%IF then else test))
   (typep closure 'closure)
   (typep (closure-function closure) 'constant)
   (eql (constant-value (closure-function closure)) '%if)
   (= (length (closure-arguments closure)) 3)
   (= (length (closure-required-params closure)) 1)
   (eql (first (closure-required-params closure))
        (third (closure-arguments closure)))
   ;; The test-arg must not be captured.
   (= (length (gethash (third (closure-arguments closure)) use-map)) 1)
   ;; And must be the argument of a one-argument closure.
   (let* ((uses (gethash closure use-map))
          (outer (first uses)))
     (and (= (length uses) 1)
          (not (eql (closure-function outer) closure))
          (typep (closure-function outer) 'closure)
          (= (length (closure-required-params (closure-function outer))) 1)
          (= (length (closure-arguments outer)) 1)
          (eql (first (closure-arguments outer)) closure)))))

(defun hoist-candidate-if (form candidate use-map)
  ;; (LAMBDA (...) ((LAMBDA (K) ...) #candidate=(LAMBDA (X) (%IF foo bar X)))) =>
  ;; (LAMBDA (...)
  ;;   ((LAMBDA (true false)
  ;;      ((LAMBDA (K) ...) (LAMBDA (X) (%IF true false X))))
  ;;    foo bar))
  (let* ((replace-this (first (gethash candidate use-map)))
         (args (closure-arguments candidate))
         (then-arg (first args))
         (else-arg (second args))
         (then-var (make-instance 'lexical :name (gensym "if-then")))
         (else-var (make-instance 'lexical :name (gensym "if-else")))
         (test-var (make-instance 'lexical :name (gensym "if-test"))))
    (values (substitute-form form replace-this
                             (! `(lambda ,(closure-required-params replace-this)
                                   ((lambda (,then-var ,else-var)
                                      (,(closure-function replace-this)
                                        (lambda (,test-var)
                                          (%if ,then-var ,else-var ,test-var))))
                                    ,then-arg
                                    ,else-arg))))
            then-var
            else-var
            (first (closure-required-params (closure-function replace-this))))))

;; (LAMBDA (...) (... closure-var ...)) =>
;; (LAMBDA (...) (... (LAMBDA (test) (%IF then-var else-var test)) ...))
(defgeneric replace-if-closure (form closure-var then-var else-var))

(defun replace-if-closure-application (form closure-var then-var else-var)
  (mapcar (lambda (f)
            (cond ((eql f closure-var)
                   (let ((test (make-instance 'lexical :name (gensym "if-test"))))
                     (! `(lambda (,test) (%if ,then-var ,else-var ,test)))))
                  (t (replace-if-closure f closure-var then-var else-var))))
          form))

(defmethod replace-if-closure ((form closure) closure-var then-var else-var)
  (make-instance 'closure
                 :name (closure-name form)
                 :required-params (closure-required-params form)
                 :body (replace-if-closure-application (closure-body form)
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

(defun simple-optimize-if-application (form &optional known-truths)
  (cond ((and (typep (first form) 'constant)
              (eql (constant-value (first form)) '%IF)
              (= (length (rest form)) 3)
              (or (typep (third (rest form)) 'constant)
                  (typep (third (rest form)) 'closure)
                  (and (typep (third (rest form)) 'lexical)
                       (assoc (third (rest form)) known-truths))))
         ;; Known test result.
         (cond ((or (typep (third (rest form)) 'closure)
                    (and (typep (third (rest form)) 'constant)
                         (not (null (constant-value (third (rest form))))))
                    (and (typep (third (rest form)) 'lexical)
                         (cdr (assoc (third (rest form)) known-truths))))
                ;; Result is true.
                (list (simple-optimize-if (first (rest form))
                                          known-truths)))
               (t ;; Result is false.
                (list (simple-optimize-if (second (rest form))
                                          known-truths)))))
        ((and (typep (first form) 'constant)
              (eql (constant-value (first form)) '%IF)
              (= (length (rest form)) 3)
              (typep (third (rest form)) 'lexical))
         ;; Unknown test result, but inner IFs can assume true/false based on the path.
         (list (make-instance 'constant :value '%if)
               (simple-optimize-if (first (rest form))
                                   (list* (cons (third (rest form)) t) known-truths))
               (simple-optimize-if (second (rest form))
                                   (list* (cons (third (rest form)) nil) known-truths))
               (third (rest form))))
        (t ;; Dunno.
         (mapcar (lambda (f) (simple-optimize-if f known-truths)) form))))

(defmethod simple-optimize-if ((form closure) &optional known-truths)
  (make-instance 'closure
                 :name (closure-name form)
                 :required-params (closure-required-params form)
                 :body (simple-optimize-if-application (closure-body form)
                                                       known-truths)))

(defmethod simple-optimize-if ((form lexical) &optional known-truths)
  (let ((info (assoc form known-truths)))
    (if info
        (make-instance 'constant :value (cdr info))
        form)))

(defmethod simple-optimize-if ((form constant) &optional known-truths)
  (declare (ignore known-truths))
  form)
