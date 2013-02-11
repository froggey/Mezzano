(in-package #:sys.newc)

(define-rewrite-rule hoist-if-branches ()
  ((clambda (if-closure) body)
   (clambda ((test :uses 1)) ('%if then else test)))
  ((clambda (then-var else-var)
     ((clambda (if-closure) body)
      (clambda (test)
        ('%if then-var else-var test))))
   then
   else)
  (list if-closure then-var else-var))

(define-rewrite-rule tricky-if ()
  (l1
   (lambda (test)
     ((clambda (c1)
        ('%if then else test))
      l2)))
  ((clambda (c1)
     (l1
      (clambda (test)
        ('%if then else test))))
   l2))

(define-rewrite-rule replace-if-closure (closure-var then-var else-var)
  closure-var
  (clambda (test)
    ('%if then-var else-var test)))

;; Replace %IF applications with calls to their appropriate branch when the
;; test is known to be true or false.
(defgeneric simple-optimize-if (form &optional known-truths))

(defun simple-optimize-if-application (form &optional known-truths)
  (cond ((and (typep (first form) 'constant)
              (eql (constant-value (first form)) '%IF)
              (= (length (rest form)) 3)
              (eql (first (rest form)) (second (rest form))))
         ;; Both branches jump to the same place.
         (list (first (rest form))))
        ((and (typep (first form) 'constant)
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
                                                       known-truths)
                 :plist (plist form)))

(defmethod simple-optimize-if ((form lexical) &optional known-truths)
  (let ((info (assoc form known-truths)))
    ;; Only substitute NIL, can't know the actual value when true.
    (if (and info (eql (cdr info) 'nil))
        (make-instance 'constant :value (cdr info))
        form)))

(defmethod simple-optimize-if ((form constant) &optional known-truths)
  (declare (ignore known-truths))
  form)
