;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Simplifiy the ast by removing empty nodes and unused variables.

(in-package :sys.c)

(defun simplify (lambda)
  (simp-form lambda))

(defgeneric simp-form (form))

(defun simp-form-list (x)
  (do ((i x (cdr i)))
      ((endp i))
    (setf (car i) (simp-form (car i)))))

(defmethod simp-form ((form ast-block))
  (cond
    ;; Unused blocks get reduced to progn.
    ((eql (lexical-variable-use-count (info form)) 0)
     (change-made)
     (simp-form (body form)))
    ;; (block foo <constantish>) => 'nil
    ((typep (body form) '(or ast-quote ast-function lexical-variable lambda-information))
     (change-made)
     (make-instance 'ast-quote :value nil))
    ;; (block foo (return-from foo form)) => (block foo form)
    ((and (typep (body form) 'ast-return-from)
          (eql (info form) (info (body form))))
     (change-made)
     (setf (body form) (value (body form)))
     form)
    (t (setf (body form) (simp-form (body form)))
       form)))

(defmethod simp-form ((form ast-function))
  form)

(defmethod simp-form ((form ast-go))
  ;; HACK: Update the tagbody location part after tagbodies have merged.
  (when (tagbody-information-p (info form))
    (setf (info form) (go-tag-tagbody (target form))))
  form)

;;; Hoist LET/M-V-B/PROGN forms out of IF tests.
;;;  (if (let bindings form1 ... formn) then else)
;;; =>
;;;  (let bindings form1 ... (if formn then else))
;;; Beware when hoisting LET/M-V-B, must not hoist special bindings.
(defun hoist-form-out-of-if (form)
  (cond ((and (typep form 'ast-if)
              (listp (test form))
              (member (first (test form)) '(let multiple-value-bind)))
         (let* ((test-form (test form))
                (len (length test-form)))
           (multiple-value-bind (leading-forms bound-variables)
               (ecase (first test-form)
                 ((let) (values 2 (mapcar #'first (second test-form))))
                 ((multiple-value-bind) (values 3 (second test-form))))
             (when (find-if (lambda (x) (typep x 'special-variable)) bound-variables)
               (return-from hoist-form-out-of-if nil))
             (append (subseq test-form 0 (max leading-forms (1- len)))
                     (if (<= len leading-forms)
                         ;; No body forms, must evaluate to NIL!
                         ;; Fold away the IF.
                         (list (if-else form))
                         (list (make-instance 'ast-if
                                              :test (first (last test-form))
                                              :then (if-then form)
                                              :else (if-else form))))))))
        ((and (typep form 'ast-if)
              (typep (test form) 'ast-progn))
         (let* ((test-form (test form)))
           (if (forms test-form)
               (make-instance 'ast-progn
                              :forms (append (butlast (forms test-form))
                                             (list (make-instance 'ast-if
                                                                  :test (first (last (forms test-form)))
                                                                  :then (if-then form)
                                                                  :else (if-else form)))))
               ;; No body forms, must evaluate to NIL!
               ;; Fold away the IF.
               (if-else form))))))

(defmethod simp-form ((form ast-if))
  (let ((new-form (hoist-form-out-of-if form)))
    (cond (new-form
           (change-made)
           (simp-form new-form))
          ((typep (test form) 'ast-if)
           ;; Rewrite (if (if ...) ...).
           (let* ((test-form (test form))
                  (new-block (make-instance 'block-information
                                            :name (gensym "if-escape")
                                            :definition-point *current-lambda*
                                            :ignore nil
                                            :dynamic-extent nil
                                            :use-count 1))
                  (new-tagbody (make-instance 'tagbody-information
                                              :definition-point *current-lambda*
                                              :go-tags '()
                                              :use-count 1))
                  (then-tag (make-instance 'go-tag
                                           :name (gensym "if-then")
                                           :tagbody new-tagbody
                                           :use-count 1))
                  (else-tag (make-instance 'go-tag
                                           :name (gensym "if-else")
                                           :tagbody new-tagbody
                                           :use-count 1)))
             (push then-tag (tagbody-information-go-tags new-tagbody))
             (push else-tag (tagbody-information-go-tags new-tagbody))
             (make-instance 'ast-block
                            :info new-block
                            :body
                            (make-instance 'ast-tagbody
                                           :info new-tagbody
                                           :statements (list
                                                        (simp-form (make-instance 'ast-if
                                                                                  :test (test test-form)
                                                                                  ;; Special case here to catch (if a a b), generated by OR.
                                                                                  :then (if (eql (test test-form) (if-then test-form))
                                                                                            (make-instance 'ast-go
                                                                                                           :target then-tag
                                                                                                           :info (go-tag-tagbody then-tag))
                                                                                            (make-instance 'ast-if
                                                                                                           :test (if-then test-form)
                                                                                                           :then (make-instance 'ast-go
                                                                                                                                :target then-tag
                                                                                                                                :info (go-tag-tagbody then-tag))
                                                                                                           :else (make-instance 'ast-go
                                                                                                                                :target else-tag
                                                                                                                                :info (go-tag-tagbody else-tag))))
                                                                                  :else (make-instance 'ast-if
                                                                                                       :test (if-else test-form)
                                                                                                       :then (make-instance 'ast-go
                                                                                                                            :target then-tag
                                                                                                                            :info (go-tag-tagbody then-tag))
                                                                                                       :else (make-instance 'ast-go
                                                                                                                            :target else-tag
                                                                                                                            :info (go-tag-tagbody else-tag)))))
                                                        then-tag
                                                        (make-instance 'ast-return-from
                                                                       :target new-block
                                                                       :value (simp-form (if-then form))
                                                                       :info new-block)
                                                        else-tag
                                                        (make-instance 'ast-return-from
                                                                       :target new-block
                                                                       :value (simp-form (if-else form))
                                                                       :info new-block))))))
          ((and (typep (if-then form) 'ast-go)
                (typep (if-else form) 'ast-go)
                (eql (target (if-then form)) (target (if-else form)))
                (eql (info (if-then form)) (info (if-else form))))
           ;; Rewrite (if x (go A-TAG) (go A-TAG)) => (go A-TAG)
           (change-made)
           (simp-form (if-then form)))
          ((and (typep (test form) 'ast-call)
                (eql (name (test form)) 'values)
                (eql (length (arguments (test form))) 1))
           ;; (if (values X) ...) => (if X ...)
           (setf (test form) (first (arguments (test form))))
           (change-made)
           form)
          ((typep (test form) 'ast-quote)
           ;; (if 'not-nil then else) => then
           ;; (if 'nil then else) => else
           (change-made)
           (simp-form (if (not (eql (value (test form)) 'nil))
                          (if-then form)
                          (if-else form))))
          (t
           (setf (test form) (simp-form (test form))
                 (if-then form) (simp-form (if-then form))
                 (if-else form) (simp-form (if-else form)))
           form))))

(defmethod simp-form ((form ast-let))
  ;; Merge nested LETs when possible, do not merge special bindings!
  (do ((nested-form (body form) (body form)))
      ((or (not (typep nested-form 'ast-let))
           (some (lambda (x) (typep x 'special-variable)) (mapcar 'first (bindings form)))
	   (and (bindings nested-form)
                (typep (first (first (bindings nested-form))) 'special-variable))))
    (change-made)
    (if (null (bindings nested-form))
	(setf (body form) (body nested-form))
	(setf (bindings form) (nconc (bindings form) (list (first (bindings nested-form))))
	      (bindings nested-form) (rest (bindings nested-form)))))
  ;; Remove unused values with no side-effects.
  (setf (bindings form) (remove-if (lambda (b)
                                     (let ((var (first b))
                                           (val (second b)))
                                       (and (lexical-variable-p var)
                                            (or (lambda-information-p val)
                                                (typep val 'ast-quote)
                                                (typep val 'ast-function)
                                                (and (lexical-variable-p val)
                                                     (localp val)
                                                     (eql (lexical-variable-write-count val) 0)))
                                            (eql (lexical-variable-use-count var) 0)
                                            (progn (change-made)
                                                   (flush-form val)
                                                   t))))
                                   (bindings form)))
  (dolist (b (bindings form))
    (setf (second b) (simp-form (second b))))
  ;; Remove the LET if there are no values.
  (cond ((bindings form)
         (setf (body form) (simp-form (body form)))
         form)
        (t
         (change-made)
         (simp-form (body form)))))

(defmethod simp-form ((form ast-multiple-value-bind))
  ;; If no variables are used, or there are no variables then
  ;; remove the form.
  (cond ((every (lambda (var)
                  (and (lexical-variable-p var)
                       (zerop (lexical-variable-use-count var))))
                (bindings form))
         (change-made)
         (simp-form (make-instance 'ast-progn
                                   :forms (list (value-form form)
                                                (body form)))))
        ;; M-V-B forms with only one variable can be lowered to LET.
        ((and (bindings form)
              (every (lambda (var)
                       (and (lexical-variable-p var)
                            (zerop (lexical-variable-use-count var))))
                     (rest (bindings form))))
         (change-made)
         (simp-form (make-instance 'ast-let
                                   :bindings (list (list (first (bindings form)) (value-form form)))
                                   :body (body form))))
        ;; Use an inner LET form to bind any special variables.
        ((some (lambda (x) (typep x 'special-variable)) (bindings form))
         (change-made)
         (let* ((specials (remove-if-not (lambda (x) (typep x 'special-variable)) (bindings form)))
                (replacements (loop for s in specials
                                 collect (make-instance 'lexical-variable
                                                        :name s
                                                        :definition-point *current-lambda*
                                                        :use-count 1)))
                ;; Also doubles up as an alist mapping specials to replacements.
                (bindings (mapcar #'list specials replacements)))
           (make-instance 'ast-multiple-value-bind
                          :bindings (mapcar (lambda (var)
                                              (if (typep var 'special-variable)
                                                  (second (assoc var bindings))
                                                  var))
                                            (second form))
                          :value-form (simp-form (value-form form))
                          :body (make-instance 'ast-let
                                               :bindings bindings
                                               :body (simp-form (body form))))))
        (t (setf (value-form form) (simp-form (value-form form))
                 (body form) (simp-form (body form)))
           form)))

(defmethod simp-form ((form ast-multiple-value-call))
  (setf (function-form form) (simp-form (function-form form))
        (value-form form) (simp-form (value-form form)))
  form)

(defmethod simp-form ((form ast-multiple-value-prog1))
  (setf (value-form form) (simp-form (value-form form))
        (body form) (simp-form (body form)))
  (cond ((typep (value-form form) 'ast-progn)
         ;; If the first form is a PROGN, then hoist all but the final value out.
         (change-made)
         (make-instance 'ast-progn
                        :forms (append (butlast (forms (value-form form)))
                                       (list (make-instance 'ast-multiple-value-prog1
                                                            :value-form (car (last (forms (value-form form))))
                                                            :body (body form))))))
        ((typep (value-form form) 'ast-multiple-value-prog1)
         ;; If the first form is a M-V-PROG1, then splice it in.
         (change-made)
         (make-instance 'ast-multiple-value-prog1
                        :value-form (value-form (value-form form))
                        :body (make-instance 'ast-progn
                                             :forms (list (body (value-form form))
                                                          (body form)))))
        ((typep (body form) '(or ast-quote ast-function lexical-variable lambda-information))
         ;; If the body form is mostly constant, then kill this completely.
         (change-made)
         (value-form form))
        (t form)))

(defun simp-progn-body (x)
  ;; Merge nested progns, remove unused quote/function/lambda/variable forms
  ;; and eliminate code after return-from/go.
  (do ((i x (rest i)))
      ((endp i)
       x)
    (let ((form (first i)))
      (cond ((and (typep form 'ast-progn)
                  (forms form))
             ;; Non-empty PROGN.
             (change-made)
             (simp-progn-body (forms form))
             ;; Rewrite ((progn v1 ... vn) . xn) to (v1 .... vn . xn).
             (setf (first i) (first (forms form))
                   (rest i) (nconc (rest (forms form)) (rest i))))
            ((and (typep form 'ast-progn)
                  (not (forms form)))
             ;; Empty progn. Replace with 'NIL.
             (setf (first i) (make-instance 'ast-quote :value 'nil))
             (change-made))
            ((and (rest i) ; not at end.
                  (or (typep form 'ast-quote)
                      (typep form 'ast-function)
                      (lexical-variable-p form)
                      (lambda-information-p form)))
             ;; This is a constantish value not at the end.
             ;; Remove it.
             (change-made)
             (setf (first i) (second i)
                   (rest i) (rest (rest i))))
            ((and (rest i) ; not at end
                  (typep form '(or ast-go ast-return-from)))
             ;; Non-local exit. Remove all following forms.
             (change-made)
             (setf (rest i) nil))
            (t (setf (first i) (simp-form form)))))))

(defmethod simp-form ((form ast-progn))
  (cond ((null (forms form))
	 ;; Flush empty PROGNs.
	 (change-made)
	 (make-instance 'ast-quote :value 'nil))
	((null (rest (forms form)))
	 ;; Reduce single form PROGNs.
	 (change-made)
	 (simp-form (first (forms form))))
	(t (simp-progn-body (forms form))
	   form)))

(defmethod simp-form ((form ast-quote))
  form)

(defmethod simp-form ((form ast-return-from))
  (setf (value form) (simp-form (value form))
        (info form) (simp-form (info form)))
  form)

(defmethod simp-form ((form ast-setq))
  (setf (value form) (simp-form (value form)))
  form)

(defmethod simp-form ((form ast-tagbody))
  (labels ((flatten (x)
	     (cond ((typep x 'ast-progn)
		    (change-made)
		    (apply #'nconc (mapcar #'flatten (forms x))))
		   ((and (consp x)
			 (eq (car x) 'tagbody))
		    ;; Merge directly nested TAGBODY forms, dropping unused go tags.
		    (change-made)
		    (setf (tagbody-information-go-tags (info form))
			  (nconc (tagbody-information-go-tags (info form))
				 (delete-if (lambda (x) (eql (go-tag-use-count x) 0))
					    (tagbody-information-go-tags (second x)))))
                    (dolist (tag (tagbody-information-go-tags (second x)))
                      (setf (go-tag-tagbody tag) (info form)))
		    (apply #'nconc (mapcar (lambda (x)
					     (if (go-tag-p x)
						 (unless (eql (go-tag-use-count x) 0)
						   (list x))
						 (flatten x)))
					   (cddr x))))
		   (t (cons (simp-form x) nil)))))
    (setf (tagbody-information-go-tags (info form))
	  (delete-if (lambda (x) (eql (go-tag-use-count x) 0))
		     (tagbody-information-go-tags (info form))))
    (do* ((i (statements form) (cdr i))
	  (result (cons nil nil))
	  (tail result))
	 ((endp i)
          (setf (statements form) (cdr result)))
      (let ((x (car i)))
	(cond ((go-tag-p x)
               (cond ((eql (go-tag-use-count x) 0)
                      ;; Drop unused go tags.
                      (change-made))
                     (t (setf (cdr tail) (cons x nil)
                              tail (cdr tail)))))
              (t ;; Flatten the body as much as possible.
               (setf (cdr tail) (flatten x)
                     tail (last tail))))))
    ;; Kill code after GO statements and try to eliminate no-op GOs.
    (do* ((i (statements form) (cdr i))
          (prev nil)
          (last-was-go nil))
         ((endp i))
      (let ((stmt (car i)))
        (cond ((go-tag-p stmt)
               (setf last-was-go nil))
              (last-was-go
               (change-made)
               (if prev
                   (setf (cdr prev) (cdr i))
                   (setf (statements form) (cdr i))))
              ((typep stmt 'ast-go)
               (cond ((eql (cadr i) (target stmt))
                      ;; This GO can be eliminated.
                      (change-made)
                      (if prev
                          (setf (cdr prev) (cdr i))
                          (setf (statements form) (cdr i))))
                     (t (setf last-was-go t)
                        (setf prev i))))
              (t (setf prev i)))))
    ;; Reduce tagbodys with no tags to progn.
    (cond ((tagbody-information-go-tags (info form))
           ;; Has go tags.
	   form)
	  ((endp (statements form))
           ;; Empty tagbody.
	   (change-made)
	   (make-instance 'ast-quote :value 'nil))
          (t
           ;; Non-empty tagbody with no go tags.
           (change-made)
           (make-instance 'ast-progn
                          :forms (append (statements form)
                                         (list (make-instance 'ast-quote :value 'nil))))))))

(defmethod simp-form ((form ast-the))
  (cond ((eql (the-type form) 't)
         (change-made)
         (simp-form (value form)))
        (t (setf (value form) (simp-form (value form)))
           form)))

(defmethod simp-form ((form ast-unwind-protect))
  (setf (protected-form form) (simp-form (protected-form form))
        (cleanup-function form) (simp-form (cleanup-function form)))
  form)

(defun eq-comparable-p (value)
  (or (not (numberp value))
      (fixnump value) ;; Use fixnump, not the type fixnum to avoid x-compiler problems.
      (typep value 'single-float)))

(defun simp-eql (form)
  (simp-form-list (arguments form))
  (when (eql (list-length (arguments form)) 2)
    ;; (eql constant non-constant) => (eql non-constant constant)
    (when (and (quoted-form-p (first (arguments form)))
               (not (quoted-form-p (second (arguments form)))))
      (change-made)
      (rotatef (first (arguments form)) (second (arguments form))))
    ;; (eql x eq-comparable-constant) => (eq x eq-comparable-constant)
    (when (and (quoted-form-p (second (arguments form)))
               (eq-comparable-p (value (second (arguments form)))))
      (change-made)
      (setf (name form) 'eq)))
  form)

(defmethod simp-form ((form ast-call))
  ;; (funcall 'symbol ...) -> (symbol ...)
  ;; (funcall #'name ...) -> (name ...)
  (cond ((and (eql (name form) 'funcall)
              (or (typep (first (arguments form)) 'ast-function)
                  (and (typep (first (arguments form)) 'ast-quote)
                       (symbolp (value (first (arguments form)))))))
         (change-made)
         (simp-form-list (rest (arguments form)))
         (let ((name (etypecase (first (arguments form))
                       (ast-quote
                        (value (first (arguments form))))
                       (ast-function
                        (name (first (arguments form)))))))
           (make-instance 'ast-call
                          :name name
                          :arguments (rest (arguments form)))))
        ((eql (name form) 'eql)
         (simp-eql form))
        (t (simp-form-list (arguments form))
           form)))

(defmethod simp-form ((form ast-jump-table))
  (setf (value form) (simp-form (value form)))
  (setf (targets form) (mapcar #'simp-form (targets form)))
  form)

(defmethod simp-form ((form lexical-variable))
  form)

(defmethod simp-form ((form lambda-information))
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (simp-form (second arg))))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (simp-form (second arg))))
    (setf (lambda-information-body form) (simp-form (lambda-information-body form))))
  form)
