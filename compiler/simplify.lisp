;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Simplifiy the ast by removing empty nodes and unused variables.

(in-package :sys.c)

(defun simp-form (form)
  (etypecase form
    (cons (ecase (first form)
	    ((block) (simp-block form))
	    ((go) (simp-go form))
	    ((let) (simp-let form))
	    ((return-from) (simp-return-from form))
	    ((tagbody) (simp-tagbody form))
	    ((unwind-protect) (simp-unwind-protect form))))
    (ast-function (simp-function form))
    (ast-if (simp-if form))
    (ast-multiple-value-bind (simp-multiple-value-bind form))
    (ast-multiple-value-call (simp-multiple-value-call form))
    (ast-multiple-value-prog1 (simp-multiple-value-prog1 form))
    (ast-progn (simp-progn form))
    (ast-quote (simp-quote form))
    (ast-setq (simp-setq form))
    (ast-the (simp-the form))
    (ast-call (simp-function-form form))
    (ast-jump-table (simp-jump-table form))
    (lexical-variable (simp-variable form))
    (lambda-information (simp-lambda form))))

(defun simp-form-list (x)
  (do ((i x (cdr i)))
      ((endp i))
    (setf (car i) (simp-form (car i)))))

(defun simp-implicit-progn (x)
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
             (simp-implicit-progn (forms form))
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
                  (consp form)
                  (member (first form) '(go return-from)))
             ;; Non-local exit. Remove all following forms.
             (change-made)
             (setf (rest i) nil))
            (t (setf (first i) (simp-form form)))))))

(defun simp-block (form)
  (cond
    ;; Unused blocks get reduced to progn.
    ((eql (lexical-variable-use-count (second form)) 0)
     (change-made)
     (simp-form (make-instance 'ast-progn :forms (cddr form))))
    ;; (block foo) => 'nil
    ((eql (length form) 2)
     (change-made)
     (make-instance 'ast-quote :value nil))
    ;; (block foo ... (return-from foo form)) => (block foo ... form)
    ((and (listp (first (last form)))
          (eql (first (first (last form))) 'return-from)
          (eql (second form) (second (first (last form)))))
     (change-made)
     (setf (first (last form)) (third (first (last form))))
     form)
    (t (simp-implicit-progn (cddr form))
       form)))

(defun simp-function (form)
  form)

(defun simp-go (form)
  ;; HACK: Update the tagbody location part after tagbodies have merged.
  (when (tagbody-information-p (third form))
    (setf (third form) (go-tag-tagbody (second form))))
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
             (when (find-if #'symbolp bound-variables)
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

(defun simp-if (form)
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
             `(block ,new-block
                (tagbody ,new-tagbody
                   ,(simp-form (make-instance 'ast-if
                                              :test (test test-form)
                                              ;; Special case here to catch (if a a b), generated by OR.
                                              :then (if (eql (test test-form) (if-then test-form))
                                                        `(go ,then-tag ,(go-tag-tagbody then-tag))
                                                        (make-instance 'ast-if
                                                                       :test (if-then test-form)
                                                                       :then `(go ,then-tag ,(go-tag-tagbody then-tag))
                                                                       :else `(go ,else-tag ,(go-tag-tagbody else-tag))))
                                              :else (make-instance 'ast-if
                                                                   :test (if-else test-form)
                                                                   :then `(go ,then-tag ,(go-tag-tagbody then-tag))
                                                                   :else `(go ,else-tag ,(go-tag-tagbody else-tag)))))
                   ,then-tag
                   (return-from ,new-block ,(simp-form (if-then form)) ,new-block)
                   ,else-tag
                   (return-from ,new-block ,(simp-form (if-else form)) ,new-block)))))
          ((and (listp (if-then form))
                (eql (first (if-then form)) 'go)
                (listp (if-else form))
                (eql (first (if-else form)) 'go)
                (eql (second (if-then form)) (second (if-else form)))
                (eql (third (if-then form)) (third (if-else form))))
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

(defun simp-let (form)
  ;; Merge nested LETs when possible, do not merge special bindings!
  (do ((nested-form (caddr form) (caddr form)))
      ((or (not (consp nested-form))
	   (not (eq (first nested-form) 'let))
           (some 'symbolp (mapcar 'first (second form)))
	   (and (second nested-form)
                (symbolp (first (first (second nested-form)))))))
    (change-made)
    (if (null (second nested-form))
	(setf (cddr form) (nconc (cddr nested-form) (cdddr form)))
	(setf (second form) (nconc (second form) (list (first (second nested-form))))
	      (second nested-form) (rest (second nested-form)))))
  ;; Remove unused values with no side-effects.
  (setf (second form) (remove-if (lambda (b)
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
				 (second form)))
  (dolist (b (second form))
    (setf (second b) (simp-form (second b))))
  ;; Remove the LET if there are no values.
  (cond ((second form)
         (simp-implicit-progn (cddr form))
         form)
        (t
         (change-made)
         (simp-form (make-instance 'ast-progn
                                   :forms (cddr form))))))

(defun simp-multiple-value-bind (form)
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
         (simp-form `(let ((,(first (bindings form))
                            ,(value-form form)))
                       ,(body form))))
        ;; Use an inner LET form to bind any special variables.
        ((some #'symbolp (bindings form))
         (change-made)
         (let* ((specials (remove-if-not #'symbolp (bindings form)))
                (replacements (loop for s in specials
                                 collect (make-instance 'lexical-variable
                                                        :name s
                                                        :definition-point *current-lambda*
                                                        :use-count 1)))
                ;; Also doubles up as an alist mapping specials to replacements.
                (bindings (mapcar #'list specials replacements)))
           (make-instance 'ast-multiple-value-bind
                          :bindings (mapcar (lambda (var)
                                              (if (symbolp var)
                                                  (second (assoc var bindings))
                                                  var))
                                            (second form))
                          :value-form (simp-form (value-form form))
                          :body `(let ,bindings
                                   ,@(simp-form (body form))))))
        (t (setf (value-form form) (simp-form (value-form form))
                 (body form) (simp-form (body form)))
           form)))

(defun simp-multiple-value-call (form)
  (setf (function-form form) (simp-form (function-form form))
        (value-form form) (simp-form (value-form form)))
  form)

(defun simp-multiple-value-prog1 (form)
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

(defun simp-progn (form)
  (cond ((null (forms form))
	 ;; Flush empty PROGNs.
	 (change-made)
	 (make-instance 'ast-quote :value 'nil))
	((null (rest (forms form)))
	 ;; Reduce single form PROGNs.
	 (change-made)
	 (simp-form (first (forms form))))
	(t (simp-implicit-progn (forms form))
	   form)))

(defun simp-quote (form)
  form)

(defun simp-return-from (form)
  (setf (third form) (simp-form (third form)))
  (setf (fourth form) (simp-form (fourth form)))
  form)

(defun simp-setq (form)
  (setf (value form) (simp-form (value form)))
  form)

(defun simp-tagbody (form)
  (labels ((flatten (x)
	     (cond ((typep x 'ast-progn)
		    (change-made)
		    (apply #'nconc (mapcar #'flatten (forms x))))
		   ((and (consp x)
			 (eq (car x) 'tagbody))
		    ;; Merge directly nested TAGBODY forms, dropping unused go tags.
		    (change-made)
		    (setf (tagbody-information-go-tags (second form))
			  (nconc (tagbody-information-go-tags (second form))
				 (delete-if (lambda (x) (eql (go-tag-use-count x) 0))
					    (tagbody-information-go-tags (second x)))))
                    (dolist (tag (tagbody-information-go-tags (second x)))
                      (setf (go-tag-tagbody tag) (second form)))
		    (apply #'nconc (mapcar (lambda (x)
					     (if (go-tag-p x)
						 (unless (eql (go-tag-use-count x) 0)
						   (list x))
						 (flatten x)))
					   (cddr x))))
		   (t (cons (simp-form x) nil)))))
    (setf (tagbody-information-go-tags (second form))
	  (delete-if (lambda (x) (eql (go-tag-use-count x) 0))
		     (tagbody-information-go-tags (second form))))
    (do* ((i (cddr form) (cdr i))
	  (result (cons (cadr form) nil))
	  (tail result))
	 ((endp i)
          (setf (cdr form) result))
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
    (do* ((i (cddr form) (cdr i))
          (prev (cdr form))
          (last-was-go nil))
         ((endp i))
      (let ((stmt (car i)))
        (cond ((go-tag-p stmt)
               (setf last-was-go nil))
              (last-was-go
               (change-made)
               (setf (cdr prev) (cdr i)))
              ((and (listp stmt)
                    (eql (first stmt) 'go))
               (cond ((eql (cadr i) (second stmt))
                      ;; This GO can be eliminated.
                      (change-made)
                      (setf (cdr prev) (cdr i)))
                     (t (setf last-was-go t)
                        (setf prev i))))
              (t (setf prev i)))))
    ;; Reduce tagbodys with no tags to progn.
    (cond ((tagbody-information-go-tags (second form))
           ;; Has go tags.
	   form)
	  ((null (cddr form))
           ;; Empty tagbody.
	   (change-made)
	   (make-instance 'ast-quote :value 'nil))
          (t
           ;; Non-empty tagbody with no go tags.
           (change-made)
           (make-instance 'ast-progn
                          :forms (append (cddr form)
                                         (list (make-instance 'ast-quote :value 'nil))))))))

(defun simp-the (form)
  (cond ((eql (the-type form) 't)
         (change-made)
         (simp-form (value form)))
        (t (setf (value form) (simp-form (value form)))
           form)))

(defun simp-unwind-protect (form)
  (setf (second form) (simp-form (second form)))
  (simp-implicit-progn (cddr form))
  form)

(defun simp-jump-table (form)
  (setf (value form) (simp-form (value form)))
  (setf (targets form) (mapcar #'simp-form (targets form)))
  form)

(defun simp-function-form (form)
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

(defun simp-variable (form)
  form)

(defun simp-lambda (form)
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (simp-form (second arg))))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (simp-form (second arg))))
    (setf (lambda-information-body form) (simp-form (lambda-information-body form))))
  form)
