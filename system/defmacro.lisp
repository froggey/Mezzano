;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun expand-destructuring-lambda-list (lambda-list name body whole current-value initial-bindings &key default-value permit-docstring)
  (let ((bindings '())
        (macro-lambda-list-keywords '(&environment &whole &optional &rest &body &key &allow-other-keys &aux)))
    (labels ((check-sublist (list lambda-list req-count opt-count)
               `(unless ,(cond
                          ((null opt-count)
                           `(<= ,req-count (dotted-list-length ,list)))
                          ((zerop req-count)
                           `(<= (list-length ,list) ,opt-count))
                          ((zerop opt-count)
                           `(= ,req-count (dotted-list-length ,list)))
                          (t `(<= ,req-count (dotted-list-length ,list) ,(+ req-count opt-count))))
                  (error "~S does not match destructuring sublist ~S." ,list ',lambda-list)))
             (handle-sublist (sublist whole)
               (let* ((x (gensym "SUBLIST"))
                      (sublist-binding (list x nil))
                      (y (gensym)))
                 (push sublist-binding bindings)
                 (multiple-value-bind (req-count opt-count)
                     (handle-one-level sublist x x)
                   ;; Update sublist binding now that lengths are known.
                   (setf (second sublist-binding) `(let ((,y ,whole))
                                                     ,(check-sublist y sublist req-count opt-count)
                                                     ,y)))))
             (handle-one-level (ll current whole)
               (let ((required-count 0)
                     (optional-count 0))
                 ;; Check for a whole-var.
                 ;; [&whole var]
                 ;; TODO: &WHOLE can destructure as well.
                 (when (eq '&whole (car ll))
                   (when (null (cdr ll))
                     (error 'invalid-macro-lambda-list
                            :lambda-list lambda-list
                            :format-control "Missing variable name after &WHOLE"))
                   (push (list (cadr ll) whole) bindings)
                   (setf ll (cddr ll)))
                 ;; Required args.
                 (do ()
                     ((or (atom ll)
                          (member (car ll) macro-lambda-list-keywords)))
                   (if (consp (car ll))
                       (handle-sublist (car ll) `(car ,current))
                       (push (list (car ll) `(car ,current)) bindings))
                   (setf required-count (1+ required-count)
                         ll (cdr ll)
                         current `(cdr ,current)))
                 (when (not (listp ll))
                   ;; Dotted lists after required arguments as rest arguments.
                   (push (list ll current) bindings)
                   (return-from handle-one-level (values required-count nil)))
                 ;; Optional args.
                 (when (eq (car ll) '&optional)
                   (setf ll (cdr ll))
                   (do ()
                       ((or (atom ll)
                            (member (car ll) macro-lambda-list-keywords)))
                     (if (consp (car ll))
                         (let ((var (first (car ll)))
                               (init-form (if (rest (car ll))
                                              (second (car ll))
                                              default-value))
                               (suppliedp (third (car ll))))
                           (if (consp var)
                               (handle-sublist var `(if ,current (car ,current) ,init-form))
                               (push (list var `(if ,current (car ,current) ,init-form)) bindings))
                           (when suppliedp
                             (push (list suppliedp `(if ,current t nil)) bindings)))
                         (push (list (car ll) `(if ,current (car ,current) ,default-value)) bindings))
                     (setf optional-count (1+ optional-count)
                           ll (cdr ll)
                           current `(cdr ,current))))
                 (when (not (listp ll))
                   ;; Dotted lists after optional arguments as rest arguments.
                   (push (list ll current) bindings)
                   (return-from handle-one-level (values required-count nil)))
                 ;; Rest arg.
                 (when (or (eq (car ll) '&rest)
                           (eq (car ll) '&body))
                   (when (endp (cdr ll))
                     (error 'invalid-macro-lambda-list
                            :lambda-list lambda-list
                            :format-control "Missing variable name after ~S"
                            :format-arguments (list (car lambda-list))))
                   (if (consp (cadr ll))
                       (handle-sublist (cadr ll) current)
                       (push (list (cadr ll) current) bindings))
                   (setf ll (cddr ll)
                         optional-count nil))
                 ;; Keyword arguments.
                 (when (eq (car ll) '&key)
                   (setf ll (cdr ll)
                         optional-count nil)
                   (do ((keywords '()))
                       ((or (endp ll)
                            (member (car ll) macro-lambda-list-keywords))
                        ;; Generate code for keyword parsing.
                        (let ((keyword-args (gensym "KEYWORDS"))
                              (barf (gensym)))
                          ;; Validate the keyword list and skip over &allow-other-keys if present.
                          (if (eq (car ll) '&allow-other-keys)
                              (progn
                                (push (list keyword-args `(progn
                                                            (when (oddp (list-length ,current))
                                                              (error "Odd number of keyword arguments."))
                                                            ,current))
                                      bindings)
                                (setf ll (cdr ll)))
                              (push (list keyword-args `(progn
                                                          (when (oddp (list-length ,current))
                                                            (error "Odd number of keyword arguments."))
                                                          (unless (cadr (member :allow-other-keys ,current))
                                                            (do ((,barf ,current (cddr ,barf)))
                                                                ((null ,barf))
                                                              (unless (member (car ,barf) ',(mapcar #'caar keywords))
                                                                (error "Invalid keyword ~S. Wanted one of ~S." (car ,barf) ',(mapcar #'caar keywords)))))
                                                          ,current))
                                    bindings))
                          (do ((i (nreverse keywords) (cdr i)))
                              ((endp i))
                            (let* ((keyword (caaar i))
                                   (var (cadaar i))
                                   (init-form (if (cdar i)
                                                  (cadar i)
                                                  default-value))
                                   (suppliedp (caddar i))
                                   (x (gensym))
                                   (form `(let ((,x (member ',keyword ,keyword-args)))
                                            (if ,x
                                                (cadr ,x)
                                                ,init-form))))
                              (if (consp var)
                                  (handle-sublist var form)
                                  (push (list var form) bindings))
                              (when suppliedp
                                (push (list suppliedp `(if (member ',keyword ,keyword-args) t nil)) bindings))))))
                     ;; Canonicalize keyword arguments to ((keyword var) init-form suppliedp).
                     (if (consp (car ll))
                         (let ((keyword (if (consp (caar ll))
                                            (caaar ll)
                                            (intern (symbol-name (caar ll)) "KEYWORD")))
                               (var (if (consp (caar ll))
                                        (cadaar ll)
                                        (caar ll)))
                               (init-form (cadar ll))
                               (suppliedp (caddar ll)))
                           (push (list (list keyword var) init-form suppliedp) keywords))
                         (push (list (list (intern (symbol-name (car ll)) "KEYWORD") (car ll)) nil nil) keywords))
                     (setf ll (cdr ll))))
                 ;; &AUX args.
                 (when (eq (car ll) '&aux)
                   (setf ll (cdr ll))
                   (do ()
                       ((or (atom ll)
                            (member (car ll) macro-lambda-list-keywords)))
                     (if (consp (car ll))
                         (let ((var (first (car ll)))
                               (init-form (if (rest (car ll))
                                              (second (car ll))
                                              default-value)))
                           (if (consp var)
                               (handle-sublist var init-form)
                               (push (list var init-form) bindings)))
                         (push (list (car ll) nil) bindings))
                     (setf ll (cdr ll))))
                 (unless (endp ll)
                   (error 'invalid-macro-lambda-list
                          :lambda-list lambda-list
                          :format-control "Unexpected macro lambda-list keyword ~S"
                          :format-arguments (list (car lambda-list))))
                 (values required-count optional-count))))
      (multiple-value-bind (req-count opt-count)
          (handle-one-level lambda-list current-value whole)
        (setf bindings (nreverse bindings))
        ;; Pull declarations up and dump them in the LET.
        ;; TODO: Docstrings.
        (multiple-value-bind (body declares docstring)
            (parse-declares body :permit-docstring permit-docstring)
          (declare (ignore docstring))
          `(progn
             ,(check-sublist current-value lambda-list req-count opt-count)
             (let* (,@initial-bindings
                    ,@(loop
                         for (name init-form) in bindings
                         collect (list (cond (name)
                                             (t (let ((sym (gensym)))
                                                  (push `(ignore ,sym) declares)
                                                  sym)))
                                       init-form)))
               (declare ,@declares)
               ,(if name
                    `(block ,name
                       ,@body)
                    `(progn ,@body)))))))))

(defun fix-lambda-list-environment (lambda-list)
  "Return a lambda-list with the &ENVIRONMENT variable removed and also return the name of
the environment variable (or a gensym if it was not specified)."
  ;; &ENVIRONMENT followed by &WHOLE is illegal.
  ;; FIXME: not general enough, &ENVIRONMENT should only be seen between variable sections.
  (when (and (eq (first lambda-list) '&environment)
             (eq (third lambda-list) '&whole))
    (error 'invalid-macro-lambda-list
           :lambda-list lambda-list
           :format-control "&ENVIRONMENT before &WHOLE"))
  ;; Assemble a new lambda-list by removing &ENVIRONMENT keywords & variables.
  (do* ((env nil)
        (i lambda-list (cdr i))
        (new-lambda-list (cons nil nil))
        (tail new-lambda-list))
       ;; ATOM, not ENDP: macro lambda-lists can be dotted lists.
       ((atom i)
        (setf (cdr tail) i)
        (values (cdr new-lambda-list) env))
    (if (eq '&environment (car i))
        (progn
          (when env
            (error 'invalid-macro-lambda-list
                   :lambda-list lambda-list
                   :format-control "Multiple &ENVIRONMENT variables"))
          ;; (&env nil) is disallowed.
          (when (or (null (cdr i)) (null (cadr i)))
            (error 'invalid-macro-lambda-list
                   :lambda-list lambda-list
                   :format-control "Invalid or missing &ENVIRONMENT variable name"
                   :format-arguments (list (cadr i))))
          ;; Skip past the &ENVIRONMENT and the variable name.
          (setf i (cdr i))
          (setf env (car i)))
        ;; Not an &ENVIRONMENT keyword, just push it on the new list.
        (setf (cdr tail) (cons (car i) nil)
              tail (cdr tail)))))
)

(defmacro defmacro (name lambda-list &body body)
  (let ((whole (gensym "WHOLE"))
        (env (gensym "ENV")))
    (multiple-value-bind (new-lambda-list env-binding)
        (fix-lambda-list-environment lambda-list)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (%defmacro ',name
                    #'(lambda (,whole ,env)
                        (declare (lambda-name (macro-function ,name))
                                 (ignorable ,whole ,env))
                        ,(expand-destructuring-lambda-list new-lambda-list name body
                                                           whole `(cdr ,whole)
                                                           (when env-binding
                                                             (list `(,env-binding ,env)))
                                                           :permit-docstring t))
                    ',lambda-list)))))

(defmacro define-compiler-macro (name lambda-list &body body)
  (let ((whole (gensym "WHOLE"))
        (args (gensym))
        (env (gensym "ENV")))
    (multiple-value-bind (new-lambda-list env-binding)
        (fix-lambda-list-environment lambda-list)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (compiler-macro-function ',name)
               #'(lambda (,whole ,env)
                   (declare (lambda-name (compiler-macro-function ,name))
                            (ignorable ,whole ,env))
                   (let ((,args (if (eql (car ,whole) 'funcall)
                                    (cddr ,whole)
                                    (cdr ,whole))))
                     ,(expand-destructuring-lambda-list new-lambda-list
                                                        (if (symbolp name)
                                                            name
                                                            (second name))
                                                        body whole args
                                                        (when env-binding
                                                          (list `(,env-binding ,env)))
                                                        :permit-docstring t))))
         ',name))))

(defmacro destructuring-bind (lambda-list expression &body body)
  (let ((whole (gensym "WHOLE")))
    `(let ((,whole ,expression))
       (declare (ignorable ,whole))
       ,(expand-destructuring-lambda-list lambda-list nil body whole whole '()))))
