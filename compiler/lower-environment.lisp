;;;; Lower non-local lexical variable accesses so they refer directly
;;;; to environment objects.
;;;; This is done in two passes.
;;;; Pass 1 discovers escaping variables & assigns them slots
;;;; in their environment vector.
;;;; Pass 2 links each environment vector together and actually
;;;; rewrites the code.
;;;; Vectors are created at LAMBDA and TAGBODY nodes.

(in-package :sys.c)

(defvar *environment-chain*)
(defvar *environment-layout*)
(defvar *active-environment-vector*)

(defun lower-environment (lambda)
  (let ((*environment-layout* (make-hash-table)))
    (compute-environment-layout lambda)
    (let ((*environment* '()))
      (lower-env-form lambda))))

(defun quoted-form-p (form)
  (and (listp form)
       (cdr form)
       (null (cddr form))
       (eql (first form) 'quote)))

(defun compute-environment-layout (form)
  (etypecase form
    (cons (case (first form)
	    ((block)
             (compute-block-environment-layout form))
	    ((go) nil)
	    ((if)
             (mapc #'compute-environment-layout (rest form)))
	    ((let)
             (compute-let-environment-layout form))
	    ((load-time-value) (error "TODO LOAD-TIME-VALUE"))
	    ((multiple-value-bind)
             (compute-mvb-environment-layout form))
	    ((multiple-value-call)
             (mapc #'compute-environment-layout (rest form)))
	    ((multiple-value-prog1)
             (mapc #'compute-environment-layout (rest form)))
	    ((progn)
             (mapc #'compute-environment-layout (rest form)))
	    ((progv)
             (mapc #'compute-environment-layout (rest form)))
	    ((quote) nil)
	    ((return-from)
             (compute-environment-layout (rest form)))
	    ((setq)
             (compute-environment-layout (third form)))
	    ((tagbody)
             (compute-tagbody-environment-layout form))
	    ((the)
             (compute-environment-layout (third form)))
	    ((unwind-protect)
             (mapc #'compute-environment-layout (rest form)))
	    (t (mapc #'compute-environment-layout (rest form)))))
    (lexical-variable nil)
    (lambda-information
     (compute-lambda-environment-layout form))))

(defun maybe-add-environment-variable (variable)
  (when (and (not (symbolp variable))
             (not (localp variable)))
    (push variable (gethash *active-environment-vector* *environment-layout*))))

(defun compute-lambda-environment-layout (lambda)
  (let ((*active-environment-vector* lambda))
    (assert (null (lambda-information-environment-arg lambda)))
    ;; Special variables are not supported here, nor are keywords or non-trivial &OPTIONAL init-forms.
    (assert (every (lambda (arg)
                     (lexical-variable-p arg))
                   (lambda-information-required-args lambda)))
    (assert (every (lambda (arg)
                     (and (lexical-variable-p (first arg))
                          (quoted-form-p (second arg))
                          (or (null (third arg))
                              (lexical-variable-p (first arg)))))
                   (lambda-information-optional-args lambda)))
    (assert (or (null (lambda-information-rest-arg lambda))
                (lexical-variable-p (lambda-information-rest-arg lambda))))
    (assert (not (lambda-information-enable-keys lambda)))
    (dolist (arg (lambda-information-required-args lambda))
      (maybe-add-environment-variable arg))
    (dolist (arg (lambda-information-optional-args lambda))
      (maybe-add-environment-variable (first arg))
      (when (third arg)
        (maybe-add-environment-variable (third arg))))
    (when (lambda-information-rest-arg lambda)
      (maybe-add-environment-variable (lambda-information-rest-arg lambda)))
    (compute-environment-layout `(progn ,@(lambda-information-body lambda)))))

(defun compute-tagbody-environment-layout (form)
  "TAGBODY defines a single variable in the enclosing environment and opens a new contour."
  (maybe-add-environment-variable (second form))
  (let ((*active-environment-vector* form))
    (mapc #'compute-environment-layout (remove-if #'go-tag-p (cddr form)))))

(defun compute-block-environment-layout (form)
  "BLOCK defines one variable."
  (maybe-add-environment-variable (second form))
  (mapc #'compute-environment-layout (cddr form)))

(defun compute-let-environment-layout (form)
  (dolist (binding (second form))
    (maybe-add-environment-variable (first binding))
    (compute-environment-layout (second binding)))
  (mapc #'compute-environment-layout (cddr form)))

(defun compute-mvb-environment-layout (form)
  (dolist (binding (second form))
    (maybe-add-environment-variable binding))
  (mapc #'compute-environment-layout (cddr form)))

(defun lower-env-form (form)
  (etypecase form
    (cons (case (first form)
	    ((block) (le-block form))
	    ((go) (le-go form))
	    ((if) (le-form*-cdr form))
	    ((let) (le-let form))
	    ((load-time-value) (le-load-time-value form))
	    ((multiple-value-bind) (le-multiple-value-bind form))
	    ((multiple-value-call) (le-form*-cdr form))
	    ((multiple-value-prog1) (le-form*-cdr form))
	    ((progn) (le-form*-cdr form))
	    ((progv) (le-form*-cdr form))
	    ((quote) form)
	    ((return-from) (le-return-from form))
	    ((setq) (le-setq form))
	    ((tagbody) (le-tagbody form))
	    ((the) (le-the form))
	    ((unwind-protect) (le-form*-cdr form))
	    (t (le-form*-cdr form))))
    (lexical-variable (le-variable form))
    (lambda-information
     (if *environment-chain*
         `(sys.int::make-closure
           ,(le-lambda form)
           ,(second (first *environment-chain*)))
         (le-lambda form)))))

(defvar *environment-chain* nil
  "The directly accessible environment vectors in this function.")

(defun le-lambda (lambda)
  (let ((*environment-chain* '())
        (*environment* *environment*)
        (local-env (gethash lambda *environment-layout*))
        (*current-lambda* lambda))
    (when *environment*
      ;; The entry environment vector.
      (let ((env (make-lexical-variable :name (gensym "Environment")
                                        :definition-point lambda)))
        (setf (lambda-information-environment-arg lambda) env)
        (push (list (first *environment*) env) *environment-chain*)))
    (cond ((not (endp local-env))
           ;; Environment is present, rewrite body with a new vector.
           (let ((new-env (make-lexical-variable :name (gensym "Environment")
                                                 :definition-point lambda)))
             (push (list lambda new-env) *environment-chain*)
             (push lambda *environment*)
             (setf (lambda-information-body lambda)
                   `((let ((,new-env (sys.int::make-simple-vector ',(1+ (length local-env)))))
                       ,@(when (rest *environment-chain*)
                           (list (list (sys.int::function-symbol '(setf sys.int::%svref))
                                       (second (second *environment-chain*))
                                       new-env
                                       ''0)))
                       ,@(mapcar (lambda (arg)
                                   (list (sys.int::function-symbol '(setf sys.int::%svref))
                                         arg
                                         new-env
                                         `',(1+ (position arg local-env))))
                                 (remove-if #'localp (lambda-information-required-args lambda)))
                       ,@(mapcar (lambda (arg)
                                   (list (sys.int::function-symbol '(setf sys.int::%svref))
                                         (first arg)
                                         new-env
                                         `',(1+ (position (first arg) local-env))))
                                 (remove-if #'localp (lambda-information-optional-args lambda)
                                            :key #'first))
                       ,@(mapcar (lambda (arg)
                                   (list (sys.int::function-symbol '(setf sys.int::%svref))
                                         (third arg)
                                         new-env
                                         `',(1+ (position (third arg) local-env))))
                                 (remove-if #'(lambda (x) (or (null x) (localp x)))
                                            (lambda-information-optional-args lambda)
                                            :key #'third))
                       ,@(when (and (lambda-information-rest-arg lambda)
                                    (not (localp (lambda-information-rest-arg lambda))))
                               (list (list (sys.int::function-symbol '(setf sys.int::%svref))
                                           (lambda-information-rest-arg lambda)
                                           new-env
                                           `',(1+ (position (lambda-information-rest-arg lambda) local-env)))))
                       ,@(mapcar #'lower-env-form (lambda-information-body lambda)))))))
          (t (setf (lambda-information-body lambda) (mapcar #'lower-env-form (lambda-information-body lambda)))))
    lambda))

(defun le-let (form)
  (setf (second form)
        (loop for (variable init-form) in (second form)
           collect (list variable (if (or (symbolp variable)
                                          (localp variable))
                                      (lower-env-form init-form)
                                      (list (sys.int::function-symbol '(setf sys.int::%svref))
                                            (lower-env-form init-form)
                                            (second (first *environment-chain*))
                                            `',(1+ (position variable (gethash (first *environment*) *environment-layout*))))))))
  (setf (cddr form) (mapcar #'lower-env-form (cddr form)))
  form)

(defun get-env-vector (vector-id)
  (let ((chain (assoc vector-id *environment-chain*)))
    (when chain
      (return-from get-env-vector
        (second chain))))
  ;; Not in the chain, walk the rest of the environment.
  (do ((e *environment* (cdr e))
       (c *environment-chain* (cdr c)))
      ((null (cdr c))
       (let ((result (second (car c))))
         (dolist (env (cdr e)
                  (error "Can't find environment for ~S?" vector-id))
           (setf result `(sys.int::%svref ,result '0))
           (when (eql env vector-id)
             (return result)))))))

;;; Locate a variable in the environment.
(defun find-var (var env chain)
  (assert chain (var env chain) "No environment chain?")
  (assert env (var env chain) "No environment?")
  (cond ((member var (first env))
         (values (first chain) 0 (position var (first env))))
        ((rest chain)
         (find-var var (rest env) (rest chain)))
        (t ;; Walk the environment using the current chain as a root.
         (let ((depth 0))
           (dolist (e (rest env)
                    (error "~S not found in environment?" var))
             (incf depth)
             (when (member var e)
               (return (values (first chain) depth
                               (position var e)))))))))

(defun le-variable (form)
  (if (localp form)
      form
      (dolist (e *environment*
               (error "Can't find variable ~S in environment." form))
        (let* ((layout (gethash e *environment-layout*))
               (offset (position form layout)))
          (when offset
            (return `(sys.int::%svref ,(get-env-vector e) ',(1+ offset))))))))

(defun le-form*-cdr (form)
  (list* (first form)
         (mapcar #'lower-env-form (rest form))))

(defun le-block (form)
  (append (list (first form)
                (second form))
          (when (not (localp (second form)))
            (let ((env-var (second (first *environment-chain*)))
                  (env-offset (1+ (position (second form) (gethash (first *environment*) *environment-layout*)))))
              (setf (block-information-env-var (second form)) env-var
                    (block-information-env-offset (second form)) env-offset)
              (list (list (sys.int::function-symbol '(setf sys.int::%svref))
                          (second form)
                          env-var
                          `',env-offset))))
          (mapcar #'lower-env-form (cddr form))))

(defun le-setq (form)
  (if (localp (second form))
      form
      (dolist (e *environment*
               (error "Can't find variable ~S in environment." (second form)))
        (let* ((layout (gethash e *environment-layout*))
               (offset (position (second form) layout)))
          (when offset
            (return (list (sys.int::function-symbol '(setf sys.int::%svref))
                          (lower-env-form (third form))
                          (get-env-vector e)
                          `',(1+ offset))))))))

(defun le-multiple-value-bind (form)
  `(multiple-value-bind ,(second form)
       ,(lower-env-form (third form))
     ,@(mapcan (lambda (var)
                 (when (and (not (symbolp var))
                            (not (localp var)))
                   (list (list (sys.int::function-symbol '(setf sys.int::%svref))
                               var
                               (second (first *environment-chain*))
                               `',(1+ (position var (gethash (first *environment*) *environment-layout*)))))))
               (second form))
     ,@(mapcar #'lower-env-form (cdddr form))))

(defun le-the (form)
  (setf (third form) (lower-env-form (third form)))
  form)

(defun le-go (form)
  (setf (third form) (lower-env-form (third form)))
  form)

(defun le-tagbody (form)
  (let ((local-env (gethash form *environment-layout*))
        (new-env (make-lexical-variable :name (gensym "Environment")
                                        :definition-point *current-lambda*)))
    (labels ((frob-outer ()
             `(tagbody ,(second form)
                 ;; Save the tagbody info.
                 ,@(when (not (localp (second form)))
                     (let ((env-var (second (first *environment-chain*)))
                           (env-offset (1+ (position (second form) (gethash (first *environment*) *environment-layout*)))))
                       (setf (tagbody-information-env-var (second form)) env-var
                             (tagbody-information-env-offset (second form)) env-offset)
                       (list (list (sys.int::function-symbol '(setf sys.int::%svref))
                                   (second form)
                                   env-var
                                   `',env-offset))))
                 ,@(if (endp local-env)
                       (frob-inner)
                       (let ((*environment-chain* (list* (list form new-env)
                                                         *environment-chain*))
                             (*environment* (list* form *environment*)))
                         (if (rest *environment-chain*)
                             (list* (list (sys.int::function-symbol '(setf sys.int::%svref))
                                          (second (second *environment-chain*))
                                          new-env
                                          ''0)
                                    (frob-inner))
                             (frob-inner))))))
             (frob-inner ()
               (mapcar (lambda (x)
                         (if (go-tag-p x)
                             x
                             (lower-env-form x)))
                       (cddr form))))
      (if (endp local-env)
          (frob-outer)
          `(let ((,new-env (sys.int::make-simple-vector ',(1+ (length local-env)))))
             ,(frob-outer))))))

(defun le-return-from (form)
  (setf (third form) (lower-env-form (third form)))
  (setf (fourth form) (lower-env-form (third form)))
  form)
