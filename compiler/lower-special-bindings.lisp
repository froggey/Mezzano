;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Lower anything that modifies the special stack to explicit compiler builtins.

(in-package :sys.c)

(defvar *special-bindings*)
(defvar *verify-special-stack* nil)

(defun lower-special-bindings (lambda)
  (lsb-form lambda))

(defgeneric lsb-form (form))

(defmethod lsb-form ((lambda lambda-information))
  (let ((*current-lambda* lambda)
        (*special-bindings* '()))
    ;; Check some assertions.
    ;; No keyword arguments, no special arguments, no non-constant
    ;; &optional init-forms and no non-local arguments.
    (assert (not (lambda-information-enable-keys lambda)) (lambda)
            "&KEY arguments did not get lowered!")
    (assert (every (lambda (arg)
                     (lexical-variable-p arg))
                   (lambda-information-required-args lambda))
            (lambda) "Special required arguments did not get lowered!")
    (assert (every (lambda (arg)
                     (and (lexical-variable-p (first arg))
                          (quoted-form-p (second arg))
                          (or (null (third arg))
                              (lexical-variable-p (first arg)))))
                   (lambda-information-optional-args lambda))
            (lambda) "Special or complex optional arguments did not get lowered!")
    (assert (or (null (lambda-information-rest-arg lambda))
                (lexical-variable-p (lambda-information-rest-arg lambda)))
            (lambda) "Special rest argument did not get lowered!")
    (assert (or (null (lambda-information-fref-arg lambda))
                (lexical-variable-p (lambda-information-fref-arg lambda)))
            (lambda) "Special fref argument did not get lowered!")
    (assert (or (null (lambda-information-closure-arg lambda))
                (lexical-variable-p (lambda-information-closure-arg lambda)))
            (lambda) "Special closure argument did not get lowered!")
    (assert (or (null (lambda-information-count-arg lambda))
                (lexical-variable-p (lambda-information-count-arg lambda)))
            (lambda) "Special count argument did not get lowered!")
    (setf (lambda-information-body lambda)
          (lsb-form (lambda-information-body lambda)))
    (when (and *verify-special-stack*
               (not (find 'sys.int::suppress-ssp-checking
                          (getf (lambda-information-plist lambda) :declares)
                          :key #'first)))
      (setf (lambda-information-body lambda)
            (ast `(let ((ssp (call sys.int::%%special-stack-pointer)))
                    (multiple-value-prog1 ,(lambda-information-body lambda)
                      (if (call eq ssp (call sys.int::%%special-stack-pointer))
                          'nil
                          (call error '"SSP mismatch"))))
                 lambda)))
    lambda))

(defmethod lsb-form ((form ast-block))
  (let ((*special-bindings* *special-bindings*)
        (info (info form)))
    (push (list :block-or-tagbody
                info
                (block-information-env-var info)
                (block-information-env-offset info))
          *special-bindings*)
    (cond
      ((block-information-env-var info)
       ;; Escaping block.
       (ast `(block ,info
               (progn
                 ;; Must be inside the block, so the special stack pointer is saved correctly.
                 (call sys.int::%%push-special-stack
                       ,(block-information-env-var info)
                       (quote ,(block-information-env-offset info)))
                 (multiple-value-prog1 ,(lsb-form (body form))
                   (call sys.int::%%disestablish-block-or-tagbody))))
            form))
      (t ;; Local block.
       (ast `(block ,info
               ,(lsb-form (body form)))
            form)))))

(defmethod lsb-form ((form ast-function))
  form)

(defmethod lsb-form ((form ast-go))
  (let ((tag (target form))
        (location (info form)))
    (cond ((eql (go-tag-tagbody tag) location)
           ;; Local GO, locate the matching TAGBODY and emit any unwind forms required.
           (ast `(progn ,@(lsb-unwind-to (lsb-find-b-or-t-binding location))
                        (go ,tag ,location))
                form))
          (t ;; Non-local GO, do the full unwind.
           (ast `(let ((info ,(lsb-form location)))
                   (progn
                     ;; Ensure it's still valid.
                     (if info
                         (quote nil)
                         (call sys.int::raise-bad-go-tag (quote (go-tag-name tag))))
                     (call sys.int::%%unwind-to
                           (call sys.int::%%tagbody-info-binding-stack-pointer info))
                     (go ,tag info)))
                form)))))

(defmethod lsb-form ((form ast-if))
  (ast `(if ,(lsb-form (test form))
            ,(lsb-form (if-then form))
            ,(lsb-form (if-else form)))
       form))

(defmethod lsb-form ((form ast-let))
  (let ((*special-bindings* *special-bindings*))
    (labels ((frob (bindings)
               (cond (bindings
                      (let* ((binding (first bindings))
                             (variable (first binding))
                             (init-form (second binding)))
                        (etypecase variable
                          (lexical-variable
                           (ast `(let ((,variable ,(lsb-form init-form)))
                                   ,(frob (rest bindings)))
                                form))
                          (special-variable
                           (push (list :special (first binding))
                                 *special-bindings*)
                           (ast `(progn
                                   (call sys.int::%%bind
                                         (quote ,(name variable))
                                         ,(lsb-form init-form))
                                   (multiple-value-prog1 ,(frob (rest bindings))
                                     (call sys.int::%%unbind)))
                                form)))))
                     (t (lsb-form (body form))))))
      (frob (bindings form)))))

(defmethod lsb-form ((form ast-multiple-value-bind))
  (ast `(multiple-value-bind ,(bindings form)
            ,(lsb-form (value-form form))
          ,(lsb-form (body form)))
       form))

(defmethod lsb-form ((form ast-multiple-value-call))
  (ast `(multiple-value-call ,(lsb-form (function-form form))
          ,(lsb-form (value-form form)))
       form))

(defmethod lsb-form ((form ast-multiple-value-prog1))
  (ast `(multiple-value-prog1 ,(lsb-form (value-form form))
          ,(lsb-form (body form)))
       form))

(defmethod lsb-form ((form ast-progn))
  (ast `(progn ,@(mapcar #'lsb-form (forms form)))
       form))

(defmethod lsb-form ((form ast-quote))
  form)

(defmethod lsb-form ((form ast-return-from))
  (let ((tag (target form))
        (value-form (value form))
        (location (info form)))
    (cond ((not (eql tag location))
           ;; Non-local RETURN-FROM, do the full unwind.
           (ast `(let ((info ,(lsb-form location)))
                   (progn
                     (if info
                         (quote nil)
                         (call sys.int::raise-bad-block (quote ,(lexical-variable-name tag))))
                     (return-from ,tag
                       (multiple-value-prog1
                           ,(lsb-form value-form)
                         (call sys.int::%%unwind-to
                               (call sys.int::%%block-info-binding-stack-pointer info)))
                       info)))
                form))
          (t
           ;; Local RETURN-FROM, locate the matching BLOCK and emit any unwind forms required.
           ;; Note: Unwinding one-past the location so as to pop the block as well.
           (ast `(return-from ,tag
                   (multiple-value-prog1
                       ,(lsb-form value-form)
                     (progn ,@(lsb-unwind-to (cdr (lsb-find-b-or-t-binding tag)))))
                   ,location)
                form)))))

(defmethod lsb-form ((form ast-setq))
  (ast `(setq ,(setq-variable form) ,(lsb-form (value form)))
       form))

(defmethod lsb-form ((form ast-tagbody))
  (let ((*special-bindings* *special-bindings*)
        (info (info form)))
    (flet ((frob-tagbody ()
             (ast `(tagbody ,(info form)
                      ,@(loop
                           for (go-tag stmt) in (statements form)
                           collect (list go-tag (lsb-form stmt))))
                  form)))
      (push (list :block-or-tagbody
                  info
                  (tagbody-information-env-var info)
                  (tagbody-information-env-offset info))
            *special-bindings*)
      (cond
        ((tagbody-information-env-var info)
         ;; Escaping TAGBODY.
         (ast `(progn
                 ;; Must be outside the tagbody, so the special stack pointer is saved correctly.
                 (call sys.int::%%push-special-stack
                       ,(tagbody-information-env-var info)
                       (quote ,(tagbody-information-env-offset info)))
                 ,(frob-tagbody)
                 (call sys.int::%%disestablish-block-or-tagbody)
                 'nil)
              form))
        (t ;; Local TAGBODY.
         (frob-tagbody))))))

(defmethod lsb-form ((form ast-the))
  (ast `(the ,(the-type form) ,(lsb-form (value form)))
       form))

(defmethod lsb-form ((form ast-unwind-protect))
  (let ((*special-bindings* (cons (list :unwind-protect) *special-bindings*))
        (protected-form (protected-form form))
        (cleanup-function (cleanup-function form)))
    ;; The cleanup function must either be a naked lambda or a
    ;; call to make-closure with a known lambda.
    (assert (or (lambda-information-p cleanup-function)
                (and (typep cleanup-function 'ast-call)
                     (eql (name cleanup-function) 'sys.int::make-closure)
                     (= (list-length (arguments cleanup-function)) 2)
                     (lambda-information-p (first (arguments cleanup-function))))))
    (when (not (lambda-information-p cleanup-function))
      ;; cleanup closures use the unwind-protect call protocol (code in r13, env in rbx, no closure indirection).
      (setf (getf (lambda-information-plist (first (arguments cleanup-function))) 'unwind-protect-cleanup) t))
    (ast `(progn (call sys.int::%%push-special-stack
                       ,@(cond
                          ((lambda-information-p cleanup-function)
                           (list (lsb-form cleanup-function)
                                 '(quote 0)))
                          (t
                           (setf (getf (lambda-information-plist (first (arguments cleanup-function))) 'unwind-protect-cleanup) t)
                           (list (lsb-form (first (arguments cleanup-function)))
                                 (lsb-form (second (arguments cleanup-function)))))))
                 (multiple-value-prog1
                     ,(lsb-form protected-form)
                   (call sys.int::%%disestablish-unwind-protect)))
         form)))

(defmethod lsb-form ((form ast-call))
  (ast `(call ,(name form)
              ,@(mapcar #'lsb-form (arguments form)))
       form))

(defmethod lsb-form ((form ast-jump-table))
  (ast `(jump-table ,(lsb-form (value form))
                    ,@(mapcar #'lsb-form (targets form)))
       form))

(defmethod lsb-form ((form lexical-variable))
  form)

(defun lsb-find-b-or-t-binding (info)
  "Locate the BLOCK or TAGBODY binding info on the *SPECIAL-BINDINGS* stack."
  (do ((i *special-bindings* (cdr i)))
      ((and (eql (first (first i)) :block-or-tagbody)
            (eql (second (first i)) info))
       i)
    (assert i () "Could not find block/tagbody information?")))

(defun lsb-unwind-to (location)
  "Generate code to unwind to a given location on the binding stack."
  (do ((current *special-bindings* (cdr current))
       (forms '()))
      ((eql current location)
       (nreverse forms))
    (assert current () "Ran off the end of the binding stack?")
    (let ((binding (first current)))
      (ecase (first binding)
        (:block-or-tagbody
         (when (third binding)
           (push (ast `(call sys.int::%%disestablish-block-or-tagbody)) forms)))
        (:special
         (push (ast `(call sys.int::%%unbind)) forms))
        (:unwind-protect
         (push (ast `(call sys.int::%%disestablish-unwind-protect)) forms))))))
