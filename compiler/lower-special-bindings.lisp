;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Lower anything that modifies the special stack to explicit compiler builtins.

(in-package :sys.c)

(defvar *special-bindings*)
(defvar *verify-special-stack* nil)

(defun lsb-lambda (lambda)
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
    (setf (lambda-information-body lambda)
          (lsb-form (lambda-information-body lambda)))
    (when (and *verify-special-stack*
               (not (find 'sys.int::suppress-ssp-checking
                          (getf (lambda-information-plist lambda) :declares)
                          :key #'first)))
      (let ((ssp (make-instance 'lexical-variable
                                :name (gensym "ssp")
                                :definition-point lambda)))
        (setf (lambda-information-body lambda)
              `(let ((,ssp (sys.int::%%special-stack-pointer)))
                 (multiple-value-prog1
                     (lambda-information-body lambda)
                   ,(make-instance 'ast-if
                                   :test `(eq ,ssp (sys.int::%%special-stack-pointer))
                                   :then (make-instance 'ast-quote :value 'nil)
                                   :else `(error ,(make-instance 'ast-quote :value "SSP mismatch"))))))))
    lambda))

(defun lsb-form (form)
  (flet ((map-form (preserve-n-leading-forms)
           (append (subseq form 0 preserve-n-leading-forms)
                   (mapcar #'lsb-form (nthcdr preserve-n-leading-forms form)))))
    (etypecase form
      (cons (case (first form)
              ((if quote) (error "old style ast"))
              ((block)
               (lsb-block form))
              ((go)
               (lsb-go form))
              ((let)
               (lsb-let form))
              ((multiple-value-bind) (map-form 2))
              ((multiple-value-call) (map-form 1))
              ((multiple-value-prog1) (map-form 1))
              ((progn) (map-form 1))
              ((function) form)
              ((return-from)
               (lsb-return-from form))
              ((setq) (map-form 2))
              ((tagbody)
               (lsb-tagbody form))
              ((the) (map-form 2))
              ((unwind-protect)
               (lsb-unwind-protect form))
              (t (map-form 1))))
      (ast-if
       (make-instance 'ast-if
                      :test (lsb-form (test form))
                      :then (lsb-form (if-then form))
                      :else (lsb-form (if-else form))))
      (ast-quote form)
      (lexical-variable form)
      (lambda-information
       (lsb-lambda form)))))

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
           (push '(sys.int::%%disestablish-block-or-tagbody) forms)))
        (:special
         (push '(sys.int::%%unbind) forms))
        (:unwind-protect
         (push '(sys.int::%%disestablish-unwind-protect) forms))))))

(defun lsb-block (form)
  (let ((*special-bindings* *special-bindings*)
        (info (second form)))
    (push (list :block-or-tagbody
                info
                (block-information-env-var info)
                (block-information-env-offset info))
          *special-bindings*)
    (cond
      ((block-information-env-var info)
       ;; Escaping block.
       `(block ,info
          ;; Must be inside the block, so the special stack pointer is saved correctly.
          (sys.int::%%push-special-stack ,(block-information-env-var info)
                                         ,(make-instance 'ast-quote
                                                         :value (block-information-env-offset info)))
          ,(list 'multiple-value-prog1
                 `(progn ,@(mapcar #'lsb-form (cddr form)))
                 '(sys.int::%%disestablish-block-or-tagbody))))
      (t ;; Local block.
       `(block ,info ,@(mapcar #'lsb-form (cddr form)))))))

(defun lsb-go (form)
  (destructuring-bind (tag location) (cdr form)
    (cond ((eql (go-tag-tagbody tag) location)
           ;; Local GO, locate the matching TAGBODY and emit any unwind forms required.
           `(progn
              ,@(lsb-unwind-to (lsb-find-b-or-t-binding location))
              (go ,tag ,location)))
          (t ;; Non-local GO, do the full unwind.
           (let ((info (make-instance 'lexical-variable
                                      :name (gensym "go-info")
                                      :definition-point *current-lambda*)))
             `(let ((,info ,(lsb-form location)))
                ;; Ensure it's still valid.
                ,(make-instance 'ast-if
                                :test info
                                :then (make-instance 'ast-quote :value 'nil)
                                :else `(sys.int::raise-bad-go-tag ,(make-instance 'ast-quote :value (go-tag-name tag))))
                (sys.int::%%unwind-to (sys.int::%%tagbody-info-binding-stack-pointer ,info))
                (go ,tag ,info)))))))

(defun lsb-let (form)
  (let ((*special-bindings* *special-bindings*))
    (labels ((frob (bindings)
               (cond (bindings
                      (let ((binding (first bindings)))
                        (cond ((lexical-variable-p (first binding))
                               `(let ((,(first binding)
                                       ,(lsb-form (second binding))))
                                  ,(frob (rest bindings))))
                              (t
                               (push (list :special (first binding))
                                     *special-bindings*)
                               (list
                                'progn
                                `(sys.int::%%bind ,(make-instance 'ast-quote
                                                                  :value (first binding))
                                                  ,(lsb-form (second binding)))
                                (list
                                 'multiple-value-prog1
                                 (frob (rest bindings))
                                 '(sys.int::%%unbind)))))))
                     (t `(progn ,@(mapcar #'lsb-form (cddr form)))))))
      (frob (second form)))))

(defun lsb-return-from (form)
  (destructuring-bind (tag value-form location) (cdr form)
    (cond ((not (eql tag location))
           ;; Non-local RETURN-FROM, do the full unwind.
           (let ((info (make-instance 'lexical-variable
                                      :name (gensym "return-from-info")
                                      :definition-point *current-lambda*)))
             `(let ((,info ,(lsb-form location)))
                ,(make-instance 'ast-if
                                :test info
                                :then (make-instance 'ast-quote :value 'nil)
                                :else `(sys.int::raise-bad-block
                                        ,(make-instance 'ast-quote
                                                        :value (lexical-variable-name tag))))
                (return-from ,tag
                  ,(list
                    'multiple-value-prog1
                    (lsb-form value-form)
                    `(sys.int::%%unwind-to (sys.int::%%block-info-binding-stack-pointer ,info)))
                  ,info))))
          (t
           ;; Local RETURN-FROM, locate the matching BLOCK and emit any unwind forms required.
           ;; Note: Unwinding one-past the location so as to pop the block as well.
           `(return-from ,tag
              (multiple-value-prog1
                  ,(lsb-form value-form)
                ,@(lsb-unwind-to (cdr (lsb-find-b-or-t-binding tag))))
              ,location)))))

(defun lsb-tagbody (form)
  (let ((*special-bindings* *special-bindings*)
        (info (second form)))
    (flet ((frob-tagbody ()
             `(tagbody ,(second form)
                 ,@(mapcar (lambda (x)
                             (if (go-tag-p x)
                                 x
                                 (lsb-form x)))
                           (cddr form)))))
      (push (list :block-or-tagbody
                  info
                  (tagbody-information-env-var info)
                  (tagbody-information-env-offset info))
            *special-bindings*)
      (cond
        ((tagbody-information-env-var info)
         ;; Escaping TAGBODY.
         (list 'progn
               ;; Must be outside the tagbody, so the special stack pointer is saved correctly.
               `(sys.int::%%push-special-stack ,(tagbody-information-env-var info)
                                               ,(make-instance 'ast-quote
                                                               :value (tagbody-information-env-offset info)))
               (frob-tagbody)
               '(sys.int::%%disestablish-block-or-tagbody)
               (make-instance 'ast-quote :value 'nil)))
        (t ;; Local TAGBODY.
         (frob-tagbody))))))

(defun lsb-unwind-protect (form)
  (let ((*special-bindings* (cons (list :unwind-protect) *special-bindings*)))
    (destructuring-bind (protected-form cleanup-function) (cdr form)
      ;; The cleanup function must either be a naked lambda or a
      ;; call to make-closure with a known lambda.
      (assert (or (lambda-information-p cleanup-function)
                  (and (listp cleanup-function)
                       (eql (first cleanup-function) 'sys.int::make-closure)
                       (= (list-length cleanup-function) 3)
                       (lambda-information-p (second cleanup-function)))))
      (when (not (lambda-information-p cleanup-function))
        ;; cleanup closures use the unwind-protect call protocol (code in r13, env in rbx, no closure indirection).
        (setf (getf (lambda-information-plist (second cleanup-function)) 'unwind-protect-cleanup) t))
      (list
       'progn
       (cond
         ((lambda-information-p cleanup-function)
          `(sys.int::%%push-special-stack ,(lsb-form cleanup-function)
                                          ,(make-instance 'ast-quote :value 0)))
         (t
          (setf (getf (lambda-information-plist (second cleanup-function)) 'unwind-protect-cleanup) t)
          `(sys.int::%%push-special-stack ,(lsb-form (second cleanup-function))
                                          ,(lsb-form (third cleanup-function)))))
       (list 'multiple-value-prog1
             (lsb-form protected-form)
             '(sys.int::%%disestablish-unwind-protect))))))
