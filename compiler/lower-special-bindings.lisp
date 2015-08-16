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
              `(let ((,ssp ,(make-instance 'ast-call
                                           :name 'sys.int::%%special-stack-pointer
                                           :arguments '())))
                 (multiple-value-prog1
                     (lambda-information-body lambda)
                   ,(make-instance 'ast-if
                                   :test (make-instance 'ast-call
                                                        :name 'eq
                                                        :arguments (list ssp
                                                                         (make-instance 'ast-call
                                                                                        :name 'sys.int::%%special-stack-pointer
                                                                                        :arguments '())))
                                   :then (make-instance 'ast-quote :value 'nil)
                                   :else (make-instance 'ast-call
                                                        :name 'error
                                                        :arguments (list (make-instance 'ast-quote :value "SSP mismatch")))))))))
    lambda))

(defun lsb-form (form)
  (flet ((map-form (preserve-n-leading-forms)
           (append (subseq form 0 preserve-n-leading-forms)
                   (mapcar #'lsb-form (nthcdr preserve-n-leading-forms form)))))
    (etypecase form
      (cons (ecase (first form)
              ((block)
               (lsb-block form))
              ((go)
               (lsb-go form))
              ((let)
               (lsb-let form))
              ((multiple-value-bind) (map-form 2))
              ((multiple-value-call) (map-form 1))
              ((multiple-value-prog1) (map-form 1))
              ((function) form)
              ((return-from)
               (lsb-return-from form))
              ((tagbody)
               (lsb-tagbody form))
              ((the) (map-form 2))
              ((unwind-protect)
               (lsb-unwind-protect form))
              ((sys.int::%jump-table) (map-form 1))))
      (ast-if
       (make-instance 'ast-if
                      :test (lsb-form (test form))
                      :then (lsb-form (if-then form))
                      :else (lsb-form (if-else form))))
      (ast-progn
       (make-instance 'ast-progn
                      :forms (mapcar #'lsb-form (forms form))))
      (ast-quote form)
      (ast-setq
       (make-instance 'ast-setq
                      :variable (setq-variable form)
                      :value (lsb-form (value form))))
      (ast-call
       (make-instance 'ast-call
                      :name (name form)
                      :arguments (mapcar #'lsb-form (arguments form))))
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
           (push (make-instance 'ast-call
                                :name 'sys.int::%%disestablish-block-or-tagbody
                                :arguments '())
                 forms)))
        (:special
         (push (make-instance 'ast-call
                                :name 'sys.int::%%unbind
                                :arguments '())
               forms))
        (:unwind-protect
         (push (make-instance 'ast-call
                                :name 'sys.int::%%disestablish-unwind-protect
                                :arguments '())
               forms))))))

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
          ,(make-instance 'ast-call
                          :name 'sys.int::%%push-special-stack
                          :arguments (list (block-information-env-var info)
                                           (make-instance 'ast-quote
                                                          :value (block-information-env-offset info))))
          (multiple-value-prog1
              ,(make-instance 'ast-progn
                              :forms (mapcar #'lsb-form (cddr form)))
            ,(make-instance 'ast-call
                            :name 'sys.int::%%disestablish-block-or-tagbody
                            :arguments '()))))
      (t ;; Local block.
       `(block ,info ,@(mapcar #'lsb-form (cddr form)))))))

(defun lsb-go (form)
  (destructuring-bind (tag location) (cdr form)
    (cond ((eql (go-tag-tagbody tag) location)
           ;; Local GO, locate the matching TAGBODY and emit any unwind forms required.
           (make-instance 'ast-progn
                          :forms (append (lsb-unwind-to (lsb-find-b-or-t-binding location))
                                         (list `(go ,tag ,location)))))
          (t ;; Non-local GO, do the full unwind.
           (let ((info (make-instance 'lexical-variable
                                      :name (gensym "go-info")
                                      :definition-point *current-lambda*)))
             `(let ((,info ,(lsb-form location)))
                ;; Ensure it's still valid.
                ,(make-instance 'ast-if
                                :test info
                                :then (make-instance 'ast-quote :value 'nil)
                                :else (make-instance 'ast-call
                                                     :name 'sys.int::raise-bad-go-tag
                                                     :arguments (list (make-instance 'ast-quote :value (go-tag-name tag)))))
                ,(make-instance 'ast-call
                                :name 'sys.int::%%unwind-to
                                :arguments (list (make-instance 'ast-call
                                                                :name 'sys.int::%%tagbody-info-binding-stack-pointer
                                                                :arguments (list info))))
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
                               (make-instance 'ast-progn
                                              :forms (list (make-instance 'ast-call
                                                                          :name 'sys.int::%%bind
                                                                          :arguments (list (make-instance 'ast-quote
                                                                                                          :value (first binding))
                                                                                           (lsb-form (second binding))))
                                                           `(multiple-value-prog1
                                                                ,(frob (rest bindings))
                                                              ,(make-instance 'ast-call
                                                                              :name 'sys.int::%%unbind
                                                                              :arguments '()))))))))
                     (t (make-instance 'ast-progn
                                       :forms (mapcar #'lsb-form (cddr form)))))))
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
                                :else (make-instance 'ast-call
                                                     :name 'sys.int::raise-bad-block
                                                     :arguments (list (make-instance 'ast-quote
                                                                                     :value (lexical-variable-name tag)))))
                (return-from ,tag
                  (multiple-value-prog1
                      ,(lsb-form value-form)
                    ,(make-instance 'ast-call
                                    :name 'sys.int::%%unwind-to
                                    :arguments (list (make-instance 'ast-call
                                                                    :name 'sys.int::%%block-info-binding-stack-pointer
                                                                    :arguments (list info)))))
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
         (make-instance 'ast-progn
                        :forms (list
                                ;; Must be outside the tagbody, so the special stack pointer is saved correctly.
                                (make-instance 'ast-call
                                               :name 'sys.int::%%push-special-stack
                                               :arguments (list (tagbody-information-env-var info)
                                                                (make-instance 'ast-quote
                                                                               :value (tagbody-information-env-offset info))))
                                (frob-tagbody)
                                (make-instance 'ast-call
                                               :name 'sys.int::%%disestablish-block-or-tagbody
                                               :arguments '())
                                (make-instance 'ast-quote :value 'nil))))
        (t ;; Local TAGBODY.
         (frob-tagbody))))))

(defun lsb-unwind-protect (form)
  (let ((*special-bindings* (cons (list :unwind-protect) *special-bindings*)))
    (destructuring-bind (protected-form cleanup-function) (cdr form)
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
      (make-instance 'ast-progn
                     :forms (list
                             (make-instance 'ast-call
                                            :name 'sys.int::%%push-special-stack
                                            :arguments (cond
                                                         ((lambda-information-p cleanup-function)
                                                          (list (lsb-form cleanup-function)
                                                                (make-instance 'ast-quote :value 0)))
                                                         (t
                                                          (setf (getf (lambda-information-plist (first (arguments cleanup-function))) 'unwind-protect-cleanup) t)
                                                          (list (lsb-form (first (arguments cleanup-function)))
                                                                (lsb-form (second (arguments cleanup-function)))))))
                             `(multiple-value-prog1
                                  ,(lsb-form protected-form)
                                ,(make-instance 'ast-call
                                                :name 'sys.int::%%disestablish-unwind-protect
                                                :arguments '())))))))
