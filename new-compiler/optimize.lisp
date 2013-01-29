(in-package #:sys.newc)

(defgeneric optimize-form (form &optional substitutions))

(defmethod optimize-form ((form application) &optional substitutions)
  (let* ((used-vars '())
         (fn (application-function form))
         ;; Optimize arguments first.
         (args (mapcar (lambda (a)
                         (multiple-value-bind (result used)
                             (optimize-form a substitutions)
                           (setf used-vars (append used used-vars))
                           result))
                       (application-arguments form))))
    (typecase fn
      (symbol
       (case fn
         ((funcall)
          ;; (funcall cont fn args...) -> (fn cont args...)
          (optimize-form (! `(,(second args) ,(first args) ,@(cddr args)))
                         substitutions))
         (t (values (! `(,fn ,@args)) used-vars))))
      (closure
       (assert (= (length args)
                  (length (closure-required-args fn))))
       (let ((paired-args (pairlis (closure-required-args fn) args)))
         ;; Substitute arguments, but not closures
         (multiple-value-bind (result used)
             (optimize-form (closure-body fn) (append (remove-if (lambda (x) (typep x 'closure))
                                                                 paired-args
                                                                 :key 'cdr)
                                                      substitutions))
           ;; Eliminate unused arguments.
           (let* ((used-args (remove-if-not (lambda (x) (member x used)) paired-args :key 'car))
                  (new-args (mapcar 'cdr used-args))
                  (new-params (mapcar 'car used-args)))
             (values (if (null new-args)
                         ;; ((lambda () body)) -> body.
                         result
                         (! `((lambda ,new-params ,result) ,@new-args)))
                     (append used-vars used))))))
      (t ;; Something else.
       (multiple-value-bind (result used)
           (optimize-form fn substitutions)
         (values (! `(,result ,@args))
                 (append used-vars used)))))))

(defmethod optimize-form ((form closure) &optional substitutions)
  (multiple-value-bind (result used)
      (optimize-form (closure-body form) substitutions)
    (values (make-instance 'closure
                           :name (closure-name form)
                           :required-args (closure-required-args form)
                           :body result)
            (remove-if (lambda (x) (member x (closure-required-args form)))
                       used))))

(defmethod optimize-form ((form constant) &optional substitutions)
  (declare (ignore substitutions))
  (values form '()))

(defmethod optimize-form ((form lexical) &optional substitutions)
  ;; Substitute lexicals.
  (let ((replacement (assoc form substitutions)))
    (if replacement
        (optimize-form (cdr replacement) substitutions)
        (values form (list form)))))

(defgeneric build-flow-map (form table))

(defmethod build-flow-map ((form application) table)
  (etypecase (application-function form)
    ;; Applications are impossible here.
    (symbol) ; uninteresting.
    (closure
     ;; Definition back-link.
     (setf (gethash (application-function form) table) form)
     (build-flow-map (application-function form) table))
    (lexical
     (push form (gethash (application-function form) table))))
  (dolist (arg (application-arguments form))
    (etypecase arg
      ;; Applications are impossible here.
      (constant) ; uninteresting.
      (closure
       ;; Definition back-link.
       (setf (gethash arg table) form)
       (build-flow-map arg table))
      (lexical
       (push form (gethash arg table))))))

(defmethod build-flow-map ((form closure) table)
  ;; Definition back-link.
  (dolist (arg (closure-required-args form))
    (push form (gethash arg table)))
  ;; Definition back-link.
  (setf (gethash (closure-body form) table) form)
  (build-flow-map (closure-body form) table))

;; Build a hash-table mapping variables to definitions and uses, and
;; from applications/closures to their containing forms.
(defun flow-map (form)
  (let ((table (make-hash-table)))
    (build-flow-map form table)
    table))

(defun filter-flow-map (fn flow-map)
  (let ((results '()))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (funcall fn k flow-map) (push k results)))
             flow-map)
    results))

;; Replace one form with another, by rebuilding the tree.
(defgeneric substitute-form (form target replacement))

(defmethod substitute-form :around (form target replacement)
  (if (eql form target)
      replacement
      (call-next-method)))

(defmethod substitute-form ((form application) target replacement)
  (make-instance 'application
                 :function (if (symbolp (application-function form))
                               (application-function form)
                               (substitute-form (application-function form)
                                                target replacement))
                 :arguments (mapcar (lambda (f)
                                      (substitute-form f target replacement))
                                    (application-arguments form))))

(defmethod substitute-form ((form closure) target replacement)
  (make-instance 'closure
                 :name (closure-name form)
                 :required-args (closure-required-args form)
                 :body (substitute-form (closure-body form)
                                        target replacement)))

(defmethod substitute-form ((form lexical) target replacement)
  (declare (ignore target replacement))
  form)

(defmethod substitute-form ((form constant) target replacement)
  (declare (ignore target replacement))
  form)
