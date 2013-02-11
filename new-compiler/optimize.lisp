(in-package #:sys.newc)

(defgeneric optimize-form (form use-map &optional substitutions))

(defun optimize-application (form use-map &optional substitutions)
  (let* ((used-vars '())
         (fn (first form))
         ;; Optimize arguments first.
         (args (mapcar (lambda (a)
                         (multiple-value-bind (result used)
                             (optimize-form a use-map substitutions)
                           (setf used-vars (append used used-vars))
                           result))
                       (rest form))))
    (typecase fn
      (constant
       (case (constant-value fn)
         ((funcall)
          ;; (funcall cont fn args...) -> (fn cont args...)
          (optimize-application (list* (second args) (first args) (cddr args))
                                use-map
                         substitutions))
         (t (values (list* fn args) used-vars))))
      (closure
       (assert (= (length args)
                  (length (closure-required-params fn))))
       (let ((paired-args (pairlis (closure-required-params fn) args)))
         ;; Substitute lexicals, constants and used-once closures.
         (multiple-value-bind (result used)
             (optimize-application (closure-body fn)
                                   use-map
                                   (append (remove-if (lambda (x)
                                                        (and (typep (cdr x) 'closure)
                                                             (not (= (length (gethash (car x) use-map)) 1))))
                                                      paired-args)
                                           substitutions))
           ;; Eliminate unused arguments.
           (let* ((used-args (remove-if-not (lambda (x) (member x used)) paired-args :key 'car))
                  (new-args (mapcar 'cdr used-args))
                  (new-params (mapcar 'car used-args)))
             (values (if (null new-args)
                         ;; ((lambda () body)) -> body.
                         (multiple-value-bind (r2 u2)
                             (optimize-application result use-map substitutions)
                           (setf used-args (union u2 used-args))
                           r2)
                         ;; Closures used in bindings are always treated as continuations.
                         (list* (! `(clambda
                                      ,new-params ,result)) new-args))
                     (append used-vars used))))))
      (t ;; Something else.
       (multiple-value-bind (result used)
           (optimize-form fn use-map substitutions)
         (values (list* result args)
                 (append used-vars used)))))))

(defmethod optimize-form ((form closure) use-map &optional substitutions)
  ;; (lambda () (foo)) => foo
  (cond ((and (null (closure-required-params form))
              (null (closure-arguments form)))
         (closure-function form))
        (t (multiple-value-bind (result used)
               (optimize-application (closure-body form) use-map substitutions)
             (values (make-instance 'closure
                                    :name (closure-name form)
                                    :required-params (closure-required-params form)
                                    :body result
                                    :plist (list 'continuation (getf (plist form) 'continuation)))
                     (remove-if (lambda (x) (member x (closure-required-params form)))
                                used))))))

(defmethod optimize-form ((form constant) use-map &optional substitutions)
  (declare (ignore substitutions use-map))
  (values form '()))

(defmethod optimize-form ((form lexical) use-map &optional substitutions)
  ;; Substitute lexicals.
  (let ((replacement (assoc form substitutions)))
    (if replacement
        (optimize-form (cdr replacement) use-map substitutions)
        (values form (list form)))))

;; Build a hash-table mapping variables and closures to their uses.
(defun use-map (closure)
  (let ((table (make-hash-table)))
    (build-use-map closure table)
    table))

(defun build-use-map (closure table)
  (dolist (x (closure-body closure))
    (pushnew closure (gethash x table))
    (when (typep x 'closure)
      (build-use-map x table))))

;; Build a hash-table mapping variables to their definitions.
(defun def-map (closure)
  (let ((table (make-hash-table)))
    (build-def-map closure table)
    table))

(defun build-def-map (closure table)
  (dolist (var (closure-required-params closure))
    (setf (gethash var table) closure))
  (dolist (x (closure-body closure))
    (when (typep x 'closure)
      (build-def-map x table))))

(defun filter-map (fn map)
  (let ((results '()))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (funcall fn k map) (push k results)))
             map)
    results))

;; Replace one form with another, by rebuilding the tree.
(defgeneric substitute-form (form target replacement))

(defmethod substitute-form :around (form target replacement)
  (if (eql form target)
      replacement
      (call-next-method)))

(defmethod substitute-form ((form closure) target replacement)
  (make-instance 'closure
                 :name (closure-name form)
                 :required-args (closure-required-params form)
                 :body (let ((form (closure-body form)))
                         (list* (substitute-form (first form)
                                                 target replacement)
                                (mapcar (lambda (f)
                                          (substitute-form f target replacement))
                                        (rest form))))))

(defmethod substitute-form ((form lexical) target replacement)
  (declare (ignore target replacement))
  form)

(defmethod substitute-form ((form constant) target replacement)
  (declare (ignore target replacement))
  form)
