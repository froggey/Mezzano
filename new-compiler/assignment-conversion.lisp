;;;; Convert assignments to cells.
;;;;
;;;; This pass completely eliminates SETQ of lexical variables
;;;; and makes all lexical variables completely read-only.

(in-package #:sys.newc)

(defgeneric convert-assignments (form &optional env))

(defun convert-application-assignments (form &optional env)
  (cond ((and (typep (first form) 'constant)
              (eql (constant-value (first form)) '%setq))
         ;; (%SETQ cont var val) -> (SET-CONTENTS cont cell-var val)
         ;; Careful here, cont and val need to be converted as well.
         (let ((args (rest form)))
           (convert-application-assignments
            (list (make-instance 'constant :value '%set-contents)
                  (convert-assignments (first args) env)
                  (or (cdr (assoc (second args) env))
                      (error "Variable assigned but not in env?"))
                  (third args))
            env)))
        (t
         ;; (fn A Y C) ->
         ;; (contents (lambda (Y-val) (fn A Y-val C)) Y-cell)
         ;; For variables in the environment.
         ;; Applications won't appear in the argument list, thanks to CPS.
         ;; The function is relatively uninteresting, deal with it quickly.
         (let* ((needed-values '())
                (args (mapcar (lambda (arg)
                                (if (assoc arg env)
                                    (let ((val (make-instance 'lexical
                                                              :name (gensym (format nil "~A-value"
                                                                                    (lexical-name arg))))))
                                      (push (cons val arg) needed-values)
                                      val)
                                    arg))
                                form)))
           (labels ((frob (vals)
                      (cond (vals
                             `(%contents ,(! `(clambda (,(car (first vals))) ,(frob (rest vals))))
                                         ,(cdr (assoc (cdr (first vals)) env))))
                            (t (mapcar (lambda (x)
                                         (convert-assignments x env))
                                       args)))))
             (frob needed-values))))))

(defmethod convert-assignments ((form constant) &optional env)
  (declare (ignore env))
  form)

(defmethod convert-assignments ((form lexical) &optional env)
  (when (assoc form env)
    ;; Thanks to CPS, lexicals are always in the
    ;; form (thing LEXICAL). This has to be handled in
    ;; the application method.
    (error "Can't deal with bare lexicals!"))
  form)

(defmethod convert-assignments ((form closure) &optional env)
  ;; (lambda (X) ...) ->
  ;; (lambda (X-orig)
  ;;   (make-cell (lambda (X-cell) ...) X-orig))
  ;; Where X has the IS-SET property.
  (let* ((req-args (remove-if-not (lambda (x) (getf (plist x) 'is-set))
                              (closure-required-params form)))
         (cell-vars (mapcar (lambda (arg)
                              (make-instance 'lexical
                                             :name (gensym (format nil "~A-cell" (lexical-name arg)))))
                            req-args)))
    (dolist (x req-args)
      (setf (getf (plist x) 'is-set) nil))
    (labels ((frob (args cells)
               (cond (args
                      `(%make-cell ,(! `(clambda (,(first cells))
                                          ,(frob (rest args) (rest cells))))
                                   ,(first args)))
                     (t (convert-application-assignments (closure-body form)
                                                         (pairlis req-args cell-vars env))))))
      (! `(,(if (getf (plist form) 'continuation) 'clambda 'lambda) ,(closure-required-params form)
            ,(frob req-args cell-vars))))))
