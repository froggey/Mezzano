;;;; Convert assignments to cells.
;;;;
;;;; This pass completely eliminates SETQ of lexical variables
;;;; and makes all lexical variables completely read-only.

(in-package #:sys.newc)

(defgeneric convert-assignments (form &optional env))

(defmethod convert-assignments ((form application) &optional env)
  (cond ((eql (application-function form) '%setq)
         ;; (%SETQ cont var val) -> (SET-CONTENTS cont cell-var val)
         ;; Careful here, cont and val need to be converted as well.
         (let ((args (application-arguments form)))
           (convert-assignments
            (! `(set-contents ,(convert-assignments (first args) env)
                              ,(or (cdr (assoc (second args) env))
                                   (error "Variable assigned but not in env?"))
                              ,(third args)))
            env)))
        (t
         ;; (fn A Y C) ->
         ;; (contents (lambda (Y-val) (fn A Y-val C)) Y-cell)
         ;; For variables in the environment.
         ;; Applications won't appear in the argument list, thanks to CPS.
         ;; The function is relatively uninteresting, deal with it quickly.
         (let* ((fn (if (symbolp (application-function form))
                        (application-function form)
                        (convert-assignments (application-function form) env)))
                (needed-values '())
                (args (mapcar (lambda (arg)
                                (if (assoc arg env)
                                    (let ((val (make-instance 'lexical
                                                              :name (gensym (format nil "~A-value"
                                                                                    (lexical-name arg))))))
                                      (push (cons val arg) needed-values)
                                      val)
                                    arg))
                                (application-arguments form))))
           (labels ((frob (vals)
                      (cond (vals
                             (! `(contents (lambda (,(car (first vals))) ,(frob (rest vals)))
                                           ,(cdr (assoc (cdr (first vals)) env)))))
                            (t (make-instance 'application
                                              :function fn
                                              :arguments (mapcar (lambda (x)
                                                                   (convert-assignments x env))
                                                                 args))))))
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
                              (closure-required-args form)))
         (cell-vars (mapcar (lambda (arg)
                              (make-instance 'lexical
                                             :name (gensym (format nil "~A-cell" (lexical-name arg)))))
                            req-args)))
    (dolist (x req-args)
      (setf (getf (plist x) 'is-set) nil))
    (labels ((frob (args cells)
               (cond (args
                      (! `(make-cell (lambda (,(first cells))
                                       ,(frob (rest args) (rest cells)))
                                     ,(first args))))
                     (t (convert-assignments (closure-body form)
                                             (pairlis req-args cell-vars env))))))
      (! `(lambda ,(closure-required-args form)
            ,(frob req-args cell-vars))))))
