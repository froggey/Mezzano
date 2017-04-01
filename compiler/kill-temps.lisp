;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

;;; Attempt to eliminate temporary variables (bound, never assigned, used once).
;;; Bound forms are pushed forward through the IR until their one use point
;;; is found, then it is replaced and the original binding removed.
;;; (let ((foo (bar))) (baz foo)) => (baz (values (bar)))
;;; The codegen doesn't deal with explicit temporaries very well, so eliminating
;;; them is fairly important.
;;; VALUES prevents additional values from leaking without any impact on the
;;; generated code.

(defun kill-temporaries (lambda)
  (kt-form lambda))

(defgeneric kt-form (form &optional target-variable replacement-form))

(defmethod kt-form ((form lexical-variable) &optional target-variable replacement-form)
  (cond ((eql form target-variable)
         (change-made)
         (values (ast `(call values ,replacement-form) form)
                 t))
        (t (values form nil))))

(defun kt-implicit-progn (forms &optional target-variable replacement-form)
  (let ((did-something nil))
    (values
     (loop
        with saw-impure = nil
        for form in forms
        collect (cond
                  ((or saw-impure
                       did-something
                       (typep (unwrap-the form) '(or ast-quote ast-function lambda-information)))
                   (kt-form form))
                  ((typep (unwrap-the form) 'lexical-variable)
                   (multiple-value-bind (new did-replace)
                       (kt-form form target-variable replacement-form)
                     (when did-replace
                       (setf did-something t))
                     new))
                  (t
                   (setf saw-impure t)
                   (multiple-value-bind (new did-replace)
                       (kt-form form target-variable replacement-form)
                     (when did-replace
                       (setf did-something t))
                     new))))
     did-something)))

(defmethod kt-form ((form lambda-information) &optional target-variable replacement-form)
  (declare (ignore target-variable replacement-form))
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (kt-form (second arg))))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (kt-form (second arg))))
    (setf (lambda-information-body form)
          (kt-form (lambda-information-body form))))
  form)

(defmethod kt-form ((form ast-call) &optional target-variable replacement-form)
  (multiple-value-bind (new-list did-replace)
      (kt-implicit-progn (arguments form)
                         target-variable
                         replacement-form)
    (values (ast `(call ,(name form) ,@new-list) form)
            did-replace)))

(defun temporary-p (varlike)
  (and (lexical-variable-p varlike)
       (eql (lexical-variable-use-count varlike) 1)
       (zerop (lexical-variable-write-count varlike))))

(defmethod kt-form ((form ast-block) &optional target-variable replacement-form)
  (multiple-value-bind (new-body did-replace)
      (kt-form (body form) target-variable replacement-form)
    (setf (body form) new-body)
    (values form did-replace)))

(defmethod kt-form ((form ast-function) &optional target-variable replacement-form)
  (declare (ignore target-variable replacement-form))
  form)

(defmethod kt-form ((form ast-go) &optional target-variable replacement-form)
  (multiple-value-bind (new-info did-replace)
      (kt-form (info form) target-variable replacement-form)
    (setf (info form) new-info)
    (values form did-replace)))

(defmethod kt-form ((form ast-if) &optional target-variable replacement-form)
  (multiple-value-bind (new-test did-replace)
      (kt-form (test form) target-variable replacement-form)
    (setf (test form) new-test
          (if-then form) (kt-form (if-then form))
          (if-else form) (kt-form (if-else form)))
    (values form did-replace)))

(defmethod kt-form ((form ast-let) &optional target-variable replacement-form)
  (let ((bindings (bindings form))
        (new-bindings '())
        (body (body form)))
    (cond ((null bindings)
           (multiple-value-bind (new-body did-replace)
               (kt-form body target-variable replacement-form)
             (values new-body
                     did-replace)))
          (t ;; Try to push the active replacement into the first binding.
           (multiple-value-bind (new-form did-replace)
               (kt-form (second (first bindings)) target-variable replacement-form)
             (setf (second (first bindings)) new-form)
             ;; Try to push bindings down into the next binding.
             (do ((b bindings (cdr b)))
                 ((null (cdr b)))
               (if (temporary-p (first (car b)))
                   (multiple-value-bind (new-form did-replace)
                       (kt-form (second (cadr b)) (first (car b)) (second (car b)))
                     (setf (second (cadr b)) new-form)
                     (unless did-replace
                       ;; No replacement, preserve this binding.
                       (push (car b) new-bindings)))
                   ;; Not a temp, preserve.
                   (push (car b) new-bindings)))
             ;; Descend into binding init-forms.
             (dolist (b bindings)
               (setf (second b) (kt-form (second b))))
             ;; Now push the last binding into the body.
             (multiple-value-bind (new-form replaced-last-binding)
                 (if (temporary-p (first (car (last bindings))))
                     (kt-form body
                              (first (car (last bindings)))
                              (second (car (last bindings))))
                     (kt-form body))
               (unless replaced-last-binding
                 (push (car (last bindings)) new-bindings))
               (values (ast `(let ,(reverse new-bindings)
                               ,new-form)
                            form)
                       did-replace)))))))

(defmethod kt-form ((form ast-multiple-value-bind) &optional target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (value-form form) target-variable replacement-form)
    (setf (value-form form) new-form
          (body form) (kt-form (body form)))
    (values form did-replace)))

(defmethod kt-form ((form ast-multiple-value-call) &optional target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (function-form form) target-variable replacement-form)
    (setf (function-form form) new-form
          (value-form form) (kt-form (value-form form)))
    (values form did-replace)))

(defmethod kt-form ((form ast-multiple-value-prog1) &optional target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (value-form form) target-variable replacement-form)
    (setf (value-form form) new-form
          (body form) (kt-form (body form)))
    (values form did-replace)))

(defmethod kt-form ((form ast-progn) &optional target-variable replacement-form)
  (multiple-value-bind (new-list did-replace)
      (kt-implicit-progn (forms form) target-variable replacement-form)
    (setf (forms form) new-list)
    (values form did-replace)))

(defmethod kt-form ((form ast-quote) &optional target-variable replacement-form)
  (declare (ignore target-variable replacement-form))
  form)

(defmethod kt-form ((form ast-return-from) &optional target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (value form) target-variable replacement-form)
    (setf (value form) new-form
          (info form) (kt-form (info form)))
    (values form did-replace)))

(defmethod kt-form ((form ast-setq) &optional target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (value form) target-variable replacement-form)
    (setf (value form) new-form)
    (values form did-replace)))

(defmethod kt-form ((form ast-tagbody) &optional target-variable replacement-form)
  (cond ((eql (go-tag-use-count (first (first (statements form)))) 1)
         ;; Entry statement only has one use, safe to push forms into it.
         (multiple-value-bind (new-form did-replace)
             (kt-form (second (first (statements form))) target-variable replacement-form)
           (setf (second (first (statements form))) new-form)
           (setf (rest (statements form))
                 (loop
                    for (go-tag statement) in (rest (statements form))
                    collect (list go-tag (kt-form statement))))
           (values form did-replace)))
        (t
         (setf (statements form)
               (loop
                  for (go-tag statement) in (statements form)
                  collect (list go-tag (kt-form statement))))
         form)))

(defmethod kt-form ((form ast-the) &optional target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (value form) target-variable replacement-form)
    (setf (value form) new-form)
    (values form did-replace)))

(defmethod kt-form ((form ast-unwind-protect) &optional target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (protected-form form) target-variable replacement-form)
    (setf (protected-form form) new-form)
    (setf (cleanup-function form) (kt-form (cleanup-function form)))
    (values form did-replace)))

(defmethod kt-form ((form ast-jump-table) &optional target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (value form) target-variable replacement-form)
    (setf (value form) new-form)
    (setf (targets form) (kt-implicit-progn (targets form)))
    (values form did-replace)))
