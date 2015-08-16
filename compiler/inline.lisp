;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Function inlining.

(in-package :sys.c)

(defun il-form (form)
  (etypecase form
    (cons (ecase (first form)
	    ((block) (il-block form))
	    ((go) (il-go form))
	    ((return-from) (il-return-from form))
	    ((tagbody) (il-tagbody form))))
    (ast-function (il-function form))
    (ast-if (il-if form))
    (ast-let (il-let form))
    (ast-multiple-value-bind (il-multiple-value-bind form))
    (ast-multiple-value-call (il-multiple-value-call form))
    (ast-multiple-value-prog1 (il-multiple-value-prog1 form))
    (ast-progn (il-progn form))
    (ast-quote (il-quote form))
    (ast-setq (il-setq form))
    (ast-the (il-the form))
    (ast-unwind-protect (il-unwind-protect form))
    (ast-call (il-function-form form))
    (ast-jump-table (il-jump-table form))
    (lexical-variable (il-variable form))
    (lambda-information (il-lambda form))))

(defun il-implicit-progn (x)
  (do ((i x (cdr i)))
      ((endp i))
    (setf (car i) (il-form (car i)))))

(defun il-block (form)
  (il-implicit-progn (cddr form))
  form)

(defun il-function (form)
  form)

(defun il-go (form)
  form)

(defun il-if (form)
  (setf (test form) (il-form (test form))
        (if-then form) (il-form (if-then form))
        (if-else form) (il-form (if-else form)))
  form)

(defun il-let (form)
  (dolist (b (bindings form))
    ;; Run on the init-form.
    (setf (second b) (il-form (second b))))
  (setf (body form) (il-form (body form)))
  form)

(defun il-multiple-value-bind (form)
  (setf (value-form form) (il-form (value-form form))
        (body form) (il-form (body form)))
  form)

(defun il-multiple-value-call (form)
  (setf (function-form form) (il-form (function-form form))
        (value-form form) (il-form (value-form form)))
  form)

(defun il-multiple-value-prog1 (form)
  (setf (value-form form) (il-form (value-form form))
        (body form) (il-form (body form)))
  form)

(defun il-progn (form)
  (il-implicit-progn (forms form))
  form)

(defun il-quote (form)
  form)

(defun il-return-from (form)
  (setf (third form) (il-form (third form)))
  (setf (fourth form) (il-form (fourth form)))
  form)

(defun il-setq (form)
  ;; Walk the value form.
  (setf (value form) (il-form (value form)))
  form)

(defun il-tagbody (form)
  (do ((i (cddr form) (cdr i)))
      ((endp i))
    (unless (go-tag-p (car i))
      (setf (car i) (il-form (car i)))))
  form)

(defun il-the (form)
  (setf (value form) (il-form (value form)))
  form)

(defun il-unwind-protect (form)
  (setf (protected-form form) (il-form (protected-form form))
        (cleanup-function form) (il-form (cleanup-function form)))
  form)

(defun il-jump-table (form)
  (setf (value form) (il-form (value form)))
  (il-implicit-progn (targets form))
  form)

(defun expand-inline-function (name arg-list)
  (multiple-value-bind (inlinep expansion)
      (function-inline-info name)
    (when inlinep
      (cond (expansion
             (make-instance 'ast-call
                            :name 'funcall
                            :arguments (list* (pass1-lambda expansion nil) arg-list)))
            ((fboundp name)
             (multiple-value-bind (expansion closurep)
                 (function-lambda-expression (fdefinition name))
               (when (and expansion (not closurep))
                 (make-instance 'ast-call
                                :name 'funcall
                                :arguments (list* (pass1-lambda expansion nil) arg-list)))))))))

(defun il-function-form (form)
  (il-implicit-progn (arguments form))
  (let ((inlined-form (expand-inline-function (name form) (arguments form))))
    (cond (inlined-form
           (change-made)
           inlined-form)
          (t form))))

(defun il-variable (form)
  form)

(defun il-lambda (form)
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (il-form (second arg))))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (il-form (second arg))))
    (setf (lambda-information-body form) (il-form (lambda-information-body form))))
  form)
