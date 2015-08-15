;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Function inlining.

(in-package :sys.c)

(defun il-form (form)
  (etypecase form
    (cons (case (first form)
	    ((block) (il-block form))
	    ((go) (il-go form))
	    ((if) (il-if form))
	    ((let) (il-let form))
	    ((multiple-value-bind) (il-multiple-value-bind form))
	    ((multiple-value-call) (il-multiple-value-call form))
	    ((multiple-value-prog1) (il-multiple-value-prog1 form))
	    ((progn) (il-progn form))
	    ((function) (il-quote form))
            ((quote) (error "old style ast"))
	    ((return-from) (il-return-from form))
	    ((setq) (il-setq form))
	    ((tagbody) (il-tagbody form))
	    ((the) (il-the form))
	    ((unwind-protect) (il-unwind-protect form))
	    (t (il-function-form form))))
    (ast-quote (il-quote form))
    (lexical-variable (il-variable form))
    (lambda-information (il-lambda form))))

(defun il-implicit-progn (x)
  (do ((i x (cdr i)))
      ((endp i))
    (setf (car i) (il-form (car i)))))

(defun il-block (form)
  (il-implicit-progn (cddr form))
  form)

(defun il-go (form)
  form)

(defun il-if (form)
  (il-implicit-progn (cdr form))
  form)

(defun il-let (form)
  (dolist (b (second form))
    ;; Run on the init-form.
    (setf (second b) (il-form (second b))))
  (il-implicit-progn (cddr form))
  form)

(defun il-multiple-value-bind (form)
  (il-implicit-progn (cddr form))
  form)

(defun il-multiple-value-call (form)
  (il-implicit-progn (cdr form))
  form)

(defun il-multiple-value-prog1 (form)
  (il-implicit-progn (cdr form))
  form)

(defun il-progn (form)
  (il-implicit-progn (cdr form))
  form)

(defun il-quote (form)
  form)

(defun il-return-from (form)
  (setf (third form) (il-form (third form)))
  (setf (fourth form) (il-form (fourth form)))
  form)

(defun il-setq (form)
  ;; Walk the value form.
  (setf (third form) (il-form (third form)))
  form)

(defun il-tagbody (form)
  (do ((i (cddr form) (cdr i)))
      ((endp i))
    (unless (go-tag-p (car i))
      (setf (car i) (il-form (car i)))))
  form)

(defun il-the (form)
  (setf (third form) (il-form (third form)))
  form)

(defun il-unwind-protect (form)
  (il-implicit-progn (cdr form))
  form)

(defun expand-inline-function (name arg-list)
  (multiple-value-bind (inlinep expansion)
      (function-inline-info name)
    (when inlinep
      (cond (expansion
             `(funcall ,(pass1-lambda expansion nil) ,@arg-list))
            ((fboundp name)
             (multiple-value-bind (expansion closurep)
                 (function-lambda-expression (fdefinition name))
               (when (and expansion (not closurep))
                 `(funcall ,(pass1-lambda expansion nil) ,@arg-list))))))))

(defun il-function-form (form)
  (il-implicit-progn (cdr form))
  (let ((inlined-form (expand-inline-function (first form) (rest form))))
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
