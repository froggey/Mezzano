;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
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

(defun kt-form (form &optional target-variable replacement-form)
  (etypecase form
    (cons (ecase (first form)
	    ((block) (kt-block form target-variable replacement-form))
	    ((go) (kt-go form target-variable replacement-form))
	    ((let) (kt-let form target-variable replacement-form))
	    ((multiple-value-bind) (kt-multiple-value-bind form target-variable replacement-form))
	    ((multiple-value-call) (kt-multiple-value-call form target-variable replacement-form))
	    ((multiple-value-prog1) (kt-multiple-value-prog1 form target-variable replacement-form))
	    ((function) (kt-quote form target-variable replacement-form))
	    ((return-from) (kt-return-from form target-variable replacement-form))
	    ((tagbody) (kt-tagbody form target-variable replacement-form))
	    ((the) (kt-the form target-variable replacement-form))
	    ((unwind-protect) (kt-unwind-protect form target-variable replacement-form))
	    ((sys.int::%jump-table) (kt-jump-table form target-variable replacement-form))))
    (ast-if (kt-if form target-variable replacement-form))
    (ast-progn (kt-progn form target-variable replacement-form))
    (ast-quote (kt-quote form target-variable replacement-form))
    (ast-setq (kt-setq form target-variable replacement-form))
    (ast-call (kt-function-form form target-variable replacement-form))
    (lexical-variable
     (cond ((eql form target-variable)
            (change-made)
            (values (make-instance 'ast-call
                                   :name 'values
                                   :arguments (list replacement-form)) t))
           (t (values form nil))))
    (lambda-information
     (kt-lambda form))))

(defun kt-implicit-progn (x &optional target-variable replacement-form)
  (when x
    ;; Only push through the first form.
    (multiple-value-bind (first-form did-replace)
        (kt-form (first x) target-variable replacement-form)
      (values (list* first-form
                     (mapcar #'kt-form (rest x)))
              did-replace))))

(defun kt-lambda (form)
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (kt-form (second arg))))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (kt-form (second arg))))
    (setf (lambda-information-body form)
          (kt-form (lambda-information-body form))))
  form)

(defun kt-function-form (form target-variable replacement-form)
  (multiple-value-bind (new-list did-replace)
      (kt-implicit-progn (arguments form)
                         target-variable
                         replacement-form)
    (values (make-instance 'ast-call
                           :name (name form)
                           :arguments new-list)
            did-replace)))

(defun temporary-p (varlike)
  (and (lexical-variable-p varlike)
       (eql (lexical-variable-use-count varlike) 1)
       (zerop (lexical-variable-write-count varlike))))

(defun kt-block (form target-variable replacement-form)
  (multiple-value-bind (new-list did-replace)
      (kt-implicit-progn (cddr form) target-variable replacement-form)
    (setf (cddr form) new-list)
    (values form did-replace)))

(defun kt-go (form target-variable replacement-form)
  (declare (ignore target-variable replacement-form))
  form)

(defun kt-if (form target-variable replacement-form)
  (multiple-value-bind (new-test did-replace)
      (kt-form (test form) target-variable replacement-form)
    (setf (test form) new-test
          (if-then form) (kt-form (if-then form))
          (if-else form) (kt-form (if-else form)))
    (values form did-replace)))

(defun kt-let (form target-variable replacement-form)
  (let ((bindings (second form))
        (new-bindings '())
        (body (cddr form)))
    (cond ((null bindings)
           (multiple-value-bind (new-list did-replace)
               (kt-implicit-progn body target-variable replacement-form)
             (values (make-instance 'ast-progn
                                    :forms new-list)
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
             ;; Now push the last binding into the body.
             (multiple-value-bind (new-form replaced-last-binding)
                 (if (temporary-p (first (car (last bindings))))
                     (kt-implicit-progn body
                                        (first (car (last bindings)))
                                        (second (car (last bindings))))
                     (kt-implicit-progn body))
               (unless replaced-last-binding
                 (push (car (last bindings)) new-bindings))
               (values `(let ,(reverse new-bindings)
                          ,@new-form)
                       did-replace)))))))

(defun kt-multiple-value-bind (form target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (third form) target-variable replacement-form)
    (setf (third form) new-form)
    (setf (cdddr form) (kt-implicit-progn (cdddr form)))
    (values form did-replace)))

(defun kt-multiple-value-call (form target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (second form) target-variable replacement-form)
    (setf (second form) new-form)
    (setf (cddr form) (kt-implicit-progn (cddr form)))
    (values form did-replace)))

(defun kt-multiple-value-prog1 (form target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (second form) target-variable replacement-form)
    (setf (second form) new-form)
    (setf (cddr form) (kt-implicit-progn (cddr form)))
    (values form did-replace)))

(defun kt-progn (form target-variable replacement-form)
  (multiple-value-bind (new-list did-replace)
      (kt-implicit-progn (forms form) target-variable replacement-form)
    (setf (forms form) new-list)
    (values form did-replace)))

(defun kt-quote (form target-variable replacement-form)
  (declare (ignore target-variable replacement-form))
  form)

(defun kt-return-from (form target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-implicit-progn (cddr form) target-variable replacement-form)
    (setf (cddr form) new-form)
    (values form did-replace)))

(defun kt-setq (form target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (value form) target-variable replacement-form)
    (setf (value form) new-form)
    (values form did-replace)))

(defun kt-tagbody (form target-variable replacement-form)
  (cond ((and (third form)
              (not (go-tag-p (third form))))
         (multiple-value-bind (new-form did-replace)
             (kt-form (third form) target-variable replacement-form)
           (setf (third form) new-form)
           (do ((i (cdddr form) (cdr i)))
               ((null i))
             (unless (go-tag-p (car i))
               (setf (car i) (kt-form (car i)))))
           (values form did-replace)))
        (t (do ((i (cdddr form) (cdr i)))
               ((null i))
             (unless (go-tag-p (car i))
               (setf (car i) (kt-form (car i)))))
           form)))

(defun kt-the (form target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (third form) target-variable replacement-form)
    (setf (third form) new-form)
    (values form did-replace)))

(defun kt-unwind-protect (form target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (second form) target-variable replacement-form)
    (setf (second form) new-form)
    (setf (third form) (kt-form (third form)))
    (values form did-replace)))

(defun kt-jump-table (form target-variable replacement-form)
  (multiple-value-bind (new-form did-replace)
      (kt-form (second form) target-variable replacement-form)
    (setf (second form) new-form)
    (setf (cddr form) (kt-implicit-progn (cddr form)))
    (values form did-replace)))
