;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Removal of redundant return-from forms.
;;;
;;; This pass tracks which blocks are active along the tail position.
;;; When a return-from is reached whose target is active, it can be
;;; reduced down to just the value.

(in-package :sys.c)

(defgeneric blexit-1 (form live-block-list))

(defun blexit (lambda)
  (blexit-1 lambda '()))

(defmethod blexit-1 ((form lexical-variable) live-block-list)
  (declare (ignore live-block-list))
  form)

(defmethod blexit-1 ((form lambda-information) live-block-list)
  (declare (ignore live-block-list))
  (let ((*current-lambda* form))
    (dolist (arg (lambda-information-optional-args form))
      (setf (second arg) (blexit-1 (second arg) '())))
    (dolist (arg (lambda-information-key-args form))
      (setf (second arg) (blexit-1 (second arg) '())))
    (setf (lambda-information-body form)
          (blexit-1 (lambda-information-body form) '())))
  form)

(defmethod blexit-1 ((form ast-call) live-block-list)
  (declare (ignore live-block-list))
  (setf (arguments form) (loop
                            for arg in (arguments form)
                            collect (blexit-1 arg '())))
  form)

(defmethod blexit-1 ((form ast-block) live-block-list)
  (setf (body form) (blexit-1 (body form) (list* (info form) live-block-list)))
  form)

(defmethod blexit-1 ((form ast-function) live-block-list)
  (declare (ignore live-block-list))
  form)

(defmethod blexit-1 ((form ast-go) live-block-list)
  (declare (ignore live-block-list))
  (setf (info form) (blexit-1 (info form) nil))
  form)

(defmethod blexit-1 ((form ast-if) live-block-list)
  (setf (test form) (blexit-1 (test form) '())
        (if-then form) (blexit-1 (if-then form) live-block-list)
        (if-else form) (blexit-1 (if-else form) live-block-list))
  form)

(defmethod blexit-1 ((form ast-let) live-block-list)
  (setf (bindings form) (loop
                           for (var initform) in (bindings form)
                           collect (list var (blexit-1 initform '()))))
  (setf (body form) (blexit-1 (body form) live-block-list))
  form)

(defmethod blexit-1 ((form ast-multiple-value-bind) live-block-list)
  (setf (value-form form) (blexit-1 (value-form form) '())
        (body form) (blexit-1 (body form) live-block-list))
  form)

(defmethod blexit-1 ((form ast-multiple-value-call) live-block-list)
  (declare (ignore live-block-list))
  (setf (function-form form) (blexit-1 (function-form form) '())
        (value-form form) (blexit-1 (value-form form) '()))
  form)

(defmethod blexit-1 ((form ast-multiple-value-prog1) live-block-list)
  (declare (ignore live-block-list))
  (setf (value-form form) (blexit-1 (value-form form) '())
        (body form) (blexit-1 (body form) '()))
  form)

(defmethod blexit-1 ((form ast-progn) live-block-list)
  (setf (forms form) (maplist (lambda (l)
                                (cond ((rest l)
                                       (blexit-1 (first l) '()))
                                      (t
                                       (blexit-1 (first l) live-block-list))))
                              (forms form)))
  form)

(defmethod blexit-1 ((form ast-quote) live-block-list)
  (declare (ignore live-block-list))
  form)

(defmethod blexit-1 ((form ast-return-from) live-block-list)
  (cond ((member (ast-target form) live-block-list)
         ;; Target block is in tail position, eliminate this return-from.
         (blexit-1 (value form) live-block-list))
        (t
         (setf (value form) (blexit-1 (value form) live-block-list)
               (info form) (blexit-1 (info form) live-block-list))
         form)))

(defmethod blexit-1 ((form ast-setq) live-block-list)
  (declare (ignore live-block-list))
  (setf (value form) (blexit-1 (value form) '()))
  form)

(defmethod blexit-1 ((form ast-tagbody) live-block-list)
  (declare (ignore live-block-list))
  (setf (statements form)
        (loop
           for (go-tag statement) in (statements form)
           collect (list go-tag (blexit-1 statement '()))))
  form)

(defmethod blexit-1 ((form ast-the) live-block-list)
  (declare (ignore live-block-list))
  (setf (value form) (blexit-1 (value form) '()))
  form)

(defmethod blexit-1 ((form ast-unwind-protect) live-block-list)
  (setf (protected-form form) (blexit-1 (protected-form form) live-block-list))
  (setf (cleanup-function form) (blexit-1 (cleanup-function form) '()))
  form)

(defmethod blexit-1 ((form ast-jump-table) live-block-list)
  (declare (ignore live-block-list))
  (setf (value form) (blexit-1 (value form) '()))
  (setf (targets form) (loop
                          for target in (targets form)
                          collect (blexit-1 target '())))
  form)
