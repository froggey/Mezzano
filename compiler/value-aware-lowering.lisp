;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

;;; Lower forms based on the number of values they're expected to generated.
;;; Either no value (for effect, not 0 values), one value or mulitple (including 0) values.

(defvar *rename-list*)

(defun value-aware-lowering (lambda)
  (let ((*rename-list* '()))
    (value-aware-lowering-1 lambda :single)))

(defgeneric value-aware-lowering-1 (form mode))

(defmethod value-aware-lowering-1 ((form ast-block) mode)
  (setf (block-information-return-mode (info form)) mode)
  (ecase mode
    (:effect
     ;; Replace this with a TAGBODY.
     (change-made)
     (let* ((info (info form))
            (end-tag (make-instance 'go-tag
                                    :inherit info
                                    :name 'block-end
                                    :use-count 5
                                    :tagbody info)))
       (push (cons (info form) end-tag) *rename-list*)
       (let ((body (value-aware-lowering-1 (body form) :effect)))
         (change-class info 'tagbody-information
                       :go-tags (list end-tag))
         (ast `(tagbody ,info
                  (entry ,body)
                  (,end-tag 'nil))
              form))))
    ((:single :multiple)
     (setf (body form) (value-aware-lowering-1 (body form) mode))
     form)))

(defmethod value-aware-lowering-1 ((form ast-function) mode)
  (ecase mode
    (:effect
     (change-made)
     (ast `(quote nil) form))
    ((:single :multiple)
     form)))

(defmethod value-aware-lowering-1 ((form ast-go) mode)
  (setf (info form) (value-aware-lowering-1 (info form) :single))
  form)

(defmethod value-aware-lowering-1 ((form ast-if) mode)
  (setf (test form) (value-aware-lowering-1 (test form) :single)
        (if-then form) (value-aware-lowering-1 (if-then form) mode)
        (if-else form) (value-aware-lowering-1 (if-else form) mode))
  form)

(defmethod value-aware-lowering-1 ((form ast-let) mode)
  (loop
     for binding in (bindings form)
     do (setf (second binding) (value-aware-lowering-1 (second binding) :single)))
  (setf (body form) (value-aware-lowering-1 (body form) mode))
  form)

(defmethod value-aware-lowering-1 ((form ast-multiple-value-bind) mode)
  (setf (value-form form) (value-aware-lowering-1 (value-form form) :multiple)
        (body form) (value-aware-lowering-1 (body form) mode))
  form)

(defmethod value-aware-lowering-1 ((form ast-multiple-value-call) mode)
  (declare (ignore mode))
  (setf (function-form form) (value-aware-lowering-1 (function-form form) :single)
        (value-form form) (value-aware-lowering-1 (value-form form) :multiple))
  form)

(defmethod value-aware-lowering-1 ((form ast-multiple-value-prog1) mode)
  (ecase mode
    (:effect
     ;; Replace with PROGN.
     (change-made)
     (value-aware-lowering-1
      (ast `(progn ,(value-form form)
                   ,(body form))
           form)
      :effect))
    (:single
     ;; Replace with PROG1.
     (change-made)
     (value-aware-lowering-1
      (ast `(let ((result-value ,(value-form form)))
              (progn
                ,(body form)
                result-value))
           form)
      :single))
    (:multiple
     (setf (value-form form) (value-aware-lowering-1 (value-form form) :multiple)
           (body form) (value-aware-lowering-1 (body form) :effect))
     form)))

(defmethod value-aware-lowering-1 ((form ast-progn) mode)
  (when (forms form)
    (do ((i (forms form) (rest i)))
        ((endp (rest i))
         (setf (first i) (value-aware-lowering-1 (first i) mode)))
      (setf (first i) (value-aware-lowering-1 (first i) :effect))))
  form)

(defmethod value-aware-lowering-1 ((form ast-quote) mode)
  (declare (ignore mode))
  form)

(defmethod value-aware-lowering-1 ((form ast-return-from) mode)
  (declare (ignore mode))
  (ecase (block-information-return-mode (target form))
    (:effect
     (change-made)
     (value-aware-lowering-1
      (ast `(progn
              ,(value form)
              (go ,(cdr (assoc (target form) *rename-list*))
                  ,(info form)))
           form)
      :effect))
    ((:single :multiple)
     (setf (value form) (value-aware-lowering-1 (value form)
                                                (block-information-return-mode (target form)))
           (info form) (value-aware-lowering-1 (info form) :single))
     form)))

(defmethod value-aware-lowering-1 ((form ast-setq) mode)
  (setf (value form) (value-aware-lowering-1 (value form) :single))
  form)

(defmethod value-aware-lowering-1 ((form ast-tagbody) mode)
  (setf (statements form) (loop
                             for (go-tag stmt) in (statements form)
                             collect (list go-tag (value-aware-lowering-1 stmt :effect))))
  form)

(defmethod value-aware-lowering-1 ((form ast-the) mode)
  (setf (value form) (value-aware-lowering-1 (value form) mode))
  form)

(defmethod value-aware-lowering-1 ((form ast-unwind-protect) mode)
  (setf (protected-form form) (value-aware-lowering-1 (protected-form form) mode)
        (cleanup-function form) (value-aware-lowering-1 (cleanup-function form) :single))
  form)

(defparameter *pure-functions* '(consp sys.int::fixnump))

(defmethod value-aware-lowering-1 ((form ast-call) mode)
  (cond ((and (eql (name form) 'values)
              (not (eql mode :multiple)))
         ;; Remove VALUES in non-multiple-value context.
         (change-made)
         (cond ((endp (arguments form))
                ;; No arguments, evaluate to NIL.
                (ast `(quote nil) form))
               ((endp (rest (arguments form)))
                ;; One argument, evaluate to it.
                (value-aware-lowering-1 (first (arguments form)) mode))
               (t ;; Many arguments, make something like PROG1.
                (value-aware-lowering-1
                 (ast `(let ((result-value ,(first (arguments form))))
                         (progn
                           ,@(rest (arguments form))
                           result-value))
                      form)
                 mode))))
        ((and (eql mode :effect)
              (member (name form) *pure-functions* :test #'equal))
         ;; (call <pure-function> ...) => (progn ...)
         (value-aware-lowering-1 (ast `(progn ,@(arguments form))
                                      form)
                                 mode))
        (t
         (setf (arguments form) (loop
                                   for arg in (arguments form)
                                   collect (value-aware-lowering-1 arg :single)))
         form)))

(defmethod value-aware-lowering-1 ((form ast-jump-table) mode)
  (declare (ignore mode))
  (setf (value form) (value-aware-lowering-1 (value form) :single)
        (targets form) (loop
                          for target in (targets form)
                          collect (value-aware-lowering-1 target :effect)))
  form)

(defmethod value-aware-lowering-1 ((form lexical-variable) mode)
  (ecase mode
    (:effect
     (change-made)
     (ast `(quote nil) form))
    ((:single :multiple)
     form)))

(defmethod value-aware-lowering-1 ((form lambda-information) mode)
  (let ((*current-lambda* form))
    (loop
       for arg in (lambda-information-optional-args form)
       ;; init-form
       do (setf (second arg) (value-aware-lowering-1 (second arg) :single)))
    (loop
       for arg in (lambda-information-key-args form)
       ;; init-form
       do (setf (second arg) (value-aware-lowering-1 (second arg) :single)))
    (setf (lambda-information-body form) (value-aware-lowering-1 (lambda-information-body form)
                                                                 :multiple))
    form))
