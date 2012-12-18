;;;; Bootstrap macros and functions for the cross-compiler.

(in-package #:sys.c)

(def-x-macro in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *package* (sys.int::find-package-or-die ',name))))

(defun sys.int::find-package-or-die (name)
  (or (find-package name) (error "Can't find package ~S." name)))

(def-x-macro defun (name lambda-list &body body)
  (let ((the-lambda `(lambda ,lambda-list
                       (declare (system:lambda-name ,name))
                       ,@body)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (sys.int::%compiler-defun ',name ',the-lambda))
       (sys.int::%defun ',name ,the-lambda))))

(defun sys.int::%compiler-defun (name source-lambda)
  (let ((sym (sys.int::function-symbol name)))
    (when (or (get sym 'sys.int::inline-mode)
              (get sym 'sys.int::inline-form))
      (setf (get sym 'sys.int::inline-form) source-lambda)))
  nil)

(defun sys.int::%defun (name lambda)
  (setf (fdefinition name) lambda)
  name)

;; Enough to load the full DEFMACRO.
(def-x-macro defmacro (name lambda-list &body body)
  (multiple-value-bind (fixed-lambda-list supplied-env-sym)
      (remove-&environment lambda-list)
    (let ((env-sym (or supplied-env-sym (gensym "ENV")))
          (whole-sym))
      (cond ((eql (first fixed-lambda-list) '&whole)
             (assert (not (null (cdr fixed-lambda-list))) ()
                     "Missing variable after &WHOLE in macro lambda-list ~S."
                     lambda-list)
             (setf whole-sym (second fixed-lambda-list)
                   fixed-lambda-list (cddr fixed-lambda-list)))
            (t (setf whole-sym (gensym))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (sys.int::%defmacro ',name (lambda (,whole-sym ,env-sym)
                                      (declare (system:lambda-name (defmacro ,name)))
                                      ,@(unless supplied-env-sym
                                                `((declare (ignore ,env-sym))))
                                      (destructuring-bind ,fixed-lambda-list (cdr ,whole-sym)
                                        (block ,name ,@body))))))))

(defun sys.int::%defmacro (name lambda)
  (unless (cl:macro-function name)
    (setf (cl:macro-function name) lambda))
  (setf (gethash name *system-macros*) lambda))

(defun sys.int::%defconstant (name value &optional docstring)
  (declare (ignore docstring))
  (eval `(cl:defconstant ,name ',value)))

(def-x-macro declaim (&rest declaration-specifiers)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (lambda (x) `(sys.int::proclaim ',x)) declaration-specifiers)))

;; SYS.INT shadows some CL symbols in the cross-environment. When loaded in the true
;; system, the correct symbol will be used.
(defun sys.int::proclaim (declaration-specifier)
  (case (first declaration-specifier)
    (sys.int::constant
     (dolist (sym (rest declaration-specifier))
       (setf (gethash sym *system-symbol-declarations*) :constant)))
    (special
     (dolist (sym (rest declaration-specifier))
       (unless (eql (symbol-package sym) (find-package "CL"))
         (cl:proclaim `(special ,sym)))
       (setf (gethash sym *system-symbol-declarations*) :special)))
    (inline
     (dolist (name (rest declaration-specifier))
       (let ((sym (sys.int::function-symbol name)))
         (setf (get sym 'sys.int::inline-mode) t))))
    (notinline
     (dolist (name (rest declaration-specifier))
       (let ((sym (sys.int::function-symbol name)))
         (setf (get sym 'sys.int::inline-mode) nil))))))

(defun sys.int::dotted-list-length (list)
  "Returns the length of LIST if list is a proper list. Returns NIL if LIST is a circular list."
  ;; Implementation from the HyperSpec
  (do ((n 0 (+ n 2))             ; Counter.
       (fast list (cddr fast))   ; Fast pointer: leaps by 2.
       (slow list (cdr slow)))   ; Slow pointer: leaps by 1.
      (nil)
    ;; If fast pointer hits the end, return the count.
    (when (atom fast) (return n))
    (when (atom (cdr fast)) (return (1+ n)))
    ;; If fast pointer eventually equals slow pointer,
    ;;  then we must be stuck in a circular list.
    (when (and (eq fast slow) (> n 0)) (return nil))))

(defun sys.int::concat-symbols (&rest symbols)
  (intern (apply 'concatenate 'string (mapcar 'string symbols))))

(defun sys.int::structure-name (x) (structure-type-name x))
(defun sys.int::structure-slots (x) (structure-type-slots x))

(defstruct cross-struct data)

(defun sys.int::%defstruct (def)
  (let ((predicate (gensym (string (structure-type-name def)))))
    (setf (symbol-function predicate) (lambda (x)
                                        (and (cross-struct-p x)
                                             (eql (sys.int::%struct-slot x 0) def))))
    (unless (eql (symbol-package (structure-type-name def))
                 (find-package "CL"))
      (eval `(cl:deftype ,(structure-type-name def) () '(satisfies ,predicate))))
    (setf (gethash (structure-type-name def) *structure-types*) def)))

(defun sys.int::%make-struct (length area)
  (declare (ignore area))
  (make-cross-struct :data (make-array length)))

(defun sys.int::%struct-slot (struct index)
  (aref (cross-struct-data struct) index))
(defun (setf sys.int::%struct-slot) (value struct index)
  (setf (aref (cross-struct-data struct) index) value))

(defun sys.int::get-structure-type (name &optional (errorp t))
  (or (gethash name *structure-types*)
      (and errorp
           (error "Unknown structure type ~S." name))))

(macrolet ((def (b-name name)
             `(def-x-macro ,b-name (&rest rest)
                (list* ',name rest))))
  (def sb-impl::backq-list list)
  (def sb-impl::backq-list* list*)
  (def sb-impl::backq-append append)
  (def sb-impl::backq-nconc nconc)
  (def sb-impl::backq-cons cons))

(defconstant sys.int::most-positive-fixnum (- (expt 2 60) 1))
(defconstant sys.int::most-negative-fixnum (- (expt 2 60)))
(alexandria:define-constant sys.int::lambda-list-keywords
    '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)
  :test 'equal)
(defvar sys.int::*features* '(:unicode :little-endian :x86-64 :lisp-os :ieee-floating-point :ansi-cl :common-lisp))

(defun sys.int::%defpackage (name nicknames documentation use-list import-list export-list intern-list)
  (eval `(cl:defpackage ,name
           (:nicknames ,@nicknames)
           ,@(mapcar (lambda (symbol)
                       `(:import-from ,(package-name (symbol-package symbol)) ,symbol))
                     import-list)
           (:export ,@export-list)
           (:intern ,@intern-list)
           ,@(when documentation
               `((:documentation ,documentation)))
           (:use ,@(mapcar (lambda (package)
                             (if (eql (find-package package)
                                      (find-package :cl))
                                 :cross-cl
                                 package))
                           use-list))))
  #+nil(let ((p (or (find-package name)
	       (make-package name :nicknames nicknames))))
    (use-package use-list p)
    (import import-list p)
    (dolist (s intern-list)
      (intern s p))
    (dolist (s export-list)
      (export (list (intern (string s) p)) p))
    p))
