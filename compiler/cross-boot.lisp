;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Bootstrap macros and functions for the cross-compiler.

(in-package :sys.c)

(setf (gethash 'cross-cl:defconstant *system-macros*)
      (cl:macro-function 'cross-cl:defconstant))

(def-x-macro in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *package* (sys.int::find-package-or-die ',name))))

(defun sys.int::find-package-or-die (name)
  (or (find-package name) (error "Can't find package ~S." name)))

(def-x-macro defun (name lambda-list &body body)
  (let ((the-lambda `(lambda ,lambda-list
                       (declare (sys.int::lambda-name ,name))
                       ,@body)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (sys.int::%compiler-defun ',name ',the-lambda))
       (sys.int::%defun ',name ,the-lambda))))

(defvar *inline-modes* (make-hash-table :test #'equal))
(defvar *inline-forms* (make-hash-table :test #'equal))
(defvar *function-types* (make-hash-table :test #'equal))

(defun sys.int::%compiler-defun (name source-lambda)
  (when (or (gethash name *inline-modes*)
            (gethash name *inline-forms*))
      (setf (gethash name *inline-forms*) source-lambda))
  nil)

(defun sys.int::%defun (name lambda)
  ;; Completely ignore CAS functions when cross compiling, they're not needed.
  (unless (and (consp name) (eql (first name) 'sys.int::cas))
    (setf (fdefinition name) lambda))
  name)

(defun function-inline-info (name)
  (values (gethash name *inline-modes*)
          (gethash name *inline-forms*)))

(defvar *variable-types* (make-hash-table))

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
                                      (declare (sys.int::lambda-name (defmacro ,name)))
                                      ,@(unless supplied-env-sym
                                          `((declare (ignore ,env-sym))))
                                      (destructuring-bind ,fixed-lambda-list (cdr ,whole-sym)
                                        (block ,name ,@body))))))))

(defun sys.int::%defmacro (name lambda &optional lambda-list)
  (declare (ignore lambda-list))
  (unless (cl:macro-function name)
    (setf (cl:macro-function name) lambda))
  (setf (gethash name *system-macros*) lambda))

(defun sys.int::%defconstant (name value &optional docstring)
  (declare (ignore docstring))
  (when (or (not (boundp name))
            (not (loose-constant-equal (symbol-value name) value)))
    (eval `(cl:defconstant ,name ',value))))

(def-x-macro declaim (&rest declaration-specifiers)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (lambda (x) `(sys.int::proclaim ',x)) declaration-specifiers)))

(defun proclaim-symbol-mode (sym mode)
  (check-type sym symbol)
  (when (not (or (null (sys.int::symbol-mode sym))
                 (eql (sys.int::symbol-mode sym) mode)))
    (cerror "Continue" "Symbol ~S being changed from ~S to ~S."
            sym (sys.int::symbol-mode sym) mode))
  (setf (gethash sym *system-symbol-declarations*) mode))

;; SYS.INT shadows some CL symbols in the cross-environment. When loaded in the true
;; system, the correct symbol will be used.
(defun sys.int::proclaim (declaration-specifier)
  (case (first declaration-specifier)
    (sys.int::constant
     (dolist (sym (rest declaration-specifier))
       (proclaim-symbol-mode sym :constant)))
    (special
     (dolist (sym (rest declaration-specifier))
       (proclaim-symbol-mode sym :special)
       (unless (eql (symbol-package sym) (find-package "CL"))
         (cl:proclaim `(special ,sym)))))
    (sys.int::global
     (dolist (sym (rest declaration-specifier))
       (proclaim-symbol-mode sym :global)
       (unless (eql (symbol-package sym) (find-package "CL"))
         (cl:proclaim `(special ,sym)))))
    (inline
     (dolist (name (rest declaration-specifier))
       (setf (gethash name *inline-modes*) t)))
    (notinline
     (dolist (name (rest declaration-specifier))
       (setf (gethash name *inline-modes*) nil)))
    (type
     (destructuring-bind (typespec &rest vars)
         (rest declaration-specifier)
       (dolist (name vars)
         (setf (gethash name *variable-types*) typespec))))
    (ftype
     (destructuring-bind (typespec &rest vars)
         (rest declaration-specifier)
       (dolist (name vars)
         (setf (gethash name *function-types*) typespec))))
    (t (warn "Unknown declaration ~S" (first declaration-specifier)))))

(defun sys.int::symbol-mode (symbol)
  (check-type symbol symbol)
  (values (gethash symbol *system-symbol-declarations*)))

(defun mezzano.runtime::symbol-type (symbol)
  (check-type symbol symbol)
  (values (gethash symbol *variable-types* 't)))

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
  (when (gethash (structure-type-name def) *structure-types*)
    (assert (eql (gethash (structure-type-name def) *structure-types*) def))
    (assert (eql (get (structure-type-name def) 'sys.int::structure-type) def)))
  (let ((predicate (gensym (string (structure-type-name def)))))
    (setf (symbol-function predicate) (lambda (x)
                                        (and (cross-struct-p x)
                                             (eql (sys.int::%struct-slot x 0) def))))
    (unless (or (eql (symbol-package (structure-type-name def))
                     (find-package "CL"))
                (eql (symbol-package (structure-type-name def))
                     (find-package "SYS.C")))
      (eval `(cl:deftype ,(structure-type-name def) () '(satisfies ,predicate))))
    (setf (get (structure-type-name def) 'sys.int::structure-type) def)
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

(defun sys.int::structure-object-p (object)
  (cross-struct-p object))

(defun sys.int::structure-type-p (object struct-type)
  (when (cross-struct-p object)
    (do ((object-type (sys.int::%struct-slot object 0)
                      (structure-type-parent object-type)))
        ((not (structure-type-p object-type))
         nil)
      (when (eq object-type struct-type)
        (return t)))))

(defconstant sys.int::most-positive-fixnum (- (expt 2 62) 1))
(defconstant sys.int::most-negative-fixnum (- (expt 2 62)))
(alexandria:define-constant sys.int::lambda-list-keywords
    '(&allow-other-keys &aux &body &environment &key &optional &rest &whole sys.int::&fref sys.int::&closure)
  :test 'equal)
(defvar sys.int::*features* '(:unicode :little-endian :mezzano :ieee-floating-point :ansi-cl :common-lisp))

(defun sys.int::%defpackage (name &key
                                    nicknames
                                    documentation
                                    ((:uses use-list))
                                    ((:imports import-list))
                                    ((:exports export-list))
                                    ((:interns intern-list))
                                    ((:shadows shadow-list))
                                    ((:shadowing-imports shadowing-import-list))
                                    ((:local-nicknames package-local-nicknames)))
  (eval `(cl:defpackage ,name
           (:nicknames ,@nicknames)
           ,@(mapcar (lambda (symbol)
                       `(:import-from ,(package-name (symbol-package symbol)) ,symbol))
                     import-list)
           (:export ,@export-list)
           (:intern ,@intern-list)
           (:shadow ,@shadow-list)
           ,@(mapcar (lambda (symbol)
                       `(:shadowing-import-from ,(package-name (symbol-package symbol)) ,symbol))
                     shadowing-import-list)
           ,@(when documentation
               `((:documentation ,documentation)))
           (:use ,@(mapcar (lambda (package)
                             (if (eql (find-package package)
                                      (find-package :cl))
                                 :cross-cl
                                 package))
                           use-list))
           (:local-nicknames ,@(loop
                                  for (nickname real-name) in package-local-nicknames
                                    collect (list nickname (if (eql (find-package real-name) (find-package :cl))
                                                               :cross-cl
                                                               real-name)))))))

(defun sys.int::round-up (n boundary)
  (if (zerop (rem n boundary))
      n
      (+ n boundary (- (rem n boundary)))))

(defun sys.int::%integer-as-single-float (integer)
  (check-type integer (unsigned-byte 32))
  #+sbcl (sb-kernel:make-single-float (if (logtest integer #x80000000)
                                          (logior integer (lognot #x7FFFFFFF))
                                          integer))
  #-sbcl (error "Not supported on this platform."))

(defun sys.int::%integer-as-double-float (integer)
  (check-type integer (unsigned-byte 64))
  #+sbcl
  (let ((lo (ldb (byte 32 0) integer))
        (hi (ldb (byte 32 32) integer)))
    (sb-kernel:make-double-float
     (if (logtest hi #x80000000)
         (logior hi (lognot #x7FFFFFFF))
         hi)
     lo))
  #-sbcl (error "Not supported on this platform."))

(macrolet ((x (nib int)
             `(progn (defun ,int (vec index) (,nib vec index))
                     (defun (setf ,int) (val vec index) (setf (,nib vec index) val)))))
   (x nibbles:ub16ref/le sys.int::ub16ref/le)
   (x nibbles:ub32ref/le sys.int::ub32ref/le)
   (x nibbles:ub64ref/le sys.int::ub64ref/le))

(defun sys.int::binary-= (x y) (= x y))
(defun sys.int::binary-< (x y) (< x y))
(defun sys.int::binary-<= (x y) (<= x y))
(defun sys.int::binary-> (x y) (> x y))
(defun sys.int::binary->= (x y) (>= x y))
(defun sys.int::binary-+ (x y) (+ x y))
(defun sys.int::binary-- (x y) (- x y))
(defun sys.int::binary-* (x y) (* x y))
(defun sys.int::binary-logand (x y) (logand x y))
(defun sys.int::binary-logeqv (x y) (logeqv x y))
(defun sys.int::binary-logior (x y) (logior x y))
(defun sys.int::binary-logxor (x y) (logxor x y))
(defun mezzano.runtime::%fixnum-< (x y) (< x y))

(defun mezzano.clos:class-precedence-list (class)
  (sb-mop:class-precedence-list class))
