;;;; The CL package system.
;;;; This file must be loaded last during system bootstrapping.

(in-package #:sys.int)

(declaim (special *package*))

(defvar *package-list* '()
  "The package registry.")
(defvar *keyword-package* nil
  "The keyword package.")

(defstruct (package
	     (:constructor %make-package (name nicknames))
	     (:predicate packagep))
  name
  nicknames
  use-list
  used-by-list
  internal-symbols
  external-symbols)

(defun list-all-packages ()
  (remove-duplicates (mapcar 'cdr *package-list*)))

(defmacro in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *package* (find-package-or-die ',name))))

(defun find-package (name)
  (if (packagep name)
      name
      (cdr (assoc (string name) *package-list* :test 'string=))))

(defun find-package-or-die (name)
  (or (find-package name)
      (error "No package named ~S." name)))

(defun use-one-package (package-to-use package)
  (when (eql package-to-use *keyword-package*)
    (error "Cannot use the KEYWORD package."))
  (pushnew package-to-use (package-use-list package))
  (pushnew package (package-used-by-list package-to-use)))

(defun use-package (packages-to-use &optional (package *package*))
  (let ((p (find-package package)))
    (if (listp packages-to-use)
	(dolist (use-package packages-to-use)
	  (use-one-package (find-package use-package) p))
	(use-one-package (find-package packages-to-use) p)))
  t)

(defun make-package (package-name &key nicknames use)
  (when (find-package package-name)
    (error "A package named ~S already exists." package-name))
  (dolist (n nicknames)
    (when (find-package n)
      (error "A package named ~S already exists." n)))
  (let ((use-list (mapcar 'find-package use))
	(package (%make-package (simplify-string (string package-name))
				(mapcar (lambda (x) (simplify-string (string x))) nicknames))))
    (setf (package-internal-symbols package) (make-hash-table :test 'equal)
          (package-external-symbols package) (make-hash-table :test 'equal))
    ;; Use packages.
    (use-package use-list package)
    ;; Install the package in the package registry.
    (push (cons (package-name package) package) *package-list*)
    (dolist (s (package-nicknames package))
      (push (cons s package) *package-list*))
    package))

(defun find-symbol (string &optional (package *package*))
  (let ((p (find-package-or-die package)))
    (multiple-value-bind (sym present-p)
        (gethash string (package-internal-symbols p))
      (when present-p
	(return-from find-symbol (values sym :internal))))
    (multiple-value-bind (sym present-p)
        (gethash string (package-external-symbols p))
      (when present-p
	(return-from find-symbol (values sym :external))))
    (dolist (pak (package-use-list p)
	     (values nil nil))
      (multiple-value-bind (symbol status)
	  (find-symbol string pak)
	(when (or (eq status :external)
		  (eq status :inherited))
	  (return (values symbol :inherited)))))))

(defun import-one-symbol (symbol package)
  ;; Check for a conflicting symbol.
  (multiple-value-bind (existing-symbol existing-mode)
      (find-symbol (symbol-name symbol) package)
    (when (and existing-mode
	       (not (eq existing-symbol symbol)))
      (ecase existing-mode
	(:inherited
	 ;; TODO: Restarts shadow-symbol and don't import.
	 (error "Newly imported symbol ~S conflicts with inherited symbol ~S." symbol existing-symbol))
	((:internal :external)
	 ;; TODO: Restarts unintern-old-symbol and don't import.
	 (error "Newly imported symbol ~S conflicts with present symbol ~S." symbol existing-symbol))))
    (unless (eql symbol (gethash (symbol-name symbol) (package-external-symbols package)))
      (unless (eql symbol (gethash (symbol-name symbol) (package-internal-symbols package)))
        (setf (gethash (symbol-name symbol) (package-internal-symbols package)) symbol)))
    (unless (symbol-package symbol)
      (setf (symbol-package symbol) package))))

(defun import (symbols &optional (package *package*))
  (let ((p (find-package-or-die package)))
    (if (listp symbols)
	(dolist (s symbols)
	  (import-one-symbol s p))
	(import-one-symbol symbols p)))
  t)

(defun export-one-symbol (symbol package)
  (dolist (q (package-used-by-list package))
    (multiple-value-bind (other-symbol status)
	(find-symbol (symbol-name symbol) q)
      (when (and (not (eq symbol other-symbol))
		 status)
	;; TODO: Restart replace-symbol.
	(error "Newly exported symbol ~S conflicts with symbol ~S in package ~S." symbol other-symbol q))))
  (import-one-symbol symbol package)
  ;; Remove from the internal-symbols list.
  (remhash (symbol-name symbol) (package-internal-symbols package))
  ;; And add to the external-symbols list.
  (setf (gethash (symbol-name symbol) (package-external-symbols package)) symbol))

(defun export (symbols &optional (package *package*))
  (let ((p (find-package-or-die package)))
    (if (listp symbols)
	(dolist (s symbols)
	  (export-one-symbol s p))
	(export-one-symbol symbols p)))
  t)

;; Function FIND-ALL-SYMBOLS
;; Function RENAME-PACKAGE
;; Function SHADOW
;; Function SHADOWING-IMPORT
;; Function DELETE-PACKAGE
;; Macro WITH-PACKAGE-ITERATOR
;; Function UNEXPORT
;; Function UNINTERN
;; Function UNUSE-PACKAGE
;; Macro DO-SYMBOLS, DO-EXTERNAL-SYMBOLS, DO-ALL-SYMBOLS
;; Function PACKAGE-SHADOWING-SYMBOLS
;; Condition Type PACKAGE-ERROR
;; Function PACKAGE-ERROR-PACKAGE

;;; This is the intern function, different name so it doesn't conflict with cold-intern.
(defun package-intern (name &optional (package *package*))
  (let ((p (find-package-or-die package)))
    (multiple-value-bind (symbol status)
        (find-symbol name p)
      (when status
        (return-from package-intern (values symbol status))))
    (let ((symbol (make-symbol name)))
      (import (list symbol) p)
      (when (eql p *keyword-package*)
        ;; TODO: Constantness.
        (proclaim `(constant ,symbol))
        (setf (symbol-value symbol) symbol)
        (export (list symbol) p))
      (values symbol nil))))

(defun delete-package (package)
  (let ((p (find-package-or-die package)))
    (when (package-used-by-list p)
      (error "Package ~S is in use." package))
    ;; Remove the package from the use list.
    (dolist (other (package-use-list p))
      (setf (package-used-by-list other) (remove p (package-used-by-list other))))
    ;; Remove all symbols.
    (maphash (lambda (name symbol)
               (when (eq (symbol-package symbol) package)
                 (setf (symbol-package symbol) nil)))
             (package-internal-symbols p))
    (maphash (lambda (name symbol)
               (when (eq (symbol-package symbol) package)
                 (setf (symbol-package symbol) nil)))
             (package-external-symbols p))
    (setf (package-name p) nil)
    (setf *package-list* (remove p *package-list* :key 'cdr))
    t))

(defun keywordp (object)
  (and (symbolp object)
       (eq (symbol-package object) *keyword-package*)))

;;; TODO: shadowing symbols.
(defun %defpackage (name nicknames documentation use-list import-list export-list intern-list)
  (let ((p (or (find-package name)
	       (make-package name :nicknames nicknames))))
    (use-package use-list p)
    (import import-list p)
    (dolist (s intern-list)
      (intern s p))
    (dolist (s export-list)
      (export-one-symbol (intern (string s) p) p))
    p))

(defmacro defpackage (defined-package-name &rest options)
  (let ((nicknames '())
	(documentation nil)
	(use-list '())
	(import-list '())
	(export-list '())
	(intern-list '()))
    (dolist (o options)
      (ecase (first o)
	(:nicknames
	 (dolist (n (rest o))
	   (pushnew (string n) nicknames)))
	(:documentation
	 (when documentation
	   (error "Multiple documentation options in DEFPACKAGE form."))
	 (unless (or (eql 2 (length o))
		     (not (stringp (second o))))
	   (error "Invalid documentation option in DEFPACKAGE form."))
	 (setf documentation (second o)))
	(:use
	 (dolist (u (rest o))
	   (if (packagep u)
	       (pushnew u use-list)
	       (pushnew (string u) use-list))))
	(:import-from
	 (let ((package (find-package-or-die (second o))))
	   (dolist (name (cddr o))
	     (multiple-value-bind (symbol status)
		 (find-symbol (string name) package)
	       (unless status
		 (error "No such symbol ~S in package ~S." (string name) package))
	       (pushnew symbol import-list)))))
	(:export
	 (dolist (name (cdr o))
	   (pushnew name export-list)))
	(:intern
	 (dolist (name (cdr o))
	   (pushnew name intern-list)))
	(:size)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%defpackage ,(string defined-package-name)
		    ',nicknames
		    ',documentation
		    ',use-list
		    ',import-list
		    ',export-list
		    ',intern-list))))

(defun rename-package (package new-name &optional new-nicknames)
  (setf package (find-package-or-die package))
  (when (packagep new-name) (error "Not sure what to do when NEW-NAME is a package?"))
  (setf new-name (string new-name))
  (setf new-nicknames (mapcar 'string new-nicknames))
  (let ((new-names (remove-duplicates (cons new-name new-nicknames)))
        (old-names (cons (package-name package) (package-nicknames package))))
    (dolist (name new-names)
      (when (and (find-package name)
                 (not (eql (find-package name) package)))
        (error "New name ~S for package ~S conflicts with existing package ~S.~%"
               name package (find-package name))))
    ;; Remove the old names.
    (setf *package-list* (remove-if (lambda (name)
                                      (member name old-names :test 'string=))
                                    *package-list*
                                    :key 'car))
    ;; Add the new names.
    (dolist (name new-names)
      (push (cons name package) *package-list*))
    ;; Rename the package
    (setf (package-name package) new-name
          (package-nicknames package) new-nicknames))
  package)

(defmacro do-external-symbols ((var &optional (package '*package*) result-form) &body body)
  (let ((name-sym (gensym "name")))
    `(block nil
       (maphash (lambda (,name-sym ,var)
                  (declare (ignore ,name-sym))
                  ,@body)
                (package-external-symbols (find-package-or-die ,package)))
       ,result-form)))

(defun shadow (symbol-names &optional (package *package*))
  (unless (listp symbol-names)
    (setf symbol-names (list symbol-names)))
  (setf package (find-package-or-die package))
  (dolist (symbol symbol-names)
    (shadow-one-symbol symbol package))
  t)

(defun initialize-package-system ()
  (write-line "Initializing package system.")
  ;; Create the core packages.
  (setf *package-list* '())
  (setf *keyword-package* (make-package "KEYWORD"))
  (make-package "COMMON-LISP" :nicknames '("CL"))
  (make-package "SYSTEM" :nicknames '("SYS") :use '("CL"))
  (make-package "SYSTEM.INTERNALS" :nicknames '("SYS.INT") :use '("CL" "SYS"))
  (make-package "COMMON-LISP-USER" :nicknames '("CL-USER") :use '("CL"))
  (setf *package* (find-package-or-die "SYSTEM.INTERNALS"))
  (locally (declare (special *cl-symbols* *system-symbols*))
    (let ((cl-package (find-package "CL")))
      ;; Now import all the symbols.
      ;; TODO: Intern into the correct package.
      (dotimes (i (length *initial-obarray*))
        (let ((sym (aref *initial-obarray* i)))
          (setf (symbol-package sym) nil)
          (let ((package (cond ((find (symbol-name sym) *cl-symbols* :test 'string=) cl-package)
                               ((find (symbol-name sym) *system-symbols* :test 'string=) (find-package "SYSTEM")))))
            (cond (package (import-one-symbol sym package)
                           (export-one-symbol sym package))
                  (t (import-one-symbol sym *package*))))))
      ;; Export all CL symbols.
      (dotimes (i (length *cl-symbols*))
        (export (list (package-intern (aref *cl-symbols* i) cl-package))
                cl-package))
      ;; And export all the SYSTEM symbols.
      (dotimes (i (length *system-symbols*))
        (export (list (package-intern (aref *system-symbols* i) "SYSTEM"))
                "SYSTEM"))))
  (dotimes (i (length *initial-keyword-obarray*))
    (let ((sym (aref *initial-keyword-obarray* i)))
      (setf (symbol-package sym) nil)
      (import-one-symbol sym *keyword-package*)
      (export-one-symbol sym *keyword-package*)))
  (setf (symbol-function 'intern) #'package-intern)
  (values))
