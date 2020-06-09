;;; Load the cold-generator on Mezzano.
;;; Expects *DEFAULT-PATHNAME-DEFAULTS* to point at the Mezzano source directory.

(in-package :mezzano.internals)

(defpackage :cross-cl
  (:use :cl)
  (:export . #.(let ((symbols '()))
                 (do-external-symbols (sym :cl symbols)
                   (push sym symbols)))))

(defvar sys.c::*target-architecture*)
(setf (fdefinition 'sys.c::resolve-fref) #'function-reference)
(setf (fdefinition 'sys.c::cross-compile-file) #'compile-file)
(deftype sys.c::cross-fref () 'function-reference)
(setf (fdefinition 'sys.c::cross-fref-name) #'function-reference-name)
(setf (fdefinition 'sys.c::structure-slot-name) #'structure-slot-name)
(setf (fdefinition 'sys.c::structure-slot-type) #'structure-slot-type)
(setf (fdefinition 'sys.c::structure-slot-read-only) #'structure-slot-read-only)

(require :nibbles)
(require :cl-ppcre)

(cal "tools/cold-generator/build-unicode.lisp")
(cal "tools/cold-generator/build-pci-ids.lisp")
(cal "tools/cold-generator/cold-generator.lisp")
(cal "tools/cold-generator/cold-generator-x86-64.lisp")
(cal "tools/cold-generator/cold-generator-arm64.lisp")
