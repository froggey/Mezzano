;;; Load the cold-generator on Mezzano.
;;; Expects *DEFAULT-PATHNAME-DEFAULTS* to point at the Mezzano source directory.

(in-package :sys.int)

(setf (fdefinition 'sys.c::resolve-fref) #'function-reference)
(setf (fdefinition 'sys.c::cross-compile-file) #'compile-file)
(deftype sys.c::cross-fref () 'function-reference)
(setf (fdefinition 'sys.c::cross-fref-name) #'function-reference-name)

(require :nibbles)
(require :cl-ppcre)

(cal "tools/build-unicode.lisp")
(cal "tools/cold-generator.lisp")
