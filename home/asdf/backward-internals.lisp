;;;; -------------------------------------------------------------------------
;;; Internal hacks for backward-compatibility

(uiop/package:define-package :asdf/backward-internals
  (:recycle :asdf/backward-internals :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/find-system)
  (:export #:load-sysdef))
(in-package :asdf/backward-internals)

(with-asdf-deprecation (:style-warning "3.2" :warning "3.4")
  (defun load-sysdef (name pathname)
    (declare (ignore name pathname))
    ;; Needed for backward compatibility with swank-asdf from SLIME 2015-12-01 or older.
    (error "Use asdf:load-asd instead of asdf::load-sysdef")))
