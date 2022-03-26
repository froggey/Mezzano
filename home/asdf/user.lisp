;;;; ---------------------------------------------------------------------------
;;;; ASDF-USER, where the action happens.

(uiop/package:define-package :asdf/user
  (:nicknames :asdf-user)
  ;; NB: releases before 3.1.2 this :use'd only uiop/package instead of uiop below.
  ;; They also :use'd uiop/common-lisp, that reexports common-lisp and is not included in uiop.
  ;; ASDF3 releases from 2.27 to 2.31 called uiop asdf-driver and asdf/foo uiop/foo.
  ;; ASDF1 and ASDF2 releases (2.26 and earlier) create a temporary package
  ;; that only :use's :cl and :asdf
  (:use :uiop/common-lisp :uiop :asdf/interface))
