;;; -*- Mode: Lisp; Package: User -*-

(in-package :common-lisp-user)

;;; Needed to keep ACL from issuing warnings about toplevel (shadow ...) forms
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :loop))
