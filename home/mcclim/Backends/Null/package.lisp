;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :common-lisp-user)

(defpackage :clim-null
  (:use :clim :clim-lisp :clim-backend)
  (:import-from #:climi #:maybe-funcall))
