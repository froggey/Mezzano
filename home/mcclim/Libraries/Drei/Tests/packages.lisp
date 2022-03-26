;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; ASDF system definition for Drei's test suite. We use the excellent
;;; FiveAM test framework.

(in-package :common-lisp-user)

(defpackage :drei-tests
  (:use :clim-lisp :it.bese.fiveam :drei-buffer :drei-base :drei-motion
        :drei-editing :automaton :eqv-hash :drei-core :drei-kill-ring
        :drei-syntax :drei :esa :esa-utils :clim :drei-lisp-syntax :drei-undo)
  (:shadowing-import-from :it.bese.fiveam #:test)
  (:shadowing-import-from :automaton #:run)
  (:shadowing-import-from :drei-lisp-syntax #:form)
  (:export #:run-tests
           #:*run-self-compilation-test*))
