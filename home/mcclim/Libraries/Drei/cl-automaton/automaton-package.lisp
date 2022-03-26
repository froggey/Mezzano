;;; -*- Mode: Lisp; Package: AUTOMATON -*-
;;;
;;;  (c) copyright 2005-2007 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;
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

(defpackage #:eqv-hash
  (:use :cl)
  (:export
   #:hash
   #:eqv
   #:key-situation
   #:builtin-key-situation
   #:eq-key-situation
   #:+eq-key-situation+
   #:eql-key-situation
   #:+eql-key-situation+
   #:equal-key-situation
   #:+equal-key-situation+
   #:equalp-key-situation
   #:+equalp-key-situation+
   #:case-sensitive-key-situation
   #:+case-sensitive-key-situation+
   #:case-insensitive-key-situation
   #:+case-insensitive-key-situation+
   #:make-generalized-hash-table
   #:generalized-hash-table
   #:ht
   #:cnt
   #:situation
   #:htref
   #:htadd
   #:htremove
   #:htpresent
   #:with-ht
   #:with-ht-collect))
(defpackage #:automaton
  (:nicknames #:cl-automaton)
  (:use :cl #:eqv-hash)
  (:export
   #:string-regexp #:regexp-automaton
   #:run #:run-to-first-match #:run-to-first-unmatch
   #:state-equal #:automaton-equal #:regexp-equal))