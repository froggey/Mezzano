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

;;; A naive attempt at implementing the protocol proposed by Robert
;;; Strandh (see eqv-hash.txt).

(in-package :eqv-hash)

;;; Perhaps the term 'intention' is better than 'situation' (as in
;;; extensional vs. intentional equality, see
;;; http://www.htdp.org/2003-09-26/Book/curriculum-Z-H-52.html#node_chap_42).

(defgeneric eqv (object1 object2 situation))

(defgeneric hash (object situation))

(defclass key-situation () ())

(defclass builtin-key-situation (key-situation) ())

(defclass eq-key-situation (builtin-key-situation) ())

(defparameter +eq-key-situation+ (make-instance 'eq-key-situation))

(defclass eql-key-situation (builtin-key-situation) ())

(defparameter +eql-key-situation+ (make-instance 'eql-key-situation))

(defclass equal-key-situation (builtin-key-situation) ())

(defparameter +equal-key-situation+ (make-instance 'equal-key-situation))

(defclass equalp-key-situation (builtin-key-situation) ())

(defparameter +equalp-key-situation+ (make-instance 'equalp-key-situation))

(defclass case-sensitive-key-situation (builtin-key-situation) ())

(defparameter +case-sensitive-key-situation+
  (make-instance 'case-sensitive-key-situation))

(defclass case-insensitive-key-situation (builtin-key-situation) ())

(defparameter +case-insensitive-key-situation+
  (make-instance 'case-insensitive-key-situation))

(defclass generalized-hash-table ()
  ((ht :initform (make-hash-table) :reader ht)
   (cnt :initform 0 :accessor cnt)
   (situation :initarg :situation :reader situation)))

(defun make-generalized-hash-table (situation)
  (make-instance 'generalized-hash-table :situation situation))

(defun htref (table key)
  (let* ((s (situation table))
	 (pair (assoc key (gethash (hash key s) (ht table))
		      :test #'(lambda (o1 o2) (eqv o1 o2 s)))))
    (if pair (values (cdr pair) t) (values nil nil))))

(defun (setf htref) (object table key)
  (let* ((ht (ht table))
	 (s (situation table))
	 (h (hash key s))
	 (p (assoc key (gethash h ht)
		   :test #'(lambda (o1 o2) (eqv o1 o2 s)))))
    (if p
	(progn
	  (rplaca p key)
	  (rplacd p object))
	(progn
	  (push (cons key object) (gethash h ht))
	  (incf (cnt table)))))
  object)

(defun htadd (table key)
  (setf (htref table key) t))

(defun htremove (table key)
  (let* ((ht (ht table))
	 (s (situation table))
	 (h (hash key s))
	 (b (remove key (gethash h ht)
		    :key #'car :test #'(lambda (o1 o2) (eqv o1 o2 s)))))
    (if (eq b (gethash h ht))
	nil
	(progn
	  (decf (cnt table))
	  (if b
	      (setf (gethash h ht) b)
	      (remhash h ht))))))

(defun htpresent (table key)
  (multiple-value-bind (v v-p)
      (htref table key)
    (declare (ignore v))
    v-p))

(defmacro with-ht ((key value) table &body body)
  (let ((bucket (gensym "BUCKET")))
    `(loop for ,bucket being the hash-values of (ht ,table) do
	  (loop for (,key . ,value) in ,bucket do
	       ,@body))))

(defmacro with-ht-collect ((key value) table &body body)
  (let ((bucket (gensym "BUCKET")))
    `(loop for ,bucket being the hash-values of (ht ,table) nconc
	  (loop for (,key . ,value) in ,bucket collect
	       ,@body))))

;; By Bruno Haible:
;; (let ((hashcode-table
;;        (make-hash-table :test #'eq
;; 			:key-type 't :value-type 'fixnum
;; 			:weak :key)))
;;   (defmethod hash (obj (situation (eql +eq-key-situation)))
;;     (or (gethash obj hashcode-table)
;; 	(setf (gethash obj hashcode-table) (random (+ most-positive-fixnum 
;; 						      1))))))