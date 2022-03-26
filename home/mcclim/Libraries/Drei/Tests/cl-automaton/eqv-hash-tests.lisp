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

(cl:in-package :drei-tests)

(def-suite eqv-hash-tests :description "The test suite for
CL-AUTOMATON eqv-hash related tests." :in automaton-tests)

(in-suite eqv-hash-tests)

(defclass foo ()
  ((slot1 :initform 0 :initarg :slot1 :type fixnum :accessor slot1)
   (slot2 :initform 0 :initarg :slot2 :type fixnum :accessor slot2)))
(defclass foo-intention (equalp-key-situation) ())
(defparameter +foo-intention+ (make-instance 'foo-intention))
(defmethod eqv ((foo1 foo) (foo2 foo) (s (eql +foo-intention+)))
  (eql (slot1 foo1) (slot1 foo2)))
(defmethod hash ((foo1 foo) (s (eql +foo-intention+)))
  (floor (slot1 foo1) 2))

(test htref.test-1              ; (eqv i1 i2), (= (hash i1) (hash i2))
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 1 :slot2 2))
	(i2 (make-instance 'foo :slot1 1 :slot2 3)))
    (setf (htref ght i1) i1)
    (setf (htref ght i2) i2)
    (is (= (cnt ght) 1))
    (is (eq (htref ght i1) i2))
    (is (htref ght i2) i2)))

(test htref.test-2        ; (not (eqv i1 i2)), (= (hash i1) (hash i2))
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2))
	(i2 (make-instance 'foo :slot1 3)))
    (setf (htref ght i1) i1)
    (setf (htref ght i2) i2)
    (is (= (cnt ght) 2))
    (is (eq (htref ght i1) i1))
    (is (eq (htref ght i2) i2))))

(test htref.test-3       ; (not (eqv i1 i2)), (/= (hash i1) (hash i2))
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2))
	(i2 (make-instance 'foo :slot1 4)))
    (setf (htref ght i1) i1)
    (setf (htref ght i2) i2)
    (is (= (cnt ght) 2))
    (is (eq (htref ght i1) i1))
    (is (eq (htref ght i2) i2))))

(test htref.test-4
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 1 :slot2 2))
	(i2 (make-instance 'foo :slot1 1 :slot2 3)))
    (setf (htref ght i1) i1)
    (is (= (cnt ght) 1))
    (is (eq (htref ght i2) i1))))

(test htref.test-5
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 1 :slot2 2))
	(i2 (make-instance 'foo :slot1 1 :slot2 3)))
    (setf (htref ght i1) i1)
    (multiple-value-bind (v vp)
	(htref ght i2)
      (declare (ignore v))
      (is (= (cnt ght) 1))
      (is-true vp))))

(test htref.test-6
  (let ((ght (make-generalized-hash-table +foo-intention+))
        (i1 (make-instance 'foo :slot1 2))
        (i2 (make-instance 'foo :slot1 3)))
    (setf (htref ght i1) i1)
    (multiple-value-bind (a b)
        (htref ght i2)
      (is-false a)
      (is-false b))))

(test htref.test-7
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2))
	(i2 (make-instance 'foo :slot1 3)))
    (is (eq (setf (htref ght i1) i2) i2))
    (is (= (cnt ght) 1))))

(test htadd.test-1
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2)))
    (is-true (htadd ght i1))
    (is-true (htref ght i1))))

(test htadd.test-2
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2)))
    (multiple-value-bind (a b)
        (htref ght i1)
      (is-false a)
      (is-false b))))

(test htadd.test-3
  (let ((ght (make-generalized-hash-table +foo-intention+))
        (i1 (make-instance 'foo :slot1 2))
        (i2 (make-instance 'foo :slot1 3)))
    (htadd ght i1)
    (multiple-value-bind (a b)
        (htref ght i2)
      (is-false a)
      (is-false b))))

(test htpresent.test-1
  (let ((ght (make-generalized-hash-table +foo-intention+))
        (i1 (make-instance 'foo :slot1 2)))
    (htadd ght i1)
    (is (= (cnt ght) 1))
    (is (htpresent ght i1))))

(test htpresent.test-2
  (let ((ght (make-generalized-hash-table +foo-intention+))
        (i1 (make-instance 'foo :slot1 2)))
    (is (= (cnt ght) 0))
    (is-false (htpresent ght i1))))

(test htremove.test-1
  (let ((ght (make-generalized-hash-table +foo-intention+))
        (i1 (make-instance 'foo :slot1 2)))
    (is-false (htremove ght i1))
    (is (= (cnt ght) 0))
    (is-false (htref ght i1))))

(test htremove.test-2
  (let ((ght (make-generalized-hash-table +foo-intention+))
        (i1 (make-instance 'foo :slot1 2)))
    (htadd ght i1)
    (is-true (htremove ght i1))
    (is-true (= (cnt ght) 0))
    (is-false (htref ght i1))))

(test with-ht.test-1
  (let ((ght (make-generalized-hash-table +foo-intention+))
        (i1 (make-instance 'foo :slot1 2))
        (i2 (make-instance 'foo :slot1 3))
        l)
    (htadd ght i1)
    (htadd ght i2)
    (with-ht (k v) ght
      (push (cons k v) l))
    (is (= (length l) 2))
    (is (equal (assoc i1 l) (cons i1 t)))
    (is (equal (assoc i2 l) (cons i2 t)))))

(test with-ht.test-2
  (let ((ght (make-generalized-hash-table +foo-intention+))
        l)
    (with-ht (k v) ght
      (push (cons k v) l))
    (is-false l)))

(test with-ht-collect.test-1
  (let ((ght (make-generalized-hash-table +foo-intention+))
        (i1 (make-instance 'foo :slot1 2))
        (i2 (make-instance 'foo :slot1 3)))
    (htadd ght i1)
    (htadd ght i2)
    (let ((l (with-ht-collect (k v) ght (cons k v))))
      (is (= (length l) 2))
      (is (equal (assoc i1 l) (cons i1 t)))
      (is (equal (assoc i2 l) (cons i2 t))))))

(test with-ht-collect.test-2
  (let ((ght (make-generalized-hash-table +foo-intention+)))
    (is-false (with-ht-collect (k v) ght (cons k v)))))
