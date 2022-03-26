;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; tests.lisp --- trivial-garbage tests.
;;;
;;; This software is placed in the public domain by Luis Oliveira
;;; <loliveira@common-lisp.net> and is provided with absolutely no
;;; warranty.

(defpackage #:trivial-garbage-tests
  (:use #:cl #:trivial-garbage #:regression-test)
  (:nicknames #:tg-tests)
  (:export #:run))

(in-package #:trivial-garbage-tests)

(defun run ()
  (let ((*package* (find-package :trivial-garbage-tests)))
    (do-tests)
    (null (set-difference (regression-test:pending-tests)
                          rtest::*expected-failures*))))

;;;; Weak Pointers

(deftest pointers.1
    (weak-pointer-p (make-weak-pointer 42))
  t)

(deftest pointers.2
    (weak-pointer-value (make-weak-pointer 42))
  42)

;;;; Weak Hashtables

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sbcl-without-weak-hash-tables-p ()
    (if (and (find :sbcl *features*)
             (not (find-symbol "HASH-TABLE-WEAKNESS" "SB-EXT")))
        '(:and)
        '(:or))))

#+(or corman scl #.(tg-tests::sbcl-without-weak-hash-tables-p))
(progn
  (pushnew 'hashtables.weak-key.1 rt::*expected-failures*)
  (pushnew 'hashtables.weak-key.2 rt::*expected-failures*)
  (pushnew 'hashtables.weak-value.1 rt::*expected-failures*))

(deftest hashtables.weak-key.1
    (let ((ht (make-weak-hash-table :weakness :key)))
      (values (hash-table-p ht)
              (hash-table-weakness ht)))
  t :key)

(deftest hashtables.weak-key.2
    (let ((ht (make-weak-hash-table :weakness :key :test 'eq)))
      (values (hash-table-p ht)
              (hash-table-weakness ht)))
  t :key)

(deftest hashtables.weak-value.1
    (let ((ht (make-weak-hash-table :weakness :value)))
      (values (hash-table-p ht)
              (hash-table-weakness ht)))
  t :value)

(deftest hashtables.not-weak.1
    (hash-table-weakness (make-hash-table))
  nil)

;;;; Finalizers
;;;
;;; These tests are, of course, not very reliable.

(defun dummy (x)
  (declare (ignore x))
  nil)

(defun test-finalizers-aux (count extra-action)
  (let ((cons (list 0))
        (obj (string (gensym))))
    (dotimes (i count)
      (finalize obj (lambda () (incf (car cons)))))
    (when extra-action
      (cancel-finalization obj)
      (when (eq extra-action :add-again)
        (dotimes (i count)
          (finalize obj (lambda () (incf (car cons)))))))
    (setq obj (gensym))
    (setq obj (dummy obj))
    cons))

(defvar *result*)

;;; I don't really understand this, but it seems to work, and stems
;;; from the observation that typing the code in sequence at the REPL
;;; achieves the desired result. Superstition at its best.
(defmacro voodoo (string)
  `(funcall
    (compile nil `(lambda ()
                    (eval (let ((*package* (find-package :tg-tests)))
                            (read-from-string ,,string)))))))

(defun test-finalizers (count &optional remove)
  (gc :full t)
  (voodoo (format nil "(setq *result* (test-finalizers-aux ~S ~S))"
                  count remove))
  (voodoo "(gc :full t)")
  ;; Normally done by a background thread every 0.3 sec:
  #+openmcl (ccl::drain-termination-queue)
  ;; (an alternative is to sleep a bit)
  (voodoo "(car *result*)"))

(deftest finalizers.1
    (test-finalizers 1)
  1)

(deftest finalizers.2
    (test-finalizers 1 t)
  0)

(deftest finalizers.3
    (test-finalizers 5)
  5)

(deftest finalizers.4
    (test-finalizers 5 t)
  0)

(deftest finalizers.5
    (test-finalizers 5 :add-again)
  5)
