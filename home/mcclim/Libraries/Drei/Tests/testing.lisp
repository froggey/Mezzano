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

;; Define some stuff to ease the pain of writing repetitive test
;; cases. Also provide global test-suite and test-running entry point.

(defclass delegating-standard-buffer (delegating-buffer) ()
  (:default-initargs :implementation (make-instance 'standard-buffer)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *buffer-classes* '((standard-buffer)
                                   (delegating-standard-buffer)
                                   (binseq-buffer
                                    persistent-left-sticky-mark
                                    persistent-right-sticky-mark)
                                   (obinseq-buffer
                                    persistent-left-sticky-mark
                                    persistent-right-sticky-mark)
                                   (binseq2-buffer
                                    persistent-left-sticky-line-mark
                                    persistent-right-sticky-line-mark))))

(defmacro buffer-test (name &body body)
  "Define FiveAM tests for all the standard buffer
classes. %%BUFFER in `body' will be substituted for a buffer
class, %%LEFT-STICKY-MARK will be substituted for a
left-sticky-mark class and %%RIGHT-STICKY-MARK will be
substituted for a right sticky mark class."
  (let (result)
    (dolist (class-spec *buffer-classes*)
      (destructuring-bind (buffer &optional (left-sticky-mark 'standard-left-sticky-mark)
                                  (right-sticky-mark 'standard-right-sticky-mark))
          class-spec
        (let ((alist (list (cons '%%buffer `',buffer)
                           (cons '%%left-sticky-mark `',left-sticky-mark)
                           (cons '%%right-sticky-mark `',right-sticky-mark))))
          (push `(test ,(intern (concatenate 'string (symbol-name buffer)
                                             "-" (symbol-name name)))
                   ,@(sublis alist body))
                result))))
    (list* 'progn result)))

(defmacro with-buffer ((buffer &key (initial-contents "")) &body body)
  `(let ((,buffer (make-instance 'drei-buffer :initial-contents ,initial-contents)))
     ,@body))

(defmacro with-view ((view &key (buffer (make-instance 'drei-buffer))
                           (syntax ''drei-fundamental-syntax:fundamental-syntax))
                     &body body)
  (once-only (buffer)
    `(let ((,view (make-instance 'textual-drei-syntax-view
                   :buffer ,buffer)))
      (setf (syntax ,view) (make-syntax-for-view ,view ,syntax))
       ,@body)))

(defun buffer-contents (&optional (buffer (current-buffer)))
  "The contents of `(current-buffer)' as a string."
  (buffer-substring buffer 0 (size buffer)))

(defun buffer-is (string &optional (buffer (current-buffer))
                  (begin-offset 0) (end-offset (size buffer)))
  "Check (using FiveAM) whether `buffer' contains `string' in the
subsequence delimited by `begin-offset' and `end-offset'."
  (is (string= string (buffer-substring buffer begin-offset end-offset))))

(defclass test-drei (drei)
  ()
  (:documentation "An instantiable Drei variant with no
display. Used for testing.")
  (:metaclass modual-class)
  (:default-initargs :no-cursors t))

(defmacro with-drei-environment ((&key (initial-contents "")
                                       (syntax ''drei-fundamental-syntax:fundamental-syntax))
                                 &body body)
  (with-gensyms (buffer view drei)
    `(with-buffer (,buffer :initial-contents ,initial-contents)
       (with-view (,view :buffer ,buffer :syntax ,syntax)
         (let ((,drei (make-instance 'test-drei :view ,view)))
           (with-bound-drei-special-variables (,drei :minibuffer nil)
             ,@body))))))

(def-suite drei-tests :description "The test suite for all Drei
test cases. Has nested test suites for the actual tests.")

(defun run-tests ()
  "Run the Drei test suite. A dot will be printed for each passed
test, a \"f\" for each failed test, a \"X\" for each test that
causes an error, and an \"s\" for each skipped test."
  (run! 'drei-tests))
