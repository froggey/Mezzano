;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/chartest.lisp,v 1.5 2009/09/17 19:17:30 edi Exp $

;;; Copyright (c) 2008-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(defun create-hash-table-from-test-function (test-function start end)
  "Creates and returns a hash table representing all characters with
character codes between START and END which satisfy TEST-FUNCTION."
  (declare #.*standard-optimize-settings*)
  (loop with hash-table = (make-hash-table)
        for code from start below end
        for char = (code-char code)
        when (and char (funcall test-function char))
        do (setf (gethash char hash-table) t)
        finally (return hash-table)))

(defun create-optimized-test-function (test-function &key
                                                     (start 0)
                                                     (end *regex-char-code-limit*)
                                                     (kind *optimize-char-classes*))
  "Given a unary test function which is applicable to characters
returns a function which yields the same boolean results for all
characters with character codes from START to \(excluding) END.  If
KIND is NIL, TEST-FUNCTION will simply be returned.  Otherwise, KIND
should be one of:

* :HASH-TABLE - builds a hash table representing all characters which
                satisfy the test and returns a closure which checks if
                a character is in that hash table

* :CHARSET - instead of a hash table uses a \"charset\" which is a
             data structure using non-linear hashing and optimized to
             represent \(sparse) sets of characters in a fast and
             space-efficient way \(contributed by Nikodemus Siivola)

* :CHARMAP - instead of a hash table uses a bit vector to represent
             the set of characters

You can also use :HASH-TABLE* or :CHARSET* which are like :HASH-TABLE
and :CHARSET but use the complement of the set if the set contains
more than half of all characters between START and END.  This saves
space but needs an additional pass across all characters to create the
data structure.  There is no corresponding :CHARMAP* kind as the bit
vectors are already created to cover the smallest possible interval
which contains either the set or its complement."
  (declare #.*standard-optimize-settings*)
  (ecase kind
    ((nil) test-function)
    (:charmap
     (let ((charmap (create-charmap-from-test-function test-function start end)))
       (lambda (char)
         (in-charmap-p char charmap))))
    ((:charset :charset*)
     (let ((charset (create-charset-from-test-function test-function start end)))
       (cond ((or (eq kind :charset)
                  (<= (charset-count charset) (ceiling (- end start) 2)))
              (lambda (char)
                (in-charset-p char charset)))
             (t (setq charset (create-charset-from-test-function (complement* test-function)
                                                                 start end))
                (lambda (char)
                  (not (in-charset-p char charset)))))))
    ((:hash-table :hash-table*)
     (let ((hash-table (create-hash-table-from-test-function test-function start end)))
       (cond ((or (eq kind :hash-table)
                  (<= (hash-table-count hash-table) (ceiling (- end start) 2)))
              (lambda (char)
                (gethash char hash-table)))
             (t (setq hash-table (create-hash-table-from-test-function (complement* test-function)
                                                                       start end))
                (lambda (char)
                  (not (gethash char hash-table)))))))))
