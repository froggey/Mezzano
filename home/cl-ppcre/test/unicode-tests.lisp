;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/test/unicode-tests.lisp,v 1.8 2008/07/23 00:17:53 edi Exp $

;;; Copyright (c) 2008, Dr. Edmund Weitz. All rights reserved.

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

(in-package :cl-ppcre-test)

(defun unicode-test (&key (file-name 
                           (make-pathname :name "unicodetestdata"
                                          :type nil :version nil
                                          :defaults *this-file*)
                           file-name-provided-p)
                          verbose)
  "Loops through all test cases in FILE-NAME and prints a report if
VERBOSE is true.  Returns a true value if all tests succeeded.

For the syntax of the tests in FILE-NAME refer to CL-UNICODE."
  (with-open-file (stream file-name)
    (let ((*regex-char-code-limit* (if file-name-provided-p *regex-char-code-limit* char-code-limit))
          (*optimize-char-classes* (if file-name-provided-p *optimize-char-classes* nil))
          ;; we only check for correctness and don't care about speed
          ;; that match (but rather about space constraints of the
          ;; trial versions)
          (*use-bmh-matchers* (if file-name-provided-p *use-bmh-matchers* nil)))
      (do-tests ((format nil "Running Unicode tests in file ~S" (file-namestring file-name))
                 (not verbose))
        (let ((input-line (or (read stream nil) (done)))
              errors)
          (destructuring-bind (char-code property-name expected-result)
              input-line
            (let ((char (and (< char-code char-code-limit) (code-char char-code))))
              (when char
                (when verbose
                  (format t "~&~A: #x~X" property-name char-code))
                (let* ((string (string char))
                       (result-1 (scan (format nil "\\p{~A}" property-name) string))
                       (result-2 (scan (format nil "[\\p{~A}]" property-name) string))
                       (inverted-result-1 (scan (format nil "\\P{~A}" property-name) string))
                       (inverted-result-2 (scan (format nil "[\\P{~A}]" property-name) string)))
                  (unless (eq expected-result (not (not result-1)))
                    (push (format nil "\(code-char #x~X) should ~:[not ~;~]have matched \"\\p{~A}\""
                                  char-code expected-result property-name)
                          errors))
                  (unless (eq expected-result (not (not result-2)))
                    (push (format nil "\(code-char #x~X) should ~:[not ~;~]have matched \"[\\p{~A}]\""
                                  char-code expected-result property-name)
                          errors))
                  (unless (eq expected-result (not inverted-result-1))
                    (push (format nil "\(code-char #x~X) should ~:[~;not ~]have matched \"\\P{~A}\""
                                  char-code expected-result property-name)
                          errors))
                  (unless (eq expected-result (not inverted-result-2))
                    (push (format nil "\(code-char #x~X) should ~:[~;not ~]have matched \"[\\P{~A}]\""
                                  char-code expected-result property-name)
                          errors)))
                errors))))))))
