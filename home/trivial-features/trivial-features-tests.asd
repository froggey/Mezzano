;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; trivial-features-tests.asd --- ASDF definition.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(eval-when (:load-toplevel :execute)
  ;; We need to load trivial-features this way before the the
  ;; defsystem form is read for the corner case when someone loads
  ;; trivial-features-tests before trivial-features since the system
  ;; definition's got a #+windows reader conditional that is supplied
  ;; by trivial-features.
  (oos 'load-op 'trivial-features))

(defsystem trivial-features-tests
  :description "Unit tests for TRIVIAL-FEATURES."
  :defsystem-depends-on (cffi-grovel)
  :depends-on (trivial-features rt cffi alexandria)
  :components
  ((:module tests
    :serial t
    :components
    ((:file "package")
     #-windows (:cffi-grovel-file "utsname")
     #+windows (:file "sysinfo")
     (:file "tests")))))

(defmethod perform ((o test-op) (c (eql (find-system 'trivial-features-tests))))
  (let ((*package* (find-package 'trivial-features-tests)))
    (funcall (find-symbol (symbol-name '#:do-tests)))))
