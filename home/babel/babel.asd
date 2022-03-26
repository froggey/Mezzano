;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; babel.asd --- ASDF system definition for Babel.
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

(defsystem babel
  :description "Babel, a charset conversion library."
  :author "Luis Oliveira <loliveira@common-lisp.net>"
  :licence "MIT"
  :depends-on (trivial-features alexandria)
  :components
  ((:module src
    :serial t
    :components
    ((:file "packages")
     (:file "encodings")
     (:file "enc-ascii")
     (:file "enc-ebcdic")
     (:file "enc-ebcdic-int")
     (:file "enc-iso-8859")
     (:file "enc-unicode")
     (:file "enc-cp1251")
     (:file "enc-cp1252")
     (:file "jpn-table")
     (:file "enc-jpn")
     (:file "enc-gbk")
     (:file "enc-koi8")
     (:file "external-format")
     (:file "strings")
     (:file "gbk-map")
     (:file "sharp-backslash")))))

(defmethod perform ((o test-op) (c (eql (find-system :babel))))
  (operate 'load-op :babel-tests)
  (operate 'test-op :babel-tests))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :babel))))
  nil)
