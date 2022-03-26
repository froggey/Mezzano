; -*- mode: lisp -*-

(cl:defpackage :nibbles-system
  (:use :cl))

(cl:in-package :nibbles-system)

(defclass nibbles-source-file (asdf:cl-source-file) ())
(defclass txt-file (asdf:doc-file) ((type :initform "txt")))
(defclass css-file (asdf:doc-file) ((type :initform "css")))

(defmethod asdf:perform :around ((op asdf:compile-op) (c nibbles-source-file))
  (let ((*print-base* 10)               ; INTERN'ing FORMAT'd symbols
        (*print-case* :upcase)
        #+sbcl (sb-ext:*inline-expansion-limit* (max sb-ext:*inline-expansion-limit* 1000))
        #+cmu (ext:*inline-expansion-limit* (max ext:*inline-expansion-limit* 1000)))
    (call-next-method)))

(defmethod asdf:perform :around ((op asdf:load-op) (c nibbles-source-file))
  (call-next-method))

(asdf:defsystem "nibbles"
  :version "0.13"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Sharp Lispers <sharplispers@googlegroups.com>"
  :description "A library for accessing octet-addressed blocks of data in big- and little-endian orders"
  :license "BSD-style (http://opensource.org/licenses/BSD-3-Clause)"
  :default-component-class nibbles-source-file
  :components ((:static-file "README.md")
               (:static-file "LICENSE")
               (:static-file "NEWS")
               (:file "package")
               (:file "types" :depends-on ("package"))
               (:file "macro-utils" :depends-on ("package"))
               (:file "vectors" :depends-on ("types" "macro-utils"))
               (:file "streams" :depends-on ("vectors"))
               (:module "doc"
                        :components ((:html-file "index")
                                     (:txt-file "nibbles-doc")
                                     (:css-file "style")))
               (:module "sbcl-opt"
                        :depends-on ("package" "macro-utils")
                        :components ((:file "fndb")
                                     (:file "nib-tran" :depends-on ("fndb"))
                                     (:file "x86-vm" :depends-on ("fndb"))
                                     (:file "x86-64-vm" :depends-on ("fndb")))))
  :in-order-to ((asdf:test-op (asdf:test-op "nibbles/tests"))))

(asdf:defsystem "nibbles/tests"
  :depends-on ("nibbles" "rt")
  :version "0.1"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Sharp Lispers <sharplispers@googlegroups.com>"
  :components ((:file "tests"))
  :perform (asdf:test-op (operation component)
             (or (uiop:symbol-call '#:rtest '#:do-tests)
                 (error "TEST-OP failed for NIBBLES-TESTS"))))
