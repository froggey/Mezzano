; -*- mode: lisp -*-
(cl:defpackage :chipz-system
  (:use :cl :asdf))
(cl:in-package :chipz-system)

(defclass txt-file (doc-file) ())
(defclass css-file (doc-file) ())

(defmethod source-file-type ((c txt-file) (s module)) "txt")
(defmethod source-file-type ((c css-file) (s module)) "css")

(asdf:defsystem :chipz
  :version "0.7.4"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :description "A library for decompressing deflate, zlib, and gzip data"
  :license "BSD style"
  :components ((:static-file "NEWS")
               (:static-file "LICENSE")
               (:static-file "TODO")
               (:file "package")
               (:module "doc"
                        :components
                        ((:html-file "chipz")
                         ;; XXX ASDF bogosity
                         (:txt-file "chipz-doc")
                         (:css-file "style")))
               (:file "constants" :depends-on ("package"))
               (:file "types-and-tables" :depends-on ("constants"))
               (:file "crc32" :depends-on ("types-and-tables"))
               (:file "adler32" :depends-on ("types-and-tables"))
               (:file "conditions" :depends-on ("package"))
               (:file "dstate" :depends-on ("package"))
               (:file "inflate-state" :depends-on ("dstate" "crc32" "adler32"))
               (:file "gzip" :depends-on ("inflate-state" "conditions"))
               (:file "zlib" :depends-on ("inflate-state" "conditions"))
               (:file "inflate" :depends-on ("inflate-state"
                                             "gzip" "zlib"
                                             "conditions"))
               #-mezzano (:file "bzip2" :depends-on ("dstate" "constants"))
               (:file "decompress" :depends-on ("inflate-state"
                                                "inflate" #-mezzano"bzip2"))
               #+(or sbcl lispworks openmcl cmu allegro)
               (:file "stream" :depends-on ("inflate-state" "inflate"))))
