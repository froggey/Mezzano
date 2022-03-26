;;;; quicklisp.asd

(defpackage #:ql-info
  (:export #:*version*))

(defvar ql-info:*version*
  (with-open-file (stream (merge-pathnames "version.txt" *load-truename*))
    (read-line stream)))

(asdf:defsystem #:quicklisp
  :description "The Quicklisp client application."
  :author "Zach Beane <zach@quicklisp.org>"
  :license "BSD-style"
  :serial t
  :version #.(remove-if-not #'digit-char-p ql-info:*version*)
  :components ((:file "package")
               (:file "utils")
               (:file "config")
               (:file "impl")
               (:file "impl-util")
               (:file "network")
               (:file "progress")
               (:file "http")
               (:file "deflate")
               (:file "minitar")
               (:file "cdb")
               (:file "dist")
               (:file "setup")
               (:file "client")
               (:file "fetch-gzipped")
               (:file "client-info")
               (:file "client-update")
               (:file "dist-update")
               (:file "misc")
               (:file "local-projects")
               (:file "bundle")))
