(in-package :asdf)

(defsystem "lispos"
  :description "Lisp operating system."
  :version "0"
  :author "Henry Harrington <henry.harrington@gmail.com>"
  :licence "None"
  :depends-on ("lispos-lap" "lispos-compiler"
               #:nibbles #:cl-ppcre #:iterate
               #:alexandria)
  :components ((:file "build-unicode")
               (:file "build-pci-ids")
               (:file "cold/cold-generator"
                :depends-on ("build-unicode"
                             "build-pci-ids"))))
