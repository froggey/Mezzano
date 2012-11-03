(in-package :asdf)

(defsystem "lispos"
  :description "Lisp operating system."
  :version "0"
  :author "Henry Harrington <henry.harrington@gmail.com>"
  :licence "None"
  :depends-on ("lispos-lap")
  :components ((:file "build-unicode")
               (:file "genesis/genesis")
               (:file "genesis/read" :depends-on ("genesis/genesis"))
               (:file "genesis/eval" :depends-on ("genesis/genesis"))
               (:file "genesis/dump" :depends-on ("genesis/genesis"
                                                  "genesis/eval"
                                                  "build-unicode"))))
