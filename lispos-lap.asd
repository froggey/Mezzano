(in-package :asdf)

(defsystem "lispos-lap"
  :description "Assembler for LispOS."
  :version "0"
  :author "Henry Harrington <henry.harrington@gmail.com>"
  :licence "None"
  :components ((:file "lap")
               (:file "lap-x86" :depends-on ("lap"))))
