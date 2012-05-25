(in-package :asdf)

(defsystem "lispos-file"
  :description "File server for lispos."
  :version "0"
  :author "Henry Harrington <henry.harrington@gmail.com>"
  :licence "None"
  :components ((:file "package")
               (:file "server" :depends-on ("package")))
  :depends-on (:iolib :iterate :alexandria :cl-fad))
