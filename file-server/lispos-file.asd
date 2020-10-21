(in-package :asdf)

(defsystem "lispos-file"
  :description "File server for lispos."
  :version "0"
  :author "Sylvia Harrington <sylvia.harrington28@gmail.com>"
  :licence "MIT"
  :depends-on (#-sbcl :iolib
               #+sbcl :sb-bsd-sockets
               :iterate :alexandria :cl-fad)
  :serial t
  :components ((:file "package")
               (:file "server")))
