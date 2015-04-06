;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :asdf)

(defsystem "lispos-file"
  :description "File server for lispos."
  :version "0"
  :author "Henry Harrington <henry.harrington@gmail.com>"
  :licence "MIT"
  :components ((:file "package")
               (:file "server" :depends-on ("package")))
  :depends-on (#-sbcl :iolib #+sbcl :sb-bsd-sockets :iterate :alexandria :cl-fad))
