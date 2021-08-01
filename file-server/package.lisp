(cl:defpackage #:file-server
  (:use :cl #+nil :iterate)
  (:export #:spawn-file-server #:kill-server))
