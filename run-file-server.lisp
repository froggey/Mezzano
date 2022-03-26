;;;; Load and start the file server.

#-quicklisp
(error "Quicklisp not found!")

(format t "Loading prerequisites....~%")
#+quicklisp
(ql:quickload '(#-sbcl :iolib #+sbcl :sb-bsd-sockets :iterate :alexandria :cl-fad))
#-quicklisp
(progn
  #-sbcl(require :iolib)
  #+sbcl(require :sb-bsd-sockets)
  (require :iterate)
  (require :alexandria)
  (require :cl-fad))

(format t "Loading file-server...~%")
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :lispos-file)
(format t "Running file-server on port ~D. Use ^C to quit.~%" file-server::*default-file-server-port*)
(file-server::run-file-server)
(quit)
