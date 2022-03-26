;;;
;;; Low-level networking implementations
;;;

(in-package #:ql-network)

(definterface host-address (host)
  (:implementation t
    host)
  (:implementation sbcl
    (ql-sbcl:host-ent-address (ql-sbcl:get-host-by-name host))))

(definterface open-connection (host port)
  (:documentation "Open and return a network connection to HOST on the
  given PORT.")
  (:implementation t
    (declare (ignore host port))
    (error "Sorry, quicklisp in implementation ~S is not supported yet."
           (lisp-implementation-type)))
  (:implementation allegro
    (ql-allegro:make-socket :remote-host host
                            :remote-port port))
  (:implementation abcl
    (let ((socket (ql-abcl:make-socket host port)))
      (ql-abcl:get-socket-stream socket :element-type '(unsigned-byte 8))))
  (:implementation ccl
    (ql-ccl:make-socket :remote-host host
                        :remote-port port))
  (:implementation clasp
    (let* ((endpoint (ql-clasp:host-ent-address
                      (ql-clasp:get-host-by-name host)))
           (socket (make-instance 'ql-clasp:inet-socket
                                  :protocol :tcp
                                  :type :stream)))
      (ql-clasp:socket-connect socket endpoint port)
      (ql-clasp:socket-make-stream socket
                                 :element-type '(unsigned-byte 8)
                                 :input t
                                 :output t
                                 :buffering :full)))
  (:implementation clisp
    (ql-clisp:socket-connect port host :element-type '(unsigned-byte 8)))
  (:implementation cmucl
    (let ((fd (ql-cmucl:connect-to-inet-socket host port)))
      (ql-cmucl:make-fd-stream fd
                               :element-type '(unsigned-byte 8)
                               :binary-stream-p t
                               :input t
                               :output t)))
  (:implementation scl
    (let ((fd (ql-scl:connect-to-inet-socket host port)))
      (ql-scl:make-fd-stream fd
			     :element-type '(unsigned-byte 8)
			     :input t
			     :output t)))
  (:implementation ecl
    (let* ((endpoint (ql-ecl:host-ent-address
                      (ql-ecl:get-host-by-name host)))
           (socket (make-instance 'ql-ecl:inet-socket
                                  :protocol :tcp
                                  :type :stream)))
      (ql-ecl:socket-connect socket endpoint port)
      (ql-ecl:socket-make-stream socket
                                 :element-type '(unsigned-byte 8)
                                 :input t
                                 :output t
                                 :buffering :full)))
  (:implementation mezzano
    (ql-mezzano:tcp-stream-connect host port
                                   :element-type '(unsigned-byte 8)))
  (:implementation mkcl
    (let* ((endpoint (ql-mkcl:host-ent-address
                      (ql-mkcl:get-host-by-name host)))
           (socket (make-instance 'ql-mkcl:inet-socket
                                  :protocol :tcp
                                  :type :stream)))
      (ql-mkcl:socket-connect socket endpoint port)
      (ql-mkcl:socket-make-stream socket
                                   :element-type '(unsigned-byte 8)
                                   :input t
                                   :output t
                                   :buffering :full)))
  (:implementation lispworks
    (ql-lispworks:open-tcp-stream host port
                                  :direction :io
                                  :errorp t
                                  :read-timeout nil
                                  :element-type '(unsigned-byte 8)
                                  :timeout 5))
  (:implementation sbcl
    (let* ((endpoint (ql-sbcl:host-ent-address
                      (ql-sbcl:get-host-by-name host)))
           (socket (make-instance 'ql-sbcl:inet-socket
                                  :protocol :tcp
                                  :type :stream)))
      (ql-sbcl:socket-connect socket endpoint port)
      (ql-sbcl:socket-make-stream socket
                                  :element-type '(unsigned-byte 8)
                                  :input t
                                  :output t
                                  :buffering :full))))

(definterface read-octets (buffer connection)
  (:documentation "Read from CONNECTION into BUFFER. Returns the
  number of octets read.")
  (:implementation t
    (read-sequence buffer connection))
  (:implementation allegro
    (ql-allegro:read-vector buffer connection))
  (:implementation clisp
    (ql-clisp:read-byte-sequence buffer connection
                                  :no-hang nil
                                  :interactive t)))

(definterface write-octets (buffer connection)
  (:documentation "Write the contents of BUFFER to CONNECTION.")
  (:implementation t
    (write-sequence buffer connection)
    (finish-output connection)))

(definterface close-connection (connection)
  (:implementation t
    (ignore-errors (close connection))))

(definterface call-with-connection (host port fun)
  (:documentation "Establish a network connection to HOST on PORT and
  call FUN with that connection as the only argument. Unconditionally
  closes the connection afterwareds via CLOSE-CONNECTION in an
  unwind-protect. See also WITH-CONNECTION.")
  (:implementation t
    (let (connection)
      (unwind-protect
           (progn
             (setf connection (open-connection host port))
             (funcall fun connection))
        (when connection
          (close-connection connection))))))

(defmacro with-connection ((connection host port) &body body)
  `(call-with-connection ,host ,port (lambda (,connection) ,@body)))
