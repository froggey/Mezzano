;;;; UDP support

(in-package :mezzano.network.udp)

(defvar *udp-connections* nil)
(defvar *udp-connection-lock* (mezzano.supervisor:make-mutex "UDP connection list"))
(defvar *allocated-udp-ports* nil)

(defun allocate-local-udp-port ()
  (mezzano.supervisor:with-mutex (*udp-connection-lock*)
    (do ()
        (nil)
      (let ((port (+ (random 32768) 32768)))
        (unless (find port *allocated-udp-ports*)
          (push port *allocated-udp-ports*)
          (return port))))))

(defclass udp4-connection ()
  ((remote-address :initarg :remote-address :reader remote-address)
   (remote-port :initarg :remote-port :reader remote-port)
   (local-address :initarg :local-address :reader local-address)
   (local-port :initarg :local-port :reader local-port)
   (packets :initarg :packets :accessor udp-connection-packets)
   (lock :initform (mezzano.supervisor:make-mutex "UDP connection lock")
         :reader udp-connection-lock)
   (cvar :initform (mezzano.supervisor:make-condition-variable "UDP connection cvar")
         :reader udp-connection-cvar))
  (:default-initargs :packets '()))

(defmacro with-udp-connection-locked ((connection) &body body)
  `(mezzano.supervisor:with-mutex ((udp-connection-lock ,connection))
     ,@body))

(defun get-udp-connection (remote-ip remote-port local-ip local-port)
  (mezzano.supervisor:with-mutex (*udp-connection-lock*)
    (dolist (connection *udp-connections*)
      (when (or (and  (mezzano.network.ip:address-equal
		       mezzano.network.ip:+ipv4-broadcast-local-network+
		       local-ip)
		      (eql (local-port connection) local-port))
		(and (mezzano.network.ip:address-equal
		      (remote-address connection)
		      remote-ip)
		     (eql (remote-port connection) remote-port)
		     (mezzano.network.ip:address-equal
		      (local-address connection)
		      local-ip)
		     (eql (local-port connection) local-port)))
        (return connection)))))

(defmacro with-udp-connection ((connection remote-host remote-port) &body body)
  `(call-with-udp-connection ,remote-host ,remote-port (lambda (,connection) ,@body)))

(defun call-with-udp-connection (remote-host remote-port fn)
  (let ((connection nil))
    (unwind-protect
         (progn (setf connection (udp4-connect remote-host remote-port))
                (funcall fn connection))
      (when connection
        (disconnect connection)))))

(defun udp4-connect (remote-host remote-port)
  (let ((remote-address (net:resolve-address remote-host)))
    (multiple-value-bind (host interface)
        (mezzano.network.ip:ipv4-route remote-address)
      (declare (ignore host))
      (let* ((source-port (allocate-local-udp-port))
             (source-address (mezzano.network.ip:ipv4-interface-address interface))
             (connection (make-instance 'udp4-connection
                                        :remote-address remote-address
                                        :remote-port remote-port
                                        :local-address source-address
                                        :local-port source-port)))
        (mezzano.supervisor:with-mutex (*udp-connection-lock*)
          (push connection *udp-connections*))
        connection))))

(defmethod disconnect ((connection udp4-connection))
  (mezzano.supervisor:with-mutex (*udp-connection-lock*)
    (setf *udp-connections* (remove connection *udp-connections*))
    (setf *allocated-udp-ports* (remove (local-port connection) *allocated-udp-ports*))))

(defmethod send (sequence (connection udp4-connection) &key (start 0) end)
  (let* ((source (local-address connection))
         (source-port (local-port connection))
         (destination (remote-address connection))
         (destination-port (remote-port connection))
         (header (make-array 8 :element-type '(unsigned-byte 8)))
         (packet (list header (subseq sequence start end))))
    (setf (ub16ref/be header 0) source-port
          (ub16ref/be header 2) destination-port
          (ub16ref/be header 4) (net:packet-length packet)
          (ub16ref/be header 6) 0)
    (mezzano.network.ip:transmit-ipv4-packet source destination
                                             mezzano.network.ip:+ip-protocol-udp+
                                             packet)))

(defmethod receive ((connection udp4-connection) &key timeout)
  (with-udp-connection-locked (connection)
    (mezzano.supervisor:condition-wait-for ((udp-connection-cvar connection)
                                            (udp-connection-lock connection)
                                            timeout)
      (pop (udp-connection-packets connection)))))

(defmethod mezzano.network.ip:ipv4-receive ((protocol (eql mezzano.network.ip:+ip-protocol-udp+)) packet local-ip remote-ip start end)
  (let* ((remote-port (ub16ref/be packet start))
         (local-port (ub16ref/be packet (+ start 2)))
         (length (ub16ref/be packet (+ start 4)))
         (checksum (ub16ref/be packet (+ start 6)))
         (connection (get-udp-connection remote-ip remote-port local-ip local-port)))
    (when connection
      (with-udp-connection-locked (connection)
        (let ((payload (make-array (- end (+ start 8)) :displaced-to packet :displaced-index-offset (+ start 8))))
          ;; Send data to the user layer
          (setf (udp-connection-packets connection) (append (udp-connection-packets connection)
                                                            (list payload)))
          (mezzano.supervisor:condition-notify (udp-connection-cvar connection) t))))))

(defmethod mezzano.network:local-endpoint ((object udp4-connection))
  (values (local-address object)
          (local-port object)))

(defmethod mezzano.network:remote-endpoint ((object udp4-connection))
  (values (remote-address object)
          (remote-port object)))

(defmethod print-object ((instance udp4-connection) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (multiple-value-bind (local-address local-port)
        (mezzano.network:local-endpoint instance)
      (multiple-value-bind (remote-address remote-port)
          (mezzano.network:remote-endpoint instance)
        (format stream ":Local ~A:~A :Remote ~A:~A"
                local-address local-port
                remote-address remote-port)))))
