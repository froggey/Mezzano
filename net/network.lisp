;;;; Generic network definitions

(in-package :mezzano.network)

(define-condition network-error (error)
  ())

(deftype octet ()
  '(unsigned-byte 8))

(defun packet-length (packet)
  (reduce '+ (mapcar 'length packet)))

(defun copy-packet (buffer packet &optional (buffer-start 0))
  (dolist (p packet)
    (dotimes (i (length p))
      (setf (aref buffer buffer-start) (aref p i))
      (incf buffer-start))))

(defun buffered-format (stream control-string &rest arguments)
  "Buffered FORMAT."
  (declare (dynamic-extent arguments))
  (write-sequence (apply 'format nil control-string arguments) stream))

(defmacro with-open-network-stream ((var address port) &body body)
  `(with-open-stream (,var (mezzano.network.tcp:tcp-stream-connect ,address ,port))
     ,@body))

(defgeneric local-endpoint (object))
(defgeneric remote-endpoint (object))

;;; Generics for working with packet-oriented connections.

(defgeneric send (sequence connection &key (start 0) end))
(defgeneric receive (connection &key timeout))
(defgeneric disconnect (connection))

;;; High-level address resolution.

(defvar *hosts* '())

(defun resolve-address (address &optional (errorp t))
  (cond ((listp address)
         (mezzano.network.ip:make-ipv4-address address))
        ((stringp address)
         (or
          ;; 1. Try to parse it as an IP address.
          (ignore-errors
            (mezzano.network.ip:make-ipv4-address address))
          ;; 2. Look in the hosts table.
          (let ((host-entry (second (assoc address *hosts* :test 'string-equal))))
            (when host-entry
              (mezzano.network.ip:make-ipv4-address host-entry)))
          ;; 3. Finally do a DNS lookup.
          (ignore-errors
            (mezzano.network.dns:resolve-address address))
          (when errorp
            (error "Unknown host ~S." address))))
        (t address)))

;;; Inteface configuration.

(defgeneric configure-interface (interface configuration-type &key)
  (:documentation "Begin acquiring an IP address/routes/etc for INTERFACE."))
(defgeneric deconfigure-interface (interface configuration-type &key)
  (:documentation "Remove IP addresses/routes/etc associated with INTERFACE.
This is called with the same options CONFIGURE-INTERFACE was originally called with."))

;;; Loopback adapter.

(defclass loopback-interface () ())
