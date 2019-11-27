;;;; Copyright (c) 2017 Eugene Zaikonnikov <eugene@funcall.org>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.network.dhcp)

(defconstant +magic-cookie+ #x63825363)

(defconstant +dhcp-client-port+ 68)
(defconstant +dhcp-server-port+ 67)

(defconstant +dhcp-discover+ 1)
(defconstant +dhcp-offer+ 2)
(defconstant +dhcp-request+ 3)
(defconstant +dhcp-decline+ 4)
(defconstant +dhcp-ack+ 5)
(defconstant +dhcp-nak+ 6)
(defconstant +dhcp-release+ 7)
(defconstant +dhcp-inform+ 8)

(defconstant +opt-netmask+ 1)
(defconstant +opt-router+ 3)
(defconstant +opt-ntp-servers+ 42)
(defconstant +opt-dns-servers+ 6)
(defconstant +opt-host-name+ 12)
(defconstant +opt-domain-name+ 15)
(defconstant +opt-ip-address+ 50)
(defconstant +opt-lease-time+ 51)
(defconstant +opt-dhcp-message-type+ 53)
(defconstant +opt-dhcp-server+ 54)
(defconstant +opt-parameter-request-list+ 55)
(defconstant +opt-tftp-server+ 66)
(defconstant +opt-custom-mezzano-server+ 212)
(defconstant +opt-end+ 255)

(define-condition dhcp-error ()
  ())

(define-condition dhcp-invalid-option (dhcp-error)
  ((type :reader option-type :initarg :type)
   (value :reader option-value :initarg :value))
  (:report (lambda (condition stream)
	     (format stream "Invalid DHCP option: ~A ~A"
                     (option-type condition) (option-value condition)))))

(defclass dhcp-lease ()
  ((ip-address :reader ip-address :initarg :ip-address)
   (netmask :reader netmask :initarg :netmask)
   (gateway :reader gateway :initarg :gateway)
   (interface :reader interface :initarg :interface)
   (ntp-servers :reader ntp-servers :initarg :ntp-servers)
   (dns-servers :reader dns-servers :initarg :dns-servers)
   (dhcp-server :reader dhcp-server :initarg :dhcp-server)
   (mezzano-server :reader mezzano-server :initarg :mezzano-server)
   (lease-timeout :accessor lease-timeout :initarg :lease-timeout)
   (lease-timestamp :accessor lease-timestamp :initarg :lease-timestamp)
   (xid :reader xid :initarg :xid)))

(defun convert-to-ipv4-address (vector)
  (mezzano.network.ip:make-ipv4-address (ub32ref/be vector 0)))

(defun build-dhcp-packet (&key xid mac-address options (siaddr 0) (ciaddr 0) (broadcast t))
  (assert (typep mac-address '(simple-array (unsigned-byte 8) (6))))
  (let ((packet (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref packet 0) #x01 ;OP
          (aref packet 1) #x01 ;HTYPE
          (aref packet 2) #x06 ;HLEN
          (aref packet 3) #x00 ;HOPS
          (ub32ref/be packet 4) xid
	  (ub16ref/be packet 8) #x0000 ;SECS
	  (ub16ref/be packet 10) (if broadcast #x8000 0) ;FLAGS
          (ub32ref/be packet 12) ciaddr
          (ub32ref/be packet 16) #x00000000 ;YIADDR
          (ub32ref/be packet 20) siaddr
          (ub32ref/be packet 24) #x00000000 ;GIADDR
	  (subseq packet 28) mac-address
	  (ub32ref/be packet 236) +magic-cookie+)
    (loop for pos = 240 then (+ pos (length option))
       for option in options
       do (setf (subseq packet pos) option)
       finally (setf (aref packet pos) +opt-end+))
    packet))

(defun decode-dhcp-xid (buffer)
  (ub32ref/be buffer 4))

(defun decode-dhcp-option (buffer position)
  "Returns (VALUES type value next-option-position)"
  (if (zerop (aref buffer position))
      (values 0 nil (1+ position))
      (values (aref buffer position)
	      (subseq buffer (+ position 2) (+ (+ position 2) (aref buffer (1+ position))))
	      (+ position 2 (aref buffer (1+ position))))))

(defun decode-all-options (buffer)
  (loop with pos = 240
     with padcount = 0
     while (and (not (= (aref buffer pos) +opt-end+)) (< padcount 4) (< pos (length buffer)))
     appending (multiple-value-bind (type value newpos) (decode-dhcp-option buffer pos)
		 (setf pos newpos)
		 (cond ((zerop type) (incf padcount) (list))
		       (t (setf padcount 0) (list (cons type value)))))))

(defun make-dhcp-option (type value)
  (let* ((size (cond ((zerop type) -1)
		     ((arrayp value) (length value))
		     ((typep value '(unsigned-byte 8)) 1)
		     ((typep value '(unsigned-byte 16)) 2)
		     ((typep value '(unsigned-byte 32)) 4)
		     (t (error 'dhcp-invalid-option :type type :value value))))
	 (option (make-array (+ 2 size) :element-type '(unsigned-byte 8) :initial-element 0)))
    (when (minusp size)
      (return-from make-dhcp-option option))
    (setf (aref option 0) type
	  (aref option 1) size)
    (setf (subseq option 2)
	  (make-array size :element-type '(unsigned-byte 8)
		      :initial-contents (cond ((arrayp value) value)
					      ((= 1 size) (list value))
					      ((= 2 size) (list (ash value -8) (logand value #xff)))
					      ((= 4 size) (list (ldb (byte 8 24) value) (ldb (byte 8 16) value)
								(ldb (byte 8 8) value) (logand value #xff)))
					      (t (error 'dhcp-invalid-option :type type :value value)))))
    option))

(defun send-broadcast-dhcp-packet (sequence interface)
  (let* ((header (make-array 8 :element-type '(unsigned-byte 8)))
         (packet (list header sequence)))
    (setf (ub16ref/be header 0) +dhcp-client-port+
          (ub16ref/be header 2) +dhcp-server-port+
          (ub16ref/be header 4) (net:packet-length packet)
          (ub16ref/be header 6) 0)
    (mezzano.network.ethernet:transmit-ethernet-packet
     interface mezzano.network.ethernet:*ethernet-broadcast* mezzano.network.ethernet:+ethertype-ipv4+
     (mezzano.network.ip::assemble-ipv4-packet mezzano.network.ip:+ipv4-broadcast-source+
                                               mezzano.network.ip:+ipv4-broadcast-local-network+
                                               mezzano.network.ip:+ip-protocol-udp+
                                               packet))))

(defun dhcp-send (iface options xid &key (siaddr 0))
  (let* ((packet (build-dhcp-packet :xid xid :mac-address (mezzano.network.ethernet:ethernet-mac iface) :options options :siaddr siaddr)))
    (send-broadcast-dhcp-packet packet iface)))

(defun get-option (alist option)
  (cdr (find option alist :key #'car)))

(defun make-xid ()
  (+ #xdeadbeef (- #x8000 (random #xffff))))

(defun acquire-lease (interface)
  (let ((connection (make-instance 'mezzano.network.udp::udp4-connection
				   :remote-address mezzano.network.ip:+ipv4-broadcast-source+ ;;unnecessary, but just to avoid the stack down choking
				   :remote-port +dhcp-server-port+
				   :local-address mezzano.network.ip:+ipv4-broadcast-local-network+
				   :local-port +dhcp-client-port+))
	(xid (make-xid)))
    (mezzano.supervisor:with-mutex (mezzano.network.udp::*udp-connection-lock*)
      (push connection mezzano.network.udp::*udp-connections*))
    (dhcp-send interface
	       (list (make-dhcp-option +opt-dhcp-message-type+ +dhcp-discover+)
		     (make-dhcp-option +opt-parameter-request-list+ #(#.+opt-netmask+ #.+opt-ntp-servers+ #.+opt-router+
								      #.+opt-domain-name+ #.+opt-dns-servers+
								      #.+opt-custom-mezzano-server+)))
	       xid)
    (unwind-protect
         (loop
            (let ((offer (net:receive connection :timeout 4)))
              (when (not offer)
                ;; Timed out
                (return-from acquire-lease nil))
              (let* ((siaddr (ub32ref/be offer 20))
                     (yiaddr (ub32ref/be offer 16))
                     (oaddr (make-array 4 :element-type '(unsigned-byte 8)))
                     (options (decode-all-options offer))
                     (dhcpserver (get-option options +opt-dhcp-server+))
                     (type (get-option options +opt-dhcp-message-type+)))
                (setf (ub32ref/be oaddr 0) yiaddr)
                (when (and (eql (aref type 0) +dhcp-offer+)
                           (eql (decode-dhcp-xid offer) xid)
                           (not (zerop yiaddr)))
                  (dhcp-send interface
                             (list (make-dhcp-option +opt-dhcp-message-type+ +dhcp-request+)
                                   (make-dhcp-option +opt-ip-address+ oaddr)
                                   (make-dhcp-option +opt-dhcp-server+ dhcpserver))
                             xid)
                  (loop
                     (let ((ack (net:receive connection :timeout 4)))
                       (when (not ack)
                         ;; Timed out
                         (return-from acquire-lease nil))
                       (let* ((ack-options (decode-all-options ack))
                              (confirmation (get-option ack-options +opt-dhcp-message-type+)))
                         (when (and (eql (aref confirmation 0) +dhcp-ack+) ; Ignore non-ack packets.
                                    (eql (decode-dhcp-xid ack) xid))
                           (return-from acquire-lease
                             (make-instance 'dhcp-lease :ip-address oaddr :netmask (get-option options +opt-netmask+)
                                            :gateway (get-option options +opt-router+) :dns-servers (get-option options +opt-dns-servers+)
                                            :dhcp-server (get-option options +opt-dhcp-server+) :interface interface
                                            :ntp-servers (get-option options +opt-ntp-servers+)
                                            :mezzano-server (get-option options +opt-custom-mezzano-server+)
                                            :lease-timestamp (get-universal-time)
                                            :lease-timeout (ub32ref/be (get-option options +opt-lease-time+) 0)
                                            :xid xid))))))))))
      (net:disconnect connection))))

(defmethod renew-lease ((lease dhcp-lease))
  (let* ((xid (xid lease))
	 (options (list (make-dhcp-option +opt-dhcp-message-type+ +dhcp-request+)))
	 (connection (make-instance 'mezzano.network.udp::udp4-connection
				    :remote-address (mezzano.network.ip:make-ipv4-address (ub32ref/be (dhcp-server lease) 0))
				    :remote-port +dhcp-server-port+
				    :local-address (mezzano.network.ip:make-ipv4-address (ub32ref/be (ip-address lease) 0))
				    :local-port +dhcp-client-port+))
	 (packet (build-dhcp-packet :xid xid :mac-address (mezzano.network.ethernet:ethernet-mac (interface lease))
                                    :options options :ciaddr (ub32ref/be (ip-address lease) 0)
                                    :broadcast nil)))
    (unwind-protect
	 (progn
	   (net:send packet connection)
	   (let ((reply (net:receive connection :timeout 4)))
	     (if reply
		 (let ((reply-options (decode-all-options reply)))
		   (setf (lease-timestamp lease) (get-universal-time)
			 (lease-timeout lease) (get-option reply-options +opt-lease-time+))
		   lease)
		 nil)))
      (net:disconnect connection))))

(defclass interaction ()
  ((%thread :initarg :thread :reader thread)
   (%lease :initarg :lease :accessor lease)))

;; This hash table is only modified from the network serial queue,
;; so can safely be left unsynchronized.
(defvar *dhcp-interactions* (make-hash-table))

(defun netmask-to-prefix-length (netmask)
  (- 32 (integer-length (logxor #xFFFFFFFF (ub32ref/be netmask 0)))))

(defun configure-interface-1 (interface lease)
  (let ((interaction (gethash interface *dhcp-interactions*))
        (local-ip (convert-to-ipv4-address (ip-address lease)))
        (prefix-length (netmask-to-prefix-length (netmask lease))))
    (setf (lease interaction) lease)
    (format t "DHCP lease acquired for ~A~%" interface)
    (format t "  ip: ~A/~D~%" local-ip prefix-length)
    ;; Bring interfaces up.
    (mezzano.network.ip::ifup interface local-ip prefix-length)
    ;; Add routes.
    ;; Local network.
    (mezzano.network.ip:add-route
     (mezzano.network.ip:address-network local-ip prefix-length)
     prefix-length
     interface
     interface)
    ;; Default route.
    (let ((gateway (gateway lease)))
      (when gateway
        (format t "  gateway: ~A~%" (convert-to-ipv4-address gateway))
        (mezzano.network.ip:add-route
         "0.0.0.0" 0
         (convert-to-ipv4-address gateway)
         interface)))
    (let ((dns-server (dns-servers lease)))
      (when dns-server
        (format t "  dns: ~A~%" (convert-to-ipv4-address dns-server))
        (mezzano.network.dns:add-dns-server (convert-to-ipv4-address dns-server) interface)))))

(defun deconfigure-interface-1 (interface)
  (let* ((interaction (gethash interface *dhcp-interactions*))
         (lease (lease interaction)))
    (when lease
      (let ((local-ip (convert-to-ipv4-address (ip-address lease)))
            (prefix-length (netmask-to-prefix-length (netmask lease))))
        (mezzano.network.ip:remove-route
         (mezzano.network.ip:address-network local-ip prefix-length)
         prefix-length
         interface)
        (let ((gateway (gateway lease)))
          (when gateway
            (mezzano.network.ip:remove-route "0.0.0.0" 0 interface)))
        (let ((dns-server (dns-servers lease)))
          (when dns-server
            (mezzano.network.dns:remove-dns-server (convert-to-ipv4-address dns-server) interface))))
      (setf (lease interaction) nil))))

(defun start-dhcp-interaction (interface)
  (mezzano.supervisor:make-thread
   #'(lambda ()
       (loop with lease do
	    (mezzano.sync.dispatch:dispatch-sync
             (lambda ()
               (deconfigure-interface-1 interface))
             net::*network-serial-queue*)
	    (loop for pause = 2 then (* 2 pause)
	       until lease
	       if (<= 16 pause) do
		 (setf lease (acquire-lease interface))
                 (unless lease
                   (sleep pause))
	       else do
                 (setf lease (acquire-lease interface))
                 (unless lease
                   (sleep (* 5 60))))
	    (when (mezzano-server lease)
	      (setf sys.int::*file-server-host-ip* (convert-to-ipv4-address (mezzano-server lease))))
	    (mezzano.sync.dispatch:dispatch-sync
             (lambda ()
               (configure-interface-1 interface lease))
             net::*network-serial-queue*)
	    (loop while lease do
		 (sleep (ceiling (lease-timeout lease) 2))
		 (setf lease (renew-lease lease)))))
   :name (format nil "DHCP interaction thread on interface ~A" interface)))

(defmethod net::configure-interface (interface (configuration-type (eql :dhcp)) &key)
  (setf (gethash interface *dhcp-interactions*)
        (make-instance 'interaction
                       :lease nil
                       :thread (start-dhcp-interaction interface))))

(defmethod net::deconfigure-interface (interface (configuration-type (eql :dhcp)) &key)
  (deconfigure-interface-1 interface)
  (let ((interaction (gethash interface *dhcp-interactions*)))
    (mezzano.supervisor:terminate-thread (thread interaction))
    (remhash interface *dhcp-interactions*)))
