;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.network.ip)

(defconstant +ip-protocol-icmp+ 1)
(defconstant +ip-protocol-igmp+ 2)
(defconstant +ip-protocol-tcp+ 6)
(defconstant +ip-protocol-udp+ 17)

(defconstant +icmp-echo-reply+ 0)
(defconstant +icmp-echo-request+ 8)
(defconstant +icmp-destination-unreachable+ 3)
(defconstant +icmp-time-exceeded+ 11)
(defconstant +icmp-parameter-problem+ 12)

;;; Destination unreachable codes.
(defconstant +icmp-code-net-unreachable+ 0)
(defconstant +icmp-code-host-unreachable+ 1)
(defconstant +icmp-code-protocol-unreachable+ 2)
(defconstant +icmp-code-port-unreachable+ 3)
(defconstant +icmp-code-fragmentation-needed+ 4)
(defconstant +icmp-code-source-route-failed+ 5)

;; Time exceeded codes.
(defconstant +icmp-code-ttl-exceeded+ 0)
(defconstant +icmp-code-fragment-timeout+ 1)

;;; (network gateway netmask interface)
(defvar *routing-table* nil)

(defvar *ipv4-interfaces* '())

(defun ifup (nic address)
  (setf (getf *ipv4-interfaces* nic) address))

(defun ifdown (nic)
  (setf (getf *ipv4-interfaces* nic) nil))

(defun ipv4-interface-address (nic &optional (errorp t))
  (or (getf *ipv4-interfaces* nic)
      (and errorp
           (error "No IPv4 address for interface ~S." nic))))

(defun ethertype (packet)
  (ub16ref/be packet 12))

(defun ipv4-protocol (packet)
  (aref packet (+ 14 9)))

(defun ipv4-source (packet)
  (ub32ref/be packet (+ 14 12)))

(defun compute-ip-partial-checksum (buffer &optional (start 0) end (initial 0))
  ;; From RFC 1071.
  (let ((total initial))
    (setf end (or end (length buffer)))
    (when (oddp (- end start))
      (decf end)
      (incf total (ash (aref buffer end) 8)))
    (do ((i start (+ i 2)))
	((>= i end))
      (incf total (ub16ref/be buffer i)))
    total))

(defun compute-ip-checksum (buffer &optional (start 0) end (initial 0))
  (let ((total (compute-ip-partial-checksum buffer start end initial)))
    (do ()
	((not (logtest total #xFFFF0000)))
      (setf total (+ (logand total #xFFFF)
		     (ash total -16))))
    (logand (lognot total) #xFFFF)))

(defun ipv4-route (destination)
  "Return the interface and destination mac for the destination IP address."
  (let ((default-route nil))
    (dolist (route *routing-table*)
      (cond ((null (first route))
	     (setf default-route route))
	    ((address-equal (address-network destination (third route))
                            (first route))
	     (return-from ipv4-route
	       (values (mezzano.network.arp:arp-lookup
                        (fourth route)
                        mezzano.network.ethernet:+ethertype-ipv4+
                        (ipv4-address-address destination))
		       (fourth route))))))
    (if default-route
        (values (mezzano.network.arp:arp-lookup
                 (fourth default-route)
                 mezzano.network.ethernet:+ethertype-ipv4+
                 (ipv4-address-address (second default-route)))
                (fourth default-route))
        (error "No route to host."))))

(defun transmit-ipv4-packet (destination protocol payload)
  (multiple-value-bind (ethernet-mac interface)
      (ipv4-route destination)
    (when (and ethernet-mac interface)
      (let* ((source (or (ipv4-interface-address interface) #x00000000))
	     (ip-header (make-array 20 :element-type '(unsigned-byte 8)
				    :initial-element 0))
	     (packet (cons ip-header payload)))
	(setf
	 ;; Version (4) and header length (5 32-bit words).
	 (aref ip-header 0) #x45
	 ;; Type of service, normal packet.
	 (aref ip-header 1) #x00
	 ;; Total length.
	 (ub16ref/be ip-header 2) (sys.net:packet-length packet)
	 ;; Packet ID(?).
	 (ub16ref/be ip-header 4) 0
	 ;; Flags & fragment offset.
	 (ub16ref/be ip-header 6) 0
	 ;; Time-to-Live. ### What should this be set to?
	 (aref ip-header 8) #xFF
	 ;; Protocol.
	 (aref ip-header 9) protocol
	 ;; Source address.
	 (ub32ref/be ip-header 12) (mezzano.network.ip::ipv4-address-address source)
	 ;; Destination address.
	 (ub32ref/be ip-header 16) (mezzano.network.ip::ipv4-address-address destination)
	 ;; Header checksum.
	 (ub16ref/be ip-header 10) (compute-ip-checksum ip-header))
	(mezzano.network.ethernet:transmit-ethernet-packet
         interface
         ethernet-mac
         mezzano.network.ethernet:+ethertype-ipv4+
         packet)))))

;;; IP addresses.

(defstruct (ipv4-address
             (:constructor %make-ipv4-address (address)))
  (address 0 :read-only t :type (unsigned-byte 32)))

(defun make-ipv4-address (address)
  "Turn ADDRESS into an IPv4 address object.
ADDRESS can be a string containing an IP address in dotted-decimal notation,
A sequence of 4 octets, with element 0 being the MSB and element 3 the LSB,
an (UNSIGNED-BYTE 32) or an IPV4-ADDRESS."
  (etypecase address
    (ipv4-address
     address)
    ((unsigned-byte 32)
     (%make-ipv4-address address))
    (string
     (make-ipv4-address (parse-ipv4-address address)))
    (sequence
     (assert (eql (length address) 4))
     (assert (every (lambda (x) (typep x 'octet)) address))
     (make-ipv4-address (logior (ash (elt address 0) 24)
                                (ash (elt address 1) 16)
                                (ash (elt address 2) 8)
                                (elt address 3))))))

(defun format-ipv4-address (stream argument &optional colon-p at-sign-p)
  "Print the (UNSIGNED-BYTE 32) argument in dotted-decimal notation."
  (check-type argument (unsigned-byte 32))
  (assert (and (not colon-p)
               (not at-sign-p)))
  (format stream "~D.~D.~D.~D"
          (ldb (byte 8 24) argument)
          (ldb (byte 8 16) argument)
          (ldb (byte 8 8) argument)
          (ldb (byte 8 0) argument)))

(defmethod print-object ((object ipv4-address) stream)
  (if (or *print-readably*
          *print-escape*)
      (call-next-method)
      (format-ipv4-address stream (ipv4-address-address object))))

(define-condition invalid-ipv4-address (simple-error)
  ((address :initarg :address
            :reader invalid-ipv4-address-address)))

(defun parse-ipv4-address (address &key (start 0) end)
  "Interpret the string ADDRESS as an IPv4 address in dotted-decimal notation,
 returning the parsed address as an (UNSIGNED-BYTE 32).
If ADDRESS is not a valid IPv4 address, an error of type INVALID-IPV4-ADDRESS is signalled."
  (setf address (string address))
  (unless end (setf end (length address)))
  (let ((value 0)
        (octet 0)
        (seen-dots 0))
    (dotimes (i (- end start))
      (let* ((c (char address (+ start i)))
             (weight (digit-char-p c)))
        (cond ((char= c #\.)
               (incf seen-dots)
               (when (>= seen-dots 4)
                 (error 'invalid-ipv4-address
                        :address address
                        :format-control "Too many dots."))
               (when (>= octet 256)
                 (error 'invalid-ipv4-address
                        :address address
                        :format-control "Part ~D too large."
                        :format-arguments (list (1- seen-dots))))
               (setf value (+ (ash value 8)
                              octet)
                     octet 0))
              (weight
               (setf octet (+ (* octet 10)
                              weight)))
              (t (error 'invalid-ipv4-address
                        :address address
                        :format-control "Invalid character ~C."
                        :format-arguments (list c))))))
    (when (>= octet (ash 1 (* (- 4 seen-dots) 8)))
      (error 'invalid-ipv4-address
             :address address
             :format-control "Final part too large."))
    (setf value (+ (ash value (* (- 4 seen-dots) 8))
                   octet))
    (check-type value (unsigned-byte 32))
    value))

(defgeneric address-equal (x y))

(defgeneric address-network (local-ip netmask)
  (:documentation "Return the address of LOCAL-IP's network, based on NETMASK."))

(defgeneric address-host (local-ip netmask)
  (:documentation "Return the address of LOCAL-IP's host, based on NETMASK."))

(defmethod address-equal ((x ipv4-address) (y ipv4-address))
  (eql (ipv4-address-address x) (ipv4-address-address y)))

(defmethod address-network ((local-ip ipv4-address) (netmask ipv4-address))
  (make-ipv4-address (logand (ipv4-address-address local-ip)
                             (ipv4-address-address netmask))))

(defmethod address-host ((local-ip ipv4-address) (netmask ipv4-address))
  (make-ipv4-address (logand (ipv4-address-address local-ip)
                             (lognot (ipv4-address-address netmask)))))

;;; ICMP.

(defun ping4-identifier (packet)
  (let ((header-length (* (ldb (byte 4 0) (aref packet 14)) 4)))
    (ub16ref/be packet (+ 14 header-length 4))))

(defun ping4-sequence-number (packet)
  (let ((header-length (* (ldb (byte 4 0) (aref packet 14)) 4)))
    (ub16ref/be packet (+ 14 header-length 6))))

(defun send-ping (destination &optional (identifier 0) (sequence-number 0) payload)
  (let ((packet (make-array (+ 8 (if payload (length payload) 56)))))
    (setf
     ;; Type.
     (aref packet 0) +icmp-echo-request+
     ;; Code.
     (aref packet 1) 0
     ;; Checksum.
     (ub16ref/be packet 2) 0
     ;; Identifier.
     (ub16ref/be packet 4) identifier
     ;; Sequence number.
     (ub16ref/be packet 6) sequence-number)
    (if payload
	(dotimes (i (length payload))
	  (setf (aref packet (+ 8 i)) (aref payload i)))
	(dotimes (i 56)
	  (setf (aref packet (+ 8 i)) i)))
    (setf (ub16ref/be packet 2) (mezzano.network.ip:compute-ip-checksum packet))
    (transmit-ipv4-packet destination mezzano.network.ip:+ip-protocol-icmp+ (list packet))))

(defun ping-host (host &optional (count 4))
  (let ((in-flight-pings nil)
        (received-pings '())
        (identifier 1234)
        (host-ip (sys.net::resolve-address host)))
    (mezzano.network.ethernet:with-raw-packet-hook
      (lambda (interface p)
        (declare (ignore interface))
        (when (and (eql (ethertype p) mezzano.network.ethernet:+ethertype-ipv4+)
                   (eql (ipv4-protocol p) +ip-protocol-icmp+)
                   (address-equal (make-ipv4-address (ipv4-source p))
                                  host-ip)
                   (eql (ping4-identifier p) identifier)
                   (find (ping4-sequence-number p) in-flight-pings))
          ;; ### Need locking here.
          (setf in-flight-pings (delete (ping4-sequence-number p) in-flight-pings))
          (push (ping4-sequence-number p) received-pings)
          (signal (make-condition 'mezzano.network.ethernet:drop-packet))))
      (dotimes (i count)
        (push i in-flight-pings)
        (send-ping host-ip identifier i))
      (let ((timeout-absolute (+ (get-universal-time) 10)))
        (loop
           (dolist (ping received-pings)
             (format t "Pong ~S.~%" ping))
           (setf received-pings '())
           (when (or (> (get-universal-time) timeout-absolute)
                     (null in-flight-pings))
             (return))
           (mezzano.supervisor:wait-for-heartbeat)))
      (when in-flight-pings
        (format t "~S pings still in-flight.~%" (length in-flight-pings))))))
