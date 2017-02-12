;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
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

;;; Routing.

;; Sorted from longest prefix to shortest.
;; Default route has a prefix of 0.
(defvar *routing-table-lock* (mezzano.supervisor:make-mutex "IPv4 routing table."))
(defvar *routing-table* nil)

(defclass ipv4-route ()
  ;; No netmask, just the network address & length of the prefix.
  ;; 192.168.0.0/24  network/prefix-length
  ((%network :initarg :network :reader route-network)
   (%prefix-length :initarg :prefix-length :reader route-prefix-length)
   (%gateway :initarg :gateway :reader route-gateway)))

(defun add-route (network prefix-length gateway)
  (if prefix-length
      (assert (eql (ipv4-address-address (address-host network prefix-length)) 0))
      (setf prefix-length 32))
  (let ((route (make-instance 'ipv4-route
                              :network (make-ipv4-address network)
                              :prefix-length prefix-length
                              :gateway gateway)))
    (mezzano.supervisor:with-mutex (*routing-table-lock*)
      (setf *routing-table*
            (merge 'list
                   (remove-if (lambda (x)
                                (and (address-equal network (route-network x))
                                     (eql prefix-length (route-prefix-length x))))
                              *routing-table*)
                   (list route)
                   #'>
                   :key #'route-prefix-length))
      route)))

(defun remove-route (network prefix-length)
  (mezzano.supervisor:with-mutex (*routing-table-lock*)
    (setf *routing-table*
          (remove-if (lambda (x)
                       (and (address-equal network (route-network x))
                            (eql prefix-length (route-prefix-length x))))
                     *routing-table*))))

(defun ipv4-route (destination &optional gateway-lookup)
  "Return the host IP and interface for the destination IP address."
  (dolist (route *routing-table*
           (error "No route to host ~A." destination))
    (when (address-equal (address-network destination
                                          (route-prefix-length route))
                         (route-network route))
      (return
        (cond ((ipv4-address-p (route-gateway route))
               (when gateway-lookup
                 (error "Gateway lookup failed."))
               ;; Route to this host is via a gateway.
               ;; Find a route to the gateway.
               (values (route-gateway route)
                       (nth-value 1 (ipv4-route (route-gateway route) t))))
              (t ;; Local host on this network
               (values destination
                       (route-gateway route))))))))

;;; Interfaces.

(defvar *ipv4-interfaces* '())

(defun ifup (nic address)
  (let ((existing (find nic *ipv4-interfaces* :key #'car)))
    (if existing
        (setf (cdr existing) address)
        (push (cons nic address) *ipv4-interfaces*))))

(defun ifdown (nic)
  (setf *ipv4-interfaces* (remove nic *ipv4-interfaces* :key #'car)))

(defun ipv4-interface-address (nic &optional (errorp t))
  (or (cdr (find nic *ipv4-interfaces* :key #'car))
      (and errorp
           (error "No IPv4 address for interface ~S." nic))))

(defun ipv4-address-interface (address &optional (errorp t))
  (or (car (find address *ipv4-interfaces* :key #'cdr :test #'address-equal))
      (and errorp
           (error "No interface for IPv4 address ~A." address))))

;;; Checksums.

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

;;; Other.

(defconstant +ipv4-header-version-and-ihl+ 0)
(defconstant +ipv4-header-version-size+ 4)
(defconstant +ipv4-header-version-position+ 4)
(defconstant +ipv4-header-ihl-size+ 4)
(defconstant +ipv4-header-ihl-position+ 0)
(defconstant +ipv4-header-dsf+ 1)
(defconstant +ipv4-header-total-length+ 2)
(defconstant +ipv4-header-identification+ 4)
(defconstant +ipv4-header-fragmentation-control+ 6)
(defconstant +ipv4-header-fragment-offset-size+ 12)
(defconstant +ipv4-header-fragment-offset-position+ 0)
(defconstant +ipv4-header-flag-more-fragments+ 13)
(defconstant +ipv4-header-flag-do-not-fragment+ 14)
(defconstant +ipv4-header-flag-reserved+ 15)
(defconstant +ipv4-header-ttl+ 8)
(defconstant +ipv4-header-protocol+ 9)
(defconstant +ipv4-header-checksum+ 10)
(defconstant +ipv4-header-source-ip+ 12)
(defconstant +ipv4-header-destination-ip+ 16)

(defgeneric transmit-ipv4-packet-on-interface (destination-host interface packet))

;; TODO: These should time out after a while.
(defvar *outstanding-sends* '()
  "Packets due to be transmitted, but are waiting for an ARP request to be resolved.")

(defun try-ethernet-transmit (destination-host interface packet)
  (let ((arp-result (mezzano.network.arp:arp-lookup interface
                                                    mezzano.network.ethernet:+ethertype-ipv4+
                                                    (ipv4-address-address destination-host))))
    (cond (arp-result
           (mezzano.network.ethernet:transmit-ethernet-packet
            interface
            arp-result
            mezzano.network.ethernet:+ethertype-ipv4+
            packet)
           t)
          (t
           nil))))

(defun arp-table-updated ()
  (setf *outstanding-sends*
        (loop
           for (destination-host interface packet attempt) in *outstanding-sends*
           do (format t "Attempting retransmit to ~S on ~S.~%"
                      destination-host interface)
           when (not (or (try-ethernet-transmit destination-host interface packet)
                         (> attempt 5)))
           collect (list destination-host interface packet (1+ attempt)))))

(defmethod transmit-ipv4-packet-on-interface (destination-host (interface mezzano.driver.network-card:network-card) packet)
  (when (not (try-ethernet-transmit destination-host interface packet))
    (push (list destination-host interface packet 0)
          *outstanding-sends*)))

(defmethod transmit-ipv4-packet-on-interface (destination-host (interface sys.net::loopback-interface) packet)
  ;; Bounce loopback packets out over the nic for testing as well.
  (mezzano.network.ethernet:transmit-ethernet-packet
   (first mezzano.driver.network-card::*nics*)
   #(0 0 0 1 2 3)
   mezzano.network.ethernet:+ethertype-ipv4+
   packet)
  ;; This is a bit very hacky...
  ;; Trying to avoid recursively calling back into the receive path. The
  ;; ethernet worker will pick packet up and deal with it normally.
  (mezzano.supervisor:fifo-push
   (cons interface
         (let ((loopback-packet (make-array (+ 14 (sys.net::packet-length packet))
                                            :element-type '(unsigned-byte 8))))
           (sys.net::copy-packet loopback-packet
                                 (cons #(0 0 0 0 0 0
                                         0 0 0 0 0 0
                                         8 0)
                                       packet))
           loopback-packet))
   mezzano.driver.network-card::*received-packets*))

(defun assemble-ipv4-packet (source destination protocol payload)
  (let* ((ip-header (make-array 20 :element-type '(unsigned-byte 8)))
         (packet (cons ip-header payload)))
    (setf
     ;; Version (4) and header length (5 32-bit words).
     (aref ip-header +ipv4-header-version-and-ihl+) #x45
     ;; Differentiated Services Field, normal packet.
     (aref ip-header +ipv4-header-dsf+) #x00
     ;; Total length.
     (ub16ref/be ip-header +ipv4-header-total-length+) (sys.net:packet-length packet)
     ;; Packet ID. No fragmentation support yet, no ID needed.
     (ub16ref/be ip-header +ipv4-header-identification+) 0
     ;; Flags & fragment offset.
     (ub16ref/be ip-header +ipv4-header-fragmentation-control+) 0
     ;; Time-to-Live. Specified in RFC1122.
     (aref ip-header +ipv4-header-ttl+) 64
     ;; Protocol.
     (aref ip-header +ipv4-header-protocol+) protocol
     ;; Source address.
     (ub32ref/be ip-header +ipv4-header-source-ip+) (mezzano.network.ip::ipv4-address-address source)
     ;; Destination address.
     (ub32ref/be ip-header +ipv4-header-destination-ip+) (mezzano.network.ip::ipv4-address-address destination)
     ;; Header checksum.
     (ub16ref/be ip-header +ipv4-header-checksum+) 0
     (ub16ref/be ip-header +ipv4-header-checksum+) (compute-ip-checksum ip-header))
    packet))

(defun transmit-ipv4-packet (source destination protocol payload)
  (multiple-value-bind (host interface)
      (ipv4-route destination)
    (when (and host interface)
      (transmit-ipv4-packet-on-interface
       host interface
       (assemble-ipv4-packet (or source (ipv4-interface-address interface) #x00000000)
                             destination
                             protocol
                             payload)))))

(defun ipv4-receive (interface packet start end)
  (let ((actual-length (- end start)))
    (when (< actual-length 20)
      ;; Runt packet with an incomplete header.
      (format t "Discarding runt IPv4 packet (~D bytes).~%" actual-length)
      (return-from ipv4-receive))
    (let* ((version-and-ihl (aref packet (+ start +ipv4-header-version-and-ihl+)))
           (version (ldb (byte +ipv4-header-version-size+ +ipv4-header-version-position+)
                         version-and-ihl))
           (header-length (* (ldb (byte +ipv4-header-ihl-size+ +ipv4-header-ihl-position+)
                                  version-and-ihl)
                             4))
           (total-length (ub16ref/be packet (+ start +ipv4-header-total-length+)))
           (frag-control (ub16ref/be packet (+ start +ipv4-header-fragmentation-control+)))
           (frag-offset (ldb (byte +ipv4-header-fragment-offset-size+ +ipv4-header-fragment-offset-position+)
                             frag-control))
           (ttl (aref packet (+ start +ipv4-header-ttl+)))
           (protocol (aref packet (+ start +ipv4-header-protocol+)))
           (source-ip (make-ipv4-address (ub32ref/be packet (+ start +ipv4-header-source-ip+))))
           (dest-ip (make-ipv4-address (ub32ref/be packet (+ start +ipv4-header-destination-ip+)))))
      (when (not (eql version 4))
        (format t "Discarding IPv4 packet with bad version ~D.~%" version)
        (return-from ipv4-receive))
      (when (< header-length 20)
        (format t "Discarding IPv4 packet with too-short header-length ~D.~%" header-length)
        (return-from ipv4-receive))
      (when (> header-length actual-length)
        (format t "Discarding IPv4 packet with too-long header-length ~D (max ~D).~%" header-length actual-length)
        (return-from ipv4-receive))
      (when (> header-length total-length)
        (format t "Discarding IPv4 packet with too-short total-length ~D (smaller than header-length ~D).~%" total-length header-length)
        (return-from ipv4-receive))
      (when (> total-length actual-length)
        (format t "Discarding IPv4 packet with too-long total-length ~D (max ~D).~%" total-length actual-length)
        (return-from ipv4-receive))
      (when (logbitp +ipv4-header-flag-reserved+ frag-control)
        (format t "Discarding IPv4 packet with reserved flag set.~%")
        (return-from ipv4-receive))
      (when (not (eql (compute-ip-checksum packet start (+ start header-length)) 0))
        (format t "Discarding IPv4 packet with bad header checksum.~%")
        (return-from ipv4-receive))
      ;; TODO: Fragmentation support.
      (when (or (not (eql frag-offset 0))
                (logbitp +ipv4-header-flag-more-fragments+ frag-control))
        (format t "Discarding fragmented IPv4 packet (not supported). ~X~%" frag-control)
        (return-from ipv4-receive))
      ;; Is it address to one of our interfaces?
      ;; If not, forward or reject it.
      (when (not (ipv4-address-interface dest-ip nil))
        (format t "Discarding IPv4 packet addressed to someone else. ~A~%" dest-ip)
        (return-from ipv4-receive))
      (case protocol
        (#.+ip-protocol-tcp+
         (mezzano.network.tcp::%tcp4-receive packet
                                             dest-ip
                                             source-ip
                                             (+ start header-length)
                                             (+ start total-length)))
        (#.+ip-protocol-udp+
         (mezzano.network.udp::%udp4-receive packet
                                             dest-ip
                                             source-ip
                                             (+ start header-length)
                                             (+ start total-length)))
        (#.+ip-protocol-icmp+
         (icmp4-receive packet
                        source-ip
                        (+ start header-length)
                        (+ start total-length)))
        (t (format t "Discarding IPv4 packet from ~X with unknown protocol ~X ~S.~%" source-ip protocol packet))))))

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
                        :format-control "Too many dots in address ~S."
                        :format-arguments (list address)))
               (when (>= octet 256)
                 (error 'invalid-ipv4-address
                        :address address
                        :format-control "Part ~D too large in address ~S."
                        :format-arguments (list (1- seen-dots) address)))
               (setf value (+ (ash value 8)
                              octet)
                     octet 0))
              (weight
               (setf octet (+ (* octet 10)
                              weight)))
              (t (error 'invalid-ipv4-address
                        :address address
                        :format-control "Invalid character ~:C in address ~S."
                        :format-arguments (list c address))))))
    (when (>= octet (ash 1 (* (- 4 seen-dots) 8)))
      (error 'invalid-ipv4-address
             :address address
             :format-control "Final part too large in address ~S."
             :format-arguments (list address)))
    (setf value (+ (ash value (* (- 4 seen-dots) 8))
                   octet))
    (check-type value (unsigned-byte 32))
    value))

(defgeneric address-equal (x y))

(defgeneric address-network (local-ip prefix-length)
  (:documentation "Return the address of LOCAL-IP's network, based on PREFIX-LENGTH."))

(defgeneric address-host (local-ip prefix-length)
  (:documentation "Return the address of LOCAL-IP's host, based on PREFIX-LENGTH."))

(defmethod address-equal ((x ipv4-address) (y ipv4-address))
  (eql (ipv4-address-address x) (ipv4-address-address y)))

(defmethod address-network ((local-ip ipv4-address) prefix-length)
  (check-type prefix-length (integer 0 32))
  (let ((mask (ash (1- (ash 1 prefix-length)) (- 32 prefix-length))))
    (make-ipv4-address (logand (ipv4-address-address local-ip)
                               mask))))

(defmethod address-host ((local-ip ipv4-address) prefix-length)
  (check-type prefix-length (integer 0 32))
  (let ((mask (ash (1- (ash 1 prefix-length)) (- 32 prefix-length))))
    (make-ipv4-address (logand (ipv4-address-address local-ip)
                               (lognot mask)))))

;;; ICMP.

(defconstant +icmp4-header-size+ 8)
(defconstant +icmp4-type+ 0)
(defconstant +icmp4-code+ 1)
(defconstant +icmp4-checksum+ 2)
(defconstant +icmp4-identifier+ 4)
(defconstant +icmp4-sequence-number+ 6)

(defvar *icmp-listeners* '())
(defvar *icmp-listener-lock* (mezzano.supervisor:make-mutex "ICMP listener list"))

(defun icmp4-receive (packet source-ip start end)
  (let ((length (- end start)))
    (when (< length +icmp4-header-size+)
      (format t "Discarding runt ICMPv4 packet from ~A.~%" source-ip)
      (return-from icmp4-receive))
    (let ((type (aref packet (+ start +icmp4-type+)))
          (code (aref packet (+ start +icmp4-code+)))
          (identifier (ub16ref/be packet (+ start +icmp4-identifier+)))
          (sequence-number (ub16ref/be packet (+ start +icmp4-sequence-number+))))
      (when (not (eql (compute-ip-checksum packet start end) 0))
        (format t "Discarding ICMPv4 packet with bad header checksum.~%")
        (return-from icmp4-receive))
      (dolist (l *icmp-listeners*)
        (funcall (first l) packet source-ip start end))
      (case type
        (#.+icmp-echo-request+
         (format t "Responding to ping from ~A.~%" source-ip)
         (transmit-icmp-packet source-ip
                               +icmp-echo-reply+ 0
                               identifier sequence-number
                               (subseq packet (+ start +icmp4-header-size+) end)))
        (#.+icmp-echo-reply+)
        (t (format t "Ignoring ICMP packet with unknown type/code ~D/~D.~%"
                   type code))))))

(defun transmit-icmp-packet (destination type code &optional (identifier 0) (sequence-number 0) payload)
  (let ((packet (make-array (+ +icmp4-header-size+ (if payload (length payload) 0)))))
    (setf (aref packet +icmp4-type+) type
          (aref packet +icmp4-code+) code
          (ub16ref/be packet +icmp4-checksum+) 0
          (ub16ref/be packet +icmp4-identifier+) identifier
          (ub16ref/be packet +icmp4-sequence-number+) sequence-number)
    (when payload
      (replace packet payload :start1 +icmp4-header-size+))
    (setf (ub16ref/be packet +icmp4-checksum+) (mezzano.network.ip:compute-ip-checksum packet))
    (transmit-ipv4-packet nil destination +ip-protocol-icmp+ (list packet))))

(defun send-ping (destination &optional (identifier 0) (sequence-number 0) payload)
  (when (not payload)
    (setf payload (make-array 56 :element-type '(unsigned-byte 8)))
    (dotimes (i 56)
      (setf (aref payload i) i)))
  (transmit-icmp-packet destination +icmp-echo-request+ 0 identifier sequence-number payload))

(defmacro with-icmp-listener (listener &body body)
  `(call-with-icmp-listener ,listener (lambda () ,@body)))

(defun call-with-icmp-listener (listener fn)
  (setf listener (cons listener nil))
  (unwind-protect
       (progn
         (mezzano.supervisor:with-mutex (*icmp-listener-lock*)
           (push listener *icmp-listeners*))
         (funcall fn))
    (mezzano.supervisor:with-mutex (*icmp-listener-lock*)
      (setf *icmp-listeners* (remove listener *icmp-listeners*)))))

(defun ping-host (host &key (count 4) quiet)
  (let ((in-flight-pings nil)
        (received-packets '())
        (identifier 1234)
        (host-ip (sys.net::resolve-address host))
        (rx-lock (mezzano.supervisor:make-mutex "ping lock")))
    (with-icmp-listener
        (lambda (packet source-ip start end)
          (mezzano.supervisor:with-mutex (rx-lock)
            (push (list packet source-ip start end) received-packets)))
      (dotimes (i count)
        (push i in-flight-pings)
        (send-ping host-ip identifier i))
      (let ((timeout-absolute (+ (get-universal-time) 10)))
        (loop
           (let ((packets (mezzano.supervisor:with-mutex (rx-lock)
                            (prog1 received-packets
                              (setf received-packets '())))))
             (loop
                for (packet source-ip start end) in packets
                do (when (and (address-equal source-ip host-ip)
                              (eql (aref packet (+ start +icmp4-type+)) +icmp-echo-reply+)
                              (eql (ub16ref/be packet (+ start +icmp4-identifier+)) identifier))
                     (let ((seq (ub16ref/be packet (+ start +icmp4-sequence-number+))))
                       (when (find seq in-flight-pings)
                         (setf in-flight-pings (remove seq in-flight-pings))
                         (when (not quiet)
                           (format t "Pong ~S.~%" seq)))))))
           (when (or (> (get-universal-time) timeout-absolute)
                     (null in-flight-pings))
             (return))
           (sleep 0.01)))
      (when (and in-flight-pings
                 (not quiet))
        (format t "~S pings still in-flight.~%" (length in-flight-pings)))
      (values (not (eql count (length in-flight-pings)))
              (length in-flight-pings)))))
