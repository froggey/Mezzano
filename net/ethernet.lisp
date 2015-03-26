;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :system.networking
  (:use :cl :sys.int)
  (:nicknames :sys.net)
  (:import-from :sys.int
                :ub16ref/be :ub16ref/le
                :ub32ref/be :ub32ref/le
                :ub64ref/be :ub64ref/le)
  (:export :register-nic
           :ethernet-mac
           :transmit-packet
           :arp-lookup
           :copy-packet :packet-length
           :buffered-format
           :send :receive))

(in-package :sys.net)

(defvar *cards* '())

(defun register-nic (nic)
  (push nic *cards*))

(defgeneric ethernet-mac (nic))

(defmethod ethernet-mac ((nic mezzano.supervisor:nic))
  (let ((mac (make-array 6 :element-type '(unsigned-byte 8)))
        (mac-int (mezzano.supervisor:nic-mac nic)))
    (dotimes (i 6)
      (setf (aref mac i) (ldb (byte 8 (* i 8)) mac-int)))
    mac))

(defun packet-length (packet)
  (reduce '+ (mapcar 'length packet)))

(defun copy-packet (buffer packet &optional (buffer-start 0))
  (dolist (p packet)
    (dotimes (i (length p))
      (setf (aref buffer buffer-start) (aref p i))
      (incf buffer-start))))

(defconstant +ethertype-ipv4+ #x0800)
(defconstant +ethertype-arp+  #x0806)
(defconstant +ethertype-ipv6+ #x86DD)

(defconstant +arp-op-request+ 1)
(defconstant +arp-op-reply+ 2)

(defconstant +arp-hrd-ethernet+ 1)

(defconstant +ip-protocol-icmp+ 1)
(defconstant +ip-protocol-igmp+ 2)
(defconstant +ip-protocol-tcp+ 6)
(defconstant +ip-protocol-udp+ 17)

(defconstant +tcp4-flag-fin+ #b00000001)
(defconstant +tcp4-flag-syn+ #b00000010)
(defconstant +tcp4-flag-rst+ #b00000100)
(defconstant +tcp4-flag-psh+ #b00001000)
(defconstant +tcp4-flag-ack+ #b00010000)

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

(defparameter *ethernet-broadcast* (make-array 6 :element-type '(unsigned-byte 8)
                                               :initial-element #xFF))

(defparameter *ipv4-interfaces* '())

(defun ifup (nic address)
  (setf (getf *ipv4-interfaces* nic) address))

(defun ifdown (nic)
  (setf (getf *ipv4-interfaces* nic) nil))

(defun ipv4-interface-address (nic &optional (errorp t))
  (or (getf *ipv4-interfaces* nic)
      (and errorp
           (error "No IPv4 address for interface ~S." nic))))

;;; The ARP table is a list of lists. Each list holds:
;;; (protocol-type protocol-address network-address age)
(defvar *arp-table* nil)

(defun arp-receive (interface packet)
  (let* ((htype (ub16ref/be packet 14))
	 (ptype (ub16ref/be packet 16))
	 (hlen (aref packet 18))
	 (plen (aref packet 19))
	 (oper (ub16ref/be packet 20))
	 (sha-start 22)
	 (spa-start (+ sha-start hlen))
	 (tha-start (+ spa-start plen))
	 (tpa-start (+ tha-start hlen))
	 (packet-end (+ tpa-start plen))
	 (merge-flag nil)
         (address (ipv4-interface-address interface nil)))
    ;; Ethernet hardware type and IPv4.
    (when (and (eql htype +arp-hrd-ethernet+) (eql hlen 6)
	       (eql ptype +ethertype-ipv4+) (eql plen 4))
      (let ((spa (ub32ref/be packet spa-start))
	    (tpa (ub32ref/be packet tpa-start)))
        (format t "Got ARP packet. ~X ~X ~X~%" spa tpa address)
	;; If the pair <protocol type, sender protocol address> is
	;; already in my translation table, update the sender
	;; hardware address field of the entry with the new
	;; information in the packet and set Merge_flag to true.
	(dolist (e *arp-table*)
	  (when (and (eql (first e) ptype)
		     (eql (second e) spa))
	    (setf (third e) (subseq packet sha-start spa-start)
		  merge-flag t)
	    (return)))
	(when (and address (eql tpa address))
	  (unless merge-flag
	    (push (list ptype spa (subseq packet sha-start spa-start) 0) *arp-table*))
	  (when (eql oper +arp-op-request+)
	    ;; Copy source hardware address to dest MAC and target h/w address.
	    (dotimes (i 6)
	      (setf (aref packet i) (aref packet (+ sha-start i))
		    (aref packet (+ tha-start i)) (aref packet (+ sha-start i))))
	    ;; Copy source protocol address to target protocol address.
	    (dotimes (i plen)
	      (setf (aref packet (+ tpa-start i)) (aref packet (+ spa-start i))))
	    ;; Set source hardware address and source MAC to the interface's MAC.
	    (let ((mac (ethernet-mac interface)))
	      (dotimes (i 6)
		(setf (aref packet (+ 6 i)) (aref mac i)
		      (aref packet (+ sha-start i)) (aref mac i))))
	    (setf (ub32ref/be packet spa-start) address
		  (ub16ref/be packet 20) +arp-op-reply+)
	    (transmit-packet interface (list packet))))))
    (format t "New ARP table: ~S~%" *arp-table*)))

(defun send-arp (interface ptype address)
  "Send an ARP packet out onto the wire."
  (unless (eql ptype +ethertype-ipv4+)
    (error "Unsupported protocol type ~S" ptype))
  (let ((packet (make-array 42 :element-type '(unsigned-byte 8)))
	(mac (ethernet-mac interface)))
    ;; Fill in various hardware address fields.
    (dotimes (i 6)
      ;; Ethernet destination.
      (setf (aref packet i) #xFF
	    ;; Ethernet source.
	    (aref packet (+ 6 i)) (aref mac i)
	    ;; ARP source hardware address.
	    (aref packet (+ 22 i)) (aref mac i)))
    ;; Set the source and target protocol addresses.
    (setf (ub32ref/be packet 28) (ipv4-interface-address interface)
	  (ub32ref/be packet 38) address
	  ;; Various other fields.
	  (ub16ref/be packet 12) +ethertype-arp+
	  (ub16ref/be packet 14) +arp-hrd-ethernet+
	  (ub16ref/be packet 16) +ethertype-ipv4+
	  (aref packet 18) 6
	  (aref packet 19) 4
	  (ub16ref/be packet 20) +arp-op-request+)
    (transmit-packet interface (list packet))))

(defun arp-lookup (interface ptype address)
  "Convert ADDRESS to an Ethernet address."
  ;; Scan the ARP table.
  (dolist (e *arp-table*)
    (when (and (eql (first e) ptype)
	       (eql (second e) address))
      (return-from arp-lookup (third e))))
  (dotimes (attempt 3)
    (send-arp interface ptype address)
    ;; FIXME: better timeout mechanism.
    (or (sys.int::process-wait-with-timeout "ARP Lookup" 5
                                            (lambda ()
                                              (dolist (e *arp-table* nil)
                                                (when (and (eql (first e) ptype)
                                                           (eql (second e) address))
                                                  (return t)))))
        (error "ARP lookup timed out."))
    (dolist (e *arp-table*)
      (when (and (eql (first e) ptype)
		 (eql (second e) address))
	(return-from arp-lookup (third e))))))

(defstruct tcp-connection
  state
  local-port
  remote-port
  remote-ip
  s-next
  r-next
  window-size
  (max-seg-size 1000)
  rx-data
  (lock (mezzano.supervisor:make-mutex "TCP connection lock"))
  (cvar (mezzano.supervisor:make-condition-variable "TCP connection cvar")))

(defmacro with-tcp-connection-locked (connection &body body)
  `(mezzano.supervisor:with-mutex ((tcp-connection-lock ,connection))
     ,@body))

(defvar *raw-packet-hooks* nil)
(defvar *tcp-connections* nil)
(defvar *tcp-connection-lock* (mezzano.supervisor:make-mutex "TCP connection list"))
(defvar *allocated-tcp-ports* nil)
(defvar *server-alist* '())

(defun get-tcp-connection (remote-ip remote-port local-port)
  (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
    (dolist (connection *tcp-connections*)
      (when (and (eql (tcp-connection-remote-ip connection) remote-ip)
                 (eql (tcp-connection-remote-port connection) remote-port)
                 (eql (tcp-connection-local-port connection) local-port))
        (return connection)))))

(defun %tcp4-receive (packet remote-ip start end)
  (let* ((remote-port (ub16ref/be packet start))
         (local-port (ub16ref/be packet (+ start 2)))
         (flags (aref packet (+ start 13)))
         (connection (get-tcp-connection remote-ip remote-port local-port)))
    (cond
      (connection
       (tcp4-receive connection packet start end))
      ((eql flags +tcp4-flag-syn+)
       (format t "Establishing TCP connection. l ~D  r ~D  from ~X.~%" local-port remote-port remote-ip)
       (let* ((seq (ub32ref/be packet (+ start 4)))
              (blah (random #x100000000))
              (connection (make-tcp-connection :state :syn-received
                                               :local-port local-port
                                               :remote-port remote-port
                                               :remote-ip remote-ip
                                               :s-next (logand #xFFFFFFFF (1+ blah))
                                               :r-next (logand #xFFFFFFFF (1+ seq))
                                               :window-size 8192)))
         (let ((server (assoc local-port *server-alist*)))
           (cond (server
                  (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
                    (push connection *tcp-connections*))
                  (tcp4-send-packet connection blah (logand #xFFFFFFFF (1+ seq)) nil
                                    :ack-p t :syn-p t)
                  (funcall (second server) connection))
                 (t (tcp4-send-packet connection blah (logand #xFFFFFFFF (1+ seq)) nil
                                      :ack-p t :rst-p t))))))
      (t (format t "Ignoring packet from ~X ~S~%" remote-ip
                 (subseq packet start end))
         (unless (logtest flags +tcp4-flag-rst+)
           (let ((connection (make-tcp-connection :state :syn-received
                                                  :local-port local-port
                                                  :remote-port remote-port
                                                  :remote-ip remote-ip
                                                  :s-next 0
                                                  :r-next 0
                                                  :window-size 8192)))
             (tcp4-send-packet connection 0 0 nil
                               :ack-p nil :rst-p t)))))))

(defun %receive-packet (interface packet)
  (dolist (hook *raw-packet-hooks*)
    (funcall hook interface packet))
  (let ((ethertype (ub16ref/be packet 12)))
    (cond
      ((eql ethertype +ethertype-arp+)
       (arp-receive interface packet))
      ((eql ethertype +ethertype-ipv4+)
       ;; Should check the IP header checksum here...
       (let ((header-length (* (ldb (byte 4 0) (aref packet 14)) 4))
             (total-length (ub16ref/be packet 16))
             (protocol (aref packet (+ 14 9))))
         (cond
           ((eql protocol +ip-protocol-tcp+)
            (%tcp4-receive packet (ub32ref/be packet (+ 14 12)) (+ 14 header-length) (+ 14 total-length)))
           ((eql protocol +ip-protocol-udp+)
            (%udp4-receive packet (ub32ref/be packet (+ 14 12)) (+ 14 header-length) (+ 14 total-length)))
           (t (format t "Unknown IPv4 protocol ~S ~S.~%" protocol packet)))))
      (t (format t "Unknown ethertype ~S ~S.~%" ethertype packet)))))

(define-condition drop-packet () ())

(defvar *ethernet-thread* nil)

(defun ethernet-thread ()
  (loop
     (with-simple-restart (abort "Ignore this packet.")
       (handler-case (multiple-value-bind (packet nic)
                         (mezzano.supervisor:net-receive-packet)
                       (%receive-packet nic packet))
         (drop-packet ())))))

(when (not *ethernet-thread*)
  (setf *ethernet-thread* (mezzano.supervisor:make-thread 'ethernet-thread
                                                          :name "Ethernet thread")))

(defun detach-tcp-connection (connection)
  (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
    (setf *tcp-connections* (remove connection *tcp-connections*)))
  (setf *allocated-tcp-ports* (remove (tcp-connection-local-port connection) *allocated-tcp-ports*)))

(defun tcp4-receive (connection packet &optional (start 0) end)
  (unless end (setf end (length packet)))
  (with-tcp-connection-locked connection
    (let* ((seq (ub32ref/be packet (+ start 4)))
           (ack (ub32ref/be packet (+ start 8)))
           (flags (aref packet (+ start 13)))
           (header-length (* (ash (aref packet (+ start 12)) -4) 4))
           (data-length (- end (+ start header-length))))
      (case (tcp-connection-state connection)
        (:syn-sent
         (if (and (logtest flags +tcp4-flag-ack+)
                  (logtest flags +tcp4-flag-syn+)
                  (eql ack (tcp-connection-s-next connection)))
             (progn
               (setf (tcp-connection-state connection) :established
                     (tcp-connection-r-next connection) (logand (1+ seq) #xFFFFFFFF))
               (tcp4-send-packet connection ack (tcp-connection-r-next connection) nil))
             (progn
               (setf (tcp-connection-state connection) :closed)
               (detach-tcp-connection connection)
               (tcp4-send-packet connection 0 0 nil :ack-p nil :rst-p t)
               (format t "TCP: got ack ~S, wanted ~S. Flags ~B~%" ack (tcp-connection-s-next connection) flags))))
        (:syn-received
         (cond ((and (eql flags +tcp4-flag-ack+)
                     (eql seq (tcp-connection-r-next connection))
                     (eql ack (tcp-connection-s-next connection)))
                (setf (tcp-connection-state connection) :established))
               (t (setf (tcp-connection-state connection) :closed)
                  (detach-tcp-connection connection)
                  (tcp4-send-packet connection 0 0 nil :ack-p nil :rst-p t)
                  (format t "TCP: Aborting connect. Got ack ~S, wanted ~S. Got seq ~S, wanted ~S. Flags ~B~%"
                          ack (tcp-connection-s-next connection)
                          seq (tcp-connection-r-next connection)
                          flags))))
        (:established
         ;; Ignore out-of-order packets.
         (when (not (eql seq (tcp-connection-r-next connection)))
           (format t "TCP: Ignoring packet with sequence number ~D, wanted ~D.~%"
                   seq (tcp-connection-r-next connection)))
         (when (eql seq (tcp-connection-r-next connection))
           (unless (eql data-length 0)
             ;; Send data to the user layer
             (if (tcp-connection-rx-data connection)
                 (setf (cdr (last (tcp-connection-rx-data connection))) (list (list packet (+ start header-length) end)))
                 (setf (tcp-connection-rx-data connection) (list (list packet (+ start header-length) end))))
             (setf (tcp-connection-r-next connection)
                   (logand (+ (tcp-connection-r-next connection) data-length)
                           #xFFFFFFFF)))
           (cond
             ((logtest flags +tcp4-flag-fin+)
              ;; Always ack FIN packets.
              (setf (tcp-connection-state connection) :closing
                    (tcp-connection-r-next connection)
                    (logand (+ (tcp-connection-r-next connection) 1)
                            #xFFFFFFFF))
              (tcp4-send-packet connection
                                (tcp-connection-s-next connection)
                                (tcp-connection-r-next connection)
                                nil
                                :fin-p t))
             ((not (eql data-length 0))
              (tcp4-send-packet connection
                                (tcp-connection-s-next connection)
                                (tcp-connection-r-next connection)
                                nil)))))
        (:closing
         (cond ((logtest flags +tcp4-flag-ack+))
               ((logtest flags +tcp4-flag-fin+)
                (tcp4-send-packet connection
                                  (tcp-connection-s-next connection)
                                  (tcp-connection-r-next connection)
                                  nil
                                  :ack-p t
                                  :fin-p t)))
         (detach-tcp-connection connection)
         (setf (tcp-connection-state connection) :closed))
        (:closed
         (detach-tcp-connection connection))
        (t (tcp4-send-packet connection 0 0 nil :ack-p nil :rst-p t)
           (format t "TCP: Unknown connection state ~S ~S ~S.~%" (tcp-connection-state connection) start packet)
           (detach-tcp-connection connection)
           (setf (tcp-connection-state connection) :closed))))
    (mezzano.supervisor:condition-notify (tcp-connection-cvar connection) t)))

(defun tcp4-send-packet (connection seq ack data &key (ack-p t) psh-p rst-p syn-p fin-p)
  (multiple-value-bind (ethernet-mac interface)
      (ipv4-route (tcp-connection-remote-ip connection))
    (cond ((and ethernet-mac interface)
           (let* ((source (ipv4-interface-address interface))
                  (source-port (tcp-connection-local-port connection))
                  (packet (assemble-tcp4-packet source source-port
                                                (tcp-connection-remote-ip connection)
                                                (tcp-connection-remote-port connection)
                                                seq ack
                                                (tcp-connection-window-size connection)
                                                data
                                                :ack-p ack-p
                                                :psh-p psh-p
                                                :rst-p rst-p
                                                :syn-p syn-p
                                                :fin-p fin-p)))
             (transmit-ethernet-packet interface ethernet-mac +ethertype-ipv4+ packet)))
           (t (format t "No route to ~/sys.int::format-ipv4-address/? Discarding TCPv4 packet.~%"
                      (tcp-connection-remote-ip connection))))))

(defun ipv4-route (destination)
  "Return the interface and destination mac for the destination IP address."
  (let ((default-route nil))
    (dolist (route *routing-table*)
      (cond ((null (first route))
	     (setf default-route route))
	    ((eql (logand destination (third route)) (first route))
	     (return-from ipv4-route
	       (values (arp-lookup (fourth route) +ethertype-ipv4+ destination)
		       (fourth route))))))
    (if default-route
        (values (arp-lookup (fourth default-route) +ethertype-ipv4+ (second default-route))
                (fourth default-route))
        (error "No route to host."))))

(defun make-ipv4-address (a b c d)
  (logior (ash a 24)
	  (ash b 16)
	  (ash c 8)
	  d))

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

(defun compute-ip-pseudo-header-partial-checksum (src-ip dst-ip protocol length)
  (+ (logand src-ip #xFFFF)
     (logand (ash src-ip -16) #xFFFF)
     (logand dst-ip #xFFFF)
     (logand (ash dst-ip -16) #xFFFF)
     protocol
     length))

(defun transmit-udp4-packet (destination source-port destination-port packet)
  (let* ((header (make-array 8 :element-type '(unsigned-byte 8)))
	 (packet (cons header packet)))
    (setf (ub16ref/be header 0) source-port
	  (ub16ref/be header 2) destination-port
	  (ub16ref/be header 4) (packet-length packet)
	  (ub16ref/be header 6) 0)
    (transmit-ipv4-packet destination +ip-protocol-udp+ packet)))

(defun assemble-tcp4-packet (src-ip src-port dst-ip dst-port seq-num ack-num window payload
			     &key (ack-p t) psh-p rst-p syn-p fin-p)
  "Build a full TCP & IP header."
  (let* ((checksum 0)
	 (payload-size (length payload))
	 (header (make-array 44 :element-type '(unsigned-byte 8)
			     :initial-element 0))
	 (packet (list header payload)))
    ;; Assemble the IP header.
    (setf
     ;; Version (4) and header length (5 32-bit words).
     (aref header 0) #x45
     ;; Type of service, normal packet.
     (aref header 1) #x00
     ;; Total length.
     (ub16ref/be header 2) (+ payload-size 44)
     ;; Packet ID(?).
     (ub16ref/be header 4) 0
     ;; Flags & fragment offset.
     (ub16ref/be header 6) 0
     ;; Time-to-Live. ### What should this be set to?
     (aref header 8) #xFF
     ;; Protocol.
     (aref header 9) +ip-protocol-tcp+
     ;; Source address.
     (ub32ref/be header 12) src-ip
     ;; Destination address.
     (ub32ref/be header 16) dst-ip
     ;; IP header checksum.
     (ub16ref/be header 10) (compute-ip-checksum header 0 20))
    ;; Assemble the TCP header.
    (setf
     (ub16ref/be header 20) src-port
     (ub16ref/be header 22) dst-port
     (ub32ref/be header 24) seq-num
     (ub32ref/be header 28) ack-num
     ;; Data offset/header length (6 32-bit words).
     (aref header 32) #x60
     ;; Flags.
     (aref header 33) (logior (if fin-p +tcp4-flag-fin+ 0)
			      (if syn-p +tcp4-flag-syn+ 0)
			      (if rst-p +tcp4-flag-rst+ 0)
			      (if psh-p +tcp4-flag-psh+ 0)
			      (if ack-p +tcp4-flag-ack+ 0))
     ;; Window.
     (ub16ref/be header 34) window)
    ;; Compute the final checksum.
    (setf checksum (compute-ip-pseudo-header-partial-checksum src-ip dst-ip +ip-protocol-tcp+ (+ 24 payload-size)))
    (setf checksum (compute-ip-partial-checksum header 20 nil checksum))
    (setf checksum (compute-ip-checksum payload 0 nil checksum))
    (setf (ub16ref/be header 36) checksum)
    packet))

(defun allocate-local-tcp-port ()
  (do ()
      (nil)
    (let ((port (+ (random 32768) 32768)))
      (unless (find port *allocated-tcp-ports*)
	(push port *allocated-tcp-ports*)
	(return port)))))

(defun tcp-connect (ip port)
  (let* ((source-port (allocate-local-tcp-port))
	 (seq (random #x100000000))
	 (connection (make-tcp-connection :state :syn-sent
					  :local-port source-port
					  :remote-port port
					  :remote-ip ip
					  :s-next (logand #xFFFFFFFF (1+ seq))
					  :r-next 0
					  :window-size 8192)))
    (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
      (push connection *tcp-connections*))
    (tcp4-send-packet connection seq 0 nil :ack-p nil :syn-p t)
    ;; FIXME: Better timeout mechanism.
    (let ((timeout (+ (get-universal-time) 10)))
      (loop
         (when (not (eql (tcp-connection-state connection) :syn-sent))
           (return))
         (when (> (get-universal-time) timeout)
           (with-tcp-connection-locked connection
             (setf (tcp-connection-state connection) :closing))
           (error "Connection timed out."))
         (mezzano.supervisor:wait-for-heartbeat)))
    connection))

(defun tcp-send (connection data &optional (start 0) end)
  (when (eql (tcp-connection-state connection) :established)
    (setf end (or end (length data)))
    (let ((mss (tcp-connection-max-seg-size connection)))
      (cond
        ((>= start end))
        ((> (- end start) mss)
         ;; Send multiple packets.
         (do ((offset start (+ offset mss)))
             ((>= offset end))
           (tcp-send connection data offset (min (+ offset mss) end))))
        (t ;; Send one packet.
         (with-tcp-connection-locked connection
           (let ((s-next (tcp-connection-s-next connection)))
             (setf (tcp-connection-s-next connection)
                   (logand (+ s-next (- end start))
                           #xFFFFFFFF))
             (tcp4-send-packet connection s-next
                               (tcp-connection-r-next connection)
                               (if (and (eql start 0)
                                        (eql end (length data)))
                                   data
                                   (subseq data start end))
                               :psh-p t))))))))

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
    (setf (ub16ref/be packet 2) (compute-ip-checksum packet))
    (transmit-ipv4-packet destination +ip-protocol-icmp+ (list packet))))

(defmacro with-raw-packet-hook (function &body body)
  (let ((old-value (gensym)))
    `(let ((,old-value *raw-packet-hooks*))
       (unwind-protect (progn (setf *raw-packet-hooks* (cons ,function *raw-packet-hooks*))
                              ,@body)
         (setf *raw-packet-hooks* ,old-value)))))

(defun ethertype (packet)
  (ub16ref/be packet 12))

(defun ipv4-protocol (packet)
  (aref packet (+ 14 9)))

(defun ipv4-source (packet)
  (ub32ref/be packet (+ 14 12)))

(defun ping4-identifier (packet)
  (let ((header-length (* (ldb (byte 4 0) (aref packet 14)) 4)))
    (ub16ref/be packet (+ 14 header-length 4))))

(defun ping4-sequence-number (packet)
  (let ((header-length (* (ldb (byte 4 0) (aref packet 14)) 4)))
    (ub16ref/be packet (+ 14 header-length 6))))

(defun sys.int::process-wait-with-timeout (reason timeout fn &rest args)
  (let ((timeout-absolute (+ (get-universal-time) timeout)))
    (sys.int::process-wait reason
                           (lambda ()
                             (when (> (get-universal-time) timeout-absolute)
                               (return-from sys.int::process-wait-with-timeout
                                 nil))
                             (apply fn args)))))

(defun sys.int::process-wait (reason fn &rest args)
  (loop
     (when (apply fn args)
       (return))
     (mezzano.supervisor:wait-for-heartbeat))
  t)

(defun ping-host (host &optional (count 4))
  (let ((in-flight-pings nil)
        (identifier 1234)
        (host-ip (resolve-address host)))
    (with-raw-packet-hook
      (lambda (interface p)
        (declare (ignore interface))
        (when (and (eql (ethertype p) +ethertype-ipv4+)
                   (eql (ipv4-protocol p) +ip-protocol-icmp+)
                   (eql (ipv4-source p) host-ip)
                   (eql (ping4-identifier p) identifier)
                   (find (ping4-sequence-number p) in-flight-pings))
          (setf in-flight-pings (delete (ping4-sequence-number p) in-flight-pings))
          (format t "Pong ~S.~%" (ping4-sequence-number p))
          (signal (make-condition 'drop-packet))))
      (dotimes (i count)
        (push i in-flight-pings)
        (send-ping host-ip identifier i))
      (sys.int::process-wait-with-timeout "Ping" 10 (lambda () (null in-flight-pings)))
      (when in-flight-pings
        (format t "~S pings still in-flight.~%" (length in-flight-pings))))))

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
	 (ub16ref/be ip-header 2) (packet-length packet)
	 ;; Packet ID(?).
	 (ub16ref/be ip-header 4) 0
	 ;; Flags & fragment offset.
	 (ub16ref/be ip-header 6) 0
	 ;; Time-to-Live. ### What should this be set to?
	 (aref ip-header 8) #xFF
	 ;; Protocol.
	 (aref ip-header 9) protocol
	 ;; Source address.
	 (ub32ref/be ip-header 12) source
	 ;; Destination address.
	 (ub32ref/be ip-header 16) destination
	 ;; Header checksum.
	 (ub16ref/be ip-header 10) (compute-ip-checksum ip-header))
	(transmit-ethernet-packet interface ethernet-mac +ethertype-ipv4+ packet)))))

(defun transmit-ethernet-packet (interface destination ethertype packet)
  (let* ((ethernet-header (make-array 14 :element-type '(unsigned-byte 8)))
	 (packet (cons ethernet-header packet))
	 (source (ethernet-mac interface)))
    (dotimes (i 6)
      (setf (aref ethernet-header i) (aref destination i)
	    (aref ethernet-header (+ i 6)) (aref source i)))
    (setf (ub16ref/be ethernet-header 12) ethertype)
    (transmit-packet interface packet)))

;;; (network gateway netmask interface)
(defvar *routing-table* nil)

(defgeneric transmit-packet (nic packet-descriptor))

(defmethod transmit-packet ((nic mezzano.supervisor:nic) packet)
  (mezzano.supervisor:net-transmit-packet nic packet))

(defclass tcp-stream (sys.gray:fundamental-character-input-stream
                      sys.gray:fundamental-character-output-stream
                      sys.gray:fundamental-binary-input-stream
                      sys.gray:fundamental-binary-output-stream
                      sys.gray:unread-char-mixin)
  ((connection :initarg :connection :reader tcp-stream-connection)
   (current-packet :initform nil :accessor tcp-stream-packet)))

(defun encode-utf8-string (sequence start end)
  (let ((bytes (make-array (- end start)
                           :element-type '(unsigned-byte 8)
                           :adjustable t
                           :fill-pointer 0)))
    (dotimes (i (- end start))
      (let ((c (char sequence (+ start i))))
        (when (eql c #\Newline)
          (vector-push-extend #x0D bytes))
        (let ((code (char-code c)))
          (unless (zerop (char-bits c)) (setf code (char-code #\REPLACEMENT_CHARACTER)))
          (unless (and (<= 0 code #x1FFFFF)
                       (not (<= #xD800 code #xDFFF)))
            (setf code (char-code #\REPLACEMENT_CHARACTER)))
          (cond ((<= code #x7F)
                 (vector-push-extend code bytes))
                ((<= #x80 code #x7FF)
                 (vector-push-extend (logior (ash (logand code #x7C0) -6) #xC0) bytes)
                 (vector-push-extend (logior (logand code #x3F) #x80) bytes))
                ((or (<= #x800 code #xD7FF)
                     (<= #xE000 code #xFFFF))
                 (vector-push-extend (logior (ash (logand code #xF000) -12) #xE0) bytes)
                 (vector-push-extend (logior (ash (logand code #xFC0) -6) #x80) bytes)
                 (vector-push-extend (logior (logand code #x3F) #x80) bytes))
                ((<= #x10000 code #x10FFFF)
                 (vector-push-extend (logior (ash (logand code #x1C0000) -18) #xF0) bytes)
                 (vector-push-extend (logior (ash (logand code #x3F000) -12) #x80) bytes)
                 (vector-push-extend (logior (ash (logand code #xFC0) -6) #x80) bytes)
                 (vector-push-extend (logior (logand code #x3F) #x80) bytes))))))
    bytes))

(defun refill-tcp-stream-buffer (stream)
  (let ((connection (tcp-stream-connection stream)))
    (when (and (null (tcp-stream-packet stream))
               (tcp-connection-rx-data connection))
      (setf (tcp-stream-packet stream) (pop (tcp-connection-rx-data connection))))))

(defun tcp-connection-closed-p (stream)
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (let ((connection (tcp-stream-connection stream)))
      (refill-tcp-stream-buffer stream)
      (and (null (tcp-stream-packet stream))
           (not (member (tcp-connection-state connection) '(:established :syn-received :syn-sent)))))))

(defmethod sys.gray:stream-listen ((stream tcp-stream))
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (refill-tcp-stream-buffer stream)
    (not (null (tcp-stream-packet stream)))))

(defmethod sys.gray:stream-read-byte ((stream tcp-stream))
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (when (not (refill-tcp-packet-buffer stream))
      (return-from sys.gray:stream-read-byte :eof))
    (let* ((packet (tcp-stream-packet stream))
           (byte (aref (first packet) (second packet))))
      (when (>= (incf (second packet)) (third packet))
        (setf (tcp-stream-packet stream) nil))
      byte)))

(defmethod sys.gray:stream-read-sequence ((stream tcp-stream) sequence &optional (start 0) end)
  (unless end (setf end (length sequence)))
  (let ((n (- end start)))
    (if (and (subtypep (stream-element-type stream) 'character)
             (or (listp sequence)
                 (not (subtypep (array-element-type sequence) 'unsigned-byte))))
        ;; Fall back on generic read-stream for strings.
        (call-next-method)
        (tcp-read-byte-sequence sequence stream start end))))

(defun refill-tcp-packet-buffer (stream)
  (let ((connection (tcp-stream-connection stream)))
    (when (null (tcp-stream-packet stream))
      (loop
         ;; Wait for data or for the connection to close.
         (when (or (tcp-connection-rx-data connection)
                   (not (member (tcp-connection-state connection)
                                '(:established :syn-received :syn-sent))))
           (return))
         (mezzano.supervisor:condition-wait (tcp-connection-cvar connection)
                                            (tcp-connection-lock connection)))
      ;; Something may have refilled while we were waiting.
      (when (tcp-stream-packet stream)
        (return-from refill-tcp-packet-buffer t))
      (when (and (null (tcp-connection-rx-data connection))
                 (not (member (tcp-connection-state connection)
                              '(:established :syn-received :syn-sent))))
        (return-from refill-tcp-packet-buffer nil))
      (setf (tcp-stream-packet stream) (pop (tcp-connection-rx-data connection))))
    t))

(defun tcp-read-byte-sequence (sequence stream start end)
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (let ((position start))
      (loop (when (or (>= position end)
                      (not (refill-tcp-packet-buffer stream)))
              (return))
         (let* ((packet (tcp-stream-packet stream))
                (pkt-data (first packet))
                (pkt-offset (second packet))
                (pkt-length (third packet))
                (bytes-to-copy (min (- end position) (- pkt-length pkt-offset))))
           (replace sequence pkt-data
                    :start1 position
                    :start2 pkt-offset
                    :end2 (+ pkt-offset bytes-to-copy))
           (when (>= (incf (second packet) bytes-to-copy) (third packet))
             (setf (tcp-stream-packet stream) nil))
           (incf position bytes-to-copy)))
      position)))

(defun utf-8-decode-leader (leader)
  "Break a UTF-8 leader byte apart into sequence length (minus one) and code-point so far."
  (cond ((eql (logand leader #b10000000) 0)
         (values 0 leader))
        ((eql (logand leader #b11100000) #b11000000)
         (values 1 (logand leader #b00011111)))
        ((eql (logand leader #b11110000) #b11100000)
         (values 2 (logand leader #b00001111)))
        ((eql (logand leader #b11111000) #b11110000)
         (values 3 (logand leader #b00000111)))))

(defmethod sys.gray:stream-read-char ((stream tcp-stream))
  (let ((leader (read-byte stream nil)))
    (unless leader
      (return-from sys.gray:stream-read-char :eof))
    (when (eql leader #x0D)
      (read-byte stream nil)
      (setf leader #x0A))
    (multiple-value-bind (length code-point)
        (utf-8-decode-leader leader)
      (when (null length)
        (return-from sys.gray:stream-read-char
          #\REPLACEMENT_CHARACTER))
      (dotimes (i length)
        (let ((byte (read-byte stream nil)))
          (when (or (null byte)
                    (/= (ldb (byte 2 6) byte) #b10))
            (return-from sys.gray:stream-read-char
              #\REPLACEMENT_CHARACTER))
          (setf code-point (logior (ash code-point 6)
                                   (ldb (byte 6 0) byte)))))
      (if (or (> code-point #x0010FFFF)
              (<= #xD800 code-point #xDFFF))
          #\REPLACEMENT_CHARACTER
          (code-char code-point)))))

(defmethod sys.gray:stream-write-byte ((stream tcp-stream) byte)
  (let ((ary (make-array 1 :element-type '(unsigned-byte 8)
                         :initial-element byte)))
    (tcp-send (tcp-stream-connection stream) ary)))

(defmethod sys.gray:stream-write-sequence ((stream tcp-stream) sequence &optional (start 0) end)
  (unless end (setf end (length sequence)))
  (cond ((stringp sequence)
         (setf sequence (encode-utf8-string sequence start end)))
        ((not (and (zerop start)
                   (eql end (length sequence))))
         (setf sequence (subseq sequence start end))))
  (tcp-send (tcp-stream-connection stream) sequence))

(defmethod sys.gray:stream-write-char ((stream tcp-stream) character)
  (when (eql character #\Newline)
    (write-byte #x0D stream))
  (write-byte (char-code character) stream))

(defmethod close ((stream tcp-stream) &key abort)
  (let* ((connection (tcp-stream-connection stream)))
    (setf (tcp-connection-state connection) :closing)
    (tcp4-send-packet connection
                      (tcp-connection-s-next connection)
                      (tcp-connection-r-next connection)
                      nil
                      :fin-p t)))

(defun buffered-format (stream control-string &rest arguments)
  "Buffered FORMAT."
  (declare (dynamic-extent argument))
  (write-sequence (apply 'format nil control-string arguments) stream))

;;; Hardcode the qemu & virtualbox network layout for now.
(defun net-setup (&key
                  (local-ip (make-ipv4-address 10 0 2 15))
                  (netmask (make-ipv4-address 255 255 255 0))
                  (gateway (make-ipv4-address 10 0 2 2))
                  (interface (first *cards*)))
  ;; Flush existing route info.
  (setf *ipv4-interfaces* nil
        *routing-table* nil
        *dns-servers* '())
  (ifup interface local-ip)
  ;; Default route.
  (push (list nil gateway netmask interface)
        *routing-table*)
  ;; Local network.
  (push (list (logand local-ip netmask)
              nil
              netmask
              interface)
        *routing-table*)
  ;; Use Google DNS, as Virtualbox does not provide a DNS server within the NAT.
  (push (make-ipv4-address 8 8 8 8) *dns-servers*)
  t)

(defun tcp-stream-connect (address port)
  (make-instance 'tcp-stream :connection (tcp-connect (resolve-address address) port)))

(defmacro with-open-network-stream ((var address port) &body body)
  `(with-open-stream (,var (tcp-stream-connect ,address ,port))
     ,@body))

(defun format-mac-address (stream mac &optional colon-p at-sign-p)
  (format stream "~2,'0X:~2,'0X:~2,'0X:~2,'0X:~2,'0X:~2,'0X"
          (ldb (byte 8 0) mac)
          (ldb (byte 8 8) mac)
          (ldb (byte 8 16) mac)
          (ldb (byte 8 24) mac)
          (ldb (byte 8 32) mac)
          (ldb (byte 8 40) mac)))

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

(defun format-tcp4-address (stream argument &optional colon-p at-sign-p)
  (format-ipv4-address stream argument colon-p at-sign-p))

(defun ethernet-boot-hook ()
  (setf *cards* (copy-list mezzano.supervisor:*nics*)
        *routing-table* '()
        *ipv4-interfaces* '()
        *arp-table* '())
  (net-setup)
  (format t "Interfaces: ~S~%" *ipv4-interfaces*))
(ethernet-boot-hook)
(mezzano.supervisor:add-boot-hook 'ethernet-boot-hook)

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

;;; Generics for working with packet-oriented connections.

(defgeneric send (sequence connection &optional (start 0) end))
(defgeneric receive (connection &optional timeout))
(defgeneric disconnect (connection))

;;; UDP stuff.

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

(defun get-udp-connection (remote-ip remote-port local-port)
  (mezzano.supervisor:with-mutex (*udp-connection-lock*)
    (dolist (connection *udp-connections*)
      (when (and (eql (remote-address connection) remote-ip)
                 (eql (remote-port connection) remote-port)
                 (eql (local-port connection) local-port))
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
  (let* ((source-port (allocate-local-udp-port))
         (remote-address (resolve-address remote-host))
	 (connection (make-instance 'udp4-connection
                                    :remote-address remote-address
                                    :remote-port remote-port
                                    :local-port source-port)))
    (mezzano.supervisor:with-mutex (*udp-connection-lock*)
      (push connection *udp-connections*))
    connection))

(defmethod disconnect ((connection udp4-connection))
  (mezzano.supervisor:with-mutex (*udp-connection-lock*)
    (setf *udp-connections* (remove connection *udp-connections*))
    (setf *allocated-udp-ports* (remove (local-port connection) *allocated-udp-ports*))))

(defmethod send (sequence (connection udp4-connection) &optional (start 0) end)
  (transmit-udp4-packet (remote-address connection) (local-port connection) (remote-port connection) (list sequence)))

(defmethod receive ((connection udp4-connection) &optional timeout)
  (cond ((not timeout)
         ;; Wait forever.
         (with-udp-connection-locked (connection)
           (loop
              (when (udp-connection-packets connection)
                (return (pop (udp-connection-packets connection))))
              (mezzano.supervisor:condition-wait (udp-connection-cvar connection)
                                                 (udp-connection-lock connection)))))
        ((zerop timeout)
         ;; Don't wait.
         (with-udp-connection-locked (connection)
           (when (udp-connection-packets connection)
             (pop (udp-connection-packets connection)))))
        (t
         ;; Wait for some time.
         (let ((timeout-absolute (+ (get-universal-time) timeout)))
           (loop
              (with-udp-connection-locked (connection)
                (when (udp-connection-packets connection)
                  (return (pop (udp-connection-packets connection)))))
              (when (> (get-universal-time) timeout-absolute)
                (return nil))
              (mezzano.supervisor:wait-for-heartbeat))))))

(defun %udp4-receive (packet remote-ip start end)
  (let* ((remote-port (ub16ref/be packet start))
         (local-port (ub16ref/be packet (+ start 2)))
         (length (ub16ref/be packet (+ start 4)))
         (checksum (ub16ref/be packet (+ start 6)))
         (connection (get-udp-connection remote-ip remote-port local-port)))
    (cond
      (connection
       (with-udp-connection-locked (connection)
         (let ((payload (make-array (- end (+ start 8)) :displaced-to packet :displaced-index-offset (+ start 8))))
           ;; Send data to the user layer
           (setf (udp-connection-packets connection) (append (udp-connection-packets connection)
                                                             (list payload)))
           (mezzano.supervisor:condition-notify (udp-connection-cvar connection) t))))
      (t (format t "Ignoring UDP4 packet from ~X ~S~%" remote-ip
                 (subseq packet start end))))))

;;; DNS support.

(defvar *dns-servers* '())

(defvar +dns-port+ 53)
(defvar +dns-standard-query+ #x0100)

(defun encode-dns-type (type)
  (ecase type
    (:a 1)
    (:ns 2)
    (:md 3)
    (:mf 4)
    (:cname 5)
    (:soa 6)
    (:mb 7)
    (:mg 8)
    (:mr 9)
    (:null 10)
    (:wks 11)
    (:ptr 12)
    (:hinfo 13)
    (:minfo 14)
    (:mx 15)
    (:txt 16)
    (:aaaa 28)))

(defun decode-dns-type (type)
  (case type
    (1 :a)
    (2 :ns)
    (3 :md)
    (4 :mf)
    (5 :cname)
    (6 :soa)
    (7 :mb)
    (8 :mg)
    (9 :mr)
    (10 :null)
    (11 :wks)
    (12 :ptr)
    (13 :hinfo)
    (14 :minfo)
    (15 :mx)
    (16 :txt)
    (28 :aaaa)
    (t (list :unknown-type type))))

(defun encode-dns-class (class)
  (ecase class
    (:in 1)
    (:cs 2)
    (:ch 3)
    (:hs 4)))

(defun decode-dns-class (class)
  (case class
    (1 :in)
    (2 :cs)
    (3 :ch)
    (4 :hs)
    (t (list :unknown-class class))))

(defun explode (string seperator &key (start 0) (end nil))
  "Break a string apart into a list using SEPERATOR as
the seperator character."
  (let ((start start)
	(list nil))
    (dotimes (i (- (or end (length string)) start))
      (when (eql (char string i) seperator)
	(push (subseq string start i) list)
	(setf start (1+ i))))
      (push (subseq string start end) list)
    (nreverse list)))

(defun write-dns-name (packet offset name)
  (when (not (zerop (length name)))
    ;; Domain names can end in a #\., trim it off.
    (dolist (part (explode name #\. :end (when (eql #\. (char name (1- (length name))))
                                           (1- (length name)))))
      (assert (<= (length part) 63))
      (assert (not (zerop (length part))))
      (setf (aref packet offset) (length part))
      (incf offset)
      (loop for c across part do
           (setf (aref packet offset) (char-code (char-downcase c)))
           (incf offset))))
  (setf (aref packet offset) 0)
  (incf offset)
  offset)

(defun build-dns-packet (id flags &key questions answers authority-rrs additional-rrs)
  (when (or answers authority-rrs additional-rrs)
    (error "TODO..."))
  (let ((packet (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0))
        (offset 12))
    (setf (ub16ref/be packet  0) id
          (ub16ref/be packet  2) flags
          (ub16ref/be packet  4) (length questions)
          (ub16ref/be packet  6) 0 #+(or)(length answers)
          (ub16ref/be packet  8) 0 #+(or)(length authority-rrs)
          (ub16ref/be packet 10) 0 #+(or)(length additional-rrs))
    (loop for (name type class) in questions do
         (setf offset (write-dns-name packet offset name))
         (setf (ub16ref/be packet offset) (encode-dns-type type))
         (setf (ub16ref/be packet (+ offset 2)) (encode-dns-class class))
         (incf offset 4))
    (subseq packet 0 offset)))

(defun read-dns-name (packet offset)
  (let ((name (make-array 255 :element-type 'character :fill-pointer 0)))
    (labels ((read-section (offset)
               (let ((section-size 0))
                 (loop
                    (let ((leader (aref packet offset)))
                      (incf section-size)
                      (when (zerop leader)
                        (return))
                      (ecase (ldb (byte 2 6) leader)
                        (0 ;; Reading a label from the packet of length LEADER.
                         (dotimes (i leader)
                           (vector-push (code-char (aref packet (+ offset 1 i))) name))
                         (incf section-size leader)
                         (incf offset (1+ leader))
                         (vector-push #\. name))
                        (3 ;; Following a pointer.
                         (let ((pointer (ldb (byte 14 0) (ub16ref/be packet offset))))
                           ;; Make sure it doesn't point to a pointer.
                           ;; This is probably permitted, but seems like a bad idea.
                           (when (eql (ldb (byte 2 6) (aref packet pointer)) 3)
                             (error "Pointer points directly to pointer."))
                           (read-section pointer)
                           (incf section-size)
                           (return))))))
                 section-size)))
      (incf offset (read-section offset))
      (when (not (zerop (length name)))
        ;; Snip trailing #\.
        (decf (fill-pointer name)))
      (values name offset))))

(defun decode-resource-record-data (type class packet offset data-len)
  (when (not (eql class :in))
    (return-from decode-resource-record-data (list (subseq packet offset (+ offset data-len)))))
  (case type
    ((:cname :ptr :mb :md :mf :mg :mr :ns) (list (read-dns-name packet offset)))
    (:mx (list (ub16ref/be packet offset) (read-dns-name packet (+ offset 2))))
    (:a (list (ub32ref/be packet offset)))
    (:soa (multiple-value-bind (mname next-offset)
              (read-dns-name packet offset)
            (multiple-value-bind (rname next-offset)
                (read-dns-name packet next-offset)
              (let ((serial (ub32ref/be packet next-offset))
                    (refresh (ub32ref/be packet (+ next-offset 4)))
                    (retry (ub32ref/be packet (+ next-offset 8)))
                    (expire (ub32ref/be packet (+ next-offset 12)))
                    (minimum (ub32ref/be packet (+ next-offset 16))))
                (list mname rname serial refresh retry expire minimum)))))
    (t (list (subseq packet offset (+ offset data-len))))))

(defun decode-dns-packet (packet)
  (let ((id (ub16ref/be packet 0))
        (flags (ub16ref/be packet 2))
        (qdcount (ub16ref/be packet 4))
        (ancount (ub16ref/be packet 6))
        (nscount (ub16ref/be packet 8))
        (arcount (ub16ref/be packet 10))
        (questions '())
        (answers '())
        (authority-records '())
        (additional-records '())
        (offset 12))
    (dotimes (i qdcount)
      (multiple-value-bind (name next-offset)
          (read-dns-name packet offset)
        (setf offset next-offset)
        (let ((type (decode-dns-type (ub16ref/be packet offset)))
              (class (decode-dns-class (ub16ref/be packet (+ offset 2)))))
          (incf offset 4)
          (push (list name type class) questions))))
    (flet ((decode-resource-record ()
             (multiple-value-bind (name next-offset)
                 (read-dns-name packet offset)
               (setf offset next-offset)
               (let ((type (decode-dns-type (ub16ref/be packet offset)))
                     (class (decode-dns-class (ub16ref/be packet (+ offset 2))))
                     (ttl (ub32ref/be packet (+ offset 4)))
                     (data-len (ub16ref/be packet (+ offset 8))))
                 (incf offset 10)
                 (prog1
                     (list* name type class ttl (decode-resource-record-data type class packet offset data-len))
                   (incf offset data-len))))))
      (dotimes (i ancount)
        (push (decode-resource-record) answers))
      (dotimes (i nscount)
        (push (decode-resource-record) authority-records))
      (dotimes (i arcount)
        (push (decode-resource-record) additional-records))
      (values id flags
              (reverse questions)
              (reverse answers)
              (reverse authority-records)
              (reverse additional-records)))))

(defun dns-request (domain)
  (dotimes (i 3) ; UDP is unreliable.
    (dolist (server *dns-servers*)
      (let ((id (random (expt 2 16))))
        (with-udp-connection (conn server +dns-port+)
          (send (build-dns-packet id +dns-standard-query+
                                  :questions `((,domain :a :in)))
                conn)
          (let ((response (receive conn 10)))
            (when response
              (multiple-value-bind (rx-id flags questions answers authority-rrs additional-rrs)
                  (decode-dns-packet response)
                (when (eql rx-id id)
                  (dolist (a answers)
                    (when (eql (second a) :a)
                      (return-from dns-request (fifth a)))))))))))))

;;; High-level address resolution.

(defvar *hosts*
  ;; FIXME: need a loopback route.
  `(("localhost" ,(make-ipv4-address 10 0 2 15))))

(defun resolve-address (address &optional (errorp t))
  (cond ((listp address)
         (apply 'sys.net::make-ipv4-address address))
        ((stringp address)
         (or
          ;; 1. Try to parse it as an IP address.
          (ignore-errors
            (parse-ipv4-address address))
          ;; 2. Look in the hosts table.
          (second (assoc address *hosts* :test 'string-equal))
          ;; 3. Finally do a DNS lookup.
          (ignore-errors
            (dns-request address))
          (when errorp
            (error "Unknown host ~S." address))))
        (t address)))
