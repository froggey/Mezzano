;;;; Copyright (c) 2017 Eugene Zaikonnikov <eugene@funcall.org>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.network.dhcp)

(defconstant +magic-cookie+ #x63825363)
(defconstant +ipv4-broadcast-source+ (mezzano.network.ip:make-ipv4-address #x00000000))
(defconstant +ipv4-broadcast-local-network+ (mezzano.network.ip:make-ipv4-address #xffffffff))
(defconstant +dhcp-client-port+ 68)
(defconstant +dhcp-server-port+ 67)

(defconstant +dhcp-discover+ 1)
(defconstant +dhcp-offer+ 2)
(defconstant +dhcp-request+ 3)
(defconstant +dhcp-ack+ 5)
(defconstant +dhcp-nak+ 6)

(defconstant +opt-dhcp-server+ 54)
(defconstant +opt-ntp-server+ 4)
(defconstant +opt-netmask+ 1)
(defconstant +opt-router+ 3)
(defconstant +opt-dns-servers+ 6)
(defconstant +opt-lease-time+ 51)
(defconstant +opt-ip-address+ 50)
(defconstant +opt-dhcp-message-type+ 53)
(defconstant +opt-tftp-server+ 66)
(defconstant +opt-parameter-request-list+ 55)
(defconstant +opt-host-name+ 12)
(defconstant +opt-end+ 255)

(defun build-dhcp-packet (&key mac-address options (siaddr 0))
  (assert (typep mac-address '(simple-array (unsigned-byte 8) (6))))
  (let ((packet (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref packet 0) #x01 ;OP
          (aref packet 1) #x01 ;HTYPE
          (aref packet 2) #x06 ;HLEN
          (aref packet 3) #x00 ;HOPS
          (ub32ref/be packet 4) #xdeadbeef ;XID
	  (ub16ref/be packet 8) #x0000 ;SECS
	  (ub16ref/be packet 10) #x8000 ;FLAGS
          (ub32ref/be packet 12) #x00000000 ;CIADDR
          (ub32ref/be packet 16) #x00000000 ;YIADDR
          (ub32ref/be packet 20) siaddr
          (ub32ref/be packet 24) #x00000000 ;GIADDR
	  (subseq packet 28) mac-address
	  (ub32ref/be packet 236) +magic-cookie+)
    (loop for pos = 240 then (+ pos (length option))
       for option in options
       do (setf (subseq packet pos) option))
    packet))

(defun decodce-dhcp-option (buffer position)
  "Returns (VALUES type value next-option-position)"
  (if (zerop (aref buffer position))
      (values 0 nil (1+ position))
      (values (aref buffer position)
	      (subseq buffer (+ position 2) (+ (+ position 2) (aref buffer (1+ position))))
	      (+ position 2 (aref buffer (1+ position))))))

(defun decode-all-options (buffer)
  (loop with pos = 240
     with padcount = 0
     while (and (< padcount 4) (< pos (length buffer)))
     appending (multiple-value-bind (type value newpos) (decode-dhcp-option buffer pos)
		 (setf pos newpos)
		 (cond ((zerop type) (incf padcount) (list))
		       (t (setf padcount 0) (list (cons type value)))))))

(defun make-dhcp-option (type value)
  (let* ((size (cond ((zerop type) -1)
		     ((arrayp value) (length value))
		     ((typep value '(unsigned-byte 8)) 1)
		     (t (cerror "Invalid DHCP option format ~A ~A" type value))))
	 (option (make-array (+ 2 size) :element-type '(unsigned-byte 8) :initial-element 0)))
    (when (zerop size)
      (return option))
    (setf (aref option 0) type
	  (aref option 1) size)
    (if (= 1 size)
	(setf (aref option 2) value)	;assume scalar
	(setf (subseq option 2) value))
    option))

(defun send-broadcast-dhcp-packet (sequence interface)
  (let* ((header (make-array 8 :element-type '(unsigned-byte 8)))
         (packet (list header sequence)))
    (setf (ub16ref/be header 0) +dhcp-client-port+
          (ub16ref/be header 2) +dhcp-server-port+
          (ub16ref/be header 4) (sys.net:packet-length packet)
          (ub16ref/be header 6) 0)
    (mezzano.network.ethernet:transmit-ethernet-packet
     interface mezzano.network.ethernet:*ethernet-broadcast* mezzano.network.ethernet:+ethertype-ipv4+
     (mezzano.network.ip::assemble-ipv4-packet +ipv4-broadcast-source+
			   +ipv4-broadcast-local-network+
			   mezzano.network.ip:+ip-protocol-udp+
			   packet))))

(defun dhcp-send (options &key (siaddr 0))
  (let* ((iface (first mezzano.driver.network-card::*nics*))	 
	 (packet (build-dhcp-packet :mac-address (mezzano.network.ethernet:ethernet-mac iface) :options options :siaddr siaddr)))
    (send-broadcast-dhcp-packet packet iface)))

(defun get-option (alist option)
  (cdr (find option alist :key #'car)))

(defun acquire-lease ()
  (let ((connection (make-instance 'mezzano.network.udp::udp4-connection
				   :remote-address +ipv4-broadcast-source+ ;;not necessary, just to avoid the stack down choking
				   :remote-port +dhcp-server-port+
				   :local-address +ipv4-broadcast-local-network+
				   :local-port +dhcp-client-port+)))
    (mezzano.supervisor:with-mutex (mezzano.network.udp::*udp-connection-lock*)
      (push connection mezzano.network.udp::*udp-connections*))
    (dhcp-send (list (make-dhcp-option 53 :size 1 :value 1)
		     (make-dhcp-option 55 :size 5 :value #(1 4 3 15 6))))
    (unwind-protect
	 (let* ((offer (sys.net:receive connection 4))
		(siaddr (ub32ref/be offer 20))
		(yiaddr (ub32ref/be offer 16))
		(oaddr (make-array 4 :element-type '(unsigned-byte 8)))
		(options (decode-all-options offer))
		(dhcpserver (get-option options +opt-dhcp-server+)))
	   (setf (ub32ref/be oaddr 0) yiaddr)
	   (if (zerop yiaddr)
	       nil
	       (progn
		 (dhcp-send (list (make-dhcp-option +opt-dhcp-message-type+ +dhcp-request+)
				  (make-dhcp-option +opt-ip-address+ oaddr)
				  (make-dhcp-option +opt-dhcp-server+ dhcpserver))
			    :siaddr siaddr)
		 (let* ((ack (sys.net:receive connection 4))
			(ack-options (decode-all-options ack))
			(confirmation (get-option ack-options +opt-dhcp-message-type+)))
		   (if (= confirmation +dhcp-ack+)
		       (values yiaddr (get-option options +opt-netmask+) (get-option options +opt-router+)
			       (get-option options +opt-dns-servers+) (get-option options +opt-dhcp-server+)
			       (get-option options +opt-ntp-server+))
		       nil)))))
      (sys.net:disconnect connection))))
