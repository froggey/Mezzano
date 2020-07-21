;;;; ARP

(in-package :mezzano.network.arp)

(defconstant +arp-op-request+ 1)
(defconstant +arp-op-reply+ 2)

(defconstant +arp-hrd-ethernet+ 1)

(defvar *arp-expiration-time* 600)
(defvar *arp-expiration-timer* nil)
;;; The ARP table is a list of lists. Each list holds:
;;; (protocol-type protocol-address network-address expiration-time)
(defvar *arp-table* '())
(defvar *arp-lock* (mezzano.supervisor:make-mutex "ARP routing table."))

(defun update-arp-expiration-timer ()
  (if *arp-table*
      (loop :with min-time := (fourth (car *arp-table*))
            :for arp :in *arp-table*
            :for arp-expiration-time := (fourth arp)
            :while (< arp-expiration-time min-time)
            :do (setf min-time arp-expiration-time)
            :finally (mezzano.supervisor:timer-arm (- min-time (get-universal-time))
                                                   *arp-expiration-timer*))
      (mezzano.supervisor:timer-disarm *arp-expiration-timer*)))

(defmethod mezzano.network.ethernet:ethernet-receive
    ((ethertype (eql mezzano.network.ethernet:+ethertype-arp+))
     interface packet start end)
  (let* ((htype (ub16ref/be packet 14))
         (ptype (ub16ref/be packet 16))
         (hlen (aref packet 18))
         (plen (aref packet 19))
         (oper (ub16ref/be packet 20))
         (sha-start 22)
         (spa-start (+ sha-start hlen))
         (tha-start (+ spa-start plen))
         (tpa-start (+ tha-start hlen))
         (merge-flag nil)
         (interface-address (mezzano.network.ip:ipv4-interface-address interface nil))
         (address (and interface-address
                       (mezzano.network.ip::ipv4-address-address interface-address))))
    ;; Ethernet hardware type and IPv4.
    (when (and (eql htype +arp-hrd-ethernet+) (eql hlen 6)
               (eql ptype mezzano.network.ethernet:+ethertype-ipv4+) (eql plen 4))
      (let ((spa (ub32ref/be packet spa-start))
            (tpa (ub32ref/be packet tpa-start)))
        ;; If the pair <protocol type, sender protocol address> is
        ;; already in my translation table, update the sender
        ;; hardware address field of the entry with the new
        ;; information in the packet and set Merge_flag to true.
        (dolist (e *arp-table*)
          (when (and (eql (first e) ptype)
                     (eql (second e) spa))
            (mezzano.supervisor:with-mutex (*arp-lock*)
              (setf (third e) (subseq packet sha-start spa-start)
                    (fourth e) (+ *arp-expiration-time* (get-universal-time))
                    merge-flag t))
            #+(or)
            (update-arp-expiration-timer)
            (return)))
        (when (and address (eql tpa address))
          (unless merge-flag
            (mezzano.supervisor:with-mutex (*arp-lock*)
              (push (list ptype spa (subseq packet sha-start spa-start)
                          (+ *arp-expiration-time* (get-universal-time)))
                    *arp-table*))
            #+(or)
            (update-arp-expiration-timer))
          (when (eql oper +arp-op-request+)
            ;; Copy source hardware address to dest MAC and target h/w address.
            (dotimes (i 6)
              (setf (aref packet i) (aref packet (+ sha-start i))
                    (aref packet (+ tha-start i)) (aref packet (+ sha-start i))))
            ;; Copy source protocol address to target protocol address.
            (dotimes (i plen)
              (setf (aref packet (+ tpa-start i)) (aref packet (+ spa-start i))))
            ;; Set source hardware address and source MAC to the interface's MAC.
            (let ((mac (mezzano.network.ethernet:ethernet-mac interface)))
              (dotimes (i 6)
                (setf (aref packet (+ 6 i)) (aref mac i)
                      (aref packet (+ sha-start i)) (aref mac i))))
            (setf (ub32ref/be packet spa-start) address
                  (ub16ref/be packet 20) +arp-op-reply+)
            (mezzano.network.ethernet:transmit-packet interface (list (subseq packet 0 44))))))
      (mezzano.network.ip::arp-table-updated))))

(defun send-arp (interface ptype address)
  "Send an ARP packet out onto the wire."
  (unless (eql ptype mezzano.network.ethernet:+ethertype-ipv4+)
    (error "Unsupported protocol type ~S" ptype))
  (let ((packet (make-array 42 :element-type '(unsigned-byte 8)))
        (mac (mezzano.network.ethernet:ethernet-mac interface)))
    ;; Fill in various hardware address fields.
    (dotimes (i 6)
      ;; Ethernet destination.
      (setf (aref packet i) #xFF
            ;; Ethernet source.
            (aref packet (+ 6 i)) (aref mac i)
            ;; ARP source hardware address.
            (aref packet (+ 22 i)) (aref mac i)))
    ;; Set the source and target protocol addresses.
    (setf (ub32ref/be packet 28) (mezzano.network.ip::ipv4-address-address
                                  (mezzano.network.ip:ipv4-interface-address
                                   interface))
          (ub32ref/be packet 38) address
          ;; Various other fields.
          (ub16ref/be packet 12) mezzano.network.ethernet:+ethertype-arp+
          (ub16ref/be packet 14) +arp-hrd-ethernet+
          (ub16ref/be packet 16) mezzano.network.ethernet:+ethertype-ipv4+
          (aref packet 18) 6
          (aref packet 19) 4
          (ub16ref/be packet 20) +arp-op-request+)
    (mezzano.network.ethernet:transmit-packet interface (list packet))))

(defun arp-lookup (interface ptype address)
  "Convert ADDRESS to an Ethernet address.
Returns NIL if there is no entry currently in the cache, this will trigger a lookup over the interface."
  (when (equalp (mezzano.network.ip::ipv4-address-address
                 (mezzano.network.ip:ipv4-interface-address interface))
                address)
    (return-from arp-lookup (mezzano.network.ethernet:ethernet-mac interface)))
  ;; Scan the ARP table.
  (dolist (e *arp-table*)
    (when (and (eql (first e) ptype)
               (eql (second e) address))
      (return-from arp-lookup (third e))))
  (send-arp interface ptype address)
  nil)

(defun arp-expiration ()
  (let ((time (1+ (get-internal-real-time))))
    (mezzano.supervisor:with-mutex (*arp-lock*)
      (setf *arp-table* (remove-if #'(lambda (arp)
                                       (>= time (fourth arp)))
                                   *arp-table*)))
    (update-arp-expiration-timer)))
