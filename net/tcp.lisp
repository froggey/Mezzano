;;; TCP
;;;
;;; Transmission Control Protocol (TCP)
;;; https://datatracker.ietf.org/doc/html/rfc9293
;;; Improving TCP's Robustness to Blind In-Window Attacks
;;; https://datatracker.ietf.org/doc/html/rfc5961
;;;

(in-package :mezzano.network.tcp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TCP protocol constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TCP header offsets in octets
(defconstant +tcp4-header-source-port+ 0)
(defconstant +tcp4-header-destination-port+ 2)
(defconstant +tcp4-header-sequence-number+ 4)
(defconstant +tcp4-header-acknowledgment-number+ 8)
(defconstant +tcp4-header-flags-and-data-offset+ 12)
(defconstant +tcp4-header-window-size+ 14)
(defconstant +tcp4-header-checksum+ 16)
(defconstant +tcp4-header-urgent-pointer+ 18)
(defconstant +tcp4-header-options+ 20)

;;; TCP control flags (bitmask values)
(defconstant +tcp4-flag-fin+ #b00000001 "Finish flag (RFC 793)")
(defconstant +tcp4-flag-syn+ #b00000010 "Synchronize flag (RFC 793)")
(defconstant +tcp4-flag-rst+ #b00000100 "Reset flag (RFC 793)")
(defconstant +tcp4-flag-psh+ #b00001000 "Push flag (RFC 793)")
(defconstant +tcp4-flag-ack+ #b00010000 "Acknowledgment flag (RFC 793)")
(defconstant +tcp4-flag-urg+ #b00100000 "Urgent flag (RFC 793)")
(defconstant +tcp4-flag-ece+ #b01000000 "ECN-Echo flag (RFC 3168)")
(defconstant +tcp4-flag-cwr+ #b10000000 "Congestion Window Reduced flag (RFC 3168)")

;;; TCP option types
(defconstant +tcp-option-eol+ 0 "End of options (RFC 793)")
(defconstant +tcp-option-nop+ 1 "No operation/Padding (RFC 793)")
(defconstant +tcp-option-mss+ 2 "Maximum Segment Size (RFC 793)")
(defconstant +tcp-option-mss-length+ 4 "MSS option length in octets")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Network configuration parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Wildcard address/port definitions
;; DEFPARAMETER used for +ip-wildcard+ due to cross-compiler constraints
(defparameter +ip-wildcard+ (mezzano.network.ip:make-ipv4-address "0.0.0.0"))
(defconstant +port-wildcard+ 0)

;;; Connection time management
(defparameter *tcp-connect-timeout* 10 "Connection establishment timeout in seconds")
(defparameter *tcp-initial-retransmit-time* 1 "Initial RTO value in seconds (RFC 6298)")
(defparameter *minimum-rto* 1 "Minimum retransmission timeout in seconds (RFC 6298)")
(defparameter *maximum-rto* 60 "Maximum retransmission timeout in seconds (RFC 6298)")
(defparameter *msl* 120 "Maximum Segment Lifetime in seconds (RFC 793)")

;;; Window and Segment Sizing
(defparameter *initial-window-size* 8192 "Initial congestion window size in octets")
(defparameter *default-snd.mss* 536 "Default maximum segment size in octets")
;; TODO: Make it less hacky
(defparameter *rcv.mss* (- (mezzano.driver.network-card:mtu (first (mezzano.sync:watchable-set-items mezzano.driver.network-card::*nics*))) 40) "Maximum segment size in octets")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging and testing parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *netmangler-force-local-retransmit* nil
  "When T, force all data segments through retransmit queue (simulates packet loss)")
(defparameter *netmangler-iss* nil
  "Force the ISS to this value.
Set to a value near 2^32 to test SND sequence number wrapping.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype tcp-connection-state ()
  "Possible states that a TCP connection can have."
  '(member
    :closed
    :syn-sent
    :syn-received
    :established
    :close-wait
    :last-ack
    :fin-wait-1
    :fin-wait-2
    :closing
    :time-wait))

(deftype tcp-port-number ()
  '(unsigned-byte 16))

(deftype tcp-sequence-number ()
  '(unsigned-byte 32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sequence Number Arithmetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +u32 (x y)
  "X + Y modulo 2^32 arithmetic"
  (ldb (byte 32 0) (+ x y)))

(defun -u32 (x y)
  "X - Y modulo 2^32 arithmetic"
  (ldb (byte 32 0) (- x y)))

(defun <u32 (x y)
  "X < Y modulo 2^32 arithmetic"
  (or (and (< x y)
           (< (- y x) (ash 1 31)))
      (and (> x y)
           (> (- x y) (ash 1 31)))))

(defun >u32 (x y)
  "X > Y modulo 2^32 arithmetic"
  (<u32 y x))

(defun <=u32 (x y)
  "X <= Y modulo 2^32 arithmetic"
  (or (= x y)
      (<u32 x y)))

(defun >=u32 (x y)
  "X >= Y modulo 2^32 arithmetic"
  (<=u32 y x))

(defun =< (a b c)
  "a <= b <= c modulo 2^32 arithmetic"
  (if (< a c)
      (<= a b c)
      (or (<= a b)
          (<= b c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TCP Listener
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *tcp-listeners* nil "List of active TCP listeners")
(defvar *tcp-listener-lock* (mezzano.supervisor:make-mutex "TCP listener list"))

;; FIXME: Inbound connections need to timeout if state :syn-received don't change.
;; TODO: Better locking on this is probably needed. It looks like it is accesed
;; from the network serial queue and from user threads.
(defclass tcp-listener ()
  ((local-port :reader tcp-listener-local-port
               :initarg :local-port
               :type tcp-port-number)
   (local-ip :reader tcp-listener-local-ip
             :initarg :local-ip
             :type mezzano.network.ip::ipv4-address)
   (pending-connections :reader tcp-listener-pending-connections
                        :initarg :pending-connections
                        :type hash-table)
   (connections :reader tcp-listener-connections
                :initarg :connections
                :type mezzano.sync:mailbox)
   (n-pending-connections :accessor tcp-listener-n-pending-connections
                          :initarg :n-pending-connections
                          :type integer)
   (backlog :reader tcp-listener-backlog
            :initarg :backlog))
  (:default-initargs
   :pending-connections (make-hash-table :test 'equalp :synchronized t)
   :connections (mezzano.sync:make-mailbox :name "TCP Listener")
   :n-pending-connections 0))

(defmethod mezzano.sync:get-object-event ((object tcp-listener))
  (mezzano.sync:get-object-event (tcp-listener-connections object)))

(defmethod mezzano.network:local-endpoint ((object tcp-listener))
  (values (tcp-listener-local-ip object)
          (tcp-listener-local-port object)))

(defmethod print-object ((instance tcp-listener) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~A:~A"
            (tcp-listener-local-ip instance)
            (tcp-listener-local-port instance))))

(defun get-tcp-listener-without-lock (local-ip local-port)
  (dolist (listener *tcp-listeners*)
    (when (and (or (mezzano.network.ip:address-equal
                    (tcp-listener-local-ip listener) local-ip)
                   (mezzano.network.ip:address-equal
                    (tcp-listener-local-ip listener) +ip-wildcard+))
               (eql (tcp-listener-local-port listener) local-port))
      (return listener))))

(defun get-tcp-listener (local-ip local-port)
  (mezzano.supervisor:with-mutex (*tcp-listener-lock*)
    (get-tcp-listener-without-lock local-ip local-port)))

(defun find-available-port (port-check)
  (loop :for port := (+ (random 32768) 32768)
        :unless (funcall port-check port)
          :do (return port)))

(defun allocate-listener-local-port (source-address)
  (find-available-port
   #'(lambda (local-port)
       (get-tcp-listener-without-lock source-address local-port))))

(defun tcp-listen (local-host local-port &key backlog)
  (let* ((local-ip (mezzano.network:resolve-address local-host))
         (source-address (if (mezzano.network.ip:address-equal local-ip +ip-wildcard+)
                             +ip-wildcard+
                             (mezzano.network.ip:ipv4-interface-address
                              (nth-value 1 (mezzano.network.ip:ipv4-route local-ip))))))
    (mezzano.supervisor:with-mutex (*tcp-listener-lock*)
      (let* ((local-port (cond ((eql local-port +port-wildcard+)
                                (allocate-listener-local-port source-address))
                               ((get-tcp-listener-without-lock source-address local-port)
                                (error "Server already listening on port ~D" local-port))
                               (t local-port)))
             (listener (make-instance 'tcp-listener
                                      :backlog backlog
                                      :local-port local-port
                                      :local-ip source-address)))
        (push listener *tcp-listeners*)
        listener))))

(defun tcp-accept (listener &key (wait-p t) element-type external-format)
  (let ((connection (mezzano.sync:mailbox-receive
                     (tcp-listener-connections listener)
                     :wait-p wait-p)))
    (when connection
      (when (tcp-listener-backlog listener)
        (decf (tcp-listener-n-pending-connections listener)))
      (tcp4-accept-connection connection
                              :element-type element-type
                              :external-format external-format))))

(defun close-tcp-listener (listener)
  (mezzano.supervisor:with-mutex (*tcp-listener-lock*)
    (setf *tcp-listeners* (remove listener *tcp-listeners*)))
  (loop :for connection :being :the :hash-values :of (tcp-listener-pending-connections listener)
        :do (with-tcp-connection-locked connection
              (abort-connection connection)))
  (loop :for connection :in (mezzano.sync:mailbox-flush (tcp-listener-connections listener))
        :do (with-tcp-connection-locked connection
              (abort-connection connection))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TCP Connection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *tcp-connections* nil "List of active TCP connections")
(defvar *tcp-connection-lock* (mezzano.supervisor:make-mutex "TCP connection list"))

(defclass tcp-connection ()
  ((%state :accessor tcp-connection-state
           :initarg :state
           :type tcp-connection-state)
   ;; Addressing
   (%local-port :reader tcp-connection-local-port
                :initarg :local-port
                :type tcp-port-number)
   (%local-ip :reader tcp-connection-local-ip
              :initarg :local-ip
              :type mezzano.network.ip::ipv4-address)
   (%remote-port :reader tcp-connection-remote-port
                 :initarg :remote-port
                 :type tcp-port-number)
   (%remote-ip :reader tcp-connection-remote-ip
               :initarg :remote-ip
               :type mezzano.network.ip::ipv4-address)
   ;; Send sequence space
   (%snd.nxt :accessor tcp-connection-snd.nxt
             :initarg :snd.nxt
             :type tcp-sequence-number)
   (%snd.una :accessor tcp-connection-snd.una
             :initarg :snd.una)
   (%snd.wnd :accessor tcp-connection-snd.wnd
             :initarg :snd.wnd)
   (%max.snd.wnd :accessor tcp-connection-max.snd.wnd
                 :initarg :max.snd.wnd)
   ;; Receive sequence space
   (%rcv.nxt :accessor tcp-connection-rcv.nxt
             :initarg :rcv.nxt
             :type tcp-sequence-number)
   (%rcv.wnd :accessor tcp-connection-rcv.wnd
             :initarg :rcv.wnd)
   ;; Flow control and options
   (%snd.mss :accessor tcp-connection-snd.mss
             :initarg :snd.mss)
   (%snd.wl1 :accessor tcp-connection-snd.wl1
             :initarg :snd.wl1)
   (%snd.wl2 :accessor tcp-connection-snd.wl2
             :initarg :snd.wl2)
   ;; Data buffers
   (%rx-data :accessor tcp-connection-rx-data
             :initform '())
   ;; Doesn't need to be synchronized, only accessed from the network serial queue.
   (%rx-data-unordered :reader tcp-connection-rx-data-unordered
                       :initform (make-hash-table))
   ;; Retransmission
   (%retransmit-queue :accessor tcp-connection-retransmit-queue
                      :initform '())
   (%retransmit-timer :reader tcp-connection-retransmit-timer)
   (%retransmit-source :reader tcp-connection-retransmit-source)
   (%rto :accessor tcp-connection-rto
         :initarg :rto)
   ;; RTT estimation
   (%srtt :accessor tcp-connection-srtt
          :initarg :srtt)
   (%rttvar :accessor tcp-connection-rttvar
            :initarg :rttvar)
   (%last-ack-time :accessor tcp-connection-last-ack-time
                   :initarg :last-ack-time)
   ;; Connection management
   (%lock :reader tcp-connection-lock)
   (%cvar :reader tcp-connection-cvar)
   (%receive-event :reader tcp-connection-receive-event)
   (%pending-error :accessor tcp-connection-pending-error
                   :initform nil)
   (%timeout-timer :reader tcp-connection-timeout-timer)
   (%timeout-source :reader tcp-connection-timeout-source)
   (%timeout :initarg :timeout :reader tcp-connection-timeout)
   (%boot-id :reader tcp-connection-boot-id
             :initarg :boot-id))
  (:default-initargs
   :max.snd.wnd 0
   :snd.mss *default-snd.mss*
   :rto *tcp-initial-retransmit-time*
   :srtt nil
   :rttvar nil
   :last-ack-time nil
   :timeout nil
   :boot-id nil))

(defun (setf tcp-connection-timeout) (timeout connection)
  (with-tcp-connection-locked connection
    (setf (slot-value connection '%timeout) timeout)
    (update-timeout-timer connection)))

(defun arm-retransmit-timer (connection)
  (mezzano.supervisor:timer-arm (tcp-connection-rto connection)
                                (tcp-connection-retransmit-timer connection))
  (values))

(defun disarm-retransmit-timer (connection)
  (mezzano.supervisor:timer-disarm (tcp-connection-retransmit-timer connection))
  (values))

(defun retransmit-timer-handler (connection)
  (unless (mezzano.supervisor:timer-expired-p
           (tcp-connection-retransmit-timer connection))
    ;; Timer is either still pending or isn't actually running.
    ;; This can happen if the timer expires but some other task reconfigures
    ;; a new retransmit time.
    (return-from retransmit-timer-handler))
  (mezzano.supervisor:with-mutex ((tcp-connection-lock connection))
    ;; Disarm it so it stops triggering the source
    (disarm-retransmit-timer connection)
    ;; What're we retransmitting?
    (ecase (tcp-connection-state connection)
      (:syn-sent
       (let ((seq (-u32 (tcp-connection-snd.nxt connection) 1)))
         (tcp4-send-packet connection seq 0 nil :ack-p nil :syn-p t)
         (arm-retransmit-timer connection)))
      (:syn-received
       (let ((iss (tcp-connection-snd.una connection))
             (irs (tcp-connection-rcv.nxt connection)))
         (tcp4-send-packet connection iss irs nil :syn-p t)
         (arm-retransmit-timer connection)))
      ((:established :close-wait :last-ack :fin-wait-1 :fin-wait-2 :closing :time-wait)
       (let ((packet (first (tcp-connection-retransmit-queue connection))))
         (apply #'tcp4-send-packet connection packet)
         (setf (tcp-connection-rto connection)
               (min *maximum-rto* (* 2 (tcp-connection-rto connection))))
         (arm-retransmit-timer connection)))
      (:closed nil))))

(defun arm-timeout-timer (seconds connection)
  (mezzano.supervisor:timer-arm seconds (tcp-connection-timeout-timer connection))
  (values))

(defun disarm-timeout-timer (connection)
  (mezzano.supervisor:timer-disarm (tcp-connection-timeout-timer connection))
  (values))

(defun timeout-timer-handler (connection)
  (unless (mezzano.supervisor:timer-expired-p
           (tcp-connection-timeout-timer connection))
    ;; Timer is either still pending or isn't actually running.
    ;; This can happen if the timer expires but some other task reconfigures
    ;; a new timeout time.
    (return-from timeout-timer-handler))
  (mezzano.supervisor:with-mutex ((tcp-connection-lock connection))
    ;; Disarm it so it stops triggering the source
    (disarm-timeout-timer connection)
    (set-connection-error 'connection-timed-out connection)
    (mezzano.supervisor:condition-notify (tcp-connection-cvar connection) t)
    (case (tcp-connection-state connection)
      ((:syn-sent :syn-received :time-wait)
       (detach-tcp-connection connection))
      (:closed)
      (t
       (close-connection connection)))))

(defmethod initialize-instance :after ((instance tcp-connection) &key)
  (setf (slot-value instance '%lock) (mezzano.supervisor:make-mutex instance)
        (slot-value instance '%cvar) (mezzano.supervisor:make-condition-variable instance)
        (slot-value instance '%receive-event) (mezzano.supervisor:make-event :name `(:data-available ,instance))
        (slot-value instance '%retransmit-timer) (mezzano.supervisor:make-timer :name `(:tcp-retransmit ,instance))
        (slot-value instance '%timeout-timer) (mezzano.supervisor:make-timer :name `(:tcp-timeout ,instance)))
  (setf (slot-value instance '%retransmit-source)
        (mezzano.sync.dispatch:make-source (tcp-connection-retransmit-timer instance)
                                           (lambda ()
                                             (retransmit-timer-handler instance))
                                           :target net::*network-serial-queue*))
  (setf (slot-value instance '%timeout-source)
        (mezzano.sync.dispatch:make-source (tcp-connection-timeout-timer instance)
                                           (lambda ()
                                             (timeout-timer-handler instance))
                                           :target net::*network-serial-queue*)))

(defmethod print-object ((instance tcp-connection) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~A :local ~A:~A :remote ~A:~A"
            (tcp-connection-state instance)
            (tcp-connection-local-ip instance)
            (tcp-connection-local-port instance)
            (tcp-connection-remote-ip instance)
            (tcp-connection-remote-port instance))))

(defmacro with-tcp-connection-locked (connection &body body)
  `(mezzano.supervisor:with-mutex ((tcp-connection-lock ,connection) :resignal-errors t)
     ,@body))

(defun get-tcp-connection (remote-ip remote-port local-ip local-port)
  (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
    (dolist (connection *tcp-connections*)
      (when (and (mezzano.network.ip:address-equal
                  (tcp-connection-remote-ip connection)
                  remote-ip)
                 (eql (tcp-connection-remote-port connection) remote-port)
                 (mezzano.network.ip:address-equal
                  (tcp-connection-local-ip connection)
                  local-ip)
                 (eql (tcp-connection-local-port connection) local-port))
        (return connection)))))

(defmethod mezzano.network.ip:ipv4-receive ((protocol (eql mezzano.network.ip:+ip-protocol-tcp+)) packet local-ip remote-ip start end)
  (let* ((remote-port (ub16ref/be packet (+ start +tcp4-header-source-port+)))
         (local-port (ub16ref/be packet (+ start +tcp4-header-destination-port+)))
         (flags (ldb (byte 12 0)
                     (ub16ref/be packet (+ start +tcp4-header-flags-and-data-offset+))))
         (connection (get-tcp-connection remote-ip remote-port local-ip local-port))
         (listener (get-tcp-listener local-ip local-port)))
    (cond (connection
           (tcp4-connection-receive connection packet start end listener))
          ;; Drop unestablished connections if they surpassed listener backlog
          ((and listener
                (eql flags +tcp4-flag-syn+)
                (or (not (tcp-listener-backlog listener))
                    (< (tcp-listener-n-pending-connections listener)
                       (tcp-listener-backlog listener))))
           (when (tcp-listener-backlog listener)
             (incf (tcp-listener-n-pending-connections listener)))
           (let* ((irs (ub32ref/be packet (+ start +tcp4-header-sequence-number+)))
                  (iss (or *netmangler-iss*
                           (random #x100000000)))
                  (header-length (tcp-packet-header-length packet start end))
                  (connection (make-instance 'tcp-connection
                                             :state :syn-received
                                             :local-port local-port
                                             :local-ip local-ip
                                             :remote-port remote-port
                                             :remote-ip remote-ip
                                             :snd.nxt (+u32 iss 1)
                                             :snd.una iss
                                             :rcv.nxt (+u32 irs 1)
                                             :rcv.wnd *initial-window-size*
                                             :boot-id (mezzano.supervisor:current-boot-id))))
             (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
               (tcp-packet-options connection packet start header-length)
               (push connection *tcp-connections*))
             (setf (gethash connection (tcp-listener-pending-connections listener))
                   connection)
             (setf (tcp-connection-last-ack-time connection)
                   (get-internal-run-time))
             (unless *netmangler-force-local-retransmit*
               (tcp4-send-packet connection iss (+u32 irs 1) nil :syn-p t))
             (arm-retransmit-timer connection)
             (arm-timeout-timer *tcp-connect-timeout* connection)))
          ((logtest flags +tcp4-flag-rst+)) ; Do nothing for resets addressed to nobody.
          ((logtest flags +tcp4-flag-fin+)) ; Do nothing for finish since the SEG.SEQ cannot be validated
          (t
           (let* ((seq (if (logtest flags +tcp4-flag-ack+)
                           (tcp-packet-acknowledgment-number packet start end)
                           0))
                  (ack (+u32 (tcp-packet-sequence-number packet start end)
                             (tcp-packet-data-length packet start end)))
                  (packet (assemble-tcp4-packet local-ip local-port
                                                remote-ip remote-port
                                                seq ack
                                                0
                                                nil
                                                :ack-p (logtest flags +tcp4-flag-ack+)
                                                :rst-p t)))
             (mezzano.network.ip:transmit-ipv4-packet
              local-ip remote-ip
              mezzano.network.ip:+ip-protocol-tcp+ packet))))))

(defun tcp4-accept-connection (connection &key element-type external-format)
  (cond ((or (not element-type)
             (sys.int::type-equal element-type 'character))
         (make-instance 'tcp-stream
                        :connection connection
                        :external-format (sys.int::make-external-format
                                          (or element-type 'character)
                                          (or external-format :default)
                                          :eol-style :crlf)))
        ((sys.int::type-equal element-type '(unsigned-byte 8))
         (assert (or (not external-format)
                     (eql external-format :default)))
         (make-instance 'tcp-octet-stream
                        :connection connection))
        (t
         (error "Unsupported element type ~S" element-type))))

;; Note - must be called on the network queue.
(defun detach-tcp-connection (connection)
  ;; Disarming here is doubly important.
  ;; 1) It stops the timer from hanging around if it was active.
  ;; 2) If the source handler is pending, then it'll return immediately.
  (setf (tcp-connection-state connection) :closed)
  (disarm-retransmit-timer connection)
  (disarm-timeout-timer connection)
  (mezzano.sync.dispatch:cancel (tcp-connection-retransmit-source connection))
  (mezzano.sync.dispatch:cancel (tcp-connection-timeout-source connection))
  (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
    (setf *tcp-connections* (remove connection *tcp-connections*))))

(defun append-data-packet (connection entry)
  (if (tcp-connection-rx-data connection)
      (setf (cdr (last (tcp-connection-rx-data connection)))
            (list entry))
      (setf (tcp-connection-rx-data connection)
            (list entry))))

(defun update-unordered-data (connection)
  "Try to find any out-of-order data in CONNECTION that is now in-order."
  ;; Check if the next packet is in tcp-connection-rx-data-unordered
  (loop
    :for (packet start end data-length)
      := (gethash (tcp-connection-rcv.nxt connection)
                  (tcp-connection-rx-data-unordered connection))
    :always packet
    :do (remhash (tcp-connection-rcv.nxt connection)
                 (tcp-connection-rx-data-unordered connection))
    :do (append-data-packet connection (list packet start end))
    :do (setf (tcp-connection-rcv.nxt connection)
              (+u32 (tcp-connection-rcv.nxt connection) data-length))))

(defun tcp4-receive-data (connection data-length end header-length packet seq start)
  (cond ((= seq (tcp-connection-rcv.nxt connection))
         ;; Send data to the user layer
         (append-data-packet connection (list packet (+ start header-length) end))
         (setf (tcp-connection-rcv.nxt connection)
               (+u32 (tcp-connection-rcv.nxt connection) data-length))
         (update-unordered-data connection)
         (setf (mezzano.supervisor:event-state
                (tcp-connection-receive-event connection))
               t))
        ;; Add future packet to tcp-connection-rx-data-unordered
        ((>u32 seq (tcp-connection-rcv.nxt connection))
         (unless (gethash seq (tcp-connection-rx-data-unordered connection))
           (setf (gethash seq (tcp-connection-rx-data-unordered connection))
                 (list packet (+ start header-length) end data-length)))))
  (when (<=u32 seq (tcp-connection-rcv.nxt connection))
    ;; Don't check *netmangler-force-local-retransmit* here,
    ;; or no acks will ever get through.
    (tcp4-send-ack connection)))

(defun tcp-packet-sequence-number (packet start end)
  (declare (ignore end))
  (ub32ref/be packet (+ start +tcp4-header-sequence-number+)))

(defun tcp-packet-acknowledgment-number (packet start end)
  (declare (ignore end))
  (ub32ref/be packet (+ start +tcp4-header-acknowledgment-number+)))

(defun tcp-packet-flags (packet start end)
  (declare (ignore end))
  (ldb (byte 12 0) (ub16ref/be packet (+ start +tcp4-header-flags-and-data-offset+))))

(defun tcp-packet-header-length (packet start end)
  (declare (ignore end))
  (* (ldb (byte 4 12) (ub16ref/be packet (+ start +tcp4-header-flags-and-data-offset+))) 4))

(defun tcp-packet-window-size (packet start end)
  (declare (ignore end))
  (ub16ref/be packet (+ start +tcp4-header-window-size+)))

(defun tcp-packet-data-length (packet start end)
  (- end (+ start (tcp-packet-header-length packet start end))))

(defun tcp-packet-options (connection packet start header-length)
  (when (> header-length +tcp4-header-options+)
    (loop :with offset := (+ start +tcp4-header-options+)
          :with end := (+ start header-length)
          :always (< offset end)
          :do (let ((kind (aref packet offset)))
                (cond ((= kind +tcp-option-eol+)
                       (return))
                      ((= kind +tcp-option-nop+)
                       (incf offset))
                      (t
                       (let ((length (aref packet (1+ offset))))
                         (when (> 2 length end)
                           ;; Ignore silly options and partial options
                           (return))
                         (when (= kind +tcp-option-mss+)
                           (when (= length +tcp-option-mss-length+)
                             (let ((snd.mss (ub16ref/be packet (+ offset 2))))
                               (setf (tcp-connection-snd.mss connection) snd.mss))))
                         (incf offset length))))))))

(defun acceptable-segment-p (connection seg.seq seg.len)
  "If (RCV.NXT <= SEG.SEQ < RCV.NXT+RCV.WND) the segment is inside the receive window."
  (let ((rcv.wnd (tcp-connection-rcv.wnd connection))
        (rcv.nxt (tcp-connection-rcv.nxt connection)))
    (if (eql rcv.wnd 0)
        (and (eql seg.len 0)
             (eql seg.seq rcv.nxt))
        ;; Arithmetic here is not wrapping, so as to avoid wrap-around problems.
        (and (<= rcv.nxt seg.seq)
             (< seg.seq (+ rcv.nxt rcv.wnd))
             (or (eql seg.len 0)
                 (let ((seq-end (+ seg.seq seg.len -1)))
                   (and (<= rcv.nxt seq-end) (< seq-end (+ rcv.nxt rcv.wnd)))))))))

(defun acceptable-ack-p (connection seg.ack)
  "If (SND.UNA < SEG.ACK <= SND.NXT) the ACK is acceptable."
  (if (< (tcp-connection-snd.una connection)
         (tcp-connection-snd.nxt connection))
      (and (< (tcp-connection-snd.una connection) seg.ack)
           (<= seg.ack (tcp-connection-snd.nxt connection)))
      ;; Sequence numbers wrapped.
      (or (< (tcp-connection-snd.una connection) seg.ack)
          (<= seg.ack (tcp-connection-snd.nxt connection)))))

(defun rfc5961-mitigation-check-p (connection seg.ack)
  "If ((SND.UNA - MAX.SND.WND) =< SEG.ACK =< SND.NXT) the ACK is acceptable."
  (let ((x (- (tcp-connection-snd.una connection)
              (tcp-connection-max.snd.wnd connection))))
    (=< x seg.ack (tcp-connection-snd.nxt connection))))

(defun update-window (connection seg.wnd seg.seq seg.ack)
  (when (> seg.wnd (tcp-connection-max.snd.wnd connection))
    (setf (tcp-connection-max.snd.wnd connection) seg.wnd))
  (setf (tcp-connection-snd.wnd connection) seg.wnd
        (tcp-connection-snd.wl1 connection) seg.seq
        (tcp-connection-snd.wl2 connection) seg.ack))

(defun update-timeout-timer (connection)
  (case (tcp-connection-state connection)
    ((:fin-wait-1 :fin-wait-2)
     (disarm-timeout-timer connection))
    ((:syn-sent :syn-received :established :closing)
     (disarm-timeout-timer connection)
     (let ((timeout (tcp-connection-timeout connection)))
       (when timeout
         (arm-timeout-timer timeout connection))))))

(defun initial-rtt-measurement (connection)
  (let ((delta-time (float (/ (- (get-internal-run-time) (tcp-connection-last-ack-time connection))
                              internal-time-units-per-second))))
    (setf (tcp-connection-srtt connection) delta-time
          (tcp-connection-rttvar connection) (/ delta-time 2))
    (setf (tcp-connection-rto connection)
          (min *maximum-rto*
               (max *minimum-rto*
                    (+ (tcp-connection-srtt connection)
                       (max 0.01 (* 4 (tcp-connection-rttvar connection))))))
          (tcp-connection-last-ack-time connection) nil)))

(defun subsequent-rtt-measurement (connection)
  (let ((delta-time (float (/ (- (get-internal-run-time) (tcp-connection-last-ack-time connection))
                              internal-time-units-per-second))))
    (setf (tcp-connection-rttvar connection)
          (+ (* 0.75 (tcp-connection-rttvar connection))
             (* 0.25 (- (tcp-connection-srtt connection) delta-time))))
    (setf (tcp-connection-srtt connection)
          (+ (* 0.875 (tcp-connection-srtt connection))
             (* 0.125 delta-time)))
    (setf (tcp-connection-rto connection)
          (min *maximum-rto*
               (max *minimum-rto*
                    (+ (tcp-connection-srtt connection)
                       (max 0.01 (* 4 (tcp-connection-rttvar connection))))))
          (tcp-connection-last-ack-time connection) nil)))

(defun when-acceptable-ack-p (connection ack seq wnd)
  (when (acceptable-ack-p connection ack)
    (when (tcp-connection-last-ack-time connection)
      (subsequent-rtt-measurement connection))
    (setf (tcp-connection-snd.una connection) ack)
    ;; Remove from the retransmit queue any segments that were fully acknowledged by this ACK.
    (loop
      (when (endp (tcp-connection-retransmit-queue connection))
        (return))
      (let* ((rtx-start-seq (first (first (tcp-connection-retransmit-queue connection))))
             (rtx-end-seq (+u32 rtx-start-seq (length (third (first (tcp-connection-retransmit-queue connection)))))))
        (unless (and (=< (tcp-connection-snd.una connection)
                         rtx-start-seq
                         ack)
                     (=< (tcp-connection-snd.una connection)
                         rtx-end-seq
                         ack))
          ;; This segment not fully acked.
          (return)))
      (pop (tcp-connection-retransmit-queue connection)))
    (if (endp (tcp-connection-retransmit-queue connection))
        (disarm-retransmit-timer connection)
        (arm-retransmit-timer connection)))
  (when (and (=< (tcp-connection-snd.una connection) ack (tcp-connection-snd.nxt connection))
             (or (>u32 seq (tcp-connection-snd.wl1 connection))
                 (and (= seq (tcp-connection-snd.wl1 connection))
                      (>=u32 ack (tcp-connection-snd.wl2 connection)))))
    (update-window connection wnd seq ack)))

(defun tcp4-connection-receive (connection packet start end listener)
  ;; Don't use WITH-TCP-CONNECTION-LOCKED here. No errors should occur
  ;; in here, so this avoids truncating the backtrace with :resignal-errors.
  (mezzano.supervisor:with-mutex ((tcp-connection-lock connection))
    (let* ((seq (tcp-packet-sequence-number packet start end))
           (ack (tcp-packet-acknowledgment-number packet start end))
           (flags (tcp-packet-flags packet start end))
           (header-length (tcp-packet-header-length packet start end))
           (wnd (tcp-packet-window-size packet start end))
           (data-length (tcp-packet-data-length packet start end)))
      (ecase (tcp-connection-state connection)
        (:syn-sent
         (cond ((logtest flags +tcp4-flag-rst+)
                (when (and (acceptable-ack-p connection ack)
                           (eql seq (tcp-connection-rcv.nxt connection)))
                  (set-connection-error 'connection-reset connection)
                  (detach-tcp-connection connection)))
               ((and (logtest flags +tcp4-flag-ack+)
                     (not (acceptable-ack-p connection ack)))
                ;; Segment comes from an old connection
                (unless *netmangler-force-local-retransmit*
                  (tcp4-send-packet connection ack seq nil :ack-p nil :rst-p t)))
               ((and (logtest flags +tcp4-flag-ack+)
                     (logtest flags +tcp4-flag-syn+)
                     (eql ack (tcp-connection-snd.nxt connection)))
                ;; Active open
                (initial-rtt-measurement connection)
                (tcp-packet-options connection packet start header-length)
                (setf (tcp-connection-state connection) :established
                      (tcp-connection-rcv.nxt connection) (+u32 seq 1)
                      (tcp-connection-snd.una connection) ack)
                (update-window connection wnd seq ack)
                (unless *netmangler-force-local-retransmit*
                  (tcp4-send-ack connection))
                ;; Cancel retransmit
                (disarm-retransmit-timer connection)
                (disarm-timeout-timer connection))
               ((logtest flags +tcp4-flag-syn+)
                ;; Simultaneous open
                (tcp-packet-options connection packet start header-length)
                (setf (tcp-connection-state connection) :syn-received
                      (tcp-connection-rcv.nxt connection) (+u32 seq 1))
                (update-window connection wnd seq ack)
                (unless *netmangler-force-local-retransmit*
                  (tcp4-send-packet connection ack (tcp-connection-rcv.nxt connection) nil
                                    :syn-p t)))))
        (:syn-received
         (cond ((not (acceptable-segment-p connection seq data-length))
                (unless (logtest flags +tcp4-flag-rst+)
                  (challenge-ack connection)))
               ((logtest flags +tcp4-flag-rst+)
                (cond ((not (eql seq (tcp-connection-rcv.nxt connection)))
                       (challenge-ack connection))
                      ((and listener
                            (gethash connection (tcp-listener-pending-connections listener)))
                       ;; Connection comes from pasive OPEN
                       (remhash connection (tcp-listener-pending-connections listener))
                       (decf (tcp-listener-n-pending-connections listener))
                       (detach-tcp-connection connection))
                      (t
                       ;; Connection comes from active OPEN
                       (set-connection-error 'connection-refused connection)
                       (detach-tcp-connection connection))))
               ((logtest flags +tcp4-flag-syn+)
                (cond ((and listener
                            (gethash connection (tcp-listener-pending-connections listener)))
                       ;; Connection comes from pasive OPEN
                       (remhash connection (tcp-listener-pending-connections listener))
                       (decf (tcp-listener-n-pending-connections listener)))
                      (t
                       ;; Connection comes from active OPEN
                       (challenge-ack connection))))
               ((not (logtest flags +tcp4-flag-ack+))) ; Ignore packets without ACK set.
               ((not (rfc5961-mitigation-check-p connection ack))
                (challenge-ack connection))
               (t
                (cond ((acceptable-ack-p connection ack)
                       ;; Pasive open
                       (initial-rtt-measurement connection)
                       (setf (tcp-connection-state connection) :established)
                       (update-window connection wnd seq ack)
                       (when listener
                         (remhash connection (tcp-listener-pending-connections listener))
                         (mezzano.sync:mailbox-send connection (tcp-listener-connections listener)))
                       ;; Cancel retransmit
                       (disarm-retransmit-timer connection)
                       (disarm-timeout-timer connection))
                      (t
                       ;; Segment from an old connection
                       (tcp4-send-packet connection ack seq nil :ack-p nil :rst-p t)))
                (when (and (logtest flags +tcp4-flag-fin+)
                           (eql seq (tcp-connection-rcv.nxt connection)))
                  (setf (tcp-connection-state connection) :close-wait
                        (tcp-connection-rcv.nxt connection) (+u32 seq 1))
                  (tcp4-send-ack connection)
                  ;; Cancel retransmit
                  (disarm-retransmit-timer connection)
                  (disarm-timeout-timer connection)))))
        (:established
         (cond ((not (acceptable-segment-p connection seq data-length))
                (unless (logtest flags +tcp4-flag-rst+)
                  (challenge-ack connection)))
               ((logtest flags +tcp4-flag-rst+)
                (cond ((eql seq (tcp-connection-rcv.nxt connection))
                       (set-connection-error 'connection-reset connection)
                       (detach-tcp-connection connection))
                      (t
                       (challenge-ack connection))))
               ((logtest flags +tcp4-flag-syn+)
                (challenge-ack connection))
               ((not (logtest flags +tcp4-flag-ack+))) ; Ignore packets without ACK set.
               ((not (rfc5961-mitigation-check-p connection ack))
                (challenge-ack connection))
               ((>u32 ack (tcp-connection-snd.nxt connection))
                ;; Remote acks something not yet sent
                (challenge-ack connection))
               (t
                (when-acceptable-ack-p connection ack seq wnd)
                (unless (zerop data-length)
                  (tcp4-receive-data connection data-length end header-length packet seq start))
                (when (and (logtest flags +tcp4-flag-fin+)
                           (eql seq (tcp-connection-rcv.nxt connection)))
                  ;; Remote has sent FIN and waiting for ACK
                  (setf (tcp-connection-state connection) :close-wait
                        (tcp-connection-rcv.nxt connection) (+u32 seq 1))
                  (tcp4-send-ack connection)))))
        (:close-wait
         ;; Remote has closed, local can still send data.
         (cond ((not (acceptable-segment-p connection seq data-length))
                (unless (logtest flags +tcp4-flag-rst+)
                  (challenge-ack connection)))
               ((logtest flags +tcp4-flag-rst+)
                (cond ((eql seq (tcp-connection-rcv.nxt connection))
                       (set-connection-error 'connection-reset connection)
                       (detach-tcp-connection connection))
                      (t
                       (challenge-ack connection))))
               ((logtest flags +tcp4-flag-syn+)
                (challenge-ack connection))
               ((not (logtest flags +tcp4-flag-ack+))) ; Ignore packets without ACK set.
               ((not (rfc5961-mitigation-check-p connection ack))
                (challenge-ack connection))
               ((>u32 ack (tcp-connection-snd.nxt connection))
                ;; Remote acks something not yet sent
                (challenge-ack connection))
               (t
                (when-acceptable-ack-p connection ack seq wnd)
                (when (and (logtest flags +tcp4-flag-fin+)
                           (eql seq (tcp-connection-rcv.nxt connection)))
                  (setf (tcp-connection-rcv.nxt connection) (+u32 seq 1))
                  (tcp4-send-ack connection)))))
        (:last-ack
         (cond ((not (acceptable-segment-p connection seq data-length))
                (unless (logtest flags +tcp4-flag-rst+)
                  (challenge-ack connection)))
               ((logtest flags +tcp4-flag-rst+)
                (if (eql seq (tcp-connection-rcv.nxt connection))
                    (detach-tcp-connection connection)
                    (challenge-ack connection)))
               ((logtest flags +tcp4-flag-syn+)
                (challenge-ack connection))
               ((not (logtest flags +tcp4-flag-ack+))) ; Ignore packets without ACK set.
               ((not (rfc5961-mitigation-check-p connection ack))
                (challenge-ack connection))
               (t
                (when (eql ack (tcp-connection-snd.nxt connection))
                  (detach-tcp-connection connection))
                (when (and (logtest flags +tcp4-flag-fin+)
                           (eql seq (tcp-connection-rcv.nxt connection)))
                  (setf (tcp-connection-rcv.nxt connection) (+u32 seq 1))
                  (tcp4-send-ack connection)))))
        (:fin-wait-1
         ;; Local closed, waiting for remote to close.
         (cond ((not (acceptable-segment-p connection seq data-length))
                (unless (logtest flags +tcp4-flag-rst+)
                  (challenge-ack connection)))
               ((logtest flags +tcp4-flag-rst+)
                (cond ((eql seq (tcp-connection-rcv.nxt connection))
                       (set-connection-error 'connection-reset connection)
                       (detach-tcp-connection connection))
                      (t
                       (challenge-ack connection))))
               ((logtest flags +tcp4-flag-syn+)
                (challenge-ack connection))
               ((not (logtest flags +tcp4-flag-ack+))) ; Ignore packets without ACK set.
               ((not (rfc5961-mitigation-check-p connection ack))
                (challenge-ack connection))
               ((>u32 ack (tcp-connection-snd.nxt connection))
                ;; Remote acks something not yet sent
                (challenge-ack connection))
               (t
                (when-acceptable-ack-p connection ack seq wnd)
                (unless (zerop data-length)
                  (tcp4-receive-data connection data-length end header-length packet seq start))
                (cond ((and (logtest flags +tcp4-flag-fin+)
                            (eql seq (tcp-connection-rcv.nxt connection)))
                       (setf (tcp-connection-state connection) :time-wait
                             (tcp-connection-rcv.nxt connection) (+u32 seq 1))
                       (tcp4-send-ack connection)
                       (arm-timeout-timer (* 2 *msl*) connection))
                      ((and (logtest flags +tcp4-flag-fin+)
                            (eql seq (tcp-connection-rcv.nxt connection)))
                       ;; Simultaneous close
                       (setf (tcp-connection-state connection) :closing
                             (tcp-connection-rcv.nxt connection) (+u32 seq 1))
                       (tcp4-send-ack connection))
                      ((eql seq (tcp-connection-rcv.nxt connection))
                       (setf (tcp-connection-state connection) :fin-wait-2))))))
        (:fin-wait-2
         ;; Local closed, still waiting for remote to close.
         (cond ((not (acceptable-segment-p connection seq data-length))
                (unless (logtest flags +tcp4-flag-rst+)
                  (challenge-ack connection)))
               ((logtest flags +tcp4-flag-rst+)
                (cond ((eql seq (tcp-connection-rcv.nxt connection))
                       (set-connection-error 'connection-reset connection)
                       (detach-tcp-connection connection))
                      (t
                       (challenge-ack connection))))
               ((logtest flags +tcp4-flag-syn+)
                (challenge-ack connection))
               ((not (logtest flags +tcp4-flag-ack+))) ; Ignore packets without ACK set.
               ((not (rfc5961-mitigation-check-p connection ack))
                (challenge-ack connection))
               ((>u32 ack (tcp-connection-snd.nxt connection))
                ;; Remote acks something not yet sent
                (challenge-ack connection))
               (t
                (when-acceptable-ack-p connection ack seq wnd)
                (unless (zerop data-length)
                  (tcp4-receive-data connection data-length end header-length packet seq start))
                (when (and (logtest flags +tcp4-flag-fin+)
                           (eql seq (tcp-connection-rcv.nxt connection)))
                  (setf (tcp-connection-state connection) :time-wait
                        (tcp-connection-rcv.nxt connection) (+u32 seq 1))
                  (tcp4-send-ack connection)
                  (arm-timeout-timer (* 2 *msl*) connection)))))
        (:closing
         ;; Waiting for ACK
         (cond ((not (acceptable-segment-p connection seq data-length))
                (unless (logtest flags +tcp4-flag-rst+)
                  (challenge-ack connection)))
               ((logtest flags +tcp4-flag-rst+)
                (if (eql seq (tcp-connection-rcv.nxt connection))
                    (detach-tcp-connection connection)
                    (challenge-ack connection)))
               ((logtest flags +tcp4-flag-syn+)
                (challenge-ack connection))
               ((not (logtest flags +tcp4-flag-ack+))) ; Ignore packets without ACK set.
               ((not (rfc5961-mitigation-check-p connection ack))
                (challenge-ack connection))
               ((>u32 ack (tcp-connection-snd.nxt connection))
                ;; Remote acks something not yet sent
                (challenge-ack connection))
               (t
                (when-acceptable-ack-p connection ack seq wnd)
                (when (eql seq (tcp-connection-rcv.nxt connection))
                  (setf (tcp-connection-state connection) :time-wait)
                  (disarm-timeout-timer connection)
                  (arm-timeout-timer (* 2 *msl*) connection))
                (when (and (logtest flags +tcp4-flag-fin+)
                           (eql seq (tcp-connection-rcv.nxt connection)))
                  (setf (tcp-connection-rcv.nxt connection) (+u32 seq 1))
                  (tcp4-send-ack connection)))))
        (:time-wait
         (cond ((not (acceptable-segment-p connection seq data-length))
                (unless (logtest flags +tcp4-flag-rst+)
                  (challenge-ack connection)))
               ((logtest flags +tcp4-flag-rst+)
                (if (eql seq (tcp-connection-rcv.nxt connection))
                    (detach-tcp-connection connection)
                    (challenge-ack connection)))
               ((logtest flags +tcp4-flag-syn+)
                (challenge-ack connection))
               ((not (logtest flags +tcp4-flag-ack+))) ; Ignore packets without ACK set.
               ((not (rfc5961-mitigation-check-p connection ack))
                (challenge-ack connection))
               ((and (logtest flags +tcp4-flag-fin+)
                     (eql seq (tcp-connection-rcv.nxt connection)))
                (setf (tcp-connection-rcv.nxt connection) (+u32 seq 1))
                (tcp4-send-ack connection)
                (disarm-timeout-timer connection)
                (arm-timeout-timer (* 2 *msl*) connection))))
        (:closed)))
    (update-timeout-timer connection)
    ;; Notify any waiters that something may have changed.
    (mezzano.supervisor:condition-notify (tcp-connection-cvar connection) t)))

(defun tcp4-send-packet (connection seq ack data &key cwr-p ece-p urg-p (ack-p t) psh-p rst-p syn-p fin-p errors-escape)
  (let* ((source (tcp-connection-local-ip connection))
         (source-port (tcp-connection-local-port connection))
         (packet (assemble-tcp4-packet source source-port
                                       (tcp-connection-remote-ip connection)
                                       (tcp-connection-remote-port connection)
                                       seq ack
                                       (tcp-connection-rcv.wnd connection)
                                       data
                                       :cwr-p cwr-p
                                       :ece-p ece-p
                                       :urg-p urg-p
                                       :ack-p ack-p
                                       :psh-p psh-p
                                       :rst-p rst-p
                                       :syn-p syn-p
                                       :fin-p fin-p)))
    (handler-case
        (mezzano.network.ip:transmit-ipv4-packet
         source (tcp-connection-remote-ip connection)
         mezzano.network.ip:+ip-protocol-tcp+ packet)
      (mezzano.network.ip:no-route-to-host (c)
        (detach-tcp-connection connection)
        (setf (tcp-connection-pending-error connection) c)
        (mezzano.supervisor:condition-notify (tcp-connection-cvar connection) t)
        (when errors-escape
          (error c))))))

(defun tcp4-send-ack (connection)
  "Send a standard ACK segment in response to valid segments."
  (tcp4-send-packet connection
                    (tcp-connection-snd.nxt connection)
                    (tcp-connection-rcv.nxt connection)
                    nil))

(defun challenge-ack (connection)
  "Send a challenge ACK segment in response to suspicious segments (RFC5961)."
  (tcp4-send-packet connection
                    (tcp-connection-snd.nxt connection)
                    (tcp-connection-rcv.nxt connection)
                    nil))

(defun compute-ip-pseudo-header-partial-checksum (src-ip dst-ip protocol length)
  (+ (logand src-ip #xFFFF)
     (logand (ash src-ip -16) #xFFFF)
     (logand dst-ip #xFFFF)
     (logand (ash dst-ip -16) #xFFFF)
     protocol
     length))

(defun assemble-tcp4-packet (src-ip src-port dst-ip dst-port seq-num ack-num window payload
                             &key cwr-p ece-p urg-p (ack-p t) psh-p rst-p syn-p fin-p)
  "Build a full TCP & IP header."
  (let* ((checksum 0)
         (payload-size (length payload))
         (header-length (if syn-p 24 20))
         (header (make-array header-length :element-type '(unsigned-byte 8)))
         (packet (list header payload)))
    ;; Assemble the TCP header.
    (setf (ub16ref/be header +tcp4-header-source-port+) src-port
          (ub16ref/be header +tcp4-header-destination-port+) dst-port
          (ub32ref/be header +tcp4-header-sequence-number+) seq-num
          (ub32ref/be header +tcp4-header-acknowledgment-number+) ack-num
          ;; Data offset/header length (5 32-bit words) and flags.
          (ub16ref/be header +tcp4-header-flags-and-data-offset+) (logior (ash (ceiling header-length 4) 12)
                                                                          (if fin-p +tcp4-flag-fin+ 0)
                                                                          (if syn-p +tcp4-flag-syn+ 0)
                                                                          (if rst-p +tcp4-flag-rst+ 0)
                                                                          (if psh-p +tcp4-flag-psh+ 0)
                                                                          (if ack-p +tcp4-flag-ack+ 0)
                                                                          (if urg-p +tcp4-flag-urg+ 0)
                                                                          (if ece-p +tcp4-flag-ece+ 0)
                                                                          (if cwr-p +tcp4-flag-cwr+ 0))
          ;; Window.
          (ub16ref/be header +tcp4-header-window-size+) window
          ;; Checksum.
          (ub16ref/be header +tcp4-header-checksum+) 0
          ;; Urgent pointer.
          (ub16ref/be header +tcp4-header-urgent-pointer+) 0)
    ;; MSS TODO: MUST-16, SHLD-6 and MUST-67
    (when syn-p
      (setf (aref header +tcp4-header-options+) 2
            (aref header (+ 1 +tcp4-header-options+)) 4
            (ub16ref/be header (+ 2 +tcp4-header-options+)) *rcv.mss*))
    ;; Compute the final checksum.
    (setf checksum (compute-ip-pseudo-header-partial-checksum
                    (mezzano.network.ip::ipv4-address-address src-ip)
                    (mezzano.network.ip::ipv4-address-address dst-ip)
                    mezzano.network.ip:+ip-protocol-tcp+
                    (+ (length header) payload-size)))
    (setf checksum (mezzano.network.ip:compute-ip-partial-checksum header 0 nil checksum))
    (when payload
      (setf checksum (mezzano.network.ip:compute-ip-partial-checksum payload 0 nil checksum)))
    (setf checksum (mezzano.network.ip:finalize-ip-checksum checksum))
    (setf (ub16ref/be header +tcp4-header-checksum+) checksum)
    packet))

(defun allocate-connection-local-port (local-ip ip port)
  (find-available-port
   #'(lambda (local-port)
       (get-tcp-connection ip port local-ip local-port))))

(define-condition connection-error (net:network-error)
  ((host :initarg :host :reader connection-error-host)
   (port :initarg :port :reader connection-error-port)))

(define-condition connection-closed (connection-error)
  ())

(define-condition connection-closing (connection-error)
  ())

(define-condition connection-refused (connection-error)
  ())

(define-condition connection-aborted (connection-error)
  ())

(define-condition connection-reset (connection-error)
  ())

(define-condition connection-timed-out (connection-error)
  ())

(define-condition connection-stale (connection-error)
  ())

(defun set-connection-error (condition-type connection)
  "Set a pending error condition on the TCP connection."
  (setf (tcp-connection-pending-error connection)
        (make-condition condition-type
                        :host (tcp-connection-remote-ip connection)
                        :port (tcp-connection-remote-port connection))))

(defun flush-stale-connections ()
  ;; Called with snapshot inhibited to prevent more connections becoming stale.
  ;; Lock ordering note:
  ;; Can't take the per-connection lock while *tcp-connection-lock* is
  ;; held. Must be the other way around. Per-connection lock first,
  ;; then *tcp-connection-lock*.
  (let ((stale-connections
         (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
           (loop
              for connection in *tcp-connections*
              when (and (tcp-connection-boot-id connection)
                        (not (eql (tcp-connection-boot-id connection)
                                  (mezzano.supervisor:current-boot-id))))
              collect connection))))
    (dolist (connection stale-connections)
      (mezzano.supervisor:with-mutex ((tcp-connection-lock connection))
        (set-connection-error 'connection-stale connection)
        (mezzano.supervisor:condition-notify (tcp-connection-cvar connection) t)
        (detach-tcp-connection connection)))))

(defun check-connection-error (connection)
  (let ((condition (tcp-connection-pending-error connection)))
    (when condition
      (error condition))))

(defun tcp-connect (ip port &key persist timeout)
  (let* ((interface (nth-value 1 (mezzano.network.ip:ipv4-route ip)))
         (source-address (mezzano.network.ip:ipv4-interface-address interface))
         (source-port (allocate-connection-local-port source-address ip port))
         (iss (or *netmangler-iss*
                  (random #x100000000)))
         (connection (make-instance 'tcp-connection
                                    :state :syn-sent
                                    :local-port source-port
                                    :local-ip source-address
                                    :remote-port port
                                    :remote-ip ip
                                    :snd.nxt (+u32 iss 1)
                                    :snd.una iss
                                    :rcv.nxt 0
                                    :rcv.wnd *initial-window-size*
                                    :boot-id (if persist nil (mezzano.supervisor:current-boot-id))
                                    :timeout timeout)))
    (mezzano.sync.dispatch:dispatch-async
     (lambda ()
       (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
         (push connection *tcp-connections*))
       (setf (tcp-connection-last-ack-time connection)
             (get-internal-run-time))
       (unless *netmangler-force-local-retransmit*
         (tcp4-send-packet connection iss 0 nil :ack-p nil :syn-p t))
       (arm-retransmit-timer connection)
       (arm-timeout-timer *tcp-connect-timeout* connection))
     net::*network-serial-queue*)
    (with-tcp-connection-locked connection
      (mezzano.supervisor:condition-wait-for ((tcp-connection-cvar connection)
                                              (tcp-connection-lock connection))
        (not (eql (tcp-connection-state connection) :syn-sent)))
      (check-connection-error connection))
    connection))

(defun subseq-ub8 (sequence start &optional end)
  "Like SUBSEQ, but returns a simple UB8 vector."
  (let ((result (make-array (- (or end (length sequence)) start) :element-type '(unsigned-byte 8))))
    (replace result sequence :start2 start :end2 end)
    result))

(defun tcp-send-1 (connection data start end &key psh-p)
  (let ((snd.nxt (tcp-connection-snd.nxt connection))
        (rcv.nxt (tcp-connection-rcv.nxt connection))
        (len (- end start))
        ;; A copy of the data must be taken as it is owned by the user
        ;; and may be modified after TCP-SEND returns. Due to retransmit
        ;; it needs to be kept around until the remote has acked the data.
        (data (subseq-ub8 data start end)))
    (setf (tcp-connection-snd.nxt connection)
          (+u32 snd.nxt len))
    (setf (tcp-connection-retransmit-queue connection)
          (append (tcp-connection-retransmit-queue connection)
                  (list (list snd.nxt rcv.nxt data :psh-p psh-p))))
    (arm-retransmit-timer connection)
    (unless *netmangler-force-local-retransmit*
      (tcp4-send-packet connection
                        snd.nxt rcv.nxt
                        data
                        :psh-p psh-p
                        :errors-escape t))))

;; TODO: Respect the send window, buffer data when it fills up.
(defun tcp-send (connection data &optional (start 0) end)
  (setf end (or end (length data)))
  (with-tcp-connection-locked connection
    (check-connection-error connection)
    (update-timeout-timer connection)
    (ecase (tcp-connection-state connection)
      ((:syn-sent :syn-received)
       ;; Data associated with SEND may be sent with SYN segment or queued for transmission after entering ESTABLISHED state
       ;; TODO: If in state :syn-sent or :syn-received queue the data for processing after the ESTABLISHED state has been reached
       (error 'connection-closed
              :host (tcp-connection-remote-ip connection)
              :port (tcp-connection-remote-port connection)))
      ((:established :close-wait)
       (unless (tcp-connection-last-ack-time connection)
         (setf (tcp-connection-last-ack-time connection)
               (get-internal-run-time)))
       (let ((snd.mss (tcp-connection-snd.mss connection)))
         (cond ((>= start end))
               ((> (- end start) snd.mss)
                ;; Send multiple packets.
                (loop :for offset :from start :by snd.mss
                      :while (> (- end offset) snd.mss)
                      :do (tcp-send-1 connection data offset (+ offset snd.mss))
                      :finally (tcp-send-1 connection data offset end :psh-p t)))
               (t
                ;; Send one packet.
                (tcp-send-1 connection data start end :psh-p t)))))
      ((:fin-wait-1 :fin-wait-2 :closing :last-ack :time-wait)
       (error 'connection-closing
              :host (tcp-connection-remote-ip connection)
              :port (tcp-connection-remote-port connection)))
      (:closed
       (error 'connection-closed
              :host (tcp-connection-remote-ip connection)
              :port (tcp-connection-remote-port connection))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TCP Stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tcp-octet-stream (gray:fundamental-binary-input-stream
                            gray:fundamental-binary-output-stream)
  ((connection :initarg :connection :reader tcp-stream-connection)
   (current-packet :initform nil :accessor tcp-stream-packet)))

(defclass tcp-stream (sys.int::external-format-mixin
                      gray:fundamental-character-input-stream
                      gray:fundamental-character-output-stream
                      tcp-octet-stream
                      gray:unread-char-mixin)
  ())

(defmethod mezzano.network:local-endpoint ((object tcp-octet-stream))
  (let ((conn (tcp-stream-connection object)))
    (values (tcp-connection-local-ip conn)
            (tcp-connection-local-port conn))))

(defmethod mezzano.network:remote-endpoint ((object tcp-octet-stream))
  (let ((conn (tcp-stream-connection object)))
    (values (tcp-connection-remote-ip conn)
            (tcp-connection-remote-port conn))))

(defmethod mezzano.sync:get-object-event ((object tcp-octet-stream))
  (tcp-connection-receive-event (tcp-stream-connection object)))

(defmethod print-object ((instance tcp-octet-stream) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (multiple-value-bind (local-address local-port)
        (mezzano.network:local-endpoint instance)
      (multiple-value-bind (remote-address remote-port)
          (mezzano.network:remote-endpoint instance)
        (format stream "~A :Local ~A:~A :Remote ~A:~A"
                (tcp-connection-state (tcp-stream-connection instance))
                local-address local-port
                remote-address remote-port)))))

(defun connection-may-have-additional-data-p (connection)
  "Returns true if CONNECTION is in a state where it may potentially receive further data."
  (member (tcp-connection-state connection)
          ;; Data can only be received in these states.
          '(:established
            :fin-wait-1
            :fin-wait-2)))

(defun refill-tcp-packet-buffer (stream)
  (let ((connection (tcp-stream-connection stream)))
    ;; Wait for data or for the connection to close.
    (mezzano.supervisor:condition-wait-for ((tcp-connection-cvar connection)
                                            (tcp-connection-lock connection))
      (when (tcp-stream-packet stream)
        ;; There was already data waiting or another thread refilled
        ;; while we were waiting.
        (return-from refill-tcp-packet-buffer t))
      (or (tcp-connection-rx-data connection)
          (not (connection-may-have-additional-data-p connection))))
    (when (and (null (tcp-connection-rx-data connection))
               (not (connection-may-have-additional-data-p connection)))
      (return-from refill-tcp-packet-buffer nil))
    (setf (tcp-stream-packet stream) (pop (tcp-connection-rx-data connection))))
  t)

(defun refill-tcp-packet-buffer-no-hang (stream)
  (let ((connection (tcp-stream-connection stream)))
    (when (and (null (tcp-stream-packet stream))
               (tcp-connection-rx-data connection))
      (setf (tcp-stream-packet stream) (pop (tcp-connection-rx-data connection))))))

(defun maybe-clear-receive-ready-event (stream)
  (let ((connection (tcp-stream-connection stream)))
    (when (and (null (tcp-stream-packet stream))
               (endp (tcp-connection-rx-data connection))
               (connection-may-have-additional-data-p connection))
      (setf (mezzano.supervisor:event-state
             (tcp-connection-receive-event connection))
            nil))))

(defmethod gray:stream-listen-byte ((stream tcp-octet-stream))
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (check-connection-error (tcp-stream-connection stream))
    (refill-tcp-packet-buffer-no-hang stream)
    (not (null (tcp-stream-packet stream)))))

;; Provide this method so that LISTEN works on octet streams too.
;; EXTERNAL-FORMAT-MIXIN implements LISTEN on a character level,
;; LISTEN-BYTE exists to poke directly at the raw byte stream.
(defmethod gray:stream-listen ((stream tcp-octet-stream))
  (let ((result (gray:stream-listen-byte stream)))
    (and result (not (eql result :eof)))))

(defmethod gray:stream-read-byte ((stream tcp-octet-stream))
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (check-connection-error (tcp-stream-connection stream))
    (unless (refill-tcp-packet-buffer stream)
      (return-from gray:stream-read-byte :eof))
    (let* ((packet (tcp-stream-packet stream))
           (byte (aref (first packet) (second packet))))
      (when (>= (incf (second packet)) (third packet))
        (setf (tcp-stream-packet stream) nil))
      (maybe-clear-receive-ready-event stream)
      byte)))

(defmethod gray:stream-read-byte-no-hang ((stream tcp-octet-stream))
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (check-connection-error (tcp-stream-connection stream))
    (refill-tcp-packet-buffer-no-hang stream)
    (unless (tcp-stream-packet stream)
      (return-from gray:stream-read-byte-no-hang
        (if (connection-may-have-additional-data-p (tcp-stream-connection stream))
            nil
            :eof)))
    (let* ((packet (tcp-stream-packet stream))
           (byte (aref (first packet) (second packet))))
      (when (>= (incf (second packet)) (third packet))
        (setf (tcp-stream-packet stream) nil))
      (maybe-clear-receive-ready-event stream)
      byte)))

(defmethod gray:stream-read-sequence ((stream tcp-octet-stream) sequence &optional (start 0) end)
  (unless end
    (setf end (length sequence)))
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (check-connection-error (tcp-stream-connection stream))
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
      (maybe-clear-receive-ready-event stream)
      position)))

(defmethod gray:stream-write-byte ((stream tcp-octet-stream) byte)
  (let ((ary (make-array 1 :element-type '(unsigned-byte 8)
                         :initial-element byte)))
    (tcp-send (tcp-stream-connection stream) ary)))

(defmethod gray:stream-write-sequence ((stream tcp-octet-stream) sequence &optional (start 0) end)
  (tcp-send (tcp-stream-connection stream) sequence start end))

(defun abort-connection (connection)
  (ecase (tcp-connection-state connection)
    (:syn-sent
     (set-connection-error 'connection-reset connection))
    ((:syn-received :established :fin-wait-1 :fin-wait-2 :close-wait)
     (set-connection-error 'connection-reset connection)
     (tcp4-send-packet connection
                       (tcp-connection-snd.nxt connection)
                       (tcp-connection-rcv.nxt connection)
                       nil
                       :ack-p nil
                       :rst-p t)
     (setf (tcp-connection-snd.nxt connection)
           (+u32 (tcp-connection-snd.nxt connection) 1)))
    ((:closing :last-ack :time-wait)
     (tcp4-send-ack connection))
    (:closed))
  (mezzano.supervisor:condition-notify (tcp-connection-cvar connection) t)
  (detach-tcp-connection connection))

(defun tcp4-send-fin (connection)
  "Queue and send a FIN packet."
  (let ((packet (list (tcp-connection-snd.nxt connection)
                      (tcp-connection-rcv.nxt connection)
                      nil
                      :fin-p t
                      :errors-escape t)))
    (setf (tcp-connection-retransmit-queue connection)
          (append (tcp-connection-retransmit-queue connection) (list packet))))
  (arm-retransmit-timer connection)
  (unless *netmangler-force-local-retransmit*
    (tcp4-send-packet connection
                      (tcp-connection-snd.nxt connection)
                      (tcp-connection-rcv.nxt connection)
                      nil
                      :fin-p t
                      :errors-escape t))
  (setf (tcp-connection-snd.nxt connection)
        (+u32 (tcp-connection-snd.nxt connection) 1)))

(defun close-connection (connection)
  (ecase (tcp-connection-state connection)
    (:syn-sent
     (set-connection-error 'connection-closing connection)
     (detach-tcp-connection connection))
    (:syn-received
     ;; TODO: If there is data to send queue for processing after entering ESTABLISHED state.
     (setf (tcp-connection-state connection) :fin-wait-1)
     (tcp4-send-fin connection))
    (:established
     (setf (tcp-connection-state connection) :fin-wait-1)
     (tcp4-send-fin connection))
    (:close-wait
     (setf (tcp-connection-state connection) :last-ack)
     (tcp4-send-fin connection))
    ((:last-ack :fin-wait-1 :fin-wait-2 :closing :time-wait :closed))))

(defmethod close ((stream tcp-octet-stream) &key abort)
  (let ((connection (tcp-stream-connection stream)))
    (with-tcp-connection-locked connection
      (if abort
          (abort-connection connection)
          (close-connection connection)))))

(defmethod open-stream-p ((stream tcp-octet-stream))
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (let ((connection (tcp-stream-connection stream)))
      (refill-tcp-packet-buffer-no-hang stream)
      (or (tcp-stream-packet stream)
          (connection-may-have-additional-data-p connection)))))

(defmethod stream-element-type ((stream tcp-octet-stream))
  '(unsigned-byte 8))

(defun tcp-stream-connect (address port &key element-type external-format persist timeout)
  (cond ((or (not element-type)
             (sys.int::type-equal element-type 'character))
         (make-instance 'tcp-stream
                        :connection (tcp-connect (net::resolve-address address) port
                                                 :persist persist
                                                 :timeout timeout)
                        :external-format (sys.int::make-external-format
                                          (or element-type 'character)
                                          (or external-format :default)
                                          :eol-style :crlf)))
        ((sys.int::type-equal element-type '(unsigned-byte 8))
         (assert (or (not external-format)
                     (eql external-format :default)))
         (make-instance 'tcp-octet-stream
                        :connection (tcp-connect (net::resolve-address address) port
                                                 :persist persist
                                                 :timeout timeout)))
        (t
         (error "Unsupported element type ~S" element-type))))
