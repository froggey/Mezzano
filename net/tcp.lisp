;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.network.tcp)

(defconstant +tcp4-header-source-port+ 0)
(defconstant +tcp4-header-destination-port+ 2)
(defconstant +tcp4-header-sequence-number+ 4)
(defconstant +tcp4-header-acknowledgment-number+ 8)
(defconstant +tcp4-header-flags-and-data-offset+ 12)
(defconstant +tcp4-header-window-size+ 14)
(defconstant +tcp4-header-checksum+ 16)
(defconstant +tcp4-header-urgent-pointer+ 18)

(defconstant +tcp4-flag-fin+ #b00000001)
(defconstant +tcp4-flag-syn+ #b00000010)
(defconstant +tcp4-flag-rst+ #b00000100)
(defconstant +tcp4-flag-psh+ #b00001000)
(defconstant +tcp4-flag-ack+ #b00010000)
(defconstant +tcp4-flag-urg+ #b00100000)
(defconstant +tcp4-flag-ece+ #b01000000)
(defconstant +tcp4-flag-cwr+ #b10000000)

;; DEFPARAMETER, not DEFCONSTANT, due to cross-compiler constraints.
(defparameter +ip-wildcard+ (mezzano.network.ip:make-ipv4-address "0.0.0.0"))
(defconstant +port-wildcard+ 0)

(defparameter *tcp-connect-timeout* 10)
(defparameter *tcp-connect-initial-retransmit-time* 1)
(defparameter *tcp-connect-retransmit-time* 3)

(defvar *tcp-connections* nil)
(defvar *tcp-connection-lock* (mezzano.supervisor:make-mutex "TCP connection list"))
(defvar *tcp-listeners* nil)
(defvar *tcp-listener-lock* (mezzano.supervisor:make-mutex "TCP listener list"))

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
    :closing))

(deftype tcp-port-number ()
  '(unsigned-byte 16))

(deftype tcp-sequence-number ()
  '(unsigned-byte 32))

;; FIXME: Inbount connections need to timeout if state :syn-received don't change.
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
  (:default-initargs :n-pending-connections 0))

(defmethod mezzano.sync:get-object-event ((object tcp-listener))
  (mezzano.sync:get-object-event (tcp-listener-connections object)))

(defmethod mezzano.network:local-endpoint ((object tcp-listener))
  (values (tcp-listener-local-ip object)
          (tcp-listener-local-port object)))

(defmethod print-object ((instance tcp-listener) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream ":local-ip ~A :local-port ~A"
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

(defun tcp-listen (local-host local-port &key backlog)
  (multiple-value-bind (host interface)
      (mezzano.network.ip:ipv4-route (mezzano.network:resolve-address local-host))
    (declare (ignore host))
    (mezzano.supervisor:with-mutex (*tcp-listener-lock*)
      (let* ((source-address (mezzano.network.ip:ipv4-interface-address interface))
             (local-port (cond ((= local-port +port-wildcard+)
                                ;; find a suitable port number
                                (loop :for local-port := (+ (random 32768) 32768)
                                      :unless (get-tcp-listener-without-lock source-address local-port)
                                      :do(return local-port)))
                               ((get-tcp-listener-without-lock source-address local-port)
                                (error "Server already listening on port ~D" local-port))
                               (t
                                local-port)))
             (listener (make-instance 'tcp-listener
                                      :pending-connections (make-hash-table :test 'equalp :size backlog)
                                      :connections (mezzano.sync:make-mailbox
                                                    :name "TCP Listener")
                                      :backlog backlog
                                      :local-port local-port
                                      :local-ip source-address)))
        (push listener *tcp-listeners*)
        listener))))

(defun tcp-accept (listener &key (wait-p t) element-type external-format)
  (let ((connection (mezzano.sync:mailbox-receive
                     (tcp-listener-connections listener)
                     :wait-p wait-p)))
    (cond (connection
           (when (tcp-listener-backlog listener)
             (decf (tcp-listener-n-pending-connections listener)))
           (tcp4-accept-connection connection :element-type element-type :external-format external-format))
          (t
           nil))))

(defun close-tcp-listener (listener)
  (mezzano.supervisor:with-mutex (*tcp-listener-lock*)
    (setf *tcp-listeners* (remove listener *tcp-listeners*)))
  (loop :for connection :being :the :hash-values :of (tcp-listener-pending-connections listener)
        :do (with-tcp-connection-locked connection
              (abort-connection connection)))
  (loop :for connection :in (mezzano.sync:mailbox-flush (tcp-listener-connections listener))
        :do (with-tcp-connection-locked connection
              (abort-connection connection))))

(defclass tcp-connection ()
  ((%state :accessor tcp-connection-state
           :initarg :state
           :type tcp-connection-state)
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
   (%s-next :accessor tcp-connection-s-next
            :initarg :s-next
            :type tcp-sequence-number)
   (%r-next :accessor tcp-connection-r-next
            :initarg :r-next
            :type tcp-sequence-number)
   (%window-size :accessor tcp-connection-window-size :initarg :window-size)
   (%max-seg-size :accessor tcp-connection-max-seg-size :initarg :max-seg-size)
   (%rx-data :accessor tcp-connection-rx-data :initform '())
   (%rx-data-unordered :reader tcp-connection-rx-data-unordered :initform (make-hash-table))
   (%lock :reader tcp-connection-lock
          :initform (mezzano.supervisor:make-mutex "TCP connection lock"))
   (%cvar :reader tcp-connection-cvar
          :initform (mezzano.supervisor:make-condition-variable "TCP connection cvar"))
   (%receive-event :reader tcp-connection-receive-event
                   :initform (mezzano.supervisor:make-event :name "TCP connection data available"))
   (%pending-error :accessor tcp-connection-pending-error :initform nil)
   (%retransmit-timer :reader tcp-connection-retransmit-timer
                      :initform (mezzano.supervisor:make-timer :name "TCP connection retransmit"))
   (%retransmit-source :reader tcp-connection-retransmit-source)
   (%timeout-timer :reader tcp-connection-timeout-timer
                   :initform (mezzano.supervisor:make-timer :name "TCP connection timeout"))
   (%timeout-source :reader tcp-connection-timeout-source)
   (%boot-id :reader tcp-connection-boot-id
             :initarg :boot-id))
  (:default-initargs
   :max-seg-size 1000
   :boot-id nil))

(defun arm-retransmit-timer (seconds connection)
  (mezzano.supervisor:timer-arm seconds
                                (tcp-connection-retransmit-timer connection))
  (values))

(defun disarm-retransmit-timer (connection)
  (mezzano.supervisor:timer-disarm (tcp-connection-retransmit-timer connection))
  (values))

(defun retransmit-timer-handler (connection)
  (when (not (mezzano.supervisor:timer-expired-p
              (tcp-connection-retransmit-timer connection)))
    ;; Timer is either still pending or isn't actually running.
    ;; This can happen if the timer expires but some other task reconfigures
    ;; a new retransmit time.
    (return-from retransmit-timer-handler))
  ;; Disarm it so it stops triggering the source
  (mezzano.supervisor:timer-disarm (tcp-connection-retransmit-timer connection))
  ;; What're we retransmitting?
  (format t "Doing retransmit on connection ~S~%" connection)
  (ecase (tcp-connection-state connection)
    (:syn-sent
     (let ((seq (logand #xFFFFFFFF (1- (tcp-connection-s-next connection)))))
       (tcp4-send-packet connection seq 0 nil :ack-p nil :syn-p t)
       (arm-retransmit-timer *tcp-connect-retransmit-time* connection)))))

(defun arm-timeout-timer (seconds connection)
  (mezzano.supervisor:timer-arm seconds
                                (tcp-connection-timeout-timer connection))
  (values))

(defun disarm-timeout-timer (connection)
  (mezzano.supervisor:timer-disarm (tcp-connection-timeout-timer connection))
  (values))

(defun timeout-timer-handler (connection)
  (when (not (mezzano.supervisor:timer-expired-p
              (tcp-connection-timeout-timer connection)))
    ;; Timer is either still pending or isn't actually running.
    ;; This can happen if the timer expires but some other task reconfigures
    ;; a new timeout time.
    (return-from timeout-timer-handler))
  ;; Disarm it so it stops triggering the source
  (mezzano.supervisor:timer-disarm (tcp-connection-timeout-timer connection))
  (mezzano.supervisor:with-mutex ((tcp-connection-lock connection))
    (setf (tcp-connection-pending-error connection)
          (make-condition 'connection-timed-out
                          :host (tcp-connection-remote-ip connection)
                          :port (tcp-connection-remote-port connection)))
    (setf (tcp-connection-state connection) :closed)
    (mezzano.supervisor:condition-notify (tcp-connection-cvar connection) t)
    (detach-tcp-connection connection)))

(defmethod initialize-instance :after ((instance tcp-connection) &key)
  (setf (slot-value instance '%retransmit-source)
        (mezzano.sync.dispatch:make-source (tcp-connection-retransmit-timer instance)
                                           (lambda ()
                                             (retransmit-timer-handler instance))
                                           :target sys.net::*network-serial-queue*))
  (setf (slot-value instance '%timeout-source)
        (mezzano.sync.dispatch:make-source (tcp-connection-timeout-timer instance)
                                           (lambda ()
                                             (timeout-timer-handler instance))
                                           :target sys.net::*network-serial-queue*)))

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

(defun %tcp4-receive (packet local-ip remote-ip start end)
  (let* ((remote-port (ub16ref/be packet (+ start +tcp4-header-source-port+)))
         (local-port (ub16ref/be packet (+ start +tcp4-header-destination-port+)))
         (flags (ldb (byte 12 0)
                     (ub16ref/be packet (+ start +tcp4-header-flags-and-data-offset+))))
         (connection (get-tcp-connection remote-ip remote-port local-ip local-port))
         (listener (get-tcp-listener local-ip local-port)))
    (cond (connection
           (tcp4-receive connection packet start end listener))
          ;; Drop unestablished connections if they surpassed listener backlog
          ((and listener
                (eql flags +tcp4-flag-syn+)
                (or (not (tcp-listener-backlog listener))
                    (< (tcp-listener-n-pending-connections listener)
                       (tcp-listener-backlog listener))))
           (when (tcp-listener-backlog listener)
             (incf (tcp-listener-n-pending-connections listener)))
           (let* ((seq (random #x100000000))
                  (ack (logand #xFFFFFFFF (1+ (ub32ref/be packet (+ start +tcp4-header-sequence-number+)))))
                  (connection (make-instance 'tcp-connection
                                             :state :syn-received
                                             :local-port local-port
                                             :local-ip local-ip
                                             :remote-port remote-port
                                             :remote-ip remote-ip
                                             :s-next (logand #xFFFFFFFF (1+ seq))
                                             :r-next ack
                                             :window-size 8192
                                             :boot-id (mezzano.supervisor:current-boot-id))))
             (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
               (push connection *tcp-connections*))
             (setf (gethash connection (tcp-listener-pending-connections listener))
                   connection)
             (tcp4-send-packet connection seq ack nil :ack-p t :syn-p t))))))

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
  (mezzano.supervisor:timer-disarm (tcp-connection-retransmit-timer connection))
  (mezzano.supervisor:timer-disarm (tcp-connection-timeout-timer connection))
  (mezzano.sync.dispatch:cancel (tcp-connection-retransmit-source connection))
  (mezzano.sync.dispatch:cancel (tcp-connection-timeout-source connection))
  (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
    (setf *tcp-connections* (remove connection *tcp-connections*))))

(defun tcp4-receive-data (connection data-length end header-length packet seq start)
  (cond ((= seq (tcp-connection-r-next connection))
         ;; Send data to the user layer
         (if (tcp-connection-rx-data connection)
             (setf (cdr (last (tcp-connection-rx-data connection)))
                   (list (list packet (+ start header-length) end)))
             (setf (tcp-connection-rx-data connection)
                   (list (list packet (+ start header-length) end))))
         (setf (tcp-connection-r-next connection)
               (logand (+ (tcp-connection-r-next connection) data-length)
                       #xFFFFFFFF))
         ;; Check if the next packet is in tcp-connection-rx-data-unordered
         (loop :for packet := (gethash (tcp-connection-r-next connection)
                                       (tcp-connection-rx-data-unordered connection))
               :always packet
               :do (remhash (tcp-connection-r-next connection)
                            (tcp-connection-rx-data-unordered connection))
               :do (setf (cdr (last (tcp-connection-rx-data connection)))
                         (list (list packet (+ start header-length) end)))
               :do (setf (tcp-connection-r-next connection)
                         (logand (+ (tcp-connection-r-next connection) data-length)
                                 #xFFFFFFFF)))
         (setf (mezzano.supervisor:event-state
                (tcp-connection-receive-event connection))
               t))
        ;; Add future packet to tcp-connection-rx-data-unordered
        ((> seq (tcp-connection-r-next connection))
         (unless (gethash seq (tcp-connection-rx-data-unordered connection))
           (setf (gethash seq (tcp-connection-rx-data-unordered connection))
                 (list (list packet (+ start header-length) end))))))
  (cond ((<= seq (tcp-connection-r-next connection))
         (tcp4-send-packet connection
                           (tcp-connection-s-next connection)
                           (tcp-connection-r-next connection)
                           nil
                           :ack-p t))))

(defun tcp4-receive (connection packet start end listener)
  ;; Don't use WITH-TCP-CONNECTION-LOCKED here. No errors should occur
  ;; in here, so this avoids truncating the backtrace with :resignal-errors.
  (mezzano.supervisor:with-mutex ((tcp-connection-lock connection))
    (let* ((seq (ub32ref/be packet (+ start +tcp4-header-sequence-number+)))
           (ack (ub32ref/be packet (+ start +tcp4-header-acknowledgment-number+)))
           (flags-and-data-offset (ub16ref/be packet (+ start +tcp4-header-flags-and-data-offset+)))
           (flags (ldb (byte 12 0) flags-and-data-offset))
           (header-length (* (ldb (byte 4 12) flags-and-data-offset) 4))
           (data-length (- end (+ start header-length))))
      (when (logtest flags +tcp4-flag-rst+)
        ;; Remote have sended RST , aborting connection
        (setf (tcp-connection-state connection) :closed)
        (setf (tcp-connection-pending-error connection)
              (make-condition 'connection-aborted
                              :host (tcp-connection-remote-ip connection)
                              :port (tcp-connection-remote-port connection)))
        (detach-tcp-connection connection))
      (case (tcp-connection-state connection)
        (:syn-sent
         ;; Active open
         (cond ((and (logtest flags +tcp4-flag-ack+)
                     (logtest flags +tcp4-flag-syn+)
                     (eql ack (tcp-connection-s-next connection)))
                ;; Remote have sended SYN+ACK and waiting for ACK
                (setf (tcp-connection-state connection) :established
                      (tcp-connection-r-next connection) (logand (1+ seq) #xFFFFFFFF))
                (tcp4-send-packet connection ack (tcp-connection-r-next connection) nil)
                ;; Cancel retransmit. FIXME: This ACK might need to be retransmitted.
                (disarm-retransmit-timer connection)
                (disarm-timeout-timer connection))
               ((logtest flags +tcp4-flag-syn+)
                ;; Simultaneous open
                (setf (tcp-connection-state connection) :syn-received
                      (tcp-connection-r-next connection) (logand (1+ seq) #xFFFFFFFF))
                (tcp4-send-packet connection ack (tcp-connection-r-next connection) nil
                                  :ack-p t :syn-p t)
                ;; Cancel retransmit. FIXME: This SYN/ACK might need to be retransmitted.
                (disarm-retransmit-timer connection)
                (disarm-timeout-timer connection))
               (t
                ;; Aborting connection
                (tcp4-send-packet connection ack seq nil :rst-p t)
                (setf (tcp-connection-state connection) :closed)
                (setf (tcp-connection-pending-error connection)
                      (make-condition 'connection-aborted
                                      :host (tcp-connection-remote-ip connection)
                                      :port (tcp-connection-remote-port connection)))
                (detach-tcp-connection connection))))
        (:syn-received
         ;; Pasive open
         (cond ((and (eql flags +tcp4-flag-ack+)
                     (eql seq (tcp-connection-r-next connection))
                     (eql ack (tcp-connection-s-next connection)))
                ;; Remote have sended ACK , connection established
                (setf (tcp-connection-state connection) :established)
                (when listener
                  (remhash connection (tcp-listener-pending-connections listener))
                  (mezzano.sync:mailbox-send connection (tcp-listener-connections listener))))
               ;; Ignore duplicated SYN packets
               ((and (logtest flags +tcp4-flag-syn+)
                     (eql ack (1- (tcp-connection-s-next connection)))))
               (t
                ;; Aborting connection
                (tcp4-send-packet connection ack seq nil :rst-p t)
                (setf (tcp-connection-state connection) :closed)
                (setf (tcp-connection-pending-error connection)
                      (make-condition 'connection-aborted
                                      :host (tcp-connection-remote-ip connection)
                                      :port (tcp-connection-remote-port connection)))
                (detach-tcp-connection connection)
                (when (and listener
                           (tcp-listener-backlog listener))
                  (remhash connection (tcp-listener-pending-connections listener))
                  (decf (tcp-listener-n-pending-connections listener))))))
        (:established
         (if (zerop data-length)
             (when (and (= seq (tcp-connection-r-next connection))
                        (logtest flags +tcp4-flag-fin+))
               ;; Remote have sended FIN and waiting for ACK
               (setf (tcp-connection-state connection) :close-wait
                     (tcp-connection-r-next connection)
                     (logand (+ (tcp-connection-r-next connection) 1)
                             #xFFFFFFFF))
               (tcp4-send-packet connection ack seq nil :ack-p t))
             (tcp4-receive-data connection data-length end header-length packet seq start)))
        (:close-wait
         ;; Remote has closed, local can still send data.
         ;; Not much to do here, just waiting for the application to close.
         )
        (:last-ack
         ;; Local closed, waiting for remote to ACK.
         (when (logtest flags +tcp4-flag-ack+)
           ;; Remote have sended ACK , connection closed
           (setf (tcp-connection-state connection) :closed)
           (detach-tcp-connection connection)))
        (:fin-wait-1
         ;; Local closed, waiting for remote to close.
         (if (zerop data-length)
             (when (= seq (tcp-connection-r-next connection))
               (cond ((logtest flags +tcp4-flag-fin+)
                      (setf (tcp-connection-r-next connection)
                            (logand (+ (tcp-connection-r-next connection) 1)
                                    #xFFFFFFFF))
                      (tcp4-send-packet connection
                                        (tcp-connection-s-next connection)
                                        (tcp-connection-r-next connection)
                                        nil)
                      (if (logtest flags +tcp4-flag-ack+)
                          ;; Remote saw our FIN and closed as well.
                          (progn
                            (setf (tcp-connection-state connection) :closed)
                            (detach-tcp-connection connection))
                          ;; Simultaneous close
                          (setf (tcp-connection-state connection) :closing)))
                     ((logtest flags +tcp4-flag-ack+)
                      ;; Remote saw our FIN
                      (setf (tcp-connection-state connection) :fin-wait-2))))
             (tcp4-receive-data connection data-length end header-length packet seq start)))
        (:fin-wait-2
         ;; Local closed, still waiting for remote to close.
         (if (zerop data-length)
             (when (and (= seq (tcp-connection-r-next connection))
                        (logtest flags +tcp4-flag-fin+))
               ;; Remote have sended FIN and waiting for ACK
               (setf (tcp-connection-r-next connection)
                     (logand (+ (tcp-connection-r-next connection) 1)
                             #xFFFFFFFF))
               (tcp4-send-packet connection
                                 (tcp-connection-s-next connection)
                                 (tcp-connection-r-next connection)
                                 nil)
               (setf (tcp-connection-state connection) :closed)
               (detach-tcp-connection connection))
             (tcp4-receive-data connection data-length end header-length packet seq start)))
        (:closing
         ;; Waiting for ACK
         (when (and (eql seq (tcp-connection-r-next connection))
                    (logtest flags +tcp4-flag-ack+))
           ;; Remote have sended ACK , connection closed
           (detach-tcp-connection connection)
           (setf (tcp-connection-state connection) :closed)))
        (t
         ;; Aborting connection
         (tcp4-send-packet connection ack seq nil :rst-p t)
         (setf (tcp-connection-state connection) :closed)
         (setf (tcp-connection-pending-error connection)
               (make-condition 'connection-aborted
                               :host (tcp-connection-remote-ip connection)
                               :port (tcp-connection-remote-port connection)))
         (detach-tcp-connection connection))))
    (mezzano.supervisor:condition-notify (tcp-connection-cvar connection) t)))

(defun tcp4-send-packet (connection seq ack data &key cwr-p ece-p urg-p (ack-p t) psh-p rst-p syn-p fin-p)
  (let* ((source (tcp-connection-local-ip connection))
         (source-port (tcp-connection-local-port connection))
         (packet (assemble-tcp4-packet source source-port
                                       (tcp-connection-remote-ip connection)
                                       (tcp-connection-remote-port connection)
                                       seq ack
                                       (tcp-connection-window-size connection)
                                       data
                                       :cwr-p cwr-p
                                       :ece-p ece-p
                                       :urg-p urg-p
                                       :ack-p ack-p
                                       :psh-p psh-p
                                       :rst-p rst-p
                                       :syn-p syn-p
                                       :fin-p fin-p)))
    (mezzano.network.ip:transmit-ipv4-packet
     source (tcp-connection-remote-ip connection)
     mezzano.network.ip:+ip-protocol-tcp+ packet)))

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
         (header (make-array 20 :element-type '(unsigned-byte 8)))
         (packet (list header payload)))
    ;; Assemble the TCP header.
    (setf (ub16ref/be header +tcp4-header-source-port+) src-port
          (ub16ref/be header +tcp4-header-destination-port+) dst-port
          (ub32ref/be header +tcp4-header-sequence-number+) seq-num
          (ub32ref/be header +tcp4-header-acknowledgment-number+) ack-num
          ;; Data offset/header length (5 32-bit words) and flags.
          (ub16ref/be header +tcp4-header-flags-and-data-offset+) (logior #x5000
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
    ;; Compute the final checksum.
    (setf checksum (compute-ip-pseudo-header-partial-checksum
                    (mezzano.network.ip::ipv4-address-address src-ip)
                    (mezzano.network.ip::ipv4-address-address dst-ip)
                    mezzano.network.ip:+ip-protocol-tcp+
                    (+ (length header) payload-size)))
    (setf checksum (mezzano.network.ip:compute-ip-partial-checksum header 0 nil checksum))
    (setf checksum (mezzano.network.ip:compute-ip-checksum payload 0 nil checksum))
    (setf (ub16ref/be header +tcp4-header-checksum+) checksum)
    packet))

(defun allocate-local-tcp-port (local-ip ip port)
  (loop :for local-port := (+ (random 32768) 32768)
        :do (unless (get-tcp-connection ip port local-ip local-port)
              (return local-port))))

(defun abort-connection (connection)
  (setf (tcp-connection-state connection) :closed)
  (tcp4-send-packet connection
                    (tcp-connection-s-next connection)
                    (tcp-connection-r-next connection)
                    nil
                    :rst-p t)
  (mezzano.sync.dispatch:dispatch-async
   (lambda ()
     (detach-tcp-connection connection))
   sys.net::*network-serial-queue*))

(defun close-tcp-connection (connection)
  (ecase (tcp-connection-state connection)
    (:syn-sent
     (abort-connection connection))
    ((:established :syn-received)
     (setf (tcp-connection-state connection) :fin-wait-1)
     (tcp4-send-packet connection
                       (tcp-connection-s-next connection)
                       (tcp-connection-r-next connection)
                       nil
                       :fin-p t))
    (:close-wait
     (setf (tcp-connection-state connection) :last-ack)
     (tcp4-send-packet connection
                       (tcp-connection-s-next connection)
                       (tcp-connection-r-next connection)
                       nil
                       :fin-p t))
    (:closed)))

(define-condition network-error (error)
  ())

(define-condition connection-error (network-error)
  ((host :initarg :host :reader connection-error-host)
   (port :initarg :port :reader connection-error-port)))

(define-condition connection-aborted (connection-error)
  ())

(define-condition connection-timed-out (connection-error)
  ())

(define-condition connection-stale (connection-error)
  ())

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
        (setf (tcp-connection-pending-error connection)
              (make-condition 'connection-stale
                              :host (tcp-connection-remote-ip connection)
                              :port (tcp-connection-remote-port connection)))
        (setf (tcp-connection-state connection) :closed)
        (mezzano.supervisor:condition-notify (tcp-connection-cvar connection) t)
        (detach-tcp-connection connection)))))

(defun check-connection-error (connection)
  (let ((condition (tcp-connection-pending-error connection)))
    (when condition
      (error condition))))

(defun tcp-connect (ip port &key persist)
  (let* ((interface (nth-value 1 (mezzano.network.ip:ipv4-route ip)))
         (source-address (mezzano.network.ip:ipv4-interface-address interface))
         (source-port (allocate-local-tcp-port source-address ip port))
         (seq (random #x100000000))
         (connection (make-instance 'tcp-connection
                                    :state :syn-sent
                                    :local-port source-port
                                    :local-ip source-address
                                    :remote-port port
                                    :remote-ip ip
                                    :s-next (logand #xFFFFFFFF (1+ seq))
                                    :r-next 0
                                    :window-size 8192
                                    :boot-id (if persist nil (mezzano.supervisor:current-boot-id)))))
    (mezzano.sync.dispatch:dispatch-async
     (lambda ()
       (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
         (push connection *tcp-connections*))
       (tcp4-send-packet connection seq 0 nil :ack-p nil :syn-p t)
       (arm-retransmit-timer *tcp-connect-initial-retransmit-time* connection)
       (arm-timeout-timer *tcp-connect-timeout* connection))
     sys.net::*network-serial-queue*)
    (with-tcp-connection-locked connection
      (mezzano.supervisor:condition-wait-for ((tcp-connection-cvar connection)
                                              (tcp-connection-lock connection))
        (not (eql (tcp-connection-state connection) :syn-sent)))
      (check-connection-error connection))
    connection))

(defun tcp-send (connection data &optional (start 0) end)
  (when (or (eql (tcp-connection-state connection) :established)
            (eql (tcp-connection-state connection) :close-wait))
    (setf end (or end (length data)))
    (let ((mss (tcp-connection-max-seg-size connection)))
      (cond ((>= start end))
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

(defclass tcp-octet-stream (gray:fundamental-binary-input-stream
                            gray:fundamental-binary-output-stream)
  ((connection :initarg :connection :reader tcp-stream-connection)
   (current-packet :initform nil :accessor tcp-stream-packet)))

(defclass tcp-stream (gray:fundamental-character-input-stream
                      gray:fundamental-character-output-stream
                      sys.int::external-format-mixin
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
               (endp (tcp-connection-rx-data connection)))
      (setf (mezzano.supervisor:event-state
             (tcp-connection-receive-event connection))
            nil))))

(defmethod gray:stream-listen-byte ((stream tcp-octet-stream))
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (refill-tcp-packet-buffer-no-hang stream)
    (not (null (tcp-stream-packet stream)))))

(defmethod gray:stream-read-byte ((stream tcp-octet-stream))
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (when (not (refill-tcp-packet-buffer stream))
      (return-from gray:stream-read-byte :eof))
    (let* ((packet (tcp-stream-packet stream))
           (byte (aref (first packet) (second packet))))
      (when (>= (incf (second packet)) (third packet))
        (setf (tcp-stream-packet stream) nil))
      (maybe-clear-receive-ready-event stream)
      byte)))

(defmethod gray:stream-read-byte-no-hang ((stream tcp-octet-stream))
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (refill-tcp-packet-buffer-no-hang stream)
    (when (not (tcp-stream-packet stream))
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
  (when (not end)
    (setf end (length sequence)))
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
      (maybe-clear-receive-ready-event stream)
      position)))

(defmethod gray:stream-write-byte ((stream tcp-octet-stream) byte)
  (let ((ary (make-array 1 :element-type '(unsigned-byte 8)
                         :initial-element byte)))
    (tcp-send (tcp-stream-connection stream) ary)))

(defmethod gray:stream-write-sequence ((stream tcp-octet-stream) sequence &optional (start 0) end)
  (tcp-send (tcp-stream-connection stream) sequence start end))

(defmethod close ((stream tcp-octet-stream) &key abort)
  (declare (ignore abort))
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (close-tcp-connection (tcp-stream-connection stream))))

(defmethod open-stream-p ((stream tcp-octet-stream))
  (with-tcp-connection-locked (tcp-stream-connection stream)
    (let ((connection (tcp-stream-connection stream)))
      (refill-tcp-packet-buffer-no-hang stream)
      (or (tcp-stream-packet stream)
          (connection-may-have-additional-data-p connection)))))

(defmethod stream-element-type ((stream tcp-octet-stream))
  '(unsigned-byte 8))

(defun tcp-stream-connect (address port &key element-type external-format)
  (cond ((or (not element-type)
             (sys.int::type-equal element-type 'character))
         (make-instance 'tcp-stream
                        :connection (tcp-connect (sys.net::resolve-address address) port)
                        :external-format (sys.int::make-external-format
                                          (or element-type 'character)
                                          (or external-format :default)
                                          :eol-style :crlf)))
        ((sys.int::type-equal element-type '(unsigned-byte 8))
         (assert (or (not external-format)
                     (eql external-format :default)))
         (make-instance 'tcp-octet-stream
                        :connection (tcp-connect (sys.net::resolve-address address) port)))
        (t
         (error "Unsupported element type ~S" element-type))))
