;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
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

(defvar *tcp-connections* nil)
(defvar *tcp-connection-lock* (mezzano.supervisor:make-mutex "TCP connection list"))
(defvar *allocated-tcp-ports* nil)

(defvar *server-alist* '())

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

(defun get-tcp-connection (remote-ip remote-port local-port)
  (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
    (dolist (connection *tcp-connections*)
      (when (and (mezzano.network.ip:address-equal
                  (tcp-connection-remote-ip connection)
                  remote-ip)
                 (eql (tcp-connection-remote-port connection) remote-port)
                 (eql (tcp-connection-local-port connection) local-port))
        (return connection)))))

(defun %tcp4-receive (packet remote-ip start end)
  (let* ((remote-port (ub16ref/be packet (+ start +tcp4-header-source-port+)))
         (local-port (ub16ref/be packet (+ start +tcp4-header-destination-port+)))
         (flags (ub16ref/be packet (+ start +tcp4-header-flags-and-data-offset+)))
         (connection (get-tcp-connection remote-ip remote-port local-port)))
    (cond
      (connection
       (tcp4-receive connection packet start end))
      ((eql flags +tcp4-flag-syn+)
       (format t "Establishing TCP connection. l ~D  r ~D  from ~X.~%" local-port remote-port remote-ip)
       (let* ((seq (ub32ref/be packet (+ start +tcp4-header-sequence-number+)))
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

(defun detach-tcp-connection (connection)
  (mezzano.supervisor:with-mutex (*tcp-connection-lock*)
    (setf *tcp-connections* (remove connection *tcp-connections*)))
  (setf *allocated-tcp-ports* (remove (tcp-connection-local-port connection) *allocated-tcp-ports*)))

(defun tcp4-receive (connection packet &optional (start 0) end)
  (unless end (setf end (length packet)))
  (with-tcp-connection-locked connection
    (let* ((seq (ub32ref/be packet (+ start +tcp4-header-sequence-number+)))
           (ack (ub32ref/be packet (+ start +tcp4-header-acknowledgment-number+)))
           (flags (ub16ref/be packet (+ start +tcp4-header-flags-and-data-offset+)))
           (header-length (* (ldb (byte 4 12) flags) 4))
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
  (multiple-value-bind (remote-host interface)
      (mezzano.network.ip:ipv4-route (tcp-connection-remote-ip connection))
    (cond ((and remote-host interface)
           (let* ((source (mezzano.network.ip:ipv4-interface-address interface))
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
             (mezzano.network.ip:transmit-ipv4-packet-on-interface
              remote-host interface packet)))
           (t (format t "No route to ~A? Discarding TCPv4 packet.~%"
                      (tcp-connection-remote-ip connection))))))

(defun compute-ip-pseudo-header-partial-checksum (src-ip dst-ip protocol length)
  (+ (logand src-ip #xFFFF)
     (logand (ash src-ip -16) #xFFFF)
     (logand dst-ip #xFFFF)
     (logand (ash dst-ip -16) #xFFFF)
     protocol
     length))

(defun assemble-tcp4-packet (src-ip src-port dst-ip dst-port seq-num ack-num window payload
			     &key (ack-p t) psh-p rst-p syn-p fin-p)
  "Build a full TCP & IP header."
  (mezzano.network.ip::assemble-ipv4-packet
   src-ip dst-ip mezzano.network.ip:+ip-protocol-tcp+
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
                                                                          (if ack-p +tcp4-flag-ack+ 0))
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
    packet)))

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

(defclass tcp-stream (sys.gray:fundamental-character-input-stream
                      sys.gray:fundamental-character-output-stream
                      sys.gray:fundamental-binary-input-stream
                      sys.gray:fundamental-binary-output-stream
                      sys.gray:unread-char-mixin)
  ((connection :initarg :connection :reader tcp-stream-connection)
   (current-packet :initform nil :accessor tcp-stream-packet)))

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

(defmethod sys.gray:stream-read-char ((stream tcp-stream))
  (let ((leader (read-byte stream nil)))
    (unless leader
      (return-from sys.gray:stream-read-char :eof))
    (when (eql leader #x0D)
      (read-byte stream nil)
      (setf leader #x0A))
    (multiple-value-bind (length code-point)
        (sys.net::utf-8-decode-leader leader)
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
         (setf sequence (sys.net::encode-utf-8-string sequence start end)))
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

(defun tcp-stream-connect (address port)
  (make-instance 'tcp-stream :connection (tcp-connect (sys.net::resolve-address address) port)))
