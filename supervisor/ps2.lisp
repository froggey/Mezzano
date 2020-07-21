;;;; Supervisor-side of the PS/2 keyboard and mouse drivers.

(in-package :mezzano.supervisor)

;; The controller is wired up this way on the PC.
(defconstant +ps/2-data-port+ #x60)
(defconstant +ps/2-control-port+ #x64)
(defconstant +ps/2-key-irq+ 1)
(defconstant +ps/2-aux-irq+ 12)

;;; PS/2 controller commands.
(defconstant +ps/2-read-config-byte+ #x20)
(defconstant +ps/2-write-config-byte+ #x60)
(defconstant +ps/2-disable-aux-port+ #xA7)
(defconstant +ps/2-enable-aux-port+ #xA8)
(defconstant +ps/2-test-aux-port+ #xA9)
(defconstant +ps/2-test-controller+ #xAA)
(defconstant +ps/2-test-key-port+ #xAB)
(defconstant +ps/2-disable-key-port+ #xAD)
(defconstant +ps/2-enable-key-port+ #xAE)
(defconstant +ps/2-read-controller-output-port+ #xD0)
(defconstant +ps/2-write-controller-output-port+ #xD1)
(defconstant +ps/2-write-key-port+ #xD2)
(defconstant +ps/2-write-aux-port+ #xD4)

;;; PS/2 status register bits.
(defconstant +ps/2-status-output-buffer-status+ (ash 1 0))
(defconstant +ps/2-status-input-buffer-status+ (ash 1 1))
(defconstant +ps/2-status-command/data+ (ash 1 3))
(defconstant +ps/2-status-timeout-error+ (ash 1 6))
(defconstant +ps/2-status-parity-error+ (ash 1 7))

;;; PS/2 config byte bits.
(defconstant +ps/2-config-key-interrupt+ (ash 1 0))
(defconstant +ps/2-config-aux-interrupt+ (ash 1 1))
(defconstant +ps/2-config-key-clock+ (ash 1 4))
(defconstant +ps/2-config-aux-clock+ (ash 1 5))
(defconstant +ps/2-config-key-translation+ (ash 1 6))

(defconstant +ps/2-command-resend+ #xFE)

(sys.int::defglobal *ps/2-present*)
(sys.int::defglobal *ps/2-key-fifo*)
(sys.int::defglobal *ps/2-aux-fifo*)
(sys.int::defglobal *ps/2-controller-lock*)
(sys.int::defglobal *ps/2-mouse-device-id*)

(sys.int::defglobal *ps/2-debug-dump-state*)

(defun ps/2-irq-handler (fifo update-debug-state)
  (let ((byte (sys.int::io-port/8 +ps/2-data-port+)))
    (irq-fifo-push byte fifo)
    (when update-debug-state
      (case *ps/2-debug-dump-state*
        ;; Start. Expect left-meta.
        (0 (cond ((eql byte #x38) ; left-meta
                  (setf *ps/2-debug-dump-state* 1))))
        ;; Saw left-meta press
        (1 (cond ((eql byte #x38) ; left-meta
                  ;; Stay in this state.
                  )
                 ((eql byte #x57) ; F11
                  (debug-magic-button)
                  (setf *ps/2-debug-dump-state* 0))
                 (t
                  (setf *ps/2-debug-dump-state* 0))))
        (t
         (setf *ps/2-debug-dump-state* 0))))))

(defun ps/2-key-irq-handler (interrupt-frame irq)
  (declare (ignore interrupt-frame irq))
  (ps/2-irq-handler *ps/2-key-fifo* t)
  :completed)

(defun ps/2-aux-irq-handler (interrupt-frame irq)
  (declare (ignore interrupt-frame irq))
  (ps/2-irq-handler *ps/2-aux-fifo* nil)
  :completed)

(defun ps/2-input-wait (&optional (what "data"))
  "Wait for space in the input buffer."
  ;; FIXME: Wait 1ms or something instead of this.
  (dotimes (i 100000
            (debug-print-line "PS/2: Timeout waiting for " what "."))
    (when (zerop (logand (sys.int::io-port/8 +ps/2-control-port+) +ps/2-status-input-buffer-status+))
      (return t))))

(defun ps/2-output-wait (&optional (what "data"))
  "Wait for data to become available in the output buffer."
  ;; FIXME: Wait 1ms or something instead of this.
  (dotimes (i 100000
            (debug-print-line "PS/2: Timeout waiting for " what "."))
    (when (not (zerop (logand (sys.int::io-port/8 +ps/2-control-port+) +ps/2-status-output-buffer-status+)))
      (return t))))

(defun ps/2-output-read (&optional (what "data"))
  (if (ps/2-output-wait what)
      (sys.int::io-port/8 +ps/2-data-port+)
      :timeout))

(defun ps/2-port-write (byte command)
  (without-interrupts
    (with-symbol-spinlock (*ps/2-controller-lock*)
      (ps/2-input-wait)
      (setf (sys.int::io-port/8 +ps/2-control-port+) command)
      (ps/2-input-wait)
      (setf (sys.int::io-port/8 +ps/2-data-port+) byte))))

(defun ps/2-key-write (byte)
  "Write a byte to the key port."
  (ps/2-port-write byte +ps/2-write-key-port+))

(defun ps/2-key-read (&optional (wait-p t))
  "Read a byte from the key port."
  (irq-fifo-pop *ps/2-key-fifo* wait-p))

(defun ps/2-aux-write (byte)
  "Write a byte to the aux port."
  (ps/2-port-write byte +ps/2-write-aux-port+))

(defun ps/2-aux-read (&optional (wait-p t))
  "Read a byte from the aux port."
  (irq-fifo-pop *ps/2-aux-fifo* wait-p))

(defun initialize-ps/2 ()
  (setf *ps/2-present* nil)
  (setf *ps/2-debug-dump-state* 0)
  (when (not (boundp '*ps/2-controller-lock*))
    (setf *ps/2-controller-lock* :unlocked
          *ps/2-key-fifo* (make-irq-fifo 50 :element-type '(unsigned-byte 8) :name "PS/2 key fifo")
          *ps/2-aux-fifo* (make-irq-fifo 50 :element-type '(unsigned-byte 8) :name "PS/2 aux fifo")))
  (irq-fifo-reset *ps/2-key-fifo*)
  (irq-fifo-reset *ps/2-aux-fifo*))

(defun check-ps/2-response (response why)
  (let ((data (ps/2-output-read why)))
    (unless (eql response data)
      (panic "Invalid mouse response. Expected " response " got " data "(" why ")"))))

(defun ps/2-mouse-init ()
  (setf *ps/2-mouse-device-id* nil)
  ;; Probe for a mouse.
  (ps/2-aux-write #xF2) ; Get device ID.
  (let ((mouse-id-ack (ps/2-output-read "get device id (status)")))
    (when (not (eql mouse-id-ack #xFA))
      (debug-print-line "PS/2 mouse init got " mouse-id-ack " during probe (1)")
      (return-from ps/2-mouse-init)))
  (let ((device-id-1 (ps/2-output-read "get device id (byte 1)")))
    ;; Device ID can be up to two bytes long, ignore the second byte.
    (ps/2-output-read "get device id (byte 2, timeout expected)")
    (when (not (member device-id-1 '(#x00 #x03 #x04)))
      (debug-print-line "PS/2 mouse init got unknown device id " device-id-1 " during probe (2)")
      (return-from ps/2-mouse-init))
    (debug-print-line "Detected PS/2 mouse with device ID " device-id-1))
  ;; Enable mouse defaults & reporting.
  (ps/2-aux-write #xF6)                 ; Set defaults (reset)
  (check-ps/2-response #xFA "set defaults")
  ;; Enable Intellimouse mode.
  ;; TODO: Recheck the device ID after doing this to see if intellimouse mode
  ;; was really enabled.
  (ps/2-aux-write #xF3)                 ; Intellimouse magic sequence i.e.,
  (check-ps/2-response #xFA "intellimouse rate 200 (1)")
  (ps/2-aux-write #xC8)                 ; support wheel mouse
  (check-ps/2-response #xFA "intellimouse rate 200 (2)") ; see documentation in
  (ps/2-aux-write #xF3)                 ; gui/input-drivers.lisp
  (check-ps/2-response #xFA "intellimouse rate 100 (1)")
  (ps/2-aux-write #x64)
  (check-ps/2-response #xFA "intellimouse rate 100 (2)")
  (ps/2-aux-write #xF3)
  (check-ps/2-response #xFA "intellimouse rate 80 (1)")
  (ps/2-aux-write #x50)
  (check-ps/2-response #xFA "intellimouse rate 80 (2)")
  ;; Recheck the device ID to see if intellimouse mode was really enabled.
  (ps/2-aux-write #xF2) ; Get device ID.
  (check-ps/2-response #xFA "get device id (second, status)")
  (let ((device-id-1 (ps/2-output-read "get device id (second, byte 1)")))
    ;; Device ID can be up to two bytes long, ignore the second byte.
    (ps/2-output-read "get device id (second, byte 2, timeout expected)")
    (setf *ps/2-mouse-device-id* device-id-1)
    (debug-print-line "Detected PS/2 mouse with device ID " device-id-1)
    (when (eql device-id-1 #x03)
      (debug-print-line "PS/2 intellimouse mode enabled")))
  (ps/2-aux-write #xF4)                 ; Mouse enable (streaming mode)
  (check-ps/2-response #xFA "mouse enable"))

(defun probe-ps/2 ()
  (debug-print-line "Probing PS/2")
  (setf *ps/2-present* t)
  ;; Enable the aux port.
  (ps/2-input-wait)
  (setf (sys.int::io-port/8 +ps/2-control-port+) +ps/2-enable-aux-port+)
  ;; Enable interrupts for key & aux.
  (ps/2-input-wait)
  (setf (sys.int::io-port/8 +ps/2-control-port+) +ps/2-read-config-byte+)
  (ps/2-output-wait)
  (let ((config (logior (sys.int::io-port/8 +ps/2-data-port+)
                        +ps/2-config-key-interrupt+
                        +ps/2-config-aux-interrupt+)))
    (ps/2-input-wait)
    (setf (sys.int::io-port/8 +ps/2-control-port+) +ps/2-write-config-byte+)
    (ps/2-input-wait)
    (setf (sys.int::io-port/8 +ps/2-data-port+) config))
  (ps/2-mouse-init)
  ;; Enable both IRQs.
  (irq-attach (platform-irq +ps/2-key-irq+)
              'ps/2-key-irq-handler
              'ps/2-key
              :exclusive t)
  (irq-attach (platform-irq +ps/2-aux-irq+)
              'ps/2-aux-irq-handler
              'ps/2-aux
              :exclusive t)
  ;; Data may have accumulated in the FIFOs.
  (irq-fifo-reset *ps/2-key-fifo*)
  (irq-fifo-reset *ps/2-aux-fifo*))
