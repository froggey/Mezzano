;;;; 8042.lisp --- driver for the Intel 8042 controller

;;; Commentary:

;; Definitions:
;;
;; PS/2:        Personal System/2 standard for keyboard and mouse ports.
;; 8042:        Intel 8042, used to be a single chip, now part of Advanced
;;              Integrated Peripheral (AIP).  Acts as the PS/2
;;              keyboard and mouse controller.
;; byte:        An 8-bit byte.
;; I/O address: An offset into the input/output address space.
;; Register:    Used instead of "data port" so that "port" describes an
;;              attachment point exclusively.
;; Port:        An attachment point on the PS/2 controller for a device.
;; IRQ:         Interrupt request.
;; FIFO:        First in/first out queue.

;;; Code:

(in-package :mezzano.supervisor)

;; PS/2 controller registers.
(defconstant +8042-register-data+ #x60
  "The I/O address of the PS/2 controller's read-write data register.")
(defconstant +8042-register-status+ #x64
  "The I/O address of the PS/2 controller's read-only status register.")
(defconstant +8042-register-command+ #x64
  "The I/O address of the PS/2 controller's write-only command register.")

;; Interrupt request lines.
(defconstant +8042-interrupt-request-line-keyboard+ 1
  "The PS/2 controller's keyboard interrupt request line.
This interrupt request number is the x86-specific value.")
(defconstant +8042-interrupt-request-line-auxiliary+ 12
  "The PS/2 controller's auxiliary (typically mouse) interrupt request line.
This interrupt request number is the x86-specific value.")

;; Status register masks.
(defconstant +8042-status-mask-output-buffer-full+ #x1
  "Mask for bit 0 of the status register.
Bit 0 is clear, 0, if the output buffer is empty, or set, 1, if the
output buffer is full.  The term \"output\" is relative to the PS/2
device; the PS/2 device sends output to the 8042.  Therefore Mezzano
reads from the output buffer.")
(defconstant +8042-status-mask-input-buffer-full+ #x2
  "Mask for bit 1 of the status register.
Bit 1 is clear, 0, if the input buffer is empty, or set, 1, if the
output buffer is full.  The term \"input\" is relative to the PS/2
device; the PS/2 device receives input from the 8042.  Therefore
Mezzano writes to the input buffer.")

;; PS/2 controller communication timeout.
(defconstant +8042-delay+ 0.01
  "Per-interval delay.
The number of seconds to sleep, using `safe-sleep', while waiting for
a PS/2 controller operation to complete.  This is set to the minimum
delay possible given the limited granularity of `safe-sleep'.")
(defconstant +8042-timeout+ 50
  "Timeout interval count.
The number of intervals, each `+8042-delay+' seconds long, to wait for
PS/2 controller operations to complete.")

;; Flushing.
(defconstant +8042-data-buffer-maximum-size+ 16
  "The maximum size, in bytes, of any PS/2 controller's data buffer.")

;; PS/2 controller commands.
(defconstant +8042-command-configuration-byte-read+ #x20
  "Read the PS/2 controller's configuration byte.")
(defconstant +8042-command-configuration-byte-write+ #x60
  "Write the PS/2 controller's configuration byte.")
(defconstant +8042-command-auxiliary-port-disable+ #xa7
  "Disable the auxiliary port of the PS/2 controller.")
(defconstant +8042-command-auxiliary-port-enable+ #xa8
  "Enable the auxiliary port of the PS/2 controller.")
(defconstant +8042-command-auxiliary-port-test+ #xa9
  "Test the auxiliary port of the PS/2 controller.")
(defconstant +8042-command-auxiliary-port-device-write+ #xd4
  "Write to the device attached to the auxiliary port of the PS/2 controller.")
(defconstant +8042-command-keyboard-port-disable+ #xad
  "Disable the keyboard port on the PS/2 controller.")
(defconstant +8042-command-keyboard-port-test+ #xab
  "Test the keyboard port on the PS/2 controller.")
(defconstant +8042-command-controller-test+ #xaa
  "Test the PS/2 controller itself.")
(defconstant +8042-command-device-reset+ #xff
  "Reset a PS/2 device.
Also referred to as BAT, basic assurance test.")
(defconstant +8042-command-reboot+ #xfe
  "Send a reboot request to the PS/2 controller.")

;; PS/2 controller configuration byte bits.
(defconstant +8042-configuration-bit-interrupt-keyboard+ #x1
  "The keyboard port interrupt bit mask.
Read-write bit 0 in the PS/2 controller configuration byte indicates
if interrupts are enabled for the keyboard port.  Bit value 0 means
disabled, 1 means enabled.")
(defconstant +8042-configuration-bit-interrupt-auxiliary+ #x2
  "The auxiliary port interrupt bit mask.
Read-write bit 1 in the PS/2 controller configuration byte indicates
if interrupts are enabled for the auxiliary port.  Bit value 0 means
disabled, 1 means enabled.")
(defconstant +8042-configuration-bit-clock-keybaord+ #x10
  "The keyboard port interrupt bit mask.
Read-write bit 4 in the PS/2 controller configuration byte indicates
if clocks are enabled for the keyboard port.  Bit value 0 means
enabled, 1 means disabled.")
(defconstant +8042-configuration-bit-clock-auxiliary+ #x20
  "The auxiliary port interrupt bit mask.
Read-write bit 5 in the PS/2 controller configuration byte indicates
if clocks are enabled for the auxiliary port.  Bit value 0 means
enabled, 1 means disabled.")
(defconstant +8042-configuration-bit-translation+ #x40
  "The translation mode bit mask.
Read-write bit 6 in the PS/2 controller configuration byte indicates
if scan-code-set-2 to scan-code-set-1 translation is enabled in the
PS/2 controller.  Bit value 1 means enabled, 0 means disabled.")

(defconstant +8042-response-acknowledge+ #xfa
  "Acknowledge response from PS/2 controller.")
(defconstant +8042-response-self-test-success+ #x55
  "Self-test success response from PS/2 controller.")

;; PS/2 mouse commands and response.
(defconstant +ps/2-mouse-command-get-device-identifier+ #xf2
  "Get the PS/2 mouse device identifer.")
(defconstant +ps/2-mouse-command-set-defaults+ #xf6
  "Restore PS/2 mouse default configuration.")
(defconstant +ps/2-mouse-command-set-sample-rate+ #xf3
  "Set PS/2 mouse sample rate.")
(defconstant +ps/2-mouse-command-set-stream-mode+ #xf4
  "Set PS/2 mouse stream mode.")

(defconstant +ps/2-mouse-response-test-success+ #xaa
  "Acknowledge response from PS/2 controller.")

;; PS/2 mouse device identifiers.
(defconstant +ps/2-mouse-device-identifier-standard+ #x00
  "The device identifier for a standard PS/2 mouse.")
(defconstant +ps/2-mouse-device-identifier-intellimouse+ #x03
  "The device identifier for an IntelliMouse PS/2 mouse.")
(defconstant +ps/2-mouse-device-identifier-explorer+ #x04
  "The device identifier for an IntelliMouse/Explorer PS/2 mouse.")

;; PS/2 keyboard command.
(defconstant +ps/2-keyboard-command-set-scan-code-set+ #xf0
  "Set PS/2 keyboard scan code set.")

;; PS/2 keyboard scan codes.
(defconstant +ps/2-keyboard-scan-code-left-meta+ #x38
  "The scan code for left-meta (left-alt) being pressed.
This assumes PS/2 scan code set 1 is in effect.")
(defconstant +ps/2-keyboard-scan-code-f11+ #x57
  "The scan code for F11 being pressed.
This assumes PS/2 scan code set 1 is in effect.")

;; 8042 and PS/2 global variables.
(sys.int::defglobal *8042-present*
  "Whether or not a PS/2 controller is present.")
(sys.int::defglobal *8042-queue-keyboard*
  "The PS/2 controller keyboard port IRQ FIFO.")
(sys.int::defglobal *8042-queue-auxiliary*
  "The PS/2 controller auxiliary port IRQ FIFO.")
(sys.int::defglobal *8042-debug-dump-state*
  "Whether or not to dump global state based on keyboard state.")
(sys.int::defglobal *ps/2-mouse-device-identifier*
  "The device identifier of the PS/2 mouse, if attached.")

(defun ps/2-read-keyboard (&optional (wait-p t))
  "Read a byte from the PS/2 controller's keyboard port.
If WAIT-P is nil, do not wait until a byte is available.  Returns two
values.  The first value is the value read from the keyboard port.
The second value is true if a value was popped, false otherwise.  It
is only possible for the second value to be false when wait-p is
false."
  (irq-fifo-pop *8042-queue-keyboard* wait-p))

(defun ps/2-read-auxiliary (&optional (wait-p t))
  "Read a byte from the PS/2 controller's auxiliary port.
If WAIT-P is nil, do not wait until a byte is available.  Returns two
values.  The first value is the value read from the auxiliary port.
The second value is true if a value was popped, false otherwise.  It
is only possible for the second value to be false when wait-p is
false."
  (irq-fifo-pop *8042-queue-auxiliary* wait-p))

(defun 8042-register-data-read ()
  "Read a byte from the PS/2 controller data register."
  (let ((byte (sys.int::io-port/8 +8042-register-data+)))
    ;; Uncomment when debugging early 8042 configuration.
    ;; (debug-print-line "8042 data port read " byte ".")
    byte))

(defun 8042-register-write (register byte)
  "Write BYTE to REGISTER on PS/2 controller.
REGISTER is one of `+8042-register-data+' or
`+8042-register-command+'."
  (setf (sys.int::io-port/8 register) byte)
  (debug-print-line "8042 register " register " write " byte ".")
  nil)

(defun 8042-status ()
  "Return the PS/2 controller's status, a byte."
  (sys.int::io-port/8 +8042-register-status+))

(defun 8042-data ()
  "Return t if input data is available in the PS/2 controller's data buffer."
  (logtest (8042-status) +8042-status-mask-output-buffer-full+))

(defun 8042-wait-until-input-is-available ()
  "Wait until the PS/2 controller's output buffer is full.
The term \"input\" is relative to Mezzano.  Return t if Mezzano can
read input from the PS/2 controller, or nil if waiting timed out."
  (loop for interval from 1 upto +8042-timeout+ thereis (8042-data)
	do (safe-sleep +8042-delay+)))

(defun 8042-room ()
  "Return t if room is available in the PS/2 controller's data buffer."
  (not (logtest (8042-status) +8042-status-mask-input-buffer-full+)))

(defun 8042-wait-until-output-is-possible ()
  "Wait until the PS/2 controller's input buffer is empty.
The term \"output\" is relative to Mezzano.  Return t if Mezzano can
write output to the PS/2 controller, or nil if waiting timed out."
  (loop for interval from 1 upto +8042-timeout+ thereis (8042-room)
	do (safe-sleep +8042-delay+)))

(defun 8042-data-buffer-flush ()
  "Flush all data out of the PS/2 controller's data buffer.
Return t on success, or nil if no 8042 is present."
  (not
   (loop for count from 1 upto +8042-data-buffer-maximum-size+
	 always (8042-data)
	 do (let ((data (8042-register-data-read)))
	      (safe-sleep +8042-delay+)
	      (debug-print-line "8042 flush " count " " data ".")
	      nil))))

(defun 8042-register-run (register command &optional parameter)
  "Fire and forget COMMAND, a byte, to REGISTER.
PARAMETER is the argument to the command.  Always return nil."
  (unless (8042-wait-until-output-is-possible)
    (debug-print-line "8042 command output wait failed.")
    (return-from 8042-register-run))
  (8042-register-write register command)
  (when parameter
    (unless (8042-wait-until-output-is-possible)
      (debug-print-line "8042 parameter output wait failed.")
      (return-from 8042-register-run))
    (8042-register-write +8042-register-data+ parameter)))

(defun 8042-run (command &optional parameter)
  "Fire and forget COMMAND, a byte, to the PS/2 controller.
PARAMETER is the argument to the command.  Always return nil."
  (8042-register-run +8042-register-command+ command parameter))

(defun 8042-register-data-read-when-available ()
  "Wait for data to be available in the data register, then return it.
Return nil if a timeout is reached."
  (if (8042-wait-until-input-is-available)
      (8042-register-data-read)
      (progn (debug-print-line "8042 result input wait failed.") nil)))

(defun 8042-register-check (register command &optional parameter)
  "Send COMMAND, a byte, to REGISTER on the PS/2 controller and return a result.
PARAMETER is the argument to the command.  Return result, a byte, read
from the controller."
  (8042-register-run register command parameter)
  (8042-register-data-read-when-available))

(defun 8042-check (command &optional parameter)
  "Send COMMAND, a byte, to the PS/2 controller and return a result.
PARAMETER is the argument to the command.  Return result, a byte, read
from the controller."
  (8042-register-check +8042-register-command+ command parameter))

(defun 8042-device-write-keyboard (byte)
  "Write BYTE to device connected to keyboard port of the PS/2 controller.
Return t on success, nil on failure."
  (8042-register-check +8042-register-data+ byte))

(defun 8042-device-write-auxiliary (byte)
  "Write BYTE to device connected to auxiliary port of the PS/2 controller.
Return t on success, nil on failure."
  (8042-register-write +8042-register-command+
		       +8042-command-auxiliary-port-device-write+)
  (8042-register-check +8042-register-data+ byte))

(defun 8042-handle-interrupt-request-keyboard (interrupt-frame
					       interrupt-request-object)
  "Handle an interrupt request from a PS/2 keyboard.
INTERRUPT-FRAME and INTERRUPT-REQUEST-OBJECT are ignored."
  (declare (ignore interrupt-frame interrupt-request-object))
  (let ((byte (8042-register-data-read)))
    (irq-fifo-push byte *8042-queue-keyboard*)
    (case *8042-debug-dump-state*
      ;; Start. Expect left-meta.
      (0 (cond ((eql byte +ps/2-keyboard-scan-code-left-meta+)
		(setf *8042-debug-dump-state* 1))))
      ;; Saw left-meta press.
      (1 (cond ((eql byte +ps/2-keyboard-scan-code-left-meta+)
		#| Stay in this state. |#)
	       ((eql byte +ps/2-keyboard-scan-code-f11+)
		(debug-magic-button)
		(setf *8042-debug-dump-state* 0))
	       (t
		(setf *8042-debug-dump-state* 0))))
      (t
       (setf *8042-debug-dump-state* 0))))
  :completed)

(defun 8042-handle-interrupt-request-auxiliary (interrupt-frame
						interrupt-request-object)
  "Handle an interrupt request from a PS/2 auxiliary device such as a mouse.
INTERRUPT-FRAME and INTERRUPT-REQUEST-OBJECT are ignored."
  (declare (ignore interrupt-frame interrupt-request-object))
  (irq-fifo-push (8042-register-data-read) *8042-queue-auxiliary*)
  :completed)

(defun ps/2-mouse-get-identifier ()
  "Get the PS/2 mouse device identifier.
Return nil if there is a failure, otherwise a numeric identifier."
  (debug-print-line "ps/2 mouse get identifier.")
  (if (eql +8042-response-acknowledge+
	   (8042-device-write-auxiliary
	    +ps/2-mouse-command-get-device-identifier+))
      (let ((identifier (8042-register-data-read-when-available)))
	(debug-print-line "ps/2 mouse get identifier identifier byte 0 "
			  identifier ".")
	(let ((ignore (8042-register-data-read-when-available)))
	  (debug-print-line "ps/2 mouse get identifier identifier byte 1 "
			    ignore "."))
	(if (or (eql identifier +ps/2-mouse-device-identifier-standard+)
		(eql identifier +ps/2-mouse-device-identifier-intellimouse+)
		(eql identifier +ps/2-mouse-device-identifier-explorer+))
	    identifier
	    (progn
	      (debug-print-line "ps/2 mouse get identifier unsupported mouse.")
	      nil)))
      (progn
	(debug-print-line "ps/2 mouse get identifier did not acknowledge.")
	nil)))

(defun ps/2-mouse-initialize ()
  "Probe for and initialize a PS/2 mouse on the PS/2 controller auxiliary port."
  (setf *ps/2-mouse-device-identifier* nil)
  (debug-print-line "ps/2 mouse reset.")
  (if (eql +8042-response-acknowledge+
	   (8042-device-write-auxiliary +8042-command-device-reset+))
      (unless (and (eql +ps/2-mouse-response-test-success+
			(8042-register-data-read-when-available))
		   (eql +ps/2-mouse-device-identifier-standard+
			(8042-register-data-read-when-available)))
	(debug-print-line "ps/2 mouse reset failed.")
	(return-from ps/2-mouse-initialize))
      (progn
	(debug-print-line "ps/2 mouse reset did not acknowledge.")
	(return-from ps/2-mouse-initialize)))

  (unless (ps/2-mouse-get-identifier)
    (return-from ps/2-mouse-initialize))

  (debug-print-line "ps/2 mouse set defaults.")
  (unless (eql +8042-response-acknowledge+
	       (8042-device-write-auxiliary +ps/2-mouse-command-set-defaults+))
    (progn
      (debug-print-line "ps/2 mouse set defaults did not acknowledge.")
      (return-from ps/2-mouse-initialize)))

  (debug-print-line "ps/2 mouse enable IntelliMouse.")
  (unless (and
	   ;; Magic sequence to enable IntelliMouse support: set
	   ;; sample rate 200, then 100, then 80.
	   (eql +8042-response-acknowledge+
		(8042-device-write-auxiliary
		 +ps/2-mouse-command-set-sample-rate+))
	   (eql +8042-response-acknowledge+
		(8042-device-write-auxiliary 200))
	   (eql +8042-response-acknowledge+
		(8042-device-write-auxiliary
		 +ps/2-mouse-command-set-sample-rate+))
	   (eql +8042-response-acknowledge+
		(8042-device-write-auxiliary 100))
	   (eql +8042-response-acknowledge+
		(8042-device-write-auxiliary
		 +ps/2-mouse-command-set-sample-rate+))
	   (eql +8042-response-acknowledge+
		(8042-device-write-auxiliary 80)))
    (debug-print-line "ps/2 mouse enable IntelliMouse failed.")
    (return-from ps/2-mouse-initialize))

  (debug-print-line "ps/2 mouse get identifier.")
  (let ((identifier (ps/2-mouse-get-identifier)))
    (when (and identifier
	       (eql +ps/2-mouse-device-identifier-intellimouse+ identifier))
      (debug-print-line "8042 IntelliMouse mode enabled."))
    (setf *ps/2-mouse-device-identifier* identifier))

  (debug-print-line "ps/2 mouse set stream mode.")
  (unless (eql +8042-response-acknowledge+
	       (8042-device-write-auxiliary
		+ps/2-mouse-command-set-stream-mode+))
    (debug-print-line "ps/2 mouse set stream mode did not acknowledge.")
    (return-from ps/2-mouse-initialize)))

(defun ps/2-keyboard-initialize ()
  "Initialize a PS/2 keyboard on the PS/2 controller keyboard port."
  (debug-print-line "ps/2 keyboard set scan code set 1.")
  (unless (and
	   (eql +8042-response-acknowledge+
		(8042-device-write-keyboard
		 +ps/2-keyboard-command-set-scan-code-set+))
	   (eql +8042-response-acknowledge+
		(8042-device-write-keyboard 1)))
    (debug-print-line "ps/2 keyboard set scan code set 1 failed.")
    (return-from ps/2-keyboard-initialize)))

(defun 8042-initialize ()
  "Initialize the PS/2 controller."
  (setf *8042-debug-dump-state* nil)
  ;; See https://wiki.osdev.org/%228042%22_PS/2_Controller.
  ;; Disable devices.
  (debug-print-line "8042 disable keyboard port.")
  (8042-run +8042-command-keyboard-port-disable+)
  (debug-print-line "8042 disable auxiliary port.")
  (8042-run +8042-command-auxiliary-port-disable+)

  ;; Now that devices are disabled, they can no longer write to the
  ;; data buffer.  Flush any data they had written previously.
  (unless (8042-data-buffer-flush)
    (debug-print-line "8042 flushing failed."))

  (let ((configuration (8042-check +8042-command-configuration-byte-read+))
	(port-good-keyboard nil)
	(port-good-auxiliary nil))
    (debug-print-line "8042 configuration byte " configuration ".")
    (let ((maybe-dual-channel
	    (logtest +8042-configuration-bit-clock-auxiliary+ configuration))
	  ;; Disable scan code set 1 to scan code set 2 translation.  Disable
	  ;; port interrupts.
	  (configuration
	    (logand (lognot +8042-configuration-bit-translation+)
		    (lognot +8042-configuration-bit-interrupt-keyboard+)
		    (lognot +8042-configuration-bit-interrupt-auxiliary+)
		    configuration)))
      (8042-run +8042-command-configuration-byte-write+ configuration)
      (unless (eql (8042-check +8042-command-controller-test+)
		   +8042-response-self-test-success+)
	(debug-print-line "8042 self-test failed.")
	(return-from 8042-initialize))
      (let ((dual-channel
	      ;; If maybe-dual-channel is nil then the 8042 is single
	      ;; channel for sure.
	      (when maybe-dual-channel
		(8042-run +8042-command-auxiliary-port-enable+)
		(not (logtest +8042-configuration-bit-clock-auxiliary+
			      (8042-check
			       +8042-command-configuration-byte-read+))))))
	(debug-print-line "8042 dual channel " dual-channel ".")
	(when dual-channel (8042-run +8042-command-auxiliary-port-disable+))
	(let ((keyboard (8042-check +8042-command-keyboard-port-test+)))
	  (debug-print-line "8042 test keyboard port " keyboard ".")
	  (setf port-good-keyboard (eql 0 keyboard))
	  (when dual-channel
	    (let ((auxiliary (8042-check +8042-command-auxiliary-port-test+)))
	      (debug-print-line "8042 test auxiliary port " auxiliary ".")
	      (setf port-good-auxiliary (eql 0 auxiliary)))))))

    (when port-good-auxiliary
      (when (not (boundp '*8042-queue-auxiliary*))
	(setf *8042-queue-auxiliary*
	      (make-irq-fifo 50 :element-type '(unsigned-byte 8)
				:name "8042 queue auxiliary")))
      (irq-fifo-reset *8042-queue-auxiliary*)
      (debug-print-line "8042 initialize mouse.")
      (ps/2-mouse-initialize)
      (debug-print-line "8042 attach interrupts auxiliary.")
      (irq-attach (platform-irq +8042-interrupt-request-line-auxiliary+)
		  '8042-handle-interrupt-request-auxiliary
		  '8042-auxiliary
		  :exclusive t)
      (debug-print-line "8042 enable port auxiliary.")
      (8042-run +8042-command-configuration-byte-write+
		(logand (lognot +8042-configuration-bit-clock-auxiliary+)
			(logior +8042-configuration-bit-interrupt-auxiliary+
				(8042-check
				 +8042-command-configuration-byte-read+)))))

    (when port-good-keyboard
      (when (not (boundp '*8042-queue-keyboard*))
	(setf *8042-queue-keyboard*
	      (make-irq-fifo 50 :element-type '(unsigned-byte 8)
				:name "8042 queue keyboard")))
      (irq-fifo-reset *8042-queue-keyboard*)
      (debug-print-line "8042 initialize keyboard.")
      (ps/2-keyboard-initialize)
      (debug-print-line "8042 attach interrupts keyboard.")
      (irq-attach (platform-irq +8042-interrupt-request-line-keyboard+)
		  '8042-handle-interrupt-request-keyboard
		  '8042-keyboard
		  :exclusive t)
      (debug-print-line "8042 enable port keyboard.")
      (8042-run +8042-command-configuration-byte-write+
		(logand (lognot +8042-configuration-bit-clock-keybaord+)
			(logior +8042-configuration-bit-interrupt-keyboard+
				(8042-check
				 +8042-command-configuration-byte-read+))))))
  (debug-print-line "8042 initialization done.")
  nil)

(defun 8042-probe ()
  "Check if a PS/2 controller is present and if so, initialize it."
  (setf *8042-present* nil)
  (when (acpi-8042-present)
    (setf *8042-present* t)
    (8042-initialize)))

;;; 8042.lisp ends here
