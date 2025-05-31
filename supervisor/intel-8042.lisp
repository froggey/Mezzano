;;; intel-8042.lisp --- driver for the Intel 8042 PS/2 controller

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

(in-package :mezzano.supervisor.intel-8042)

;; PS/2 controller registers.
(defconstant +register-data+ #x60
  "The I/O address of the PS/2 controller's read-write data register.")
(defconstant +register-status+ #x64
  "The I/O address of the PS/2 controller's read-only status register.")
(defconstant +register-command+ #x64
  "The I/O address of the PS/2 controller's write-only command register.")

;; Interrupt request lines.
(defconstant +interrupt-request-line-keyboard+ 1
  "The PS/2 controller's keyboard interrupt request line.
This interrupt request number is the x86-specific value.")
(defconstant +interrupt-request-line-auxiliary+ 12
  "The PS/2 controller's auxiliary (typically mouse) interrupt request line.
This interrupt request number is the x86-specific value.")

;; Status register masks.
(defconstant +status-mask-output-buffer-full+ #x1
  "Mask for bit 0 of the status register.
Bit 0 is clear, 0, if the output buffer is empty, or set, 1, if the
output buffer is full.  The term \"output\" is relative to the PS/2
device; the PS/2 device sends output to the 8042.  Therefore Mezzano
reads from the output buffer.")
(defconstant +status-mask-input-buffer-full+ #x2
  "Mask for bit 1 of the status register.
Bit 1 is clear, 0, if the input buffer is empty, or set, 1, if the
output buffer is full.  The term \"input\" is relative to the PS/2
device; the PS/2 device receives input from the 8042.  Therefore
Mezzano writes to the input buffer.")

;; PS/2 controller communication timeout.
(defconstant +delay+ 0.01
  "Per-interval delay.
The number of seconds to sleep, using `safe-sleep', while waiting for
a PS/2 controller operation to complete.  This is set to the minimum
delay possible given the limited granularity of `safe-sleep'.")
(defconstant +timeout+ 50
  "Timeout interval count.
The number of intervals, each `+delay+' seconds long, to wait for
PS/2 controller operations to complete.")

;; Flushing.
(defconstant +data-buffer-maximum-size+ 16
  "The maximum size, in bytes, of any PS/2 controller's data buffer.")

;; PS/2 controller commands.
(defconstant +command-configuration-byte-read+ #x20
  "Read the PS/2 controller's configuration byte.")
(defconstant +command-configuration-byte-write+ #x60
  "Write the PS/2 controller's configuration byte.")
(defconstant +command-auxiliary-port-disable+ #xa7
  "Disable the auxiliary port of the PS/2 controller.")
(defconstant +command-auxiliary-port-enable+ #xa8
  "Enable the auxiliary port of the PS/2 controller.")
(defconstant +command-auxiliary-port-test+ #xa9
  "Test the auxiliary port of the PS/2 controller.")
(defconstant +command-auxiliary-port-device-write+ #xd4
  "Write to the device attached to the auxiliary port of the PS/2 controller.")
(defconstant +command-keyboard-port-disable+ #xad
  "Disable the keyboard port on the PS/2 controller.")
(defconstant +command-keyboard-port-test+ #xab
  "Test the keyboard port on the PS/2 controller.")
(defconstant +command-controller-test+ #xaa
  "Test the PS/2 controller itself.")
(defconstant +command-device-reset+ #xff
  "Reset a PS/2 device.
Also referred to as BAT, basic assurance test.")
(defconstant +command-reboot+ #xfe
  "Send a reboot request to the PS/2 controller.")

;; PS/2 controller configuration byte bits.
(defconstant +configuration-bit-interrupt-keyboard+ #x1
  "The keyboard port interrupt bit mask.
Read-write bit 0 in the PS/2 controller configuration byte indicates
if interrupts are enabled for the keyboard port.  Bit value 0 means
disabled, 1 means enabled.")
(defconstant +configuration-bit-interrupt-auxiliary+ #x2
  "The auxiliary port interrupt bit mask.
Read-write bit 1 in the PS/2 controller configuration byte indicates
if interrupts are enabled for the auxiliary port.  Bit value 0 means
disabled, 1 means enabled.")
(defconstant +configuration-bit-clock-keybaord+ #x10
  "The keyboard port interrupt bit mask.
Read-write bit 4 in the PS/2 controller configuration byte indicates
if clocks are enabled for the keyboard port.  Bit value 0 means
enabled, 1 means disabled.")
(defconstant +configuration-bit-clock-auxiliary+ #x20
  "The auxiliary port interrupt bit mask.
Read-write bit 5 in the PS/2 controller configuration byte indicates
if clocks are enabled for the auxiliary port.  Bit value 0 means
enabled, 1 means disabled.")
(defconstant +configuration-bit-translation+ #x40
  "The translation mode bit mask.
Read-write bit 6 in the PS/2 controller configuration byte indicates
if scan-code-set-2 to scan-code-set-1 translation is enabled in the
PS/2 controller.  Bit value 1 means enabled, 0 means disabled.")

(defconstant +response-acknowledge+ #xfa
  "Acknowledge response from PS/2 controller.")
(defconstant +response-self-test-success+ #x55
  "Self-test success response from PS/2 controller.")

;; PS/2 keyboard scan codes.  These constants are in the intel-8042
;; package because they are needed to intercept the debug dump key
;; combination.
(defconstant +scan-code-left-meta+ #x38
  "The scan code for left-meta (left-alt) being pressed.
This assumes PS/2 scan code set 1 is in effect.")
(defconstant +scan-code-f11+ #x57
  "The scan code for F11 being pressed.
This assumes PS/2 scan code set 1 is in effect.")

;; Intel 8042 and PS/2 global variables.
(defvar *intel-8042-present* nil
  "Whether or not a PS/2 controller is present.")
;; These must be sys.int::defglobal, not defvar'd normal special
;; variables.  See doc/internals/supervisor-restrictions.md.
(sys.int::defglobal *intel-8042-queue-keyboard* nil
  "The PS/2 controller keyboard port IRQ FIFO.")
(sys.int::defglobal *intel-8042-queue-auxiliary* nil
  "The PS/2 controller auxiliary port IRQ FIFO.")
(sys.int::defglobal *intel-8042-debug-dump-state* nil
  "Whether or not to dump global state based on keyboard state.")

(defun read-keyboard (&optional (wait-p t))
  "Read a byte from the PS/2 controller's keyboard port.
If WAIT-P is nil, do not wait until a byte is available.  Returns two
values.  The first value is the value read from the keyboard port.
The second value is true if a value was popped, false otherwise.  It
is only possible for the second value to be false when wait-p is
false."
  (sup:irq-fifo-pop *intel-8042-queue-keyboard* wait-p))

(defun read-auxiliary (&optional (wait-p t))
  "Read a byte from the PS/2 controller's auxiliary port.
If WAIT-P is nil, do not wait until a byte is available.  Returns two
values.  The first value is the value read from the auxiliary port.
The second value is true if a value was popped, false otherwise.  It
is only possible for the second value to be false when wait-p is
false."
  (sup:irq-fifo-pop *intel-8042-queue-auxiliary* wait-p))

(defun register-data-read ()
  "Read a byte from the PS/2 controller data register."
  (let ((byte (sys.int::io-port/8 +register-data+)))
    ;; Uncomment when debugging early Intel 8042 configuration.
    ;; (sup:debug-print-line "8042 data port read " byte ".")
    byte))

(defun register-write (register byte)
  "Write BYTE to REGISTER on PS/2 controller.
REGISTER is one of `+register-data+' or
`+register-command+'."
  (setf (sys.int::io-port/8 register) byte)
  (sup:debug-print-line "8042 register " register " write " byte ".")
  nil)

(defun status ()
  "Return the PS/2 controller's status, a byte."
  (sys.int::io-port/8 +register-status+))

(defun data ()
  "Return t if input data is available in the PS/2 controller's data buffer."
  (logtest (status) +status-mask-output-buffer-full+))

(defun wait-until-input-is-available ()
  "Wait until the PS/2 controller's output buffer is full.
The term \"input\" is relative to Mezzano.  Return t if Mezzano can
read input from the PS/2 controller, or nil if waiting timed out."
  (loop for interval from 1 upto +timeout+ thereis (data)
        do (sup:safe-sleep +delay+)))

(defun room-in-buffer ()
  "Return t if room is available in the PS/2 controller's data buffer."
  (not (logtest (status) +status-mask-input-buffer-full+)))

(defun wait-until-output-is-possible ()
  "Wait until the PS/2 controller's input buffer is empty.
The term \"output\" is relative to Mezzano.  Return t if Mezzano can
write output to the PS/2 controller, or nil if waiting timed out."
  (loop for interval from 1 upto +timeout+ thereis (room-in-buffer)
        do (sup:safe-sleep +delay+)))

(defun data-buffer-flush ()
  "Flush all data out of the PS/2 controller's data buffer.
Return t on success, or nil if no 8042 is present."
  (not
   (loop for count from 1 upto +data-buffer-maximum-size+
         always (data)
         do (let ((data (register-data-read)))
              (sup:safe-sleep +delay+)
              (sup:debug-print-line "8042 flush " count " " data ".")
              nil))))

(defun register-run (register command &optional parameter)
  "Fire and forget COMMAND, a byte, to REGISTER.
PARAMETER is the argument to the command.  Always return nil."
  (unless (wait-until-output-is-possible)
    (sup:debug-print-line "8042 command output wait failed.")
    (return-from register-run))
  (register-write register command)
  (when parameter
    (unless (wait-until-output-is-possible)
      (sup:debug-print-line "8042 parameter output wait failed.")
      (return-from register-run))
    (register-write +register-data+ parameter)))

(defun run (command &optional parameter)
  "Fire and forget COMMAND, a byte, to the PS/2 controller.
PARAMETER is the argument to the command.  Always return nil."
  (register-run +register-command+ command parameter))

(defun register-data-read-when-available ()
  "Wait for data to be available in the data register, then return it.
Return nil if a timeout is reached."
  (if (wait-until-input-is-available)
      (register-data-read)
      (progn (sup:debug-print-line "8042 result input wait failed.") nil)))

(defun register-check (register command &optional parameter)
  "Send COMMAND, a byte, to REGISTER on the PS/2 controller and return a result.
PARAMETER is the argument to the command.  Return result, a byte, read
from the controller."
  (register-run register command parameter)
  (register-data-read-when-available))

(defun check (command &optional parameter)
  "Send COMMAND, a byte, to the PS/2 controller and return a result.
PARAMETER is the argument to the command.  Return result, a byte, read
from the controller."
  (register-check +register-command+ command parameter))

(defun device-write-keyboard (byte)
  "Write BYTE to device connected to keyboard port of the PS/2 controller.
Return t on success, nil on failure."
  (eql +response-acknowledge+ (register-check +register-data+ byte)))

(defun device-write-auxiliary (byte)
  "Write BYTE to device connected to auxiliary port of the PS/2 controller.
Return t on success, nil on failure."
  (register-write +register-command+ +command-auxiliary-port-device-write+)
  (eql +response-acknowledge+ (register-check +register-data+ byte)))

(defun handle-interrupt-request-keyboard (interrupt-frame
                                          interrupt-request-object)
  "Handle an interrupt request from a PS/2 keyboard.
INTERRUPT-FRAME and INTERRUPT-REQUEST-OBJECT are ignored."
  (declare (ignore interrupt-frame interrupt-request-object))
  (let ((byte (register-data-read)))
    (sup:irq-fifo-push byte *intel-8042-queue-keyboard*)
    (case *intel-8042-debug-dump-state*
      ;; Start. Expect left-meta.
      (0 (cond ((eql byte +scan-code-left-meta+)
                (setf *intel-8042-debug-dump-state* 1))))
      ;; Saw left-meta press.
      (1 (cond ((eql byte +scan-code-left-meta+)
                #| Stay in this state. |#)
               ((eql byte +scan-code-f11+)
                (sup::debug-magic-button)
                (setf *intel-8042-debug-dump-state* 0))
               (t
                (setf *intel-8042-debug-dump-state* 0))))
      (t
       (setf *intel-8042-debug-dump-state* 0))))
  :completed)

(defun handle-interrupt-request-auxiliary (interrupt-frame
                                           interrupt-request-object)
  "Handle an interrupt request from a PS/2 auxiliary device such as a mouse.
INTERRUPT-FRAME and INTERRUPT-REQUEST-OBJECT are ignored."
  (declare (ignore interrupt-frame interrupt-request-object))
  (sup:irq-fifo-push (register-data-read) *intel-8042-queue-auxiliary*)
  :completed)

(defun initialize ()
  "Initialize the PS/2 controller."
  (setf *intel-8042-debug-dump-state* nil)
  ;; See https://wiki.osdev.org/%228042%22_PS/2_Controller.
  ;; Disable devices.
  (sup:debug-print-line "8042 disable keyboard port.")
  (run +command-keyboard-port-disable+)
  (sup:debug-print-line "8042 disable auxiliary port.")
  (run +command-auxiliary-port-disable+)

  ;; Now that devices are disabled, they can no longer write to the
  ;; data buffer.  Flush any data they had written previously.
  (unless (data-buffer-flush)
    (sup:debug-print-line "8042 flushing failed."))

  (let ((configuration (check +command-configuration-byte-read+))
        (port-good-keyboard nil)
        (port-good-auxiliary nil))
    (sup:debug-print-line "8042 configuration byte " configuration ".")
    (let ((maybe-dual-channel
            (logtest +configuration-bit-clock-auxiliary+ configuration))
          ;; Disable scan code set 1 to scan code set 2 translation.  Disable
          ;; port interrupts.
          (configuration
            (logand (lognot +configuration-bit-translation+)
                    (lognot +configuration-bit-interrupt-keyboard+)
                    (lognot +configuration-bit-interrupt-auxiliary+)
                    configuration)))
      (run +command-configuration-byte-write+ configuration)
      (unless (eql (check +command-controller-test+)
                   +response-self-test-success+)
        (sup:debug-print-line "8042 self-test failed.")
        (return-from initialize))
      (let ((dual-channel
              ;; If maybe-dual-channel is nil then the Intel 8042 is
              ;; single channel for sure.
              (when maybe-dual-channel
                (run +command-auxiliary-port-enable+)
                (not (logtest +configuration-bit-clock-auxiliary+
                              (check
                               +command-configuration-byte-read+))))))
        (sup:debug-print-line "8042 dual channel " dual-channel ".")
        (when dual-channel (run +command-auxiliary-port-disable+))
        (let ((keyboard (check +command-keyboard-port-test+)))
          (sup:debug-print-line "8042 test keyboard port " keyboard ".")
          (setf port-good-keyboard (eql 0 keyboard))
          (when dual-channel
            (let ((auxiliary (check +command-auxiliary-port-test+)))
              (sup:debug-print-line "8042 test auxiliary port " auxiliary ".")
              (setf port-good-auxiliary (eql 0 auxiliary)))))))

    (when port-good-auxiliary
      (when (not (boundp '*intel-8042-queue-auxiliary*))
        (setf *intel-8042-queue-auxiliary*
              (sup:make-irq-fifo 50 :element-type '(unsigned-byte 8)
                                    :name "8042 queue auxiliary")))
      (sup:irq-fifo-reset *intel-8042-queue-auxiliary*)
      (sup:debug-print-line "8042 initialize mouse.")
      (mezzano.supervisor.ps/2-mouse:initialize)
      (sup:debug-print-line "8042 attach interrupts auxiliary.")
      (sup:irq-attach (sup:platform-irq +interrupt-request-line-auxiliary+)
                      'handle-interrupt-request-auxiliary
                      'intel-8042-auxiliary
                      :exclusive t)
      (sup:debug-print-line "8042 enable port auxiliary.")
      (run +command-configuration-byte-write+
           (logand (lognot +configuration-bit-clock-auxiliary+)
                   (logior +configuration-bit-interrupt-auxiliary+
                           (check
                            +command-configuration-byte-read+)))))

    (when port-good-keyboard
      (when (not (boundp '*intel-8042-queue-keyboard*))
        (setf *intel-8042-queue-keyboard*
              (sup:make-irq-fifo 50 :element-type '(unsigned-byte 8)
                                    :name "8042 queue keyboard")))
      (sup:irq-fifo-reset *intel-8042-queue-keyboard*)
      (sup:debug-print-line "8042 initialize keyboard.")
      (mezzano.supervisor.ps/2-keyboard:initialize)
      (sup:debug-print-line "8042 attach interrupts keyboard.")
      (sup:irq-attach (sup:platform-irq +interrupt-request-line-keyboard+)
                      'handle-interrupt-request-keyboard
                      'intel-8042-keyboard
                      :exclusive t)
      (sup:debug-print-line "8042 enable port keyboard.")
      (run +command-configuration-byte-write+
           (logand (lognot +configuration-bit-clock-keybaord+)
                   (logior +configuration-bit-interrupt-keyboard+
                           (check
                            +command-configuration-byte-read+))))))

  (sup:debug-print-line "8042 initialization done.")
  nil)

(defun probe ()
  "Check if a PS/2 controller is present and if so, initialize it."
  (setf *intel-8042-present* nil)
  (when (sup:acpi-8042-present)
    (setf *intel-8042-present* t)
    (initialize)))

;;; intel-8042.lisp ends here
