;;; ps2-mouse.lisp --- driver for PS/2 mouse

;;; Commentary:

;; Definitions:
;;
;; PS/2: Personal System/2 standard for keyboard and mouse ports.
;; byte: An 8-bit byte.

;;; Code:

(in-package :mezzano.supervisor.ps/2-mouse)

(defvar *device-identifier*
  "The device identifier of the PS/2 mouse, or nil if none attached.")

;; PS/2 mouse commands and response.
(defconstant +command-get-device-identifier+ #xf2
  "Get the PS/2 mouse device identifer.")
(defconstant +command-set-defaults+ #xf6
  "Restore PS/2 mouse default configuration.")
(defconstant +command-set-sample-rate+ #xf3
  "Set PS/2 mouse sample rate.")
(defconstant +command-set-stream-mode+ #xf4
  "Set PS/2 mouse stream mode.")

(defconstant +response-test-success+ #xaa
  "Acknowledge response from PS/2 controller.")

;; PS/2 mouse device identifiers.
(defconstant +device-identifier-standard+ #x00
  "The device identifier for a standard PS/2 mouse.")
(defconstant +device-identifier-intellimouse+ #x03
  "The device identifier for an IntelliMouse PS/2 mouse.")
(defconstant +device-identifier-explorer+ #x04
  "The device identifier for an IntelliMouse/Explorer PS/2 mouse.")

(defun get-identifier ()
  "Get the PS/2 mouse device identifier.
Return nil if there is a failure, otherwise a numeric identifier."
  (sup:debug-print-line "ps/2 mouse get identifier.")
  (if (i8042:device-write-auxiliary +command-get-device-identifier+)
      (let ((identifier (i8042:register-data-read-when-available)))
        (sup:debug-print-line "ps/2 mouse get identifier identifier byte 0 "
                                 identifier ".")
        (let ((ignore (i8042:register-data-read-when-available)))
          (sup:debug-print-line "ps/2 mouse get identifier identifier byte 1 "
                                ignore "."))
        (if (or (eql identifier +device-identifier-standard+)
                (eql identifier +device-identifier-intellimouse+)
                (eql identifier +device-identifier-explorer+))
            identifier
            (progn
              (sup:debug-print-line
               "ps/2 mouse get identifier unsupported mouse.")
              nil)))
      (progn
        (sup:debug-print-line "ps/2 mouse get identifier did not acknowledge.")
        nil)))

(defun intellimouse-attached-p ()
  "Return t if an IntelliMouse-capable PS/2 mouse is attached, nil otherwise."
  (eql *device-identifier* +device-identifier-intellimouse+))

(defun initialize ()
  "Probe for and initialize a PS/2 mouse on the PS/2 controller auxiliary port."
  (setf *device-identifier* nil)
  (sup:debug-print-line "ps/2 mouse reset.")
  (if (i8042:device-write-auxiliary i8042:+command-device-reset+)
      (unless (and (eql +response-test-success+
                        (i8042:register-data-read-when-available))
                   (eql +device-identifier-standard+
                        (i8042:register-data-read-when-available)))
        (sup:debug-print-line "ps/2 mouse reset failed.")
        (return-from initialize))
      (progn
        (sup:debug-print-line "ps/2 mouse reset did not acknowledge.")
        (return-from initialize)))

  (unless (get-identifier)
    (return-from initialize))

  (sup:debug-print-line "ps/2 mouse set defaults.")
  (unless (i8042:device-write-auxiliary +command-set-defaults+)
    (progn
      (sup:debug-print-line "ps/2 mouse set defaults did not acknowledge.")
      (return-from initialize)))

  (sup:debug-print-line "ps/2 mouse enable IntelliMouse.")
  (unless (and
           ;; Magic sequence to enable IntelliMouse support: set
           ;; sample rate 200, then 100, then 80.
           (i8042:device-write-auxiliary +command-set-sample-rate+)
           (i8042:device-write-auxiliary 200)
           (i8042:device-write-auxiliary +command-set-sample-rate+)
           (i8042:device-write-auxiliary 100)
           (i8042:device-write-auxiliary +command-set-sample-rate+)
           (i8042:device-write-auxiliary 80))
    (sup:debug-print-line "ps/2 mouse enable IntelliMouse failed.")
    (return-from initialize))

  (sup:debug-print-line "ps/2 mouse get identifier.")
  (let ((identifier (get-identifier)))
    (when (and identifier
               (eql +device-identifier-intellimouse+ identifier))
      (sup:debug-print-line "ps/2 mouse IntelliMouse mode enabled."))
    (setf *device-identifier* identifier))

  (sup:debug-print-line "ps/2 mouse set stream mode.")
  (unless (i8042:device-write-auxiliary +command-set-stream-mode+)
    (sup:debug-print-line "ps/2 mouse set stream mode did not acknowledge."))
  nil)

;;; ps2-mouse.lisp ends here
