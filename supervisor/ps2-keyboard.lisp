;;; ps2-keyboard.lisp --- driver for PS/2 keyboard

;;; Commentary:

;; Definitions:
;;
;; PS/2: Personal System/2 standard for keyboard and mouse ports.

;;; Code:

(in-package :mezzano.supervisor.ps/2-keyboard)

;; PS/2 keyboard command.
(defconstant +command-set-scan-code-set+ #xf0
  "Set PS/2 keyboard scan code set.")

;; PS/2 keyboard scan codes.
(defconstant +scan-code-left-meta+ #x38
  "The scan code for left-meta (left-alt) being pressed.
This assumes PS/2 scan code set 1 is in effect.")
(defconstant +scan-code-f11+ #x57
  "The scan code for F11 being pressed.
This assumes PS/2 scan code set 1 is in effect.")

(defun initialize ()
  "Initialize a PS/2 keyboard on the PS/2 controller keyboard port."
  (i8042::debug-print-line "ps/2 keyboard set scan code set 1.")
  (unless (and
           (i8042::device-write-keyboard +command-set-scan-code-set+)
           (i8042::device-write-keyboard 1))
    (i8042::debug-print-line "ps/2 keyboard set scan code set 1 failed.")))

;;; ps2-keyboard.lisp ends here
