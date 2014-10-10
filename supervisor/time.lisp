(in-package :mezzanine.supervisor)

(defconstant +pit-irq+ 0)

(defvar *heartbeat-lock*)
(defvar *heartbeat-cvar*)

(defun pit-irq-handler (irq)
  (declare (ignore irq))
  (with-mutex (*heartbeat-lock*)
    (condition-notify *heartbeat-cvar* t)))

(defun initialize-time ()
  (when (not (boundp '*heartbeat-lock*))
    (setf *heartbeat-lock* (make-mutex "Heartbeat lock" :spin)
          *heartbeat-cvar* (make-condition-variable "Heartbeat")))
  (i8259-hook-irq +pit-irq+ 'pit-irq-handler)
  (i8259-unmask-irq +pit-irq+))

(defun wait-for-heartbeat ()
  (with-mutex (*heartbeat-lock*)
    (condition-wait *heartbeat-cvar* *heartbeat-lock*)))
