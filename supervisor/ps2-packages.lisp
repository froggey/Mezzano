;;; ps2-packages.lisp --- packages for PS/2 controller and devices

;;; Commentary:

;; There are some circular dependencies between the
;; mezzano.supervisor.intel-8042 package and the
;; mezzano.supervisor.ps/2-keyboard and mezzano.supervisor.ps/2-mouse
;; packages.  To resolve the circularity, define the packages together
;; in this file.  The cold image generator can then load these
;; definitions prior to loading the package implementations.

;;; Code:

(defpackage :mezzano.supervisor.intel-8042
  (:documentation "A driver for the Intel 8042 PS/2 controller.")
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:sys.int :mezzano.internals))
  (:export #:probe
           #:wait-until-output-is-possible
           #:register-write
           #:+register-command+
           #:+command-reboot+
           #:read-keyboard
           #:device-write-keyboard
           #:read-auxiliary
           #:+command-device-reset+
           #:device-write-auxiliary
           #:register-data-read-when-available))

(defpackage :mezzano.supervisor.ps/2-keyboard
  (:documentation "A driver for PS/2 keyboards.")
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:i8042 :mezzano.supervisor.intel-8042))
  (:export #:initialize))

(defpackage :mezzano.supervisor.ps/2-mouse
  (:documentation "A driver for PS/2 mice.")
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:i8042 :mezzano.supervisor.intel-8042))
  (:export #:initialize
           #:intellimouse-attached-p))

;;; ps2-packages.lisp ends here
