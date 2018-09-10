(in-package :cl-user)

;;;; -------------------------------------------------
;;;; Thread management

;; List all threads
(mezzano.supervisor::all-threads)

;; Set thread priority
(mapcar (lambda (thread)
          (if (string= "Compositor"
                       (mezzano.supervisor::thread-name thread))
              (setf (mezzano.supervisor::thread-priority thread)
                    :high)))
        (mezzano.supervisor::all-threads))

;;;; -------------------------------------------------
;;;; Files

;; Copy file
;; (sys.int::copy-file orig new '(unsigned-byte 8))
(sys.int::copy-file "REMOTE:/home/leo/Lain.jpg"
                    "LOCAL:>Lain.jpg"
                    '(unsigned-byte 8))

;;;; -------------------------------------------------
;;;; Desktop

;; Set background image
(let ((image "LOCAL:>Lain.jpg"))
  (mezzano.supervisor:fifo-push
   (make-instance 'mezzano.gui.desktop::set-background-image :image-pathname image)
   sys.int::*desktop*))

;;;; -------------------------------------------------
;;;; SMP

;; Detect and boot secondary cpus.
;; This is an internal function, which is supposed to be called automatically during the boot process.
;; SMP feature is currently experimental and is disabled by default, so you may call it by yourself.
(mezzano.supervisor::boot-secondary-cpus)
