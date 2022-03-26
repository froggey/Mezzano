(in-package :mcclim-fontconfig)

(define-condition fontconfig-error (error)
  ())

(define-condition fontconfig-match-error (fontconfig-error)
  ((status :initarg :status
           :reader fontconfig-match-error/status))
  (:report (lambda (condition out)
             (format out "Match error. Type: ~s" (fontconfig-match-error/status condition)))))
