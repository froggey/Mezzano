(in-package :pngload)

(define-condition png-warning (warning) ())

(define-condition png-error (error) ())

(define-condition invalid-png-stream (png-error) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Stream does not contain a valid PNG datastream: ~A." (get-path)))))

(define-condition unknown-chunk-detected (png-warning) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Detected an unknown chunk type in PNG datastream: ~A." (get-path)))))
