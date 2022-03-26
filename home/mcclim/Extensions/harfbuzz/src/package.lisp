(defpackage :mcclim-harfbuzz
  (:use :cl)
  (:documentation "CFFI interface to Harfbuzz")
  (:export #:with-buffer
           #:hb-buffer-create
           #:hb-buffer-destroy
           #:hb-buffer-set-direction
           #:buffer-add-string
           #:hb-ft-font-create
           #:hb-shape
           #:hb-buffer-get-glyph-positions
           #:hb-buffer-get-glyph-infos
           #:hb-buffer-set-script
           #:hb-buffer-guess-segment-properties
           #:hb-buffer-set-cluster-level
           #:hb-ft-font-set-load-flags
           #:hb-ft-font-get-load-flags))
