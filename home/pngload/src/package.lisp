(in-package :cl-user)

(defpackage #:pngload
  (:use #:cl)
  (:export #:load-file
           #:load-stream
           #:width
           #:height
           #:bit-depth
           #:color-type
           #:palette-count
           #:palette
           #:gamma
           #:transparency
           #:rendering-intent
           #:compression-method
           #:interlace-method
           #:filter-method
           #:pixel-size
           #:last-modified
           #:text
           #:data
           #:with-png-in-static-vector))
