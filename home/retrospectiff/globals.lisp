
(in-package :retrospectiff.globals)

(defparameter *byte-order*
  #+little-endian :little-endian
  #+big-endian :big-endian
  #-(or little-endian big-endian) :little-endian)

(defvar *tiff-file-offset*)

