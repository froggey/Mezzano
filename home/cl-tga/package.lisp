;;;; package.lisp

(defpackage #:cl-tga
  (:use #:cl)
  (:nicknames #:tga)
  (:export #:read-tga
	   #:image-width
	   #:image-height
	   #:image-bpp
	   #:image-data
	   #:image-channels))

