;;;
;;; Copyright (c) 2002 Edi Weitz <edi@agharta.de>
;;;
;;; zlib-cmucl.lisp - Code to compress a Lisp string to an array of
;;; bytes, using the zlib compression library (http://www.zlib.org). 
;;;
;;; Usage:
;;;
;;; 1. Set *libgz-path* to the location of the zlib library on your
;;; machine. Compile and load this file
;;;
;;; 2. Call (COMPRESS-STRING "your string"). Two values are returned:
;;; the array of bytes containing the compressed data, and the
;;; actual number of bytes in the compressed output. The length of the 
;;; resulting array is a fixed function of the length of the input
;;; string, while the length of the compressed data is variable and
;;; depends on the randomness of the input (strings with a lot of
;;; repetitions will compress better).
;;;

(in-package :pdf)

#-CMU (error "This code should be loaded in CMUCL.")

(use-package "ALIEN")
(use-package "C-CALL")

;; - cph 17-Aug-2004
#+ignore
(defvar *libgz-path* "/usr/lib/libz.so.1"
  "Set this variable to point to the location of the zlib library on
your system.")

;; - cph 17-Aug-2004
#+ignore
(defvar *libgz-loaded* nil)

;; - cph 17-Aug-2004
#+ignore
(unless *libgz-loaded*
  (load-foreign *libgz-path*):q
  (setq *libgz-loaded* t))

(declaim (inline gz-string))
(def-alien-routine ("compress" gz-string)
    integer
  (dest (* (unsigned 8))) 
  (destlen long :in-out)
  (source c-string)
  (sourcelen long))

(defun compress-string (source)
  "Compress the string SOURCE. Returns two values: the array of bytes
representing the compressed data and the number of compressed bytes."
  (let* ((sourcelen (length source))
	 (destlen (+ 12 (ceiling (* sourcelen 1.05))))
	 (dest (make-array destlen
                           :element-type '(unsigned-byte 8)
			   :initial-element 0)))
    (multiple-value-bind (res new-destlen)
        (gz-string (system:vector-sap dest)
                   destlen source sourcelen)
      (if (zerop res)
          (values (map 'string #'code-char (subseq dest 0 new-destlen))
                  new-destlen)
          (error "zlib error, code ~d" res)))))
