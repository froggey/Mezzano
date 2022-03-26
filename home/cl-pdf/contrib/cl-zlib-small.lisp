;;;
;;; Copyright (c) 2002 Alberto Riva <Alberto.Riva@TCH.Harvard.edu>
;;;
;;; cl-zlib-small.cl - Code to compress a Lisp string to an array of
;;; bytes, using the zlib compression library (http://www.zlib.org). 
;;; This is an extract form the full cl-zlib package, available at:
;;;   http://chip.org/~alb/lisp.html
;;;
;;; Usage:
;;;
;;; 1. Set *libgz-path* to the location of the zlib library on your
;;; machine. Compile and load this file
;;;
;;; 2. Call (COMPRESS-STRING "your string"). The function returns an
;;; array of bytes containing the compressed data.
;;;





(in-package :pdf) ; changed to :pdf (MB)

#-allegro (error "This code should be loaded in Allegro Common Lisp.")

(defvar *libgz-path* "/usr/lib/libz.so" 
  "Set this variable to point to the location of the zlib library 
(libz.so or zlib.dll) on your system.")

(defvar *libgz-loaded* nil)

(unless *libgz-loaded*
  (load *libgz-path*)
  (setq *libgz-loaded* t))

(ff:def-foreign-call (gz-string "compress")
    ((dest (* :int) (simple-array (unsigned-byte 8) (*)))
     (destlen (* :long) (simple-array (unsigned-byte 32) (*)))
     (source (* :char) integer)
     (sourcelen :long))
  :returning :int)

(defun compress-string (source)
  "Compress the string SOURCE. Returns an array of bytes representing
the compressed data."
  (let* ((sourceptr #+mswindows (ff::string-to-native source :external-format :1252-base)
                    #+unix (ff::string-to-native source)
                    )			; avoid CRLF translation under MS Windows
         (sourcelen (length source))
         (destsize (+ 12 (ceiling (* sourcelen 1.05))))
         (dest (make-array destsize 
                           :element-type '(unsigned-byte 8)
                           :initial-element 0))
         (destlen (make-array 1 
                              :element-type '(unsigned-byte 32)
                              :initial-element destsize)))
    (let ((res (gz-string dest destlen sourceptr sourcelen)))
      (ff::aclfree sourceptr)
      (if (zerop res)
          (subseq dest 0 (aref destlen 0))
        (error "zlib error, code ~d" res)))))
