(in-package :user)


;;; note: this file should be loaded via :cl from *lisp-listener* window
;;; Stanley Knutson
;;; set the root directory 

(defparameter *cl-pdf-home* excl:*source-pathname*)

(defun cl-file (x)
  (tpl:do-command "CL" (namestring (merge-pathnames x *cl-pdf-home*))))

(eval-when (load)
  (mapc #'cl-file
	'("defpackage"
	  "cl-zlib-small"
	  "pdf"
	  "pdf-base"
	  "pdf-geom"
	  )))
