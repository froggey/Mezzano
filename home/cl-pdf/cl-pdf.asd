;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; cl-pdf copyright 2002-2013 Marc Battyani and contributors see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package :common-lisp-user)

(defpackage #:cl-pdf-system
    (:use #:cl #:asdf))

(in-package #:cl-pdf-system)

;;;Choose the zlib implementation you want to use (only one!)
(eval-when (:load-toplevel :compile-toplevel :execute)
  ;;(pushnew :use-salza2-zlib *features*)
  ;;(pushnew :use-salza-zlib *features*)
  ;;(pushnew :use-uffi-zlib *features*)
  ;;(pushnew :use-abcl-zlib *features*)
  (pushnew :use-no-zlib *features*)
  )

#-(or use-uffi-zlib use-salza-zlib use-salza2-zlib use-abcl-zlib use-no-zlib)
(error "You must choose which zlib implementation you want to use!")

#+(and (not uffi) use-uffi-zlib)
(ignore-errors
  (print "Trying to load UFFI:")
  (operate 'load-op :uffi)
  (pushnew :uffi *features*)
  (print "UFFI loaded."))

#+clisp (setf *warn-on-floating-point-contagion* nil)

(defsystem :cl-pdf
  :name "cl-pdf"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :version "2.0"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :licence "BSD like licence"
  :description "Common Lisp PDF Generation Library"
  :long-description "The cl-pdf package provides a stand-alone Common Lisp library to generate PDF files."
  :perform (load-op :after (op cl-pdf)
                    (pushnew :cl-pdf *features*))
  :components ((:file "defpackage")
               (:file "config" :depends-on ("defpackage"))
               #+use-uffi-zlib (:file "init" :depends-on ("config"))
               (:file "zlib"
                      :depends-on ("config" "defpackage"
                                            #+use-uffi-zlib "init"))
               (:file "font-metrics"  :depends-on ("config"))
               (:file "encodings"  :depends-on ("config"))
               (:file "t1-font" :depends-on ("font-metrics" "encodings"))
               (:file "ttu-font" :depends-on ("font-metrics"))
               (:file "zpb-ttf-load" :depends-on ("ttu-font"))
               (:file "font" :depends-on ("t1-font" "ttu-font"))
               (:file "pdf" :depends-on ("font"))
               (:file "x11-colors" :depends-on ("defpackage"))
               (:file "pdf-base" :depends-on ("pdf" "x11-colors"))
               (:file "png" :depends-on ("pdf-base"))
               (:file "pdf-geom" :depends-on ("pdf-base"))
               (:file "text" :depends-on ("pdf-base"))
               (:file "bar-codes" :depends-on ("pdf-geom"))
               (:file "chart" :depends-on ("text" "pdf-geom"))
               (:file "zzinit" :depends-on ("config")))
  :depends-on (:iterate #+use-salza-zlib :salza #+use-salza2-zlib :salza2 :zpb-ttf :uiop))
