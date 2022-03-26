;;; -*- mode: lisp -*-
(in-package :asdf)

#-asdf3
(unless (or #+asdf2 (version-satisfies (asdf:asdf-version) "2.11.4"))
  (error "UIOP requires ASDF 2.011.4 or later."))

(defun call-without-redefinition-warnings (thunk)
  (handler-bind (((or
                   #+allegro simple-warning
                   #+clozure ccl:compiler-warning
                   #+cmucl kernel:simple-style-warning
                   #-(or allegro clozure cmucl) warning)
                   #'muffle-warning))
    (funcall thunk)))

(defsystem "uiop"
  #+asdf3 :long-name #+asdf3 "Utilities for Implementation- and OS- Portability"
  :description "Portability library for Common Lisp programs"
  :long-description "UIOP provides runtime support for Common Lisp programs:
Basic general-purpose utilities that are in such a need that you can't portably construct a
complete program without using some of them. UIOP replaces ASDF/DRIVER and ASDF-UTILS, and offers a
superset of the functionality provided by CL-FAD, EXTERNAL-PROGRAM, TRIVIAL-SHELL, TRIVIAL-BACKTRACE
and a lot of the functionality formerly provided by CL-LAUNCH, XCVB-DRIVER, TRIVIAL-FEATURES,
plus a tiny subset of functionality from ALEXANDRIA and FARE-UTILS.
It is transcluded into asdf.lisp together with ASDF/DEFSYSTEM, so if you did (require \"asdf\")
you already have a matching UIOP loaded."
  :author "Francois-Rene Rideau"
  :licence "MIT"
  :components
  ((:static-file "contrib/debug.lisp")
   (:file "package")
   (:file "common-lisp" :depends-on ("package"))
   (:file "utility" :depends-on ("common-lisp"))
   (:file "version" :depends-on ("utility"))
   (:file "os" :depends-on ("utility"))
   (:file "pathname" :depends-on ("utility" "os"))
   (:file "filesystem" :depends-on ("os" "pathname"))
   (:file "stream" :depends-on ("filesystem"))
   (:file "image" :depends-on ("stream"))
   (:file "lisp-build" :depends-on ("image"))
   (:file "launch-program" :depends-on ("stream"))
   (:file "run-program" :depends-on ("launch-program" "version"))
   (:file "configuration" :depends-on ("image"))
   (:file "backward-driver" :depends-on ("lisp-build" "run-program" "configuration" "version"))
   (:file "driver" :depends-on ("backward-driver")))
  . #-asdf3 () #+asdf3
  (:encoding :utf-8
   :around-compile call-without-redefinition-warnings
   . #-asdf3.1 () #+asdf3.1
   (:class package-system
    :version (:read-file-form "version.lisp" :at (2 2 2)))))
