;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license.              ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2001-2019 Daniel Barlow and contributors           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :asdf)

;; We can't rely on it being defined in uiop.asd, since that file isn't loaded.
(defun call-without-redefinition-warnings (thunk)
  (handler-bind (((or
                   #+allegro simple-warning
                   #+clozure ccl:compiler-warning
                   #+cmucl kernel:simple-style-warning
                   #-(or allegro clozure cmucl) warning)
                   #'muffle-warning))
    (funcall thunk)))

;; Note that it's polite to sort the defsystem forms in dependency order,
;; and compulsory to sort them in defsystem-depends-on order.
#+asdf3
(defsystem "asdf/prelude"
  :version (:read-file-form "version.lisp-expr")
  :around-compile call-without-redefinition-warnings ;; we need be the same as uiop
  :encoding :utf-8
  :components ((:file "header")))

#+asdf3
(defsystem "asdf/driver"
  ;; Since ASDF 3.3, asdf.asd can't afford to depend on reading uiop.asd,
  ;; which would cause circularity, since everything depends on reading asdf.asd.
  ;; Therefore, we can't "just" :depends-on ("uiop") like we used to do, and instead
  ;; we transclude the list of uiop component into this secondary system.
  :pathname "uiop"
  :around-compile call-without-redefinition-warnings ;; we need be the same as uiop
  :components #.(getf (read-file-form (subpathname *load-pathname* "uiop/uiop.asd") :at 2) :components))

#+asdf3
(defsystem "asdf/defsystem"
  :licence "MIT"
  :description "The defsystem part of ASDF"
  :long-name "Another System Definition Facility"
  :description "The portable defsystem for Common Lisp"
  :long-description "ASDF/DEFSYSTEM is the de facto standard DEFSYSTEM facility for Common Lisp,
   a successor to Dan Barlow's ASDF and Francois-Rene Rideau's ASDF2.
   For bootstrap purposes, it comes bundled with UIOP in a single file, asdf.lisp."
  :homepage "http://common-lisp.net/projects/asdf/"
  :bug-tracker "https://launchpad.net/asdf/"
  :mailto "asdf-devel@common-lisp.net"
  :source-control (:git "git://common-lisp.net/projects/asdf/asdf.git")
  :version (:read-file-form "version.lisp-expr")
  :build-operation monolithic-concatenate-source-op
  :build-pathname "build/asdf" ;; our target
  :around-compile call-without-redefinition-warnings ;; we need be the same as uiop
  :depends-on ("asdf/prelude" "asdf/driver")
  :encoding :utf-8
  :components
  ((:file "upgrade")
   (:file "session" :depends-on ("upgrade"))
   (:file "component" :depends-on ("session"))
   (:file "operation" :depends-on ("session"))
   (:file "system" :depends-on ("component"))
   (:file "system-registry" :depends-on ("system"))
   (:file "action" :depends-on ("session" "system" "operation"))
   (:file "lisp-action" :depends-on ("action"))
   (:file "find-component" :depends-on ("component"))
   (:file "forcing" :depends-on ("operation" "system-registry"))
   (:file "plan" :depends-on ("lisp-action" "find-component" "forcing"))
   (:file "operate" :depends-on ("plan"))
   (:file "find-system" :depends-on ("system-registry" "operate"))
   (:file "parse-defsystem" :depends-on ("system-registry" "lisp-action" "operate"
                                                           "find-system"))
   (:file "bundle" :depends-on ("lisp-action" "parse-defsystem"))
   (:file "concatenate-source" :depends-on ("bundle"))
   (:file "package-inferred-system" :depends-on ("parse-defsystem"))
   (:file "output-translations" :depends-on ("operate"))
   (:file "source-registry" :depends-on ("find-system"))
   (:file "backward-internals" :depends-on ("find-system" "parse-defsystem"))
   (:file "backward-interface" :depends-on ("output-translations"))
   (:file "interface" :depends-on
          ("parse-defsystem" "concatenate-source"
           "output-translations" "source-registry" "package-inferred-system"
           "backward-interface" "backward-internals"))
   (:file "user" :depends-on ("interface"))
   (:file "footer" :depends-on ("user"))))

(defsystem "asdf"
  :author ("Daniel Barlow")
  :maintainer ("Robert Goldman")
  :licence "MIT"
  :description "Another System Definition Facility"
  :long-description "ASDF builds Common Lisp software organized into defined systems."
  :version "3.3.3.3" ;; to be automatically updated by make bump-version
  :depends-on ()
  :components ((:module "build" :components ((:file "asdf"))))
  . #-asdf3 () #+asdf3
  (:encoding :utf-8
   :class #+asdf3.1 package-inferred-system #-asdf3.1 system
   ;; For most purposes, asdf itself specially counts as a builtin system.
   ;; If you want to link it or do something forbidden to builtin systems,
   ;; specify separate dependencies on uiop (aka asdf/driver) and asdf/defsystem.
   :builtin-system-p t
   :in-order-to ((prepare-op (monolithic-concatenate-source-op "asdf/defsystem")))))
