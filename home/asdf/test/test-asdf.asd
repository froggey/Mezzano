(defpackage :test-asdf-system
  (:use :cl :asdf :uiop))
(in-package :test-asdf-system)

(defvar *times-loaded* 0)
(incf *times-loaded*)
;;(format! t "Times that test-asdf.asd was loaded: ~D~%" *times-loaded*)

(defsystem :test-asdf :class package-inferred-system)

(defsystem :test-asdf/all
  :version "0"
  :depends-on ((:version :test-asdf/2 "2")
               :test-asdf/4))

(defsystem :test-asdf/1
  :components ((:file "file1" :if-feature :common-lisp)
               (:file "does-not-exist" :if-feature (:not :common-lisp))))

(defsystem :test-asdf/2
  :version "2.2"
  :depends-on (:test-asdf/1)
  :components ((:module "foo" :pathname ""
                :components ((:module "bar" :pathname ""
                              :components ((:file "file2")))))))

(defsystem :test-asdf/4
  :components ((:file "file3")
               (:file "file4" :in-order-to ((load-op (load-op "file3"))
                                            (compile-op (load-op "file3"))))))

(defsystem :test-asdf/test9-1
  :version "1.1"
  :components ((:file "file2"))
  :depends-on ((:version :test-asdf/test9-2 "2.0")))

(defsystem :test-asdf/test-module-depend
  :depends-on
  ((:feature :sbcl (:require :sb-posix))
   (:feature :allegro (:require "osi")))
  :components
  ((:file "file1" :if-feature :common-lisp)
   (:file "doesnt-exist" :if-feature (:not :common-lisp))
   (:module "quux"
    :pathname ""
    :depends-on ("file1")
    :components
    ((:file "file2")
     (:module "file3mod"
      :pathname ""
      :components
      ((:file "file3" :if-feature :common-lisp)
       (:file "does-not-exist" :if-feature (:not :common-lisp))))))))


(defsystem :test-asdf/test9-2
  :version "1.0"
  :components ((:file "file1")))

(defsystem :test-asdf/test9-3
  :depends-on ((:version :test-asdf/test9-2 "1.0")))

(defsystem :test-asdf/test-source-directory-1
  :pathname "some/relative/pathname/")

(defsystem :test-asdf/test-source-directory-2
  :pathname "some/relative/pathname/with-file.type")

(defsystem :test-asdf/bundle-1
  :components ((:file "file1") (:file "file3")))

(defsystem :test-asdf/bundle-2
  :depends-on (:test-asdf/bundle-1) :components
  ((:file "file2")
   (:file "invalid-file" :if-feature (:not :common-lisp))))

(defsystem :test-asdf/force
  :depends-on (:test-asdf/force1)
  :components ((:file "file4")))

(defsystem :test-asdf/force1
  ;; Importantly, this depends on a system that is NOT defined in the same file,
  ;; so that we can check that forcing causes one file to be reloaded but not the other.
  :depends-on (:file3-only)
  :components ((:file "file1")))

;; These are used by test-defsystem-depends-on-change.asd, test-defsystem-depends-on-change.script
(defvar *ta/dcc* 0)
(defsystem "test-asdf/dep-can-change"
  :depends-on ("test-asdf-location-change" "test-asdf/dep-forced")
  :components ((:file "show-progress"))
  :perform (load-op (o c) (incf *ta/dcc*)))

(defsystem "test-asdf/dep-forced"
  :components ((:file "file1")))

(defvar *ta/dcd* 0)
(defsystem "test-asdf/dep-can-disappear"
  :depends-on ("test-asdf-location-change")
  :perform (load-op (o c) (incf *ta/dcd*)))

(defvar *ta/dca* 0)
(defsystem "test-asdf/dep-can-appear"
  :depends-on ("test-asdf-location-change")
  :perform (load-op (o c) (incf *ta/dca*)))

(defsystem :test-asdf/upgrade
  :version "0"
  :depends-on (:test-asdf/all)
  :components ((:file "properly-upgraded")))

