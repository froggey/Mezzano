;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or armedbear
        (and allegro multiprocessing)
        (and clisp mt)
        (and openmcl openmcl-native-threads)
        (and cmu mp)
        corman
        (and ecl threads)
        mezzano
        mkcl
        lispworks
        (and digitool ccl-5.1)
        (and sbcl sb-thread)
        scl)
  (pushnew :thread-support *features*))

(defsystem :bordeaux-threads
  :author "Greg Pfeil <greg@technomadic.org>"
  :licence "MIT"
  :description "Bordeaux Threads makes writing portable multi-threaded apps simple."
  :version (:read-file-form "version.sexp")
  :depends-on (:alexandria
               #+(and allegro (version>= 9))       (:require "smputil")
               #+(and allegro (not (version>= 9))) (:require "process")
               #+corman                            (:require "threads"))
  :components ((:static-file "version.sexp")
               (:module "src"
                :serial t
                :components
                ((:file "pkgdcl")
                 (:file "bordeaux-threads")
                 (:file #+(and thread-support armedbear) "impl-abcl"
                        #+(and thread-support allegro)   "impl-allegro"
                        #+(and thread-support clisp)     "impl-clisp"
                        #+(and thread-support openmcl)   "impl-clozure"
                        #+(and thread-support cmu)       "impl-cmucl"
                        #+(and thread-support corman)    "impl-corman"
                        #+(and thread-support ecl)       "impl-ecl"
                        #+(and thread-support mezzano)   "impl-mezzano"
                        #+(and thread-support mkcl)      "impl-mkcl"
                        #+(and thread-support lispworks) "impl-lispworks"
                        #+(and thread-support digitool)  "impl-mcl"
                        #+(and thread-support sbcl)      "impl-sbcl"
                        #+(and thread-support scl)       "impl-scl"
                        #-thread-support                 "impl-null")
                 #+(and thread-support lispworks (not (or lispworks6 lispworks7)))
                 (:file "impl-lispworks-condition-variables")
                 #+(and thread-support digitool)
                 (:file "condition-variables")
                 (:file "default-implementations")))))

(defsystem :bordeaux-threads/test
  :author "Greg Pfeil <greg@technomadic.org>"
  :description "Bordeaux Threads test suite."
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :depends-on (:bordeaux-threads :fiveam)
  :components ((:module "test"
                :components ((:file "bordeaux-threads-test")))))

(defmethod perform ((o test-op) (c (eql (find-system :bordeaux-threads))))
  (load-system :bordeaux-threads/test :force '(:bordeaux-threads/test))
  (symbol-call :5am :run! :bordeaux-threads))
