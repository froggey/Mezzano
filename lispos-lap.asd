;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :asdf)

(defsystem "lispos-lap"
  :description "Assembler for LispOS."
  :version "0"
  :author "Henry Harrington <henry.harrington@gmail.com>"
  :licence "MIT"
  :components ((:file "lap")
               (:file "lap-x86" :depends-on ("lap"))))
