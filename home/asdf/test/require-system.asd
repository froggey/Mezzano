(defsystem "require-system" :version "2.0"
   :depends-on ("require-system/immutable"))

(defsystem "require-system/preloaded" :version "2.0")
(defsystem "require-system/immutable" :version "2.0")

(defsystem "require-system/not-loaded" :version "2.0")

(defsystem "require-system/ordinary" :version "2.0")

(defsystem "require-system/require"
  :version "2.0"
  :depends-on ((:require "require-system/module1")
               (:require "require-system/module2")))

(defsystem "require-system/depends"
  :version "2.0"
  :depends-on ("test-asdf/module1"
               "require-system-module2"))

