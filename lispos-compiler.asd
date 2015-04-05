;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :asdf)

(defsystem "lispos-compiler"
  :description "Compiler for LispOS."
  :version "0"
  :author "Henry Harrington <henry.harrington@gmail.com>"
  :licence "MIT"
  :depends-on (#:alexandria #:iterate #:nibbles)
  :components ((:file "compiler/cross")
               (:file "lap" :depends-on ("compiler/cross"))
               (:file "lap-x86" :depends-on ("compiler/cross" "lap"))
               (:file "system/data-types" :depends-on ("compiler/cross"))
               (:file "system/parse" :depends-on ("compiler/cross"))
               (:file "system/backquote" :depends-on ("compiler/cross"))
               (:file "compiler/cross-compile"
                      :depends-on ("compiler/cross" "system/backquote" "system/data-types"))
               (:file "compiler/compiler"
                      :depends-on ("compiler/cross"))
               (:file "compiler/cross-boot"
                      :depends-on ("compiler/cross" "compiler/cross-compile"))
               (:file "compiler/pass1"
                      :depends-on ("compiler/cross" "compiler/compiler"))
               (:file "compiler/inline"
                      :depends-on ("compiler/cross" "compiler/compiler"))
               (:file "compiler/lift"
                      :depends-on ("compiler/cross" "compiler/compiler"))
               (:file "compiler/simplify"
                      :depends-on ("compiler/cross" "compiler/compiler"))
               (:file "compiler/constprop"
                      :depends-on ("compiler/cross" "compiler/compiler"))
               (:file "compiler/kill-temps"
                      :depends-on ("compiler/cross" "compiler/compiler"))
               (:file "compiler/lower-environment"
                      :depends-on ("compiler/cross" "compiler/compiler"))
               (:file "compiler/lower-special-bindings"
                      :depends-on ("compiler/cross" "compiler/compiler"))
               (:file "compiler/builtins/builtins"
                      :depends-on ("compiler/cross" "compiler/cross-compile" "compiler/compiler"
                                   "compiler/codegen" "system/data-types" "lap" "lap-x86"))
               (:file "compiler/builtins/array" :depends-on ("compiler/builtins/builtins"))
               (:file "compiler/builtins/character" :depends-on ("compiler/builtins/builtins"))
               (:file "compiler/builtins/cons" :depends-on ("compiler/builtins/builtins"))
               (:file "compiler/builtins/memory" :depends-on ("compiler/builtins/builtins"))
               (:file "compiler/builtins/misc" :depends-on ("compiler/builtins/builtins"))
               (:file "compiler/builtins/numbers" :depends-on ("compiler/builtins/builtins"))
               (:file "compiler/builtins/objects" :depends-on ("compiler/builtins/builtins"))
               (:file "compiler/builtins/symbols" :depends-on ("compiler/builtins/builtins"))
               (:file "compiler/builtins/unwind" :depends-on ("compiler/builtins/builtins"))
               (:file "compiler/codegen"
                      :depends-on ("compiler/cross" "compiler/cross-compile" "compiler/compiler"
                                   "system/data-types" "lap" "lap-x86"))
               (:file "compiler/branch-tension"
                      :depends-on ("compiler/cross" "compiler/compiler" "compiler/codegen"
                                   "lap" "lap-x86"))
))
