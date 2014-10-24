(in-package :asdf)

(defsystem "lispos-compiler"
  :description "Compiler for LispOS."
  :version "0"
  :author "Henry Harrington <henry.harrington@gmail.com>"
  :licence "None"
  :depends-on ("lispos-lap" #:alexandria #:iterate #+sbcl #:sb-cltl2 #:nibbles)
  :components ((:file "compiler/cross")
               (:file "system/data-types" :depends-on ("compiler/cross"))
               (:file "system/parse" :depends-on ("compiler/cross"))
               (:file "system/backquote" :depends-on ("compiler/cross"))
               (:file "compiler/cross-compile"
                      :depends-on ("compiler/cross" "system/backquote"))
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
               (:file "compiler/builtins"
                      :depends-on ("compiler/cross" "compiler/cross-compile" "compiler/compiler"
                                   "compiler/codegen" "system/data-types"))
               (:file "compiler/codegen"
                      :depends-on ("compiler/cross" "compiler/cross-compile" "compiler/compiler"
                                   "system/data-types"))
               (:file "compiler/branch-tension"
                      :depends-on ("compiler/cross" "compiler/compiler" "compiler/codegen"))
))
