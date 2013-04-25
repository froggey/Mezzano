(in-package :asdf)

(defsystem "lispos-compiler"
  :description "Compiler for LispOS."
  :version "0"
  :author "Henry Harrington <henry.harrington@gmail.com>"
  :licence "None"
  :depends-on ("lispos-lap" #:alexandria #:iterate #+sbcl #:sb-cltl2 #:nibbles)
  :components ((:file "cross")
               (:file "data-types" :depends-on ("cross"))
               (:file "cross-compile" :depends-on ("cross"))
               (:file "cross-boot" :depends-on ("cross" "cross-compile"))
               (:file "parse" :depends-on ("cross"))
               (:file "compiler" :depends-on ("cross"))
               (:file "pass1" :depends-on ("cross" "compiler"))
               (:file "inline" :depends-on ("cross" "compiler"))
               (:file "lift" :depends-on ("cross" "compiler"))
               (:file "simplify" :depends-on ("cross" "compiler"))
               (:file "constprop" :depends-on ("cross" "compiler"))
               (:file "kill-temps" :depends-on ("cross" "compiler"))
               (:file "builtins" :depends-on ("cross" "cross-compile" "compiler" "codegen" "data-types"))
               (:file "codegen" :depends-on ("cross" "cross-compile" "compiler" "data-types"))
               (:file "branch-tension" :depends-on ("cross" "compiler" "codegen"))
))
