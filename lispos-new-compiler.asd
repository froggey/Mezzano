(in-package :asdf)

(defsystem "lispos-new-compiler"
  :description "New compiler."
  :version "0"
  :author "Henry Harrington <henry.harrington@gmail.com>"
  :licence "None"
  :depends-on ("lispos-lap" "lispos-compiler" "lispos")
  :components ((:file "new-compiler/cl-translate")
               (:file "new-compiler/assignment-conversion")
               (:file "new-compiler/rewriter")
               (:file "new-compiler/optimize")
               (:file "new-compiler/target-cl")
               (:file "new-compiler/hoist-if"
                :depends-on ("new-compiler/rewriter"))))
