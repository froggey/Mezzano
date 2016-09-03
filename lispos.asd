;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :asdf)

(defsystem "lispos"
  :description "Lisp operating system."
  :version "0"
  :author "Henry Harrington <henry.harrington@gmail.com>"
  :licence "MIT"
  :depends-on (#:nibbles #:cl-ppcre #:iterate
               #:alexandria)
  :serial t
  :components ((:file "compiler/cross")
               (:file "system/data-types")
               (:file "system/parse")
               (:file "system/backquote")
               (:file "compiler/compiler")
               (:file "compiler/environment")
               (:file "compiler/cross-compile")
               (:file "compiler/cross-boot")
               (:file "compiler/lap")
               (:file "compiler/lap-x86")
               (:file "compiler/ast")
               (:file "compiler/ast-generator")
               (:file "compiler/keyword-arguments")
               (:file "compiler/simplify-arguments")
               (:file "compiler/pass1")
               (:file "compiler/inline")
               (:file "compiler/lift")
               (:file "compiler/simplify")
               (:file "compiler/constprop")
               (:file "compiler/kill-temps")
               (:file "compiler/value-aware-lowering")
               (:file "compiler/lower-environment")
               (:file "compiler/lower-special-bindings")
               (:file "compiler/simplify-control-flow")
               (:file "compiler/codegen")
               (:file "compiler/branch-tension")
               (:file "compiler/builtins/builtins")
               (:file "compiler/builtins/array")
               (:file "compiler/builtins/character")
               (:file "compiler/builtins/cons")
               (:file "compiler/builtins/memory")
               (:file "compiler/builtins/misc")
               (:file "compiler/builtins/numbers")
               (:file "compiler/builtins/objects")
               (:file "compiler/builtins/unwind")
               (:file "tools/build-unicode")
               (:file "tools/build-pci-ids")
               (:file "tools/cold-generator")))
