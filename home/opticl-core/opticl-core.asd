
(asdf:defsystem :opticl-core
  :name "opticl-core"
  :description "A library for representing and processing images"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :serial t
  :default-component-class cl-source-file
  :depends-on (alexandria)
  :components
  ((:file "package")
   (:file "opticl-core")))




