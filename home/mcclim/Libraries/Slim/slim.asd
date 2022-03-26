(in-package #:asdf-user)

(defsystem #:slim
  :depends-on (#:mcclim)
  :components ((:file "slim")
               (:doc-file "slim.md")))
