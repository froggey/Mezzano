;;;; cl-riff.asd

(asdf:defsystem #:cl-riff
  :version "0.0.2"
  :author "Rob Blackwell"
  :description "Reads Resource Interchange File Format (RIFF) files."
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
	       (:file "riff")
               (:file "write")))
