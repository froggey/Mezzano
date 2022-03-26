(asdf:defsystem #:parsley
  :description "A toolset for parsing binary data formats."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/parsley"
  :source-control (:git "https://github.com/mfiano/parsley.git")
  :bug-tracker "https://github.com/mfiano/parsley/issues"
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:fast-io
               #:bitio
               #:chipz
               #:babel)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "buffer")
   (:file "common")
   (:file "processors")
   (:file "readers")))
