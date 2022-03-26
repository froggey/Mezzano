(asdf:defsystem #:pngload.test
  :description "Tests for pngload."
  :author ("Michael Fiano <mail@michaelfiano.com>"
           "Bart Botta <00003b@gmail.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/pngload"
  :bug-tracker "https://github.com/HackerTheory/pngload/issues"
  :source-control (:git "https://github.com/HackerTheory/pngload.git")
  :encoding :utf-8
  :depends-on (#:alexandria
               #:local-time
               #:opticl
               #:png-read
               #:pngload)
  :pathname "src"
  :serial t
  :components
  ((:file "test")))
