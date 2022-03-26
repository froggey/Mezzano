;;;; cl-video.asd

(asdf:defsystem #:cl-video
  :description "Video decoder core implemented in Common Lisp"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :version "1.5"
  :depends-on (#:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "cl-video")))
