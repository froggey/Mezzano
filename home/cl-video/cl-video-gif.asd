(asdf:defsystem #:cl-video-gif
  :description "Animated GIF module of CL-VIDEO"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :depends-on (#:cl-video #:skippy #:alexandria)
  :serial t
  :components ((:file "gif")))
