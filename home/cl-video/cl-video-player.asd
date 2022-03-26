(asdf:defsystem #:cl-video-player
  :description "Video decoder implemented in Common Lisp"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :depends-on (#:cl-video-avi #:cl-video-gif #:cl-video-wav #:clx #:bordeaux-threads #:cl-portaudio)
  :serial t
  :components ((:file "portaudio-out")
	       (:file "player")))
