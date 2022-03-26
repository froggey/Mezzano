(asdf:defsystem #:cl-video-wav
  :description "WAV decoding module of CL-VIDEO"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :depends-on (#:alexandria #:cl-riff #:cl-video #:flexi-streams)
  :serial t
  :components ((:file "static")
	       (:file "wav")))

