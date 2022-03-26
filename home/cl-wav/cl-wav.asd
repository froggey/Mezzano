;;;; cl-wav.asd

(asdf:defsystem #:cl-wav
  :version "0.0.1"
  :author "Rob Blackwell"
  :description "Reads Wave Audio File Format, WAV files."
  :serial t
  :depends-on (#:alexandria #:cl-riff)
  :components ((:file "package")
	       (:file "wav")))





