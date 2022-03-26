;;;; package.lisp

(defpackage #:wav
  (:use #:cl #:alexandria)
  (:export #:+unknown+
	   #:+pcmi-uncompressed+
	   #:+microsoft-adpcm+
	   #:+itu-g711-a-law+
	   #:+itu-g711-mu-law+
	   #:+ima-adpcm+
	   #:+itu-g723-adpcm-yamaha+
	   #:+gsm-610+
	   #:+itu-g721+adpcm+
	   #:+mpeg+
	   #:+experimental+
	   #:read-wav-file
	   #:format-chunk-data-reader
	   #:wrap-format-chunk-data-reader
	   #:data-chunk-data-samples-reader
	   #:wrap-data-chunk-data-samples-reader))




