(defpackage #:jpeg
  (:use #:common-lisp)
  (:nicknames #:cl-jpeg)
  (:export #:encode-image
           #:decode-stream
           #:decode-image
	   #:jpeg-error #:jpeg-encoder-error #:jpeg-decoder-error #:unsupported-jpeg-format #:unrecognized-file-format
	   #:convert-cmyk-to-rgb
	   #:allocate-buffer
	   #:jpeg-file-dimensions
           #:jpeg-to-bmp
	   #:descriptor-source-cache
	   #:read-dht
	   #:inverse-llm-dct #:llm-dct
	   #:make-descriptor #:descriptor-byte-reader))
