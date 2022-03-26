;;;; package.lisp

(defpackage #:riff
  (:use #:cl
	#:alexandria)
  (:export #:read-riff-chunk
	   #:read-riff-chunks
	   #:read-riff-file
	   #:find-riff-chunk
	   #:read-u2
	   #:read-u4
	   #:default-chunk-data-reader
	   #:riff-chunk-id
	   #:riff-chunk-data
	   #:riff-chunk-data-size
           #:riff-chunk-data-start
           #:riff-chunk-data-end
	   #:riff-file-type)
  (:export #:write-riff-chunk
           #:write-riff-chunks
           #:write-riff-file
           #:write-u2
           #:write-u4
           #:default-chunk-data-writer))
