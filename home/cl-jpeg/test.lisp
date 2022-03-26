(cl:in-package :jpeg)

(defun test-reencode (&key (pathname #P"/home/eugene/Pictures/nyc.jpg") cached)
  (time (multiple-value-bind (buf h w nc)
	    (jpeg:decode-image pathname :cached-source-p cached)
	  (jpeg:encode-image #P"/tmp/test.jpg" buf nc h w))))

(defun test-decode (&key (pathname #P"/home/eugene/Pictures/nyc.jpg") cached)
  (time (jpeg:decode-image pathname :cached-source-p cached))
  nil)

(defun test-decode-reuse (&key (pathname #P"/home/eugene/Pictures/nyc.jpg"))
  (let ((d (make-descriptor)))
    (with-open-file (stream pathname :direction :input :element-type 'uint8)
      (time (jpeg:decode-stream stream :cached-source-p t :descriptor d)))
    (with-open-file (stream pathname :direction :input :element-type 'uint8)
      (time (jpeg:decode-stream stream :cached-source-p t :descriptor d))))
  nil)

(defun test-reencode-prealloc (&key (pathname #P"/home/eugene/Pictures/nyc.jpg") cached)
  (multiple-value-bind (h w ncomp)
      (jpeg-file-dimensions pathname)
    (let ((buf (allocate-buffer h w ncomp)))
      (decode-image pathname :buffer buf :cached-source-p cached)
      (encode-image #P"/tmp/test.jpg" buf ncomp h w))))

(defparameter *gray-q-tabs* (vector jpeg::+q-luminance+))

(defun test-encode-grayscale (&optional (output #P"/tmp/gray.jpg"))
  (let ((width 32)
        (height 32))
    (let ((buf (make-array (* width height) :initial-element 42)))
      (time (jpeg:encode-image output buf 1 width height
                          :q-tabs *gray-q-tabs*)))))

(defun test-flexi-decode (&optional (pathname #P"/home/eugene/Pictures/nyc.jpg"))
  (time (with-open-file (stream pathname :direction :input :element-type 'uint8)
    (let ((seq (make-array (file-length stream))))
    (read-sequence seq stream)
    (flexi-streams:with-input-from-sequence (is seq) (jpeg:decode-stream is)))))
  nil)
