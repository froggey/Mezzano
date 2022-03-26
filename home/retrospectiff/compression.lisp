
(in-package :retrospectiff.compression)

(defparameter *compressions*
  (list (list +no-compression+ #'identity #'identity)
	(list +packbits-compression+ 'packbits-decode 'packbits-encode)
	(list +lzw-compression+ 'lzw-decode 'lzw-encode)
        (list +jpeg-compression+ 'jpeg-decode nil)
        (list +deflate-compression+ 'deflate-decode nil)))

(defclass image-info () ())

(defclass jpeg-image-info (image-info)
  ((jpeg-tables :initarg :jpeg-tables :accessor jpeg-tables)
   (jpeg-image :initarg :jpeg-image :accessor jpeg-image :initform nil)))

(defun find-compression-decoder (compression)
  (let ((compression (or compression +no-compression+)))
    (let ((decoder (cadr (assoc compression *compressions*))))
      (if decoder
	  decoder
	  (error "Compression not supported: ~a" compression)))))

