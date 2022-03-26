;;;; cl-tga.lisp

(in-package #:cl-tga)

(defparameter +TGA-RGB+ 2)
(defparameter +TGA-A+ 3)
(defparameter +TGA-RLE+ 10)

(defclass tga ()
  ((length :type fixnum
	   :initform 0)
   (width :type fixnum
	  :initform 0
	  :accessor image-width)
   (height :type fixnum
	   :initform 0
	   :accessor image-height)
   (img-type :type fixnum
	     :initform 0)
   (bits :type fixnum
	 :initform 0
	 :accessor image-bpp)
   (channels :type fixnum
	     :initform 0
	     :accessor image-channels)
   (stride :type fixnum
	   :initform 0)
   (img-data :initform 0
	     :accessor image-data)))

(defun read-word (stream)
  (let ((r 0))
    (setf (ldb (byte 16 0) r) (read-byte stream)
	  (ldb (byte 16 8) r) (read-byte stream))
    r))

(defun skip (stream &optional (bytes 1))
  (dotimes (i bytes)
    (read-byte stream)))

(defun read-tga (filespec)
  (declare (optimize speed space))
  (with-open-file (s filespec :element-type '(unsigned-byte 8))
    (let ((image (make-instance 'tga)))
      (with-slots (length width height img-type bits channels stride img-data) image
	(setf length (read-byte s))
	(skip s) ;; Skip a byte
	(setf img-type (read-byte s))
	(skip s 9) ;; Skip 9 bytes
	(setf width (read-word s))
	(setf height (read-word s))
	(setf bits (read-byte s))
	(skip s (1+ length)) ;; Skip to image information
	(setf channels (if (= bits 16)
				 3
				 (/ bits 8)))
	(if (not (= img-type +TGA-RLE+))
	    ;; No color table, just load the data
	    (let* ((stride (* channels width))
		   (data (make-array (* height stride)
				     :element-type '(unsigned-byte 8)))
		   (line (make-array stride
				     :element-type (if (= bits 16)
						       '(unsigned-byte 4)
						       '(unsigned-byte 8)))))
	      (declare (type (simple-array (unsigned-byte 8) (*)) data line))
	      #|
	      (loop for i of-type fixnum from 0 below height
		    for offset of-type fixnum = (* stride i)
		    do (read-sequence line s)
		    do
		       (loop for j of-type fixnum
			       from 0 below stride by channels
			     do
				(setf (aref data (+ offset   j)) (aref line (+ 2 j))
				      (aref data (+ offset 1 j)) (aref line (+ 1 j))
				      (aref data (+ offset 2 j)) (aref line j))
			     when (= channels 4)
			       do
				  (setf (aref data (+ offset 3 j)) (aref line (+ 3 j)))))
	      |#
	      (read-sequence data s)
	      (setf img-data data))
	    ;; FIXME: RLE Decoding goes here!
	    (warn "RLE Decoding not yet implemented.")))
      image)))
