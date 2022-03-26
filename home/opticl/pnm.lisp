;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(defparameter *whitespace-chars* #(#\# #\Space #\Newline #\Tab #\Linefeed #\Return))
(defparameter *whitespace-bytes* (map 'vector #'char-code *whitespace-chars*))

;;;
;;; various utility functions for reading and writing PNM files
(defun whitespace-byte-p (int)
  (find int *whitespace-bytes*))

(defun skip-line (stream)
  (loop for c = (read-byte stream)
     until (find c (map 'vector #'char-code '(#\Newline #\Linefeed)))))

(defun read-until-whitespace (stream)
  (loop for c = (read-byte stream)
     until (whitespace-byte-p c)
     collect c))

(defun read-byte-skipping-whitespace (stream)
  (loop for c = (read-byte stream)
     while (whitespace-byte-p c)
     do (when (eq c (char-code #\#))
          (skip-line stream))
     finally (return c)))

(defun read-number (stream)
  (let ((first-char (read-byte-skipping-whitespace stream)))
    (map 'string #'code-char
         (cons first-char (read-until-whitespace stream)))))

(defun read-binary-value (stream)
  (let ((byte (read-byte stream)))
    (if (whitespace-byte-p byte)
        (read-binary-value stream)
        (digit-char-p (code-char byte)))))

(defun write-integer (int stream)
  (map nil (lambda (x) (write-byte (char-code x) stream)) (format nil "~D" int)))

(defun write-word (word stream)
  (write-byte (ash word -8) stream)
  (write-byte (logand word #xff) stream))

;;;
;;; PBM Files

(defun %read-pbm-ascii-stream (stream)
  (let ((width (parse-integer (read-number stream)))
        (height (parse-integer (read-number stream))))
    (let ((img (make-1-bit-gray-image height width)))    
      (loop for i below height
         do 
           (loop for j below width
              do (setf (pixel img i j)
                       (read-binary-value stream))))
      img)))

(defun %read-pbm-binary-stream (stream)
  (let ((width (parse-integer (read-number stream)))
        (height (parse-integer (read-number stream))))
    (let* ((img (make-1-bit-gray-image height width))
           (bytes (make-array (1+ (ash (1- (* height width)) -3))
                              :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      (loop for i below height
         do (loop for j below width
               do (setf (pixel img i j)
                        (ldb (byte 1 (- 7 (mod (+ (* i width) j) 8)))
                             (aref bytes (ash (+ (* i width) j) -3))))))
      img)))

(defun read-pbm-stream (stream)
  (let ((magic (make-array 2 :element-type '(unsigned-byte 8))))
    (read-sequence magic stream)
    (cond ((equalp magic #(80 49)) ;; P1 magic number
           (%read-pbm-ascii-stream stream))
          ((equalp magic #(80 52)) ;; P4 magic number
           (%read-pbm-binary-stream stream))
          (t (error "Invalid PBM Magic Number")))))

(defun read-pbm-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-pbm-stream stream)))

(defun %write-pbm-ascii-stream (stream image)
  (map nil (lambda (x) (write-byte (char-code x) stream)) "P1")
  (write-byte (char-code #\Newline) stream)
  (with-image-bounds (height width)
      image
    (write-integer width stream)
    (write-byte (char-code #\Space) stream)
    (write-integer height stream)
    (write-byte (char-code #\Newline) stream)
    (loop for i below height
       do (loop for j below width
             do
               (write-byte (char-code (digit-char (pixel image i j))) stream)
               (if (and (plusp j) (= (mod (+ (* i width) j 1) 35) 0))
                 (write-byte (char-code #\Newline) stream)
                 (write-byte (char-code #\Space) stream))))))

(defun %write-pbm-binary-stream (stream image)
  (map nil (lambda (x) (write-byte (char-code x) stream)) "P4")
  (write-byte (char-code #\Newline) stream)
  (with-image-bounds (height width)
      image
    (write-integer width stream)
    (write-byte (char-code #\Space) stream)
    (write-integer height stream)
    (write-byte (char-code #\Newline) stream)
    (let ((bytes (make-array (1+ (ash (1- (* height width)) -3))
                             :element-type '(unsigned-byte 8))))
      (loop for i below height
         do (loop for j below width
               do
                 (setf (ldb (byte 1 (- 7 (mod (+ (* i width) j) 8)))
                            (aref bytes (ash (+ (* i width) j) -3)))
                       (pixel image i j))))
      (write-sequence bytes stream))))

(defun write-pbm-stream (stream image &key (binary t))
  (if binary
      (%write-pbm-binary-stream stream image)
      (%write-pbm-ascii-stream stream image)))

(defun write-pbm-file (pathname image
                       &key (binary t))
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-pbm-stream stream image :binary binary)
    (truename pathname)))


;;;
;;; PGM files

(defun %read-pgm-ascii-stream (stream)
  (let ((width (parse-integer (read-number stream)))
        (height (parse-integer (read-number stream)))
        (maxgray (parse-integer (read-number stream))))
    (if (< maxgray 256)
        (let ((img (make-8-bit-gray-image height width)))    
          (loop for i below height
             do 
             (loop for j below width
                do (setf (pixel img i j)
                         (parse-integer (read-number stream)))))
          img)
        (let ((img (make-16-bit-gray-image height width)))    
          (loop for i below height
             do 
               (loop for j below width
                  do (setf (pixel img i j)
                           (parse-integer (read-number stream)))))
          img))))

(defun %read-pgm-binary-stream (stream)
  (let ((width (parse-integer (read-number stream)))
        (height (parse-integer (read-number stream)))
        (maxgray (parse-integer (read-number stream))))
    (if (< maxgray 256)
        (let ((img (make-8-bit-gray-image height width)))    
          (loop for i below height
             do 
             (loop for j below width
                do (setf (pixel img i j)
                         (read-byte stream))))
          img)
        (let ((img (make-16-bit-gray-image height width)))    
          (loop for i below height
             do 
             (loop for j below width
                do (setf (pixel img i j)
                         (+ (ash (read-byte stream) 8)
                            (read-byte stream)))))
          img))))

(defun read-pgm-stream (stream)
  (let ((magic (make-array 2 :element-type '(unsigned-byte 8))))
    (read-sequence magic stream)
    (cond ((equalp magic #(80 50)) ;; P2 magic number
           (%read-pgm-ascii-stream stream))
          ((equalp magic #(80 53)) ;; P5 magic number
           (%read-pgm-binary-stream stream))
          (t (error "Invalid PGM Magic Number")))))

(defun read-pgm-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-pgm-stream stream)))


(defun %write-pgm-ascii-stream (stream image)
  (map nil (lambda (x) (write-byte (char-code x) stream)) "P2")
  (write-byte (char-code #\Newline) stream)
  (with-image-bounds (height width)
      image
    (write-integer width stream)
    (write-byte (char-code #\Space) stream)
    (write-integer height stream)
    (write-byte (char-code #\Newline) stream)
    (etypecase image
      (8-bit-gray-image
       (write-integer #xff stream))
      (16-bit-gray-image
       (write-integer #xffff stream)))
    (write-byte (char-code #\Newline) stream)
    (loop for i below height
       do (loop for j below width
             do 
               (write-integer (pixel image i j) stream)
               (unless (= j (1- width))
                 (write-byte (char-code #\Space) stream)))
         (write-byte (char-code #\Newline) stream))))

(defun %write-pgm-binary-stream (stream image)
  (map nil (lambda (x) (write-byte (char-code x) stream)) "P5")
  (write-byte (char-code #\Newline) stream)
  (with-image-bounds (height width)
      image
    (write-integer width stream)
    (write-byte (char-code #\Newline) stream)
    (write-integer height stream)
    (write-byte (char-code #\Newline) stream)
    (etypecase image
      (8-bit-gray-image
       (locally
           (declare (type 8-bit-gray-image image))
         (write-integer #xff stream)
         (write-byte (char-code #\Newline) stream)
         (loop for i below height
            do (loop for j below width
                  do (write-byte (pixel image i j) stream)))))
      (16-bit-gray-image
       (locally
           (declare (type 16-bit-gray-image image))
         (write-integer #xffff stream)
         (write-byte (char-code #\Newline) stream)
         (loop for i below height
            do (loop for j below width
                  do (let ((val (pixel image i j)))
                       (write-byte (ash val -8) stream)
                       (write-byte (logand val #xff) stream)))))))))

(defun write-pgm-stream (stream image &key (binary t))
  (if binary
      (%write-pgm-binary-stream stream image)
      (%write-pgm-ascii-stream stream image)))

(defun write-pgm-file (pathname image
                       &key (binary t))
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-pgm-stream stream image :binary binary)
    (truename pathname)))

;;;
;;; PPM Files

(defun %read-ppm-ascii-stream (stream)
  (let ((width (parse-integer (read-number stream)))
        (height (parse-integer (read-number stream)))
        (maxval (parse-integer (read-number stream))))
    (if (< maxval 256)
        (let ((img (make-8-bit-rgb-image height width)))    
          (loop for i below height
             do 
               (loop for j below width
                  do (setf (pixel img i j)
                           (values (read-number stream)
                                   (read-number stream)
                                   (read-number stream)))))
          img)
        (let ((img (make-16-bit-gray-image height width)))    
          (loop for i below height
             do 
               (loop for j below width
                  do (setf (pixel img i j)
                           (values (read-number stream)
                                   (read-number stream)
                                   (read-number stream)))))
          img))))

(defun %read-ppm-binary-stream (stream)
  (let ((width (parse-integer (read-number stream)))
        (height (parse-integer (read-number stream)))
        (maxval (parse-integer (read-number stream))))
    (if (< maxval 256)
        (let ((img (make-8-bit-rgb-image height width)))    
          (loop for i below height
             do 
             (loop for j below width
                do (setf (pixel img i j)
                         (values (read-byte stream)
                                 (read-byte stream)
                                 (read-byte stream)))))
          img)
        (let ((img (make-16-bit-gray-image height width)))    
          (loop for i below height
             do 
             (loop for j below width
                do (setf (pixel img i j)
                         (values (+ (ash (read-byte stream) 8)
                                    (read-byte stream))
                                 (+ (ash (read-byte stream) 8)
                                    (read-byte stream))
                                 (+ (ash (read-byte stream) 8)
                                    (read-byte stream))))))
          img))))

(defun read-ppm-stream (stream)
  (let ((magic (make-array 2 :element-type '(unsigned-byte 8))))
    (read-sequence magic stream)
    (cond ((equalp magic #(80 51)) ;; P3 magic number
           (%read-ppm-ascii-stream stream))
          ((equalp magic #(80 54)) ;; P6 magic number
           (%read-ppm-binary-stream stream))
          (t (error "Invalid PPM Magic Number")))))

(defun read-ppm-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-ppm-stream stream)))


(defun %write-ppm-ascii-stream (stream image)
  (map nil (lambda (x) (write-byte (char-code x) stream)) "P3")
  (write-byte (char-code #\Newline) stream)
  (with-image-bounds (height width)
      image
    (write-integer width stream)
    (write-byte (char-code #\Space) stream)
    (write-integer height stream)
    (write-byte (char-code #\Newline) stream)
    (etypecase image
      (8-bit-rgb-image (write-integer #xff stream))
      (8-bit-rgba-image (write-integer #xff stream))
      (16-bit-rgb-image (write-integer #xffff stream))
      (16-bit-rgba-image (write-integer #xffff stream)))
    (write-byte (char-code #\Newline) stream)
    (loop for i below height
       do (loop for j below width
             do 
               (write-integer (pixel image i j) stream)
               (unless (= j (1- width))
                 (write-byte (char-code #\Space) stream)))
         (write-byte (char-code #\Newline) stream))))

(defun %write-8-bit-rgb-ppm-binary-data (stream image height width)
  (write-integer #xff stream)
  (write-byte (char-code #\Newline) stream)
  (loop for i below height
     do (loop for j below width
           do (multiple-value-bind (r g b)
                  (pixel image i j) 
                (write-byte r stream)
                (write-byte g stream)
                (write-byte b stream)))))

(defun %write-16-bit-rgb-ppm-binary-data (stream image height width)
  (write-integer #xffff stream)
  (write-byte (char-code #\Newline) stream)
  (loop for i below height
     do (loop for j below width
           do (multiple-value-bind (r g b)
                  (pixel image i j) 
                (write-word r stream)
                (write-word g stream)
                (write-word b stream)))))

(defun %write-ppm-binary-stream (stream image)
  (map nil (lambda (x) (write-byte (char-code x) stream)) "P6")
  (write-byte (char-code #\Newline) stream)
  (with-image-bounds (height width)
      image
    (write-integer width stream)
    (write-byte (char-code #\Newline) stream)
    (write-integer height stream)
    (write-byte (char-code #\Newline) stream)
    (etypecase image
      (8-bit-rgb-image
       (%write-8-bit-rgb-ppm-binary-data stream image height width))
      (8-bit-rgba-image
       (%write-8-bit-rgb-ppm-binary-data stream image height width))
      (16-bit-rgb-image
       (%write-16-bit-rgb-ppm-binary-data stream image height width))
      (16-bit-rgba-image
       (%write-16-bit-rgb-ppm-binary-data stream image height width)))))

(defun write-ppm-stream (stream image &key (binary t))
  (if binary
      (%write-ppm-binary-stream stream image)
      (%write-ppm-ascii-stream stream image)))

(defun write-ppm-file (pathname image
                       &key (binary t))
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-ppm-stream stream image :binary binary)
    (truename pathname)))

;;;
;;; PNM Reading (based on magic number)

(defun read-pnm-stream (stream)
  (let ((magic (make-array 2 :element-type '(unsigned-byte 8))))
    (read-sequence magic stream)
    (cond ((equalp magic #(80 49)) ;; P1 magic number
           (%read-pbm-ascii-stream stream))
          ((equalp magic #(80 50)) ;; P2 magic number
           (%read-pgm-ascii-stream stream))
          ((equalp magic #(80 51)) ;; P3 magic number
           (%read-ppm-ascii-stream stream))
          ((equalp magic #(80 52)) ;; P4 magic number
           (%read-pbm-binary-stream stream))
          ((equalp magic #(80 53)) ;; P5 magic number
           (%read-pgm-binary-stream stream))
          ((equalp magic #(80 54)) ;; P6 magic number
           (%read-ppm-binary-stream stream))
          (t (error "Invalid PGM Magic Number")))))

(defun read-pnm-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-pnm-stream stream)))


