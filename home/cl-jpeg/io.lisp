;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here are some useful routines

(in-package #:jpeg)

;;; Produces outfile (Windows 24-bit bitmap) from a JPEG infile
(defun jpeg-to-bmp (&key infile outfile)
  (with-open-file (o outfile :direction :output :element-type 'unsigned-byte)
    (multiple-value-bind (rgb h w number-components)
        (decode-image infile)
      (let* ((compl (rem w 4))
             (len (+ 54 (* h w 3) (mul compl h))))
        ;; BITMAPFILEHEADER
        (write-byte #x42 o) ; type
        (write-byte #x4d o)
        (write-byte (logand len 255) o) ; file size
        (write-byte (logand (ash len -8) 255) o)
        (write-byte (logand (ash len -16) 255) o)
        (write-byte (logand (ash len -24) 255) o)
        (write-byte 0 o) ; reserved
        (write-byte 0 o)
        (write-byte 0 o)
        (write-byte 0 o)
        (write-byte #x36 o) ; offset
        (write-byte 0 o)
        (write-byte 0 o)
        (write-byte 0 o)
        ;; BITMAPINFOHEADER
        (write-byte 40 o) ; struct size
        (write-byte 0 o)
        (write-byte 0 o)
        (write-byte 0 o)
        (write-byte (logand w 255) o) ; width
        (write-byte (logand (ash w -8) 255) o)
        (write-byte (logand (ash w -16) 255) o)
        (write-byte (logand (ash w -24) 255) o)
        (write-byte (logand h 255) o) ; height
        (write-byte (logand (ash h -8) 255) o)
        (write-byte (logand (ash h -16) 255) o)
        (write-byte (logand (ash h -24) 255) o)
        (write-byte 1 o) ; planes, always one for BMP
        (write-byte 0 o)
        (write-byte 24 o) ; bitcount, 24-bit BMP
        (write-byte 0 o)
        ;; the rest of header
        (write-sequence (make-array 24 :initial-element 0 :element-type 'unsigned-byte) o)
        (ecase number-components
          (1
           (loop :for y :from (1- h) :downto 0 :do
                 (loop :for x :from (1- w) :downto 0 :do
                       (let ((grey (aref rgb (+ x (* y w)))))
                         (write-byte grey o)
                         (write-byte grey o)
                         (write-byte grey o)))
                 (dotimes (i compl)
                   (write-byte 0 o))))
          (3
           (loop for y fixnum from (1- h) downto 0
                 for ypos fixnum = (* y 3 w) do
                 (loop for x fixnum from ypos to (plus ypos (* (1- w) 3)) by 3 do
                       (write-byte (the unsigned-byte (aref rgb x)) o)
                       (write-byte (the unsigned-byte (aref rgb (1+ x))) o)
                       (write-byte (the unsigned-byte (aref rgb (plus 2 x))) o))
                 (loop for i fixnum from 0 below compl do ; adjusting to double-word
                       (write-byte 0 o)))))))))


;;; Provides simple user interface for encoder: quality may vary 1 to 5 (decreasing)
(defun encoding-wrapper (filename image ncomp h w &key (quality 4))
  (case quality
    ;; quite good
    (1 (encode-image filename image ncomp h w
                     :q-tabs (vector +q-luminance-hi+ +q-chrominance-hi+) :sampling '((1 1)(1 1)(1 1))))
    ;; quite good either
    (2 (encode-image filename image ncomp h w
                     :q-tabs (vector +q-luminance-hi+ +q-chrominance-hi+) :sampling '((2 2)(1 1)(1 1))))
    ;; satisfactory
    (3 (encode-image filename image ncomp h w
                     :q-tabs (vector +q-luminance+ +q-chrominance+) :sampling '((1 1)(1 1)(1 1))))
    ;; fair, but slightly worse
    (4 (encode-image filename image ncomp h w
                     :q-tabs (vector +q-luminance+ +q-chrominance+) :sampling '((2 2)(1 1)(1 1))))
    ;; poor, but tolerable in a case of blurry original, gives a sufficient compression
    (5 (encode-image filename image ncomp h w
                     :q-tabs (vector +q-luminance+ +q-chrominance+) :sampling '((2 3)(1 1)(1 1))))
    (otherwise (error "Illegal encoding quality number specified (valid 1 till 5)"))))
