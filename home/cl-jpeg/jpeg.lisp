;;; ANSI Common Lisp (mostly) baseline JPEG encoder/decoder implementation
;;; Copyright [c] 1999,2015-2017 Eugene Zaikonnikov <eugene@funcall.org>
;;;               
;;; This software is distributed under the terms of BSD-like license
;;; [see LICENSE for details]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This software was sponsored by Kelly E. Murray
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Two main functions available:
;;;
;;; (encode-image filename image ncomp h w &key sampling q-tabs q-factor), where:
;;; filename - output file name
;;; ncomp - number of components (1-4)
;;; h, w - source image height and width respectively
;;; image - array of B, G, R pixels in case of three component image,
;;;         array of grayscale pixels in case of single component,
;;;         array of 2 or 4 pixels in the case of two or four component image respectively
;;; :q-tabs - specifies quantization tables vector, should be 1 for 1,
;;;           2 for 2, 2 for 3 and 4 entries for 4 components
;;; :sampling - sampling frequency for ncomp components by X and Y axis,
;;;             e.g. '((2 2) (1 1) (1 1)) for three components, can be omitted
;;;             for grayscale and RGB images
;;; :q-factor - quality specifier (1-64), default is 64
;;; Returns nothing of practical use
;;;
;;; (decode-image filename &key buffer (colorspace-conversion t))
;;; filename - jpeg file name
;;; Returns (multiple-valued) IMAGE array in the same format as encoder source image,
;;; image HEIGHT and image WIDTH
;;; A pre-allocated BUFFER can be specified (see JPEG:ALLOCATE-BUFFER).
;;; If :colorspace-conversion is NIL, no conversion from YUV space is performed.
;;;
;;; For those impatient additional function defined:
;;; (jpeg-to-bmp &key infile outfile)
;;; Converts JPEG image specified by infile into Microsoft Windows 24-bit BMP format (outfile),
;;; returns NIL
;;;
;;; Additionaly, you may use more user-friendly version of encode-image: encode-wrapper.
;;; (encoding-wrapper filename image ncomp h w &key quality)
;;; All parameters have the same meaning as in encode-image, except quality.
;;; It is an integer value ranging 1 to 5 which specifies
;;; subjective quality of a resulting image.

;;; Technical details: encoder produces interleaved jpeg file, without restarts.
;;; In a case of 3 components image will be written in JFIF format.

;;; Decoder can deal with *almost* all baseline jpeg files, regardless JFIF or not.

;;; It supports restarts, interleaved/noninterleaved files, multiscan images, 1 to 4 color
;;; channels, up to 4 quantization tables and two sets of huffman tables with random order
;;; of their definition inside the image. Decoder *does not* support DNL marker, due to
;;; it's rarity and amount of work needed to implement it, so decoder isn't baseline in a
;;; strict sense.

;;; Both encoder and decoder utilize Loeffer, Ligtenberg and Moschytz integer discrete
;;; cosine transform algorithms with 12 multiplications in each loop.

;;; Based on CCITT Rec. T.81
;;; "Information technology - digital compression and coding of continious-tone still images
;;; - requirements and guidelines".
;;; Credits:
;;; to the Independent JPEG Group -
;;; colorspace conversion and DCT algorithms were adopted from their sources;
;;; to Jeff Dalton for his wise paper "Common Lisp Pitfalls".

(in-package #:jpeg)

(declaim (inline csize quantize get-average zigzag read-jpeg-byte
                 llm-dct descale crunch colorspace-convert subsample inverse-llm-dct
                 dequantize upsample extend recieve decode-ac decode-dc decode-block
                 izigzag write-bits limit))

(deftype uint8 () '(unsigned-byte 8))
(deftype uint8-array () '(simple-array uint8 (*)))
(deftype uint8-2d-array () '(simple-array uint8-array (*)))

(deftype suint8 () '(signed-byte 8))
(deftype sint8-array () '(simple-array sint8 (*)))
(deftype sint8-2d-array () '(simple-array sint8-array (*)))

(deftype sint16 () '(signed-byte 16))
(deftype sint16-array () '(simple-array sint16 (*)))
(deftype sint16-2d-array () '(simple-array sint16-array (*)))

(deftype uint16 () '(unsigned-byte 16))
(deftype uint16-array () '(simple-array uint16 (*)))
(deftype uint16-2d-array () '(simple-array uint16-array (*)))

(deftype fixnum-array () '(simple-array fixnum (*)))
(deftype fixnum-2d-array () '(simple-array fixnum-array (*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize* '(optimize (safety 0) (space 0) (debug 0) (speed 3))))

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; For ease of reference
(defmacro u8ref (data x y)
  `(the uint8 (aref (the uint8-array (aref (the uint8-2d-array ,data) ,y)) ,x)))

(defmacro s16ref (data x y)
  `(the sint16 (aref (the sint16-array (aref (the sint16-2d-array ,data) ,y)) ,x)))

(defmacro u16ref (data x y)
  `(the uint16 (aref (the uint16-array (aref (the uint16-2d-array ,data) ,y)) ,x)))

(defmacro fixref (data x y)
  `(the fixnum (aref (the fixnum-array (aref (the fixnum-2d-array ,data) ,y)) ,x)))

;;; Integer arithmetic wrappers
(defmacro plus (a b)
  `(the fixnum (+ (the fixnum ,a) (the fixnum ,b))))

(defmacro minus (a b)
  #+(or clisp abcl)
  `(- ,a ,b)
  #-(or clisp abcl)
  `(the fixnum (- (the fixnum ,a) (the fixnum ,b))))

(defmacro mul (a b)
  `(the fixnum (* (the fixnum ,a) (the fixnum ,b))))

(defmacro plus3 (x y z)
  `(plus (plus ,x ,y) ,z))

(defmacro mul3 (x y z)
  `(mul (mul ,x ,y) ,z)))

;;; Somewhat silly, but who knows...
(when (/= (integer-length most-positive-fixnum)
          (integer-length most-negative-fixnum))
  (error "Can't compile with asymmetric fixnums!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here we define some constants (markers, quantization and huffman tables etc.)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(eval-when (:compile-toplevel :load-toplevel)

(defun uint8-array (&rest contents)
  (the uint8-array (make-array (length contents) :element-type 'uint8
			       :initial-contents contents)))
  
(defun 2d-uint8-array (&rest contents)
  (let ((nrow (length contents)))
    (the uint8-2d-array
	 (make-array nrow
		:element-type 'uint8-array
		:initial-contents
                (loop for row in contents
                      collecting (make-array (length row) :element-type 'uint8
                                             :initial-contents row))))))

(defun 2d-sint16-array (&rest contents)
  (let ((nrow (length contents)))
    (the sint16-2d-array
	 (make-array nrow
		:element-type 'sint16-array
		:initial-contents
                (loop for row in contents
                      collecting (make-array (length row) :element-type 'sint16
                                             :initial-contents row))))))

;;; Source huffman tables for the encoder
(define-constant +luminance-dc-bits+
    (uint8-array #x00 #x01 #x05 #x01 #x01 #x01 #x01 #x01
		 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00))

(define-constant +luminance-dc-values+
    (uint8-array #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b))

(define-constant +chrominance-dc-bits+
    (uint8-array #x00 #x03 #x01 #x01 #x01 #x01 #x01 #x01
		 #x01 #x01 #x01 #x00 #x00 #x00 #x00 #x00))

(define-constant +chrominance-dc-values+
    (uint8-array #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b))

(define-constant +luminance-ac-bits+
    (uint8-array #x00 #x02 #x01 #x03 #x03 #x02 #x04 #x03
		 #x05 #x05 #x04 #x04 #x00 #x00 #x01 #x7d))

(define-constant +luminance-ac-values+
    (uint8-array
     #x01 #x02 #x03 #x00 #x04 #x11 #x05 #x12
     #x21 #x31 #x41 #x06 #x13 #x51 #x61 #x07
     #x22 #x71 #x14 #x32 #x81 #x91 #xa1 #x08
     #x23 #x42 #xb1 #xc1 #x15 #x52 #xd1 #xf0
     #x24 #x33 #x62 #x72 #x82 #x09 #x0a #x16
     #x17 #x18 #x19 #x1a #x25 #x26 #x27 #x28
     #x29 #x2a #x34 #x35 #x36 #x37 #x38 #x39
     #x3a #x43 #x44 #x45 #x46 #x47 #x48 #x49
     #x4a #x53 #x54 #x55 #x56 #x57 #x58 #x59
     #x5a #x63 #x64 #x65 #x66 #x67 #x68 #x69
     #x6a #x73 #x74 #x75 #x76 #x77 #x78 #x79
     #x7a #x83 #x84 #x85 #x86 #x87 #x88 #x89
     #x8a #x92 #x93 #x94 #x95 #x96 #x97 #x98
     #x99 #x9a #xa2 #xa3 #xa4 #xa5 #xa6 #xa7
     #xa8 #xa9 #xaa #xb2 #xb3 #xb4 #xb5 #xb6
     #xb7 #xb8 #xb9 #xba #xc2 #xc3 #xc4 #xc5
     #xc6 #xc7 #xc8 #xc9 #xca #xd2 #xd3 #xd4
     #xd5 #xd6 #xd7 #xd8 #xd9 #xda #xe1 #xe2
     #xe3 #xe4 #xe5 #xe6 #xe7 #xe8 #xe9 #xea
     #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 #xf8
     #xf9 #xfa))

(define-constant +chrominance-ac-bits+
    (uint8-array #x00 #x02 #x01 #x02 #x04 #x04 #x03 #x04
		#x07 #x05 #x04 #x04 #x00 #x01 #x02 #x77))

(define-constant +chrominance-ac-values+
    (uint8-array
     #x00 #x01 #x02 #x03 #x11 #x04 #x05 #x21
     #x31 #x06 #x12 #x41 #x51 #x07 #x61 #x71
     #x13 #x22 #x32 #x81 #x08 #x14 #x42 #x91
     #xa1 #xb1 #xc1 #x09 #x23 #x33 #x52 #xf0
     #x15 #x62 #x72 #xd1 #x0a #x16 #x24 #x34
     #xe1 #x25 #xf1 #x17 #x18 #x19 #x1a #x26
     #x27 #x28 #x29 #x2a #x35 #x36 #x37 #x38
     #x39 #x3a #x43 #x44 #x45 #x46 #x47 #x48
     #x49 #x4a #x53 #x54 #x55 #x56 #x57 #x58
     #x59 #x5a #x63 #x64 #x65 #x66 #x67 #x68
     #x69 #x6a #x73 #x74 #x75 #x76 #x77 #x78
     #x79 #x7a #x82 #x83 #x84 #x85 #x86 #x87
     #x88 #x89 #x8a #x92 #x93 #x94 #x95 #x96
     #x97 #x98 #x99 #x9a #xa2 #xa3 #xa4 #xa5
     #xa6 #xa7 #xa8 #xa9 #xaa #xb2 #xb3 #xb4
     #xb5 #xb6 #xb7 #xb8 #xb9 #xba #xc2 #xc3
     #xc4 #xc5 #xc6 #xc7 #xc8 #xc9 #xca #xd2
     #xd3 #xd4 #xd5 #xd6 #xd7 #xd8 #xd9 #xda
     #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 #xe8 #xe9
     #xea #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 #xf8
     #xf9 #xfa))

;;;Zigzag encoding matrix
(define-constant +zigzag-index+
  (2d-uint8-array '(0  1  5  6 14 15 27 28)
                  '(2  4  7 13 16 26 29 42)
                  '(3  8 12 17 25 30 41 43)
                  '(9 11 18 24 31 40 44 53)
                  '(10 19 23 32 39 45 52 54)
                  '(20 22 33 38 46 51 55 60)
                  '(21 34 37 47 50 56 59 61)
                  '(35 36 48 49 57 58 62 63)))

;;;JPEG file markers
(defconstant +M_COM+ #xfe)
(defconstant +M_SOF0+ #xc0)
(defconstant +M_SOF2+ #xc2)
(defconstant +M_DHT+ #xc4)
(defconstant +M_RST0+ #xd0)
(defconstant +M_RST7+ #xd7)
(defconstant +M_SOI+ #xd8)
(defconstant +M_EOI+ #xd9)
(defconstant +M_SOS+ #xda)
(defconstant +M_DQT+ #xdb)
(defconstant +M_DNL+ #xdc)
(defconstant +M_DRI+ #xdd)
(defconstant +M_DAC+ #xcc)
(defconstant +M_APP0+ #xe0)
(defconstant +M_APP14+ #xee)

;;; Default quantization tables
(define-constant +q-luminance+
  (2d-uint8-array '(16 11 10 16 24 40 51 61)
                  '(12 12 14 19 26 58 60 55)
                  '(14 13 16 24 40 57 69 56)
                  '(14 17 22 29 51 87 80 62)
                  '(18 22 37 56 68 109 103 77)
                  '(24 35 55 64 81 104 113 92)
                  '(49 64 78 87 103 121 120 101)
                  '(72 92 95 98 112 100 103 99)))

(define-constant +q-chrominance+
  (2d-uint8-array '(17 18 24 47 99 99 99 99)
                  '(18 21 26 66 99 99 99 99)
                  '(24 26 56 99 99 99 99 99)
                  '(47 66 99 99 99 99 99 99)
                  '(99 99 99 99 99 99 99 99)
                  '(99 99 99 99 99 99 99 99)
                  '(99 99 99 99 99 99 99 99)
                  '(99 99 99 99 99 99 99 99)))

(define-constant +q-luminance-hi+
  (2d-uint8-array '(10 7 6 10 15 25 32 38)
                  '(8 8 9 12 16 36 38 34)
                  '(9 8 10 15 25 36 43 35)
                  '(9 11 14 18 32 54 50 39)
                  '(11 14 23 35 42 68 64 48)
                  '(15 22 34 40 51 65 71 58)
                  '(31 40 49 54 64 76 75 63)
                  '(45 58 59 61 70 62 64 62)))

(define-constant +q-chrominance-hi+
  (2d-uint8-array '(11 11 15 29 62 62 62 62)
                  '(11 13 16 41 62 62 62 62)
                  '(15 16 35 62 62 62 62 62)
                  '(29 41 62 62 62 62 62 62)
                  '(62 62 62 62 62 62 62 62)
                  '(62 62 62 62 62 62 62 62)
                  '(62 62 62 62 62 62 62 62)
                  '(62 62 62 62 62 62 62 62)))

(defconstant +max-sample+ 255)

)

;;; Quantization performance test, each branch quantizes 30000 random matrixes
(eval-when (:compile-toplevel)

  (defconstant +quantize-calibration-loops+ 30000)
  
  (format t "Performing compile-time optimization.. please wait.~%")
  (finish-output)

  (defun qat1 ()
    (loop for i fixnum from 1 to +quantize-calibration-loops+ do
		(loop for row across +q-luminance+ do
		      (loop for q-coef fixnum across row
			    maximize (round (random 128) q-coef)))))

  (defun qat2 ()
    (loop for i fixnum from 1 to +quantize-calibration-loops+ do
		(loop for q-row across +q-luminance+ do
		      (loop for val fixnum = (random 128)
			    for absval fixnum = (abs val)
			    for qc fixnum across q-row
			    maximize
			    (cond ((< absval (ash qc -1))
				   0)
				  ((<= absval qc)
				   (if (minusp val)
				       -1
				     1))
				  ((<= (ash absval -1) qc)
				   (if (zerop (logand absval 1))
				       (if (minusp val)
					   -1
					 1)
				     (if (minusp val)
					 -2
				       2)))
				  (t
				   (round val qc)))))))

  (compile 'qat1)
  (compile 'qat2)
  
  (defvar *quantize-optimization*
    (<= (let ((time1 (get-internal-run-time)))
	  (qat1)
	  (minus (get-internal-run-time) time1))
	(let ((time1 (get-internal-run-time)))
	  (qat2)
	  (minus (get-internal-run-time) time1))))
  (format t "Done.~%")
  (finish-output))

(define-constant +q-tables+ (vector +q-luminance+ +q-chrominance+))

;;; This table is used to map coefficients into SSSS value
(define-constant +csize+ (make-array 2047
                                 :initial-contents
                                 (loop for i fixnum from 0 to 2046
                                       collecting (integer-length (abs (minus i 1023))))))

;;; Some constants for colorspace mapper
(defconstant shift (1- (integer-length (ash most-positive-fixnum -7))))
(defconstant +.299+ (round (+ (* 0.299 (ash 1 shift)) 0.5)))
(defconstant +.587+ (round (+ (* 0.587 (ash 1 shift)) 0.5)))
(defconstant +.114+ (round (+ (* 0.114 (ash 1 shift)) 0.5)))
(defconstant +-.1687+ (round (+ (* -0.1687 (ash 1 shift)) 0.5)))
(defconstant +-.3313+ (round (+ (* -0.3313 (ash 1 shift)) 0.5)))
(defconstant +-.4187+ (round (+ (* -0.4187 (ash 1 shift)) 0.5)))
(defconstant +-.0813+ (round (+ (* -0.0813 (ash 1 shift)) 0.5)))
(defconstant +.5+ (round (+ (* 0.5 (ash 1 shift)) 0.5)))
(defconstant +uvoffset+ (ash 128 shift))
(defconstant +one-half+ (1- (ash 1 (1- shift))))
(defconstant +r-y-off+ 0)
(defconstant +g-y-off+ 256)
(defconstant +b-y-off+ (* 2 256))
(defconstant +r-u-off+ (* 3 256))
(defconstant +g-u-off+ (* 4 256))
(defconstant +b-u-off+ (* 5 256))
(defconstant +r-v-off+ +b-u-off+)
(defconstant +g-v-off+ (* 6 256))
(defconstant +b-v-off+ (* 7 256))

(declaim (type fixnum-array +ctab+ +cr-r-tab+ +cb-g-tab+ +cr-g-tab+ +cb-b-tab+))

;;;Direct color conversion table
(define-constant +ctab+
    (let ((table (make-array 2048 :element-type 'fixnum :initial-element 0)))
      (loop for i fixnum from 0 to 255 do
           (setf (aref table (plus i +r-y-off+))
                 (mul +.299+ i))
           (setf (aref table (plus i +g-y-off+))
                 (mul +.587+ i))
           (setf (aref table (plus i +b-y-off+))
                 (mul +.114+ i))
           (setf (aref table (plus i +r-u-off+))
                 (mul +-.1687+ i))
           (setf (aref table (plus i +g-u-off+))
                 (mul +-.3313+ i))
           (setf (aref table (plus i +b-u-off+))
                 (+ (mul +.5+ i) +uvoffset+ +one-half+))
           (setf (aref table (plus i +r-v-off+))
                 (+ (mul +.5+ i) +uvoffset+ +one-half+))
           (setf (aref table (plus i +g-v-off+))
                 (mul +-.4187+ i))
           (setf (aref table (plus i +b-v-off+))
                 (mul +-.0813+ i)))
      table))

;;; Constantsants for the inverse colorspace conversion
(defconstant +1.40200+ (round (+ (* 1.40200 (ash 1 shift)) 0.5)))
(defconstant +1.77200+ (round (+ (* 1.77200 (ash 1 shift)) 0.5)))
(defconstant +-0.71414+ (round (+ (* -0.71414 (ash 1 shift)) 0.5)))
(defconstant +-0.34414+ (round (+ (* -0.34414 (ash 1 shift)) 0.5)))

;;; Inverse color conversion tables
(define-constant +cr-r-tab+ (make-array 256
                                        :element-type 'fixnum
                                        :initial-contents
                                        (loop for i from 0 to 255
                                              for x from -127
                                              collect (ash (plus (mul +1.40200+ x) +one-half+) (- shift)))))
(define-constant +cb-g-tab+ (make-array 256
                                        :element-type 'fixnum
                                        :initial-contents
                                        (loop for i from 0 to 255
                                              for x from -127
                                              collect (plus (mul +-0.34414+ x) +one-half+))))
(define-constant +cr-g-tab+ (make-array 256
                                        :element-type 'fixnum
                                        :initial-contents
                                        (loop for i from 0 to 255
                                              for x from -127
                                              collect (mul +-0.71414+ x))))
(define-constant +cb-b-tab+ (make-array 256
                                        :element-type 'fixnum
                                        :initial-contents
                                        (loop for i from 0 to 255
                                              for x from -127
                                              collect (ash (plus (mul +1.77200+ x) +one-half+) (- shift)))))

;;; Constants for LLM DCT
(defconstant dct-shift  ; defining DCT scaling
  (if (<= (integer-length most-positive-fixnum) 31)
      (minus 13 (round (minus 31 (integer-length most-positive-fixnum)) 2))
    13))

(defconstant +shift-1+ (1- dct-shift))
(defconstant +shift+1+ (1+ dct-shift))
(defconstant +shift+4+ (+ dct-shift 4))
(defconstant +FIX-0-298631336+ (round (+ (* 0.298631336 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-0-390180644+ (round (+ (* 0.390180644 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-0-541196100+ (round (+ (* 0.541196100 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-0-765366865+ (round (+ (* 0.765366865 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-0-899976223+ (round (+ (* 0.899976223 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-1-175875602+ (round (+ (* 1.175875602 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-1-501321110+ (round (+ (* 1.501321110 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-1-847759065+ (round (+ (* 1.847759065 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-1-961570560+ (round (+ (* 1.961570560 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-2-053119869+ (round (+ (* 2.053119869 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-2-562915447+ (round (+ (* 2.562915447 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-3-072711026+ (round (+ (* 3.072711026 (ash 1 dct-shift)) 0.5)))

(declaim (type uint8-array *idct-limit-array*))
;;; Post-IDCT limiting array
(defvar *idct-limit-array* (make-array 512 :initial-element 0 :element-type 'uint8))
(loop for n from 0
      for i from 128 to 383 do
      (setf (aref *idct-limit-array* i) n))
(loop for i from 384 to 511 do
      (setf (aref *idct-limit-array* i) 255))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error conditions

(define-condition jpeg-error (error)
  ())

(define-condition jpeg-encoder-error (jpeg-error)
  ())

(define-condition internal-jpeg-encoder-error (jpeg-encoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Internal error"))))

(define-condition illegal-number-of-components (jpeg-encoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Illegal number of components specified"))))

(define-condition invalid-sampling-list (jpeg-encoder-error)
  ((components :reader components :initarg :ncomp))
  (:report (lambda (condition stream)
	     (format stream "Wrong sampling list for ~D component(s)" (components condition)))))

(define-condition invalid-quantization-tables (jpeg-encoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Too many quantization tables specified"))))

(define-condition invalid-q-factor (jpeg-encoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Invalid Q factor!"))))

(define-condition invalid-sampling (jpeg-encoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Invalid sampling specification!"))))

(define-condition jpeg-decoder-error (jpeg-error)
  ())

(define-condition unsupported-jpeg-frame-marker (jpeg-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Unsupported marker in the frame header"))))

(define-condition unsupported-jpeg-format (jpeg-decoder-error) 
  ((code :reader marker-code :initarg :code))
  (:report (lambda (condition stream)
	     (format stream "Unsupported JPEG format: ~X" (marker-code condition)))))

(define-condition unrecognized-file-format (jpeg-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Unrecognized JPEG format"))))

(define-condition invalid-buffer-supplied (jpeg-decoder-error)
  ((buffer :reader buffer :initarg :buffer))
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Invalid buffer supplied: ~A" (buffer condition)))))

(define-condition unsupported-arithmetic-encoding (jpeg-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Arithmetic encoding not supported"))))

(define-condition unsupported-dnl-marker (jpeg-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "DNL marker is not supported"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Encoder part

;;; Subsamples inbuf into outbuf
(defun subsample (inbuf outbuf H V xlim ylim iH iV)
  (declare #.*optimize*
           (type fixnum H V xlim ylim iV iH)
           (type sint16-2d-array inbuf)
	   (type (simple-array sint16-2d-array (*)) outbuf))
  (loop for by fixnum from 0 below V do
        (loop for bx fixnum from 0 below H
              for block = (aref outbuf (plus bx (mul by H))) do
              (loop for y fixnum from (the fixnum (ash by 3)) by iV
                    for yp fixnum from 0 to 7 do
                    (loop for x fixnum from (the fixnum (ash bx 3)) by iH
                          for xp fixnum from 0 to 7 do
                          (setf (s16ref block xp yp)
                                (the sint16 (cond ((and (<= x xlim) (<= y ylim))
                                                   (s16ref inbuf x y))
                                                  ((and (> x xlim) (> y ylim))
                                                   (s16ref inbuf xlim ylim))
                                                  ((> x xlim)
                                                   (s16ref inbuf xlim y))
                                                  ((> y ylim)
                                                   (s16ref inbuf x ylim))
                                                  (t
                                                   (error 'internal-jpeg-encoder-error))
                                                   ))))))))

;;; Returns sum of Vi*Hi
(defun count-relation (smp)
  (loop for entry in smp
        summing (mul (first entry) (second entry))))

;;; Cutting specified part of image (used for non-RGB images)
(defun crop-image (inbuf outbuf dx dy h w height width ncomp)
  (let ((xend (plus dx (1- width)))
        (yend (plus dy (1- height))))
    (declare #.*optimize*
             (type fixnum dx dy h w height width ncomp xend yend)
	     (type simple-array inbuf)
	     (type (simple-array sint16-2d-array (*)) outbuf))
    (setf xend (min xend (1- w)))
    (setf yend (min yend (1- h)))
    (loop for yd fixnum from dy to yend
          for ypos fixnum = (* w yd ncomp) do
          (loop for xd fixnum from dx to xend
                for pos fixnum = (plus (mul xd ncomp) ypos)
                for cx fixnum = (minus xd dx)
                for cy fixnum = (minus yd dy) do
                (loop for i fixnum from 0 below ncomp do
                      (setf (s16ref (aref outbuf i) cx cy)
                            (minus (aref inbuf (plus pos i)) 128)))))
    (values xend yend)))

;;; Direct color mapping
(defun colorspace-convert (RGB YUV dx dy h w height width)
  (let ((xend (plus dx (1- width)))
        (yend (plus dy (1- height)))
        (Y (aref YUV 0))
        (U (aref YUV 1))
        (V (aref YUV 2)))
    (declare #.*optimize*
             (type fixnum dx dy h w height width xend yend)
	     (type sint16-2d-array Y U V)
	     (type fixnum-array +ctab+)
             (type uint8-array RGB))
    (setf xend (min xend (1- w)))
    (setf yend (min yend (1- h)))
    (loop for yd fixnum from dy to yend
          for ypos fixnum = (mul3 w yd 3) do
          (loop for xd fixnum from dx to xend
                for pos fixnum = (plus (mul xd 3) ypos)
                for r fixnum = (aref rgb (plus pos 2))
                for g fixnum = (aref rgb (1+ pos))
                for b fixnum = (aref rgb pos)
                for cx fixnum = (minus xd dx)
                for cy fixnum = (minus yd dy) do
	       (setf (s16ref Y cx cy) (minus (ash (the fixnum (+ (aref +ctab+ (plus r +r-y-off+))
								 (aref +ctab+ (plus g +g-y-off+))
								 (aref +ctab+ (plus b +b-y-off+))))
						  (- shift))
                                             128))
                (setf (s16ref U cx cy) (minus (ash (the fixnum (+ (aref +ctab+ (plus r +r-u-off+))
								  (aref +ctab+ (plus g +g-u-off+))
								  (aref +ctab+ (plus b +b-u-off+))))
						   (- shift))
                                             128))
	       (setf (s16ref V cx cy) (minus (ash (the fixnum (+ (aref +ctab+ (plus r +r-v-off+))
								 (aref +ctab+ (plus g +g-v-off+))
								 (aref +ctab+ (plus b +b-v-off+))))
						  (- shift))
                                             128))))
    (values xend yend)))

;;; Converts given image sampling into frequencies of pixels of components
(defun convert-sampling (s Hmax Vmax)
  (declare (type fixnum Hmax Vmax))
  (make-array (length s)
              :initial-contents (loop for entry in s
                                      collecting (list (the fixnum (/ Hmax (first entry)))
                                                       (the fixnum (/ Vmax (second entry)))))))

;;; Quantization (also removes factor of 8 after DCT)

(defmacro quantize-block ()
  (if *quantize-optimization*
      '(loop for block-row across block
             for q-row across q-table do
             (loop for x fixnum from 0 to 7
                   for val fixnum = (ash (aref block-row x) -3)
                   for qc fixnum = (aref q-row x) do
                   (setf (aref block-row x) (the fixnum (round val qc)))))
    '(loop for block-row across block
           for q-row across q-table do
           (loop for x fixnum from 0 to 7
                 for val fixnum = (ash (aref block-row x) -3)
                 for absval fixnum = (abs val)
                 for qc fixnum = (aref q-row x) do
                 (cond ((< absval (ash qc -1))
                        ;; you won't believe, but under LWW 4.1 such ugly hack gives
                        ;; very sufficient speedup
                        (setf (aref block-row x) 0))
                       ((<= absval qc)
                        (if (minusp val)
                            (setf (aref block-row x) -1)
                         (setf (aref block-row x) 1)))
                       ((<= (ash absval -1) qc)
                        (if (zerop (logand absval 1))
                            (if (minusp val)
                                (setf (aref block-row x) -1)
                              (setf (aref block-row x) 1))
                          (if (minusp val)
                              (setf (aref block-row x) -2)
                            (setf (aref block-row x) 2))))
                       (t
                        (setf (aref block-row x) (the fixnum (round val qc)))))))))

(defun quantize (block q-table)
  (declare #.*optimize*
	   (type sint16-2d-array block)
	   (type uint8-2d-array q-table))
  (quantize-block))

;;; LLM DCT aux definitions
(defun descale (x n)
  (declare #.*optimize* (type fixnum x n))
  (the fixnum (ash (plus x (ash 1 (1- n))) (- n))))

;;; Implementation of Loeffer, Ligtenberg and Moschytz forward DCT
(defun llm-dct (data)
  (declare #.*optimize* (type sint16-2d-array data))
  (loop with tmp0 fixnum and tmp1 fixnum and tmp2 fixnum
        and tmp3 fixnum and tmp4 fixnum and tmp5 fixnum
        and tmp6 fixnum and tmp7 fixnum and tmp10 fixnum
        and tmp11 fixnum and tmp12 fixnum and tmp13 fixnum
        and z1 fixnum and z2 fixnum and z3 fixnum
        and z4 fixnum and z5 fixnum do
        (loop for dptr across data do   ; iterating over rows
              (setf tmp0 (plus (aref dptr 0) (aref dptr 7)))
              (setf tmp7 (minus (aref dptr 0) (aref dptr 7)))
              (setf tmp1 (plus (aref dptr 1) (aref dptr 6)))
              (setf tmp6 (minus (aref dptr 1) (aref dptr 6)))
              (setf tmp2 (plus (aref dptr 2) (aref dptr 5)))
              (setf tmp5 (minus (aref dptr 2) (aref dptr 5)))
              (setf tmp3 (plus (aref dptr 3) (aref dptr 4)))
              (setf tmp4 (minus (aref dptr 3) (aref dptr 4)))
              (setf tmp10 (plus tmp0 tmp3))
              (setf tmp13 (minus tmp0 tmp3))
              (setf tmp11 (plus tmp1 tmp2))
              (setf tmp12 (minus tmp1 tmp2))
              (setf (aref dptr 0) (ash (plus tmp10 tmp11) 1))
              (setf (aref dptr 4) (ash (minus tmp10 tmp11) 1))
              (setf z1 (mul (plus tmp12 tmp13) +FIX-0-541196100+))
              (setf (aref dptr 2) (descale (plus z1 (mul tmp13 +FIX-0-765366865+)) +shift-1+))
              (setf (aref dptr 6) (descale (plus z1 (mul tmp12 (- +FIX-1-847759065+))) +shift-1+))
              (setf z1 (plus tmp4 tmp7))
              (setf z2 (plus tmp5 tmp6))
              (setf z3 (plus tmp4 tmp6))
              (setf z4 (plus tmp5 tmp7))
              (setf z5 (mul (plus z3 z4) +FIX-1-175875602+))
              (setf tmp4 (mul tmp4 +fix-0-298631336+))
              (setf tmp5 (mul tmp5 +fix-2-053119869+))
              (setf tmp6 (mul tmp6 +fix-3-072711026+))
              (setf tmp7 (mul tmp7 +fix-1-501321110+))
              (setf z1 (mul z1 (- +fix-0-899976223+)))
              (setf z2 (mul z2 (- +fix-2-562915447+)))
              (setf z3 (mul z3 (- +fix-1-961570560+)))
              (setf z4 (mul z4 (- +fix-0-390180644+)))
              (incf z3 z5)
              (incf z4 z5)
              (setf (aref dptr 7) (descale (plus3 tmp4 z1 z3) +shift-1+))
              (setf (aref dptr 5) (descale (plus3 tmp5 z2 z4) +shift-1+))
              (setf (aref dptr 3) (descale (plus3 tmp6 z2 z3) +shift-1+))
              (setf (aref dptr 1) (descale (plus3 tmp7 z1 z4) +shift-1+)))
        (loop for cnt fixnum from 7 downto 0 do ; second pass: on columns
              (setf tmp0 (plus (s16ref data cnt 0) (s16ref data cnt 7)))
              (setf tmp7 (minus (s16ref data cnt 0) (s16ref data cnt 7)))
              (setf tmp1 (plus (s16ref data cnt 1) (s16ref data cnt 6)))
              (setf tmp6 (minus (s16ref data cnt 1) (s16ref data cnt 6)))
              (setf tmp2 (plus (s16ref data cnt 2) (s16ref data cnt 5)))
              (setf tmp5 (minus (s16ref data cnt 2) (s16ref data cnt 5)))
              (setf tmp3 (plus (s16ref data cnt 3) (s16ref data cnt 4)))
              (setf tmp4 (minus (s16ref data cnt 3) (s16ref data cnt 4)))
              (setf tmp10 (plus tmp0 tmp3))
              (setf tmp13 (minus tmp0 tmp3))
              (setf tmp11 (plus tmp1 tmp2))
              (setf tmp12 (minus tmp1 tmp2))
              (setf (s16ref data cnt 0) (descale (plus tmp10 tmp11) 1))
              (setf (s16ref data cnt 4) (descale (minus tmp10 tmp11) 1))
              (setf z1 (mul (plus tmp12 tmp13) +fix-0-541196100+))
              (setf (s16ref data cnt 2) (descale (plus z1 (mul tmp13 +fix-0-765366865+)) +shift+1+))
              (setf (s16ref data cnt 6) (descale (plus z1 (mul tmp12 (- +fix-1-847759065+))) +shift+1+))
              (setf z1 (plus tmp4 tmp7))
              (setf z2 (plus tmp5 tmp6))
              (setf z3 (plus tmp4 tmp6))
              (setf z4 (plus tmp5 tmp7))
              (setf z5 (mul (plus z3 z4) +fix-1-175875602+))
              (setf tmp4 (mul tmp4 +fix-0-298631336+))
              (setf tmp5 (mul tmp5 +fix-2-053119869+))
              (setf tmp6 (mul tmp6 +fix-3-072711026+))
              (setf tmp7 (mul tmp7 +fix-1-501321110+))
              (setf z1 (mul z1 (- +fix-0-899976223+)))
              (setf z2 (mul z2 (- +fix-2-562915447+)))
              (setf z3 (mul z3 (- +fix-1-961570560+)))
              (setf z4 (mul z4 (- +fix-0-390180644+)))
              (incf z3 z5)
              (incf z4 z5)
              (setf (s16ref data cnt 7) (descale (plus3 tmp4 z1 z3) +shift+1+))
              (setf (s16ref data cnt 5) (descale (plus3 tmp5 z2 z4) +shift+1+))
              (setf (s16ref data cnt 3) (descale (plus3 tmp6 z2 z3) +shift+1+))
              (setf (s16ref data cnt 1) (descale (plus3 tmp7 z1 z4) +shift+1+)))
        (return)))

;;; Forward DCT and quantization
(defun crunch (buf pos table)
  (declare #.*optimize*
           (type fixnum pos)
           (type (simple-array sint16-2d-array (*)) buf))
  (llm-dct (aref buf pos))
  (quantize (aref buf pos) table))

;;; Q-tables scaling
(defun q-scale (table q-factor)
  (declare #.*optimize*
	   (type uint8-2d-array table)
	   (type uint8 q-factor))
  (when (/= q-factor 64)
    (let ((factor (the uint8 (/ q-factor 64))))
      (loop for q-row of-type uint8-array across table do
            (loop for x fixnum from 0 to 7 do
                  (setf (aref q-row x)
                        (the fixnum (round (* (aref q-row x) factor)))))))))

;;; Function that maps value into SSSS
(defun csize (n)
    (declare #.*optimize* (type fixnum n))
    (aref +csize+ (plus n 1023)))

;;; zigzag ordering
(defun zigzag (buffer zz-result)
  (declare #.*optimize*
	   (type sint16-2d-array buffer))
  (loop for row of-type sint16-array across buffer
     for z-row of-type uint8-array across +zigzag-index+ do
       (loop for x fixnum from 0 to 7 do
	    (setf (aref zz-result (aref z-row x))
		  (the sint16 (aref row x)))))
  zz-result)

(defun zigzag8 (buffer zz-result)
  (declare #.*optimize*
           (type uint8-2d-array buffer)
           (type uint8-array zz-result))
  (loop for row of-type uint8-array across buffer
     for z-row of-type uint8-array across +zigzag-index+ do
       (loop for x fixnum from 0 to 7 do
            (setf (aref zz-result (aref z-row x))
                  (the fixnum (aref row x)))))
  zz-result)

;;; Writes frame header
(defun write-frame-header (maxX maxY cn q-tables sampling tqv out-stream)
  (declare #.*optimize* (type fixnum maxX maxY cn)
	   (type (simple-array uint8-2d-array (*)) q-tables)
	   ;;(type uint8-array tqv)
	   )
  (write-huffman-tables out-stream)
  (write-quantization-tables q-tables out-stream)
  ;; writing frame header
  (write-marker +M_SOF0+ out-stream)
  (write-byte 0 out-stream) ; length
  (write-byte (plus 8 (mul 3 cn)) out-stream)
  (write-byte 8 out-stream) ; sample precision
  (write-byte (ash maxY -8) out-stream) ; max height
  (write-byte (logand maxY #xff) out-stream)
  (write-byte (ash maxX -8) out-stream) ; max width
  (write-byte (logand maxX #xff) out-stream)
  (write-byte cn out-stream) ; number of components
  (loop for entry in sampling
        for i fixnum from 0 by 1 do
        (write-byte i out-stream)
        (write-byte         ; H and V
         (deposit-field (second entry) (byte 4 0)(ash (first entry) 4))
         out-stream)
        (write-byte (aref tqv i) out-stream))) ; Tq

;;; Writes byte with stuffing (adds zero after FF code)
(defun write-stuffed (b s)
  (declare #.*optimize* (type uint8 b)
           (type stream s))
   (write-byte b s)
   (if (= b #xFF)
      (write-byte 0 s)))

;;; A function for bit streaming
;;; NB: probably it's a good idea to encapsulate this behavior into a class, but I'm
;;; afraid that method dispatch would be too slow

(defstruct (write-bits-state (:conc-name write-bits-))
  (prev-byte 0 :type uint8)
  (prev-length 0 :type fixnum))

(defun write-bits (bi ni s write-bits-state)
  (declare #.*optimize*
           (type fixnum bi ni)
           (type stream s))
  (loop with lim fixnum = (if (> ni 8) 1 0)
        for i fixnum from lim downto 0 do
        (let ((b (ldb (byte 8 (ash i 3)) bi))
              (n (cond ((and (= i 1) (= ni 16)) 8)
                       ((and (= i 0) (/= lim 0)) 8)
                       ((= ni 8) 8)
                       (t (logand ni 7)))))
          (declare (type fixnum b n)
                   (dynamic-extent b n))
          (cond ((zerop n))
                ((>= (plus n (write-bits-prev-length write-bits-state)) 8)
                 (let* ((result (ash (write-bits-prev-byte write-bits-state)
                                     (minus 8 (write-bits-prev-length write-bits-state))))
                        (total-length (plus n (write-bits-prev-length write-bits-state)))
                        (overflow (minus total-length 8)))
                   (declare (type fixnum overflow total-length result)
                            (dynamic-extent overflow total-length result))
                   (setf (write-bits-prev-byte write-bits-state) (ldb (byte overflow 0) b))
                   (write-stuffed (deposit-field
                                   (ldb (byte (minus n overflow) overflow) b)
                                   (byte (minus 8 (write-bits-prev-length write-bits-state)) 0)
                                   result)
                                  s)
                   (setf (write-bits-prev-length write-bits-state) overflow)))
                (t  (setf (write-bits-prev-byte write-bits-state)
                          (deposit-field b
                                         (byte n 0)
                                         (ash (write-bits-prev-byte write-bits-state) n)))
                    (incf (write-bits-prev-length write-bits-state) n))))))

;;; Encodes block using specified huffman tables, returns new pred (DC prediction value)
;;; and last code written to stream for padding
(defun encode-block (block tables pred s write-bits-state)
  (declare #.*optimize* (type fixnum pred)
           (type sint16-array block))
  (let* ((ehufsi-dc (first (first tables)))
         (ehufco-dc (second (first tables)))
         (ehufsi-ac (first (second tables)))
         (ehufco-ac (second (second tables)))
         (newpred (aref block 0))
         (diff (minus newpred pred))
         (dcpos (csize diff)))
    (declare (type fixnum pred newpred diff dcpos)
	     (type fixnum-array ehufco-ac ehufco-dc ehufsi-dc ehufsi-ac)
             (dynamic-extent diff dcpos))
    ;; writing dc code first
    (write-bits (aref ehufco-dc dcpos) (aref ehufsi-dc dcpos) s write-bits-state)
    (cond ((minusp diff) (write-bits (1- diff) (csize diff) s write-bits-state))
          (t (write-bits diff (csize diff) s write-bits-state)))
    ;; writing ac sequence
    (loop with r fixnum = 0 for k fixnum from 1 to 63 do
          (if (zerop (aref block k))
              (if (= k 63)
                  (progn
                    (write-bits (aref ehufco-ac 0) (aref ehufsi-ac 0) s write-bits-state) ; writing EOB
                    (return))
                (incf r))
            (progn
              (loop while (> r 15) do
                    (write-bits (aref ehufco-ac #xf0) (aref ehufsi-ac #xf0) s write-bits-state)
                    (decf r 16))
              (let* ((ssss (csize (aref block k)))
                     (rs (plus ssss (ash r 4))))
                (write-bits (aref ehufco-ac rs) (aref ehufsi-ac rs) s write-bits-state)
                (when (minusp (aref block k))
                  (decf (aref block k) 1))
                (write-bits (aref block k) ssss s write-bits-state))
              (setf r 0))))
    newpred))

;;; Emits q-tables
(defun write-quantization-tables (tables s)
  (let ((len (plus 2 (mul 65 (length tables))))
        (zz-result (make-array 64 :element-type 'uint8)))
    (write-marker +M_DQT+ s)
    (write-byte (ash len -8) s) ; MSB
    (write-byte (logand len #xff) s) ; LSB
    (loop for table across tables
          for i fixnum from 0 do
          (write-byte i s)
          (write-sequence (zigzag8 table zz-result) s))))

;;; Emits huffman tables in the following order:
;;; luminance DC
;;; luminance AC
;;; chrominance DC
;;; chrominance AC
(defun write-huffman-tables (s)
  (let ((len (+ 2 (* 17 4)
                (length +luminance-dc-values+)
                (length +luminance-ac-values+)
                (length +chrominance-dc-values+)
                (length +chrominance-ac-values+))))
    (write-marker +M_DHT+ s)
    (write-byte (ash len -8) s) ; MSB
    (write-byte (logand len #xff) s) ; LSB
    (write-hufftable +luminance-dc-bits+ +luminance-dc-values+ 0 s)
    (write-hufftable +luminance-ac-bits+ +luminance-ac-values+ 16 s)
    (write-hufftable +chrominance-dc-bits+ +chrominance-dc-values+ 1 s)
    (write-hufftable +chrominance-ac-bits+ +chrominance-ac-values+ 17 s)))

;;; Writes single huffman table
(defun write-hufftable (bits vals tcti s)
  (declare (type fixnum tcti))
    (write-byte tcti s) ; Tc/Th
    (write-sequence bits s)
    (write-sequence vals s))

;;; Drops specified marker into the stream
(defun write-marker (code to)
   (write-byte #xFF to)
   (write-byte code to))

;;; Writing some markers into the stream
(defun prepare-JFIF-stream (out-stream)
   (write-marker +M_SOI+ out-stream)
   (write-marker +M_APP0+ out-stream)
   (write-byte 0 out-stream) ; length
   (write-byte 16 out-stream)
   (write-byte #x4a out-stream)
   (write-byte #x46 out-stream)
   (write-byte #x49 out-stream)
   (write-byte #x46 out-stream)
   (write-byte 0 out-stream)
   (write-byte 1 out-stream) ; version
   (write-byte 2 out-stream)
   (write-byte 0 out-stream) ; units
   (write-byte 0 out-stream) ; density
   (write-byte 1 out-stream)
   (write-byte 0 out-stream)
   (write-byte 1 out-stream)
   (write-byte 0 out-stream) ; thumbnail
   (write-byte 0 out-stream))

;;; Builds common decoding and encoding tables
(defun build-universal-tables (bits)
  (let ((huffsize (make-array 256 :element-type 'fixnum))
        (huffcode (make-array 256 :element-type 'fixnum))
        (lastk 0))
    (declare #.*optimize* (type fixnum lastk)
             (type fixnum-array huffcode huffsize)
	     (type uint8-array bits))
    ;; generating huffsize
      (loop for i fixnum from 1 to 16
            with k fixnum = 0 and j fixnum = 1 do
            (loop until (> j (aref bits (1- i))) do
                  (setf (aref huffsize k) i)
                  (incf k)
                  (incf j)
                  finally (setf j 1))
            finally (progn (setf lastk k) (setf (aref huffsize lastk) 0)))

      ;; generating huffcode
      (loop with k fixnum = 0 and code fixnum = 0 and si fixnum = (aref huffsize 0) do
            (loop do
                  (setf (aref huffcode k) code)
                  (incf code)
                  (incf k)
                  when (/= (aref huffsize k) si) do (return))
            when (zerop (aref huffsize k)) do
            (return)
            else do
            (loop do
                  (setf code (ash code 1))
                  (incf si)
                  when (= (aref huffsize k) si) do (return)))
      (values huffcode huffsize lastk)))

;;;Builds ordered code tables for encoder
(defun build-tables (bits huffval)
  (let ((ehufco (make-array 256 :element-type 'fixnum))
        (ehufsi (make-array 256 :element-type 'fixnum)))
    (multiple-value-bind (huffcode huffsize lastk)
        (build-universal-tables bits)
      (declare (type fixnum-array huffsize huffcode)
               (type fixnum lastk))
      (loop with i fixnum for k from 0 below lastk do
            (setf i (aref huffval k))
            (setf (aref ehufco i) (aref huffcode k))
            (setf (aref ehufsi i) (aref huffsize k)))
      (list ehufsi ehufco))))

;;; Main encoder function (user interface)
(defun encode-image-stream (out-stream image ncomp h w
                            &key (q-tabs +q-tables+) (sampling '((2 2)(1 1)(1 1))) (q-factor 64))
  (declare #.*optimize*
           (type fixnum ncomp h w q-factor)
           (type uint8-array image))
  (when (= ncomp 1)
    (setq sampling '((1 1))))
  (let* ((wd (loop for entry in sampling maximize (the fixnum (first entry))))
         (ht (loop for entry in sampling maximize (the fixnum (second entry))))
	 (zz-result (make-array 64 :element-type 'sint16))
         (write-bits-state (make-write-bits-state))
         (isampling (convert-sampling sampling wd ht))
         (height (the fixnum (ash ht 3)))
         (width (the fixnum (ash wd 3)))
         (YUV (make-array ncomp
			  :element-type 'sint16-2d-array
                          :initial-contents
                          (loop for i fixnum from 0 below ncomp collecting
                               (make-array height
					   :element-type 'sint16-array
                                           :initial-contents
                                           (loop for j fixnum from 0 below height
                                              collecting (make-array width :element-type 'sint16))))))
         (sampled-buf (make-array (mul ht wd)
				  :element-type 'sint16-2d-array
                                  :initial-contents
                                  (loop for b fixnum from 0 below (mul ht wd)
                                     collecting (make-array 8
							    :element-type 'sint16-array
                                                            :initial-contents
                                                            (loop for i fixnum from 0 to 7
                                                               collecting (make-array 8 :element-type 'sint16))))))
         (preds (make-array ncomp :initial-element 0))
         (tqv (case ncomp
                (3 #(0 1 1)) ; q-tables destinations for various component numbers
                (1 #(0))
                (2 #(0 1))
                (4 #(0 1 2 3))
                (otherwise (error 'illegal-number-of-components)))))
    (declare (type (simple-array sint16-2d-array (*)) YUV sampled-buf))
    (cond ((/= ncomp (length sampling))
           (error 'invalid-sampling-list :ncomp ncomp))
          ((> (length q-tabs) ncomp)
           (error 'invalid-quantization-tables))
          ((zerop q-factor)
           (error 'invalid-q-factor))
          ((> (count-relation sampling) 10)
           (error 'invalid-sampling)))
    (when (< q-factor 64)
      (let ((q-tabs2 (make-array (length q-tabs)
				 :element-type 'uint8-2d-array
                                 :initial-contents
                                 (loop for k fixnum from 0 below (length q-tabs)
                                    collecting (make-array 8 :element-type 'uint8-array
							   :initial-contents
                                                           (loop for i fixnum from 0 to 7
                                                              collecting (make-array 8 :element-type 'uint8)))))))
	(declare (type (simple-array uint8-2d-array (*)) q-tabs2))
        (loop for entry across q-tabs
           for entry2 across q-tabs2 do
           (loop for x fixnum from 0 to 7 do
                (loop for y fixnum from 0 to 7 do
                     (setf (u8ref entry2 x y) (the fixnum (u8ref entry x y))))))
        (setq q-tabs q-tabs2))
      (loop for entry across q-tabs do  ; scaling all q-tables
           (q-scale entry q-factor)))
    (if (and (/= ncomp 1) (/= ncomp 3))
        (write-marker +M_SOI+ out-stream)
        (prepare-JFIF-stream out-stream))
    (write-frame-header w h ncomp q-tabs sampling tqv out-stream) ; frame header
    ;; writing scan header
    (write-marker +M_SOS+ out-stream)
    (write-byte 0 out-stream)           ; length
    (write-byte (plus 6 (ash ncomp 1)) out-stream)
    (write-byte ncomp out-stream)    ; number of components in the scan
    (loop for Cj from 0 below ncomp do
         (write-byte Cj out-stream)     ; component ID
         (write-byte (if (zerop Cj) 0 17) out-stream)) ; TdTa
    (write-byte 0 out-stream)                          ; Ss
    (write-byte 63 out-stream)                         ; Se
    (write-byte 0 out-stream)                          ; AhAl

    (let ((luminance-tabset (list
                             (build-tables +luminance-dc-bits+ +luminance-dc-values+)
                             (build-tables +luminance-ac-bits+ +luminance-ac-values+)))
          (chrominance-tabset (list (build-tables +chrominance-dc-bits+ +chrominance-dc-values+)
                                    (build-tables +chrominance-ac-bits+ +chrominance-ac-values+))))
      (loop for dy fixnum from 0 below h by height do
           (loop for dx fixnum from 0 below w by width do
                (multiple-value-bind (xlim ylim)
                    (if (= ncomp 3)
                       (colorspace-convert image YUV dx dy h w height width)
                        (crop-image image YUV dx dy h w height width ncomp))
                  (declare (type fixnum xlim ylim)
                           (dynamic-extent xlim ylim))
                  (loop for comp across YUV
                     for freq in sampling
                     for ifreq across isampling
                     for iH fixnum = (first ifreq)
                     for iV fixnum = (second ifreq)
                     for cn fixnum from 0
                     for hufftabs = (if (zerop cn)
                                        luminance-tabset
                                        chrominance-tabset)
                     ;; choosing appropriate q-table for a component
                     for q-tab = (aref q-tabs (aref tqv cn))
                     for H fixnum = (first freq)
                     for V fixnum = (second freq) do
                     (subsample comp sampled-buf H V (minus xlim dx) (minus ylim dy) iH iV)
                     (loop for y fixnum from 0 below V
                        for ypos fixnum = (if (> (plus dy (ash y 3)) ylim)
                                              (mul (rem (ash ylim -3) V) H)
                                              (mul y H)) do
                        (loop for x fixnum from 0 below H
                           for pos fixnum = (if (> (plus dx (ash x 3)) xlim)
                                                (plus (rem (ash xlim -3) H) ypos)
                                                (plus x ypos)) do
                           (crunch sampled-buf pos q-tab)
                           (setf (aref preds cn)
                                 (encode-block (zigzag (aref sampled-buf pos) zz-result)
                                               hufftabs (aref preds cn) out-stream
                                               write-bits-state)))))))))
    (unless (zerop (write-bits-prev-length write-bits-state))
      (write-stuffed (deposit-field #xff ; byte padding & flushing
                                    (byte (minus 8 (write-bits-prev-length write-bits-state)) 0)
                                    (the fixnum (ash (the uint8 (write-bits-prev-byte write-bits-state))
                                                     (the uint8 (minus 8 (write-bits-prev-length write-bits-state))))))
                     out-stream))
    (write-marker +M_EOI+ out-stream)))

(defun encode-image (filename image ncomp h w &rest args)
  (with-open-file (out-stream filename
                              :direction :output
                              :element-type 'uint8
                              :if-exists :supersede)
    (apply #'encode-image-stream out-stream image ncomp h w args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Decoder part

;;; Contains all information about a single scan
(defstruct scan
  (ncomp 1 :type fixnum) ; number of image components in the scan
  (x 0 :type fixnum) ; current processing
  (y 0 :type fixnum) ; positions
  (cdesc (make-array 4
                     :initial-contents (loop repeat 4 collect (list 0 0)))
         :type (simple-array t (*)))) ; descriptors of all components in the scan

;;; Contains huffman decoding tables
(defstruct huffstruct
  mincode
  maxcode
  (bits (make-array 16 :element-type 'uint8) :type uint8-array)
  huffval
  huffcode
  valptr)

;;; This structure contains all neccessary information about the decoded image
(defstruct descriptor
  (restart-interval 0 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  buffer
  (qtables
   (make-array 4 :initial-contents
               (loop for j fixnum from 0 to 3
                     collecting
                        (make-array 8 :initial-contents
                                    (loop for i fixnum from 0 to 7
                                          collecting (make-array 8 :element-type 'uint8))))) :type (simple-array uint8-2d-array (*)))
  (huff-ac (make-array 2 :initial-contents
                       (list (make-huffstruct) (make-huffstruct))) :type (simple-array t (*)))
  (huff-dc (make-array 2 :initial-contents
                       (list (make-huffstruct) (make-huffstruct))) :type (simple-array t (*)))
  (cid (make-array 4) :type (simple-array t (*)))
  (scans (make-array 4 :initial-contents
                     (loop for i fixnum from 0 to 3 collecting (make-scan))) :type (simple-array t (*)))
  (H (make-array 4) :type (simple-array t (*)))
  (V (make-array 4) :type (simple-array t (*)))
  (iH (make-array 4) :type (simple-array t (*)))
  (iV (make-array 4) :type (simple-array t (*)))
  (qdest (make-array 4) :type (simple-array t (*)))
  (zz (make-array 64 :element-type 'sint16) :type sint16-array)
  (ncomp 0 :type fixnum)
  (ws (2d-sint16-array
       '(0  0  0  0  0  0  0  0)
       '(0  0  0  0  0  0  0  0)
       '(0  0  0  0  0  0  0  0)
       '(0  0  0  0  0  0  0  0)
       '(0  0  0  0  0  0  0  0)
       '(0  0  0  0  0  0  0  0)
       '(0  0  0  0  0  0  0  0)
       '(0  0  0  0  0  0  0  0))
      :type sint16-2d-array)	; Temporary workspace for IDCT
  (byte-reader #'(lambda () ;in case setup fails somehow
		   (error 'jpeg-decoder-error)) :type function)
  (source-cache)
  (adobe-app14-transform nil))

(defun read-jpeg-byte (image)
  (declare #.*optimize*)
  (the uint8 (funcall (descriptor-byte-reader image))))

;;; Reads an JPEG marker from the stream
(defun read-marker (s)
  (loop for b fixnum = (read-jpeg-byte s)
        when (/= b #xff) do (return b)))

;;; Reads 16-bit word from the stream
(defun read-word (s)
  "Reads 16-bit word from the stream"
  (let* ((msb (ash (read-jpeg-byte s) 8))
         (lsb (read-jpeg-byte s))
         (word (logior msb lsb)))
    word))

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; APPn marker reading: just skipping the whole marker
  (defun read-app (s)
    "APPn marker reading: just skipping the whole marker"
    (loop for i fixnum from 0 below (minus (read-word s) 2) do
         (read-jpeg-byte s)))

;;; COM marker reading, same as read-app
  (setf (symbol-function 'read-com) #'read-app))

;;; Sets up restart interval
(defun read-dri (image)
  "Reads restart interval"
  (read-jpeg-byte image) ; skipping length
  (read-jpeg-byte image)
  (setf (descriptor-restart-interval image) (read-word image)))

;;; 'Inverse zigzag transform'
(defun izigzag (inbuf zzbuf)
  (declare #.*optimize*
           (type sint16-array inbuf)
	   (type sint16-2d-array zzbuf)
	   (type uint8-2d-array +zigzag-index+))
  "Performs inverse zigzag block arrangement"
  (loop for zrow of-type uint8-array across +zigzag-index+
        for row of-type sint16-array across zzbuf do
        (loop for pos fixnum across zrow
              for x fixnum from 0 do
              (setf (aref row x) (aref inbuf pos))))
  zzbuf)

(defun izigzag8 (inbuf zzbuf)
  (declare #.*optimize*
           (type uint8-array inbuf)
	   (type uint8-2d-array zzbuf +zigzag-index+))
  "Performs inverse zigzag block arrangement"
  (loop for zrow of-type uint8-array across +zigzag-index+
        for row of-type uint8-array across zzbuf do
        (loop for pos fixnum across zrow
              for x fixnum from 0 do
              (setf (aref row x) (aref inbuf pos))))
  zzbuf)

;;; Reads in quantization tables
(defun read-dqt (image)
  "Reads in quantization tables"
  (let ((len (minus (read-word image) 2)))
    (loop for i fixnum from (1- len) downto 0 by 65
          for tq fixnum = (logand (read-jpeg-byte image) 7)
          with intable of-type uint8-array = (make-array 64 :element-type 'uint8)
          for table = (aref (descriptor-qtables image) tq) do
          (loop for pos from 0 to 63 do
                (setf (aref intable pos) (read-jpeg-byte image)))
          (izigzag8 intable table))))

;;; Builds up decoder tables
(defun build-decoder-tables (bits huffcode)
  "Builds up decoder-specific tables"
  (let ((maxcode (make-array 17 :element-type 'fixnum))
        (mincode (make-array 17 :element-type 'fixnum))
        (valptr (make-array 17 :element-type 'fixnum)))
    (loop with i fixnum = 0
          and j fixnum = 0 do
          (when (loop
                 (incf i)
                 (when (> i 16)
                   (return t))
                 (cond ((zerop (aref bits (1- i)))
                        (setf (aref maxcode i) -1))
                       (t (return nil))))
            (return (values maxcode mincode valptr)))
          (setf (aref valptr i) j)
          (setf (aref mincode i) (aref huffcode j))
          (incf j (1- (aref bits (1- i))))
          (setf (aref maxcode i) (aref huffcode j))
          (incf j))))

;;; Loads huffman tables
(defun read-dht (image)
  "Loads huffman tables on specified destinations"
  (let ((len (minus (read-word image) 2))
        (count 0))
    (loop for tcth fixnum = (read-jpeg-byte image)
          for tc fixnum = (ash tcth -4)
          for th fixnum = (logand tcth 15)
          for tables = (if (zerop tc)
                           (aref (descriptor-huff-ac image) th)
                         (aref (descriptor-huff-dc image) th))
          for bits = (huffstruct-bits tables)
          for sum fixnum = 0 do
          (loop for i fixnum from 0 to 15
                for entry fixnum = (read-jpeg-byte image) do
                (incf sum entry)
                (setf (aref bits i) entry))
          (setf (huffstruct-huffval tables)
                (make-array sum :element-type 'uint8
			    :initial-contents (loop for i fixnum from 0 below sum
                                                        collecting (read-jpeg-byte image))))
          (incf count (plus sum 17))
	 (multiple-value-bind (maxcode mincode valptr)
              (build-decoder-tables bits (setf (huffstruct-huffcode tables)
                                               (build-universal-tables bits)))
            ;;(declare (type uint8-array mincode valptr))
            (setf (huffstruct-maxcode tables) maxcode)
            (setf (huffstruct-mincode tables) mincode)
            (setf (huffstruct-valptr tables) valptr))
          (unless (< count len) (return t)))))

;;; APP14 is assumed to be Adobe proprietary extension marker
;;; We parse it to figure out if some special color transform is necessary
(defun read-app14 (image)
  (let ((length (minus (read-word image) 2)))
    (loop repeat 11 do (read-jpeg-byte image))
    (setf (descriptor-adobe-app14-transform image)
	  (case (read-jpeg-byte image)
	    (0 :unknown)
	    (1 :ycbcr-rgb)
	    (2 :ycck-cmyk)
	    (otherwise :invalid)))
    (loop repeat (- length 12) do (read-jpeg-byte image))))

;;; Reads tables etc., returns the first unrecognized marker it met
(defun interpret-markers (image term)
  "Reads tables etc., returns the first unrecognized marker it met"
  (loop for mk fixnum = (cond ((zerop term) (read-marker image))
                              (t term)) do
       (setf term 0)
       (cond ((= mk +M_APP14+) (read-app14 image)) ;Adobe marker
	     ((= #xe0 (logand #xf0 mk)) ; Unrecognized APPn marker
	      (read-app image))
	     (t (cond ((= mk +M_DAC+) (error 'unsupported-arithmetic-encoding))
		      ((= mk +M_DRI+) (read-dri image))
		      ((= mk +M_DHT+) (read-dht image))
		      ((= mk +M_DQT+) (read-dqt image))
		      ((= mk +M_COM+) (read-com image))		      
		      (t (return mk)))))))

;;; EXTEND procedure, as described in the standard
(defun extend (v tt)
  "EXTEND procedure, as described in spec."
  (let ((vt (if (> tt 0)
                (ash 1 (the (and fixnum (integer 0)) (minus tt 1)))
                0)))
    (declare (type fixnum v vt tt)
             #.*optimize*)
    (if (< v vt)
        (plus v (plus 1 (ash -1 (the (and fixnum (integer 0)) tt))))
      v)))

;;; Returns the closure which reads specified numbers of bits from the stream
(defun make-nextbit (b cnt)
  "Returns the closure which reads specified numbers of bits from the stream"
  #'(lambda (s)
      (let ((bit 0))
        (declare #.*optimize*
                 (type fixnum b cnt bit))
        (when (zerop cnt)
          (setf b (read-jpeg-byte s))
          (setf cnt 8)
          (when (= b #xff)
            (let ((b2 (read-jpeg-byte s)))
              (declare (type uint8 b2))
              (cond ((zerop b2))
                    ((<= +M_RST0+ b2 +M_RST7+)
                     (throw 'marker 'restart))
                    ((= b2 +M_DNL+)
                     (error 'unsupported-dnl-marker))
                    (t (throw 'marker b2))))))
        (decf cnt)
        (setf bit (ash (logand b 255) -7))
        (setf b (ash b 1))
        bit)))

;;; The DECODE procedure
(defun decode (maxcode mincode valptr huffval nextbit s)
  "The DECODE procedure, as described in CCITT rec."
  (let ((i 1)
        (code (funcall nextbit s)))
    (declare #.*optimize*
             (type (simple-array uint8 (*)) huffval)
             (type (simple-array fixnum (*)) maxcode mincode valptr)
             (type fixnum i code)
             (type function nextbit))
    (loop while (> code (aref maxcode i)) do
          (incf i)
          (setf code (plus (ash code 1) (funcall nextbit s))))
    (aref huffval (plus (aref valptr i) (minus code (aref mincode i))))))

;;; Recieves ssss bits from the stream
(defun recieve (ssss nextbit s)
  "Recieves ssss bits from the stream"
  (let ((v 0))
    (declare #.*optimize*
             (type fixnum v ssss)
             (type function nextbit))
    (loop for i fixnum from 0
          until (= i ssss) do
          (setf v (plus (ash v 1) (funcall nextbit s))))
    v))

;;; Decodes AC coefficients
(defun decode-ac (zz maxcode mincode valptr huffval nextbit s)
  "Decodes AC coefficients"
  (declare #.*optimize*
           (type (simple-array uint8 (*)) huffval))
  (fill zz 0 :start 1)
  (loop with k fixnum = 1
        for rs fixnum = (decode maxcode mincode valptr huffval nextbit s)
        for ssss fixnum = (logand rs 15)
        for r fixnum = (ash rs -4) do
        (cond ((zerop ssss)
               (if (= r 15)
                   (incf k 16)
                 (return zz)))
              (t (incf k r)
                 (setf (aref zz k)
                       (extend (recieve ssss nextbit s) ssss))
                 (if (= k 63)
                     (return zz)
                   (incf k))))))

;;; Decodes DC value
(defun decode-dc (maxcode mincode valptr huffval nextbit s)
  "Decodes DC value"
  (let ((tt (decode maxcode mincode valptr huffval nextbit s)))
  (declare #.*optimize*
           (type (simple-array uint8 (*)) huffval)
           (fixnum tt))
    (extend (recieve tt nextbit s) tt)))

;;; Decodes single 8x8 block
(defun decode-block (zz tabs nextbit s)
  "Reads one 8x8 block. Doesn't deals with predictors."
  (let ((tdc (aref tabs 0))
        (tac (aref tabs 1)))
  (declare #.*optimize*
           (type sint16-array zz)
	   (type (simple-array sint16-2d-array (*)) tabs)
           (type huffstruct tac tdc))
  (setf (aref zz 0) (decode-dc (huffstruct-maxcode tdc)
                                (huffstruct-mincode tdc)
                                (huffstruct-valptr tdc)
                                (huffstruct-huffval tdc) nextbit s))
  (decode-ac zz
             (huffstruct-maxcode tac)
             (huffstruct-mincode tac)
             (huffstruct-valptr tac)
             (huffstruct-huffval tac) nextbit s)
  zz))

;;; Dequanitzation
(defun dequantize (x y block table)
  "Dequantizes a single sample"
  (declare #.*optimize*
           (type fixnum x y))
  (mul (s16ref block x y) (u8ref table x y)))

;;;Macro that bounds value in IDCT
(defmacro dct-limit (n)
  `(aref *idct-limit-array* (logand (plus ,n 255) 511)))

;;; Inverse LLM DCT and dequantization
(defun inverse-llm-dct (block q-table ws)
  "Performs Inverse LMM DCT and dequantization"
  (let ((tmp0 0) (tmp1 0) (tmp2 0) (tmp3 0)
        (tmp10 0) (tmp11 0) (tmp12 0) (tmp13 0)
        (z1 0) (z2 0) (z3 0) (z4 0) (z5 0)
        (dcval 0))
    (declare #.*optimize*
             (type fixnum tmp0 tmp1 tmp2 tmp3 tmp10 tmp11 tmp12 tmp13 z1 z2 z3 z4 z5 dcval)
             (type sint16-2d-array block)
             (dynamic-extent tmp0 tmp1 tmp2 tmp3 tmp10 tmp11 tmp12 tmp13 z1 z2 z3 z4 z5 dcval))
    (loop for dptr fixnum from 0 to 7 ; iterating over columns
          if (and (zerop (s16ref block dptr 1))
                  (zerop (s16ref block dptr 2))
                  (zerop (s16ref block dptr 3))
                  (zerop (s16ref block dptr 4))
                  (zerop (s16ref block dptr 5))
                  (zerop (s16ref block dptr 6))
                  (zerop (s16ref block dptr 7))) do
          (setf dcval (ash (the fixnum (dequantize dptr 0 block q-table)) 1))
          (setf (s16ref ws dptr 0) dcval)
          (setf (s16ref ws dptr 1) dcval)
          (setf (s16ref ws dptr 2) dcval)
          (setf (s16ref ws dptr 3) dcval)
          (setf (s16ref ws dptr 4) dcval)
          (setf (s16ref ws dptr 5) dcval)
          (setf (s16ref ws dptr 6) dcval)
          (setf (s16ref ws dptr 7) dcval)
          else do
          (setf z2 (dequantize dptr 2 block q-table))
          (setf z3 (dequantize dptr 6 block q-table))
          (setf z1 (mul (plus z2 z3) +FIX-0-541196100+))
          (setf tmp2 (plus z1 (mul z3 (- +FIX-1-847759065+))))
          (setf tmp3 (plus z1 (mul z2 +FIX-0-765366865+)))
          (setf z2 (dequantize dptr 0 block q-table))
          (setf z3 (dequantize dptr 4 block q-table))
          (setf tmp0 (ash (plus z2 z3) dct-shift))
          (setf tmp1 (ash (minus z2 z3) dct-shift))
          (setf tmp10 (plus tmp0 tmp3))
          (setf tmp13 (minus tmp0 tmp3))
          (setf tmp11 (plus tmp1 tmp2))
          (setf tmp12 (minus tmp1 tmp2))
          (setf tmp0 (dequantize dptr 7 block q-table))
          (setf tmp1 (dequantize dptr 5 block q-table))
          (setf tmp2 (dequantize dptr 3 block q-table))
          (setf tmp3 (dequantize dptr 1 block q-table))
          (setf z1 (plus tmp0 tmp3))
          (setf z2 (plus tmp1 tmp2))
          (setf z3 (plus tmp0 tmp2))
          (setf z4 (plus tmp1 tmp3))
          (setf z5 (mul (plus z3 z4) +FIX-1-175875602+))
          (setf tmp0 (mul tmp0 +FIX-0-298631336+))
          (setf tmp1 (mul tmp1 +FIX-2-053119869+))
          (setf tmp2 (mul tmp2 +FIX-3-072711026+))
          (setf tmp3 (mul tmp3 +FIX-1-501321110+))
          (setf z1 (mul z1 (- +FIX-0-899976223+)))
          (setf z2 (mul z2 (- +FIX-2-562915447+)))
          (setf z3 (mul z3 (- +FIX-1-961570560+)))
          (setf z4 (mul z4 (- +FIX-0-390180644+)))
          (incf z3 z5)
          (incf z4 z5)
          (incf tmp0 (plus z1 z3))
          (incf tmp1 (plus z2 z4))
          (incf tmp2 (plus z2 z3))
          (incf tmp3 (plus z1 z4))
          (setf (s16ref ws dptr 0) (descale (plus tmp10 tmp3) +shift-1+))
          (setf (s16ref ws dptr 7) (descale (minus tmp10 tmp3) +shift-1+))
          (setf (s16ref ws dptr 1) (descale (plus tmp11 tmp2) +shift-1+))
          (setf (s16ref ws dptr 6) (descale (minus tmp11 tmp2) +shift-1+))
          (setf (s16ref ws dptr 2) (descale (plus tmp12 tmp1) +shift-1+))
          (setf (s16ref ws dptr 5) (descale (minus tmp12 tmp1) +shift-1+))
          (setf (s16ref ws dptr 3) (descale (plus tmp13 tmp0) +shift-1+))
          (setf (s16ref ws dptr 4) (descale (minus tmp13 tmp0) +shift-1+)))

    (loop for row of-type sint16-array across block ; iterating over rows
          for inrow of-type sint16-array across ws
          if (not (find-if-not #'zerop inrow :start 1)) do
          (setf dcval (dct-limit (descale (aref inrow 0) 4)))
          (setf (aref row 0) dcval)
          (setf (aref row 1) dcval)
          (setf (aref row 2) dcval)
          (setf (aref row 3) dcval)
          (setf (aref row 4) dcval)
          (setf (aref row 5) dcval)
          (setf (aref row 6) dcval)
          (setf (aref row 7) dcval)
          else do
          (setf z2 (aref inrow 2))
          (setf z3 (aref inrow 6))
          (setf z1 (mul (plus z2 z3) +FIX-0-541196100+))
          (setf tmp2 (plus z1 (mul z3 (- +FIX-1-847759065+))))
          (setf tmp3 (plus z1 (mul z2 +FIX-0-765366865+)))
          (setf tmp0 (ash (plus (aref inrow 0) (aref inrow 4)) dct-shift))
          (setf tmp1 (ash (minus (aref inrow 0) (aref inrow 4)) dct-shift))
          (setf tmp10 (plus tmp0 tmp3))
          (setf tmp13 (minus tmp0 tmp3))
          (setf tmp11 (plus tmp1 tmp2))
          (setf tmp12 (minus tmp1 tmp2))
          (setf tmp0 (aref inrow 7))
          (setf tmp1 (aref inrow 5))
          (setf tmp2 (aref inrow 3))
          (setf tmp3 (aref inrow 1))
          (setf z1 (plus tmp0 tmp3))
          (setf z2 (plus tmp1 tmp2))
          (setf z3 (plus tmp0 tmp2))
          (setf z4 (plus tmp1 tmp3))
          (setf z5 (mul (plus z3 z4) +FIX-1-175875602+))
          (setf tmp0 (mul tmp0 +FIX-0-298631336+))
          (setf tmp1 (mul tmp1 +FIX-2-053119869+))
          (setf tmp2 (mul tmp2 +FIX-3-072711026+))
          (setf tmp3 (mul tmp3 +FIX-1-501321110+))
          (setf z1 (mul z1 (- +FIX-0-899976223+)))
          (setf z2 (mul z2 (- +FIX-2-562915447+)))
          (setf z3 (mul z3 (- +FIX-1-961570560+)))
          (setf z4 (mul z4 (- +FIX-0-390180644+)))
          (incf z3 z5)
          (incf z4 z5)
          (incf tmp0 (plus z1 z3))
          (incf tmp1 (plus z2 z4))
          (incf tmp2 (plus z2 z3))
          (incf tmp3 (plus z1 z4))
          (setf (aref row 0) (dct-limit (descale (plus tmp10 tmp3) +shift+4+)))
          (setf (aref row 7) (dct-limit (descale (minus tmp10 tmp3) +shift+4+)))
          (setf (aref row 1) (dct-limit (descale (plus tmp11 tmp2) +shift+4+)))
          (setf (aref row 6) (dct-limit (descale (minus tmp11 tmp2) +shift+4+)))
          (setf (aref row 2) (dct-limit (descale (plus tmp12 tmp1) +shift+4+)))
          (setf (aref row 5) (dct-limit (descale (minus tmp12 tmp1) +shift+4+)))
          (setf (aref row 3) (dct-limit (descale (plus tmp13 tmp0) +shift+4+)))
          (setf (aref row 4) (dct-limit (descale (minus tmp13 tmp0) +shift+4+))))))

;;; Places decoded block into the image buffer, with necessary upsampling
(defun upsample (image scan block x y H V offset nwidth nw nx dend)
  "Places decoded block into the image buffer, with necessary upsampling"
  (let* ((buffer (descriptor-buffer image))
         (ncomp (descriptor-ncomp image))
         (xbase (plus (scan-x scan) x)) ; (mul (ash x 3) H)))
         (ybase (plus (scan-y scan) y)) ; (mul (ash y 3) V)))
         (nxbase (mul xbase ncomp))
         (nybase (mul ybase nwidth)))
    (declare #.*optimize*
             (type sint16-2d-array block)
             (type uint8-array buffer)
             (type fixnum x y H V ncomp xbase ybase nwidth nx dend nxbase nybase offset)
             (dynamic-extent ncomp xbase ybase nxbase nybase))
    (loop for row of-type sint16-array across block
          for y fixnum from ybase below (descriptor-height image) by V
          for ypos fixnum from nybase by nw do
          (loop for val fixnum across row
                for x fixnum from xbase below (descriptor-width image) by H
                for pos fixnum from (the fixnum (+ ypos offset nxbase)) by nx do
                (if (= 1 H V)
                    (setf (aref buffer pos) val)
                  (loop for dy fixnum from 0 below V
                        for dypos fixnum from pos below dend by nwidth
                        for dxend from (mul (plus (1+ y) dy) nwidth) by nwidth do
                        (loop for dx fixnum from 0 below H
                              for dpos fixnum from dypos below dxend by 3 do
                              (setf (aref buffer dpos) val))))))))

;;; Reads and decodes either whole scan or restart interval
(defun decode-chunk (image scan zzbuf)
  "Reads and decodes either a whole scan (if no restarts) or restart interval"
  (let* ((nextbit (make-nextbit 0 0))
         (ncomp (scan-ncomp scan))
         (nwidth (mul (descriptor-width image) (descriptor-ncomp image)))
         (dend (mul (descriptor-height image) nwidth))
         (fr (make-array ncomp :initial-contents
                         (loop
                           ;; collecting sampling rates for a components in the scan
                           for i fixnum from 0 below ncomp
                           for cid fixnum = (first (aref (scan-cdesc scan) i))
                           for pos fixnum = (position cid (descriptor-cid image))
                           collecting (list (aref (descriptor-H image) pos)
                                            (aref (descriptor-V image) pos)))))
         (Hmax (loop for entry across fr maximize (the fixnum (first entry))))
         (Vmax (loop for entry across fr maximize (the fixnum (second entry))))
         (x-growth (ash Hmax 3))
         (y-growth (ash Vmax 3))
         (freqs (make-array ncomp :initial-contents
                            (loop for i fixnum from 0 below ncomp ; collecting sampling frequencies
                                  for cid fixnum = (first (aref (scan-cdesc scan) i))
                                  for pos fixnum = (position cid (descriptor-cid image))
                                  collecting (list (aref (descriptor-iH image) pos)
                                                   (aref (descriptor-iV image) pos)))))
         (preds (make-array ncomp :initial-element 0 :element-type 'sint16))
         (tables (make-array ncomp
                     :initial-contents
                     (loop for i fixnum from 0 below ncomp
                           for ta fixnum = (logand (second (aref (scan-cdesc scan) i)) 15)
                           for td fixnum = (ash (the fixnum (second (aref (scan-cdesc scan) i))) -4)
                           collecting (vector (aref (descriptor-huff-ac image) ta)
                                              (aref (descriptor-huff-dc image) td))))))
    (declare #.*optimize*
             (type fixnum ncomp Hmax Vmax x-growth y-growth nwidth)
             (type (simple-array t (*)) freqs fr tables)
             (type (simple-array sint16 (*)) preds)
             (dynamic-extent fr freqs))
    (catch 'marker
      (loop
        do (loop for comp fixnum from 0 below ncomp
                 for pos fixnum =
                    (position (the fixnum (first (aref (scan-cdesc scan) comp)))
                              (descriptor-cid image)) ; an offset for byte positioning
                 for q-tab = (aref (descriptor-qtables image) (aref (descriptor-qdest image) comp))
                 for H fixnum = (first (aref freqs comp))
                 for V fixnum = (second (aref freqs comp))
                 for nw fixnum = (mul nwidth V)
                 for nx fixnum = (mul (descriptor-ncomp image) H)
                 for blocks-y fixnum = (second (aref fr comp))
                 for blocks-x fixnum = (first (aref fr comp)) do
                    (loop for y fixnum from 0 below blocks-y
		       for y-pos fixnum from (mul (ash y 3) V) by (ash V 3) do
                             (loop for x fixnum from 0 below blocks-x
                                   for x-pos fixnum from (mul (ash x 3) H) by (ash H 3)
                                   for decoded-block of-type sint16-2d-array =
				  (izigzag (decode-block (descriptor-zz image)
							 (aref tables comp) nextbit image) zzbuf) do
                                   ;; DC decoding and predictor update
                                      (incf (s16ref decoded-block 0 0) (aref preds comp))
                                      (setf (aref preds comp) (s16ref decoded-block 0 0))
                                      (when (and (< (plus x-pos (scan-x scan)) (descriptor-width image))
                                                 (< (plus y-pos (scan-y scan)) (descriptor-height image)))
                                        ;; inverse DCT and block write to the buffer
                                        (inverse-llm-dct decoded-block q-tab (descriptor-ws image))
                                        (upsample image scan decoded-block x-pos y-pos
                                                  H V pos nwidth nw nx dend)))))
           (incf (scan-x scan) x-growth)
           (when (<= (descriptor-width image) (scan-x scan))
             (incf (scan-y scan) y-growth)
             (setf (scan-x scan) 0))))))

;;; Scan decoding subroutine
(defun decode-scan (image i)
  (let ((scan (aref (descriptor-scans image) i))
	(zzbuf (2d-sint16-array
			    '(0  0  0  0  0  0  0  0)
			    '(0  0  0  0  0  0  0  0)
			    '(0  0  0  0  0  0  0  0)
			    '(0  0  0  0  0  0  0  0)
			    '(0  0  0  0  0  0  0  0)
			    '(0  0  0  0  0  0  0  0)
			    '(0  0  0  0  0  0  0  0)
			    '(0  0  0  0  0  0  0  0))))
    (read-jpeg-byte image) ; length
    (read-jpeg-byte image)
    (loop with ncomp fixnum = (setf (scan-ncomp scan) (read-jpeg-byte image))
          for j fixnum from 0 below ncomp do
          (setf (first (aref (scan-cdesc scan) j)) (read-jpeg-byte image)) ; component ID
          (setf (second (aref (scan-cdesc scan) j)) (read-jpeg-byte image))) ; Td and Ta nibbles
    (read-jpeg-byte image)
    (read-jpeg-byte image)
    (read-jpeg-byte image)
    (if (= (descriptor-restart-interval image) 0)
        (decode-chunk image scan zzbuf) ; reading the whole scan at once
      (loop for term = (decode-chunk image scan zzbuf)
            while (eq 'restart term)
            finally (return term))))) ; or in pieces

;;; Function that bounds value in 0..255 range
(defun limit (n)
  (cond ((> n 254) 255)
        ((< n 1) 0)
        (t n)))

;;; Inverse colorspace conversion
(defun inverse-colorspace-convert (image)
  (let* ((buffer (descriptor-buffer image))
         (nw (mul (descriptor-width image) 3)))
    (declare #.*optimize*
             (type (simple-array uint8 (*)) buffer)
             (type fixnum nw))
    (loop for y fixnum from 0 below (descriptor-height image)
          for yp fixnum from 0 by nw do
          (loop for x fixnum from 0 below (descriptor-width image)
                for py fixnum from yp by 3
                for pu fixnum = (1+ py)
                for pv fixnum = (plus py 2)
                for yy fixnum = (aref buffer py)
                for cb fixnum = (aref buffer pu)
                for cr fixnum = (aref buffer pv) do
                (setf (aref buffer py) ; BLUE
                      (the uint8 (limit (plus yy (aref +cb-b-tab+ cb)))))
                (setf (aref buffer pu) ; GREEN
                      (the uint8 (limit (plus yy (ash (plus
						       (aref +cb-g-tab+ cb)
						       (aref +cr-g-tab+ cr))
						      (- shift))))))
                (setf (aref buffer pv) ; RED
                      (the uint8 (limit (plus yy (aref +cr-r-tab+ cr)))))))))

(defun ycck-cmyk-convert (image)
  (let* ((buffer (descriptor-buffer image))
         (nw (mul (descriptor-width image) 4)))
    (declare #.*optimize*
	     (type (simple-array uint8 (*)) buffer)
             (type fixnum nw))
    (loop for y fixnum from 0 below (descriptor-height image)
       for yp fixnum from 0 by nw do
	 (loop for x fixnum from 0 below (descriptor-width image)
	    for py fixnum from yp by 4
	    for pu fixnum = (1+ py)
	    for pv fixnum = (plus py 2)
	    for yy fixnum = (aref buffer py)
	    for cb fixnum = (aref buffer pu)
	    for cr fixnum = (aref buffer pv) do
	      (setf (aref buffer pv)	; BLUE
		    (the uint8 (limit (minus +max-sample+ (plus yy (aref +cb-b-tab+ cb))))))
	      (setf (aref buffer pu)	; GREEN
		    (the uint8 (limit (minus +max-sample+
					     (plus yy (ash (plus
							    (aref +cb-g-tab+ cb)
							    (aref +cr-g-tab+ cr))
							   (- shift)))))))
	      (setf (aref buffer py)	; RED
		    (the uint8 (limit (minus +max-sample+ (plus yy (aref +cr-r-tab+ cr))))))))))

(defun convert-cmyk-to-rgb (buffer h w &key rgb-buffer)
  (unless rgb-buffer
    (setf rgb-buffer (make-array (* h w 3) :element-type 'uint8)))
  (loop for y fixnum from 0 below h
     for yrgb fixnum from 0 by (* w 3)
     for yp fixnum from 0 by (* w 4) do
       (loop for x fixnum from 0 below w
	  for py fixnum from yp by 4
	  for pu fixnum = (1+ py)
	  for pv fixnum = (plus py 2)
	  for pb fixnum from yrgb by 3
	  for pg fixnum = (1+ pb)
	  for pr fixnum = (plus pb 2)
	  for cyan fixnum = (aref buffer py)
	  for magenta fixnum = (aref buffer pu)
	  for yellow fixnum = (aref buffer pv)
	  for black fixnum = (aref buffer (1+ pv)) do
	    (setf (aref rgb-buffer pr) (round (mul cyan black) 255)
		  (aref rgb-buffer pg) (round (mul magenta black) 255)
		  (aref rgb-buffer pb) (round (mul yellow black) 255))))
  rgb-buffer)

(defun allocate-buffer (height width ncomp)
  (make-array (* height width ncomp) 
	      :element-type 'uint8
	      :initial-element 0))

(defun decode-frame-beginning (image buffer)
  (read-jpeg-byte image) ; length
  (read-jpeg-byte image)
  (read-jpeg-byte image) ; sample precision
  (let ((height (read-word image))
        (width (read-word image))
        (ncomp (read-jpeg-byte image)))
    (if (arrayp buffer)
        (if (< (length buffer) (* (setf (descriptor-height image) height)
                                  (setf (descriptor-width image) width)
                                  (setf (descriptor-ncomp image) ncomp)))
            (error 'invalid-buffer-supplied :buffer buffer)
            (setf (descriptor-buffer image) buffer))
        (setf (descriptor-buffer image)
              (allocate-buffer (setf (descriptor-height image) height)
                               (setf (descriptor-width image) width)
                               (setf (descriptor-ncomp image) ncomp))))))

;;; Frame decoding subroutine
(defun decode-frame (image buffer)
  (decode-frame-beginning image buffer)
  (loop for i fixnum from 0 below (descriptor-ncomp image)
     with hv fixnum do
       (setf (aref (descriptor-cid image) i) (read-jpeg-byte image)) ; Cj
       (setf hv (read-jpeg-byte image))				    ; HV
       (setf (aref (descriptor-H image) i) (ash hv -4))
       (setf (aref (descriptor-V image) i) (logand hv 7))
       (setf (aref (descriptor-qdest image) i) (read-jpeg-byte image)))
	(let* ((frl (loop for i fixnum from 0 below (descriptor-ncomp image)
		       collecting (list (aref (descriptor-H image) i)
					(aref (descriptor-V image) i))))
	       (Hmax (loop for entry in frl maximize (first entry)))
	       (Vmax (loop for entry in frl maximize (second entry)))
	       (freqs (convert-sampling frl Hmax Vmax)))
	  (loop for entry across freqs
	     for i fixnum from 0 do
	       (setf (aref (descriptor-iH image) i) (first entry))
	       (setf (aref (descriptor-iV image) i) (second entry)))
	  (loop with term fixnum = 0
	     for j fixnum from 0
	     until (= term +M_EOI+) do
	       (when (/= (interpret-markers image term) +M_SOS+)
		 (error 'unsupported-jpeg-frame-marker))
	       (setf term (decode-scan image j)))))

(defun decode-stream (stream &key buffer (colorspace-conversion t) descriptor cached-source-p (decode-frame t))
  "Return image array, height, width, number of components and APP14 Adobe transform. Does not support
progressive DCT-based JPEGs."
  (when (and (null stream) (not cached-source-p))
    (error 'invalid-buffer-supplied))
  (when descriptor
    (loop for scan across (descriptor-scans descriptor) do ;required if we reuse descriptors
	 (setf (scan-x scan) 0
	       (scan-y scan) 0)))
  (let* ((image (or descriptor (make-descriptor)))
	 (pos 0))
    (cond (cached-source-p
           (when (or (not (typep (descriptor-source-cache image) 'array))
                     (and (typep stream 'file-stream) (< (length (descriptor-source-cache image)) (file-length stream))))
             (setf (descriptor-source-cache image) (make-array (file-length stream) :element-type 'uint8)))
           (when stream ;; NULL stream means the cache in descriptor is already read
             (read-sequence (descriptor-source-cache image) stream))
           (let ((cache (descriptor-source-cache image)))
             (setf (descriptor-byte-reader image)
                   #'(lambda ()
                       (declare #.*optimize*
                                (type uint8-array cache)
                                (type fixnum pos))
                       (prog1 (aref cache pos) (incf pos))))))
          ;; if descriptor is NULL or the descriptor's byte-reader is
          ;; NULL, set it here.
          ((or (not descriptor)
               (not (descriptor-byte-reader descriptor)))
           (setf (descriptor-byte-reader image)
                 #'(lambda ()
                     (read-byte stream)))))
     (unless (= (read-marker image) +M_SOI+)
      (error 'unrecognized-file-format))
     (let ((marker (interpret-markers image 0)))
       (if decode-frame
           ;; decode-frame currently only supports baseline DCT frames
           (cond ((= +M_SOF0+ marker)
                  (decode-frame image buffer)
                  (when colorspace-conversion
                    (cond ((and (= (descriptor-ncomp image) 3) (eql (descriptor-adobe-app14-transform image) :ycbcr-rgb))
                           (inverse-colorspace-convert image))
                          ((eql (descriptor-adobe-app14-transform image) :ycck-cmyk)
                           (ycck-cmyk-convert image))
                          ((= (descriptor-ncomp image) 3)
                           (inverse-colorspace-convert image)))))
                 (t (error 'unsupported-jpeg-format :code marker)))
           ;; decode-frame-beginning supports baselnie DCT frames
           ;; and progressive DCT frames, so allow those
           (cond ((or (= +M_SOF0+ marker)
                      (= +M_SOF2+ marker))
                  (decode-frame-beginning image nil))
                 (t (error 'unsupported-jpeg-format :code marker)))))
     (values (when decode-frame
               (descriptor-buffer image))
             (descriptor-height image)
             (descriptor-width image)
             (descriptor-ncomp image)
             (descriptor-adobe-app14-transform image))))

;;; Top level decoder function
(defun decode-image (filename &key buffer (colorspace-conversion t) cached-source-p (decode-frame t))
  (with-open-file (in filename :direction :input :element-type 'uint8)
    (decode-stream in
                   :buffer buffer
                   :colorspace-conversion colorspace-conversion
                   :cached-source-p cached-source-p
                   :decode-frame decode-frame)))

(defun decode-stream-height-width-ncomp (stream &key descriptor cached-source-p)
  "Return the height and width of the JPEG data read from STREAM. Does less work than
DECODE-STREAM and also supports progressive DCT-based JPEGs."
  (multiple-value-bind (nil-buffer height width ncomp adobe-app14-transform)
      (apply #'decode-image stream
             :decode-frame nil
             (append
              (when descriptor
                `(:descriptor ,descriptor))
              (when cached-source-p
                `(:cached-source-p ,cached-source-p))))
    (declare (ignore nil-buffer))
    (values height width ncomp adobe-app14-transform)))

(defun jpeg-file-dimensions (filename)
  "Return image height, width and number of components, plus the type of Adobe colorpsace transform"
  (with-open-file (in filename :direction :input :element-type 'uint8)
    (decode-stream-height-width-ncomp in)))
