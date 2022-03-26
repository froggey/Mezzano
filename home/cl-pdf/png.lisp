;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)

;;; PNG Images code proposal by Dmitri Ivanov
;;; Notes:
;;;  The /ProcSet entry of page resource dictionary should be expanded to
;;;	[/PDF /Text /ImageB /ImageC /ImageI]
;;;  for compatibility with existing viewer applications.
;;;  It is considered obsolete in PDF 1.4 (section 9.1)
;;;
;;;  Acrobat error "There was an error processing a page. A drawing error occurred"
;;;  Reason: Acrobat 5 has a bug and is unable to display transparent monochrome images.
;;;  Workaround: Remove transparency or save your image in 16 colors (4 bits per pixel)
;;;  or more.


(declaim (ftype (function (stream) (unsigned-byte 16)) read-byte16)
         (inline read-byte16))

(defun read-byte16 (stream)
 ;;; Read a 2-byte integer
  (the (unsigned-byte 16) (+ (ash (read-byte stream) 8) (read-byte stream))))

(declaim (ftype (function (stream) (unsigned-byte 32)) read-byte32))
(defun read-byte32 (stream)
 ;;; Read a 4-byte integer
  (the (unsigned-byte 32)
        (+ (ash (read-byte stream) 24) (ash (read-byte stream) 16)
           (ash (read-byte stream) 8) (read-byte stream))))

(defun read-base-string (stream byte-count)
  (let ((result (make-string byte-count :element-type 'base-char)))
    (dotimes (i byte-count result)
      (declare (fixnum i))
      (setf (schar result i) (code-char (read-byte stream))))))

(defclass png-image (bitmap-image)
 ((bits-per-color :accessor bits-per-color :initarg :bits-per-color)
  ;(color-space :accessor color-space :initarg :color-space)
  (filename :accessor filename :initarg :filename)
  (palette :accessor palette :initarg :palette)
  (mask :accessor mask :initarg :mask)))

(defun read-png-file (pathname &key header-only)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
   (flet ((skip-octets (octet-length)
            (dotimes (i octet-length) (read-byte stream))
            (values)))
    #-lispworks(declare (ftype (function (fixnum) (values)) skip-octets))
    ;; Check signature
    (unless (and (= (read-byte stream) 137)
                 (= (read-byte stream) #.(char-code #\P))
                 (= (read-byte stream) #.(char-code #\N))
                 (= (read-byte stream) #.(char-code #\G))
                 (= (read-byte stream) 13)
                 (= (read-byte stream) 10)
                 (= (read-byte stream) 26)
                 (= (read-byte stream) 10))
      (error 'image-file-parse-error :stream pathname
             :message "Wrong PNG signature"))

    ;; Read header
    (skip-octets 4)
    (when (string/= (read-base-string stream 4) "IHDR")
      (error 'image-file-parse-error :stream stream
             :message "Wrong PNG header"))
    (let* ((width (read-byte32 stream))
           (height (read-byte32 stream))
           (bits-per-color (read-byte stream))
           (cs (read-byte stream))
           (color-space (case cs
                          (0 "DeviceGray")
                          (2 "DeviceRGB")
                          (3 "Indexed")
                          (otherwise (error 'image-file-parse-error :stream stream
                                            :message "Alpha channel is not supported"))))
           palette mask data)
      (when (> bits-per-color 8)
        (error 'image-file-parse-error :stream stream
               :message "16-bit depth is not supported"))
      (when (/= (read-byte stream) 0)
        (error 'image-file-parse-error :stream stream
               :message "Unknown compression method"))
      (when (/= (read-byte stream) 0)
        (error 'image-file-parse-error :stream stream
               :message "Unknown filter method"))
      (when (/= (read-byte stream) 0)
        (error 'image-file-parse-error :stream stream
               :message "Interlacing is not supported"))
      (skip-octets 4)

      ;; Extract palette, transparency and data if any
      (do ((octet-length (read-byte32 stream) (read-byte32 stream))
           (marker (read-base-string stream 4) (read-base-string stream 4)))
          ((= octet-length 0))
        ;(declare (type (unsigned-byte 32) octet-length)
        (cond ((string= marker "PLTE")
	       ;; Palette: octet-length should be a multiple of 3
               (setq palette (make-array octet-length :element-type '(unsigned-byte 8)))
               (read-sequence palette stream))
              ((string= marker "tRNS")				; transparency info
               (let ((trns (make-array octet-length :element-type '(unsigned-byte 8))))
                 (read-sequence trns stream)
                 (setq mask (case cs
                              (0				; DeviceGray
                               (list (aref trns 1)))
                              (2				; DeviceRGB
                               (list (aref trns 1) (aref trns 3) (aref trns 5)))
                              (otherwise			; Indexed
                               (let ((position (position 0 trns)))
                                 (when position (list position))))))))
              ((string= marker "IDAT")				; image data block
               (if header-only
                   (skip-octets octet-length)
                   (let ((start 0))
                     (if (null data)
                         (setf data (make-array octet-length
                                                :element-type '(unsigned-byte 8)
                                                :adjustable t))
                         (progn
                           (setf start (first (array-dimensions data)))
                           (adjust-array data (+ start octet-length))))
                     (read-sequence data stream :start start))))
              ((string= marker "IEND")
               (return))
              (t		;"pHYs"
               (skip-octets octet-length)))
        (skip-octets 4))

      (when (and (= cs 3) (null palette))			; Indexed
        (error 'image-file-parse-error :stream stream
               :message "Palette is missing"))
      (make-instance 'png-image :nb-components color-space
		     :width width :height height  :data data
		     :filename pathname
                     :bits-per-color bits-per-color  :palette palette  :mask mask)))))

(defmethod make-image ((png png-image) &key &allow-other-keys)
  ;; For color key masking, the Mask entry is an array of 2*N integers,
  ;; [min1 max1 ... minN maxN], where N is the number of color components in the
  ;; image's color space.
  (let* ((nb-components (nb-components png))
         (palette (palette png))
         (lookup
          (if palette ;(string= nb-components "Indexed")
              (make-instance 'indirect-object :content	; comperess is controlled by config
                (make-instance 'pdf-stream
                               ;:dict-values `(("/Filter" . ,filter))
                               :content palette
			       :no-compression t))))
         (mask (mask png)))
    (make-instance 'pdf:image
         :bits (if *load-images-lazily*
		   #'(lambda () (data (read-png-file (filename png))))
		   (data png))
         :width (width png) :height (height png)
         :color-space (if (string= nb-components "Indexed")
                          (vector (pdf-name nb-components)
                            "/DeviceRGB"			; base
                            (1- (truncate (length palette) 3))	; maximum valid index value
                            lookup)
                          (pdf-name nb-components))
         :bits-per-color (bits-per-color png)
         :decode (if (string= nb-components "DeviceCMYK") #(1 0 1 0 1 0 1 0))
         :mask (when mask
                 (apply #'vector (mapcan (lambda (i) (list i i)) mask)))
         :filter "/FlateDecode"				; the only method we recognize
         :decode-parms `(("/Predictor" . 15)
                         ("/Colors" . ,(if (string= nb-components "DeviceRGB") 3 1))
                         ("/BitsPerComponent" . ,(bits-per-color png))
                         ("/Columns" . ,(width png)))
         :no-compression t)))				; data bits already come compressed

(defun read-png-file2 (pathname &key header-only)
  (handler-case
      (read-png-file pathname :header-only header-only)
    (image-file-parse-error (err)
      (or (read-convert-jpg-file pathname :header-only header-only)
	  err))))
