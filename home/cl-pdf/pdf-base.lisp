;;; cl-pdf copyright 2002-2009 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)

;;;text functions

(defmacro in-text-mode (&body body)
  `(unwind-protect
     (let ((*font* nil))
       (write-line "BT" *page-stream*)
       ,@body)
    (write-line "ET" *page-stream*)))

(defun set-font (font size)
  (setf *font* font)
  (setf *font-size* size)
  (format *page-stream* "~a ~,2f Tf~%" (name (add-font-to-page font)) size))

(defun set-gstate (&rest gstate)
  (format *page-stream* "~a gs~%" (name (apply #'add-gstate-to-page gstate))))

(defmacro def-pdf-op (name (&rest args) format)
  (if args
    `(defun ,name ,args (format *page-stream* ,format ,@args))
    `(defun ,name () (write-line ,format *page-stream*))))

(def-pdf-op move-text (dx dy) "~8f ~8f Td~%")

;;; String output

(defgeneric write-to-page (object encoding &optional escape)
 (:documentation
  "Write object (only text for now) into current *page-stream* in text mode"))

;;; show-text writes string as is, draw-text escapes dangerous characters.
;;; Args: text String or a single character

(defun show-text (text)
  (if *font*
      (progn (write-to-page text (if *font* (encoding *font*)))
        (write-line "Tj" *page-stream*))
      (format *page-stream* "(~a) Tj~%" text)))

(defun draw-text (text)
  (if *font*
      (progn (write-to-page text (if *font* (encoding *font*)) t)
        (write-line "Tj" *page-stream*))
      (format *page-stream* "(~a) Tj~%" text)))

(defun show-text-on-next-line (string)
  (write-to-page string (if *font* (encoding *font*)))
  (write-line "'" *page-stream*))

(defun draw-text-on-next-line (string)
  (write-to-page string (if *font* (encoding *font*)) t)
  (write-line "'" *page-stream*))

(defun show-spaced-strings (strings)
  (write-string "[ " *page-stream*)
  (let ((encoding (if *font* (encoding *font*))))
    (dolist (item strings)
      (if (numberp item)
          (format *page-stream* "~a " item)
          (write-to-page item encoding))))
  (write-line "] TJ" *page-stream*))

(defun draw-spaced-strings (strings)
  (write-string "[ " *page-stream*)
  (let ((encoding (if *font* (encoding *font*))))
    (dolist (item strings)
      (if (numberp item)
          (format *page-stream* "~a " item)
          (write-to-page item encoding t))))
  (write-line "] TJ" *page-stream*))

(defmethod write-to-page ((string string) encoding &optional escape)
  (declare (ignore encoding))
  (write-char #\( *page-stream*)
  (if escape
      (loop for char across string
            do (case char
                 ((#\( #\) #\\)
                  (write-char #\\ *page-stream*)
                  (write-char char *page-stream*))
                 (otherwise
                  (write-char char *page-stream*))))
    (write-string string *page-stream*))
  (write-string ") " *page-stream*))

(defmethod write-to-page ((string string) (encoding single-byte-encoding) &optional escape)
  ;; There is no point to interpret \n and others in a special way
  ;; as they are not control characters within content stream
  (write-char #\( *page-stream*)
  (if (or escape
          #+lispworks (lw:text-string-p string)		; may include unicode
          #+allegro t)
      (loop with charset = (charset encoding)
            for char across string
            do (case char
                 ((#\( #\) #\\)
                  (write-char #\\ *page-stream*)
                  (write-char char *page-stream*))
                 ;(#\Newline
                 ; (write-string "\\n" *page-stream*))
                 ;(#\Return
                 ; (write-string "\\r" *page-stream*))
                 ;(#\Tab
                 ; (write-string "\\t" *page-stream*))
                 (otherwise
                  (write-char (if #+lispworks (lw:base-char-p char)
                                  #+(or allegro sbcl) (standard-char-p char)
                                  #-(or lispworks allegro sbcl) t
                                  char
                                  (code-char (char-external-code char charset)))
                              *page-stream*))))
      (write-string string *page-stream*))
  (write-string ") " *page-stream*))

(defmethod write-to-page ((string string) (encoding unicode-encoding) &optional escape)
  (declare (ignore escape))
  (write-char #\< *page-stream*)
  (loop for char across string do
        (format *page-stream* "~4,'0x" (char-code char)))
  (write-string "> " *page-stream*))

;;; Single character output

(defun show-char (char)
 ;;; Deprecated in favor of show-text or draw-text
  (write-to-page char (if *font* (encoding *font*)) t)
  (write-line "Tj" *page-stream*))

(defmethod write-to-page ((char character) encoding &optional escape)
 ;;; This default method is only needed for deprecated and legacy code,
  ;; e.g. bar-codes.lisp: draw-chars (*font* is nil) -> draw-char
  (declare (ignore encoding))
  (write-char #\( *page-stream*)
  (when escape (case char
                 ((#\( #\) #\\) (write-char #\\ *page-stream*))))
  (write-char char *page-stream*)
  (write-char #\) *page-stream*))

(defmethod write-to-page ((char character)(encoding single-byte-encoding) &optional escape)
  (write-char #\( *page-stream*)
  (when escape (case char
                 ((#\( #\) #\\) (write-char #\\ *page-stream*))))
  (write-char (if #+lispworks (lw:base-char-p char)
                  #+(or allegro sbcl) (standard-char-p char)
                  #-(or lispworks allegro sbcl) t
                  char
                  (code-char (char-external-code char (charset encoding))))
              *page-stream*)
  (write-char #\) *page-stream*))

(defmethod write-to-page ((char character) (encoding unicode-encoding) &optional escape)
  (declare (ignorable encoding escape))
  (write-char #\< *page-stream*)
  (format *page-stream* "~4,'0x" (char-code char))
  (write-char #\> *page-stream*))

(def-pdf-op set-text-rendering-mode (mode) "~d Tr~%")

(def-pdf-op set-char-spacing (space) "~8f Tc~%")

(def-pdf-op set-text-x-scale (scale) "~8f Tz~%")

(def-pdf-op set-text-leading (space) "~8f TL~%")

(def-pdf-op set-text-rise (rise) "~8f Ts~%")

(def-pdf-op move-to-next-line () " T*")

(def-pdf-op set-text-matrix (a b c d e f) "~10f ~10f ~10f ~10f ~10f ~10f Tm~%")

;;; graphic functions
(defconstant +deg-to-rad+ #.(/ pi 180))

(defmacro with-saved-state (&body body)
  `(unwind-protect
     (progn (write-line "q" *page-stream*)
	    ,@body)
    (write-line "Q" *page-stream*)))

(def-pdf-op set-transform-matrix (a b c d e f) "~8f ~8f ~8f ~8f ~8f ~8f cm~%")

(def-pdf-op translate (dx dy) "1.0 0.0 0.0 1.0 ~8f ~8f cm~%")

(defun rotate (deg)
  (let* ((angle (* +deg-to-rad+ deg))
	 (s (sin angle))
	 (c (cos angle)))
    (format *page-stream* "~10f ~10f ~10f ~10f 0.0 0.0 cm~%" c s (- s) c)))

(defun rotate* (radians)
  (let* ((s (sin radians))
	 (c (cos radians)))
    (format *page-stream* "~10f ~10f ~10f ~10f 0.0 0.0 cm~%" c s (- s) c)))

(def-pdf-op scale (sx sy) " ~8f 0.0 0.0 ~8f 0.0 0.0 cm~%")

(defun skew (x-deg y-deg)
  (format *page-stream* " 1.0 ~10f ~10f 1.0 0.0 0.0 cm~%"
	  (tan (* +deg-to-rad+ x-deg))(tan (* +deg-to-rad+ y-deg))))

(defun skew* (x-radians y-radians)
  (set-transform-matrix 1.0 (tan x-radians) (tan y-radians) 1.0 0.0 0.0))

(def-pdf-op set-line-width (width) "~8f w~%")

(def-pdf-op set-line-cap (mode) "~d J~%")

(def-pdf-op set-line-join (mode) "~d j~%")

(def-pdf-op set-dash-pattern (dash-array phase) "[~{~d~^ ~}] ~d d~%")

(def-pdf-op set-miter-limit (limit) "~8f M~%")

(def-pdf-op move-to (x y) "~8f ~8f m~%")

(def-pdf-op line-to (x y) "~8f ~8f l~%")

(def-pdf-op bezier-to (x1 y1 x2 y2 x3 y3) "~8f ~8f ~8f ~8f ~8f ~8f c~%")

(def-pdf-op bezier2-to (x2 y2 x3 y3) "~8f ~8f ~8f ~8f v~%")

(def-pdf-op bezier3-to (x1 y1 x3 y3) "~8f ~8f ~8f ~8f y~%")

(def-pdf-op close-path () " h")

(def-pdf-op basic-rect (x y dx dy) "~8f ~8f ~8f ~8f re~%")

(defun paint-image (image)
  (format *page-stream* "~a Do~%" (name image)))

(def-pdf-op stroke () " S")

(def-pdf-op close-and-stroke () " s")

(def-pdf-op fill-path () " f")

(def-pdf-op close-and-fill () " h f")

(def-pdf-op even-odd-fill () " f*")

(def-pdf-op fill-and-stroke () " B")

(def-pdf-op even-odd-fill-and-stroke () " B*")

(def-pdf-op close-fill-and-stroke () " b")

(def-pdf-op close-even-odd-fill-and-stroke () " b*")

(def-pdf-op end-path-no-op  () " n")

(def-pdf-op clip-path () " W")

(def-pdf-op even-odd-clip-path () " W*")

(def-pdf-op set-gray-stroke (gray) "~5f G~%")

(def-pdf-op set-gray-fill (gray) "~5f g~%")

(def-pdf-op set-rgb-stroke (r g b) "~5f ~5f ~5f RG~%")

(defgeneric get-rgb (color)
 (:method ((color list))
  (values (first color)(second color)(third color)))

 (:method ((color vector))
  #+lispworks
  (if (numberp (aref color 0))
      (values (aref color 0)(aref color 1)(aref color 2))
      (case (aref color 0)		; convert from (color:make-rgb ...) or other model
	(:RGB	(values (aref color 1)(aref color 2)(aref color 3)))
	(:GRAY	(values (aref color 1)(aref color 1)(aref color 1)))))
  #-lispworks
  (values (aref color 0)(aref color 1)(aref color 2)))

 (:method ((color string))	; takes a CSS color string like "#CCBBFF"
  (if (eql #\# (aref color 0))
      (values (/ (parse-integer color :start 1 :end 3 :radix 16) 255.0)
	      (/ (parse-integer color :start 3 :end 5 :radix 16) 255.0)
	      (/ (parse-integer color :start 5 :end 7 :radix 16) 255.0))
      (find-color-from-string color)))

 (:method ((color integer))	; a la CSS but specified as a Lisp number like #xCCBBFF
  (values (/ (ldb (byte 8 16) color) 255.0)
          (/ (ldb (byte 8 8) color) 255.0)
          (/ (ldb (byte 8 0) color) 255.0)))

 (:method ((color symbol))	; :blue, :darkgreen, or win32:color_3dface
   (find-color-from-symbol color)))

(defun set-color-stroke (color)
  (multiple-value-call #'set-rgb-stroke (get-rgb color)))

(defun set-color-fill (color)
  (multiple-value-call #'set-rgb-fill (get-rgb color)))

(def-pdf-op set-rgb-fill (r g b) "~5f ~5f ~5f rg~%")

(def-pdf-op set-cymk-stroke (c y m k) "~5f ~5f ~5f ~5f K~%")

(def-pdf-op set-cymk-fill (c y m k) "~5f ~5f ~5f ~5f k~%")

;;; Transparency support by Eric Marsden
;;; Affects both stroking operations ("CA" op) and non-stroking ("ca" op)
;;; alpha ranges from 0 (transparent) to 1 (opaque)

(defun set-transparency (alpha &optional (blend-mode :normal))
  (declare (type float alpha))
  (let* ((a (format nil "~3F" alpha))
         (bm (ecase blend-mode
               (:normal "/Normal")
               (:multiple "/Multiply")
               (:screen "/Screen")
               (:overlay "/Overlay")
               (:darken "/Darken")
               (:lighten "/Lighten")
               (:ColorDodge "/ColorDodge")
               (:ColorBurn "/ColorBurn")
               (:HardLight "/HardLight")
               (:SoftLight "/SoftLight")
               (:difference "/Difference")
               (:exclusion "/Exclusion")
               (:saturation "/Saturation")
               (:color "/Color")
               (:luminosity "/Luminosity"))))
    (set-gstate "CA" a "ca" a "BM" bm)))

(defun set-fill-transparency (alpha)
  (declare (type float alpha))
  (set-gstate "ca" (format nil "~3F" alpha)))

(defun set-stroke-transparency (alpha)
  (declare (type float alpha))
  (set-gstate "CA" (format nil "~3F" alpha)))

(defun draw-image (image x y dx dy rotation &optional keep-aspect-ratio)
  (when keep-aspect-ratio
    (let ((r1 (/ dy dx))
	  (r2 (/ (height image)(width image))))
      (if (> r1 r2)
	(setf dy (* dx r2)))
	(when (< r1 r2)(setf dx (/ dy r2)))))
  (with-saved-state
      (translate x y)
      (rotate rotation)
      (scale dx dy)
      (paint-image image)))

(defun add-link (x y dx dy ref-name &key (border #(0 0 0)))
  (let ((annotation (make-instance 'annotation :rect (vector x y (+ x dx) (+ y dy))
				   :type "/Link" :border border)))
    (push (cons "/Dest" (get-named-reference ref-name)) (dict-values (content annotation)))
    annotation))

(defun add-URI-link (x y dx dy uri &key (border #(0 0 0)))
  (let ((annotation (make-instance 'annotation :rect (vector x y (+ x dx) (+ y dy))
				   :type "/Link" :border border ))
	(action (make-instance 'dictionary :dict-values '(("/S" . "/URI")))))
    (add-dict-value (content annotation) "/A" action)
    (add-dict-value action "/URI" (concatenate 'string "(" uri ")"))
    annotation))

(defun add-external-link (x y dx dy filename page-nb &key (border #(0 0 0)))
  (let ((annotation (make-instance 'annotation :rect (vector x y (+ x dx) (+ y dy))
				   :type "/Link" :border border ))
	(action (make-instance 'dictionary :dict-values '(("/S" . "/GoToR")))))
    (add-dict-value (content annotation) "/A" action)
    (add-dict-value action "/F" (concatenate 'string "(" filename ")"))
    (add-dict-value action "/D" (vector page-nb "/Fit"))
    annotation))

(defparameter +jpeg-color-spaces+ #("?" "/DeviceGray" "?" "/DeviceRGB" "/DeviceCMYK"))

(defclass bitmap-image ()
  ((width  :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (nb-components :accessor nb-components :initarg :nb-components)
   (data   :accessor data :initarg :data)))

;;; Condition class of errors while parsing image files.
;;; Sometimes the parser of specific format cannot identify what is wrong an end-of-file
;;; is signaled. Both situations can be trapped by handling stream-error.
;;; CAUTION: Could the stream slot hold a pathname (not only stream)?

(define-condition image-file-parse-error (parse-error stream-error)
  ((message :initarg :message :reader error-message :initform nil))
  (:report (lambda (condition stream)
             (let ((stream-error-stream (stream-error-stream condition)))
               (format stream "Error~@[ at position ~d~] while reading image file~% ~a.~@[~%~a.~]"
                       (if (streamp stream-error-stream)
                           (file-position stream-error-stream)
                           nil)
                       (pathname stream-error-stream)
                       (error-message condition))))))

(defclass jpeg-image (bitmap-image)
  ((filename :accessor filename :initarg :filename)))

(defun %read-jpeg-file% (filename &key header-only)
  (with-open-file (s filename :direction :input :element-type '(unsigned-byte 8))
    (loop with width and height and nb-components and data
	  for marker = (read-byte s)
	  when (= marker #xFF)
          do (setf marker (read-byte s))
	      (cond
              ((member marker '(#xC0 #xC1 #xC2))
               ;; SOF markers
		 (read-byte s)(read-byte s) ;size
               (when (/= (read-byte s) 8)
                 (error 'image-file-parse-error :stream s
                        :message "JPEG must have 8bits per component"))
               (setf height (+ (ash (read-byte s) 8) (read-byte s))
                     width (+ (ash (read-byte s) 8) (read-byte s))
                     nb-components (read-byte s))
               (unless header-only
		 (file-position s :start)
		 (setf data (make-array (file-length s) :element-type '(unsigned-byte 8)))
                 (read-sequence data s))
		 (return (values nb-components width height data)))
              ((member marker '(#xC3 #xC5 #xC6 #xC7 #xC8 #xC9 #xCA #xCB #xCD #xCE #xCF))
               ;; unsupported markers
               (error 'image-file-parse-error :stream s
                      :message "Unsupported JPEG format"))
              ((not (member marker '(#xD0 #xD1 #xD2 #xD3 #xD4 #xD5 #xD6 #xD7 #xD8 #x01)))
               ;; no param markers. CAUTION 2010-Oct-17: Was -2!
		 (file-position s (+ (file-position s) (ash (read-byte s) 8) (read-byte s))))))))

(defun read-jpeg-file (filename &key header-only)
  (multiple-value-bind (nb-components width height data)
      (%read-jpeg-file% filename :header-only header-only)
    (when nb-components
      (make-instance 'jpeg-image :nb-components nb-components
		     :filename filename
		     :width width :height height :data data))))

(defgeneric make-jpeg-image (jpeg))

(defmethod make-jpeg-image (jpeg)
  (make-image jpeg))

(defmethod make-jpeg-image ((pathname pathname))
  (make-jpeg-image (read-jpeg-file pathname)))

(defmethod make-jpeg-image ((string string))
  (make-jpeg-image (read-jpeg-file string)))

(defgeneric make-image (object &key type &allow-other-keys)
 (:documentation "Returns more than just one pdf:image object when mask is supplied"))

(defmethod make-image ((object pathname) &rest args &key (type (pathname-type object)))
  (cond ((member type '("jpeg" "jpg") :test #'equalp)
         (apply 'make-image (read-jpeg-file object) args))
        ((equalp type "png")
         (apply 'make-image (read-png-file2 object) args))
        (t (or (apply 'make-image (read-convert-jpg-file object) args)
	       (error 'image-file-parse-error :stream object
		      :message (format nil "Unsupported image file type ~s" type))))))

(defmethod make-image ((object string) &rest args &key type)
  (apply #'make-image (merge-pathnames object (make-pathname :type type))
         args))

(defmethod make-image ((jpeg jpeg-image) &key &allow-other-keys)
  (make-instance 'pdf:image
         :bits (if *load-images-lazily*
		   #'(lambda () (data (read-jpeg-file (filename jpeg))))
		   (data jpeg))
         :width (width jpeg) :height (height jpeg)
         :filter "/DCTDecode" 
         :color-space (aref +jpeg-color-spaces+ (nb-components jpeg))
         :no-compression t))

(defun %generate-temp-filename% (filename filetype)
  (make-pathname :directory (pathname-directory (uiop:temporary-directory))
		 :name filename :type filetype))

(defun %convert-image-file% (source target &rest options)
  (uiop:run-program
   (format nil "convert ~a ~{~a ~}~a" source options target)))

(defun %read-convert-jpg-file% (pathname header-only)
  (let ((temp-jpg  (%generate-temp-filename% (pathname-name pathname) "jpg")))
    (%convert-image-file% pathname temp-jpg "-alpha" "remove")
    (prog1
	(read-jpeg-file temp-jpg :header-only header-only)
      (uiop:delete-file-if-exists temp-jpg))))

(defun read-convert-jpg-file (pathname &key header-only)
   (multiple-value-bind (retval err2)
       (ignore-errors
	 (%read-convert-jpg-file% pathname header-only))
	(if err2 nil retval)))
