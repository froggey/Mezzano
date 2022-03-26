;;; cl-pdf copyright 2002-2009 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)

(defvar *font* nil
  "The current font in text mode")

(defvar *font-size* nil
  "The current font in text mode")

(defvar *font-cache* (make-hash-table :test #'equal))

(defclass font ()
 ((name :accessor name :initform "helvetica" :initarg :name)
  (encoding :accessor encoding :initform *standard-encoding*)
  (hyphen-code :accessor hyphen-code :initform 0)
  (hyphen-char :accessor hyphen-char :initform nil)
  (font-metrics :accessor font-metrics)
  (kernings :accessor kernings :initform (make-hash-table))
  (characters :accessor characters :initform (make-array 256 :initial-element nil))
  (pdf-widths :accessor pdf-widths :initform (make-array 256 :initial-element 0))
  (descender :accessor descender :initform 0)))		; minimum descender

(defmethod print-object ((self font) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~a" (name self))))

(defmethod initialize-instance :after ((font font) &key encoding &allow-other-keys)
  (let ((font-metrics (gethash (name font) *font-metrics*)))
    (unless font-metrics (error "Font ~s not found" (name font)))
    (setf (font-metrics font) font-metrics
          (descender font) (descender font-metrics))
    (unless encoding
      (setf (gethash (list (name font) nil) *font-cache*) font))
    (setf (encoding font)
	  (if encoding
	      (get-encoding encoding)
	      (extract-font-metrics-encoding font-metrics)))
    (if (eql (keyword-name (encoding font)) :unicode-encoding)
        (setf (pdf-widths font) (pdf-widths font-metrics)
              (characters font) (encoding-vector font-metrics)
              (hyphen-code font) (if (gethash "hyphen" (characters font-metrics))
                                     (code (gethash "hyphen" (characters font-metrics)))
                                     0)
              (hyphen-char font) (code-char (hyphen-code font)))
        (loop with font-characters = (characters font-metrics)
           with pdf-widths = (pdf-widths font)
           with void-char = (gethash "VoidCharacter" font-characters)
           and characters = (characters font)
           and hyphen-code = nil
           for i from 0 to 255
           for char-name across (char-names (encoding font))
           for char = (or (gethash char-name font-characters)
                          (aref (encoding-vector font-metrics) i)
                          void-char)
           do (setf (aref characters i) char
                    (aref pdf-widths i) (round (* 1000 (width char))))
             (when (and (not hyphen-code) (string= char-name "hyphen"))
               (setf hyphen-code i
                     (hyphen-code font) i
                     (hyphen-char font) (code-char i)))))
    (compute-kern-pairs font)
    (setf (gethash (list (name font) (encoding font)) *font-cache*) font)))

(defun get-font-descender (font &optional font-size)
  (if font-size (* (descender font) font-size) (descender font)))

(defun compute-kern-pairs (font)
  (let ((char-to-code (make-hash-table))
	(characters (characters font))
	(kernings (kernings font)))
    (loop for c across characters
	  for code from 0
	  when c do (setf (gethash c char-to-code) code))
    (maphash #'(lambda (k v)
		 (let ((code1 (gethash (car k) char-to-code))
		       (code2 (gethash (cdr k) char-to-code)))
		   (when (and code1 code2)
		     (setf (gethash (+ (* code1 65536) code2) kernings) (car v)))))
	     (kernings (font-metrics font)))))


(defgeneric get-char-metrics (char-or-code font encoding)
 ;;; This generic allows to customize treating charset by the lisp implementation
  ;; and is intended to replace get-char.
  ;; Args: char-or-code  Lisp character or its char-code
 (:method (char-or-code font encoding)
   (declare (ignore encoding))
  (aref (characters font)
        (if (characterp char-or-code) (char-code char-or-code) char-or-code))))

(defmethod get-char-metrics (char font (encoding single-byte-encoding))
  (aref (characters font)
        (if #+lispworks (lw:base-char-p char)
            #+(or allegro sbcl clisp) (standard-char-p char)
            #-(or lispworks allegro sbcl clisp) t
            (char-code char)
            (char-external-code char (charset encoding)))))

(defmethod get-char-metrics ((code integer) font (encoding single-byte-encoding))
  (let ((char (code-char code)))
  (aref (characters font)
        (if #+lispworks (lw:base-char-p char)
              #+(or allegro sbcl clisp) (standard-char-p char)
              #-(or lispworks allegro sbcl clisp) t
            code
              (char-external-code char (charset encoding))))))

#+unused
(defun get-char (code font)
  (aref (characters font) code))

#+unused
(defmacro force-char-code (char-or-code)
  (let ((char (gensym "char")))
    `(let ((,char ,char-or-code))
      (if (characterp ,char) (char-code ,char) ,char))))

(defun get-char-width (char-or-code font &optional font-size)
  (let ((char-metrics (get-char-metrics char-or-code font (encoding font))))
    (if font-size (* (width char-metrics) font-size) (width char-metrics))))

(defun get-char-size (char-or-code font &optional font-size)
  (let* ((char-metrics (get-char-metrics char-or-code font (encoding font)))
	 (width (width char-metrics))
	 (bbox (bbox char-metrics))
	 (ascender (aref bbox 3))
	 (descender (aref bbox 1)))
    (if font-size
	(values (* width font-size)(* ascender font-size)(* descender font-size))
	(values width ascender descender))))

(defun get-char-italic-correction (char-or-code font &optional font-size)
  (let* ((char-metrics (get-char-metrics char-or-code font (encoding font)))
	 (left (left-italic-correction char-metrics))
	 (right (right-italic-correction char-metrics)))
    (if font-size
	(values (* left font-size)(* right font-size))
	(values left right))))

(defun get-font-italic-correction (font &optional font-size)
  (let* ((italic-sin (italic-sin (font-metrics font)))
	 (left (* italic-sin (ascender (font-metrics font))))
	 (right (* italic-sin (descender (font-metrics font)))))
    (if font-size
	(values (* left font-size)(* right font-size))
	(values left right))))

(defun get-kerning (char1 char2 font &optional font-size)
  (let* ((encoding (encoding font))
         (char-metrics1 (get-char-metrics char1 font encoding))
         (char-metrics2 (get-char-metrics char2 font encoding))
         (kerning (gethash (+ (ash (code char-metrics1) 16) (code char-metrics2))
                           (kernings font)
                           0)))
    (if font-size (* font-size kerning) kerning)))

(defun get-font (&optional (name "helvetica") (encoding *default-encoding*))
  (setf name (string-downcase name))
  (let ((font-metrics (gethash name *font-metrics*)))
    (when (typep font-metrics 'ttu-font-metrics)
      (setf encoding *unicode-encoding*)))
  (let ((font (gethash (list name (get-encoding encoding)) *font-cache*)))
    (if font
	font
	(make-instance 'font :name name :encoding encoding))))

(defun clear-font-cache ()
  (clrhash *font-cache*))

(defvar %fonts-loaded% nil)

(defun load-fonts (&optional force)
  (when (or (not %fonts-loaded%) force)
    (dolist (font-dir *afm-files-directories*)
      (map nil 'read-afm-file (directory (merge-pathnames font-dir "*.afm"))))
    (clear-font-cache)
    (setf %fonts-loaded% t)))

(eval-when (:load-toplevel :execute)
  (load-fonts))
