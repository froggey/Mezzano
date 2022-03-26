;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

;;;
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;;

;;; Support for TrueTypeUnicode fonts

(in-package #:pdf)

(defclass ttu-font-metrics (font-metrics)
  ((c2g :accessor c2g
	:initform (make-array 131072 :element-type 'character :initial-element #\Null))
   (cid-widths :accessor cid-widths :initform (make-array 0 :adjustable t :fill-pointer 0))
   (pdf-widths :accessor pdf-widths :initform nil)
   (binary-data :accessor binary-data :initform nil)
   (min-code :accessor min-code :initform 0)
   (max-code :accessor max-code :initform 0)
   (length1 :accessor length1)))

(defmethod font-type ((fm ttu-font-metrics))
  "Type0")

(defun load-ttu-font (ufm-file &optional ttf-file)
  (let ((ttufm (read-ufm-file ufm-file 'ttu-font-metrics)))
    (when ttf-file
      (with-open-file (in ttf-file :direction :input :element-type '(unsigned-byte 8))
	(setf (length1 ttufm)
	      (file-length in)
	      (binary-data ttufm)
	      (make-array (length1 ttufm) :element-type '(unsigned-byte 8)))
	(read-sequence (binary-data ttufm) in)))
    ttufm))

;;; example: (pdf:load-ttu-font #P"/tmp/arial.ufm" #P"/tmp/arial.ttf")

(defmethod font-descriptor ((fm ttu-font-metrics) &key (embed *embed-fonts*) &allow-other-keys)
  (flet ((conv-dim (d) (round (* 1000 d))))
    (make-instance
     'indirect-object
     :content
     (make-instance
      'dictionary ; :obj-number 0 :no-link t
      :dict-values
      `(("/Type" . "/FontDescriptor")
	("/FontName"  . ,(add-/ (font-name fm)))
	("/Flags"
	 . ,(logior
	     (if (fixed-pitch-p fm) 1 0)
	     ;; 4 ? non-ascii present
	     32
	     (if (< 0 (italic-angle fm)) 64 0)))
	("/FontBBox" . ,(map 'vector #'conv-dim (font-bbox fm)))
	("/ItalicAngle" . ,(conv-dim (italic-angle fm)))
	("/Ascent" . ,(conv-dim (ascender fm)))
	("/Descent" . ,(conv-dim (descender fm)))
	("/CapHeight" . ,(conv-dim (cap-height fm)))
	("/XHeight" . ,(conv-dim (x-height fm)))
	("/StemV" . ,10)
	,@(when (and embed (binary-data fm))
	    `(("/FontFile2"
	       . ,(make-instance
		   'indirect-object
		   :content
		   (make-instance
		    'pdf-stream
		    :content (binary-data fm)
		    :no-compression (not *compress-fonts*)
		    :dict-values `(("/Length1" . ,(length1 fm)))))))))))))

(defclass cid-font ()
  ((base-font :accessor base-font :initarg :base-font)
   (descriptor :accessor descriptor :initarg :descriptor)
   (widths :accessor widths :initarg :widths)
   (c2g :accessor c2g :initarg :c2g)))

(defmethod make-dictionary ((font cid-font) &key &allow-other-keys)
  (make-instance
   'dictionary
   :dict-values
   `(("/Type" . "/Font")
     ("/Subtype" . "/CIDFontType2")
     ("/BaseFont" . ,(add-/ (base-font font)))
     ("/CIDSystemInfo"
      . ,(make-instance
	  'dictionary
	  :dict-values
	  `(("/Registry" . ,(pdf-string "Adobe"))
	    ("/Ordering" . ,(pdf-string "UCS"))
	    ("/Supplement" . 0))))
     ("/FontDescriptor" . ,(descriptor font))
     ("/W" . ,(widths font))
     ("/CIDToGIDMap"
      . ,(make-instance
	  'indirect-object
	  :content
	  (make-instance
	   'pdf-stream
	   :content (c2g font)
	   :no-compression (not *compress-fonts*)))))))

(defmethod make-dictionary ((fm ttu-font-metrics)
                            &key font (encoding (encoding font)) (embed *embed-fonts*))
  (declare (ignore encoding))
  (let* ((font-descriptor (font-descriptor fm :embed embed :errorp nil))
	 (cid-font (make-instance
		    'cid-font
		    :base-font (font-name fm)
		    :descriptor font-descriptor
		    :widths (cid-widths fm)
		    :c2g (c2g fm))))
    (make-instance
     'dictionary
     :dict-values
     `(("/Type" . "/Font")
       ("/Subtype" . ,(add-/ (font-type fm)))
       ("/BaseFont" . ,(add-/ (concatenate 'string (font-name fm) "-UCS")))
       ("/Encoding" . "/Identity-H")
       ;; TODO shouldn't it be this? if not, then delete encoding keyword argument...
       #+nil("/Encoding" . (if (standard-encoding encoding)
                          (add-/ (name encoding))
                          (find-encoding-object encoding)))
       ("/DescendantFonts"
	. ,(vector
	    (make-instance
	     'indirect-object
	     :content (make-dictionary cid-font))))))))
