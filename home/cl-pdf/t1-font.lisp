;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)

(defconstant +pfb-marker+ 128)
(defconstant +pfb-ascii+ 1)
(defconstant +pfb-binary+ 2)
(defconstant +pfb-done+ 3)

(defclass t1-font-metrics (font-metrics)
  ((binary-data :accessor binary-data :initform nil)
   (length1 :accessor length1)
   (length2 :accessor length2)
   (length3 :accessor length3)))

(defun read-pfb-length (data start)
  (let ((length (aref data start)))
    (setf (ldb (byte 8 8) length) (aref data (1+ start)))
    (setf (ldb (byte 8 16) length) (aref data (+ start 2)))
    (setf (ldb (byte 8 24) length) (aref data (+ start 3)))
    length))

(defun read-pfb-seg-size (data start marker)
  (assert (and (= (aref data start) +pfb-marker+)(= (aref data (1+ start)) marker)))
  (values (+ start 6) (read-pfb-length data (+ start 2))))

(defun read-pfb-file (pathname t1fm)
  (let (data length start1 length1 start2 length2 start3 length3 binary-data)
    (with-open-file (s pathname :direction :input :element-type '(unsigned-byte 8))
      (setf length (file-length s))
      (setf data (make-array length :element-type '(unsigned-byte 8)))
      (read-sequence data s))
    (setf (values start1 length1) (read-pfb-seg-size data 0 +pfb-ascii+))
    (setf (values start2 length2) (read-pfb-seg-size data (+ start1 length1) +pfb-binary+))
    (setf (values start3 length3) (read-pfb-seg-size data (+ start2 length2) +pfb-ascii+))
    (assert (<= (+ start3 length3) length))
    (setf binary-data (make-array (+ length1 length2 length3) :element-type '(unsigned-byte 8)))
    (setf (subseq binary-data 0 length1)(subseq data start1 (+ start1 length1)))
    (setf (subseq binary-data length1 (+ length1 length2))
	  (subseq data start2 (+ start2 length2)))
    (setf (subseq binary-data (+ length1 length2)(+ length1 length2 length3))
	  (subseq data start3 (+ start3 length3)))
    (setf (binary-data t1fm) binary-data
	  (length1 t1fm) length1
	  (length2 t1fm) length2
	  (length3 t1fm) length3)))

(defun load-t1-font (afm-file  &optional pfb-file)
  (let ((t1fm (read-afm-file afm-file 't1-font-metrics)))
    (when pfb-file
      (read-pfb-file pfb-file t1fm))
    t1fm))

(defmethod font-descriptor ((t1fm t1-font-metrics) &key (embed *embed-fonts*) &allow-other-keys)
  (flet ((conv-dim (d) (round (* 1000 d))))
    (make-instance 'indirect-object :content
      (make-instance 'dictionary  ;:obj-number 0 :no-link t
        :dict-values        
	`(("/Type" . "/FontDescriptor")
	  ("/FontName"  . ,(add-/ (font-name t1fm)))
	  ;; 4=Symbolic - contains characters outside the standard Latin character set.
	  ("/Flags" . 4)
	  ("/FontBBox" . ,(map 'vector #'conv-dim (font-bbox t1fm)))
	  ("/ItalicAngle" . ,(conv-dim (italic-angle t1fm)))
	  ("/Ascent" . ,(conv-dim (ascender t1fm)))
	  ("/Descent" . ,(conv-dim (descender t1fm)))
	  ("/CapHeight" . ,(conv-dim (cap-height t1fm)))
	  ("/XHeight" . ,(conv-dim (x-height t1fm)))
	  ("/StemV" . ,10)
          ;; When binary-data is not available, don't embded.
          ,@(when (and embed (binary-data t1fm))
              `(("/FontFile" . ,(make-instance 'indirect-object :content
                                  (make-instance 'pdf-stream
                                    :content (binary-data t1fm)
                                    :no-compression (not *compress-fonts*)
                                    :dict-values `(;("/Type" . "/Pages") ;remove!
                                                   ("/Length1" . ,(length1 t1fm))
                                                   ("/Length2" . ,(length2 t1fm))
                                                   ("/Length3" . ,(length3 t1fm))
                                                   )))))))))))

;example of T1 font loading:
#+nil
(pdf:load-t1-font #P"/tmp/cmb10.afm" #P"/tmp/cmb10.pfb")
