;;; cl-pdf copyright 2002-2009 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)

(defparameter *version* 2.03)

(defparameter +pdf-header+ "%PDF-1.4")

(defvar *document*)
(defvar *outlines-stack*)
(defvar *root-page*)
(defvar *page*)
(defvar *page-number*)
(defvar *page-stream*)
(defvar *pdf-stream*)
(defvar *xrefs*)
(defvar *name-counter*)

(defun gen-name (prefix)
  (format nil "~a~d" prefix (incf *name-counter*)))

(defclass dictionary ()
  ((dict-values :accessor dict-values :initform nil :initarg :dict-values)))

(defun add-dict-value (dict name value)
  (push (cons name value)(dict-values dict)))

(defun get-dict-value (dict name)
  (when dict
   (cdr (assoc name (dict-values dict) :test #'string=))))

(defun change-dict-value (dict name value)
  (let ((key-val (assoc name (dict-values dict) :test #'string=)))
    (if key-val
	(setf (cdr key-val) value)
	(add-dict-value dict name value))))

(defun copy-dict (dict)
  (when dict
    (make-instance 'dictionary
		   :dict-values (copy-alist (dict-values dict)))))

(defclass pdf-stream (dictionary)
  ((content :accessor content :initform "" :initarg :content)
   (no-compression :accessor no-compression :initarg :no-compression :initform nil)))

(defmethod initialize-instance :after ((obj pdf-stream) &key empty &allow-other-keys)
  (unless empty
    (add-dict-value obj "/Length"
		    #'(lambda ()
			(let ((content (content obj)))
			  (if (consp content)
			      (reduce #'+ content :key #'length)
			      (length content)))))))

(defclass lazy-pdf-stream (pdf-stream) ())

(defun compress-pdf-stream (pdf-stream)
;  #+use-no-zlib (declare (ignore pdf-stream))
;  #-use-no-zlib
  (when (and *compress-streams* (not (no-compression pdf-stream))
	     (> (length (content pdf-stream)) *min-size-for-compression*))
    (setf (content pdf-stream) (compress-string (content pdf-stream)))
    (let ((filter (get-dict-value pdf-stream "/Filter")))
      (if filter
	  (change-dict-value pdf-stream "/Filter" (vector "/FlateDecode" filter))
	  (add-dict-value pdf-stream "/Filter" "/FlateDecode")))
    (setf (no-compression pdf-stream) t)))

(defclass document ()
  ((objects :accessor objects :initform nil)
   (root-page :accessor root-page :initform nil)
   (catalog :accessor catalog :initform nil)
   (outline-root :accessor outline-root :initform nil)
   (named-refs :accessor named-refs :initform (make-hash-table :test #'equal))
   (fonts :accessor fonts :initform '())
   (gstates :accessor gstates :initform '())
   (encodings :accessor encodings :initform '())
   (last-object-number :accessor last-object-number :initform 0)
   (docinfo :accessor docinfo :initform nil)
   (author :accessor author :initarg :author :initform nil)
   (title  :accessor title :initarg :title :initform nil)
   (keywords :accessor keywords :initarg :keywords :initform nil)
   (subject :accessor subject :initarg :subject :initform nil)))

(defmethod initialize-instance :after ((doc document)
				       &key empty mode layout
				            (creator "") author title subject keywords
                                       &allow-other-keys)
 ;;; Args: empty  If true, do not set any slots
  ;;       mode	  PageMode in catalog
  ;;       layout PageLayout in catalog
  (unless empty
    (let ((*document* doc))
      (setf (objects doc) (make-array 10 :fill-pointer 0 :adjustable t)
            (catalog doc) (make-instance 'indirect-object)
            (root-page doc) (make-instance 'page-node)
            (outline-root doc) (make-instance 'outline)
            (content (catalog doc))
	     (make-instance 'dictionary :dict-values
              `(("/Type" . "/Catalog")
                ("/Pages" . ,(root-page doc))
                ,@(when layout `(("/PageLayout" . ,(case layout
                                                     (:page    "/SinglePage")
                                                     (:column  "/OneColumn")
                                                     (:left    "/TwoColumnLeft")
                                                     (:right   "/TwoColumnRight")
                                                     (otherwise (pdf-name layout))))))
                ,@(when mode `(("/PageMode" . ,(case mode
                                                 (:none     "/UseNone")
                                                 (:outlines "/UseOutlines")
                                                 (:thumbs   "/UseThumbs")
                                                 (:full     "/FullScreen")
                                                 (otherwise (pdf-name mode)))))) )))
      (add-doc-info doc :creator creator :author author
		    :title title :subject subject :keywords keywords) )))

(defun add-doc-info (doc &key (creator "") author title subject keywords)
  (setf (docinfo doc) (make-instance 'indirect-object))
  (setf (content (docinfo doc))
	(make-instance 'dictionary
		       :dict-values `(("/Creator" . ,(format nil "(cl-pdf ~A - ~A)" *version* creator))
				      ,@(when author `(("/Author" . ,(format nil "(~A)" author))))
				      ,@(when title `(("/Title" . ,(format nil "(~A)" title))))
				      ,@(when subject `(("/Subject" . ,(format nil "(~A)" subject))))
				      ,@(when keywords `(("/Keywords" . ,(format nil "(~A)" keywords))))
				      ("/CreationDate" .
                                          ,(multiple-value-bind (second minute hour date month year)
					       (get-decoded-time)
					     (format nil "(D:~D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D)"
						     year month date hour minute second)))))))

(defclass indirect-object ()
  ((obj-number :accessor obj-number :initform (incf (last-object-number *document*)) :initarg :obj-number)
   (gen-number :accessor gen-number :initform 0 :initarg :gen-number)
   (content :accessor content :initform nil :initarg :content)))

(defmethod initialize-instance :after ((obj indirect-object)
				       &key no-link &allow-other-keys)
  (unless no-link
    (vector-push-extend obj (objects *document*))))

(defclass object-ref ()
  ((obj-number :accessor obj-number :initform 0 :initarg :obj-number)
   (gen-number :accessor gen-number :initform 0 :initarg :gen-number)))

(defclass page-node (indirect-object)
  ((pages :accessor pages :initform (make-array 1 :fill-pointer 0 :adjustable t))))

(defmethod initialize-instance :after ((obj page-node) &key no-link &allow-other-keys)
  (when (and *root-page* (not no-link))
    (vector-push-extend obj (pages *root-page*)))
  (setf (content obj) (make-instance 'dictionary
		       :dict-values `(("/Type" . "/Pages")
				      ("/Count" . ,#'(lambda ()(length (pages obj))))
				      ,@(when *root-page* `(("/Parent" . ,*root-page*)))
				      ("/Kids" . ,(pages obj))))))

(defclass page (indirect-object)
  ((bounds :accessor bounds :initform *default-page-bounds* :initarg :bounds)
   (resources :accessor resources :initform (make-instance 'dictionary))
   (fonts :accessor fonts :initform '())
   (font-objects :accessor font-objects :initform (make-instance 'dictionary))
   (gstates :accessor gstates :initform '())
   (gstate-objects :accessor gstate-objects :initform (make-instance 'dictionary))
   (xobjects :accessor xobjects :initform (make-instance 'dictionary))
   (annotations :accessor annotations :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (content-stream :accessor content-stream)
   ))

(defmethod initialize-instance :after ((page page)
				       &key no-link (rotate 0) &allow-other-keys)
  (when (and *root-page* (not no-link))
    (incf *page-number*)
    (when (> *page-number* *max-number-of-pages*)
      (throw 'max-number-of-pages-reached nil))
    (vector-push-extend page (pages *root-page*)))
  (setf (content-stream page) (make-instance 'pdf-stream))
  (add-dict-value (resources page) "/Font" (font-objects page))
  (add-dict-value (resources page) "/ExtGState" (gstate-objects page))
  (add-dict-value (resources page) "/ProcSet" "[ /PDF /Text ]")
  (add-dict-value (resources page) "/XObject" (xobjects page))
  (let ((content (make-instance 'indirect-object :content (content-stream page))))
    (setf (content page)
	  (make-instance 'dictionary
		:dict-values `(("/Type" . "/Page")
			       ("/Parent" ,*root-page*)
			       ("/MediaBox" . ,#'(lambda ()(bounds page)))
			       ("/Resources" . ,(resources page))
			       ("/Annots" ,(annotations page))
			       ("/Rotate" . ,rotate)
			       ("/Contents" . ,content))))))

(defclass named-reference ()
  ((name :accessor name :initarg :name)
   (reference :accessor reference :initform nil)))

(defun get-named-reference (name)
  (let ((ref (gethash name (named-refs *document*))))
    (unless ref
      (setf ref (make-instance 'named-reference :name name))
      (setf (gethash name (named-refs *document*)) ref))
    ref))

(defun register-named-reference (reference &optional (name (gen-name "R")))
  (setf (reference (get-named-reference name)) reference)
  name)

(defun register-page-reference (&optional (name (gen-name "R")))
  (register-named-reference (vector *page* "/Fit") name)
  name)

(defclass outline (indirect-object)
  ((title :accessor title :initarg :title :initform nil)
   (reference :accessor reference :initform nil :initarg :reference)
   (sub-levels :accessor sub-levels :initform nil)
   (prev-outline :accessor prev-outline :initform nil)
   (next-outline :accessor next-outline :initform nil)))

(defun enter-outline-level (title ref-name)
  (let ((outline (make-instance 'outline :title title :reference (get-named-reference ref-name)))
	(parent (first *outlines-stack*)))
    (setf (sub-levels parent)(nconc (sub-levels parent)(list outline)))
    (push outline *outlines-stack*)))

(defun close-outline-level ()
  (pop *outlines-stack*))

(defmacro string-append (&rest args)
  #+lispworks `(lw:string-append ,@args)
  #-lispworks `(concatenate 'string ,@args))

(defun pdf-string (obj &key (unicode :default))
 ;;; Used to embrace a pdf string used in places other than content streams,
  ;; e.g. titles, annotations etc.
  ;; Args: unicode  If true or defaults to true, the PDF text string is encoded in Unicode
  ;;	   lang ?
  ;; Q: Rename or create separate pdf-text-string?
  (let ((string (if (stringp obj) obj (princ-to-string obj))))
    (when (eq unicode :default)
      (setq unicode (notevery #+lispworks #'lw:base-char-p
                              #-lispworks (lambda (char) (<= (char-code char) 255))
                              string)))
    (with-output-to-string (stream nil :element-type 'base-char)
      (write-char #\( stream)
      (when unicode			; write the Unicode byte order marker U+FEFF
        (write-char #.(code-char 254) stream) (write-char #.(code-char 255) stream))
      (loop for char across string
            for code = (char-code char)
            if unicode
            do (write-char (code-char (ldb (byte 8 8) code)) stream)	; hi
               (write-char (code-char (ldb (byte 8 0) code)) stream)	; lo
            else if (> code 255)
            do (write-char (code-char (ldb (byte 8 0) code)) stream)	; lo
            else do (case char ((#\( #\) #\\)
                                (write-char #\\ stream)))
                      (write-char char stream))
      (write-char #\) stream))))

(defmacro with-outline-level ((title ref-name) &body body)
 `(unwind-protect
   (progn (enter-outline-level ,title ,ref-name)
	  ,@body)
   (close-outline-level)))

(defun compute-outline-tree (outlines &optional (parent nil))
  (loop for prev = nil then outline
	for outline in outlines
	do
	(when prev (setf (next-outline prev) outline))
	(setf (prev-outline outline) prev)
	(compute-outline-tree (sub-levels outline) outline))
  (loop for outline in outlines
	for sub-levels = (sub-levels outline)
	for first = (first sub-levels)
	for last = (first (last sub-levels))
	do
	(with-slots ((reference reference)(prev-outline prev-outline)(next-outline next-outline)) outline
	  (setf (content outline)
		(make-instance 'dictionary
  		   :dict-values `(,@(if parent `(("/Title" . ,(pdf-string (title outline)))
						 ("/Parent" . ,parent))
					'(("/Type" "/Outlines")))
				  ,@(when first `(("/First" . ,first)))
				  ,@(when last `(("/Last" . ,last)))
				  ,@(when prev-outline `(("/Prev" . ,prev-outline)))
				  ,@(when next-outline `(("/Next" . ,next-outline)))
				  ,@(when reference `(("/Dest" . ,reference)))
				  ("/Count" . "0")))))))

(defun process-outlines (document)
  (when (and (outline-root document) (sub-levels (outline-root document)))
    (compute-outline-tree (list (outline-root document)))
    (add-dict-value (content (catalog document)) "/Outlines" (outline-root document))))

(defun pdf-name (obj &optional (prefix #\/))
  "Helper (akin to pdf-string) to escape non-alphanumeric characters in PDF names
   by writing 2-digit hexadecimal code, preceded by the number sign character (#).
   CAUTION: PDF names are case-sensitive!"
  (let ((string (if (stringp obj)
                    (if (and prefix (char= (schar obj 0) prefix))
                        (return-from pdf-name obj) ; PDF-ready
                        obj)
                    (princ-to-string obj))))
    (with-output-to-string (stream nil #-cmu :element-type #-cmu (array-element-type string))
      (when prefix
        (write-char prefix stream))
      (dotimes (i (length string))
        (let ((char (schar string i)))
          (if (or (alphanumericp char)
                  (find char "-_." :test #'char=)) ; often used regular chars
              (write-char char stream)
              (format stream "#~2,'0x" (char-code char))))))))

(defmacro enforce-/ (&rest names)
  "Verify and prefix each name by / unless it is PDF-ready."
  `(setf ,@(loop for name in names
                 collect name
                 collect `(pdf-name ,name))))

(defun add-/ (name)
  (concatenate 'string "/" name))

(defclass encoding-object (indirect-object)
  ((encoding :accessor encoding :initarg :encoding)))

(defmethod initialize-instance :after ((encoding-object encoding-object)
                                       &key encoding &allow-other-keys)
  (setf (content encoding-object) (make-dictionary encoding)))

(defun find-encoding-object (encoding)
  (let ((encoding-object (cdr (assoc encoding (encodings *document*)))))
    (unless encoding-object
      (setf encoding-object (make-instance 'encoding-object :encoding encoding))
      (push (cons encoding encoding-object) (encodings *document*)))
    encoding-object))

(defclass font-object (indirect-object)
  ((name :accessor name :initform (gen-name "/CLF") :initarg :name)
   (font :accessor font :initarg :font)))

(defmethod initialize-instance :after ((font-object font-object)
                                       &key font (embed *embed-fonts*)
                                       &allow-other-keys)
  (setf (content font-object) (make-dictionary (font-metrics font)
                                               :font font :embed embed)))

(defun find-font-object (font &key (embed :default))
  (let ((font-object (cdr (assoc font (fonts *document*)))))
    (unless font-object
      (setf font-object (make-instance 'font-object :font font :embed embed))
      (push (cons font font-object) (fonts *document*)))
    font-object))

(defun add-font-to-page (font &key (embed :default))
  (let ((font-object (cdr (assoc font (fonts *page*)))))
    (unless font-object
      (setf font-object (find-font-object font :embed embed))
      (push (cons font font-object) (fonts *page*))
      (add-dict-value (font-objects *page*) (name font-object) font-object))
    font-object))

(defclass gstate-object (indirect-object)
  ((name :accessor name :initform (gen-name "/GS") :initarg :name)))

(defmethod initialize-instance :after ((gstate-object gstate-object) &key gstate &allow-other-keys)
  (setf (content gstate-object) (make-instance 'dictionary :dict-values '(("/Type" . "/ExtGState"))))
  (loop for (name value) on gstate by #'cddr
	do (add-dict-value (content gstate-object) (format nil "/~a" name) value)))

(defun find-gstate-object (&rest gstate)
  (let ((gstate-object (cdr (assoc gstate (gstates *document*) :test #'equal))))
    (unless gstate-object
      (setf gstate-object (make-instance 'gstate-object :gstate gstate))
      (push (cons gstate gstate-object) (gstates *document*)))
    gstate-object))

(defun add-gstate-to-page (&rest gstate)
  (let ((gstate-object (cdr (assoc gstate (gstates *page*) :test #'equal))))
    (unless gstate-object
      (setf gstate-object (apply #'find-gstate-object gstate))
      (push (cons gstate gstate-object) (gstates *page*))
      (add-dict-value (gstate-objects *page*) (name gstate-object) gstate-object))
    gstate-object))

(defclass image (indirect-object)
  ((name  :accessor name  :initform (gen-name "/CLI") :initarg :name)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (filename :accessor filename :initarg :filename)))

(defmethod initialize-instance :after ((image image) &key
				       bits width height (filter "ASCIIHexDecode") decode-parms
				       (color-space "DeviceRGB") (bits-per-color 8) mask decode
				       no-compression
				       &allow-other-keys)
 ;;; Args: color-space - can be an array!
  (enforce-/ filter) ; color-space)
  (when (stringp color-space)
    (enforce-/ color-space))
  (setf (content image)
	(make-instance (if (functionp bits) 'lazy-pdf-stream 'pdf-stream)
	       :no-compression no-compression
	       :dict-values `(("/Type" . "/XObject")("/Subtype" . "/Image")
			      ("/Width" . ,width)("/Height" . ,height)
			      ("/Filter" . ,filter)
                              ,@(when decode-parms `(("/DecodeParms" .
                                                      ,(make-instance 'dictionary
                                                         :dict-values decode-parms))))
			      ("/ColorSpace" . ,color-space)
			      ("/BitsPerComponent" . ,bits-per-color)
                              ,@(when decode `(("/Decode" . ,decode)))
                              ,@(when mask `(("/Mask" . ,mask))) )))
  (setf (content (content image)) bits))

(defun add-images-to-page (&rest images)
  (dolist (image images)
    (add-dict-value (xobjects *page*) (name image) image)))

(defclass annotation (indirect-object)
  ())

(defmethod initialize-instance :after ((annotation annotation) &key
				       rect type (border #(0 0 0))
				       &allow-other-keys)
  (enforce-/ type)
  (vector-push-extend annotation (annotations *page*))
  (setf (content annotation)
	(make-instance 'dictionary
		       :dict-values `(("/Type" . "/Annot")("/Subtype" . ,type)
				      ("/Rect" . ,rect)("/Border" . ,border)))))

(defclass annotation2 (indirect-object)
  ())

(defmethod initialize-instance :after ((annotation annotation2) &key rect text
				       &allow-other-keys)
  (vector-push-extend annotation (annotations *page*))
  (setf (content annotation)
	(make-instance 'dictionary
		       :dict-values `(("/Type" . "/Annot")("/Subtype" . "/Text")
				      ("/Rect" . ,rect)("/Contents" . ,text)))))

(defgeneric write-object (obj &optional root-level))

(defmethod write-object ((obj null) &optional root-level)
  (declare (ignorable root-level))
  (write-string "null" *pdf-stream*))

(defmethod write-object ((obj dictionary) &optional root-level)
  (declare (ignorable root-level))
  (write-string "<< " *pdf-stream*)
  (loop for (key . val) in (dict-values obj)
	when val do
	(write-string key *pdf-stream*)
	(write-char #\Space *pdf-stream*)
	(write-object val)
	(write-char #\Newline *pdf-stream*))
  (write-line " >>" *pdf-stream*))

(defgeneric write-stream-content (content))

(defmethod write-stream-content ((content string))
  ;; Args: content Base string, may include
  ;;	   - either one-byte codes (already converted to external format if needed)
  ;;	   - or Unicode two-byte character codes (big-endian CIDs)
  #+pdf-binary
  (loop for char across content
        do (write-byte (ldb (byte 8 0) (char-code char)) *pdf-stream*))
  #-pdf-binary
  (write-sequence content *pdf-stream*))

(defmethod write-stream-content ((obj sequence))
  #+pdf-binary
  (write-sequence obj *pdf-stream*)
  #-pdf-binary
  (loop for c across obj do
	(write-char (code-char c) *pdf-stream*)))

(defmethod write-stream-content ((obj list))
  (map nil 'write-stream-content obj))

(defmethod write-object ((obj pdf-stream) &optional root-level)
  (declare (ignorable root-level))
  (compress-pdf-stream obj)
  (call-next-method)
  (write-line "stream" *pdf-stream*)
  (write-stream-content (content obj))
  (write-char #\Newline *pdf-stream*)
  (write-line "endstream" *pdf-stream*))

(defmethod write-object ((obj lazy-pdf-stream) &optional root-level)
  (declare (ignorable root-level))
  (let ((dstream (make-instance 'pdf-stream 
				:content (funcall (content obj))
				:dict-values (dict-values obj))))
    (write-object dstream root-level)))

(defmethod write-object ((obj object-ref) &optional root-level)
  (declare (ignorable root-level))
  (format *pdf-stream* "~d ~d R" (obj-number obj)(gen-number obj)))

(defmethod write-object ((obj indirect-object) &optional root-level)
  (if root-level
    (progn
      (vector-push-extend (format nil "~10,'0d ~5,'0d n "
				  (file-position *pdf-stream*)(gen-number obj))
			  *xrefs*)
      (format *pdf-stream* "~d ~d obj~%" (obj-number obj)(gen-number obj))
      (when (content obj)(write-object (content obj)))
      (write-string " endobj" *pdf-stream*)
      (write-char #\Newline *pdf-stream*))
    (format *pdf-stream* "~d ~d R" (obj-number obj)(gen-number obj))))

(defmethod write-object ((list list) &optional root-level)
  (declare (ignorable root-level))
  (dolist (obj list)
    (write-object obj)
    (write-char #\Space *pdf-stream*)))

(defmethod write-object ((obj string) &optional root-level)
  (declare (ignorable root-level))
  #+(and lispworks pdf-binary)
  (if (lw:text-string-p obj)		; may include unicode characters
      (loop for char across obj
            if (lw:base-char-p char)
            do (write-char char *pdf-stream*)
            else do (write-byte (char-external-code char *default-charset*) *pdf-stream*))
      (write-string obj *pdf-stream*))
  #-(and lispworks pdf-binary)
  (write-string obj *pdf-stream*))

(defmethod write-object ((obj symbol) &optional root-level)
  (declare (ignorable root-level))
  (write-string (symbol-name obj) *pdf-stream*))

(defmethod write-object ((obj function) &optional root-level)
  (declare (ignorable root-level))
  (write-object (funcall obj)))

(defmethod write-object ((obj number) &optional root-level)
  (declare (ignorable root-level))
  (if (integerp obj)
      (princ obj *pdf-stream*)
      ;; rationals and such aren't allowed.
      (format *pdf-stream* "~,3f" obj)))

(defmethod write-object ((obj t) &optional root-level)
  (declare (ignorable root-level))
  (princ obj *pdf-stream*))

(defmethod write-object ((array array) &optional root-level)
  (declare (ignorable root-level))
  (write-string "[ " *pdf-stream*)
  (loop for obj across array do
	(write-object obj)
	(write-char #\Space *pdf-stream*))
  (write-char #\] *pdf-stream*))

(defmethod write-object ((obj named-reference) &optional root-level)
  (declare (ignorable root-level))
  (write-object (reference obj)))

(defgeneric write-document (target &optional document))

(defmethod write-document ((s stream) &optional (document *document*))
   (let ((*xrefs* (make-array 10 :adjustable t :fill-pointer 0))
	 startxref
	 (*pdf-stream* s))
     (with-standard-io-syntax
       (process-outlines document)
       (vector-push-extend "0000000000 65535 f " *xrefs*)
       (write-line +pdf-header+ *pdf-stream*)
       (loop for obj across (objects document)
	     for first = t then nil
	     if obj do (write-object obj t)
	     else do (unless first (vector-push-extend "0000000000 00001 f " *xrefs*)))
       (setf startxref (file-position s))
       (format *pdf-stream* "xref~%0 ~d~%" (length *xrefs*))
       (loop for xref across *xrefs*
	     do (write-line xref s))
       (format s "trailer~%<< /Size ~d~%/Root " (length *xrefs*));(1- (length (objects document))))
       (write-object (catalog document))
       (when (docinfo document)
	 (format s " /Info ")
	 (write-object (docinfo document)))
       (format s "~%>>~%startxref~%~d~%%%EOF~%" startxref))))

#-(or allegro ecl)
(defmethod write-document ((filename pathname) &optional (document *document*))
   (with-open-file (stream filename
                           :direction :output :if-exists :supersede
                           :element-type #+pdf-binary #+sbcl :default #-sbcl'(unsigned-byte 8)
                                         #-pdf-binary 'base-char
                           :external-format +external-format+)
     (write-document stream document)
     filename))				; indicate that operation succeeded

#+(or allegro ecl)
(defmethod write-document ((filename pathname) &optional (document *document*))
   (with-open-file (stream filename
                           :direction :output :if-exists :supersede
			   ;; when :element-type is not specified, simple-stream is created
                           :external-format +external-format+)
     (write-document stream document)
     filename))				; indicate that operation succeeded

(defmethod write-document ((filename string) &optional (document *document*))
  (write-document (pathname filename) document))

(defmacro with-document ((&rest args &key (max-number-of-pages '*max-number-of-pages*) &allow-other-keys)
			 &body body)
  `(let* ((*root-page* nil)
          (*document* (make-instance 'document ,@args))
	  (*outlines-stack* (list (outline-root *document*)))
	  (*page* nil)
	  (*page-number* 0)
          (*name-counter* 100)
	  (*max-number-of-pages* ,max-number-of-pages))
    (setf *root-page* (root-page *document*))
    (catch 'max-number-of-pages-reached
      ,@body)))

(defmacro with-page ((&rest args) &body body)
  `(let* ((*page* (make-instance 'page ,@args)))
    (with-standard-io-syntax
	(setf (content (content-stream *page*))
	 (with-output-to-string (*page-stream*)
	   ,@body)))
     t))
