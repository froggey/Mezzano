;;; cl-pdf copyright 2002-2009 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)

;;Many thanks to Alexey Dejneka (adejneka@comail.ru) who finished the parsing of the AFM files.

(defvar *font-metrics* (make-hash-table :test #'equal))

(defgeneric font-descriptor (font-metrics &key embed errorp))

(defgeneric font-type (font-metrics))

(defclass char-metrics ()
  ((code  :accessor code  :initarg :code)
   (name  :accessor name  :initarg :name)
   (index :accessor index  :initarg :index)
   (width :accessor width :initarg :width)
   (spacing :accessor spacing :initarg :spacing)
   (right-italic-correction :accessor right-italic-correction :initarg :right-italic-correction)
   (left-italic-correction :accessor left-italic-correction :initarg :left-italic-correction)
   (bbox  :accessor bbox  :initarg :bbox)))

(defmethod print-object ((self char-metrics) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a" (name self))))

(defclass font-metrics ()
  ((font-name :accessor font-name)
   (full-name :accessor full-name)
   (family-name :accessor family-name)
   (weight :accessor weight)
   (underline-position :accessor underline-position :initform 0)
   (underline-thickness :accessor underline-thickness :initform 0)
   (italic-angle :accessor italic-angle :initform 0)
   (italic-sin :accessor italic-sin :initform 0)
   (fixed-pitch-p :accessor fixed-pitch-p :initform nil)
   (char-width :accessor char-width :initform nil)
   (font-bbox :accessor font-bbox)
   (version :accessor version)
   (notice :accessor notice)
   (encoding-scheme :accessor encoding-scheme)
   (encoding-vector :accessor encoding-vector :initform (make-array 256 :initial-element nil))
   (characters :accessor characters :initform (make-hash-table :test #'equal))
   (mapping-scheme :accessor mapping-scheme)
   (esc-char :accessor esc-char)
   (character-set :accessor character-set)
   (base-font-p :initform t :accessor base-font-p)
   (vvector :accessor vvector)
   (fixed-v-p :accessor fixed-v-p)
   (cap-height :accessor cap-height :initform 1)
   (x-height :accessor x-height :initform 0.5)
   (ascender :accessor ascender :initform 1)
   (descender :accessor descender :initform 0)
   (leading :accessor leading :initform 1)
;   (char-metrics :accessor char-metrics)
   (kernings :accessor kernings :initform (make-hash-table :test #'equal))))

(defmethod print-object ((self font-metrics) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~a" (full-name self))))

;;; Utilities
(defmacro mcond (&rest clauses &environment env)
  "An analog of COND, but MACROEXPANDs every clause."
  `(cond ,@(mapcar (lambda (clause)
                     (loop
                        (unless (and (consp clause)
                                     (symbolp (first clause))
                                     (multiple-value-bind (expansion expanded-p)
                                         (macroexpand-1 clause env)
                                       (setq clause expansion)
                                       expanded-p))
                          (return clause))))
                   clauses)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro with-gensyms ((&rest names) &body body)
  `(let (,@(mapcar (lambda (sym) `(,sym (gensym ,(symbol-name sym)))) names))
     ,@body)))

;;; Parser
(defun whitespace-p (char)
  (or (char= char #\Space) (char= char #\Tab)))

(defun get-afm-string (line start)
  (declare (type string line))
  (let* ((length (length line))
         (start (or (position-if-not #'whitespace-p line :start start) length)))
    (values (subseq line start) length)))

(defun get-afm-integer (line start)
  (declare (type string line))
  (parse-integer line :start start :junk-allowed t))

(defun get-afm-hex (line start)
  (declare (type string line))
  (let ((length (length line))
        (start (position-if-not #'whitespace-p line :start start)))
    (unless (and start (<= start (- length 2))
                 (char= (aref line start) #\<)
                 (digit-char-p (aref line (1+ start)) 16))
      (error 'parse-error))
    (multiple-value-bind (value position)
        (parse-integer line :start (1+ start) :radix 16 :junk-allowed t)
      (unless (and value
                   (< position length)
                   (char= (aref line position) #\>))
        (error 'parse-error))
      (values value (1+ position)))))

(defun get-afm-number (line start)
  (declare (type string line))
  (multiple-value-bind (value position)
      (parse-integer line :start start :junk-allowed t)
    (cond ((not value) (error 'parse-error))
          ((or (= position (length line))
               (whitespace-p (aref line position)))
           (values value position))
          ((not (char= (aref line position) #\.))
           (error 'parse-error))
          ((= (incf position) (length line)) (values value position))
          ((not (or (digit-char-p (aref line position))
                    (whitespace-p (aref line position))))
           (error 'parse-error))
          (t (multiple-value-bind (fraction end)
                 (parse-integer line :start position :junk-allowed t)
               (values (+ value (*  (signum value)
                                    (/ fraction (expt 10.0 (- end position)))))
                       end))))))

(defun get-afm-name (line start)
  (declare (type string line))
  (let ((name-start (position-if-not #'whitespace-p line :start start)))
    (if name-start
        (let ((name-end (or (position-if #'whitespace-p line :start name-start)
                            (length line))))
          (values (subseq line name-start name-end)
                  name-end))
        nil)))

(defun get-afm-boolean (line start)
  (multiple-value-bind (word pos) (get-afm-name line start)
    (values (cond ((string= word "true") t)
                  ((string= word "false") nil)
                  (t (error 'parse-error)))
            pos)))

(defmacro define-afm-section ((name afm-name)(stream &rest args) &body body)
  (with-gensyms (file-line line keyword position start value new-position)
    `(defun ,name (,stream ,@args)
       (macrolet ((process-keywords (&rest clauses)
                    `(loop
                        for ,',file-line = (or (read-line ,',stream nil nil)
                                               (error "Unclosed AFM section ~S."
                                                      ,',afm-name))
                        for ,',line = (string-trim '(#\space #\newline #\return)
                                                   ,',file-line)
                        do (multiple-value-bind (,',keyword ,',position)
                               (get-afm-name ,',line 0)
                             (declare (ignorable ,',position))
                             (when ,',keyword
                               (mcond ,@clauses)))))
                  (process-keywords-in-line (&rest clauses)
                    `(loop
                        for ,',start = -1 then (position #\; ,',line :start ,',position)
                        for ,',position = (and ,',start
                                               (position-if-not #'whitespace-p
                                                                ,',line
                                                                :start (1+ ,',start)))
                        while ,',position
                        do (multiple-value-bind (,',keyword ,',position)
                               (get-afm-name ,',line ,',position)
                             (declare (ignorable ,',position))
                             (when ,',keyword
                               (mcond ,@clauses)))))
                  (get-object-of-type (type)
                    `(multiple-value-bind (,',value ,',new-position)
                         (,(read-from-string (format nil "get-afm-~A" (string-downcase type))) ,',line ,',position)
                       (setq ,',position ,',new-position)
                       ,',value))
                  (key (key arglist &body body)
                    (loop for (name type) in arglist
                       collect `(,name (get-object-of-type ,type)) into bindings
                       finally (return `((string= ,',keyword ,key)
                                         (let ,bindings
                                           ,@body))))))
         ,@body))))

(define-afm-section (afm-font-metrics "FontMetrics") (stream font-metrics-class)
  (let ((font-metrics (make-instance font-metrics-class)))
    (macrolet ((named-parameter (key type param)
                 `(key ,key ((,param ,type)) (setf (,param font-metrics) ,param)))
	       (scaled-parameter (key type param)
                 `(key ,key ((,param ,type)) (setf (,param font-metrics) (* 0.001 ,param)))))
      (process-keywords
       (key "EndFontMetrics" ()
	    (setf (gethash (string-downcase (font-name font-metrics)) *font-metrics*) font-metrics
;		  (gethash (string-downcase (full-name font-metrics)) *font-metrics*) font-metrics
		  (leading font-metrics)(- 1 (descender font-metrics))
		  (italic-sin font-metrics)(sin (/ (* pi (italic-angle font-metrics)) -180)))
	    (return-from afm-font-metrics font-metrics))
       (named-parameter "FontName" string font-name)
       (named-parameter "FullName" string full-name)
       (named-parameter "FamilyName" string family-name)
       (named-parameter "Weight" string weight)
       (key "FontBBox" ((llx number) (lly number) (urx number) (ury number))
            (setf (font-bbox font-metrics)
		  (vector (* 0.001 llx) (* 0.001 lly) (* 0.001 urx) (* 0.001 ury))))
       (named-parameter "Version" string version)
       (named-parameter "Notice" string notice)
       (named-parameter "EncodingScheme" string encoding-scheme)
       (named-parameter "MappingScheme" integer mapping-scheme)
       (named-parameter "EscChar" integer esc-char)
       (named-parameter "CharacterSet" string character-set)
       (named-parameter "Characters" integer characters)
       (named-parameter "IsBaseFont" boolean base-font-p)
       ;; vvector
       (named-parameter "IsFixedV" boolean fixed-v-p)
       (scaled-parameter "CapHeight" number cap-height)
       (scaled-parameter "XHeight" number x-height)
       (scaled-parameter "Ascender" number ascender)
       (scaled-parameter "Descender" number descender)
       (named-parameter "IsFixedPitch" boolean fixed-pitch-p)
       (key "CharWidth" ((x number) (y number))
            (setf (char-width font-metrics)(list (* 0.001 x) (* 0.001 y))
		  (fixed-pitch-p font-metrics) t))
       (named-parameter "ItalicAngle" number italic-angle)
       (scaled-parameter "UnderlinePosition" number underline-position)
       (scaled-parameter "UnderlineThickness" number underline-thickness)
       (key "StartCharMetrics" ()
            (setf (characters font-metrics)
                  (afm-char-metrics stream (char-width font-metrics)(italic-sin font-metrics) font-metrics)))
       (key "StartKernPairs" ()
	    (afm-char-kernings stream (characters font-metrics)(kernings font-metrics)))
       ))))

(define-afm-section (afm-char-metrics "CharMetrics")(stream default-width italic-sin font-metrics)
  (let ((metrics (make-hash-table :test #'equal))
	(encoding (encoding-vector font-metrics))
	char-metrics)
    (setf (gethash "VoidCharacter" metrics)
	  (make-instance 'char-metrics :code -1 :name "VoidChar" :index 0
                         :width 0 :bbox #(0 0 0 0) :spacing 0))
    (process-keywords
     (key "EndCharMetrics" () (return-from afm-char-metrics metrics))
     (t (let ((width default-width)
	      (stroke-width 0)
              (index 0)
              (code -1)
	      (name nil)
	      (bbox (font-bbox font-metrics)))
          (process-keywords-in-line
           (key "C" ((p-code integer)) (setq code p-code))
           (key "CH" ((p-code hex)) (setq code p-code))
           (key "WX" ((p-width number)) (setq width (* 0.001 p-width)))
           (key "N" ((p-name name)) (setq name p-name))
           (key "I" ((p-index number)) (setq index p-index))
	   (key "B" ((llx number) (lly number) (urx number) (ury number))
		(setf bbox (vector (* 0.001 llx) (* 0.001 lly) (* 0.001 urx) (* 0.001 ury)))
		(setf stroke-width (if (zerop urx) width (* 0.001 urx)))))
          (unless width
            (error "Width is not given for a character C ~D." code))
	  (setf char-metrics
		(make-instance 'char-metrics :code code :name name :index index :width width :bbox bbox
			       :spacing (- width stroke-width)
			       :left-italic-correction (if bbox (* italic-sin (aref bbox 3)) 0)
			       :right-italic-correction (if bbox (* italic-sin (aref bbox 1)) 0)))
	  (when (<= 0 code 255)
	    (setf (aref encoding code) char-metrics))
	  (when name
	    (setf (gethash name metrics) char-metrics)))))))

(define-afm-section (afm-char-kernings "CharKernPairs")(stream characters kernings)
  (flet ((register-kern-pair (name1 name2 dx dy)
	   (let* ((char1 (gethash name1 characters))
		  (char2 (when char1 (gethash name2 characters))))
	     (when char2
	       (setf (gethash (cons char1 char2) kernings) (cons (* 0.001 dx) (* 0.001 dy)))))))
    (process-keywords
     (key "EndKernPairs" () (return-from afm-char-kernings))
     (t (process-keywords-in-line
	 (key "KP" ((name1 name)(name2 name)(dx number)(dy number))
	      (register-kern-pair name1 name2 dx dy))
	 (key "KPX" ((name1 name)(name2 name)(dx number))
	      (register-kern-pair name1 name2 dx 0)))))))

(defun read-afm-file (filename &optional (font-metrics-class 'font-metrics))
  (with-open-file (s filename :direction :input :external-format +external-format+)
    (afm-font-metrics s font-metrics-class)))

(defun read-ufm-file (filename &optional (font-metrics-class 'ttu-font-metrics))
  (let ((min-code #xfffe)
        (max-code 0)
        void-char encoding-vector pdf-widths font-metrics)
    (with-open-file (s filename :direction :input :external-format +external-format+)
      (setf font-metrics (afm-font-metrics s font-metrics-class)))
    (setf void-char (gethash "VoidCharacter" (characters font-metrics)))
    (iter (for (nil char-metrics) in-hashtable (characters font-metrics))
          (for gid = (index char-metrics))
          (for code = (code char-metrics))
          (when (and (<= 0 code #xfffe))
            (when (> code max-code) (setf max-code code))
            (when (< code min-code) (setf min-code code))
            (setf (aref (c2g font-metrics) (* 2 code))
		  (code-char (ldb (byte 8 8) gid))
		  (aref (c2g font-metrics) (+ (* 2 code) 1))
		  (code-char (ldb (byte 8 0) gid)))
	    (vector-push-extend code (cid-widths font-metrics))
	    (vector-push-extend (vector (round (* 1000 (width char-metrics)))) (cid-widths font-metrics))))
    (setf encoding-vector (make-array (1+ max-code) :initial-element void-char)
          pdf-widths (make-array (1+ max-code) :initial-element 0))
    (iter (for (nil char-metrics) in-hashtable (characters font-metrics))
          (for code = (code char-metrics))
          (when (<= min-code code max-code)
            (setf (aref encoding-vector code) char-metrics
                  (aref pdf-widths code) (round (* 1000 (width char-metrics))))))
    (setf (min-code font-metrics) min-code
          (max-code font-metrics) max-code
          (encoding-vector font-metrics) encoding-vector
          (pdf-widths font-metrics) pdf-widths
          (encoding-scheme font-metrics) :unicode-encoding
          (gethash (string-downcase (font-name font-metrics)) *font-metrics*) font-metrics
          (leading font-metrics) (- 1 (descender font-metrics))
          (italic-sin font-metrics) (sin (/ (* pi (italic-angle font-metrics)) -180)))
    font-metrics))

(defmethod font-type (font-metrics)
  (declare (ignore font-metrics))
  "Type1")

(defmethod font-descriptor (font-metrics &key (errorp nil) &allow-other-keys)
  (declare (ignore font-metrics))
  (if errorp
      (error "Generic fonts do not have descriptors.")
      nil))

(defgeneric make-dictionary (thing &key &allow-other-keys))

(defmethod make-dictionary ((fm font-metrics)
                            &key font (encoding (encoding font)) (embed *embed-fonts*)
                            &allow-other-keys)
  (let ((font-descriptor (font-descriptor fm :embed embed :errorp nil)))
    (make-instance 'dictionary :dict-values
      `(("/Type" . "/Font")
        ("/Subtype" . ,(add-/ (font-type fm)))
        ("/BaseFont" . ,(add-/ (font-name fm)))
        ("/Encoding" . ,(if (standard-encoding encoding)
                            (add-/ (name encoding))
                            (find-encoding-object encoding)))
        ,@(when font-descriptor
            `(("/FirstChar" . 0)
              ("/LastChar" . 255)
              ("/Widths" . ,(pdf-widths font))
              ("/FontDescriptor" . ,font-descriptor))) )) ))


(defun extract-font-metrics-encoding (font-metrics)
 ;; Make extract-font-metrics-encoding generic?
  (let ((encoding (or (get-encoding (encoding-scheme font-metrics))
		      (get-encoding (font-name font-metrics)))))
    (if encoding
	encoding
	(make-instance 'single-byte-encoding :name (font-name font-metrics)
		       :standard-encoding nil
		       :char-names (map 'vector #'(lambda (char)
						    (and char (name char)))
					(encoding-vector font-metrics))))))
