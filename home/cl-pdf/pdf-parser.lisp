;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

;; Thanks to Arthur Lemmens for the recursive descent parser :)
;; (It's much nicer than my original LALR parser ;-)
;; See the example at the end for usage.

(in-package #:pdf)

(defvar *pdf-input-stream* nil)
(defvar *indirect-objects* nil)

(defun find-indirect-object (obj-number gen-number)
  (let ((object.pos (gethash (cons obj-number gen-number) *indirect-objects*)))
    (if object.pos
	(values (car object.pos) (cdr object.pos))
	(make-indirect-object obj-number gen-number 0))))

(defun make-indirect-object (obj-number gen-number position)
  (let ((object (or (car (gethash (cons obj-number gen-number) *indirect-objects*))
		    (make-instance 'indirect-object
				   :obj-number obj-number
				   :gen-number gen-number
				   :content :unread
				   :no-link t))))
    (setf (gethash (cons obj-number gen-number) *indirect-objects*) (cons object position))
    object))

(defun read-indirect-object (obj-number gen-number)
  (multiple-value-bind (object position) (find-indirect-object obj-number gen-number)
    (when (eq (content object) :unread)
      (setf (content object) (read-indirect-object-content position)))
    object))

(defun load-indirect-object (object)
  (when (eq (content object) :unread)
    (read-indirect-object (obj-number object) (gen-number object)))
  object)

(defun delete-indirect-object (object)
  (remhash (cons (obj-number object) (gen-number object)) *indirect-objects*))

(defun load-all-indirect-objects ()
  (loop for object.pos being the hash-value of *indirect-objects*
        do (load-indirect-object (car object.pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition pdf-parse-error ()
  ((stream :initarg :stream :reader pdf-parse-error-stream)
   (message :initarg :message :reader pdf-parse-error-message))
  (:report (lambda (condition stream)
             (format stream "~&Error at position ~D while reading PDF document.~%~A~%~A~%"                     (file-position (pdf-parse-error-stream condition))                     (pdf-parse-error-message condition)
                     (pdf-parse-error-stream condition)))))

(defun unexpected-character-error (char)
  (cerror "Ignore the character and continue."
          'pdf-parse-error
          :stream *pdf-input-stream*
          :message (format nil "Unexpected character ~S at ~A." char
                           (file-position *pdf-input-stream*))))

(defvar +white-char+ (coerce '(#\Space #\Newline #\Return #\Tab #\Null #\Page) 'string))

(defun white-char-p (c)
  (find c +white-char+))

(defun octal-digit-char-p (char)
  (find char "01234567"))

(defun hex-digit-char-p (char)
  (find char "0123456789ABCDEF"))

(defun numeric-char-p (char)
  (find char "0123456789-."))

(defun name-char-p (char)
  (and (<= #x21 (char-code char) #x7E)
       (not (find char "%()<>[]{}/#"))))

;;; Skipping characters

(defun eat-char (expected-char)
  (let ((char (read-char *pdf-input-stream*)))
    (unless (char= char expected-char)
      (cerror "Ignore the character and continue."
              'pdf-parse-error
              :stream *pdf-input-stream*
              :message (format nil "Unexpected character ~S (expected ~S) at ~A."
                               char expected-char (file-position *pdf-input-stream*))))))

(defun eat-chars (chars)
  (loop for char across chars
        do (eat-char char)))

(defun eat-keyword (keyword)
  (skip-whitespace t)
  (eat-chars keyword)
  (skip-whitespace t))

(defun skip-whitespace (eof-error-p &key (skip-comments t))
  (loop
   (let ((char (read-char *pdf-input-stream* eof-error-p)))
     (cond ((null char)
            (return :eof))
           ((and skip-comments (eql char #\%))
            (loop for c = (read-char *pdf-input-stream* nil)
                  until (or (null c) (eq c #\Newline))))
           ((not (white-char-p char))
            (unread-char char *pdf-input-stream*)
            (return char))))))

;;; PDF Basic objects (see Chapter 4)

(defun read-object (&optional (eof-error-p t))
  "Returns one of the following PDF objects: boolean (:true or :false),
number (Lisp number), string (Lisp string), name (Lisp symbol in the PDF
package), array (Lisp vector), dictionary (Lisp property list), stream
(Lisp pdf-stream) or null (Lisp NIL). When EOF-ERRORP is nil, it returns
:eof for the end of the stream (otherwise it signals an error)." 
  (skip-whitespace eof-error-p)
  (let ((char (peek-char nil *pdf-input-stream* eof-error-p)))
    (cond ((numeric-char-p char)
           (read-number))
          ((eql char #\()
           (eat-char #\()
           (read-pdf-string))
          ((eql char #\/)
           (eat-char #\/)
           (read-name ))
          ((eql char #\[)
           (eat-char #\[)
           (read-array))
          ((eql char #\<) 
           (eat-char #\<)
           (let ((next-char (peek-char nil *pdf-input-stream*)))
             (if (char= next-char #\<) 
                 (progn (eat-char #\<)
                   (read-dictionary-or-stream))
               (read-hex-string))))
          ((eql char #\t)
           (eat-chars "true")
           "true")
          ((eql char #\f)
           (eat-chars "false")
           "false")
          ((eql char #\n)
           (eat-chars "null")
           nil)
          ((eql char #\e)
           ;; this is probably an empty indirect object.  WRITE-OBJECT
           ;; can write them, so we should be able to read them too.
           nil)
          (t (unexpected-character-error char)))))

(defun read-number ()
  (read-from-string
   (with-output-to-string (s)
     (loop for char = (read-char *pdf-input-stream* nil nil)
           until (or (null char) (not (numeric-char-p char)))
           do (write-char char s)
           finally (when char
                     (unread-char char *pdf-input-stream*))))))

;;; Strings

(defun read-pdf-string ()
  ;; TODO: Deal with encodings.
  (with-output-to-string (s)
    (write-char #\( s)
    (loop for prev-char = #\( then char
          for char = (read-char *pdf-input-stream*)
          until (and (char= char #\))(not (char= prev-char #\\)))
	  do (write-char char s))
    (write-char #\) s)))

(defun read-hex-string ()
  (with-output-to-string (s)
    (write-char #\< s)
    (loop for char = (read-char *pdf-input-stream*)
       until (char= char #\>)
       do (write-char char s))
    (write-char #\> s)))

(defun parse-hex (digit-1 digit-2)
  (flet ((parse-digit (digit)
           (- (char-code digit)
              (if (char<= #\0 digit #\9) #.(char-code #\0) #.(- (char-code #\A) 10)))))
    (+ (* 16 (parse-digit (char-upcase digit-1)))
       (parse-digit (char-upcase digit-2)))))

;;; Names

(defun read-name ()
  (with-output-to-string (s)
    (write-char #\/ s)
    (loop (let ((char (read-char *pdf-input-stream* nil nil)))
	    (cond ((null char) (return))
		  ((eql char #\#)
		   (let ((digit-1 (read-char *pdf-input-stream*))
			 (digit-2 (read-char *pdf-input-stream*)))
		     (unless (and (hex-digit-char-p digit-1)
				  (hex-digit-char-p digit-2))
		       (error 'pdf-parse-error
			      :stream *pdf-input-stream*
			      :message "Illegal hexadecimal escape sequence in PDF name."))
		     (write-char (code-char (parse-hex digit-1 digit-2))
				 s)))
		  ((name-char-p char)
		   (write-char char s))
		  (t (unread-char char *pdf-input-stream*)
		     (return)))))))

;;; Arrays

(defun read-array ()
  ;; #\[ has already been read
  (let ((stack '()))
    (loop (skip-whitespace t)
          (let ((char (peek-char nil *pdf-input-stream*)))
            (case char
              ( #\]
                (eat-char #\])
                (return))
              ( #\R
                (eat-char #\R)
                ;; Reduce last two numbers to indirect-reference
                (let ((generation-number (pop stack))
                      (object-number (pop stack)))
                  (assert (integerp generation-number))
                  (assert (integerp object-number))
                  (push (find-indirect-object object-number generation-number) stack)))
              (otherwise
               (push (read-object) stack)))))
    (make-array (length stack)
                :initial-contents (nreverse stack))))

;;; Dictionaries and streams

(defun read-dictionary-properties ()
  (let ((plist '()))
    (loop (skip-whitespace t)
          (let ((char (peek-char nil *pdf-input-stream*)))
            (case char
              ( #\>
                (eat-chars ">>")
                (return))
              ( #\R
                (eat-char #\R)
                ;; Reduce last two numbers to indirect-object
                (let ((generation-number (pop plist))
                      (object-number (pop plist)))
                  (assert (integerp generation-number))
                  (assert (integerp object-number))
                  (push (find-indirect-object object-number generation-number) plist)))
              (otherwise
               (push (read-object t)
                     plist)))))
    (loop for (k v . rest) on (nreverse plist) by #'cddr
          collect (cons k v))))

(defun read-dictionary ()
  (make-instance 'dictionary :dict-values (read-dictionary-properties)))

(defun read-dictionary-or-stream ()
  (let ((properties (read-dictionary-properties)))
    ;; Check if dictionary is followed by a stream
    (skip-whitespace nil)
    (let ((char (peek-char nil *pdf-input-stream* nil nil)))
      (cond ((eql char #\s)
             ;; Don't use EAT-KEYWORD here, because it may eat too many newlines!
             (eat-chars "stream") 
             (read-line *pdf-input-stream*)
             (read-pdf-stream properties))
            (t (make-instance 'dictionary :dict-values properties))))))

(defun read-pdf-stream (properties)
  (let ((length (cdr (assoc "/Length" properties :test #'string=))))
    (when (typep length 'indirect-object)
      (let ((saved-filepos (file-position *pdf-input-stream*)))
        (setq length (content (load-indirect-object length)))
        (file-position *pdf-input-stream* saved-filepos)))
    (assert (integerp length))
    (let ((content (loop repeat (ceiling length array-total-size-limit)
		      for buffer-size = (min length array-total-size-limit)
		      ;; while (plusp buffer-size)
		      collect (let* ((buffer (make-string buffer-size))
				     (bytes-read (read-sequence buffer *pdf-input-stream*)))
				(when (< bytes-read buffer-size)
				  (error 'pdf-parse-error
					 :stream *pdf-input-stream*
					 :message "Unexpected end of PDF-stream."))
				buffer)
		      do (decf length buffer-size))))
      (eat-keyword "endstream")
      (make-instance 'pdf-stream :dict-values properties :content content :no-compression t))))

;;; Integers

(defun read-integer ()
  (let ((object (read-object)))
    (unless (integerp object)
      (error 'pdf-parse-error 
             :stream *pdf-input-stream*
             :message "Integer expected."))
    object))

;;; Indirect objects

(defun read-indirect-object-content (file-position)
  (file-position *pdf-input-stream* file-position)
  (let* ((object-number (read-integer))
         (generation-number (read-integer)))
    (eat-keyword "obj")
    (let ((object (read-object)))
      ;; Some producers forget the "endobj" at the end of a stream
      ;; Let's try to be tolerant.
      (skip-whitespace t)
      (when (char= #\e (peek-char nil *pdf-input-stream*))
        (eat-keyword "endobj"))
      object)))

;;; xref

(defun read-xref-and-trailer (position)
  (let (first-trailer)
    (loop
       (read-cross-reference-subsections position)
       (let* ((trailer (read-trailer)))
	 (unless first-trailer (setf first-trailer trailer))
	 (let ((prev-position (get-dict-value trailer "/Prev")))
	   (if prev-position
	       (setq position prev-position)
	       (return first-trailer)))))))

(defun read-cross-reference-subsections (position)
  (file-position *pdf-input-stream* position)
  (eat-keyword "xref")
  (loop
   ;; the eat-keyword skips whitespace the first time, but there can
   ;; be more whitespace the second time around.
     (skip-whitespace t)
     (let ((char (peek-char nil *pdf-input-stream*)))
       (cond ((char= char #\t) (return))
             (t (read-cross-reference-subsection))))))

(defun read-cross-reference-subsection ()
  (let ((first-entry (read-integer))
        (nr-entries (read-integer)))
    (loop repeat nr-entries
	  for number from first-entry
          do (read-cross-reference-entry number))))

(defun read-cross-reference-entry (number)
  (let ((position (read-integer)))
    (skip-whitespace nil)
    (let ((gen (read-integer)))
      (skip-whitespace nil)
      (let ((type (read-char *pdf-input-stream*)))
        (skip-whitespace nil)
        (if (char= type #\n)
	    (make-indirect-object number gen position))))))

(defun read-trailer ()
  (eat-keyword "trailer")
  (skip-whitespace t)
  (eat-chars "<<")
  (read-dictionary))

(defconstant +xref-search-size+ 1024
  "Read this many bytes at end of file to look for 'startxref'")

(defun find-cross-reference-start ()
  (let ((file-length (file-length *pdf-input-stream*))
        (buffer (make-string +xref-search-size+)))
    (file-position *pdf-input-stream* (- file-length +xref-search-size+))
    (read-sequence buffer *pdf-input-stream*)
    (let ((position (search "startxref" buffer)))
      (unless position
        (error 'pdf-parse-error :stream *pdf-input-stream*
               :message "Can't find start address of cross-reference table."))
      (parse-integer buffer :start (+ position 10) :junk-allowed t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Higher level Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun renumber-all-indirect-objects ()
  "Gives all indirect objects a consecutive numbering.
Returns the first unused object-number."
  (let ((objects (make-array (hash-table-count *indirect-objects*)
			     :fill-pointer 0 :adjustable t
			     :initial-element nil)))
    (setf (objects *document*) objects)
    (setf (last-object-number *document*) (hash-table-count *indirect-objects*))
    (loop for object.pos being the hash-value of *indirect-objects*
	  for object = (car object.pos)
          for number from 1 do
	 (setf (obj-number object) number
	       (gen-number object) 0)
	 (vector-push-extend object objects))))

(defun read-pdf-file (file)
  (let ((*indirect-objects* (make-hash-table :test #'equal))
	(*document* (make-instance 'document :empty t)))
    (with-open-file (*pdf-input-stream* file
		     :direction :input
		     :external-format +external-format+)
      (read-pdf))
    *document*))

(defun collect-pages (page pages-vector root-page-node)
  (if (string= (get-dict-value (content page) "/Type") "/Pages")
      (progn
	(loop for kid-page across (get-dict-value (content page) "/Kids")
  	      do (collect-pages kid-page pages-vector root-page-node))
	(unless (eq page root-page-node) (delete-indirect-object page)))
      (progn
	(unless (eq page root-page-node)
	  (change-dict-value (content page) "/Parent" root-page-node))
	(vector-push-extend page pages-vector))))

(defun read-pdf ()
  (let* ((trailer (read-xref-and-trailer (find-cross-reference-start)))
	 (%visited-object% (make-hash-table :test #'eql)))
    (setf (catalog *document*) (get-dict-value trailer "/Root")
          (docinfo *document*) (get-dict-value trailer "/Info"))
    (load-all-indirect-objects)
    (let* ((root-page-node (change-class (get-dict-value (content (catalog *document*)) "/Pages") 'page-node))
	   (pages-vector (make-array 1 :fill-pointer 0 :adjustable t)))
      (collect-pages root-page-node pages-vector root-page-node)
      (setf (pages root-page-node) pages-vector)
      (change-dict-value (content root-page-node) "/Count" #'(lambda () (length (pages root-page-node))))
      (change-dict-value (content root-page-node) "/Kids" (pages root-page-node))
      (renumber-all-indirect-objects)
      (setf (root-page *document*) root-page-node))))

(defvar *original-content* nil)
(defvar *current-content* nil)

(defun insert-original-page-content ()
  ;; save graphics state
  (write-line " q" *page-stream*)
  (vector-push-extend (make-instance 'indirect-object :content 
				     (make-instance 'pdf-stream :content 
						    (get-output-stream-string *page-stream*)))
		      *current-content*)
  (if (vectorp *original-content*)
      (loop for content across *original-content* do
	   (vector-push-extend content *current-content*))
      (vector-push-extend *original-content* *current-content*))
  (setf *page-stream* (make-string-output-stream))
  ;; restore graphics state
  (write-line " Q" *page-stream*))

(export 'insert-original-page-content)

(defun ensure-dictionary (obj)
  (if (typep obj 'indirect-object)
      (content obj)
      obj))

(defun open-page (page-num)
  (let* ((page (aref (pages *root-page*) page-num))
	 (dict (content page))
	 (resources (ensure-dictionary (get-dict-value dict "/Resources"))))
    (let ((fonts (ensure-dictionary (get-dict-value resources "/Font")))
	  (xobjects (ensure-dictionary (get-dict-value resources "/XObject")))
	  (content-stream (make-instance 'pdf-stream)))
      (setf *original-content* (get-dict-value dict "/Contents"))
      (setf *current-content* (make-array 10 :fill-pointer 0 :adjustable t ))
      (change-class page 'page)
      (unless resources
	(setf resources (make-instance 'dictionary)))
      (change-dict-value dict "/Resources" resources)
      (unless fonts
	(setf fonts (make-instance 'dictionary)))
      (change-dict-value resources "/Font" fonts)
      (unless xobjects
	(setf xobjects (make-instance 'dictionary)))
      (change-dict-value resources "/XObject" xobjects)
      (setf (bounds page)(get-dict-value dict "/MediaBox")
	    (resources page) resources
	    (font-objects page) fonts
	    (xobjects page) xobjects
	    (content-stream page) content-stream)
      (change-dict-value dict "/Contents" *current-content*))
    page))

(defun copy-page (page-num)
  (let* ((src-page (aref (pages *root-page*) page-num))
	 (src-dict (content src-page))
	 (resources (copy-dict (ensure-dictionary (get-dict-value src-dict "/Resources"))))
	 (fonts (copy-dict (ensure-dictionary (get-dict-value resources "/Font"))))
	 (xobjects (copy-dict (ensure-dictionary (get-dict-value resources "/XObject"))))
	 (new-page (make-instance 'page))
	 (new-dict (setf (content new-page) (copy-dict src-dict)))
	 (content-stream (make-instance 'pdf-stream)))
    (setf *original-content* (get-dict-value src-dict "/Contents"))
    (setf *current-content* (make-array 10 :fill-pointer 0 :adjustable t))
    (unless resources
      (setf resources (make-instance 'dictionary)))
    (change-dict-value new-dict "/Resources" resources)
    (unless fonts
      (setf fonts (make-instance 'dictionary)))
    (change-dict-value resources "/Font" fonts)
    (unless xobjects
      (setf xobjects (make-instance 'dictionary)))
    (change-dict-value resources "/XObject" xobjects)
    (setf (bounds new-page) (get-dict-value src-dict "/MediaBox")
	  (resources new-page) resources
	  (font-objects new-page) fonts
	  (xobjects new-page) xobjects
	  (content-stream new-page) content-stream)
    (change-dict-value new-dict "/Contents" *current-content*)
    new-page))

(defun remove-page (page-num)
  (let ((page (aref (pages *root-page*) page-num)))
    (setf (pages *root-page*)
	  (remove page (pages *root-page*)))
    (change-dict-value (content *root-page*) "/Count" #'(lambda () (length (pages *root-page*))))
    (change-dict-value (content *root-page*) "/Kids" (pages *root-page*))))


(export 'remove-page)

(defmacro with-existing-document ((file &key (creator "") author title subject keywords) &body body)
  `(let* ((*document* (read-pdf-file ,file))
	  (*root-page* (root-page *document*))
	  (*page-number* 0)
          (*name-counter* 100))
     (add-doc-info *document* :creator ,creator :author ,author
		   :title ,title :subject ,subject :keywords ,keywords)
    ,@body))

(export 'with-existing-document)

(defmacro with-existing-page ((page-number &key copy-p) &body body)
  `(let* ((*original-content* nil)
	  (*current-content* nil)
	  (*page* (if ,copy-p
		      (copy-page ,page-number)
		      (open-page ,page-number))))
     (setf (content (content-stream *page*))
	   (let ((*page-stream* (make-string-output-stream)))
	     ,@body
	     (vector-push-extend
	      (make-instance 'indirect-object :content 
			     (make-instance 'pdf-stream :content 
					    (get-output-stream-string pdf::*page-stream*)))
	      *current-content*)))))

(export 'with-existing-page)

#|

(pdf:with-existing-document (#P"/tmp/MS-32.pdf")
  (pdf:with-existing-page (0)
    (let ((helvetica (pdf:get-font "Helvetica")))
      (pdf:in-text-mode
       (pdf:set-font helvetica 45.0)
       (pdf:move-text 95 700)
       (pdf:draw-text "cl-pdf-parser example"))
      (pdf:with-saved-state
	  (pdf:translate 200 60)
	(pdf:scale 0.7 0.7)
	(pdf:rotate 15)
	(pdf:insert-original-page-content))
      (pdf:translate 230 400)
      (loop repeat 170
	 for i = 0.67 then (* i 1.03)
	 do (pdf:in-text-mode
	     (pdf:set-font helvetica i)
	     (pdf:set-rgb-fill (/ (random 255) 255.0)(/ (random 255) 255.0)(/ (random 255) 255.0))
	     (pdf:move-text (* i 3) 0)
	     (pdf:draw-text "cl-pdf"))
	   (pdf:rotate 13))))
  (pdf:with-existing-page (1)
    (let ((helvetica (pdf:get-font "Helvetica")))
      (pdf:insert-original-page-content)
      (pdf:in-text-mode
       (pdf:set-font helvetica 60.0)
       (pdf:move-text 250 250)
       (pdf:rotate 30)
       (pdf:set-rgb-fill 1.0 0.0 0.0)
       (pdf:draw-text "cl-pdf-parser example"))))
  (pdf:with-page ()       ;add a new page
    (let ((helvetica (pdf:get-font "Helvetica")))
      (pdf:in-text-mode
       (pdf:set-font helvetica 36.0)
       (pdf:move-text 100 800)
       (pdf:draw-text "cl-pdf: Example 1"))
      (pdf:translate 230 500)
      (loop repeat 150
	 for i = 0.67 then (* i 1.045)
	 do (pdf:in-text-mode
	     (pdf:set-font helvetica i)
	     (pdf:set-rgb-fill (/ (random 255) 255.0)(/ (random 255) 255.0)(/ (random 255) 255.0))
	     (pdf:move-text (* i 3) 0)
	     (pdf:draw-text "cl-typesetting"))
	   (pdf:rotate 13))))
  (pdf:write-document #P"/tmp/t.pdf"))

|#
