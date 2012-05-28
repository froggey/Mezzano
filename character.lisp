(in-package #:sys.int)

(declaim (special *unicode-info*
                  *unicode-name-store*
                  *unicode-encoding-table*
                  *unicode-name-trie*))

(defun %make-character (code &optional bits)
  (check-type code (or (integer 0 #x0010FFFF))
              "a unicode code-point")
  (check-type bits (or null (integer 0 15)))
  (if (<= #xD800 code #xDFFF)
      nil
      (%%assemble-value (ash (logior code (ash (or bits 0) 21)) 4) #b1010)))

(defconstant +char-control-bit+ #b0001)
(defconstant +char-meta-bit+    #b0010)
(defconstant +char-super-bit+   #b0100)
(defconstant +char-hyper-bit+   #b1000)

(defun make-character (code &key control meta super hyper)
  (%make-character code (logior (if control +char-control-bit+ 0)
                                (if meta +char-meta-bit+ 0)
                                (if super +char-super-bit+ 0)
                                (if hyper +char-hyper-bit+ 0))))

(defun char-code (character)
  (check-type character character)
  (logand (ash (lisp-object-address character) -4) #x1FFFFF))

(defun char-int (character)
  (check-type character character)
  (ash (lisp-object-address character) -4))

(defun code-char (code)
  (%make-character code))

(defun system:char-bits (character)
  (check-type character character)
  (logand (ash (ash (lisp-object-address character) -4) -21) 15))

(defun char-bit (character bit)
  (let ((bits (char-bits character)))
    (logtest bits
             (ecase bit
               (:control +char-control-bit+)
               (:meta +char-meta-bit+)
               (:super +char-super-bit+)
               (:hyper +char-hyper-bit+)))))

(defun set-char-bit (character bit set-it)
  (let ((bit (ecase bit
               (:control +char-control-bit+)
               (:meta +char-meta-bit+)
               (:super +char-super-bit+)
               (:hyper +char-hyper-bit+)))
        (bits (char-bits character)))
    (if set-it
        (setf bits (logior bits bit))
        (setf bits (logand bits (lognot bit))))
    (%make-character (char-code character) bits)))

(define-setf-expander char-bit (character bit &environment env)
  (multiple-value-bind (temps vals stores
                              store-form access-form)
      (get-setf-expansion character env)
    (let ((btemp (gensym))     ;Temp var for bit name.
          (store (gensym))     ;Temp var for bit value to store.
          (stemp (first stores))) ;Temp var for character to store.
      (when (cdr stores) (error "Can't expand this."))
      ;; Return the setf expansion for LDB as five values.
      (values (append temps (list btemp))       ;Temporary variables.
              (append temps (list bit))          ;Value forms.
              (list store)             ;Store variables.
              `(let ((,stemp (set-char-bit ,access-form ,btemp ,store)))
                 ,store-form
                 ,store)               ;Storing form.
              `(char-bit ,access-form ,btemp) ;Accessing form.
              ))))

(defun char-upcase (char)
  "If CHAR is a lowercase character, the corresponding uppercase character. Otherwise, CHAR is returned unchanged."
  (let ((code (if (<= (char-code char) #x7F)
		  (if (char<= #\a char #\z)
		      (logand #xFFFFDF (char-code char))
		      (char-code char))
		  (let ((info (unicode-char-info char)))
		    (when (and info (eql (logand info #x1800000000000) #x1000000000000))
		      (logand (ash info -26) #x1FFFFF))))))
    (if code
	(%make-character code (char-bits char))
	char)))

(defun char-downcase (char)
  "If CHAR is an uppercase character, the corresponding lowercase character. Otherwise, CHAR is returned unchanged."
  (let ((code (if (<= (char-code char) #x7F)
		  (if (char<= #\A char #\Z)
		      (logior #x20 (char-code char))
		      (char-code char))
		  (let ((info (unicode-char-info char)))
		    (when (and info (eql (logand info #x1800000000000) #x0800000000000))
		      (logand (ash info -26) #x1FFFFF))))))
    (if code
	(%make-character code (char-bits char))
	char)))

(defun upper-case-p (char)
  "Returns true if CHAR is an uppercase character; otherwise, false is returned."
  ;; Fast path for ASCII
  (if (<= (char-code char) #x7F)
      (char<= #\A char #\Z)
      (let ((info (unicode-char-info char)))
	(and info (eql (logand info #x1800000000000) #x0800000000000)))))

(defun lower-case-p (char)
  "Returns true if CHAR is an lowercase character; otherwise, false is returned."
  ;; Fast path for ASCII
  (if (<= (char-code char) #x7F)
      (char<= #\a char #\z)
      (let ((info (unicode-char-info char)))
	(and info (eql (logand info #x1800000000000) #x1000000000000)))))

(defun both-case-p (char)
  "Returns true if CHAR has case; otherwise false is returned."
  ;; Fast path for ASCII
  (if (<= (char-code char) #x7F)
      (or (char<= #\A char #\Z) (char<= #\a char #\z))
      (let ((info (unicode-char-info char)))
	(and info (or (eql (logand info #x1800000000000) #x0800000000000)
		      (eql (logand info #x1800000000000) #x1000000000000))))))

(define-compiler-macro char= (&whole whole character &rest more-characters)
  (declare (dynamic-extent more-characters))
  (cond ((null more-characters) (progn (check-type ,character character) 't))
        ((null (rest more-characters))
         `(eql ,character ,(first more-characters)))
        (t whole)))

(defun char= (character &rest more-characters)
  (declare (dynamic-extent more-characters))
  (check-type character character)
  (every (lambda (c)
           (check-type c character)
           (eql character c))
         more-characters))

(defun char<= (character &rest more-characters)
  (declare (type character character)
	   (dynamic-extent more-characters))
  (do ((i more-characters (cdr i))
       (c character (car i)))
      ((null i) t)
    (unless (<= (char-int c) (char-int (car i)))
      (return nil))))

(defun char-equal (character &rest more-characters)
  (declare (dynamic-extent more-characters))
  (check-type character character)
  (every (lambda (c)
           (check-type c character)
           (eql (char-upcase character) (char-upcase c)))
         more-characters))

(defun base-char-p (character)
  (check-type character character)
  (and (zerop (system:char-bits character))
       (< (char-code character) 256)))

(defun standard-char-p (character)
  (check-type character character)
  (and (zerop (system:char-bits character))
       (or (<= #x20 (char-code character) #x7E)
           (eql character #\Newline))))

(defun graphic-char-p (char)
  "Returns true if CHAR is a graphic character."
  ;; Treat everything but the Latin1 control characters as graphic characters.
  (not (or (<= #x00 (char-code char) #x1F)
	   (<= #x7F (char-code char) #x9F))))

(defun alpha-char-p (char)
  "Returns true if CHAR is an alphabetic character."
  ;; Fast path for ASCII.
  (if (<= (char-code char) #x7F)
      (or (char<= #\A char #\Z) (char<= #\a char #\z))
      ;; Assume all Unicode characters are alphabetic.
      ;; TODO.
      t))

(defun alphanumericp (char)
  (or (digit-char-p char) (alpha-char-p char)))

(defparameter *char-name-alist*
  '((#x0000 "Nul" "Null")
    (#x0007 "Bel" "Bell")
    (#x0008 "Backspace")
    (#x0009 "Tab")
    (#x000A "Newline" "Linefeed")
    (#x000C "Page")
    (#x001B "Esc" "Escape")
    (#x0020 "Space")
    (#x007F "Rubout")))

(defun char-name (character)
  "Returns a string that is the name of CHARACTER, or NIL if CHARACTER has no name."
  (declare (type character character))
  (let* ((code (char-code character))
	 ;; Prioritize the CL names over the Unicode names.
	 (name (or (cadr (assoc code *char-name-alist*))
		   (unicode-char-name character :space-char #\_)
		   (if (> code #xFFFF)
		       (format nil "U~8,'0X" code)
		       (format nil "U~4,'0X" code)))))
    (when (char-bit character :hyper)
      (setf name (concatenate 'string "H-" name)))
    (when (char-bit character :super)
      (setf name (concatenate 'string "S-" name)))
    (when (char-bit character :meta)
      (setf name (concatenate 'string "M-" name)))
    (when (char-bit character :control)
      (setf name (concatenate 'string "C-" name)))
    name))

(defun valid-codepoint-p (string &optional (start 0) end)
  "Test if string describes a valid codepoint."
  (unless end (setf end (length string)))
  (do ((i start (1+ i)))
      ((>= i end) t)
    (unless (digit-char-p (char string i) 16)
      (return nil))))

(defun name-char (name)
  "Returns the character whose name is NAME or NIL if no such character exists."
  (let ((start 0)
	(control nil) (meta nil) (super nil) (hyper nil))
    ;; TODO: Allow prefixes in any order.
    (when (and (> (- (length name) start) 2)
	       (string= "C-" name :start2 start :end2 (+ start 2)))
      (setf control t)
      (incf start 2))
    (when (and (> (- (length name) start) 2)
	       (string= "M-" name :start2 start :end2 (+ start 2)))
      (setf meta t)
      (incf start 2))
    (when (and (> (- (length name) start) 2)
	       (string= "S-" name :start2 start :end2 (+ start 2)))
      (setf super t)
      (incf start 2))
    (when (and (> (- (length name) start) 2)
	       (string= "H-" name :start2 start :end2 (+ start 2)))
      (setf hyper t)
      (incf start 2))
    (when (= start (length name))
      (return-from name-char nil))
    (when (= start (1- (length name)))
      (return-from name-char (make-character (char-code (char name start))
					     :control control
					     :meta meta
					     :super super
					     :hyper hyper)))
    ;; SBCL-style Unicode notation (Java style?)
    ;; #\uXXXX or #\uXXXXXXXX
    ;; TODO: catch invalid Unicode codepoints
    (when (and (char-equal (char name start) #\U)
	       (or (= (- (length name) start) 5) (= (- (length name) start) 9))
	       (valid-codepoint-p name (1+ start)))
      (multiple-value-bind (value end)
	  (parse-integer name :start (1+ start) :radix 16)
	(when (and value (= end (length name)))
	  (return-from name-char (make-character value
						 :control control
						 :meta meta
						 :super super
						 :hyper hyper)))))
    (dolist (names *char-name-alist*)
      (dolist (ch (cdr names))
	(when (string-equal ch name :start2 start)
	  (return-from name-char (make-character (car names)
						 :control control
						 :meta meta
						 :super super
						 :hyper hyper)))))
    (let ((codepoint (match-unicode-name name *unicode-name-trie* :start start)))
      (when codepoint
	(return-from name-char (make-character codepoint
					       :control control
					       :meta meta
					       :super super
					       :hyper hyper))))))

;;; Unicode support functions and data.

(defun unicode-char-info (char)
  (let* ((code (char-code char))
	 (plane (ash (logand #x1F0000 code) -16))
	 (row (ash (logand #xFF00 code) -8))
	 (cell (logand #xFF code)))
    (when (aref *unicode-info* plane)
      (when (aref (aref *unicode-info* plane) row)
	(let ((info (aref (aref (aref *unicode-info* plane) row) cell)))
	  (if (eql 0 info)
	      nil
	      info))))))

(defun unicode-char-name (char &key (space-char #\Space))
  (let ((info (unicode-char-info char)))
    (when info
      (let ((start (logand info #xFFFFF))
	    (length (ash (logand info #x3F00000) -20)))
	(decode-unicode-name *unicode-name-store* *unicode-encoding-table*
			     :start start
			     :end (+ start length)
			     :space-char space-char)))))

(defparameter *unicode-direct-name-codes* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789- ")

(defun decode-unicode-name (encoded encoding-table &key (start 0) end (space-char #\Space))
  (unless end (setf end (length encoded)))
  (let ((name (make-array 8 :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (dotimes (i (- end start))
      (let ((short (if (eql (aref encoded (+ i start)) 0)
		       (find (aref encoded (+ (incf i) start)) encoding-table
			     :key (lambda (x) (when (consp (car x)) (caar x))))
		       (find (aref encoded (+ i start)) encoding-table :key #'car))))
	(if short
	    (dotimes (j (length (cdr short)))
	      (if (eql (char (cdr short) j) #\Space)
		  (vector-push-extend space-char name)
		  (vector-push-extend (char (cdr short) j) name)))
	    (let ((decoded-char (aref *unicode-direct-name-codes* (1- (aref encoded (+ i start))))))
	      (if (eql decoded-char #\Space)
		  (vector-push-extend space-char name)
		  (vector-push-extend decoded-char name))))))
    name))

(defun match-unicode-name (name trie &key (start 0) end)
  "Match a character name against the Unicode name trie using the fuzzy match algorithm."
  (let ((prev-was-letter t)
	(node 0))
    (unless end (setf end (length name)))
    (do ((i start (1+ i)))
	((>= i end)
	 (when (eql (logand (aref trie node) #xFC000000) #xFC000000)
	   (let ((codepoint (logand (aref trie node) #x00FFFFFF)))
	     (when (eql codepoint #x116C)
	       ;; Disambiguate U+1180 HANGUL JUNGSEONG O-E
	       ;; Skip trailing spaces.
	       (let ((end (length name)))
		 (do ()
		     ((or (eql 0 end)
			  (not (eql #\Space (char name (1- end))))))
		   (decf end))
		 (when (and (> end 3)
			    (string-equal "O-E" name :start2 (- end 3) :end2 end))
		   (setf codepoint #x1180))))
	     codepoint)))
      (flet ((advance (ch)
	       (let ((ofs (1+ (position ch *unicode-direct-name-codes*))))
		 (if (eql (ash (logand (aref trie (+ node ofs)) #xFC000000) -26) ofs)
		     (setf node (logand (aref trie (+ node ofs)) #x00FFFFFF))
		     (return-from match-unicode-name nil)))))
	(let ((ch (char name i)))
	  ;; Ignore spaces and medial hyphens
	  (cond ((eql ch #\-)
		 (unless (and prev-was-letter (not (eql (char name (1+ i)) #\Space)))
		   (advance #\-)))
		((or (eql ch #\Space)
		     (eql ch #\_))
		 (setf prev-was-letter nil))
		((find (char-upcase ch) *unicode-direct-name-codes*)
		 (advance (char-upcase ch))
		 (setf prev-was-letter t))
		(t (return-from match-unicode-name nil))))))))
