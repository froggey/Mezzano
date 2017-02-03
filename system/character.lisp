;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defconstant char-code-limit #x110000)

(declaim (special *unicode-info*
                  *unicode-name-store*
                  *unicode-encoding-table*
                  *unicode-name-trie*))

(defun %make-character (code &optional bits)
  (check-type code (or (integer 0 #x0010FFFF))
              "a unicode code-point")
  (check-type bits (or null (integer 0 15)))
  (if (or (<= #xD800 code #xDFFF) ; UTF-16 surrogates.
          ;; Noncharacters.
          (<= #xFDD0 code #xFDEF)
          (member code '#.(loop for i to #x10
                             collect (logior (ash i 16) #xFFFE)
                             collect (logior (ash i 16) #xFFFF))))
      nil
      (%%assemble-value (ash (logior code (ash (or bits 0) 21)) 4)
                        +tag-character+)))

(defconstant +char-control-bit+ #b0001)
(defconstant +char-meta-bit+    #b0010)
(defconstant +char-super-bit+   #b0100)
(defconstant +char-hyper-bit+   #b1000)

(defun make-character (code &key control meta super hyper)
  (%make-character code (logior (if control +char-control-bit+ 0)
                                (if meta +char-meta-bit+ 0)
                                (if super +char-super-bit+ 0)
                                (if hyper +char-hyper-bit+ 0))))

(defun char-int (character)
  (check-type character character)
  (ash (lisp-object-address character) -4))

(defun code-char (code)
  (%make-character code))

(defun char-bits (character)
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

(defun character-designator-p (object)
  (or (characterp object)
      (and (or (stringp object)
               (symbolp object))
           (eql (length (string object)) 1))))

(deftype character-designator ()
  `(satisfies character-designator-p))

(defun character (character-designator)
  (cond
    ((characterp character-designator)
     character-designator)
    ((and (or (stringp character-designator)
              (symbolp character-designator))
          (eql (length (string character-designator)) 1))
     (char (string character-designator) 0))
    (t (error 'type-error
              :expected-type 'character-designator
              :datum character-designator))))

(defun char-upcase (char)
  "If CHAR is a lowercase character, the corresponding uppercase character. Otherwise, CHAR is returned unchanged."
  (let ((code (if (<= (char-code char) #x7F)
                  (if (char<= #\a char #\z)
                      (logand #xFFFFDF (char-code char))
                      (char-code char))
                  (when (eql (unicode-char-general-category char) :lowercase-letter)
                    (ldb +unicode-info-othercase-code+
                         (unicode-char-info char))))))
    (if (and code
             (not (eql code 0)))
        (%make-character code (char-bits char))
        char)))

(defun char-downcase (char)
  "If CHAR is an uppercase character, the corresponding lowercase character. Otherwise, CHAR is returned unchanged."
  (let ((code (if (<= (char-code char) #x7F)
                  (if (char<= #\A char #\Z)
                      (logior #x20 (char-code char))
                      (char-code char))
                  (when (eql (unicode-char-general-category char) :uppercase-letter)
                    (ldb +unicode-info-othercase-code+
                         (unicode-char-info char))))))
    (if (and code
             (not (eql code 0)))
        (%make-character code (char-bits char))
        char)))

(defun upper-case-p (char)
  "Returns true if CHAR is an uppercase character; otherwise, false is returned."
  ;; Fast path for ASCII
  (if (<= (char-code char) #x7F)
      (char<= #\A char #\Z)
      (eql (unicode-char-general-category char) :uppercase-letter)))

(defun lower-case-p (char)
  "Returns true if CHAR is an lowercase character; otherwise, false is returned."
  ;; Fast path for ASCII
  (if (<= (char-code char) #x7F)
      (char<= #\a char #\z)
      (eql (unicode-char-general-category char) :lowercase-letter)))

(defun both-case-p (char)
  "Returns true if CHAR has case; otherwise false is returned."
  ;; Fast path for ASCII
  (if (<= (char-code char) #x7F)
      (or (char<= #\A char #\Z) (char<= #\a char #\z))
      (let ((category (unicode-char-general-category char)))
        (or (eql category :lowercase-letter)
            (eql category :uppercase-letter)))))

(define-compiler-macro char= (&whole whole character &rest more-characters)
  (cond ((null more-characters)
         `(progn
            (check-type ,character character)
            't))
        ((null (rest more-characters))
         (let ((x (gensym))
               (y (gensym)))
           `(let ((,x ,character)
                  (,y ,(first more-characters)))
              (check-type ,x character)
              (check-type ,y character)
              (eql ,x ,y))))
        (t whole)))

(defun char= (character &rest more-characters)
  (declare (dynamic-extent more-characters))
  (check-type character character)
  (dolist (c more-characters t)
    (check-type c character)
    (unless (eq character c)
      (return nil))))

(defun char-equal (character &rest more-characters)
  (declare (dynamic-extent more-characters))
  (check-type character character)
  (setf character (char-downcase character))
  (dolist (c more-characters t)
    (check-type c character)
    (unless (eq character (char-downcase c))
      (return nil))))

(define-compiler-macro char/= (&whole whole character &rest more-characters)
  (cond ((null more-characters)
         `(progn
            (check-type ,character character)
            't))
        ((null (rest more-characters))
         (let ((x (gensym))
               (y (gensym)))
           `(let ((,x ,character)
                  (,y ,(first more-characters)))
              (check-type ,x character)
              (check-type ,y character)
              (not (eql ,x ,y)))))
        (t whole)))

(defun char/= (character &rest more-characters)
  (declare (dynamic-extent more-characters))
  (check-type character character)
  (do ((lhs character (car n))
       (n more-characters (cdr n)))
      ((endp n)
       t)
    (dolist (rhs n)
      (check-type rhs character)
      (when (char= lhs rhs)
        (return-from char/= nil)))))

(defun char-not-equal (character &rest more-characters)
  (declare (dynamic-extent more-characters))
  (check-type character character)
  (do ((lhs character (car n))
       (n more-characters (cdr n)))
      ((endp n)
       t)
    (dolist (rhs n)
      (check-type rhs character)
      (when (char-equal lhs rhs)
        (return-from char-not-equal nil)))))

(macrolet ((def (name comparator)
             `(defun ,name (character &rest more-characters)
                (declare (dynamic-extent more-characters))
                (check-type character character)
                (dolist (c more-characters t)
                  (unless (,comparator (char-int character) (char-int c))
                    (return nil))
                  (setf character c)))))
  (def char< <)
  (def char> >)
  (def char<= <=)
  (def char>= >=))

(macrolet ((def (name comparator)
             `(defun ,name (character &rest more-characters)
                (declare (dynamic-extent more-characters))
                (check-type character character)
                (dolist (c more-characters t)
                  (unless (,comparator (char-int (char-downcase character))
                                       (char-int (char-downcase c)))
                    (return nil))
                  (setf character c)))))
  (def char-lessp <)
  (def char-greaterp >)
  (def char-not-greaterp <=)
  (def char-not-lessp >=))

(defun latin1-char-p (character)
  (check-type character character)
  (and (zerop (char-bits character))
       (< (char-code character) 256)))

(defun standard-char-p (character)
  (check-type character character)
  (and (zerop (char-bits character))
       (or (<= #x20 (char-code character) #x7E)
           (eql character #\Newline))))

(defun graphic-char-p (char)
  "Returns true if CHAR is a graphic character."
  (when (not (zerop (char-bits char)))
    ;; Control bits set, not graphic.
    (return-from graphic-char-p nil))
  ;; Fast path for ASCII.
  (if (<= (char-code char) #x7F)
      ;; Control characters are non-graphic.
      (not (or (<= #x00 (char-code char) #x1F)
               (eql (char-code char) #x7F)))
      (member (unicode-char-general-category char)
              '(:uppercase-letter
                :lowercase-letter
                :titlecase-letter
                :modifier-letter
                :other-letter
                :nonspacing-mark
                :spacing-mark
                :enclosing-mark
                :decimal-number
                :letter-number
                :other-number
                :connector-punctuation
                :dash-punctuation
                :open-punctuation
                :close-punctuation
                :initial-punctuation
                :final-punctuation
                :other-punctuation
                :math-symbol
                :currency-symbol
                :modifier-symbol
                :other-symbol
                :space-separator))))

(defun alpha-char-p (char)
  "Returns true if CHAR is an alphabetic character."
  ;; Fast path for ASCII.
  (if (<= (char-code char) #x7F)
      (or (char<= #\A char #\Z) (char<= #\a char #\z))
      (member (unicode-char-general-category char)
              '(:uppercase-letter
                :lowercase-letter
                :titlecase-letter
                :modifier-letter
                :other-letter))))

(defun digit-char-p (char &optional (radix 10))
  "Tests whether CHAR is a digit in the specified RADIX.
If it is, then its weight is returned as an integer; otherwise, nil is returned."
  (check-type char character)
  (check-type radix (integer 2 36) "a radix")
  (do ((weight 0 (1+ weight)))
      ((>= weight radix))
    (when (char= (char-upcase char) (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" weight))
      (return weight))))

(defun digit-char (weight &optional (radix 10))
  (check-type weight (integer 0))
  (check-type radix (integer 2 36) "a radix")
  (when (< weight radix)
    (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" weight)))

(defun alphanumericp (char)
  (or (digit-char-p char) (alpha-char-p char)))

(defparameter *char-name-alist*
  ;; C0 control characters, prioritize friendly names.
  '((#x0000 "Null" "Nul")
    (#x0001 "Start-Of-Heading" "Soh")
    (#x0002 "Start-Of-Text" "Stx")
    (#x0003 "End-Of-Text" "Etx")
    (#x0004 "End-Of-Transmission" "Eot")
    (#x0005 "Enquiry" "Enq")
    (#x0006 "Acknowledge" "Ack")
    (#x0007 "Bel")                      ; Not to be confused with Unicode BELL.
    (#x0008 "Backspace" "Bs")
    (#x0009 "Tab" "Horizontal-Tab" "Ht")
    (#x000A "Newline" "Linefeed" "Lf")
    (#x000B "Vertical-Tab" "Vt")
    (#x000C "Page" "Ff")
    (#x000D "Return" "Carriage-Return" "Cr")
    (#x000E "Shift-Out" "So")
    (#x000F "Shift-In" "Si")
    (#x0010 "Data-Link-Escape" "Dle")
    (#x0011 "Device-Control-One" "Xon" "Dc1")
    (#x0012 "Device-Control-Two" "Dc2")
    (#x0013 "Device-Control-Three" "Xoff" "Dc3")
    (#x0014 "Device-Control-Four" "Dc4")
    (#x0015 "Negative-Acknowledge" "Nak")
    (#x0016 "Synchronous-Idle" "Syn")
    (#x0017 "End-Of-Transmission-Block" "Etb")
    (#x0018 "Cancel" "Can")
    (#x0019 "End-Of-Medium" "Em")
    (#x001A "Substitute" "Sub")
    (#x001B "Escape" "Esc")
    (#x001C "File-Seperator" "Fs")
    (#x001D "Group-Seperator" "Gs")
    (#x001E "Record-Seperator" "Rs")
    (#x001F "Unit-Seperator" "Us")
    (#x0020 "Space" "Sp")
    (#x007F "Rubout" "Delete" "Del")
    ;; C1 control characters.
    ;; Full names from SBCL and abbrivations from Wikipedia.
    (#x0080 "C80" "Pad")
    (#x0081 "C81" "Hop")
    (#x0082 "Break-Permitted" "Bph")
    (#x0083 "No-Break-Permitted" "Nbh")
    (#x0084 "C84" "Ind")
    (#x0085 "Next-Line" "Nel")
    (#x0086 "Start-Selected-Area" "Ssa")
    (#x0087 "End-Selected-Area" "Esa")
    (#x0088 "Character-Tabulation-Set" "Hts")
    (#x0089 "Character-Tabulation-With-Justification" "Htj")
    (#x008A "Line-Tabulation-Set" "Vts")
    (#x008B "Partial-Line-Forward" "Pld")
    (#x008C "Partial-Line-Backward" "Plu")
    (#x008D "Reverse-Linefeed" "Ri")
    (#x008E "Single-Shift-Two" "Ss2")
    (#x008F "Single-Shift-Three" "Ss3")
    (#x0090 "Device-Control-String" "Dcs")
    (#x0091 "Private-Use-One" "Pu1")
    (#x0092 "Private-Use-Two" "Pu2")
    (#x0093 "Set-Transmit-State" "Sts")
    (#x0094 "Cancel-Character" "Cch")
    (#x0095 "Message-Waiting" "Mw")
    (#x0096 "Start-Guarded-Area" "Spa")
    (#x0097 "End-Guarded-Area" "Epa")
    (#x0098 "Start-String" "Sos")
    (#x0099 "C99" "Sgci")
    (#x009A "Single-Character-Introducer" "Sci")
    (#x009B "Control-Sequence-Introducer" "Csi")
    (#x009C "String-Terminator" "St")
    (#x009D "Operating-System-Command" "Osc")
    (#x009E "Privacy-Message" "Pm")
    (#x009F "Application-Program-Command" "Apc")
    ;; Special PC keys.
    (#x104001 "F1")
    (#x104002 "F2")
    (#x104003 "F3")
    (#x104004 "F4")
    (#x104005 "F5")
    (#x104006 "F6")
    (#x104007 "F7")
    (#x104008 "F8")
    (#x104009 "F9")
    (#x10400A "F10")
    (#x10400B "F11")
    (#x10400C "F12")
    (#x10400D "F13")
    (#x10400E "F14")
    (#x10400F "F15")
    (#x104010 "Insert")
    (#x104011 "Delete")
    (#x104012 "Home")
    (#x104013 "End")
    (#x104014 "Page-Up" "PageUp" "PgUp")
    (#x104015 "Page-Down" "PageDown" "PgDn")
    (#x104016 "Left-Arrow")
    (#x104017 "Right-Arrow")
    (#x104018 "Up-Arrow")
    (#x104019 "Down-Arrow")
    (#x10401A "Menu")
    (#x10401B "Print-Screen")
    (#x10401C "SysRq")
    (#x10401D "Pause")
    (#x10401E "Break")
    (#x10401F "Caps-Lock" "Capslock")
    (#x104020 "Left-Shift")
    (#x104021 "Right-Shift")
    (#x104022 "Left-Control")
    (#x104023 "Right-Control")
    (#x104024 "Left-Meta")
    (#x104025 "Right-Meta")
    (#x104026 "Left-Super")
    (#x104027 "Right-Super")
    (#x104028 "Left-Hyper")
    (#x104029 "Right-Hyper")
    (#x1040F0 "KP-0")
    (#x1040F1 "KP-1")
    (#x1040F2 "KP-2")
    (#x1040F3 "KP-3")
    (#x1040F4 "KP-4")
    (#x1040F5 "KP-5")
    (#x1040F6 "KP-6")
    (#x1040F7 "KP-7")
    (#x1040F8 "KP-8")
    (#x1040F9 "KP-9")
    (#x1040FA "KP-Period")
    (#x1040FB "KP-Divide")
    (#x1040FC "KP-Multiply")
    (#x1040FD "KP-Minus")
    (#x1040FE "KP-Plus")
    (#x1040FF "KP-Enter")
    ;; ACPI keys.
    (#x104080 "Power")
    (#x104081 "Sleep")
    (#x104082 "Wake")
    ;; Windows Multimedia keys.
    (#x104090 "Next-Track")
    (#x104091 "Previous-Track")
    (#x104092 "Stop")
    (#x104093 "Play")
    (#x104094 "Mute")
    (#x104095 "Volume-Up")
    (#x104096 "Volume-Down")
    (#x104097 "Media-Select")
    (#x104098 "E-Mail")
    (#x104099 "Calculator")
    (#x10409A "Computer")
    (#x10409B "WWW-Search")
    (#x10409C "WWW-Home")
    (#x10409D "WWW-Back")
    (#x10409E "WWW-Forward")
    (#x10409F "WWW-Stop")
    (#x1040A0 "WWW-Refresh")
    (#x1040A1 "WWW-Favorites")))

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

(defun unicode-char-general-category (char)
  (let ((info (unicode-char-info char)))
    (when info
      (unicode-general-category-decode (ldb +unicode-info-general-category+ info)))))

(defun unicode-char-name (char &key (space-char #\Space))
  (let ((info (unicode-char-info char)))
    (when info
      (let ((start (ldb +unicode-info-name-offset+ info))
            (length (ldb +unicode-info-name-length+ info)))
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
                     (return-from match-unicode-name nil))))
             (peek-next ()
               (if (eql (1+ i) end)
                   nil
                   (char name (1+ i))))
             (spacep (char)
               (or (eql char #\Space)
                   (eql char #\_))))
        (let ((ch (char name i)))
          ;; Ignore spaces and medial hyphens
          (cond ((eql ch #\-)
                 (when (not (or (and prev-was-letter
                                     (not (spacep (peek-next))))
                                (and (not prev-was-letter)
                                     (spacep (peek-next)))))
                   (advance #\-)))
                ((spacep ch)
                 (setf prev-was-letter nil))
                ((find (char-upcase ch) *unicode-direct-name-codes*)
                 (advance (char-upcase ch))
                 (setf prev-was-letter t))
                (t (return-from match-unicode-name nil))))))))
