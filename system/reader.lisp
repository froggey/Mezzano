;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defvar *read-base* 10 "The current input base.")
(defvar *read-eval* t "Controls the #. reader macro.")
(defvar *read-default-float-format* 'single-float)
(defvar *read-suppress* nil)

(defvar *read-lookahead-table*)

(declaim (special *readtable* *standard-readtable*)
         (type readtable *readtable* *standard-readtable*))

(defstruct (readtable
             (:predicate readtablep)
             (:copier))
  (case :upcase :type (member :upcase :downcase :preserve :invert))
  (base-characters (make-array 256 :initial-element nil) :type (simple-vector 256))
  (extended-characters (make-hash-table) :type hash-table))

;;; TODO: At some point the init code must copy the standard readtable to create
;;; the initial readtable.
(defvar *protect-the-standard-readtable* nil)
(setf *standard-readtable* (make-readtable)
      *readtable* *standard-readtable*)

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  (when (eql to-readtable 'nil)
    (setf to-readtable (make-readtable)))
  (when (eql from-readtable 'nil)
    (setf from-readtable *standard-readtable*))
  (check-type to-readtable readtable)
  (check-type from-readtable readtable)
  (setf (readtable-case to-readtable) (readtable-case from-readtable))
  (dotimes (i 256)
    (set-syntax-from-char (code-char i) (code-char i) to-readtable from-readtable))
  (clrhash (readtable-extended-characters to-readtable))
  (maphash (lambda (k v)
             (declare (ignore))
             (set-syntax-from-char k k to-readtable from-readtable))
           (readtable-extended-characters from-readtable))
  to-readtable)

(defun readtable-syntax-type (char &optional (readtable *readtable*))
  (check-type readtable (or readtable null) "a readtable designator")
  (check-type char character)
  (unless readtable
    (setf readtable *standard-readtable*))
  (cond ((latin1-char-p char)
         ;; Base character.
         (svref (readtable-base-characters readtable) (char-code char)))
        (t ;; Extended character.
         (gethash char (readtable-extended-characters readtable) nil))))

(defun (setf readtable-syntax-type) (value char &optional (readtable *readtable*))
  (check-type readtable (or readtable null) "a readtable designator")
  (check-type char character)
  (unless readtable
    (setf readtable *standard-readtable*))
  (cond ((latin1-char-p char)
         ;; Base character.
         (setf (svref (readtable-base-characters readtable) (char-code char)) value))
        (t ;; Extended character.
         (setf (gethash char (readtable-extended-characters readtable)) value))))

(defun get-macro-character (char &optional (readtable *readtable*))
  (let ((data (readtable-syntax-type char readtable)))
    (if (listp data)
        (values (first data) (second data))
        (values nil nil))))

(defun set-macro-character (char new-function &optional non-terminating-p (readtable *readtable*))
  (check-type new-function (or symbol function) "a function designator")
  (when (and (null readtable) *protect-the-standard-readtable*)
    (cerror "Carry on!" "This would modify the standard readtable."))
  (if new-function
      (setf (readtable-syntax-type char readtable)
            (list new-function (not non-terminating-p)))
      (setf (readtable-syntax-type char readtable) nil))
  t)

(defun make-dispatch-macro-character (char &optional non-terminating-p (readtable *readtable*))
  (when (and (null readtable) *protect-the-standard-readtable*)
    (cerror "Carry on!" "This would modify the standard readtable."))
  (setf (readtable-syntax-type char readtable)
        (list 'read-dispatch-char (not non-terminating-p)
              (make-array 256 :initial-element nil)
              (make-hash-table)))
  t)

(defun get-dispatch-macro-character (disp-char sub-char &optional (readtable *readtable*))
  (check-type sub-char character)
  (let ((data (readtable-syntax-type disp-char readtable)))
    (unless (and (listp data) (= (length data) 4))
      (error "Character ~S is not a dispatching macro character." disp-char))
    (cond ((latin1-char-p sub-char)
           (svref (third data) (char-code sub-char)))
          (t (gethash sub-char (fourth data))))))

(defun set-dispatch-macro-character (disp-char sub-char new-function &optional (readtable *readtable*))
  (check-type sub-char character)
  (setf sub-char (char-upcase sub-char))
  (when (and (null readtable) *protect-the-standard-readtable*)
    (cerror "Carry on!" "This would modify the standard readtable."))
  (check-type new-function (or symbol function) "a function designator")
  (let ((data (readtable-syntax-type disp-char readtable)))
    (unless (and (listp data) (= (length data) 4))
      (error "Character ~S is not a dispatching macro character." disp-char))
    (cond ((latin1-char-p sub-char)
           (setf (svref (third data) (char-code sub-char)) new-function))
          (t (setf (gethash sub-char (fourth data)) new-function))))
  t)

(defun set-syntax-from-char (to-char from-char &optional (to-readtable *readtable*) from-readtable)
  (check-type to-char character)
  (check-type from-char character)
  (let ((data (readtable-syntax-type from-char from-readtable)))
    (cond ((not (consp data))
           (setf (readtable-syntax-type to-char to-readtable) data))
          ((= (length data) 4)
           ;; Dispatching character, must copy the dispatch tables.
           (let ((ht (make-hash-table)))
             (maphash (lambda (k v) (setf (gethash k ht) v)) (fourth data))
             (setf (readtable-syntax-type to-char to-readtable)
                   (list (first data) (second data)
                         (make-array 256 :initial-contents (third data))
                         ht))))
          (t (setf (readtable-syntax-type to-char to-readtable)
                   (copy-list data)))))
  t)

(defun case-correct (c &optional (rt *readtable*))
  "Change the case of C depending on the readtable-case of RT."
  (ecase (readtable-case rt)
    (:upcase (char-upcase c))
    (:downcase (char-downcase c))
    (:preserve c)
    (:invert (if (upper-case-p c)
                 (char-downcase c)
                 (char-upcase c)))))

(defun follow-stream-designator (stream default)
  (cond ((null stream) default)
        ((eql stream 't) *terminal-io*)
        ((streamp stream) stream)
        (t (error 'type-error
                  :expected-type '(or stream null (eql t))
                  :datum stream))))

(defun whitespace[2]p (char &optional (readtable *readtable*))
  "Test if CHAR is a whitespace[2] character under READTABLE."
  (eql (readtable-syntax-type char readtable) :whitespace))

;;; TODO: Unicode awareness.
(defun invalidp (char &optional (readtable *readtable*))
  "Test if CHAR is an invalid character under READTABLE."
  (and (eql (readtable-syntax-type char readtable) nil)
       (or (member char '(#\Backspace #\Tab #\Newline #\Linefeed #\Page
                          #\Return #\Space #\Rubout)))))

(defun decimal-point-p (char)
  "Test if CHAR is a decimal point character."
  (eql char #\.))

(defun plus-sign-p (char)
  "Test if CHAR is a plus sign character."
  (eql char #\+))

(defun minus-sign-p (char)
  "Test if CHAR is a minus sign character."
  (eql char #\-))

(defun terminating-macro-p (char &optional (readtable *readtable*))
  (multiple-value-bind (fn terminatingp)
      (get-macro-character char readtable)
    (declare (ignore fn))
    terminatingp))

(defun read-token (stream first)
  "Read a normal Lisp token from STREAM with FIRST as the initial character."
  (when *read-suppress*
    ;; Read characters until EOF or a whitespace character or terminating macro character is seen.
    ;; When *READ-SUPPRESS* is in effect, a reduced version of the normal token reading
    ;; code is used. Package markers and dots are ignored and integers are not parsed.
    (do ((x first (read-char stream nil 'nil t)))
        ((or (eql x 'nil)
             (when (or (terminating-macro-p x) (whitespace[2]p x))
               (unread-char x stream)
               t)))
      (let ((syntax (readtable-syntax-type x)))
        (cond ((eql syntax :single-escape)
               ;; Single escape character, read the next character directly.
               (read-char stream t nil t))
              ((eql syntax :multiple-escape)
               ;; Multiple escape character, read characters until another multiple escape character
               ;; is seen. Treat single escape characters as above.
               (do ((y (read-char stream t nil t)
                       (read-char stream t nil t)))
                   ((multiple-escape-p y))
                 (when (single-escape-p y)
                   (read-char stream t nil t)))))))
    (return-from read-token nil))
  ;; Normal read code, without *READ-SUPPRESS* enabled
  (let ((token (make-array 16 :element-type 'character
                           :adjustable t
                           :fill-pointer 0))
        (seen-escape nil)
        (package-name nil)
        ;; Set to true when a double package marker is used. ::
        (intern-symbol nil))
    (do ((x first (read-char stream nil 'nil t)))
        ;; Read characters until EOF or a whitespace character or terminating macro character is seen.
        ((or (eql x 'nil)
             (when (or (terminating-macro-p x) (whitespace[2]p x))
               (unread-char x stream)
               t)))
      (let ((syntax (readtable-syntax-type x)))
        (cond ((eql syntax :single-escape)
               ;; Single escape character, read the next character directly
               ;; and do not try to parse the token as a number.
               (let ((y (read-char stream t nil t)))
                 (setf seen-escape t)
                 (vector-push-extend y token)))
              ((eql syntax :multiple-escape)
               ;; Multiple escape character, read characters until another multiple escape character
               ;; is seen. Treat single escape characters as above. Don't parse the token as a number.
               (setf seen-escape t)
               (do ((y (read-char stream t nil t)
                       (read-char stream t nil t)))
                   ((eql (readtable-syntax-type y) :multiple-escape))
                 (if (eql (readtable-syntax-type y) :single-escape)
                     (vector-push-extend (read-char stream t nil t) token)
                     (vector-push-extend y token))))
              ((eql x #\:)
               ;; Treat the token that was read in as a package name.
               (when package-name
                 (error 'simple-reader-error :stream stream
                        :format-control "Too many package markers."
                        :format-arguments '()))
               ;; Test for "::" and invalid uses of the package marker
               (let ((y (read-char stream t nil t)))
                 (when (or (terminating-macro-p y) (whitespace[2]p y))
                   (error 'simple-reader-error :stream stream
                          :format-control "Invalid character ~S following package marker."
                          :format-arguments (list y)))
                 (if (eql y #\:)
                     (setf intern-symbol t)
                     (unread-char y stream))
                 ;; ":" and "::" with no leading package name denotes the KEYWORD package.
                 (if (and (not seen-escape) (= 0 (length token)))
                     (setf package-name "KEYWORD")
                     (setf package-name token
                           token (make-array 16
                                             :element-type 'character
                                             :adjustable t
                                             :fill-pointer 0)))))
              (t (vector-push-extend (case-correct x) token)))))
    ;; Check for invalid uses of the dot. Tokens that are constructed
    ;; entirely from dots are invalid unless one or more of the dots was
    ;; escaped or a package name was explicitly specified.
    (unless (or package-name seen-escape)
      (do ((offset 0 (1+ offset))
           (dot-count 0))
          ((>= offset (fill-pointer token))
           (when (= dot-count (fill-pointer token))
             (error 'simple-reader-error :stream stream
                    :format-control "Token ~S only contains dots."
                    :format-arguments (list token))))
        (when (eql (char token offset) #\.)
          (incf dot-count))))
    (cond
      ;; Return a symbol immediately if a package marker was seen.
      (package-name
       (if (or intern-symbol (string= "KEYWORD" package-name))
           (intern token package-name)
           (multiple-value-bind (symbol status) (find-symbol token package-name)
             (unless (eql status :external)
               (error 'simple-reader-error :stream stream
                      :format-control "Symbol ~S is internal to package ~S."
                      :format-arguments (list token package-name)))
             symbol)))
      ;; If an escape character was seen, then do not try to parse the token as
      ;; an number, intern it and return it immediately.
      (seen-escape
       (intern token))
      ;; Attempt to parse a number.
      (t (or (read-integer token)
             (read-float token)
             (read-ratio token)
             (intern token))))))

(defun read-ratio (string)
  ;; ratio = sign? digits+ slash digits+
  (let ((numerator nil)
        (denominator nil)
        (negativep nil)
        (start 0)
        (len (length string)))
    (when (zerop (length string))
      (return-from read-ratio))
    ;; Check for a leading sign.
    (case (char string 0)
      (#\- (incf start)
           (setf negativep t))
      (#\+ (incf start)))
    ;; Ensure at least one numerator digit.
    (when (>= start len)
      (return-from read-ratio))
    (setf numerator (digit-char-p (char string start) *read-base*))
    (when (not numerator)
      (return-from read-ratio))
    (loop
       (incf start)
       (when (>= start len)
         (return-from read-ratio))
       (let ((ch (char string start)))
         ;; Numerator is terminated by a slash.
         (when (eql ch #\/)
           (return))
         (let ((weight (digit-char-p ch *read-base*)))
           (when (not weight)
             (return-from read-ratio))
           (setf numerator (+ (* numerator *read-base*)
                              weight)))))
    ;; Skip over the slash.
    (incf start)
    ;; Ensure at least one denominator digit.
    (when (>= start len)
      (return-from read-ratio))
    (setf denominator (digit-char-p (char string start) *read-base*))
    (when (not denominator)
      (return-from read-ratio))
    (loop
       (incf start)
       (when (>= start len)
         (return))
       (let ((weight (digit-char-p (char string start) *read-base*)))
         (when (not weight)
           (return-from read-ratio))
         (setf denominator (+ (* denominator *read-base*)
                              weight))))
    (/ numerator denominator)))

(defvar *exponent-markers* "DdEeFfLlSs")
(defvar *decimal-digits* "0123456789")

(defun read-float (string)
  ;; float    = sign? decimal-digit* decimal-point decimal-digit+
  ;;          = sign? decimal-digit+ [decimal-point decimal-digit*] exponent
  ;; exponent = exponent-marker [sign] decimal-digit+
  ;; exponent-marker = d | D | e | E | f | F | l | L | s | S
  (let ((integer-part 0)
        ;; Work in double-precision.
        (decimal-part 0.0d0)
        (saw-integer-digits nil)
        (saw-decimal-digits nil)
        (saw-decimal-point nil)
        (exponent nil)
        (exponent-sign 1)
        (exponent-value 0)
        (sign 1)
        (position 0))
    (flet ((peek ()
             (when (< position (length string))
               (char string position)))
           (consume ()
             (prog1 (char string position)
               (incf position))))
      ;; Check for a leading sign.
      (case (peek)
        (#\- (consume)
             (setf sign -1))
        (#\+ (consume)))
      ;; Remaining string must not be empty.
      (when (null (peek))
        (return-from read-float))
      ;; Parse the integer portion.
      (loop
         (let ((weight (position (peek) *decimal-digits*)))
           (when (not weight) (return))
           (consume)
           (setf saw-integer-digits t)
           (setf integer-part (+ (* integer-part 10.0d0) weight))))
      ;; Parse the decimal portion.
      (when (decimal-point-p (peek))
        (setf saw-decimal-point t)
        (consume)
        ;; If there was an integer part, then the next character
        ;; must be either a decimal-digit or an exponent marker.
        ;; If there was no integer part, it must be a decimal-digit.
        (when (and (not (or (not saw-integer-digits)
                            (find (peek) *exponent-markers*)))
                   (not (find (peek) *decimal-digits*)))
          (return-from read-float))
        ;; Accumulate decimal digits.
        (let ((first-decimal position))
          (loop
             (when (not (find (peek) *decimal-digits*))
               (return))
             (setf saw-decimal-digits t)
             (consume))
          ;; Now works backwards and build the decimal part.
          (dotimes (i (- position first-decimal))
            (incf decimal-part (digit-char-p (char string (- position i 1))))
            (setf decimal-part (/ decimal-part 10)))))
      ;; And look for an exponent.
      (when (find (peek) *exponent-markers*)
        (setf exponent (consume))
        (case (peek)
          (#\- (consume)
               (setf exponent-sign -1))
          (#\+ (consume)))
        ;; Must be at least one digit in the exponent
        ;; and one digit in the integer part
        (when (or (not (find (peek) *decimal-digits*))
                  (not saw-integer-digits))
          (return-from read-float))
        ;; Read exponent part.
        (loop (when (not (find (peek) *decimal-digits*))
                (return))
           (setf exponent-value (+ (* exponent-value 10)
                                   (digit-char-p (consume))))))
      ;; Must be at the end.
      (when (peek)
        (return-from read-float))
      ;; Must have seen either a decimal point or exponent.
      (when (not (or saw-decimal-point exponent))
        (return-from read-float))
      ;; TODO, deal with float type selection correctly.
      (coerce
       (* sign
          (+ integer-part decimal-part)
          ;; ### 10.0 to work around a missing feature in FLOAT. No bignum support.
          (expt 10.0d0 (* exponent-sign exponent-value)))
       (ecase (char-upcase (or exponent #\E))
         (#\S 'short-float)
         (#\F 'single-float)
         (#\D 'double-float)
         (#\L 'long-float)
         (#\E *read-default-float-format*))))))

(defun read-integer (token)
  "Attempt to parse TOKEN as an integer. Return false if it can't be parsed as an integer."
  ;; integer = sign? decimal-digit+ decimal-point
  ;;         = sign? digit+
  (let ((read-base *read-base*)
        (negative nil)
        (saw-sign nil)
        (number 0)
        (start 0)
        (end (length token)))
    ;; Check for a trailing decimal point, identifying a decimal integer
    (when (decimal-point-p (char token (1- (length token))))
      (setf read-base 10
            end (1- end)))
    ;; Check for a leading sign.
    (when (or (plus-sign-p (char token 0))
              (minus-sign-p (char token 0)))
      (setf saw-sign t
            negative (minus-sign-p (char token 0))
            start (1+ start)))
    ;; If the token is empty, aside from the sign, then it isn't a number.
    (when (not (= (- end start) 0))
      ;; Main digit-reading loop for integers.
      (do ((offset start (1+ offset)))
          ((>= offset end)
           ;; Successfully parsed an integer!
           (if negative
               (- number)
               number))
        (let ((weight (digit-char-p (char token offset) read-base)))
          (when (not weight)
            ;; This character is not a digit in the current base.
            (return))
          (setf number (+ (* number read-base) weight)))))))

;; Can't use read-delimited-list because it doesn't handle dotted lists
(defun read-left-parenthesis (stream first)
  "Read a possibly dotted list from STREAM."
  (declare (ignore first))
  (do* ((list (cons nil nil))
        (tail list))
       (nil)
    (let ((x (peek-char t stream t nil t)))
      (cond
        ((eql x #\))
         ;; Done, finish list
         ;; Read the ) and drop it
         (read-char stream t nil t)
         (return (if *read-suppress* nil (cdr list))))
        ((and (eql x #\.)
              (not *read-suppress*))
         ;; Reading a potentially dotted list
         ;; Read the dot and drop it
         (read-char stream t nil t)
         (let ((y (peek-char nil stream t nil t)))
           (when (or (terminating-macro-p y) (whitespace[2]p y))
             (let ((final (read stream t nil t)))
               (when (eql list tail)
                 (error 'simple-reader-error :stream stream
                        :format-control "No forms before dot in dotted list."
                        :format-arguments '()))
               (unless (char= (peek-char t stream t nil t) #\))
                 (error 'simple-reader-error :stream stream
                        :format-control "Too many elements after dot in dotted list."
                        :format-arguments '()))
               (read-char stream t nil t)
               (setf (cdr tail) final)
               (return (if *read-suppress* nil (cdr list))))))
         ;; Oops, it wasn't a dotted list and we can't unread
         ;; the dot because of the peek-char call
         ;; Manually dispatch to the next reader function
         (let ((value (multiple-value-list
                       (funcall (or (get-macro-character x)
                                    #'read-token)
                                stream x))))
           (when value
             (setf (cdr tail) (cons (car value) nil))
             (setf tail (cdr tail)))))
        (t (let* ((c (read-char stream t nil t))
                  (value (multiple-value-list (funcall (or (get-macro-character c)
                                                           #'read-token)
                                                       stream
                                                       c))))
             (when value
               (setf (cdr value) nil
                     (cdr tail) value
                     tail value))))))))

(defun read-right-parenthesis (stream first)
  "Signals a reader-error when an unexpected #\) is seen."
  (declare (ignore stream))
  (error 'simple-reader-error :stream stream
         :format-control "Unexpected ~S."
         :format-arguments (list first)))

(defun read-single-quote (stream first)
  "Reads a quoted form from STREAM."
  (declare (ignore first))
  (list 'quote (read stream t nil t)))

(defun read-semicolon (stream first)
  "Reads a newline-terminated comment from STREAM."
  (declare (ignore first))
  (do () ((char= (read-char stream nil #\Newline nil) #\Newline)))
  (values))

(defun read-double-quote (stream first)
  "Reads a string terminated by FIRST from STREAM."
  (do ((string (make-array 16
                           :element-type 'character
                           :adjustable t
                           :fill-pointer 0))
       (x (read-char stream t nil t)
          (read-char stream t nil t)))
      ((eql x first)
       ;; Convert to simple-string.
       (make-array (length string)
                   :element-type 'character
                   :initial-contents string))
    (if (eql (readtable-syntax-type x) :single-escape)
        (vector-push-extend (read-char stream t nil t) string)
        (vector-push-extend x string))))

(defun read-backquote (stream first)
  (declare (ignore first))
  (list 'backquote (read stream t nil t)))

(defun read-comma (stream first)
  (declare (ignore first))
  (case (peek-char nil stream t)
    (#\@ (read-char stream t nil t)
         (list 'bq-comma-atsign (read stream t nil t)))
    (#\. (read-char stream t nil t)
         (list 'bq-comma-dot (read stream t nil t)))
    (otherwise
     (list 'bq-comma (read stream t nil t)))))

(defun read-dispatch-char (stream first)
  "Dispatch to a dispatching macro character."
  (let ((c (read-char stream t nil t))
        (p nil))
    ;; If there's a leading number, read it
    (when (digit-char-p c)
      (setf p 0)
      (do () ((not (digit-char-p c)))
        (setf p (+ (digit-char-p c) (* p 10)))
        (setf c (read-char stream t nil t))))
    (setf c (char-upcase c))
    ;; Dispatch to the function
    (let ((fn (get-dispatch-macro-character first c)))
      (unless fn
        (error "No dispatch character defined for ~S." c))
      (funcall fn stream c p))))

(defun ignore-#-argument (ch p)
  "Issue a warning if p is not NIL."
  (when (and p (not *read-suppress*))
    (warn "Ignored numeric argument in #~A~A." p ch)))

(defun read-#-asterisk (stream ch p)
  (declare (ignore ch))
  (let ((bits (make-array 50 :element-type 'bit :adjustable t :fill-pointer 0)))
    (loop (let ((ch (read-char stream nil nil t)))
            (when (or (not ch) (whitespace[2]p ch) (terminating-macro-p ch))
              (when ch
                (unread-char ch stream))
              (return))
            (case ch
              (#\0 (vector-push-extend 0 bits))
              (#\1 (vector-push-extend 1 bits))
              (t (error "Invalid character ~S in #*." ch)))))
    (when (and p (not (zerop p)) (zerop (length bits)))
      (error "Need at least one bit for #* with an explicit length."))
    (when (and p (> (length bits) p))
      (error "Too many bits."))
    (let ((final-array (make-array (or p (length bits))
                                   :element-type 'bit)))
      (unless (zerop (length final-array))
        (fill final-array (aref bits (1- (length bits))))
        (setf (subseq final-array 0 (length bits)) bits))
      final-array)))

(defun read-#-backslash (stream ch p)
  (ignore-#-argument ch p)
  (let ((x (read-char stream t nil t))
        (y (peek-char nil stream nil nil t)))
    (if (or (eql nil y)
            (get-macro-character y)
            (whitespace[2]p y))
        ;; Simple form: Single character followed by EOF or a non-constituent character.
        ;; Just return the character that was read.
        x
        ;; Reading a character name, similar to read-token, but no special handling
        ;; is done for packages or numbers.
        (let ((token (make-array 1
                                 :element-type 'character
                                 :initial-element x
                                 :adjustable t
                                 :fill-pointer t)))
          (do ((z (read-char stream nil nil t)
                  (read-char stream nil nil t)))
              ((or (eql nil z)
                   (when (or (get-macro-character z) (whitespace[2]p z))
                     (unread-char z stream)
                     t)))
            (let ((syntax (readtable-syntax-type x)))
              (cond ((eql syntax :single-escape)
                     (vector-push-extend (read-char stream t nil t) token))
                    ((eql syntax :multiple-escape)
                     (do ((y (read-char stream t nil t)
                             (read-char stream t nil t)))
                         ((multiple-escape-p y))
                       (if (single-escape-p y)
                           (vector-push-extend (read-char stream t nil t) token)
                           (vector-push-extend y token))))
                    (t (vector-push-extend (case-correct z) token)))))
          ;; Finished reading the token, convert it to a character
          (let ((c (name-char token)))
            (when (and (not c) (not *read-suppress*))
              (error 'simple-reader-error :stream stream
                     :format-control "Unrecognized character name ~S."
                     :format-arguments (list token)))
            c)))))

(defun read-#-quote (stream ch p)
  (ignore-#-argument ch p)
  (list 'function (read stream t nil t)))

(defun read-#-colon (stream ch p)
  (ignore-#-argument ch p)
  ;; Use read-token to read the symbol name as a keyword.
  ;; Reading it as a keyword suppresses the integer parsing code
  ;; and forces it to produce a symbol.
  ;; FIXME: This causes a symbol with the same name to be added
  ;; to the KEYWORD package as a side effect. It would be nice to
  ;; avoid that.
  (let ((token (read-token stream #\:)))
    (if *read-suppress*
        nil
        (make-symbol (symbol-name token)))))

(defun read-#-dot (stream ch p)
  (ignore-#-argument ch p)
  (cond (*read-suppress*
         (read stream t nil t))
        (*read-eval*
         (eval (read stream t nil t)))
        (t (error 'simple-reader-error :stream stream
                  :format-control "Cannot #. when *READ-EVAL* is false."
                  :format-arguments '()))))

(defun read-#-radix (stream ch p)
  "Read a number in the specified radix."
  ;; This function is used for #B, #O and #X in addition to #R
  (when *read-suppress*
    (read stream t nil t)
    (return-from read-#-radix))
  (let* ((bases '((#\B . 2) (#\O . 8) (#\X . 16)))
         (fixed-base (assoc ch bases))
         (base (cond (fixed-base
                      (when p
                        (warn "Ignored explicit base in #~A~A" p ch))
                      (cdr fixed-base))
                     (p p)
                     (t (error "No radix specified in #~A" ch)))))
    (declare (type (integer 2 36) base))
    ;; Successfully computed what base to use, rebind
    ;; *READ-BASE* and read a value, checking that it's of type RATIONAL
    (let* ((*read-base* base))
      (the rational (read stream t nil t)))))

(defun read-#-left-parenthesis (stream ch p)
  (let ((foo (read-left-parenthesis stream #\()))
    (unless *read-suppress*
      (apply #'vector foo))))

(defun read-#-complex (stream ch p)
  "Read a complex number."
  (ignore-#-argument ch p)
  (when *read-suppress*
    (read stream t nil t)
    (return-from read-#-complex))
  (let ((number (read stream t nil t)))
    (when (or (not (listp number))
              (/= (length number) 2)
              (not (realp (car number)))
              (not (realp (cadr number))))
      (error "Invalid complex number ~S" number))
    (complex (first number) (second number))))

(defun read-#-array (stream ch n-dimensions)
  (declare (ignore ch))
  (check-type n-dimensions array-axis)
  (when *read-suppress*
    (read stream t nil t)
    (return-from read-#-array))
  (let* ((object (read stream t nil t))
         (current-dim object)
         (dimensions (make-list n-dimensions :initial-element 0)))
    (dotimes (i n-dimensions)
      (let ((len (length current-dim)))
        (setf (elt dimensions i) len)
        (when (zerop len) (return))
        (setf current-dim (elt current-dim 0))))
    (make-array dimensions :initial-contents object)))

(defun read-#-pathname (stream ch p)
  (ignore-#-argument ch p)
  (cond (*read-suppress*
         (read stream t nil t)
         nil)
        (t (parse-namestring (read stream t nil t)))))

(defun eval-feature-test (test)
  "Evaluate the feature expression TEST."
  (etypecase test
    (symbol (member test *features*))
    (cons (case (car test)
            (:not (when (or (null (cdr test)) (cddr test))
                    (error "Invalid feature expression ~S" test))
                  (not (eval-feature-test (cadr test))))
            (:and (dolist (subexpr (cdr test) t)
                    (when (not (eval-feature-test subexpr))
                      (return nil))))
            (:or (dolist (subexpr (cdr test) nil)
                   (when (eval-feature-test subexpr)
                     (return t))))
            (t (error "Invalid feature expression ~S" test))))))

(defun read-#-features (stream suppress-if-false)
  "Common function to implement #+ and #-."
  (let* ((test (let ((*package* (find-package "KEYWORD")))
                 (read stream t nil t)))
         (*read-suppress* (or *read-suppress*
                              (if suppress-if-false
                                  (not (eval-feature-test test))
                                  (eval-feature-test test))))
         (value (read stream t nil t)))
    (if *read-suppress*
        (values)
        value)))

(defun read-#-plus (stream ch p)
  (ignore-#-argument ch p)
  (read-#-features stream t))

(defun read-#-minus (stream ch p)
  (ignore-#-argument ch p)
  (read-#-features stream nil))

(defun read-#-vertical-bar (stream ch p)
  "Read the nestable #| ... |# comment."
  (ignore-#-argument ch p)
  (loop
     (case (read-char stream t nil t)
       (#\#
        ;; Look for | and recurse.
        (when (eql (peek-char nil stream t nil t) #\|)
          (read-char stream) ; eat |.
          (read-#-vertical-bar stream #\| nil)))
       (#\| ;; Check for |# and finish.
        (when (eql (peek-char nil stream t nil t) #\#)
          (read-char stream)
          (return)))))
  (values))

(defun read-#-invalid (stream ch p)
  "Handle explicitly invalid # dispatch characters."
  (declare (ignore stream p))
  (error 'simple-reader-error :stream stream
         :format-control "Illegal syntax #~A."
         :format-arguments (list ch)))

(defun read-#-equal-sign (stream ch p)
  (declare (ignore ch))
  (let ((value (read stream t nil t)))
    (cond (*read-suppress*
           (values))
          (t
           (setf (gethash p *read-lookahead-table*) value)
           value))))

(defun read-#-sharp-sign (stream ch p)
  (declare (ignore stream ch))
  (cond (*read-suppress*
         (values))
        (t
         (multiple-value-bind (value existsp)
             (gethash p *read-lookahead-table*)
           (when (not existsp)
             (cerror "Read NIL" "Unknown read ## value ~D" p))
           value))))

(defun read-common (stream eof-error-p eof-value recursive-p)
  (let ((*read-lookahead-table* (if recursive-p
                                    *read-lookahead-table*
                                    (make-hash-table))))
    ;; Skip leading whitespace.
    (loop (let ((c (read-char stream eof-error-p 'nil t)))
            (when (eql c 'nil)
              (return eof-value))
            (when (invalidp c)
              (error 'simple-reader-error :stream stream
                     :format-control "Read invalid character ~S."
                     :format-arguments (list c)))
            (unless (whitespace[2]p c)
              ;; Done reading whitespace. Dispatch to the appropriate
              ;; read subfunction.
              (let ((value (multiple-value-list (funcall (or (get-macro-character c)
                                                             #'read-token)
                                                         stream c))))
                (when value
                  (return (first value)))))))))

(defun read (&optional input-stream (eof-error-p t) eof-value recursive-p)
  "READ parses the printed representation of an object from STREAM and builds such an object."
  (let ((stream (follow-stream-designator input-stream *standard-input*)))
    (with-stream-editor (stream recursive-p)
      (let ((result (read-common stream
                                 eof-error-p
                                 eof-value
                                 recursive-p)))
        (unless (or (eql result eof-value) recursive-p)
          ;; Munch trailing whitespace iff not at EOF and not in a recursive call.
          (let ((ch (read-char stream nil nil)))
            (when (and ch (not (whitespace[2]p ch)))
              (unread-char ch stream))))
        result))))

(defun read-preserving-whitespace (&optional input-stream (eof-error-p t) eof-value recursive-p)
  (read-common (follow-stream-designator input-stream *standard-input*)
               eof-error-p
               eof-value
               recursive-p))

(defun read-from-string (string &optional (eof-error-p t) eof-value &key (start 0) end preserve-whitespace)
  (let (index)
    (values
     (with-input-from-string (stream string :start start :end end :index index)
       (if preserve-whitespace
           (read-preserving-whitespace stream eof-error-p eof-value)
           (read stream eof-error-p eof-value)))
     index)))

(defmacro with-standard-io-syntax (&body body)
  `(%with-standard-io-syntax (lambda () (progn ,@body))))

(defun %with-standard-io-syntax (fn)
  (let ((*package* (find-package-or-die "CL-USER"))
        (*print-array* t)
        (*print-base* 10)
        (*print-case* :upcase)
        (*print-circle* nil)
        (*print-escape* t)
        (*print-gensym* t)
        (*print-length* nil)
        (*print-level* nil)
        (*print-lines* nil)
        (*print-miser-width* nil)
        ;;(*print-pprint-dispatch* standard-pprint-dispatch-table)
        (*print-pretty* nil)
        (*print-radix* nil)
        (*print-readably* t)
        (*print-right-margin* nil)
        (*read-base* 10)
        (*read-default-float-format* 'single-float)
        (*read-eval* t)
        (*read-suppress* nil)
        (*readtable* (copy-readtable nil)))
    (funcall fn)))

(defun read-delimited-list (char &optional input-stream recursive-p)
  (do* ((result '()))
       (nil)
    (let ((x (peek-char t input-stream)))
      (cond
        ((eql x char)
         ;; Done, finish list
         ;; Read the ) and drop it
         (read-char input-stream)
         (return (if *read-suppress*
                     nil
                     (reverse result))))
        (t
         (push (read input-stream t nil recursive-p) result))))))

;;; Set standard reader macros.
(defun initialize-standard-readtable (readtable)
;;; Set basic syntax traits for standard characters.
  (setf (readtable-syntax-type #\Tab readtable) :whitespace
        (readtable-syntax-type #\Newline readtable) :whitespace
        (readtable-syntax-type #\Linefeed readtable) :whitespace
        (readtable-syntax-type #\Page readtable) :whitespace
        (readtable-syntax-type #\Return readtable) :whitespace
        (readtable-syntax-type #\Space readtable) :whitespace
        (readtable-syntax-type #\\ readtable) :single-escape
        (readtable-syntax-type #\| readtable) :multiple-escape)
  ;; And standard reader macros.
  (set-macro-character #\( 'read-left-parenthesis nil readtable)
  (set-macro-character #\) 'read-right-parenthesis nil readtable)
  (set-macro-character #\' 'read-single-quote nil readtable)
  (set-macro-character #\; 'read-semicolon nil readtable)
  (set-macro-character #\" 'read-double-quote nil readtable)
  (set-macro-character #\` 'read-backquote nil readtable)
  (set-macro-character #\, 'read-comma nil readtable)
  (make-dispatch-macro-character #\# t readtable)
  (set-dispatch-macro-character #\# #\\ 'read-#-backslash readtable)
  (set-dispatch-macro-character #\# #\' 'read-#-quote readtable)
  (set-dispatch-macro-character #\# #\( 'read-#-left-parenthesis readtable)
  (set-dispatch-macro-character #\# #\* 'read-#-asterisk readtable)
  (set-dispatch-macro-character #\# #\: 'read-#-colon readtable)
  (set-dispatch-macro-character #\# #\. 'read-#-dot readtable)
  (set-dispatch-macro-character #\# #\B 'read-#-radix readtable)
  (set-dispatch-macro-character #\# #\O 'read-#-radix readtable)
  (set-dispatch-macro-character #\# #\X 'read-#-radix readtable)
  (set-dispatch-macro-character #\# #\R 'read-#-radix readtable)
  (set-dispatch-macro-character #\# #\C 'read-#-complex readtable)
  (set-dispatch-macro-character #\# #\A 'read-#-array readtable)
  (set-dispatch-macro-character #\# #\S 'read-#-struct readtable)
  (set-dispatch-macro-character #\# #\P 'read-#-pathname readtable)
  (set-dispatch-macro-character #\# #\= 'read-#-equal-sign readtable)
  (set-dispatch-macro-character #\# #\# 'read-#-sharp-sign readtable)
  (set-dispatch-macro-character #\# #\+ 'read-#-plus readtable)
  (set-dispatch-macro-character #\# #\- 'read-#-minus readtable)
  (set-dispatch-macro-character #\# #\| 'read-#-vertical-bar readtable)
  (set-dispatch-macro-character #\# #\< 'read-#-invalid readtable)
  (set-dispatch-macro-character #\# #\Newline 'read-#-invalid readtable)
  (set-dispatch-macro-character #\# #\Space 'read-#-invalid readtable)
  (set-dispatch-macro-character #\# #\Tab 'read-#-invalid readtable)
  (set-dispatch-macro-character #\# #\Page 'read-#-invalid readtable)
  (set-dispatch-macro-character #\# #\) 'read-#-invalid readtable))

(setf *protect-the-standard-readtable* nil)
(initialize-standard-readtable *standard-readtable*)
(setf *protect-the-standard-readtable* t)
(setf *readtable* (copy-readtable nil))
