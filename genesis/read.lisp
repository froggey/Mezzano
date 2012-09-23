;;;; Early Genesis reader.

(in-package #:genesis)

(defun looks-like-a-float (token)
  "An optional leading sign, followed by an integer part, then a
decimal point, then a decimal part."
  (let ((start 0)
        (saw-decimal-point nil))
    (when (and (not (zerop (length token)))
               (member (char token 0) '(#\- #\+)))
      (incf start))
    (when (zerop (- (length token) start))
      (return-from looks-like-a-float nil))
    (iter (for i from start below (length token))
          (unless (digit-char-p (char token i))
            (cond ((or saw-decimal-point
                       (not (eql (char token i) #\.)))
                   (return-from looks-like-a-float nil))
                  (t (setf saw-decimal-point t)))))
    (and saw-decimal-point
         (not (eql (char token (1- (length token))) #\.)))))

(defun primitive-read-token (stream first-char &optional (intern-it t))
  (do ((token (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
       (x (if (or (not intern-it)
		  (not (eql first-char #\:)))
	      first-char
	      (read-char stream nil))
	  (read-char stream nil))
       (leading-sign (or (eql first-char #\+)
			 (eql first-char #\-)))
       (number 0))
      ((or (null x)
	   (member x '(#\( #\) #\' #\" #\; #\` #\, #\Newline #\Space)))
       (when x (unread-char x stream))
       (if intern-it
	   (cond
	     ((eql first-char #\:)
	      ;; Keyword.
              (genesis-intern token t))
	     ((eql (length token) (if leading-sign 1 0))
	      ;; Tokens with no digits can't be numbers.
              (genesis-intern token))
             ((looks-like-a-float token)
              ;; Pass it off to the host's reader, but ensure it's a single-float.
              (coerce (read-from-string token) 'single-float))
	     ;; Attempt to parse an integer.
	     ;; The trailing dot for decimal integers is not supported.
	     (t (do ((offset (if leading-sign 1 0) (1+ offset)))
		    ((>= offset (length token))
		     (if (eql first-char #\-)
			 (- number)
			 number))
		  (let ((weight (digit-char-p (char token offset) *read-base*)))
		    (when (not weight)
		      ;; This character is not a digit in the current base.
		      ;; Treat the token as a symbol.
		      (return (genesis-intern token)))
		    (setf number (+ (* number *read-base*) weight))))))
	   (make-symbol token)))
    (if (eql x #\\)
	(vector-push-extend (read-char stream) token)
	(vector-push-extend (char-upcase x) token))))

(defun primitive-read-list (stream)
  (do* ((list (list nil))
	(tail list (cdr tail))
	(x (peek-char t stream)
	   (peek-char t stream)))
       (nil)
    (case x
      (#\)
       (read-char stream)
       (return (cdr list)))
      (#\.
       (read-char stream)
       (setf x (peek-char nil stream))
       (cond ((or (alphanumericp x)
                  (char= x #\-)
                  (char= x #\+))
              ;; Not part of a dotted list.
              (setf (cdr tail) (cons (primitive-read-token stream #\.) nil)))
             (t (let ((final (primitive-read stream)))
                  (when (eql list tail)
                    (error "Nothing before dot."))
                  ;; Use PEEK-CHAR to skip leading whitespace.
                  (unless (char= (peek-char t stream) #\))
                    (error "Expected ')' after dot tail element."))
                  ;; Then READ-CHAR to drop the ).
                  (read-char stream)
                  (setf (cdr tail) final)
                  (return (cdr list))))))
      (otherwise
       (setf (cdr tail) (cons (primitive-read stream) nil))))))

(defun primitive-read (&optional stream (eof-error-p t) eof-value)
  ;; Skip leading whitespace.
  (peek-char t stream eof-error-p 'nil)
  (let ((c (read-char stream eof-error-p 'nil)))
    (case c
      ;; EOF.
      ((nil) eof-value)
      ;; Comment, read to EOL or EOF.
      (#\;
       (loop (let ((c (read-char stream eof-error-p 'nil)))
               (when (or (char= c #\Newline)
                         (eql c 'nil))
                 (return))))
       (primitive-read stream eof-error-p eof-value))
      (#\'
       ;; Quoted value.
       (list (genesis-intern "QUOTE")
             (primitive-read stream)))
      (#\"
       ;; String.
       (do ((buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t))
	    (char (read-char stream) (read-char stream)))
	   ((eql char #\") buf)
	 (if (eql char #\\)
	     (vector-push-extend (read-char stream) buf)
	     (vector-push-extend char buf))))
      (#\(
       (primitive-read-list stream))
      (#\) (error "Unexpected close-parenthesis."))
      (#\` (list (genesis-intern "BACKQUOTE")
                 (primitive-read stream)))
      (#\, (case (peek-char nil stream t)
             (#\@ (read-char stream t)
                  (list (genesis-intern "BQ-COMMA-ATSIGN")
                        (primitive-read stream)))
             (#\. (read-char stream t)
                  (list (genesis-intern "BQ-COMMA-DOT")
                        (primitive-read stream)))
             (otherwise
              (list (genesis-intern "BQ-COMMA")
                        (primitive-read stream)))))
      (#\#
       (setf c (read-char stream))
       (ecase (char-upcase c)
         (#\\
          (let* ((token (primitive-read-token stream #\\ nil))
                 (name (symbol-name token)))
            (if (eql (length name) 1)
                (char name 0)
                (or (name-char (string-upcase name))
                    (error "Unrecognized character name ~A." name)))))
         (#\'
          (list (genesis-intern "FUNCTION") (primitive-read stream)))
         (#\:
          (values (primitive-read-token stream (read-char stream) nil)))
         (#\B (let ((*read-base* 2)) (primitive-read stream)))
         (#\O (let ((*read-base* 8)) (primitive-read stream)))
         (#\X (let ((*read-base* 16)) (primitive-read stream)))))
      (t (primitive-read-token stream c)))))
