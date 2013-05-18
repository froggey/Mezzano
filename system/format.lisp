(in-package :sys.int)

(defun format-c (s args params at-sign colon)
  (when params (error "Expected 0 parameters."))
  (let ((c (car args)))
    (check-type c character)
    (cond ((and at-sign (not colon))
           (write c :stream s :escape t))
	  (colon
           (if (and (graphic-char-p c) (not (eql #\Space c)))
               (write-char c s)
               (write-string (char-name c) s))
           ;; TODO: colon & at-sign.
           ;; Describes how to type the character if it requires
           ;; unusual shift keys to type.
           (when at-sign))
          (t (write-char c s))))
  (cdr args))

(defun format-integer (s args base params at-sign colon)
  (let ((mincol (first params))
	(padchar (or (second params) #\Space))
	(commachar (or (third params) #\,))
	(comma-interval (or (fourth params) 3))
	(n (car args)))
    (unless (integerp n)
      (return-from format-integer
        (let ((*print-base* base))
          (format-a s args '() nil nil))))
    (check-type padchar character)
    (check-type commachar character)
    (check-type comma-interval integer)
    (when (cddddr params)
      (error "Expected 0 to 4 parameters."))
    (if (or mincol colon)
	;; Fancy formatting.
	(let ((buffer (make-array 8
				  :element-type 'character
				  :adjustable t
				  :fill-pointer 0))
	      (negative nil))
	  (when (minusp n)
	    (setf negative t
		  n (- n)))
	  (unless mincol (setf mincol 0))
          ;; Write the number backwards into the buffer, no commas or padding yet.
	  (if (= n 0)
	      (vector-push-extend #\0 buffer)
	      (do () ((= n 0))
		(multiple-value-bind (quot rem)
		    (truncate n base)
		  (vector-push-extend (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" rem) buffer)
		  (setf n quot))))
	  ;; TODO: count commas as well
	  (dotimes (i (- mincol (+ (length buffer) (if (or negative at-sign) 1 0))))
	    (write-char padchar s))
          (cond
            (negative
             (write-char #\- s))
            (at-sign
             (write-char #\+ s)))
	  (if colon
	      (dotimes (i (length buffer))
		(when (and (not (zerop i))
                           (zerop (rem (- (length buffer) i) comma-interval)))
		  (write-char commachar s))
		(write-char (char buffer (- (length buffer) i 1)) s))
	      (dotimes (i (length buffer))
		(write-char (char buffer (- (length buffer) i 1)) s))))
	(progn
	  (when (and at-sign (not (minusp n)))
	    (write-char #\+ s))
	  (write n :stream s :escape nil :radix nil :base base :readably nil))))
  (cdr args))

(defun format-r (s args params at-sign colon)
  (if params
      (let ((base (or (car params) 10)))
	(check-type base integer)
	(format-integer s args base (cdr params) at-sign colon))
      (error "TODO: Fancy ~~R")))

(defun format-b (s args params at-sign colon)
  (format-integer s args 2 params at-sign colon))

(defun format-o (s args params at-sign colon)
  (format-integer s args 8 params at-sign colon))

(defun format-d (s args params at-sign colon)
  (format-integer s args 10 params at-sign colon))

(defun format-x (s args params at-sign colon)
  (format-integer s args 16 params at-sign colon))

(defun format-a (s args params at-sign colon)
  (if (and colon (null (car args)))
      (write-string "()" s)
      (write (car args) :stream s :escape nil :readably nil))
  (cdr args))

(defun format-s (s args params at-sign colon)
  (if (and colon (null (car args)))
      (write-string "()" s)
      (write (car args) :stream s :escape t))
  (cdr args))

(defun format-~ (s args params at-sign colon)
  (when (cdr params)
    (error "Expected 0 or 1 parameters."))
  (let ((count (or (car params)
		   1)))
    (check-type count integer)
    (dotimes (i count)
      (write-char #\~ s)))
  args)

(defun format-% (s args params at-sign colon)
  (when (cdr params)
    (error "Expected 0 or 1 parameters."))
  (let ((count (or (car params)
		   1)))
    (check-type count integer)
    (dotimes (i count)
      (terpri s)))
  args)

(defun format-& (s args params at-sign colon)
  (when (cdr params)
    (error "Expected 0 or 1 parameters."))
  (let ((count (or (car params)
		   1)))
    (check-type count integer)
    (fresh-line s)
    (dotimes (i (1- count))
      (terpri s)))
  args)

(defun format-page (s args params at-sign colon)
  (when (cdr params)
    (error "Expected 0 or 1 parameters."))
  (let ((count (or (car params)
		   1)))
    (check-type count integer)
    (dotimes (i count)
      (write-char #\Page s)))
  args)

(defun format-write (s args params at-sign colon)
  (when params
    (error "~W takes no parameters."))
  (cond
    ((and at-sign colon)
     (write (car args) :stream s :pretty t :level nil :length nil))
    (at-sign
     (write (car args) :stream s :level nil :length nil))
    (colon
     (write (car args) :stream s :pretty t))
    (t (write (car args) :stream s)))
  (cdr args))

(defun format-conditional-newline (s args params at-sign colon)
  (when params
    (error "~_ takes no parameters."))
  (cond
    ((and at-sign colon)
     (pprint-newline :mandatory s))
    (at-sign
     (pprint-newline :miser s))
    (colon
     (pprint-newline :fill s))
    (t (pprint-newline :linear s)))
  args)

(defun format-indent (s args params at-sign colon)
  (when (cdr params)
    (error "Expected 0 or 1 parameters."))
  (let ((count (or (car params) 1)))
    (when at-sign
      (error "~I does not take the at-sign modifier."))
    (pprint-indent (if colon
                       :current
                       :block)
                   count))
  args)

(defun format-tabulate (s args params at-sign colon)
  (when (cddr params)
    (error "Expected 0, 1 or 2 parameters."))
  (let ((colnum (or (first params) 1))
        (colinc (or (second params) 1)))
    (cond (colon
           (pprint-tab (if at-sign :section-relative :section)
                       colnum colinc
                       s))
          (at-sign
           (dotimes (i colnum)
             (write-char #\Space s))
           (let ((current (sys.gray:stream-line-column s)))
             (when current
               (dotimes (i (- colinc (rem current colinc)))
                 (write-char #\Space s)))))
          (t (let ((current (sys.gray:stream-line-column s)))
               (cond ((not current)
                      (write-string "  " s))
                     ((< current colnum)
                      (dotimes (i (- colnum current))
                        (write-char #\Space s)))
                     ((not (zerop colinc))
                      (dotimes (i (- colinc (rem (- current colnum) colinc)))
                        (write-char #\Space s))))))))
  args)

(defun format-recurse (s args params at-sign colon)
  (when colon
    (error "~? does not take the colon modifier."))
  (when params
    (error "~? takes no parameters."))
  (cond (at-sign
         (multiple-value-bind (offset remaining-args)
             (interpret-format-substring s (pop args) 0 nil args)
           (declare (ignore offset))
           remaining-args))
        (t (interpret-format-substring s (pop args) 0 nil (pop args))
           args)))

(defun format-ignore (s args params at-sign colon)
  (declare (ignore s params at-sign colon))
  args)

(defparameter *format-functions*
  '((#\C format-c)
    (#\R format-r)
    (#\B format-b)
    (#\O format-o)
    (#\D format-d)
    (#\X format-x)
    (#\S format-s)
    (#\A format-a)
    (#\~ format-~)
    (#\% format-%)
    (#\& format-&)
    (#\| format-page)
    (#\W format-write)
    (#\_ format-conditional-newline)
    (#\I format-indent)
    (#\T format-tabulate)
    (#\? format-recurse)
    (#\< format-ignore)
    (#\> format-ignore)
    (#\[ format-ignore)
    (#\] format-ignore)
    (#\; format-ignore)
    (#\{ format-ignore)
    (#\} format-ignore)
    (#\^ format-ignore)))

(defun whitespace[1]p (c)
  (or (eql c #\Newline)
      (eql c #\Space)
      (eql c #\Rubout)
      (eql c #\Page)
      (eql c #\Tab)
      (eql c #\Backspace)))

(defun interpret-format-substring (destination control-string start end-char args)
  (do ((offset start (1+ offset)))
      ((>= offset (length control-string))
       (values offset args))
    (if (eql #\~ (char control-string offset))
        (let ((prefix-parameters nil)
              (at-sign-modifier nil)
              (colon-modifier nil)
              (current-prefix nil))
          (incf offset)
          ;; Read prefix parameters
          (do () (nil)
            (case (char-upcase (char control-string offset))
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\-)
               (when current-prefix (error "Invalid format control string ~S." control-string))
               ;; Eat digits until non-digit
               (let ((negative nil))
                 (setf current-prefix 0)
                 (case (char control-string offset)
                   (#\+ (incf offset))
                   (#\- (incf offset)
                        (setf negative t)))
                 (when (not (digit-char-p (char control-string offset)))
                   ;; The sign is optional, the digits are not.
                   (error "Invalid format control string ~S." control-string))
                 (do () ((not (digit-char-p (char control-string offset))))
                   (setf current-prefix (+ (* current-prefix 10)
                                           (digit-char-p (char control-string offset))))
                   (incf offset))
                 (when negative
                   (setf current-prefix (- current-prefix)))))
              (#\#
               (when current-prefix (error "Invalid format control string ~S." control-string))
               (incf offset)
               (setf current-prefix (length args)))
              (#\'
               (when current-prefix (error "Invalid format control string ~S." control-string))
               (incf offset)
               (setf current-prefix (char control-string offset))
               (incf offset))
              (#\,
               (incf offset)
               (push current-prefix prefix-parameters)
               (setf current-prefix nil))
              (t (return))))
          (when current-prefix
            (push current-prefix prefix-parameters))
          (setf prefix-parameters (nreverse prefix-parameters))
          ;; Munch all colons and at-signs
          (do () (nil)
            (case (char control-string offset)
              (#\@ (setf at-sign-modifier t)
                   (incf offset))
              (#\: (setf colon-modifier t)
                   (incf offset))
              (t (return))))
          (case (char control-string offset)
            (#\Newline
             ;; Newline must be handled specially, as it advances through the control string.
             (when (and at-sign-modifier colon-modifier)
               (error "Cannot specify colon and at-sign for this directive."))
             (when at-sign-modifier
               ;; Leave the newline in place.
               (write-char #\Newline destination))
             (unless colon-modifier
               ;; Eat trailing whitespace[1].
               (do () ((not (whitespace[1]p (char control-string (1+ offset)))))
                 (incf offset))))
            (#\( ;; Case conversion.
             ;; FIXME: Must check for outer ~(.
             (when prefix-parameters (error "~~( Expects no parameters."))
             (let ((s (make-case-correcting-stream destination (cond ((and colon-modifier at-sign-modifier)
                                                                      :upcase)
                                                                     (colon-modifier
                                                                      :titlecase)
                                                                     (at-sign-modifier
                                                                      :sentencecase)
                                                                     (t :downcase)))))
               (multiple-value-bind (new-offset new-args)
                   (interpret-format-substring s control-string (1+ offset) #\) args)
                 (setf offset new-offset
                       args new-args))))
            (#\) (unless (eql end-char #\))
                   (error "Unexpected format control character ~~) in ~S." control-string))
                 (return (values offset args)))
            (#\/
             (let ((package-name "COMMON-LISP-USER")
                   (symbol-name (make-array 20 :element-type 'character :adjustable t :fill-pointer 0))
                   (allow-internal nil))
               (do ((ch (char control-string (incf offset))
                        (char control-string (incf offset))))
                   ((eql ch #\/))
                 (cond ((and (eql ch #\:)
                             (zerop (length symbol-name)))
                        (setf allow-internal t))
                        ((eql ch #\:)
                         (setf package-name symbol-name
                               symbol-name (make-array 20 :element-type 'character :adjustable t :fill-pointer 0)))
                        (t (vector-push-extend (char-upcase ch) symbol-name))))
               (apply (intern symbol-name package-name)
                      destination (pop args)
                      colon-modifier at-sign-modifier
                      prefix-parameters)))
            (t (let ((fn (cadr (assoc (char control-string offset) *format-functions*
                                      :test #'char-equal))))
                 (if fn
                     (setf args (funcall fn destination args prefix-parameters
                                         at-sign-modifier colon-modifier))
                     (cerror "Ignore it."
                             "Invalid format control character ~S in ~S."
                             (char control-string offset) control-string))))))
        (write-char (char control-string offset) destination))))

(defun format* (destination control-string args)
  (if (functionp control-string)
      (apply control-string destination args)
      (interpret-format-substring destination control-string 0 nil args)))

(defun format (stream control &rest arguments)
  (declare (dynamic-extent arguments))
  (cond ((eql stream 'nil)
         (with-output-to-string (s)
           (format* s control arguments)))
        ((eql stream 't)
         (format* *standard-output* control arguments)
         nil)
        (t (format* stream control arguments)
           nil)))
