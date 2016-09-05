;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defvar *print-array* t)
(defvar *print-base* 10)
(defvar *print-case* :upcase)
(defvar *print-circle* nil)
(defvar *print-escape* t)
(defvar *print-gensym* t)
(defvar *print-length* nil)
(defvar *print-level* nil)
(defvar *print-lines* nil)
(defvar *print-miser-width* nil)
(defvar *print-pprint-dispatch* nil)
(defvar *print-pretty* nil)
(defvar *print-radix* nil)
(defvar *print-readably* nil)
(defvar *print-right-margin* nil)

(defvar *print-safe* nil)
(defvar *print-space-char-ansi* nil)
(defvar *print-bignums-safely* nil)

(defun write-unsigned-integer-inner (x base n-digits stream)
  (unless (zerop n-digits)
    (multiple-value-bind (quot rem)
        (truncate x base)
      (write-unsigned-integer-inner quot base (1- n-digits) stream)
      (write-char (schar "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" rem)
                  stream))))

(defun write-unsigned-integer-outer (x base base-divisor base-digits stream)
  (unless (zerop x)
    (multiple-value-bind (quot rem)
        (truncate x base-divisor)
      (write-unsigned-integer-outer quot base base-divisor base-digits stream)
      (if (zerop quot)
          (write-unsigned-integer-simple rem base stream)
          (write-unsigned-integer-inner rem base base-digits stream)))))

(defun write-unsigned-integer-simple (x base stream)
  (unless (zerop x)
    (multiple-value-bind (quot rem)
        (truncate x base)
      (write-unsigned-integer-simple quot base stream)
      (write-char (schar "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" rem)
                  stream))))

(defun write-unsigned-integer (x base stream)
  (cond ((fixnump x)
         (write-unsigned-integer-simple x base stream))
        (t
         (let ((digits (floor (log most-positive-fixnum base))))
           (write-unsigned-integer-outer x base
                                         (expt base digits)
                                         digits
                                         stream)))))

(defun write-integer (x &optional (base 10) stream)
  (cond ((and *print-bignums-safely*
              (bignump x)
              (>= (%n-bignum-fragments x) 2))
         ;; Very large bignum.
         (write-string "#<Bignum" stream)
         (dotimes (i (%n-bignum-fragments x))
           (format stream " ~16,'0X"
                   (%bignum-fragment x (- (%n-bignum-fragments x) i 1))))
         (write-char #\> stream))
        ((= x 0)
         (write-char #\0 stream))
        ((< x 0)
         (write-char #\- stream)
         (write-unsigned-integer (- 0 x) base stream))
        (t (write-unsigned-integer x base stream))))

(defun write-float (float stream &optional (trailing-digits 6))
  (when (float-nan-p float)
    (print-unreadable-object (float stream :type t)
      (format stream "~A NaN" (if (float-trapping-nan-p float) "signalling" "quiet")))
    (return-from write-float))
  (when (float-infinity-p float)
    (if (minusp float)
        (format stream "#.~S" (if (single-float-p float)
                                  'single-float-negative-infinity
                                  'double-float-negative-infinity))
        (format stream "#.~S" (if (single-float-p float)
                                  'single-float-positive-infinity
                                  'double-float-positive-infinity)))
    (return-from write-float))
  (when (< float 0.0)
    (write-char #\- stream)
    (setf float (- float)))
  (multiple-value-bind (integer-part decimal-part)
      (truncate float)
    (write integer-part :stream stream :base 10)
    (write-char #\. stream)
    ;; Print the decimal part number-by-number to ensure
    ;; proper leading zeros.
    (let ((adjusted-decimal (truncate (* decimal-part
                                         (expt 10 trailing-digits))))
          (trailing-zeros 0))
      (cond ((zerop adjusted-decimal)
             (write-char #\0 stream))
            (t (let ((x adjusted-decimal))
                 (loop
                    (multiple-value-bind (quot rem)
                        (truncate x 10)
                      (when (zerop x) (return))
                      (setf x quot)
                      (if (zerop rem)
                          (incf trailing-zeros)
                          (return)))))
               (labels ((frob (val digit-position)
                          (when (< digit-position trailing-digits)
                            (multiple-value-bind (quot rem)
                                (truncate val 10)
                              (frob quot (1+ digit-position))
                              (when (>= digit-position trailing-zeros)
                                (write rem :stream stream :base 10))))))
                 (frob adjusted-decimal 0))))))
  (when (not (eql (type-of float) *read-default-float-format*))
    (etypecase float
      (single-float
       (write-string "F0" stream))
      (double-float
       (write-string "D0" stream)))))

(defun terpri (&optional stream)
  (write-char #\Newline stream)
  nil)

(defun fresh-line (&optional stream)
  (cond ((start-line-p stream)
         nil)
        (t (terpri stream)
           t)))

(defun write-string (string &optional stream &key (start 0) end)
  (unless end (setf end (length string)))
  (dotimes (i (- end start))
    (write-char (char string (+ start i)) stream))
  string)

(defun write-line (string &optional stream &key (start 0) end)
  (write-string string stream :start start :end end)
  (terpri stream)
  string)

(defun write-case-escaped-string (string stream)
  "Print STRING while obeying readtable case, *PRINT-CASE* and *PRINT-BASE*."
  (ecase *print-case*
    (:upcase
     (let ((need-escaping (or (some (lambda (c)
                                      (or (and (upper-case-p c)
                                               (digit-char-p c *print-base*))
                                          (member c '(#\| #\\))
                                          (lower-case-p c)))
                                    string)
                              (zerop (length string)))))
       (when need-escaping
         (write-char #\| stream))
       (dotimes (i (length string))
         (write-char (char string i) stream))
       (when need-escaping
         (write-char #\| stream))))
    (:downcase
     (dotimes (i (length string))
       (let ((c (char string i)))
         (cond ((or (and (upper-case-p c)
                         (digit-char-p c *print-base*))
                    (member c '(#\| #\\))
                    (lower-case-p c))
                (write-char #\\ stream)
                (write-char c stream))
               (t (write-char (char-downcase c) stream))))))))

(defun write-symbol (object stream)
  (cond ((or *print-escape* *print-readably*)
         (cond ((null (symbol-package object))
                (when *print-gensym*
                  (write-string "#:" stream)))
               ((keywordp object)
                (write-char #\: stream))
               (t (multiple-value-bind (symbol status)
                      (find-symbol (symbol-name object) *package*)
                    (unless (and status (eql symbol object))
                      ;; Not accessible in the current package.
                      (multiple-value-bind (symbol status)
                          (find-symbol (symbol-name object) (symbol-package object))
                        (write-case-escaped-string (package-name (symbol-package object)) stream)
                        (write-char #\: stream)
                        (when (not (eql status :external))
                          (write-char #\: stream)))))))
         (write-case-escaped-string (symbol-name object) stream))
        (t (write-case-escaped-string (symbol-name object) stream))))

(defun write-ratio (object stream)
  (when *print-radix*
    (case *print-base*
      (2 (write-string "#b" stream))
      (8 (write-string "#o" stream))
      (16 (write-string "#x" stream))
      (t (write-char #\# stream)
         (write-integer *print-base* 10 stream)
         (write-char #\r stream))))
  (let ((numerator (numerator object)))
    (when (minusp numerator)
      (write-char #\- stream)
      (setf numerator (- numerator)))
    (write-integer numerator *print-base* stream))
  (write-char #\/ stream)
  (write-integer (denominator object) *print-base* stream))

(defmacro with-printer-level/length ((stream) &body body)
  (let ((length (gensym)))
    `(cond ((or (not *print-level*)
                (plusp *print-level*))
            (let ((*print-level* (if *print-level*
                                     (1- *print-level*)
                                     nil))
                  (,length *print-length*))
              (flet ((output (obj)
                       (cond ((not ,length)
                              (write obj :stream ,stream)
                              t)
                             ((plusp ,length)
                              (decf ,length)
                              (write obj :stream ,stream)
                              t)
                             (t
                              (write-string "..." ,stream)
                              nil))))
                ,@body)))
           (t
            (write-char #\# ,stream)))))

(defun write-cons (object stream)
  (with-printer-level/length (stream)
    (let ((length *print-length*))
      (write-char #\( stream)
      (when (output (car object))
        (do ((i (cdr object) (cdr i)))
            ((atom i)
             (when i
               (write-string " . " stream)
               (write i :stream stream)))
          (write-char #\Space stream)
          (when (not (output (car i)))
            (return))))
      (write-char #\) stream))))

(defun write-vector (object stream)
  (with-printer-level/length (stream)
    (write-char #\# stream)
    (write-char #\( stream)
    (dotimes (i (length object))
      (unless (zerop i)
        (write-char #\Space stream))
      (when (not (output (aref object i)))
        (return)))
    (write-char #\) stream)))

(defun write-bit-vector (object stream)
  (write-char #\# stream)
  (write-char #\* stream)
  (dotimes (i (length object))
    (if (zerop (aref object i))
        (write-char #\0 stream)
        (write-char #\1 stream))))

(defun write-complex (object stream)
  (write-string "#C(" stream)
  (write (realpart object) :stream stream)
  (write-char #\Space stream)
  (write (imagpart object) :stream stream)
  (write-char #\) stream))

(defun write-character (object stream)
  (cond ((or *print-readably* *print-escape*)
         (write-char #\# stream)
         (write-char #\\ stream)
         (cond ((and (or *print-space-char-ansi* (not (eql object #\Space)))
                     (not (eql object #\Newline))
                     (standard-char-p object))
                (write-char object stream))
               (t (write-string (char-name object) stream))))
        (t (write-char object stream))))

(defun write-object (object stream)
  (typecase object
    (integer
     (when *print-radix*
       (case *print-base*
         (2 (write-string "#b" stream))
         (8 (write-string "#o" stream))
         (10) ; Nothing.
         (16 (write-string "#x" stream))
         (t (write-char #\# stream)
            (write-integer *print-base* 10 stream)
            (write-char #\r stream))))
     (write-integer object *print-base* stream)
     (when (and *print-radix* (eql *print-base* 10))
       (write-char #\. stream)))
    (float
     (write-float object stream))
    (ratio
     (write-ratio object stream))
    (cons
     (write-cons object stream))
    (symbol
     (write-symbol object stream))
    (string
     (cond ((or *print-escape* *print-readably*)
            (write-char #\" stream)
            (dotimes (i (length object))
              (let ((c (char object i)))
                (case c
                  (#\\ (write-char #\\ stream) (write-char #\\ stream))
                  (#\" (write-char #\\ stream) (write-char #\" stream))
                  (t (write-char c stream)))))
            (write-char #\" stream))
           (t (write-string object stream))))
    (character
     (write-character object stream))
    (function
     (cond ((and (not *print-safe*)
                 (typep object 'mezzano.clos:funcallable-standard-object))
            (print-object object stream))
           (t (let ((name (function-name object)))
                ;; So that only one space is printed if there is no name.
                (if name
                    (print-unreadable-object (object stream :type t :identity t)
                      (write name :stream stream))
                    (print-unreadable-object (object stream :type t :identity t)))))))
    (bit-vector
     (if *print-array*
         (write-bit-vector object stream)
         (print-unreadable-object (object stream :type t :identity t))))
    (vector
     (if *print-array*
         (write-vector object stream)
         (print-unreadable-object (object stream :type t :identity t))))
    (complex
     (write-complex object stream))
    (t (if *print-safe*
           (print-unreadable-object (object stream :type t :identity t))
           (print-object object stream)))))

;; Overridden later when the pretty printer is loaded.
(defun write-pretty (object stream)
  (write object :stream stream :pretty nil))

(defun write (object &key
                       (stream *standard-output*)
                       (readably *print-readably*)
                       (escape *print-escape*)
                       (radix *print-radix*)
                       (base *print-base*)
                       (circle *print-circle*)
                       (pretty *print-pretty*)
                       (level *print-level*)
                       (length *print-length*)
                       (case *print-case*)
                       (gensym *print-gensym*)
                       (array *print-array*)
                       (pprint-dispatch *print-pprint-dispatch*)
                       (right-margin *print-right-margin*)
                       (lines *print-lines*)
                       (miser-width *print-miser-width*))
  (let ((*print-readably* readably)
        (*print-escape* escape)
        (*print-radix* radix)
        (*print-base* base)
        (*print-circle* circle)
        (*print-pretty* pretty)
        (*print-level* level)
        (*print-length* length)
        (*print-case* case)
        (*print-gensym* gensym)
        (*print-array* array)
        (*print-pprint-dispatch* pprint-dispatch)
        (*print-right-margin* right-margin)
        (*print-lines* lines)
        (*print-miser-width* miser-width))
    (case stream
      ((nil) (setf stream *standard-output*))
      ((t) (setf stream *terminal-io*)))
    (cond
      (pretty
       (write-pretty object stream))
      (t
       (write-object object stream)))
    object))

(defmacro print-unreadable-object ((object stream &rest keys &key type identity) &body body)
  `(%print-unreadable-object ,(when body `(lambda () (progn ,@body))) ,object ,stream ,@keys))

(defun %print-unreadable-object (fn object stream &key type identity)
  (write-char #\# stream)
  (write-char #\< stream)
  (when type
    (write (type-of object) :stream (make-case-correcting-stream stream :titlecase)))
  (when fn
    (when type
      (write-char #\Space stream))
    (funcall fn))
  (when identity
    (when (or type fn)
      (write-char #\Space stream))
    (write-integer (sys.int::lisp-object-address object) 16 stream))
  (write-char #\> stream)
  nil)

(defun prin1 (object &optional output-stream)
  (write object :stream output-stream :escape t))

(defun princ (object &optional output-stream)
  (write object :stream output-stream :escape nil :readably nil))

(defun print (object &optional output-stream)
  (terpri output-stream)
  (write object :stream output-stream :escape nil :readably nil)
  (write-char #\Space output-stream)
  object)

(defun pprint (object &optional output-stream)
  (terpri output-stream)
  (write object :stream output-stream :escape nil :readably nil :pretty t)
  (values))
